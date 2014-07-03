// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2014.

namespace NuEdit
open NuEditDesign
open SDL2
open OpenTK
open TiledSharp
open Prime
open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open System.Windows.Forms
open System.ComponentModel
open System.Xml
open System.Xml.Serialization
open Microsoft.FSharp.Reflection
open Prime
open Nu
open Nu.NuConstants
open NuEdit.NuEditConstants
open NuEdit.NuEditReflection

// TODO: increase warning level to 5.

[<AutoOpen>]
module ProgramModule =

    type WorldChanger = World -> World

    type WorldChangers = WorldChanger LinkedList

    type DragEntityState =
        | DragEntityNone
        | DragEntityPosition of Vector2 * Vector2 * Address
        | DragEntityRotation of Vector2 * Vector2 * Address

    type DragCameraState =
        | DragCameraNone
        | DragCameraPosition of Vector2 * Vector2

    type EditorState =
        { DragEntityState : DragEntityState
          DragCameraState : DragCameraState
          PastWorlds : World list
          FutureWorlds : World list
          Clipboard : (Entity option) ref }

module Program =

    let DefaultPositionSnap = 8
    let DefaultRotationSnap = 5
    let DefaultCreationDepth = 0.0f
    let CameraSpeed = 4.0f // NOTE: might be nice to be able to configure this just like entity creation depth in the editor

    let getPickableEntities world =
        World.getEntities EditorGroupAddress world |>
            Map.toValueSeq |>
            Seq.filter (fun entity -> Entity.dispatchesAs typeof<Entity2dDispatcher> entity world) |>
            Seq.toList

    let pushPastWorld pastWorld world =
        let editorState = world.ExtData :?> EditorState
        let editorState = { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = [] }
        { world with ExtData = editorState }

    let clearOtherWorlds world =
        let editorState = world.ExtData :?> EditorState
        let editorState = { editorState with PastWorlds = []; FutureWorlds = [] }
        { world with ExtData = editorState }

    type [<TypeDescriptionProvider (typeof<EntityTypeDescriptorProvider>)>] EntityTypeDescriptorSource =
        { Address : Address
          Form : NuEditForm
          WorldChangers : WorldChangers
          RefWorld : World ref }

    and EntityPropertyDescriptor (property) =
        inherit PropertyDescriptor ((match property with EntityXFieldDescriptor x -> x.FieldName | EntityPropertyInfo p -> p.Name), Array.empty)

        let propertyName = match property with EntityXFieldDescriptor x -> x.FieldName | EntityPropertyInfo p -> p.Name
        let propertyType = match property with EntityXFieldDescriptor x -> findType x.TypeName | EntityPropertyInfo p -> p.PropertyType
        let propertyCanWrite = match property with EntityXFieldDescriptor _ -> true | EntityPropertyInfo x -> x.CanWrite

        override this.ComponentType with get () = propertyType.DeclaringType
        override this.PropertyType with get () = propertyType
        override this.CanResetValue _ = false
        override this.ResetValue _ = ()
        override this.ShouldSerializeValue _ = true

        override this.IsReadOnly
            with get () =
                not propertyCanWrite ||
                not <| Xtension.isPropertyNameWriteable propertyName

        override this.GetValue optSource =
            match optSource with
            | null -> null
            | source ->
                let entityTds = source :?> EntityTypeDescriptorSource
                let entity = World.getEntity entityTds.Address !entityTds.RefWorld
                getEntityPropertyValue property entity

        override this.SetValue (source, value) =
            let entityTds = source :?> EntityTypeDescriptorSource
            let changer = (fun world ->
                let pastWorld = world
                let world =
                    match propertyName with
                    | "Name" -> // MAGIC_VALUE
                        // handle special case for an entity's Name field change
                        let valueStr = string value
                        if Int64.TryParse (valueStr, ref 0L) then
                            trace <| "Invalid entity name '" + valueStr + "' (must not be a number)."
                            world
                        else
                            // TODO: factor out a renameEntity function
                            let entity = World.getEntity entityTds.Address world
                            let world = World.removeEntityImmediate entityTds.Address world
                            let entity = { entity with Name = valueStr }
                            let entityAddress = addrstr EditorGroupAddress valueStr
                            let world = World.addEntity entityAddress entity world
                            entityTds.RefWorld := world // must be set for property grid
                            entityTds.Form.propertyGrid.SelectedObject <- { entityTds with Address = entityAddress }
                            world
                    | _ ->
                        let world = setEntityPropertyValue entityTds.Address property value world
                        let entity = World.getEntity entityTds.Address world
                        entity.PropagatePhysics (entityTds.Address, world)
                pushPastWorld pastWorld world)
            // in order to update the view immediately, we have to apply the changer twice, once
            // now and once in the update function
            entityTds.RefWorld := changer !entityTds.RefWorld
            ignore <| entityTds.WorldChangers.AddLast changer

        // NOTE: This has to be a static member in order to see the relevant types in the recursive definitions.
        static member GetPropertyDescriptors (aType : Type) optSource =
            let properties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
            let properties = Seq.filter (fun (property : PropertyInfo) -> Seq.isEmpty <| property.GetCustomAttributes<ExtensionAttribute> ()) properties
            let optProperty = Seq.tryFind (fun (property : PropertyInfo) -> property.PropertyType = typeof<Xtension>) properties
            let propertyDescriptors = Seq.map (fun property -> EntityPropertyDescriptor (EntityPropertyInfo property) :> PropertyDescriptor) properties
            let propertyDescriptors =
                match (optProperty, optSource) with
                | (None, _) 
                | (_, None) -> propertyDescriptors
                | (Some property, Some entity) ->
                    let xtension = property.GetValue entity :?> Xtension
                    let xFieldDescriptors =
                        Seq.map
                            (fun (xField : KeyValuePair<string, obj>) ->
                                let fieldName = xField.Key
                                let typeName = (xField.Value.GetType ()).FullName
                                let xFieldDescriptor = EntityXFieldDescriptor { FieldName = fieldName; TypeName = typeName }
                                EntityPropertyDescriptor xFieldDescriptor :> PropertyDescriptor)
                            xtension.XFields
                    Seq.append xFieldDescriptors propertyDescriptors
            List.ofSeq propertyDescriptors

    and EntityTypeDescriptor (optSource : obj) =
        inherit CustomTypeDescriptor ()
        override this.GetProperties _ =
            let propertyDescriptors =
                match optSource with
                | :? EntityTypeDescriptorSource as source ->
                    let entity = World.getEntity source.Address !source.RefWorld
                    EntityPropertyDescriptor.GetPropertyDescriptors typeof<Entity> <| Some entity
                | _ -> EntityPropertyDescriptor.GetPropertyDescriptors typeof<Entity> None
            PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    and EntityTypeDescriptorProvider () =
        inherit TypeDescriptionProvider ()
        override this.GetTypeDescriptor (_, optSource) =
            EntityTypeDescriptor optSource :> ICustomTypeDescriptor

    let getSnaps (form : NuEditForm) =
        let positionSnap = ref 0
        ignore <| Int32.TryParse (form.positionSnapTextBox.Text, positionSnap)
        let rotationSnap = ref 0
        ignore <| Int32.TryParse (form.rotationSnapTextBox.Text, rotationSnap)
        (!positionSnap, !rotationSnap)
    
    let getCreationDepth (form : NuEditForm) =
        let creationDepth = ref 0.0f
        ignore <| Single.TryParse (form.creationDepthTextBox.Text, creationDepth)
        !creationDepth

    let beginEntityDrag (form : NuEditForm) worldChangers refWorld message world =
        let pastWorld = world
        match message.Data with
        | MouseButtonData _ ->
            let entities = getPickableEntities world
            let mousePosition = world.MouseState.MousePosition
            let optPicked = Entity.tryPick mousePosition entities world
            match optPicked with
            | None -> (Handled, world)
            | Some entity ->
                let mousePositionEntity = Entity.mouseToEntity mousePosition world entity
                let entityAddress = addrstr EditorGroupAddress entity.Name
                let dragState = DragEntityPosition (entity.Position + mousePositionEntity, mousePositionEntity, entityAddress)
                let editorState = world.ExtData :?> EditorState
                let editorState = { editorState with DragEntityState = dragState }
                let world = { world with ExtData = editorState }
                let world = pushPastWorld pastWorld world
                refWorld := world // must be set for property grid
                form.propertyGrid.SelectedObject <- { Address = entityAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                (Handled, world)
        | _ -> failwith <| "Expected MouseButtonData in message '" + string message + "'."

    let endEntityDrag (form : NuEditForm) message world =
        match message.Data with
        | MouseButtonData _ ->
            let editorState = world.ExtData :?> EditorState
            match editorState.DragEntityState with
            | DragEntityNone -> (Handled, world)
            | DragEntityPosition _
            | DragEntityRotation _ ->
                let editorState = { editorState with DragEntityState = DragEntityNone }
                form.propertyGrid.Refresh ()
                (Handled, { world with ExtData = editorState })
        | _ -> failwith <| "Expected MouseButtonData in message '" + string message + "'."

    let updateEntityDrag (form : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        match editorState.DragEntityState with
        | DragEntityNone -> world
        | DragEntityPosition (pickOffset, mousePositionEntityOrig, address) ->
            let (positionSnap, _) = getSnaps form
            let entity = World.getEntity address world
            let mousePositionEntity = Entity.mouseToEntity world.MouseState.MousePosition world entity
            let entityPosition = (pickOffset - mousePositionEntityOrig) + (mousePositionEntity - mousePositionEntityOrig)
            let entity = Entity.setPositionSnapped positionSnap entityPosition entity
            let world = World.setEntity address entity world
            let editorState = { editorState with DragEntityState = DragEntityPosition (pickOffset, mousePositionEntityOrig, address) }
            let world = { world with ExtData = editorState }
            let world = entity.PropagatePhysics (address, world)
            form.propertyGrid.Refresh ()
            world
        | DragEntityRotation _ -> world

    let beginCameraDrag (_ : NuEditForm) message world =
        match message.Data with
        | MouseButtonData _ ->
            let mousePosition = world.MouseState.MousePosition
            let mousePositionScreen = Camera.mouseToScreen mousePosition world.Camera
            let dragState = DragCameraPosition (world.Camera.EyeCenter + mousePositionScreen, mousePositionScreen)
            let editorState = world.ExtData :?> EditorState
            let editorState = { editorState with DragCameraState = dragState }
            let world = { world with ExtData = editorState }
            (Handled, world)
        | _ -> failwith <| "Expected MouseButtonData in message '" + string message + "'."

    let endCameraDrag (_ : NuEditForm) message world =
        match message.Data with
        | MouseButtonData _ ->
            let editorState = world.ExtData :?> EditorState
            match editorState.DragCameraState with
            | DragCameraNone -> (Handled, world)
            | DragCameraPosition _ ->
                let editorState = { editorState with DragCameraState = DragCameraNone }
                (Handled, { world with ExtData = editorState })
        | _ -> failwith <| "Expected MouseButtonData in message '" + string message + "'."

    let simulantRemovedHandler (form : NuEditForm) message world =
        match form.propertyGrid.SelectedObject with
        | null -> (Unhandled, world)
        | :? EntityTypeDescriptorSource as entityTds ->
            if message.Publisher <> entityTds.Address then (Unhandled, world)
            else
                form.propertyGrid.SelectedObject <- null
                let editorState = { (world.ExtData :?> EditorState) with DragEntityState = DragEntityNone }
                (Unhandled, { world with ExtData = editorState })
        | _ -> failwith "Unexpected match failure in NuEdit.Program.simulantRemovedHandler."

    let updateCameraDrag (_ : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        match editorState.DragCameraState with
        | DragCameraNone -> world
        | DragCameraPosition (pickOffset, mousePositionScreenOrig) ->
            let mousePosition = world.MouseState.MousePosition
            let mousePositionScreen = Camera.mouseToScreen mousePosition world.Camera
            let eyeCenter = (pickOffset - mousePositionScreenOrig) + -CameraSpeed * (mousePositionScreen - mousePositionScreenOrig)
            let camera = { world.Camera with EyeCenter = eyeCenter }
            let world = { world with Camera = camera }
            let editorState = { editorState with DragCameraState = DragCameraPosition (pickOffset, mousePositionScreenOrig) }
            { world with ExtData = editorState }

    let handleExit (form : NuEditForm) _ =
        form.Close ()

    let handleCreate (form : NuEditForm) (worldChangers : WorldChangers) refWorld atMouse _ =
        let world = !refWorld
        let entityXDispatcherName = form.createEntityComboBox.Text
        try let entity = Entity.makeDefault entityXDispatcherName None world
            ignore <| worldChangers.AddLast (fun world ->
                let pastWorld = world
                let (positionSnap, rotationSnap) = getSnaps form
                let mousePositionEntity = Entity.mouseToEntity world.MouseState.MousePosition world entity
                let entityPosition = if atMouse then mousePositionEntity else world.Camera.EyeCenter
                let entityTransform = { Transform.Position = entityPosition; Depth = getCreationDepth form; Size = DefaultEntitySize; Rotation = DefaultEntityRotation }
                let entity = Entity.setTransform positionSnap rotationSnap entityTransform entity
                let entityAddress = addrstr EditorGroupAddress entity.Name
                let world = World.addEntity entityAddress entity world
                let world = pushPastWorld pastWorld world
                refWorld := world // must be set for property grid
                form.propertyGrid.SelectedObject <- { Address = entityAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                world)
        with exn ->
            ignore <| MessageBox.Show ("Invalid entity XDispatcher name '" + entityXDispatcherName + "'.")

    let handleDelete (form : NuEditForm) (worldChangers : WorldChangers) (_ : World ref) _ =
        let selectedObject = form.propertyGrid.SelectedObject
        ignore <| worldChangers.AddLast (fun world ->
            let pastWorld = world
            match selectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = World.removeEntity entityTds.Address world
                let world = pushPastWorld pastWorld world
                form.propertyGrid.SelectedObject <- null
                world
            | _ -> world)

    let handleSave (form : NuEditForm) (_ : WorldChangers) refWorld _ =
        form.saveFileDialog.FileName <- String.Empty
        let saveFileResult = form.saveFileDialog.ShowDialog form
        match saveFileResult with
        | DialogResult.OK -> saveFile form.saveFileDialog.FileName !refWorld
        | _ -> ()

    let handleOpen (form : NuEditForm) (worldChangers : WorldChangers) (_ : World ref) _ =
        let openFileResult = form.openFileDialog.ShowDialog form
        match openFileResult with
        | DialogResult.OK ->
            ignore <| worldChangers.AddLast (fun world ->
                let world = loadFile form.openFileDialog.FileName world
                let world = clearOtherWorlds world
                form.propertyGrid.SelectedObject <- null
                form.interactivityButton.Checked <- false
                world)
        | _ -> ()

    let handleUndo (form : NuEditForm) (worldChangers : WorldChangers) (_ : World ref) _ =
        ignore <| worldChangers.AddLast (fun world ->
            let editorState = world.ExtData :?> EditorState
            match editorState.PastWorlds with
            | [] -> world
            | pastWorld :: pastWorlds ->
                let futureWorld = world
                let world = World.rebuildPhysicsHack EditorGroupAddress pastWorld
                let editorState = { editorState with PastWorlds = pastWorlds; FutureWorlds = futureWorld :: editorState.FutureWorlds }
                let world = { world with ExtData = editorState; Interactivity = GuiAndPhysics }
                form.interactivityButton.Checked <- false
                form.propertyGrid.SelectedObject <- null
                world)

    let handleRedo (form : NuEditForm) (worldChangers : WorldChangers) (_ : World ref) _ =
        ignore <| worldChangers.AddLast (fun world ->
            let editorState = world.ExtData :?> EditorState
            match editorState.FutureWorlds with
            | [] -> world
            | futureWorld :: futureWorlds ->
                let pastWorld = world
                let world = World.rebuildPhysicsHack EditorGroupAddress futureWorld
                let editorState = { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = futureWorlds }
                let world = { world with ExtData = editorState; Interactivity = GuiAndPhysics }
                form.interactivityButton.Checked <- false
                form.propertyGrid.SelectedObject <- null
                world)

    let handleInteractivityChanged (form : NuEditForm) (worldChangers : WorldChangers) (_ : World ref) _ =
        // TODO: enable disabling of physics as well
        let interactivity = if form.interactivityButton.Checked then GuiAndPhysicsAndGamePlay else GuiAndPhysics
        ignore <| worldChangers.AddLast (fun world ->
            let pastWorld = world
            let world = { world with Interactivity = interactivity }
            if Interactivity.gamePlaying interactivity then pushPastWorld pastWorld world
            else world)

    let handleCut (form : NuEditForm) (worldChangers : WorldChangers) (_ : World ref) _ =
        let optEntityTds = form.propertyGrid.SelectedObject
        match optEntityTds with
        | null -> ()
        | :? EntityTypeDescriptorSource as entityTds ->
            ignore <| worldChangers.AddLast (fun world ->
                let editorState = world.ExtData :?> EditorState
                let entity = World.getEntity entityTds.Address world
                let world = World.removeEntity entityTds.Address world
                editorState.Clipboard := Some entity
                form.propertyGrid.SelectedObject <- null
                world)
        | _ -> trace <| "Invalid cut operation (likely a code issue in NuEdit)."
        
    let handleCopy (form : NuEditForm) (_ : WorldChangers) refWorld _ =
        let optEntityTds = form.propertyGrid.SelectedObject
        match optEntityTds with
        | null -> ()
        | :? EntityTypeDescriptorSource as entityTds ->
            let entity = World.getEntity entityTds.Address !refWorld
            let editorState = (!refWorld).ExtData :?> EditorState
            editorState.Clipboard := Some entity
        | _ -> trace <| "Invalid copy operation (likely a code issue in NuEdit)."

    let handlePaste (form : NuEditForm) (worldChangers : WorldChangers) refWorld atMouse _ =
        let editorState = (!refWorld).ExtData :?> EditorState
        match !editorState.Clipboard with
        | None -> ()
        | Some entity ->
            ignore <| worldChangers.AddLast (fun world ->
                let (positionSnap, rotationSnap) = getSnaps form
                let id = NuCore.makeId ()
                let mousePositionEntity = Entity.mouseToEntity world.MouseState.MousePosition world entity
                let entity = { entity with Id = id; Name = string id }
                let entityPosition = if atMouse then mousePositionEntity else world.Camera.EyeCenter
                let entityTransform = { Entity.getTransform entity with Position = entityPosition }
                let entity = Entity.setTransform positionSnap rotationSnap entityTransform entity
                let address = addrstr EditorGroupAddress entity.Name
                let world = pushPastWorld world world
                World.addEntity address entity world)

    // TODO: add undo to quick size
    let handleQuickSize (form : NuEditForm) (worldChangers : WorldChangers) refWorld _ =
        let optEntityTds = form.propertyGrid.SelectedObject
        match optEntityTds with
        | null -> ()
        | :? EntityTypeDescriptorSource as entityTds ->
            ignore <| worldChangers.AddLast (fun world ->
                let entity = World.getEntity entityTds.Address world
                let entity = entity.SetSize <| entity.GetQuickSize world
                let world = World.setEntity entityTds.Address entity world
                let world = entity.PropagatePhysics (entityTds.Address, world)
                refWorld := world // must be set for property grid
                form.propertyGrid.Refresh ()
                world)
        | _ -> trace <| "Invalid quick size operation (likely a code issue in NuEdit)."

    let handleResetCamera (_ : NuEditForm) (worldChangers : WorldChangers) (_ : World ref) _ =
        ignore <| worldChangers.AddLast (fun world ->
            let camera = { world.Camera with EyeCenter = Vector2.Zero }
            { world with Camera = camera })

    let handleAddXField (form : NuEditForm) (worldChangers : WorldChangers) refWorld _ =
        match (form.xFieldNameTextBox.Text, form.typeNameTextBox.Text) with
        | ("", _) -> ignore <| MessageBox.Show "Enter an XField name."
        | (_, "") -> ignore <| MessageBox.Show "Enter a type name."
        | (xFieldName, typeName) ->
            match tryFindType typeName with
            | None -> ignore <| MessageBox.Show "Enter a valid type name."
            | Some aType ->
                let selectedObject = form.propertyGrid.SelectedObject
                match selectedObject with
                | :? EntityTypeDescriptorSource as entityTds ->
                    ignore <| worldChangers.AddLast (fun world ->
                        let pastWorld = world
                        let entity = World.getEntity entityTds.Address world
                        let xFieldValue = if aType = typeof<string> then String.Empty :> obj else Activator.CreateInstance aType
                        let xFields = Map.add xFieldName xFieldValue entity.Xtension.XFields
                        let entity = { entity with Xtension = { entity.Xtension with XFields = xFields }}
                        let world = World.setEntity entityTds.Address entity world
                        let world = pushPastWorld pastWorld world
                        refWorld := world // must be set for property grid
                        form.propertyGrid.Refresh ()
                        form.propertyGrid.Select ()
                        form.propertyGrid.SelectedGridItem <- form.propertyGrid.SelectedGridItem.Parent.GridItems.[xFieldName]
                        world)
                | _ -> ignore <| MessageBox.Show "Select an entity to add the XField to."

    let handleRemoveSelectedXField (form : NuEditForm) (worldChangers : WorldChangers) refWorld _ =
        let selectedObject = form.propertyGrid.SelectedObject
        match selectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.propertyGrid.SelectedGridItem.Label with
            | "" -> ignore <| MessageBox.Show "Select an XField."
            | xFieldName ->
                ignore <| worldChangers.AddLast (fun world ->
                    let pastWorld = world
                    let entity = World.getEntity entityTds.Address world
                    let xFields = Map.remove xFieldName entity.Xtension.XFields
                    let entity = { entity with Xtension = { entity.Xtension with XFields = xFields }}
                    let world = World.setEntity entityTds.Address entity world
                    let world = pushPastWorld pastWorld world
                    refWorld := world // must be set for property grid
                    form.propertyGrid.Refresh ()
                    world)
        | _ -> ignore <| MessageBox.Show "Select an entity to remove an XField from."

    let handleClearAllXFields (form : NuEditForm) (worldChangers : WorldChangers) refWorld _ =
        let selectedObject = form.propertyGrid.SelectedObject
        match selectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            ignore <| worldChangers.AddLast (fun world ->
                let pastWorld = world
                let entity = World.getEntity entityTds.Address world
                let entity = { entity with Xtension = { entity.Xtension with XFields = Map.empty }}
                let world = World.setEntity entityTds.Address entity world
                let world = pushPastWorld pastWorld world
                refWorld := world // must be set for property grid
                form.propertyGrid.Refresh ()
                world)
        | _ -> ignore <| MessageBox.Show "Select an entity to clear all XFields from."

    let createNuEditForm worldChangers refWorld =
        let form = new NuEditForm ()
        form.displayPanel.MaximumSize <- Drawing.Size (ResolutionX, ResolutionY)
        form.positionSnapTextBox.Text <- string DefaultPositionSnap
        form.rotationSnapTextBox.Text <- string DefaultRotationSnap
        form.creationDepthTextBox.Text <- string DefaultCreationDepth
        form.exitToolStripMenuItem.Click.Add (handleExit form)
        form.createEntityButton.Click.Add (handleCreate form worldChangers refWorld false)
        form.createToolStripMenuItem.Click.Add (handleCreate form worldChangers refWorld false)
        form.createContextMenuItem.Click.Add (handleCreate form worldChangers refWorld true)
        form.deleteEntityButton.Click.Add (handleDelete form worldChangers refWorld)
        form.deleteToolStripMenuItem.Click.Add (handleDelete form worldChangers refWorld)
        form.deleteContextMenuItem.Click.Add (handleDelete form worldChangers refWorld)
        form.saveToolStripMenuItem.Click.Add (handleSave form worldChangers refWorld)
        form.openToolStripMenuItem.Click.Add (handleOpen form worldChangers refWorld)
        form.undoButton.Click.Add (handleUndo form worldChangers refWorld)
        form.undoToolStripMenuItem.Click.Add (handleUndo form worldChangers refWorld)
        form.redoButton.Click.Add (handleRedo form worldChangers refWorld)
        form.redoToolStripMenuItem.Click.Add (handleRedo form worldChangers refWorld)
        form.interactivityButton.CheckedChanged.Add (handleInteractivityChanged form worldChangers refWorld)
        form.cutToolStripMenuItem.Click.Add (handleCut form worldChangers refWorld)
        form.cutContextMenuItem.Click.Add (handleCut form worldChangers refWorld)
        form.copyToolStripMenuItem.Click.Add (handleCopy form worldChangers refWorld)
        form.copyContextMenuItem.Click.Add (handleCopy form worldChangers refWorld)
        form.pasteToolStripMenuItem.Click.Add (handlePaste form worldChangers refWorld false)
        form.pasteContextMenuItem.Click.Add (handlePaste form worldChangers refWorld true)
        form.quickSizeToolStripButton.Click.Add (handleQuickSize form worldChangers refWorld)
        form.resetCameraButton.Click.Add (handleResetCamera form worldChangers refWorld)
        form.addXFieldButton.Click.Add (handleAddXField form worldChangers refWorld)
        form.removeSelectedXFieldButton.Click.Add (handleRemoveSelectedXField form worldChangers refWorld)
        form.Show ()
        form

    let tryMakeEditorWorld gameDispatcher form worldChangers refWorld sdlDeps =
        let screen = Screen.makeDissolve typeof<ScreenDispatcher>.Name 100L 100L
        let editorState =
            { DragEntityState = DragEntityNone
              DragCameraState = DragCameraNone
              PastWorlds = []
              FutureWorlds = []
              Clipboard = ref None }
        let optWorld = World.tryMakeEmpty sdlDeps gameDispatcher GuiAndPhysics editorState
        match optWorld with
        | Left errorMsg -> Left errorMsg
        | Right world ->
            refWorld := world
            refWorld := World.addScreen EditorScreenAddress screen [(EditorGroupName, Group.makeDefault typeof<GroupDispatcher>.Name !refWorld, [])] !refWorld
            refWorld := World.setOptSelectedScreenAddress (Some EditorScreenAddress) !refWorld 
            refWorld := World.subscribe4 DownMouseLeftEvent [] (CustomSub <| beginEntityDrag form worldChangers refWorld) !refWorld
            refWorld := World.subscribe4 UpMouseLeftEvent [] (CustomSub <| endEntityDrag form) !refWorld
            refWorld := World.subscribe4 DownMouseCenterEvent [] (CustomSub <| beginCameraDrag form) !refWorld
            refWorld := World.subscribe4 UpMouseCenterEvent [] (CustomSub <| endCameraDrag form) !refWorld
            refWorld := World.subscribe4 (RemovingEvent @ AnyEvent) [] (CustomSub <| simulantRemovedHandler form) !refWorld
            Right !refWorld

    let populateCreateEntityComboBox (form : NuEditForm) world =
        form.createEntityComboBox.Items.Clear ()
        let entity2dDispatcherType = typeof<Entity2dDispatcher>
        for dispatcherKvp in world.Dispatchers do
            if Xtension.dispatchesAs2 entity2dDispatcherType dispatcherKvp.Value then
                ignore <| form.createEntityComboBox.Items.Add dispatcherKvp.Key
        world

    let tryCreateEditorWorld gameDispatcher form worldChangers refWorld sdlDeps =
        match tryMakeEditorWorld gameDispatcher form worldChangers refWorld sdlDeps with
        | Left _ as left -> left
        | Right _ -> Right <| populateCreateEntityComboBox form !refWorld

    let invalidateDisplayPanel (form : NuEditForm) world =
        form.displayPanel.Invalidate ()
        world

    // TODO: remove code duplication with below
    let updateUndo (form : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty editorState.PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not <| List.isEmpty editorState.PastWorlds then
            form.undoButton.Enabled <- true
            form.undoToolStripMenuItem.Enabled <- true

    let updateRedo (form : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        if form.redoToolStripMenuItem.Enabled then
            if List.isEmpty editorState.FutureWorlds then
                form.redoButton.Enabled <- false
                form.redoToolStripMenuItem.Enabled <- false
        elif not <| List.isEmpty editorState.FutureWorlds then
            form.redoButton.Enabled <- true
            form.redoToolStripMenuItem.Enabled <- true

    let updateEditorWorld form (worldChangers : WorldChangers) refWorld world =
        refWorld := updateEntityDrag form world
        refWorld := updateCameraDrag form !refWorld
        while worldChangers.Count <> 0 do
            refWorld := worldChangers.First.Value !refWorld
            worldChangers.RemoveFirst ()
        updateUndo form !refWorld
        updateRedo form !refWorld
        refWorld := { !refWorld with Liveness = if form.IsDisposed then Exiting else Running }
        !refWorld

    let selectWorkingDirectoryAndMakeGameDispatcher () =
        use openDialog = new OpenFileDialog ()
        openDialog.Filter <- "Executable Files (*.exe)|*.exe"
        openDialog.Title <- "Select your game's executable file to make its assets and XDispatchers available in the editor (or cancel for defaults)."
        if openDialog.ShowDialog () <> DialogResult.OK then GameDispatcher ()
        else
            Directory.SetCurrentDirectory <| Path.GetDirectoryName openDialog.FileName
            let assembly = Assembly.LoadFrom openDialog.FileName
            let optDispatcherType = assembly.GetTypes () |> Array.tryFind (fun aType -> aType.IsSubclassOf typeof<GameDispatcher>)
            match optDispatcherType with
            | None -> GameDispatcher ()
            | Some aType -> Activator.CreateInstance aType :?> GameDispatcher

    let [<EntryPoint; STAThread>] main _ =
        World.initTypeConverters ()
        let worldChangers = WorldChangers ()
        let refWorld = ref Unchecked.defaultof<World>
        let gameDispatcher = selectWorkingDirectoryAndMakeGameDispatcher ()
        use form = createNuEditForm worldChangers refWorld
        let sdlViewConfig = ExistingWindow form.displayPanel.Handle
        let sdlRendererFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
        let sdlConfig =
            { ViewConfig = sdlViewConfig
              ViewW = form.displayPanel.MaximumSize.Width
              ViewH = form.displayPanel.MaximumSize.Height
              RendererFlags = sdlRendererFlags
              AudioChunkSize = AudioBufferSizeDefault }
        World.run4
            (tryCreateEditorWorld gameDispatcher form worldChangers refWorld)
            (updateEditorWorld form worldChangers refWorld)
            (invalidateDisplayPanel form)
            sdlConfig