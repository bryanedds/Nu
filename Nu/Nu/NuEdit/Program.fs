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

    type WorldChangers = WorldChanger List

    type DragEntityState =
        | DragEntityNone
        | DragEntityPosition of Vector2 * Vector2 * Address
        | DragEntityRotation of Vector2 * Vector2 * Address

    type DragCameraState =
        | DragCameraNone
        | DragCameraPosition of Vector2 * Vector2

    type EditorState =
        { TargetDirectory : string
          RightClickPosition : Vector2
          DragEntityState : DragEntityState
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
        let entityMap = World.getEntities EditorGroupAddress world
        Map.toValueList entityMap

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

        override this.ComponentType = propertyType.DeclaringType
        override this.PropertyType = propertyType
        override this.CanResetValue _ = false
        override this.ResetValue _ = ()
        override this.ShouldSerializeValue _ = true

        override this.IsReadOnly =
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
                let world = pushPastWorld world world
                match propertyName with
                | "Name" ->
                    let valueStr = string value
                    if Int64.TryParse (valueStr, ref 0L) then
                        trace <| "Invalid entity name '" + valueStr + "' (must not be a number)."
                        world
                    else
                        let entity = World.getEntity entityTds.Address world
                        let world = World.removeEntityImmediate entityTds.Address world
                        let entity = { entity with Name = valueStr }
                        let entityAddress = addrlist EditorGroupAddress [valueStr]
                        let world = World.addEntity entityAddress entity world
                        entityTds.RefWorld := world // must be set for property grid
                        entityTds.Form.propertyGrid.SelectedObject <- { entityTds with Address = entityAddress }
                        world
                | _ ->
                    let entity = World.getEntity entityTds.Address world
                    let optOldOverlayName = entity.OptOverlayName
                    let entity = setEntityPropertyValue property value entity
                    let entity =
                        match propertyName with
                        | "OptOverlayName" ->
                            match entity.OptOverlayName with
                            | None -> entity
                            | Some overlayName ->
                                let entity = { entity with Id = entity.Id } // hacky copy
                                Overlayer.applyOverlay optOldOverlayName overlayName entity world.Overlayer
                                entity
                        | _ -> entity
                    let world = World.setEntity entityTds.Address entity world
                    let world = Entity.propagatePhysics entityTds.Address entity world
                    entityTds.RefWorld := world // must be set for property grid
                    entityTds.Form.propertyGrid.Refresh ()
                    world)
            // in order to update the view immediately, we have to apply the changer twice, once
            // now and once in the update function
            entityTds.RefWorld := changer !entityTds.RefWorld
            ignore <| entityTds.WorldChangers.Add changer

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

    let getExpansionState (treeView : TreeView) =
        let nodeStates =
            Seq.fold
                (fun state (node : TreeNode) -> if node.Nodes.Count > 0 then (node.Name, node.IsExpanded) :: state else state)
                []
                (enumerable treeView.Nodes)
        Map.ofSeq nodeStates
        
    let restoreExpansionState (treeView : TreeView) treeState =
        Map.iter
            (fun nodeName nodeExpansion ->
                match treeView.Nodes.Find (nodeName, true) with
                | [||] -> ()
                | nodes -> let node = nodes.[0] in if nodeExpansion then node.Expand () else node.Collapse ())
            treeState

    let addTreeViewNode (form : NuEditForm) entityAddress world =
        let entity = World.getEntity entityAddress world
        let entityGroupName = Option.getOrDefault entity.Xtension.OptXDispatcherName "[No Dispatcher]"
        let treeGroup = form.treeView.Nodes.[entityGroupName]
        if not <| treeGroup.Nodes.ContainsKey (string entityAddress) then
            let treeNode = TreeNode entity.Name
            treeNode.Name <- string entityAddress
            ignore <| treeGroup.Nodes.Add treeNode
        else () // when changing an entity name, entity will be added twice - once from win forms, once from world

    let clearTreeViewNodes (form : NuEditForm) =
        form.treeView.Nodes.Clear ()

    let populateCreateComboBox (form : NuEditForm) world =
        form.createEntityComboBox.Items.Clear ()
        for dispatcherKvp in world.Dispatchers do
            if Xtension.dispatchesAs2 typeof<EntityDispatcher> dispatcherKvp.Value then
                ignore <| form.createEntityComboBox.Items.Add dispatcherKvp.Key

    let populateTreeViewGroups (form : NuEditForm) world =
        for dispatcherKvp in world.Dispatchers do
            if Xtension.dispatchesAs2 typeof<EntityDispatcher> dispatcherKvp.Value then
                let treeGroup = TreeNode dispatcherKvp.Key
                treeGroup.Name <- treeGroup.Text
                ignore <| form.treeView.Nodes.Add treeGroup
        let noneGroup = TreeNode "[No Dispatcher]"
        ignore <| form.treeView.Nodes.Add noneGroup

    let populateTreeViewNodes (form : NuEditForm) world =
        for entityKvp in World.getEntities EditorGroupAddress world do
        let entityAddress = addrlist EditorGroupAddress [entityKvp.Key]
        addTreeViewNode form entityAddress world

    let tryScrollTreeViewToPropertyGridSelection (form : NuEditForm) =
        match form.propertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.treeView.Nodes.Find (string entityTds.Address, true) with
            | [||] -> ()
            | nodes ->
                let node = nodes.[0]
                if node.Parent.IsExpanded then
                    form.treeView.SelectedNode <- node
                    node.EnsureVisible ()
        | _ -> ()

    let refreshPropertyGrid (form : NuEditForm) world =
        match form.propertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            entityTds.RefWorld := world // must be set for property grid
            if World.containsEntity entityTds.Address world then form.propertyGrid.Refresh ()
            else form.propertyGrid.SelectedObject <- null
        | _ -> form.propertyGrid.SelectedObject <- null

    let refreshTreeView (form : NuEditForm) world =
        let treeState = getExpansionState form.treeView
        clearTreeViewNodes form
        populateTreeViewGroups form world
        populateTreeViewNodes form world
        restoreExpansionState form.treeView treeState
        tryScrollTreeViewToPropertyGridSelection form

    let refreshFormOnUndoRedo (form : NuEditForm) world =
        form.interactivityButton.Checked <- false
        refreshPropertyGrid form world
        refreshTreeView form world

    let canEditWithMouse (form : NuEditForm) world =
        World.isGamePlaying world &&
        not form.editWhileInteractiveCheckBox.Checked

    let tryMousePick (form : NuEditForm) mousePosition worldChangers refWorld world =
        let entities = getPickableEntities world
        let optPicked = Entity.tryPick mousePosition entities world
        match optPicked with
        | None -> None
        | Some entity ->
            let entityAddress = addrlist EditorGroupAddress [entity.Name]
            refWorld := world // must be set for property grid
            form.propertyGrid.SelectedObject <- { Address = entityAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
            tryScrollTreeViewToPropertyGridSelection form
            Some (entity, entityAddress)

    let trySaveFile fileName world =
        try let editorGroup = World.getGroup NuEditConstants.EditorGroupAddress world
            let editorEntities = World.getEntities NuEditConstants.EditorGroupAddress world
            World.saveGroupToFile editorGroup editorEntities fileName world
        with exn ->
            ignore <|
                MessageBox.Show
                    ("Could not save file due to: " + string exn,
                     "File save error.",
                     MessageBoxButtons.OK,
                     MessageBoxIcon.Error)

    let tryLoadFile (form : NuEditForm) fileName world =
        try let world = World.removeGroupImmediate NuEditConstants.EditorGroupAddress world
            let (group, entities) = World.loadGroupFromFile fileName world
            let world = World.addGroup NuEditConstants.EditorGroupAddress group entities world
            form.saveFileDialog.FileName <- Path.GetFileName fileName
            world
        with exn ->
            ignore <|
                MessageBox.Show
                    ("Could not load file due to: " + string exn,
                     "File load error.",
                     MessageBoxButtons.OK,
                     MessageBoxIcon.Error)
            world

    let handleNuDownMouseRight (form : NuEditForm) worldChangers refWorld (_ : Event) world =
        let handled = if World.isGamePlaying world then Unhandled else Handled
        let mousePosition = World.getMousePositionF world
        ignore <| tryMousePick form mousePosition worldChangers refWorld world
        let editorState = world.ExtData :?> EditorState
        let editorState = { editorState with RightClickPosition = mousePosition }
        let world = { world with ExtData = editorState }
        (handled, world)

    let handleNuBeginEntityDrag (form : NuEditForm) worldChangers refWorld (_ : Event) world =
        if canEditWithMouse form world then (Unhandled, world)
        else
            let handled = if World.isGamePlaying world then Unhandled else Handled
            let mousePosition = World.getMousePositionF world
            match tryMousePick form mousePosition worldChangers refWorld world with
            | None -> (handled, world)
            | Some (entity, entityAddress) ->
                let world = pushPastWorld world world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world entity
                let dragState = DragEntityPosition (entity.Position + mousePositionEntity, mousePositionEntity, entityAddress)
                let editorState = world.ExtData :?> EditorState
                let editorState = { editorState with DragEntityState = dragState }
                let world = { world with ExtData = editorState }
                (handled, world)

    let handleNuEndEntityDrag (form : NuEditForm) (_ : Event) world =
        if canEditWithMouse form world then (Unhandled, world)
        else
            let handled = if World.isGamePlaying world then Unhandled else Handled
            let editorState = world.ExtData :?> EditorState
            match editorState.DragEntityState with
            | DragEntityNone -> (Handled, world)
            | DragEntityPosition _
            | DragEntityRotation _ ->
                let editorState = { editorState with DragEntityState = DragEntityNone }
                form.propertyGrid.Refresh ()
                (handled, { world with ExtData = editorState })

    let handleNuBeginCameraDrag (_ : NuEditForm) (_ : Event) world =
        let mousePosition = World.getMousePositionF world
        let mousePositionScreen = Camera.mouseToScreen mousePosition world.Camera
        let dragState = DragCameraPosition (world.Camera.EyeCenter + mousePositionScreen, mousePositionScreen)
        let editorState = world.ExtData :?> EditorState
        let editorState = { editorState with DragCameraState = dragState }
        let world = { world with ExtData = editorState }
        (Handled, world)

    let handleNuBeginEndCameraDrag (_ : NuEditForm) (_ : Event) world =
        let editorState = world.ExtData :?> EditorState
        match editorState.DragCameraState with
        | DragCameraNone -> (Handled, world)
        | DragCameraPosition _ ->
            let editorState = { editorState with DragCameraState = DragCameraNone }
            (Handled, { world with ExtData = editorState })

    let handleNuEntityAdd (form : NuEditForm) event world =
        addTreeViewNode form event.Publisher world
        (Unhandled, world)

    let handleNuEntityRemoving (form : NuEditForm) event world =
        match form.treeView.Nodes.Find (string event.Publisher, true) with
        | [||] -> () // when changing an entity name, entity will be removed twice - once from win forms, once from world
        | treeNodes -> form.treeView.Nodes.Remove treeNodes.[0]
        match form.propertyGrid.SelectedObject with
        | null -> (Unhandled, world)
        | :? EntityTypeDescriptorSource as entityTds ->
            if event.Publisher <> entityTds.Address then (Unhandled, world)
            else
                form.propertyGrid.SelectedObject <- null
                let editorState = { (world.ExtData :?> EditorState) with DragEntityState = DragEntityNone }
                (Unhandled, { world with ExtData = editorState })
        | _ -> failwith "Unexpected match failure in NuEdit.Program.handleNuEntityRemoving."

    let handleFormExit (form : NuEditForm) (_ : EventArgs) =
        form.Close ()
    
    let handleFormTreeViewNodeSelect (form : NuEditForm) (worldChangers : WorldChangers) (refWorld : World ref) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let entityAddress = addr form.treeView.SelectedNode.Name
            match entityAddress.AddrList with
            | [_; _; _] ->
                refWorld := world // must be set for property grid
                let entityTds = { Address = entityAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                form.propertyGrid.SelectedObject <- entityTds
                world
            | _ -> world) // not an entity address

    let handleFormCreate atMouse (form : NuEditForm) (worldChangers : WorldChangers) refWorld (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            try let entityXDispatcherName = form.createEntityComboBox.Text
                let entity = Entity.makeDefault entityXDispatcherName None world
                let world = pushPastWorld world world
                let (positionSnap, rotationSnap) = getSnaps form
                let mousePosition = World.getMousePositionF world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world entity
                let entityPosition = if atMouse then mousePositionEntity else world.Camera.EyeCenter
                let entityTransform = { Transform.Position = entityPosition; Depth = getCreationDepth form; Size = entity.Size; Rotation = entity.Rotation }
                let entity = Entity.setTransform positionSnap rotationSnap entityTransform entity
                let entityAddress = addrlist EditorGroupAddress [entity.Name]
                let world = World.addEntity entityAddress entity world
                refWorld := world // must be set for property grid
                form.propertyGrid.SelectedObject <- { Address = entityAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                world
            with exn -> ignore <| MessageBox.Show (string exn); world)

    let handleFormDelete (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let selectedObject = form.propertyGrid.SelectedObject
            let world = pushPastWorld world world
            match selectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = World.removeEntity entityTds.Address world
                form.propertyGrid.SelectedObject <- null
                world
            | _ -> world)

    let handleFormSave (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let saveFileResult = form.saveFileDialog.ShowDialog form
            match saveFileResult with
            | DialogResult.OK -> trySaveFile form.saveFileDialog.FileName world; world
            | _ -> world)

    let handleFormOpen (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let openFileResult = form.openFileDialog.ShowDialog form
            match openFileResult with
            | DialogResult.OK ->
                let world = tryLoadFile form form.openFileDialog.FileName world
                let world = clearOtherWorlds world
                form.propertyGrid.SelectedObject <- null
                form.interactivityButton.Checked <- false
                world
            | _ -> world)

    let handleFormUndo (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let editorState = world.ExtData :?> EditorState
            match editorState.PastWorlds with
            | [] -> world
            | pastWorld :: pastWorlds ->
                let futureWorld = world
                let world = World.continueHack EditorGroupAddress pastWorld
                let editorState = { editorState with PastWorlds = pastWorlds; FutureWorlds = futureWorld :: editorState.FutureWorlds }
                let world = { world with ExtData = editorState; Interactivity = GuiAndPhysics }
                refreshFormOnUndoRedo form world
                world)

    let handleFormRedo (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let editorState = world.ExtData :?> EditorState
            match editorState.FutureWorlds with
            | [] -> world
            | futureWorld :: futureWorlds ->
                let pastWorld = world
                let world = World.continueHack EditorGroupAddress futureWorld
                let editorState = { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = futureWorlds }
                let world = { world with ExtData = editorState; Interactivity = GuiAndPhysics }
                refreshFormOnUndoRedo form world
                world)

    let handleFormInteractivityChanged (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            // TODO: enable disabling of physics as well
            let interactivity = if form.interactivityButton.Checked then GuiAndPhysicsAndGamePlay else GuiAndPhysics
            let pastWorld = world
            let world = { world with Interactivity = interactivity }
            if Interactivity.isGamePlaying interactivity then pushPastWorld pastWorld world
            else world)

    let handleFormCut (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let optEntityTds = form.propertyGrid.SelectedObject
            match optEntityTds with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let editorState = world.ExtData :?> EditorState
                let entity = World.getEntity entityTds.Address world
                let world = World.removeEntity entityTds.Address world
                editorState.Clipboard := Some entity
                form.propertyGrid.SelectedObject <- null
                world
            | _ -> trace <| "Invalid cut operation (likely a code issue in NuEdit)."; world)
        
    let handleFormCopy (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let optEntityTds = form.propertyGrid.SelectedObject
            match optEntityTds with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = World.getEntity entityTds.Address world
                let editorState = world.ExtData :?> EditorState
                editorState.Clipboard := Some entity
                world
            | _ -> trace <| "Invalid copy operation (likely a code issue in NuEdit)."; world)

    let handleFormPaste atMouse (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let editorState = world.ExtData :?> EditorState
            match !editorState.Clipboard with
            | None -> world
            | Some entity ->
                let world = pushPastWorld world world
                let (positionSnap, rotationSnap) = getSnaps form
                let id = NuCore.makeId ()
                let entity = { entity with Id = id; Name = string id }
                let entityPosition =
                    if atMouse
                    then Entity.mouseToEntity editorState.RightClickPosition world entity
                    else world.Camera.EyeCenter
                let entityTransform = { Entity.getTransform entity with Position = entityPosition }
                let entity = Entity.setTransform positionSnap rotationSnap entityTransform entity
                let address = addrlist EditorGroupAddress [entity.Name]
                World.addEntity address entity world)

    let handleFormQuickSize (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let optEntityTds = form.propertyGrid.SelectedObject
            match optEntityTds with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let entity = World.getEntity entityTds.Address world
                let entity = Entity.setSize (Entity.getQuickSize entity world) entity
                let world = World.setEntity entityTds.Address entity world
                let world = Entity.propagatePhysics entityTds.Address entity world
                entityTds.RefWorld := world // must be set for property grid
                form.propertyGrid.Refresh ()
                world
            | _ -> trace <| "Invalid quick size operation (likely a code issue in NuEdit)."; world)

    let handleFormResetCamera (_ : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world -> // XXX
            let camera = { world.Camera with EyeCenter = Vector2.Zero } // XXX
            { world with Camera = camera })

    let handleFormAddXField (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            match (form.xFieldNameTextBox.Text, form.typeNameTextBox.Text) with
            | ("", _) -> ignore <| MessageBox.Show "Enter an XField name."; world
            | (_, "") -> ignore <| MessageBox.Show "Enter a type name."; world
            | (xFieldName, typeName) ->
                match tryFindType typeName with
                | None -> ignore <| MessageBox.Show "Enter a valid type name."; world
                | Some aType ->
                    let selectedObject = form.propertyGrid.SelectedObject
                    match selectedObject with
                    | :? EntityTypeDescriptorSource as entityTds ->
                        let world = pushPastWorld world world
                        let entity = World.getEntity entityTds.Address world
                        let xFieldValue =
                            if aType = typeof<string> then String.Empty :> obj
                            else Activator.CreateInstance aType
                        let xFields = Map.add xFieldName xFieldValue entity.Xtension.XFields
                        let entity = { entity with Xtension = { entity.Xtension with XFields = xFields }}
                        let world = World.setEntity entityTds.Address entity world
                        entityTds.RefWorld := world // must be set for property grid
                        form.propertyGrid.Refresh ()
                        form.propertyGrid.Select ()
                        form.propertyGrid.SelectedGridItem <- form.propertyGrid.SelectedGridItem.Parent.GridItems.[xFieldName]
                        world
                    | _ -> ignore <| MessageBox.Show "Select an entity to add the XField to."; world)

    let handleFormRemoveSelectedXField (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let selectedObject = form.propertyGrid.SelectedObject
            match selectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                match form.propertyGrid.SelectedGridItem.Label with
                | "" -> ignore <| MessageBox.Show "Select an XField."; world
                | xFieldName ->
                    let world = pushPastWorld world world
                    let entity = World.getEntity entityTds.Address world
                    let xFields = Map.remove xFieldName entity.Xtension.XFields
                    let entity = { entity with Xtension = { entity.Xtension with XFields = xFields }}
                    let world = World.setEntity entityTds.Address entity world
                    entityTds.RefWorld := world // must be set for property grid
                    form.propertyGrid.Refresh ()
                    world
            | _ -> ignore <| MessageBox.Show "Select an entity to remove an XField from."; world)

    let handleClearAllXFields (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let selectedObject = form.propertyGrid.SelectedObject
            match selectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let entity = World.getEntity entityTds.Address world
                let entity = { entity with Xtension = { entity.Xtension with XFields = Map.empty }}
                let world = World.setEntity entityTds.Address entity world
                entityTds.RefWorld := world // must be set for property grid
                form.propertyGrid.Refresh ()
                world
            | _ -> ignore <| MessageBox.Show "Select an entity to clear all XFields from."; world)

    let handleFormReloadAssets (_ : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let targetDirectory = (world.ExtData :?> EditorState).TargetDirectory
            let assetSourceDirectory = Path.Combine (targetDirectory, "..\\..")
            match World.tryReloadAssets assetSourceDirectory targetDirectory world with
            | Right world -> world
            | Left error ->
                ignore <|
                    MessageBox.Show
                        ("Asset reload error due to: " + error + "'.",
                         "Asset reload error.",
                         MessageBoxButtons.OK,
                         MessageBoxIcon.Error)
                world)

    let handleFormReloadOverlays (_ : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let world = pushPastWorld world world
            let targetDirectory = (world.ExtData :?> EditorState).TargetDirectory
            let overlayDirectory = Path.Combine (targetDirectory, "..\\..")
            match World.tryReloadOverlays overlayDirectory targetDirectory world with
            | Right world -> world
            | Left error ->
                ignore <|
                    MessageBox.Show
                        ("Overlay reload error due to: " + error + "'.",
                         "Overlay reload error.",
                         MessageBoxButtons.OK,
                         MessageBoxIcon.Error)
                world)

    let updateEntityDrag (form : NuEditForm) world =
        if canEditWithMouse form world then world
        else
            let editorState = world.ExtData :?> EditorState
            match editorState.DragEntityState with
            | DragEntityNone -> world
            | DragEntityPosition (pickOffset, mousePositionEntityOrig, address) ->
                let (positionSnap, _) = getSnaps form
                let entity = World.getEntity address world
                let mousePosition = World.getMousePositionF world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world entity
                let entityPosition = (pickOffset - mousePositionEntityOrig) + (mousePositionEntity - mousePositionEntityOrig)
                let entity = Entity.setPositionSnapped positionSnap entityPosition entity
                let world = World.setEntity address entity world
                let editorState = { editorState with DragEntityState = DragEntityPosition (pickOffset, mousePositionEntityOrig, address) }
                let world = { world with ExtData = editorState }
                let world = Entity.propagatePhysics address entity world
                form.propertyGrid.Refresh ()
                world
            | DragEntityRotation _ -> world

    let updateCameraDrag (_ : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        match editorState.DragCameraState with
        | DragCameraNone -> world
        | DragCameraPosition (pickOffset, mousePositionScreenOrig) ->
            let mousePosition = World.getMousePositionF world
            let mousePositionScreen = Camera.mouseToScreen mousePosition world.Camera
            let eyeCenter = (pickOffset - mousePositionScreenOrig) + -CameraSpeed * (mousePositionScreen - mousePositionScreenOrig)
            let camera = { world.Camera with EyeCenter = eyeCenter }
            let world = { world with Camera = camera }
            let editorState = { editorState with DragCameraState = DragCameraPosition (pickOffset, mousePositionScreenOrig) }
            { world with ExtData = editorState }

    // TODO: remove code duplication with below
    let updateUndoButton (form : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty editorState.PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not <| List.isEmpty editorState.PastWorlds then
            form.undoButton.Enabled <- true
            form.undoToolStripMenuItem.Enabled <- true

    let updateRedoButton (form : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        if form.redoToolStripMenuItem.Enabled then
            if List.isEmpty editorState.FutureWorlds then
                form.redoButton.Enabled <- false
                form.redoToolStripMenuItem.Enabled <- false
        elif not <| List.isEmpty editorState.FutureWorlds then
            form.redoButton.Enabled <- true
            form.redoToolStripMenuItem.Enabled <- true

    let updateEditorWorld form (worldChangers : WorldChangers) world =
        let worldChangersCopy = List.ofSeq worldChangers
        worldChangers.Clear ()
        let world = Seq.fold (fun world worldChanger -> worldChanger world) world worldChangersCopy
        let world = updateEntityDrag form world
        let world = updateCameraDrag form world
        updateUndoButton form world
        updateRedoButton form world
        { world with Liveness = if form.IsDisposed then Exiting else Running }

    let selectTargetDirectoryAndMakeGameDispatcher () =
        use openDialog = new OpenFileDialog ()
        openDialog.Filter <- "Executable Files (*.exe)|*.exe"
        openDialog.Title <- "Select your game's executable file to make its assets and XDispatchers available in the editor (or cancel for defaults)."
        if openDialog.ShowDialog () <> DialogResult.OK then (".", GameDispatcher ())
        else
            let directoryName = Path.GetDirectoryName openDialog.FileName
            Directory.SetCurrentDirectory directoryName
            let assembly = Assembly.LoadFrom openDialog.FileName
            let optDispatcherType = assembly.GetTypes () |> Array.tryFind (fun aType -> aType.IsSubclassOf typeof<GameDispatcher>)
            match optDispatcherType with
            | None -> (".", GameDispatcher ())
            | Some aType ->
                let gameDispatcher = Activator.CreateInstance aType :?> GameDispatcher
                (directoryName, gameDispatcher)

    let createNuEditForm worldChangers refWorld =
        let form = new NuEditForm ()
        form.displayPanel.MaximumSize <- Drawing.Size (ResolutionX, ResolutionY)
        form.positionSnapTextBox.Text <- string DefaultPositionSnap
        form.rotationSnapTextBox.Text <- string DefaultRotationSnap
        form.creationDepthTextBox.Text <- string DefaultCreationDepth
        // shitty hack to make Ctrl+Whatever work while manipulating the scene - probably not
        // necessary if we can figure out how to keep SDL from stealing input events...
        form.displayPanel.MouseClick.Add (fun _ -> ignore <| form.createEntityComboBox.Focus ())
        form.exitToolStripMenuItem.Click.Add (handleFormExit form)
        form.treeView.AfterSelect.Add (handleFormTreeViewNodeSelect form worldChangers refWorld)
        form.createEntityButton.Click.Add (handleFormCreate false form worldChangers refWorld)
        form.createToolStripMenuItem.Click.Add (handleFormCreate false form worldChangers refWorld)
        form.createContextMenuItem.Click.Add (handleFormCreate true form worldChangers refWorld)
        form.deleteEntityButton.Click.Add (handleFormDelete form worldChangers)
        form.deleteToolStripMenuItem.Click.Add (handleFormDelete form worldChangers)
        form.deleteContextMenuItem.Click.Add (handleFormDelete form worldChangers)
        form.saveToolStripMenuItem.Click.Add (handleFormSave form worldChangers)
        form.openToolStripMenuItem.Click.Add (handleFormOpen form worldChangers)
        form.undoButton.Click.Add (handleFormUndo form worldChangers)
        form.undoToolStripMenuItem.Click.Add (handleFormUndo form worldChangers)
        form.redoButton.Click.Add (handleFormRedo form worldChangers)
        form.redoToolStripMenuItem.Click.Add (handleFormRedo form worldChangers)
        form.interactivityButton.CheckedChanged.Add (handleFormInteractivityChanged form worldChangers)
        form.cutToolStripMenuItem.Click.Add (handleFormCut form worldChangers)
        form.cutContextMenuItem.Click.Add (handleFormCut form worldChangers)
        form.copyToolStripMenuItem.Click.Add (handleFormCopy form worldChangers)
        form.copyContextMenuItem.Click.Add (handleFormCopy form worldChangers)
        form.pasteToolStripMenuItem.Click.Add (handleFormPaste false form worldChangers)
        form.pasteContextMenuItem.Click.Add (handleFormPaste true form worldChangers)
        form.quickSizeToolStripButton.Click.Add (handleFormQuickSize form worldChangers)
        form.resetCameraButton.Click.Add (handleFormResetCamera form worldChangers)
        form.addXFieldButton.Click.Add (handleFormAddXField form worldChangers)
        form.removeSelectedXFieldButton.Click.Add (handleFormRemoveSelectedXField form worldChangers)
        form.reloadAssetsButton.Click.Add (handleFormReloadAssets form worldChangers)
        form.reloadOverlaysButton.Click.Add (handleFormReloadOverlays form worldChangers)
        form.Show ()
        form

    let tryMakeEditorWorld targetDirectory gameDispatcher form worldChangers refWorld sdlDeps =
        let screen = Screen.makeDissolve typeof<ScreenDispatcher>.Name 100L 100L
        let editorState =
            { TargetDirectory = targetDirectory
              RightClickPosition = Vector2.Zero
              DragEntityState = DragEntityNone
              DragCameraState = DragCameraNone
              PastWorlds = []
              FutureWorlds = []
              Clipboard = ref None }
        let optWorld = World.tryMakeEmpty sdlDeps gameDispatcher GuiAndPhysics true editorState
        match optWorld with
        | Right world ->
            let world = World.addScreen EditorScreenAddress screen [(EditorGroupName, Group.makeDefault typeof<GroupDispatcher>.Name world, [])] world
            let world = World.setOptSelectedScreenAddress (Some EditorScreenAddress) world 
            let world = World.subscribe4 DownMouseRightEventName Address.empty (CustomSub <| handleNuDownMouseRight form worldChangers refWorld) world
            let world = World.subscribe4 DownMouseLeftEventName Address.empty (CustomSub <| handleNuBeginEntityDrag form worldChangers refWorld) world
            let world = World.subscribe4 UpMouseLeftEventName Address.empty (CustomSub <| handleNuEndEntityDrag form) world
            let world = World.subscribe4 DownMouseCenterEventName Address.empty (CustomSub <| handleNuBeginCameraDrag form) world
            let world = World.subscribe4 UpMouseCenterEventName Address.empty (CustomSub <| handleNuBeginEndCameraDrag form) world
            let world = World.subscribe4 (AddEventName + EditorGroupAddress + AnyEventName) Address.empty (CustomSub <| handleNuEntityAdd form) world
            let world = World.subscribe4 (RemovingEventName + EditorGroupAddress + AnyEventName) Address.empty (CustomSub <| handleNuEntityRemoving form) world
            Right world
        | Left errorMsg -> Left errorMsg

    let tryCreateEditorWorld targetDirectory gameDispatcher form worldChangers refWorld sdlDeps =
        match tryMakeEditorWorld targetDirectory gameDispatcher form worldChangers refWorld sdlDeps with
        | Right world ->
            populateCreateComboBox form world
            populateTreeViewGroups form world
            Right world
        | Left _ as left -> left

    let [<EntryPoint; STAThread>] main _ =
        World.init ()
        let worldChangers = WorldChangers ()
        let refWorld = ref Unchecked.defaultof<World>
        let (targetDirectory, gameDispatcher) = selectTargetDirectoryAndMakeGameDispatcher ()
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
            (fun sdlDeps ->
                match tryCreateEditorWorld targetDirectory gameDispatcher form worldChangers refWorld sdlDeps with
                | Right world as right ->
                    refWorld := world
                    right
                | Left _ as left -> left)
            (fun world ->
                let world = updateEditorWorld form worldChangers world
                refWorld := world
                world)
            (fun world ->
                form.displayPanel.Invalidate ()
                world)
            sdlConfig