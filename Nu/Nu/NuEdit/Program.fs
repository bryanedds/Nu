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
open Nu.Constants
open Nu.WorldConstants
open NuEdit.Constants
open NuEdit.Reflection

[<AutoOpen>]
module ProgramModule =

    // TODO: increase warning level to 5.
    // TODO: implement entity freezing, then quick size on create.
    // TODO: implement selection box rendering.

    type WorldChanger = World -> World

    type WorldChangers = WorldChanger List

    type DragEntityState =
        | DragEntityPosition of Vector2 * Vector2 * Entity Address
        | DragEntityRotation of Vector2 * Vector2 * Entity Address
        | DragEntityNone

    type DragCameraState =
        | DragCameraPosition of Vector2 * Vector2
        | DragCameraNone

    type EditorState =
        { TargetDirectory : string
          RefinementDirectory : string
          GroupAddress : Group Address
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
        let groupAddress = (World.getUserState world).GroupAddress
        let entityMap = World.getEntities groupAddress world
        Map.toValueList entityMap

    let pushPastWorld pastWorld world =
        World.transformUserState
            (fun editorState -> { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = [] })
            world

    let clearOtherWorlds world =
        World.transformUserState (fun editorState -> { editorState with PastWorlds = []; FutureWorlds = [] }) world

    type [<TypeDescriptionProvider (typeof<EntityTypeDescriptorProvider>)>] EntityTypeDescriptorSource =
        { Address : Entity Address
          Form : NuEditForm
          WorldChangers : WorldChangers
          RefWorld : World ref }

    and EntityPropertyDescriptor (property, attributes) =
        inherit PropertyDescriptor
            ((match property with
              | EntityXFieldDescriptor x -> x.FieldName
              | EntityPropertyInfo p -> p.Name),
             attributes)

        let propertyName = match property with EntityXFieldDescriptor x -> x.FieldName | EntityPropertyInfo p -> p.Name
        let propertyType = match property with EntityXFieldDescriptor x -> x.FieldType | EntityPropertyInfo p -> p.PropertyType
        let propertyCanWrite = match property with EntityXFieldDescriptor _ -> true | EntityPropertyInfo x -> x.CanWrite

        /// Synchonize an entity after an overlay change.
        let synchronizeEntity oldOptOverlayName address (entity : Entity) world =
            match entity.OptOverlayName with
            | Some overlayName ->
                let entity = { entity with Id = entity.Id } // hacky copy for in-place mutation
                let (entity, world) =
                    let oldFacetNames = entity.FacetNames
                    let overlayer = world.State.Overlayer
                    Overlayer.applyOverlayToFacetNames oldOptOverlayName overlayName entity overlayer overlayer
                    match World.trySynchronizeFacets oldFacetNames (Some address) entity world with
                    | Right (entity, world) -> (entity, world)
                    | Left error -> debug error; (entity, world)
                Overlayer.applyOverlay oldOptOverlayName overlayName entity world.State.Overlayer
                let world = World.setEntity address entity world
                (entity, world)
            | None -> (entity, world)

        override this.ComponentType = propertyType.DeclaringType
        override this.PropertyType = propertyType
        override this.CanResetValue _ = false
        override this.ResetValue _ = ()
        override this.ShouldSerializeValue _ = true

        override this.IsReadOnly =
            not propertyCanWrite ||
            not <| Serialization.isPropertyPersistentByName propertyName

        override this.GetValue optSource =
            match optSource with
            | null -> null
            | source ->
                let entityTds = source :?> EntityTypeDescriptorSource
                let entity = World.getEntity entityTds.Address !entityTds.RefWorld
                getEntityPropertyValue property entity

        override this.SetValue (source, value) =
            
            // grab the type descriptor and assign the value
            let entityTds = source :?> EntityTypeDescriptorSource
            let changer = (fun world ->
                let world = pushPastWorld world world
                match propertyName with
                | "Name" ->
                    let valueStr = acstring value
                    if Int64.TryParse (valueStr, ref 0L) then
                        trace <| "Invalid entity name '" + valueStr + "' (must not be a number)."
                        world
                    else
                        let entity = World.getEntity entityTds.Address world
                        let (entity, world) = World.removeEntityImmediate entityTds.Address entity world
                        let entity = { entity with Name = valueStr }
                        let groupAddress = (World.getUserState world).GroupAddress
                        let entityAddress = gatoea groupAddress valueStr
                        let world = snd <| World.addEntity entityAddress entity world
                        entityTds.RefWorld := world // must be set for property grid
                        entityTds.Form.propertyGrid.SelectedObject <- { entityTds with Address = entityAddress }
                        world
                | "FacetNames" ->
                    let facetNames = value :?> obj list |> List.map (fun obj -> obj :?> string)
                    let entity = World.getEntity entityTds.Address world
                    let world =
                        match World.trySetFacetNames entity.FacetNames facetNames (Some entityTds.Address) entity world with
                        | Right (_, world) -> world
                        | Left error -> trace error; world
                    entityTds.RefWorld := world // must be set for property grid
                    entityTds.Form.propertyGrid.Refresh ()
                    world
                | _ ->
                    let entity = World.getEntity entityTds.Address world
                    let (entity, world) =
                        match propertyName with
                        | "OptOverlayName" ->
                            let oldOptOverlayName = entity.OptOverlayName
                            let entity = setEntityPropertyValue property value entity
                            let (entity, world) = synchronizeEntity oldOptOverlayName entityTds.Address entity world
                            let world = World.setEntity entityTds.Address entity world
                            (entity, world)
                        | _ ->
                            let entity = setEntityPropertyValue property value entity
                            let world = World.setEntity entityTds.Address entity world
                            (entity, world)
                    let world = Entity.propagatePhysics entityTds.Address entity world
                    entityTds.RefWorld := world // must be set for property grid
                    entityTds.Form.propertyGrid.Refresh ()
                    world)

            // NOTE: in order to update the view immediately, we have to apply the changer twice,
            // once immediately and once in the update function
            entityTds.RefWorld := changer !entityTds.RefWorld
            ignore <| entityTds.WorldChangers.Add changer

        // NOTE: This has to be a static member in order to see the relevant types in the recursive definitions.
        static member GetPropertyDescriptors (aType : Type) optSource =
            // OPTIMIZATION: seqs used for speed.
            let properties = aType.GetProperties ()
            let typeConverterAttribute = TypeConverterAttribute (typeof<AlgebraicConverter>) // TODO: make this static?
            let optXtensionProperty = Seq.tryFind (fun (property : PropertyInfo) -> property.PropertyType = typeof<Xtension>) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.PropertyType <> typeof<Xtension>) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Seq.isEmpty <| property.GetCustomAttributes<ExtensionAttribute> ()) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Serialization.isPropertyPersistentByName property.Name) properties
            let propertyDescriptors = Seq.map (fun property -> EntityPropertyDescriptor (EntityPropertyInfo property, [|typeConverterAttribute|]) :> PropertyDescriptor) properties
            let propertyDescriptors =
                match (optXtensionProperty, optSource) with
                | (None, _) 
                | (_, None) -> propertyDescriptors
                | (Some property, Some entity) ->
                    let xtension = property.GetValue entity :?> Xtension
                    let xFieldDescriptors =
                        Seq.fold
                            (fun xFieldDescriptors (xFieldKvp : KeyValuePair<string, XField>) ->
                                let fieldName = xFieldKvp.Key
                                let fieldType = xFieldKvp.Value.FieldType
                                if Serialization.isPropertyPersistentByName fieldName then
                                    let xFieldDescriptor = EntityXFieldDescriptor { FieldName = fieldName; FieldType = fieldType }
                                    let xFieldDescriptor = EntityPropertyDescriptor (xFieldDescriptor, [|typeConverterAttribute|])
                                    xFieldDescriptor :> PropertyDescriptor :: xFieldDescriptors
                                else xFieldDescriptors)
                            []
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
        ignore <| Single.TryParse (form.createDepthTextBox.Text, creationDepth)
        !creationDepth

    let getExpansionState (treeView : TreeView) =
        let nodeStates =
            Seq.fold
                (fun state (node : TreeNode) ->
                    if node.Nodes.Count = 0 then state
                    else (node.Name, node.IsExpanded) :: state)
                []
                (enumerable treeView.Nodes)
        Map.ofSeq nodeStates
        
    let restoreExpansionState (treeView : TreeView) treeState =
        Map.iter
            (fun nodeName nodeExpansion ->
                match treeView.Nodes.Find (nodeName, true) with
                | [||] -> ()
                | nodes ->
                    let node = nodes.[0]
                    if nodeExpansion then node.Expand ()
                    else node.Collapse ())
            treeState

    let addTreeViewNode (form : NuEditForm) entityAddress world =
        let entity = World.getEntity entityAddress world
        let entityGroupName = Reflection.getTypeName entity.DispatcherNp
        let treeGroup = form.treeView.Nodes.[entityGroupName]
        if not <| treeGroup.Nodes.ContainsKey (acstring entityAddress) then
            let treeNode = TreeNode entity.Name
            treeNode.Name <- acstring entityAddress
            ignore <| treeGroup.Nodes.Add treeNode
        else () // when changing an entity name, entity will be added twice - once from win forms, once from world

    let clearTreeViewNodes (form : NuEditForm) =
        form.treeView.Nodes.Clear ()

    let populateCreateComboBox (form : NuEditForm) world =
        form.createEntityComboBox.Items.Clear ()
        for dispatcherKvp in world.Components.EntityDispatchers do
            ignore <| form.createEntityComboBox.Items.Add dispatcherKvp.Key
        form.createEntityComboBox.SelectedIndex <- 0

    let populateTreeViewGroups (form : NuEditForm) world =
        for dispatcherKvp in world.Components.EntityDispatchers do
            let treeGroup = TreeNode dispatcherKvp.Key
            treeGroup.Name <- treeGroup.Text
            ignore <| form.treeView.Nodes.Add treeGroup

    let populateTreeViewNodes (form : NuEditForm) world =
        let groupAddress = (World.getUserState world).GroupAddress
        for entityKvp in World.getEntities groupAddress world do
        let entityAddress = gatoea groupAddress entityKvp.Key
        addTreeViewNode form entityAddress world

    let tryScrollTreeViewToPropertyGridSelection (form : NuEditForm) =
        match form.propertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.treeView.Nodes.Find (acstring entityTds.Address, true) with
            | [||] -> ()
            | nodes ->
                let node = nodes.[0]
                if node.Parent.IsExpanded then
                    form.treeView.SelectedNode <- node
                    node.EnsureVisible ()
                else form.treeView.SelectedNode <- null
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

    let selectEntity (form : NuEditForm) entityAddress worldChangers refWorld world =
        refWorld := world // must be set for property grid
        form.propertyGrid.SelectedObject <- { Address = entityAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
        tryScrollTreeViewToPropertyGridSelection form

    let canEditWithMouse (form : NuEditForm) world =
        World.isGamePlaying world &&
        not form.editWhileInteractiveCheckBox.Checked

    let tryMousePick (form : NuEditForm) mousePosition worldChangers refWorld world =
        let entities = getPickableEntities world
        let optPicked = Entity.tryPick mousePosition entities world
        match optPicked with
        | Some entity ->
            let groupAddress = (World.getUserState world).GroupAddress
            let entityAddress = gatoea groupAddress entity.Name
            selectEntity form entityAddress worldChangers refWorld world
            Some (entity, entityAddress)
        | None -> None

    let handleNuEntityAdd (form : NuEditForm) event world =
        addTreeViewNode form (atoea event.PublisherAddress) world
        (Cascade, world)

    let handleNuEntityRemoving (form : NuEditForm) event world =
        match form.treeView.Nodes.Find (acstring event.PublisherAddress, true) with
        | [||] -> () // when changing an entity name, entity will be removed twice - once from winforms, once from world
        | treeNodes -> form.treeView.Nodes.Remove treeNodes.[0]
        match form.propertyGrid.SelectedObject with
        | null -> (Cascade, world)
        | :? EntityTypeDescriptorSource as entityTds ->
            if event.PublisherAddress = atooa entityTds.Address then
                form.propertyGrid.SelectedObject <- null
                let world = World.transformUserState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                (Cascade, world)
            else (Cascade, world)
        | _ -> failwith "Unexpected match failure in NuEdit.Program.handleNuEntityRemoving."

    let handleNuMouseRightDown (form : NuEditForm) worldChangers refWorld (_ : MouseButtonData Event) world =
        let handled = if World.isGamePlaying world then Cascade else Resolve
        let mousePosition = World.getMousePositionF world
        ignore <| tryMousePick form mousePosition worldChangers refWorld world
        let world = World.transformUserState (fun editorState -> { editorState with RightClickPosition = mousePosition }) world
        (handled, world)

    let handleNuEntityDragBegin (form : NuEditForm) worldChangers refWorld (_ : MouseButtonData Event) world =
        if not <| canEditWithMouse form world then
            let handled = if World.isGamePlaying world then Cascade else Resolve
            let mousePosition = World.getMousePositionF world
            match tryMousePick form mousePosition worldChangers refWorld world with
            | Some (entity, entityAddress) ->
                let world = pushPastWorld world world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world entity
                let dragState = DragEntityPosition (entity.Position + mousePositionEntity, mousePositionEntity, entityAddress)
                let world = World.transformUserState (fun editorState -> { editorState with DragEntityState = dragState }) world
                (handled, world)
            | None -> (handled, world)
        else (Cascade, world)

    let handleNuEntityDragEnd (form : NuEditForm) (_ : MouseButtonData Event) world =
        if canEditWithMouse form world then (Cascade, world)
        else
            let handled = if World.isGamePlaying world then Cascade else Resolve
            let editorState = World.getUserState world
            match editorState.DragEntityState with
            | DragEntityPosition _
            | DragEntityRotation _ ->
                let editorState = { editorState with DragEntityState = DragEntityNone }
                let world = World.setUserState editorState world
                form.propertyGrid.Refresh ()
                (handled, world)
            | DragEntityNone -> (Resolve, world)

    let handleNuCameraDragBegin (_ : NuEditForm) (_ : MouseButtonData Event) world =
        let mousePosition = World.getMousePositionF world
        let mousePositionScreen = Camera.mouseToScreen mousePosition world.Camera
        let dragState = DragCameraPosition (world.Camera.EyeCenter + mousePositionScreen, mousePositionScreen)
        let world = World.transformUserState (fun editorState -> { editorState with DragCameraState = dragState }) world
        (Resolve, world)

    let handleNuCameraDragEnd (_ : NuEditForm) (_ : MouseButtonData Event) world =
        let editorState = World.getUserState world
        match editorState.DragCameraState with
        | DragCameraPosition _ ->
            let editorState = { editorState with DragCameraState = DragCameraNone }
            (Resolve, World.setUserState editorState world)
        | DragCameraNone -> (Resolve, world)

    let subscribeToEntityEvents form world =
        let groupAddress = (World.getUserState world).GroupAddress
        let world = World.subscribe AddEntityKey GameAddress (AddEventAddress ->>- groupAddress ->- AnyEventAddress) (handleNuEntityAdd form) world
        World.subscribe RemovingEntityKey GameAddress (RemovingEventAddress ->>- groupAddress ->- AnyEventAddress) (handleNuEntityRemoving form) world

    let unsubscribeFromEntityEvents world =
        let world = World.unsubscribe AddEntityKey world
        World.unsubscribe RemovingEntityKey world

    let trySaveFile filePath world =
        try let groupAddress = (World.getUserState world).GroupAddress
            let group = World.getGroup groupAddress world
            let entities = World.getEntities groupAddress world
            let groupHierarchy = (group, entities)
            World.writeGroupToFile filePath groupHierarchy world
        with exn ->
            ignore <|
                MessageBox.Show
                    ("Could not save file due to: " + acstring exn,
                     "File save error.",
                     MessageBoxButtons.OK,
                     MessageBoxIcon.Error)

    let tryLoadFile (form : NuEditForm) filePath world =
        
        try // remove current group
            let editorState = World.getUserState world
            let world = unsubscribeFromEntityEvents world
            let groupAddress = editorState.GroupAddress
            let group = World.getGroup groupAddress world
            let world = snd <| World.removeGroupImmediate groupAddress group world
            
            // load and add group
            let groupHierarchy = World.readGroupFromFile filePath world
            let groupAddress = satoga EditorScreenAddress <| (fst groupHierarchy).Name
            let editorState = { editorState with GroupAddress = groupAddress }
            let world = World.setUserState editorState world
            let world = snd <| World.addGroup groupAddress groupHierarchy world
            let world = subscribeToEntityEvents form world

            // refresh tree view
            refreshTreeView form world

            // update save file name
            form.saveFileDialog.FileName <- Path.GetFileName filePath
            world

        // handle load failure
        with exn ->
            ignore <|
                MessageBox.Show
                    ("Could not load file due to: " + acstring exn,
                     "File load error.",
                     MessageBoxButtons.OK,
                     MessageBoxIcon.Error)
            world

    let handleFormExit (form : NuEditForm) (_ : EventArgs) =
        form.Close ()

    let handleFormCreateDepthPlusClick (form : NuEditForm) (_ : EventArgs) =
        let depth = ref 0.0f
        ignore <| Single.TryParse (form.createDepthTextBox.Text, depth)
        form.createDepthTextBox.Text <- acstring (!depth + 1.0f)

    let handleFormCreateDepthMinusClick (form : NuEditForm) (_ : EventArgs) =
        let depth = ref 0.0f
        ignore <| Single.TryParse (form.createDepthTextBox.Text, depth)
        form.createDepthTextBox.Text <- acstring (!depth - 1.0f)
    
    let handleFormTreeViewNodeSelect (form : NuEditForm) (worldChangers : WorldChangers) (refWorld : World ref) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let entityAddress = stoa<Entity> form.treeView.SelectedNode.Name
            match entityAddress.Names with
            | [_; _; _] ->
                refWorld := world // must be set for property grid
                let entityTds = { Address = entityAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                form.propertyGrid.SelectedObject <- entityTds
                world
            | _ -> world) // not an entity address

    let handleFormCreate atMouse (form : NuEditForm) (worldChangers : WorldChangers) refWorld (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            try let groupAddress = (World.getUserState world).GroupAddress
                let entityDispatcherName = form.createEntityComboBox.Text
                let entity = World.makeEntity entityDispatcherName None world
                let world = pushPastWorld world world
                let (positionSnap, rotationSnap) = getSnaps form
                let mousePosition = World.getMousePositionF world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world entity
                let entityPosition = if atMouse then mousePositionEntity else world.Camera.EyeCenter
                let entityTransform = { Transform.Position = entityPosition; Depth = getCreationDepth form; Size = entity.Size; Rotation = entity.Rotation }
                let entity = Entity.setTransform positionSnap rotationSnap entityTransform entity
                let entityAddress = gatoea groupAddress entity.Name
                let world = snd <| World.addEntity entityAddress entity world
                refWorld := world // must be set for property grid
                form.propertyGrid.SelectedObject <- { Address = entityAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                world
            with exn -> ignore <| MessageBox.Show (acstring exn); world)

    let handleFormDelete (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let selectedObject = form.propertyGrid.SelectedObject
            let world = pushPastWorld world world
            match selectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = World.getEntity entityTds.Address world
                let world = snd <| World.removeEntity entityTds.Address entity world
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
            let editorState = World.getUserState world
            match editorState.PastWorlds with
            | [] -> world
            | pastWorld :: pastWorlds ->
                let futureWorld = world
                let world = World.continueHack editorState.GroupAddress pastWorld
                let editorState = { editorState with PastWorlds = pastWorlds; FutureWorlds = futureWorld :: editorState.FutureWorlds }
                let world = World.setUserState editorState world
                let world = World.setInteractivity GuiAndPhysics world
                refreshFormOnUndoRedo form world
                world)

    let handleFormRedo (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let editorState = World.getUserState world
            match editorState.FutureWorlds with
            | [] -> world
            | futureWorld :: futureWorlds ->
                let pastWorld = world
                let world = World.continueHack editorState.GroupAddress futureWorld
                let editorState = { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = futureWorlds }
                let world = World.setUserState editorState world
                let world = World.setInteractivity GuiAndPhysics world
                refreshFormOnUndoRedo form world
                world)

    let handleFormInteractivityChanged (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            // TODO: enable disabling of physics as well
            let interactivity = if form.interactivityButton.Checked then GuiAndPhysicsAndGamePlay else GuiAndPhysics
            let pastWorld = world
            let world = World.setInteractivity interactivity world
            if Interactivity.isGamePlaying interactivity then pushPastWorld pastWorld world
            else world)

    let handleFormCut (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let optEntityTds = form.propertyGrid.SelectedObject
            match optEntityTds with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let entity = World.getEntity entityTds.Address world
                let (entity, world) = World.removeEntity entityTds.Address entity world
                let world = World.transformUserState (fun editorState -> editorState.Clipboard := Some entity; editorState) world
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
                World.transformUserState (fun editorState -> editorState.Clipboard := Some entity; editorState) world
            | _ -> trace <| "Invalid copy operation (likely a code issue in NuEdit)."; world)

    let handleFormPaste atMouse (form : NuEditForm) (worldChangers : WorldChangers) refWorld (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let editorState = World.getUserState world
            match !editorState.Clipboard with
            | Some entity ->
                let world = pushPastWorld world world
                let (positionSnap, rotationSnap) = getSnaps form
                let id = Core.makeId ()
                let entity = { entity with Id = id; Name = acstring id }
                let entityPosition =
                    if atMouse
                    then Entity.mouseToEntity editorState.RightClickPosition world entity
                    else world.Camera.EyeCenter
                let entityTransform = { Entity.getTransform entity with Position = entityPosition }
                let entity = Entity.setTransform positionSnap rotationSnap entityTransform entity
                let entityAddress = gatoea editorState.GroupAddress entity.Name
                let world = snd <| World.addEntity entityAddress entity world
                selectEntity form entityAddress worldChangers refWorld world
                world
            | None -> world)

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
            World.setCamera camera world)

    let handleFormAddXField (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            match (form.xFieldNameTextBox.Text, form.typeNameTextBox.Text) with
            | ("", _) -> ignore <| MessageBox.Show "Enter an XField name."; world
            | (_, "") -> ignore <| MessageBox.Show "Enter a type name."; world
            | (xFieldName, typeName) ->
                match Type.TryGetTypeUnqualified typeName with
                | Some aType ->
                    let selectedObject = form.propertyGrid.SelectedObject
                    match selectedObject with
                    | :? EntityTypeDescriptorSource as entityTds ->
                        let world = pushPastWorld world world
                        let entity = World.getEntity entityTds.Address world
                        let xFieldValue = if aType = typeof<string> then String.Empty :> obj else Activator.CreateInstance aType
                        let xField = { FieldValue = xFieldValue; FieldType = aType }
                        let xFields = Map.add xFieldName xField entity.Xtension.XFields
                        let entity = { entity with Xtension = { entity.Xtension with XFields = xFields }}
                        let world = World.setEntity entityTds.Address entity world
                        entityTds.RefWorld := world // must be set for property grid
                        form.propertyGrid.Refresh ()
                        form.propertyGrid.Select ()
                        form.propertyGrid.SelectedGridItem <- form.propertyGrid.SelectedGridItem.Parent.GridItems.[xFieldName]
                        world
                    | _ -> ignore <| MessageBox.Show "Select an entity to add the XField to."; world
                | None -> ignore <| MessageBox.Show "Enter a valid type name."; world)

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
            let editorState = world.State.UserState :?> EditorState
            let targetDirectory = editorState.TargetDirectory
            let refinementDirectory = editorState.RefinementDirectory
            let assetSourceDirectory = Path.Combine (targetDirectory, "..\\..")
            match World.tryReloadAssets assetSourceDirectory targetDirectory refinementDirectory world with
            | Right world -> world
            | Left error ->
                ignore <|
                    MessageBox.Show
                        ("Asset reload error due to: " + error + "'.",
                         "Asset reload error.",
                         MessageBoxButtons.OK,
                         MessageBoxIcon.Error)
                world)

    let handleFormReloadOverlays (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let world = pushPastWorld world world
            let targetDirectory = (world.State.UserState :?> EditorState).TargetDirectory
            let overlayDirectory = Path.Combine (targetDirectory, "..\\..")
            match World.tryReloadOverlays overlayDirectory targetDirectory world with
            | Right world ->
                refreshPropertyGrid form world
                world
            | Left error ->
                ignore <|
                    MessageBox.Show
                        ("Overlay reload error due to: " + error + "'.",
                         "Overlay reload error.",
                         MessageBoxButtons.OK,
                         MessageBoxIcon.Error)
                world)

    let updateEntityDrag (form : NuEditForm) world =
        if not <| canEditWithMouse form world then
            let editorState = World.getUserState world
            match editorState.DragEntityState with
            | DragEntityPosition (pickOffset, mousePositionEntityOrig, address) ->
                let (positionSnap, _) = getSnaps form
                let entity = World.getEntity address world
                let mousePosition = World.getMousePositionF world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world entity
                let entityPosition = (pickOffset - mousePositionEntityOrig) + (mousePositionEntity - mousePositionEntityOrig)
                let entity = Entity.setPositionSnapped positionSnap entityPosition entity
                let world = World.setEntity address entity world
                let editorState = { editorState with DragEntityState = DragEntityPosition (pickOffset, mousePositionEntityOrig, address) }
                let world = World.setUserState editorState world
                let world = Entity.propagatePhysics address entity world
                form.propertyGrid.Refresh ()
                world
            | DragEntityRotation _ -> world
            | DragEntityNone -> world
        else world

    let updateCameraDrag (_ : NuEditForm) world =
        let editorState = World.getUserState world
        match editorState.DragCameraState with
        | DragCameraPosition (pickOffset, mousePositionScreenOrig) ->
            let mousePosition = World.getMousePositionF world
            let mousePositionScreen = Camera.mouseToScreen mousePosition world.Camera
            let eyeCenter = (pickOffset - mousePositionScreenOrig) + -CameraSpeed * (mousePositionScreen - mousePositionScreenOrig)
            let camera = { world.Camera with EyeCenter = eyeCenter }
            let world = World.setCamera camera world
            let editorState = { editorState with DragCameraState = DragCameraPosition (pickOffset, mousePositionScreenOrig) }
            World.setUserState editorState world
        | DragCameraNone -> world

    // TODO: remove code duplication with below
    let updateUndoButton (form : NuEditForm) world =
        let editorState = World.getUserState world
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty editorState.PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not <| List.isEmpty editorState.PastWorlds then
            form.undoButton.Enabled <- true
            form.undoToolStripMenuItem.Enabled <- true

    let updateRedoButton (form : NuEditForm) world =
        let editorState = World.getUserState world
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
        if form.IsDisposed then World.exit world
        else world
    let selectTargetDirectoryAndMakeUserComponentFactory () =
        use openDialog = new OpenFileDialog ()
        openDialog.Filter <- "Executable Files (*.exe)|*.exe"
        openDialog.Title <- "Select your game's executable file to make its assets and components available in the editor (or cancel for defaults)."
        if openDialog.ShowDialog () = DialogResult.OK then
            let directoryName = Path.GetDirectoryName openDialog.FileName
            Directory.SetCurrentDirectory directoryName
            let assembly = Assembly.LoadFrom openDialog.FileName
            let assemblyTypes = assembly.GetTypes ()
            let optDispatcherType =
                Array.tryFind
                    (fun (aType : Type) -> aType.IsSubclassOf typeof<UserComponentFactory>)
                    assemblyTypes
            match optDispatcherType with
            | Some aType ->
                let userComponentFactory = Activator.CreateInstance aType :?> UserComponentFactory
                (directoryName, userComponentFactory)
            | None -> (".", UserComponentFactory ())
        else (".", UserComponentFactory ())

    let createNuEditForm worldChangers refWorld =
        let form = new NuEditForm ()
        form.displayPanel.MaximumSize <- Drawing.Size (ResolutionX, ResolutionY)
        form.positionSnapTextBox.Text <- acstring DefaultPositionSnap
        form.rotationSnapTextBox.Text <- acstring DefaultRotationSnap
        form.createDepthTextBox.Text <- acstring DefaultCreationDepth
        form.exitToolStripMenuItem.Click.Add (handleFormExit form)
        form.createDepthPlusButton.Click.Add (handleFormCreateDepthPlusClick form)
        form.createDepthMinusButton.Click.Add (handleFormCreateDepthMinusClick form)
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
        form.pasteToolStripMenuItem.Click.Add (handleFormPaste false form worldChangers refWorld)
        form.pasteContextMenuItem.Click.Add (handleFormPaste true form worldChangers refWorld)
        form.quickSizeToolStripButton.Click.Add (handleFormQuickSize form worldChangers)
        form.resetCameraButton.Click.Add (handleFormResetCamera form worldChangers)
        form.addXFieldButton.Click.Add (handleFormAddXField form worldChangers)
        form.removeSelectedXFieldButton.Click.Add (handleFormRemoveSelectedXField form worldChangers)
        form.reloadAssetsButton.Click.Add (handleFormReloadAssets form worldChangers)
        form.reloadOverlaysButton.Click.Add (handleFormReloadOverlays form worldChangers)
        form.Show ()
        form

    let tryMakeEditorWorld targetDirectory refinementDirectory form worldChangers refWorld sdlDeps userComponentFactory =
        let groupAddress = satoga EditorScreenAddress DefaultGroupName
        let editorState =
            { TargetDirectory = targetDirectory
              RefinementDirectory = refinementDirectory
              GroupAddress = groupAddress
              RightClickPosition = Vector2.Zero
              DragEntityState = DragEntityNone
              DragCameraState = DragCameraNone
              PastWorlds = []
              FutureWorlds = []
              Clipboard = ref None }
        let eitherWorld = World.tryMake false true GuiAndPhysics editorState userComponentFactory sdlDeps
        match eitherWorld with
        | Right world ->
            let screen = World.makeScreen typeof<ScreenDispatcher>.Name (Some EditorScreenName) world
            let group = World.makeGroup typeof<GroupDispatcher>.Name (Some DefaultGroupName) world
            let groupsHierarchy = Map.singleton group.Name (group, Map.empty)
            let screenHierarchy = (screen, groupsHierarchy)
            let world = snd <| World.addScreen EditorScreenAddress screenHierarchy world
            let world = World.setOptSelectedScreenAddress (Some EditorScreenAddress) world 
            let world = World.subscribe4 GameAddress MouseRightDownEventAddress (handleNuMouseRightDown form worldChangers refWorld) world
            let world = World.subscribe4 GameAddress MouseLeftDownEventAddress (handleNuEntityDragBegin form worldChangers refWorld) world
            let world = World.subscribe4 GameAddress MouseLeftUpEventAddress (handleNuEntityDragEnd form) world
            let world = World.subscribe4 GameAddress MouseCenterDownEventAddress (handleNuCameraDragBegin form) world
            let world = World.subscribe4 GameAddress MouseCenterUpEventAddress (handleNuCameraDragEnd form) world
            let world = subscribeToEntityEvents form world
            Right world
        | Left errorMsg -> Left errorMsg

    let tryCreateEditorWorld targetDirectory refinementDirectory form worldChangers refWorld sdlDeps userComponentFactory =
        match tryMakeEditorWorld targetDirectory refinementDirectory form worldChangers refWorld sdlDeps userComponentFactory with
        | Right world ->
            populateCreateComboBox form world
            populateTreeViewGroups form world
            Right world
        | Left _ as left -> left

    let [<EntryPoint; STAThread>] main _ =
        World.init ()
        let worldChangers = WorldChangers ()
        let refWorld = ref Unchecked.defaultof<World>
        let (targetDirectory, userComponentFactory) = selectTargetDirectoryAndMakeUserComponentFactory ()
        let refinementDirectory = "Refinement"
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
                match tryCreateEditorWorld targetDirectory refinementDirectory form worldChangers refWorld sdlDeps userComponentFactory with
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