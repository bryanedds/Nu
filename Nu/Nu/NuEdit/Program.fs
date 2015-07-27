// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2015.

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
open NuEdit.Constants

// TODO: increase warning level to 5.
// TODO: implement entity freezing, then quick size on create.
// TODO: implement selection box rendering.

type WorldChanger = World -> World

type WorldChangers = WorldChanger List

type DragEntityState =
    | DragEntityPosition of Vector2 * Vector2 * Entity
    | DragEntityRotation of Vector2 * Vector2 * Entity
    | DragEntityNone

type DragCameraState =
    | DragCameraPosition of Vector2 * Vector2
    | DragCameraNone

type EditorState =
    { TargetDirectory : string
      RefinementDirectory : string
      RightClickPosition : Vector2
      DragEntityState : DragEntityState
      DragCameraState : DragCameraState
      PastWorlds : World list
      FutureWorlds : World list
      Clipboard : (Entity option) ref }

[<AutoOpen>]
module ProgramModule =

    let pushPastWorld pastWorld world =
        World.updateUserState
            (fun editorState -> { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = [] })
            world

type [<TypeDescriptionProvider (typeof<EntityTypeDescriptorProvider>)>] EntityTypeDescriptorSource =
    { DescribedEntity : Entity
      Form : NuEditForm
      WorldChangers : WorldChangers
      RefWorld : World ref }

and EntityPropertyDescriptor (property, attributes) =
    inherit PropertyDescriptor (
        (match property with EntityXFieldDescriptor xfd -> xfd.FieldName | EntityPropertyInfo pi -> pi.Name),
        attributes)

    let propertyName = match property with EntityXFieldDescriptor xfd -> xfd.FieldName | EntityPropertyInfo pi -> pi.Name
    let propertyType = match property with EntityXFieldDescriptor xfd -> xfd.FieldType | EntityPropertyInfo pi -> pi.PropertyType
    let propertyCanWrite = match property with EntityXFieldDescriptor _ -> true | EntityPropertyInfo xfd -> xfd.CanWrite

    override this.ComponentType = propertyType.DeclaringType
    override this.PropertyType = propertyType
    override this.CanResetValue _ = false
    override this.ResetValue _ = ()
    override this.ShouldSerializeValue _ = true

    override this.IsReadOnly =
        not propertyCanWrite ||
        not <| Reflection.isPropertyPersistentByName propertyName

    override this.GetValue source =
        match source with
        | null -> null // WHY THE FUCK IS THIS EVER null???
        | source ->
            let entityTds = source :?> EntityTypeDescriptorSource
            EntityMemberValue.getValue property entityTds.DescribedEntity !entityTds.RefWorld

    override this.SetValue (source, value) =
        
        // grab the type descriptor and assign the value
        let entityTds = source :?> EntityTypeDescriptorSource
        let changer = (fun world ->

            // TODO: comment
            let world = pushPastWorld world world
            match propertyName with
            | "Name" ->
                let valueStr = acstring value
                if fst <| Int64.TryParse valueStr then
                    trace <| "Invalid entity name '" + valueStr + "' (must not be a number)."
                    world
                else
                    let entity = entityTds.DescribedEntity
                    let (entity, world) = World.reassignEntity entity (Some valueStr) EditorGroup world
                    entityTds.RefWorld := world // must be set for property grid
                    entityTds.Form.propertyGrid.SelectedObject <- { entityTds with DescribedEntity = entity }
                    world

            // TODO: comment
            | "FacetNames" ->
                let facetNames = value :?> string list
                let entity = entityTds.DescribedEntity
                let world =
                    match World.trySetEntityFacetNames facetNames entity world with
                    | Right world -> world
                    | Left error -> trace error; world
                entityTds.RefWorld := world // must be set for property grid
                entityTds.Form.propertyGrid.Refresh ()
                world

            // TODO: comment
            | _ ->
                let entity = entityTds.DescribedEntity
                let world =
                    match propertyName with
                    | "OptOverlayName" ->
                        match World.trySetEntityOptOverlayName (value :?> string option) entity world with
                        | Right world -> world
                        | Left error -> trace error; world
                    | _ -> EntityMemberValue.setValue property value entity world
                let world = World.propagateEntityPhysics entityTds.DescribedEntity world
                entityTds.RefWorld := world // must be set for property grid
                entityTds.Form.propertyGrid.Refresh ()
                world)

        // NOTE: in order to update the view immediately, we have to apply the changer twice,
        // once immediately and once in the update function
        entityTds.RefWorld := changer !entityTds.RefWorld
        ignore <| entityTds.WorldChangers.Add changer

and EntityTypeDescriptor (optSource : obj) =
    inherit CustomTypeDescriptor ()
    override this.GetProperties _ =
        let optXtension =
            match optSource with
            | :? EntityTypeDescriptorSource as source -> Some (source.DescribedEntity.GetXtension !source.RefWorld)
            | _ -> None
        ignore optXtension
        let makePropertyDescriptor = fun (emv, tcas) -> (EntityPropertyDescriptor (emv, Array.map (fun attr -> attr :> Attribute) tcas)) :> PropertyDescriptor
        let propertyDescriptors = EntityMemberValue.getPropertyDescriptors makePropertyDescriptor optXtension
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

and EntityTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, optSource) = EntityTypeDescriptor optSource :> ICustomTypeDescriptor

module Program =

    let DefaultPositionSnap = 8
    let DefaultRotationSnap = 5
    let DefaultCreationDepth = 0.0f
    let CameraSpeed = 4.0f // NOTE: might be nice to be able to configure this just like entity creation depth in the editor

    let getPickableEntities world =
        World.proxyEntities EditorGroup world

    let clearOtherWorlds world =
        World.updateUserState (fun editorState -> { editorState with PastWorlds = []; FutureWorlds = [] }) world

    let getSnaps (form : NuEditForm) =
        let positionSnap = snd <| Int32.TryParse form.positionSnapTextBox.Text
        let rotationSnap = snd <| Int32.TryParse form.rotationSnapTextBox.Text
        (positionSnap, rotationSnap)
    
    let getCreationDepth (form : NuEditForm) =
        snd <| Single.TryParse form.createDepthTextBox.Text

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
                    if nodeExpansion
                    then node.Expand ()
                    else node.Collapse ())
            treeState

    let addTreeViewNode (form : NuEditForm) (entity : Entity) world =
        let entityCategoryName = Reflection.getTypeName <| entity.GetDispatcherNp world
        let treeCategory = form.treeView.Nodes.[entityCategoryName]
        let treeCategoryNodeName = acstring entity.EntityAddress
        if not <| treeCategory.Nodes.ContainsKey treeCategoryNodeName then
            let treeCategoryNode = TreeNode (entity.GetName world)
            treeCategoryNode.Name <- treeCategoryNodeName
            ignore <| treeCategory.Nodes.Add treeCategoryNode
        else () // when changing an entity name, entity will be added twice - once from win forms, once from world

    let clearTreeViewNodes (form : NuEditForm) =
        form.treeView.Nodes.Clear ()

    let populateCreateComboBox (form : NuEditForm) world =
        form.createEntityComboBox.Items.Clear ()
        for dispatcherKvp in World.getEntityDispatchers world do
            ignore <| form.createEntityComboBox.Items.Add dispatcherKvp.Key
        form.createEntityComboBox.SelectedIndex <- 0

    let populateTreeViewGroups (form : NuEditForm) world =
        for dispatcherKvp in World.getEntityDispatchers world do
            let treeGroup = TreeNode dispatcherKvp.Key
            treeGroup.Name <- treeGroup.Text
            ignore <| form.treeView.Nodes.Add treeGroup

    let populateTreeViewNodes (form : NuEditForm) world =
        for entity in World.proxyEntities EditorGroup world do
            addTreeViewNode form entity world

    let tryScrollTreeViewToPropertyGridSelection (form : NuEditForm) =
        match form.propertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.treeView.Nodes.Find (acstring entityTds.DescribedEntity.EntityAddress, true) with
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
            if World.containsEntity entityTds.DescribedEntity world
            then form.propertyGrid.Refresh ()
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
        form.tickingButton.Checked <- false
        refreshPropertyGrid form world
        refreshTreeView form world

    let selectEntity (form : NuEditForm) entity worldChangers refWorld world =
        refWorld := world // must be set for property grid
        form.propertyGrid.SelectedObject <- { DescribedEntity = entity; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
        tryScrollTreeViewToPropertyGridSelection form

    let canEditWithMouse (form : NuEditForm) world =
        World.isTicking world &&
        not form.editWhileInteractiveCheckBox.Checked

    let tryMousePick (form : NuEditForm) mousePosition worldChangers refWorld world =
        let entities = getPickableEntities world
        let optPicked = World.tryPickEntity mousePosition entities world
        match optPicked with
        | Some entity ->
            selectEntity form entity worldChangers refWorld world
            Some entity
        | None -> None

    let handleNuEntityAdd (form : NuEditForm) event world =
        addTreeViewNode form (Entity.proxy <| atoea event.Publisher.SimulantAddress) world
        (Cascade, world)

    let handleNuEntityRemoving (form : NuEditForm) event world =
        match form.treeView.Nodes.Find (acstring event.Publisher.SimulantAddress, true) with
        | [||] -> () // when changing an entity name, entity will be removed twice - once from winforms, once from world
        | treeNodes -> form.treeView.Nodes.Remove treeNodes.[0]
        match form.propertyGrid.SelectedObject with
        | null -> (Cascade, world)
        | :? EntityTypeDescriptorSource as entityTds ->
            if atoea event.Publisher.SimulantAddress = entityTds.DescribedEntity.EntityAddress then
                form.propertyGrid.SelectedObject <- null
                let world = World.updateUserState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                (Cascade, world)
            else (Cascade, world)
        | _ -> failwith "Unexpected match failure in NuEdit.Program.handleNuEntityRemoving."

    let handleNuMouseRightDown (form : NuEditForm) worldChangers refWorld (_ : Event<MouseButtonData, Game>) world =
        let handled = if World.isTicking world then Cascade else Resolve
        let mousePosition = World.getMousePositionF world
        ignore <| tryMousePick form mousePosition worldChangers refWorld world
        let world = World.updateUserState (fun editorState -> { editorState with RightClickPosition = mousePosition }) world
        (handled, world)

    let handleNuEntityDragBegin (form : NuEditForm) worldChangers refWorld (_ : Event<MouseButtonData, Game>) world =
        if not <| canEditWithMouse form world then
            let handled = if World.isTicking world then Cascade else Resolve
            let mousePosition = World.getMousePositionF world
            match tryMousePick form mousePosition worldChangers refWorld world with
            | Some entity ->
                let world = pushPastWorld world world
                let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (entity.GetViewType world) mousePosition) world
                let dragState = DragEntityPosition (entity.GetPosition world + mousePositionWorld, mousePositionWorld, entity)
                let world = World.updateUserState (fun editorState -> { editorState with DragEntityState = dragState }) world
                (handled, world)
            | None -> (handled, world)
        else (Cascade, world)

    let handleNuEntityDragEnd (form : NuEditForm) (_ : Event<MouseButtonData, Game>) world =
        if canEditWithMouse form world then (Cascade, world)
        else
            let handled = if World.isTicking world then Cascade else Resolve
            match (World.getUserState world).DragEntityState with
            | DragEntityPosition _
            | DragEntityRotation _ ->
                let world = World.updateUserState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                form.propertyGrid.Refresh ()
                (handled, world)
            | DragEntityNone -> (Resolve, world)

    let handleNuCameraDragBegin (_ : NuEditForm) (_ : Event<MouseButtonData, Game>) world =
        let camera = World.getCamera world
        let mousePosition = World.getMousePositionF world
        let mousePositionScreen = Camera.mouseToScreen mousePosition camera
        let dragState = DragCameraPosition (camera.EyeCenter + mousePositionScreen, mousePositionScreen)
        let world = World.updateUserState (fun editorState -> { editorState with DragCameraState = dragState }) world
        (Resolve, world)

    let handleNuCameraDragEnd (_ : NuEditForm) (_ : Event<MouseButtonData, Game>) world =
        match (World.getUserState world).DragCameraState with
        | DragCameraPosition _ ->
            let world = World.updateUserState (fun editorState -> { editorState with DragCameraState = DragCameraNone }) world
            (Resolve, world)
        | DragCameraNone -> (Resolve, world)

    let subscribeToEntityEvents form world =
        world |>
            World.subscribe AddEntityKey (handleNuEntityAdd form) (EventAddresses.EntityAdd ->>- EditorGroup.GroupAddress ->- EventAddresses.Any) Simulants.Game |>
            World.subscribe RemovingEntityKey (handleNuEntityRemoving form) (EventAddresses.EntityRemoving ->>- EditorGroup.GroupAddress ->- EventAddresses.Any) Simulants.Game

    let unsubscribeFromEntityEvents world =
        world |>
            World.unsubscribe AddEntityKey |>
            World.unsubscribe RemovingEntityKey

    let trySaveFile filePath world =
        try World.writeGroupToFile filePath EditorGroup world
        with exn ->
            ignore <|
                MessageBox.Show
                    ("Could not save file due to: " + acstring exn,
                     "File save error.",
                     MessageBoxButtons.OK,
                     MessageBoxIcon.Error)

    let tryLoadFile (form : NuEditForm) filePath world =

        try // destroy current group
            let world = unsubscribeFromEntityEvents world
            let world = World.destroyGroupImmediate EditorGroup world

            // load and add group
            let world = snd <| World.readGroupFromFile filePath (Some EditorGroupName) EditorScreen world
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
        let depth = snd <| Single.TryParse form.createDepthTextBox.Text
        form.createDepthTextBox.Text <- acstring <| depth + 1.0f

    let handleFormCreateDepthMinusClick (form : NuEditForm) (_ : EventArgs) =
        let depth = snd <| Single.TryParse form.createDepthTextBox.Text
        form.createDepthTextBox.Text <- acstring <| depth - 1.0f

    let handleFormTreeViewNodeSelect (form : NuEditForm) (worldChangers : WorldChangers) (refWorld : World ref) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let entity = Entity.proxy <| stoa form.treeView.SelectedNode.Name
            match Address.getNameKeys entity.EntityAddress with
            | [_; _; _] ->
                refWorld := world // must be set for property grid
                let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                form.propertyGrid.SelectedObject <- entityTds
                world
            | _ -> world) // not an entity address

    let handleFormCreate atMouse (form : NuEditForm) (worldChangers : WorldChangers) refWorld (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            try let world = pushPastWorld world world
                let (entity, world) = World.createEntity form.createEntityComboBox.Text None EditorGroup world
                let (positionSnap, rotationSnap) = getSnaps form
                let mousePosition = World.getMousePositionF world
                let camera = World.getCamera world
                let entityPosition =
                    if atMouse
                    then Camera.mouseToWorld (entity.GetViewType world) mousePosition camera
                    else Camera.mouseToWorld (entity.GetViewType world) (camera.EyeSize * 0.5f) camera
                let entityTransform =
                    { Transform.Position = entityPosition
                      Depth = getCreationDepth form
                      Size = entity.GetSize world
                      Rotation = entity.GetRotation world }
                let world = entity.SetTransformSnapped positionSnap rotationSnap entityTransform world
                let world = World.propagateEntityPhysics entity world
                refWorld := world // must be set for property grid
                let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                form.propertyGrid.SelectedObject <- entityTds
                world
            with exn -> ignore <| MessageBox.Show (acstring exn); world)

    let handleFormDelete (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let world = pushPastWorld world world
            match form.propertyGrid.SelectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = World.destroyEntity entityTds.DescribedEntity world
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
                form.tickingButton.Checked <- false
                world
            | _ -> world)

    let handleFormUndo (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            match (World.getUserState world).PastWorlds with
            | [] -> world
            | pastWorld :: pastWorlds ->
                let futureWorld = world
                let world = World.continueHack EditorGroup pastWorld
                let world =
                    World.updateUserState (fun editorState ->
                        { editorState with PastWorlds = pastWorlds; FutureWorlds = futureWorld :: (World.getUserState futureWorld).FutureWorlds })
                        world
                let world = World.setTickRate 0L world
                refreshFormOnUndoRedo form world
                world)

    let handleFormRedo (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            match (World.getUserState world).FutureWorlds with
            | [] -> world
            | futureWorld :: futureWorlds ->
                let pastWorld = world
                let world = World.continueHack EditorGroup futureWorld
                let world =
                    World.updateUserState (fun editorState ->
                        { editorState with PastWorlds = pastWorld :: (World.getUserState pastWorld).PastWorlds; FutureWorlds = futureWorlds })
                        world
                let world = World.setTickRate 0L world
                refreshFormOnUndoRedo form world
                world)

    let handleFormInteractivityChanged (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let tickRate = if form.tickingButton.Checked then 1L else 0L
            let (pastWorld, world) = (world, World.setTickRate tickRate world)
            if tickRate = 1L then pushPastWorld pastWorld world else world)

    (* TODO: replace this cut paste functionality with a reflective approach
    
    let handleFormCut (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let entity = World.getEntity entityTds.Address world
                let world = World.removeEntity entityTds.Address world
                let world = World.updateUserState (fun editorState -> editorState.Clipboard := Some entity; editorState) world
                form.propertyGrid.SelectedObject <- null
                world
            | _ -> trace <| "Invalid cut operation (likely a code issue in NuEdit)."; world)
        
    let handleFormCopy (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = World.getEntity entityTds.Address world
                World.updateUserState (fun editorState -> editorState.Clipboard := Some entity; editorState) world
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
                let camera = World.getCamera world
                let entityPosition =
                    if atMouse
                    then Camera.mouseToWorld entity.ViewType editorState.RightClickPosition camera
                    else Camera.mouseToWorld entity.ViewType (camera.EyeSize * 0.5f) camera
                let entityTransform = { Entity.getTransform entity with Position = entityPosition }
                let entity = Entity.setTransform positionSnap rotationSnap entityTransform entity
                let entityAddress = gatoea editorState.GroupAddress entity.Name
                let world = snd <| World.addEntityData entity entityAddress world
                selectEntity form entityAddress worldChangers refWorld world
                world
            | None -> world)*)

    let handleFormQuickSize (form : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let optEntityTds = form.propertyGrid.SelectedObject
            match optEntityTds with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let entity = entityTds.DescribedEntity
                let world = entity.SetSize (World.getEntityQuickSize entity world) world
                let world = World.propagateEntityPhysics entity world
                entityTds.RefWorld := world // must be set for property grid
                form.propertyGrid.Refresh ()
                world
            | _ -> trace <| "Invalid quick size operation (likely a code issue in NuEdit)."; world)

    let handleFormResetCamera (_ : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            World.updateCamera (fun camera -> { camera with EyeCenter = Vector2.Zero }) world)

    let handleFormReloadAssets (_ : NuEditForm) (worldChangers : WorldChangers) (_ : EventArgs) =
        ignore <| worldChangers.Add (fun world ->
            let editorState = World.getUserState world
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
            let targetDirectory = (World.getUserState world).TargetDirectory
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
            match (World.getUserState world).DragEntityState with
            | DragEntityPosition (pickOffset, mousePositionWorldOrig, entity) ->
                let (positionSnap, _) = getSnaps form
                let mousePosition = World.getMousePositionF world
                let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (entity.GetViewType world) mousePosition) world
                let entityPosition = (pickOffset - mousePositionWorldOrig) + (mousePositionWorld - mousePositionWorldOrig)
                let world = entity.SetPositionSnapped positionSnap entityPosition world
                let world = World.propagateEntityPhysics entity world
                let world =
                    World.updateUserState (fun editorState ->
                        { editorState with DragEntityState = DragEntityPosition (pickOffset, mousePositionWorldOrig, entity) })
                        world
                form.propertyGrid.Refresh ()
                world
            | DragEntityRotation _ -> world
            | DragEntityNone -> world
        else world

    let updateCameraDrag (_ : NuEditForm) world =
        match (World.getUserState world).DragCameraState with
        | DragCameraPosition (pickOffset, mousePositionScreenOrig) ->
            let world =
                World.updateCamera (fun camera ->
                    let mousePosition = World.getMousePositionF world
                    let mousePositionScreen = Camera.mouseToScreen mousePosition camera
                    let eyeCenter = (pickOffset - mousePositionScreenOrig) + -CameraSpeed * (mousePositionScreen - mousePositionScreenOrig)
                    { camera with EyeCenter = eyeCenter })
                    world
            World.updateUserState (fun editorState ->
                { editorState with DragCameraState = DragCameraPosition (pickOffset, mousePositionScreenOrig) })
                world
        | DragCameraNone -> world

    // TODO: remove code duplication with below
    let updateUndoButton (form : NuEditForm) world =
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty (World.getUserState world).PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not <| List.isEmpty (World.getUserState world).PastWorlds then
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
        if form.IsDisposed
        then World.exit world
        else world

    let selectTargetDirectoryAndMakeNuPlugin () =
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
                    (fun (aType : Type) -> aType.IsSubclassOf typeof<NuPlugin>)
                    assemblyTypes
            match optDispatcherType with
            | Some aType ->
                let nuPlugin = Activator.CreateInstance aType :?> NuPlugin
                (directoryName, nuPlugin)
            | None -> (".", NuPlugin ())
        else (".", NuPlugin ())

    let createNuEditForm worldChangers refWorld =
        let form = new NuEditForm ()
        form.displayPanel.MaximumSize <- Drawing.Size (Constants.Render.ResolutionX, Constants.Render.ResolutionY)
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
        form.tickingButton.CheckedChanged.Add (handleFormInteractivityChanged form worldChangers)
        (*form.cutToolStripMenuItem.Click.Add (handleFormCut form worldChangers)
        form.cutContextMenuItem.Click.Add (handleFormCut form worldChangers)
        form.copyToolStripMenuItem.Click.Add (handleFormCopy form worldChangers)
        form.copyContextMenuItem.Click.Add (handleFormCopy form worldChangers)
        form.pasteToolStripMenuItem.Click.Add (handleFormPaste false form worldChangers refWorld)
        form.pasteContextMenuItem.Click.Add (handleFormPaste true form worldChangers refWorld)*)
        form.quickSizeToolStripButton.Click.Add (handleFormQuickSize form worldChangers)
        form.resetCameraButton.Click.Add (handleFormResetCamera form worldChangers)
        form.reloadAssetsButton.Click.Add (handleFormReloadAssets form worldChangers)
        form.reloadOverlaysButton.Click.Add (handleFormReloadOverlays form worldChangers)
        form.Show ()
        form

    let tryMakeEditorWorld targetDirectory refinementDirectory form worldChangers refWorld sdlDeps nuPlugin =
        let editorState =
            { TargetDirectory = targetDirectory
              RefinementDirectory = refinementDirectory
              RightClickPosition = Vector2.Zero
              DragEntityState = DragEntityNone
              DragCameraState = DragCameraNone
              PastWorlds = []
              FutureWorlds = []
              Clipboard = ref None }
        let eitherWorld = World.tryMake false 0L editorState nuPlugin sdlDeps
        match eitherWorld with
        | Right world ->
            let world = snd <| World.createScreen typeof<ScreenDispatcher>.Name (Some EditorScreenName) world
            let world = snd <| World.createGroup typeof<GroupDispatcher>.Name (Some EditorGroupName) EditorScreen world
            let world = World.setOptSelectedScreen (Some EditorScreen) world 
            let world = World.subscribe4 (handleNuMouseRightDown form worldChangers refWorld) EventAddresses.MouseRightDown Simulants.Game world
            let world = World.subscribe4 (handleNuEntityDragBegin form worldChangers refWorld) EventAddresses.MouseLeftDown Simulants.Game world
            let world = World.subscribe4 (handleNuEntityDragEnd form) EventAddresses.MouseLeftUp Simulants.Game world
            let world = World.subscribe4 (handleNuCameraDragBegin form) EventAddresses.MouseCenterDown Simulants.Game world
            let world = World.subscribe4 (handleNuCameraDragEnd form) EventAddresses.MouseCenterUp Simulants.Game world
            let world = subscribeToEntityEvents form world
            Right world
        | Left error -> Left error

    let tryCreateEditorWorld targetDirectory refinementDirectory form worldChangers refWorld sdlDeps nuPlugin =
        match tryMakeEditorWorld targetDirectory refinementDirectory form worldChangers refWorld sdlDeps nuPlugin with
        | Right world ->
            populateCreateComboBox form world
            populateTreeViewGroups form world
            Right world
        | Left _ as left -> left

    let [<EntryPoint; STAThread>] main _ =
        World.init ()
        let worldChangers = WorldChangers ()
        let refWorld = ref Unchecked.defaultof<World>
        let (targetDirectory, nuPlugin) = selectTargetDirectoryAndMakeNuPlugin ()
        let refinementDirectory = "Refinement"
        use form = createNuEditForm worldChangers refWorld
        let sdlViewConfig = ExistingWindow form.displayPanel.Handle
        let sdlRendererFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
        let sdlConfig =
            { ViewConfig = sdlViewConfig
              ViewW = form.displayPanel.MaximumSize.Width
              ViewH = form.displayPanel.MaximumSize.Height
              RendererFlags = sdlRendererFlags
              AudioChunkSize = Constants.Audio.AudioBufferSizeDefault }
        World.run4
            (fun sdlDeps ->
                match tryCreateEditorWorld targetDirectory refinementDirectory form worldChangers refWorld sdlDeps nuPlugin with
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