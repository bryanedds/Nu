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

[<RequireQualifiedAccess>]
module NuEdit =

    // TODO: move these into Constants
    let private DefaultPositionSnap = 8
    let private DefaultRotationSnap = 5
    let private DefaultCreationDepth = 0.0f
    let private CameraSpeed = 4.0f // NOTE: might be nice to be able to configure this just like entity creation depth in the editor
    let private RefinementDir = "Refinement"
    let private WorldChangers = WorldChangers ()
    let private RefWorld = ref Unchecked.defaultof<World>

    let pushPastWorld pastWorld world =
        world |> World.updateUserState (fun editorState ->
            { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = [] })

    let private getPickableEntities world =
        let selectedGroup = (World.getUserState world).SelectedGroup
        World.proxyEntities selectedGroup world

    let private clearOtherWorlds world =
        World.updateUserState (fun editorState -> { editorState with PastWorlds = []; FutureWorlds = [] }) world

    let private getSnaps (form : NuEditForm) =
        let positionSnap = snd ^ Int32.TryParse form.positionSnapTextBox.Text
        let rotationSnap = snd ^ Int32.TryParse form.rotationSnapTextBox.Text
        (positionSnap, rotationSnap)
    
    let private getCreationDepth (form : NuEditForm) =
        snd ^ Single.TryParse form.createDepthTextBox.Text

    let private getExpansionState (treeView : TreeView) =
        let nodeStates =
            Seq.fold
                (fun state (node : TreeNode) ->
                    if node.Nodes.Count = 0 then state
                    else (node.Name, node.IsExpanded) :: state)
                []
                (enumerable treeView.Nodes)
        Map.ofSeq nodeStates
        
    let private restoreExpansionState (treeView : TreeView) treeState =
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

    let private addTreeViewNode (form : NuEditForm) (entity : Entity) world =
        let entityCategoryName = Reflection.getTypeName ^ entity.GetDispatcherNp world
        let treeCategory = form.treeView.Nodes.[entityCategoryName]
        let treeCategoryNodeName = acstring entity.EntityAddress
        if not ^ treeCategory.Nodes.ContainsKey treeCategoryNodeName then
            let treeCategoryNode = TreeNode (entity.GetName world)
            treeCategoryNode.Name <- treeCategoryNodeName
            ignore ^ treeCategory.Nodes.Add treeCategoryNode
        else () // when changing an entity name, entity will be added twice - once from win forms, once from world

    let private clearTreeViewNodes (form : NuEditForm) =
        form.treeView.Nodes.Clear ()

    let private populateCreateComboBox (form : NuEditForm) world =
        form.createEntityComboBox.Items.Clear ()
        for dispatcherKvp in World.getEntityDispatchers world do
            ignore ^ form.createEntityComboBox.Items.Add dispatcherKvp.Key
        form.createEntityComboBox.SelectedIndex <- 0

    let private populateTreeViewGroups (form : NuEditForm) world =
        for dispatcherKvp in World.getEntityDispatchers world do
            let treeGroup = TreeNode dispatcherKvp.Key
            treeGroup.Name <- treeGroup.Text
            ignore ^ form.treeView.Nodes.Add treeGroup

    let private populateTreeViewNodes (form : NuEditForm) world =
        let selectedGroup = (World.getUserState world).SelectedGroup
        for entity in World.proxyEntities selectedGroup world do
            addTreeViewNode form entity world

    let private populateGroupTabs (form : NuEditForm) world =
        let groupTabPages = form.groupTabs.TabPages
        groupTabPages.Clear ()
        let groups = World.proxyGroups Simulants.EditorScreen world
        for group in groups do
            groupTabPages.Add group.GroupName

    let private tryScrollTreeViewToPropertyGridSelection (form : NuEditForm) =
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

    let private refreshPropertyGrid (form : NuEditForm) world =
        match form.propertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            entityTds.RefWorld := world // must be set for property grid
            if World.containsEntity entityTds.DescribedEntity world
            then form.propertyGrid.Refresh ()
            else form.propertyGrid.SelectedObject <- null
        | _ -> form.propertyGrid.SelectedObject <- null

    let private refreshTreeView (form : NuEditForm) world =
        let treeState = getExpansionState form.treeView
        clearTreeViewNodes form
        populateTreeViewGroups form world
        populateTreeViewNodes form world
        restoreExpansionState form.treeView treeState
        tryScrollTreeViewToPropertyGridSelection form

    let private refreshFormOnUndoRedo (form : NuEditForm) world =
        form.tickingButton.Checked <- false
        refreshPropertyGrid form world
        refreshTreeView form world

    let private selectEntity (form : NuEditForm) entity world =
        RefWorld := world // must be set for property grid
        form.propertyGrid.SelectedObject <- { DescribedEntity = entity; Form = form; WorldChangers = WorldChangers; RefWorld = RefWorld }
        tryScrollTreeViewToPropertyGridSelection form

    let private canEditWithMouse (form : NuEditForm) world =
        World.isTicking world &&
        not form.editWhileInteractiveCheckBox.Checked

    let private tryMousePick (form : NuEditForm) mousePosition world =
        let entities = getPickableEntities world
        let optPicked = World.tryPickEntity mousePosition entities world
        match optPicked with
        | Some entity ->
            selectEntity form entity world
            Some entity
        | None -> None

    let private handleNuEntityAdd (form : NuEditForm) event world =
        addTreeViewNode form (Entity.proxy ^ atoa event.Publisher.SimulantAddress) world
        (Cascade, world)

    let private handleNuEntityRemoving (form : NuEditForm) event world =
        match form.treeView.Nodes.Find (acstring event.Publisher.SimulantAddress, true) with
        | [||] -> () // when changing an entity name, entity will be removed twice - once from winforms, once from world
        | treeNodes -> form.treeView.Nodes.Remove treeNodes.[0]
        match form.propertyGrid.SelectedObject with
        | null -> (Cascade, world)
        | :? EntityTypeDescriptorSource as entityTds ->
            if atoa event.Publisher.SimulantAddress = entityTds.DescribedEntity.EntityAddress then
                form.propertyGrid.SelectedObject <- null
                let world = World.updateUserState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                (Cascade, world)
            else (Cascade, world)
        | _ -> failwith "Unexpected match failure in NuEdit.Program.handleNuEntityRemoving."

    let private handleNuMouseRightDown (form : NuEditForm) (_ : Event<MouseButtonData, Game>) world =
        let handled = if World.isTicking world then Cascade else Resolve
        let mousePosition = World.getMousePositionF world
        ignore ^ tryMousePick form mousePosition world
        let world = World.updateUserState (fun editorState -> { editorState with RightClickPosition = mousePosition }) world
        (handled, world)

    let private handleNuEntityDragBegin (form : NuEditForm) (_ : Event<MouseButtonData, Game>) world =
        if not ^ canEditWithMouse form world then
            let handled = if World.isTicking world then Cascade else Resolve
            let mousePosition = World.getMousePositionF world
            match tryMousePick form mousePosition world with
            | Some entity ->
                let world = pushPastWorld world world
                let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (entity.GetViewType world) mousePosition) world
                let dragState = DragEntityPosition (entity.GetPosition world + mousePositionWorld, mousePositionWorld, entity)
                let world = World.updateUserState (fun editorState -> { editorState with DragEntityState = dragState }) world
                (handled, world)
            | None -> (handled, world)
        else (Cascade, world)

    let private handleNuEntityDragEnd (form : NuEditForm) (_ : Event<MouseButtonData, Game>) world =
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

    let private handleNuCameraDragBegin (_ : NuEditForm) (_ : Event<MouseButtonData, Game>) world =
        let camera = World.getCamera world
        let mousePosition = World.getMousePositionF world
        let mousePositionScreen = Camera.mouseToScreen mousePosition camera
        let dragState = DragCameraPosition (camera.EyeCenter + mousePositionScreen, mousePositionScreen)
        let world = World.updateUserState (fun editorState -> { editorState with DragCameraState = dragState }) world
        (Resolve, world)

    let private handleNuCameraDragEnd (_ : NuEditForm) (_ : Event<MouseButtonData, Game>) world =
        match (World.getUserState world).DragCameraState with
        | DragCameraPosition _ ->
            let world = World.updateUserState (fun editorState -> { editorState with DragCameraState = DragCameraNone }) world
            (Resolve, world)
        | DragCameraNone -> (Resolve, world)

    let private subscribeToEntityEvents form world =
        // TODO: change subscriptions when groups are changed
        let selectedGroup = (World.getUserState world).SelectedGroup
        world |>
            World.subscribe5 Constants.SubscriptionKeys.AddEntity (handleNuEntityAdd form) (Events.EntityAdd ->- selectedGroup ->- Events.Any) Simulants.Game |>
            World.subscribe5 Constants.SubscriptionKeys.RemovingEntity (handleNuEntityRemoving form) (Events.EntityRemoving ->- selectedGroup ->- Events.Any) Simulants.Game

    let private unsubscribeFromEntityEvents world =
        world |>
            World.unsubscribe Constants.SubscriptionKeys.AddEntity |>
            World.unsubscribe Constants.SubscriptionKeys.RemovingEntity

    let private trySaveFile filePath world =
        let selectedGroup = (World.getUserState world).SelectedGroup
        try World.writeGroupToFile filePath selectedGroup world
        with exn ->
            ignore ^
                MessageBox.Show
                    ("Could not save file due to: " + acstring exn,
                     "File save error.",
                     MessageBoxButtons.OK,
                     MessageBoxIcon.Error)

    let private tryLoadFile (form : NuEditForm) filePath world =

        try // destroy current group
            let selectedGroup = (World.getUserState world).SelectedGroup
            let world = unsubscribeFromEntityEvents world
            let world = World.destroyGroupImmediate selectedGroup world

            // load and add group
            let world = World.readGroupFromFile filePath (Some selectedGroup.GroupName) Simulants.EditorScreen world |> snd
            let world = subscribeToEntityEvents form world

            // refresh tree view
            refreshTreeView form world

            // update save file name
            form.saveFileDialog.FileName <- Path.GetFileName filePath
            world

        // handle load failure
        with exn ->
            ignore ^
                MessageBox.Show
                    ("Could not load file due to: " + acstring exn,
                     "File load error.",
                     MessageBoxButtons.OK,
                     MessageBoxIcon.Error)
            world

    let private handleFormExit (form : NuEditForm) (_ : EventArgs) =
        form.Close ()

    let private handleFormCreateDepthPlusClick (form : NuEditForm) (_ : EventArgs) =
        let depth = snd ^ Single.TryParse form.createDepthTextBox.Text
        form.createDepthTextBox.Text <- acstring ^ depth + 1.0f

    let private handleFormCreateDepthMinusClick (form : NuEditForm) (_ : EventArgs) =
        let depth = snd ^ Single.TryParse form.createDepthTextBox.Text
        form.createDepthTextBox.Text <- acstring ^ depth - 1.0f

    let private handleFormTreeViewNodeSelect (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            let entity = Entity.proxy ^ ftoa form.treeView.SelectedNode.Name
            match Address.getNameKeys entity.EntityAddress with
            | [_; _; _] ->
                RefWorld := world // must be set for property grid
                let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = WorldChangers; RefWorld = RefWorld }
                form.propertyGrid.SelectedObject <- entityTds
                world
            | _ -> world) // not an entity address

    let private handleFormCreate atMouse (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            try let world = pushPastWorld world world
                let selectedGroup = (World.getUserState world).SelectedGroup
                let (entity, world) = World.createEntity form.createEntityComboBox.Text None selectedGroup world
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
                RefWorld := world // must be set for property grid
                let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = WorldChangers; RefWorld = RefWorld }
                form.propertyGrid.SelectedObject <- entityTds
                world
            with exn -> ignore ^ MessageBox.Show (acstring exn); world)

    let private handleFormDelete (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            let world = pushPastWorld world world
            match form.propertyGrid.SelectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = World.destroyEntity entityTds.DescribedEntity world
                form.propertyGrid.SelectedObject <- null
                world
            | _ -> world)

    let private handleFormSave (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            let saveFileResult = form.saveFileDialog.ShowDialog form
            match saveFileResult with
            | DialogResult.OK -> trySaveFile form.saveFileDialog.FileName world; world
            | _ -> world)

    let private handleFormOpen (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            let openFileResult = form.openFileDialog.ShowDialog form
            match openFileResult with
            | DialogResult.OK ->
                let world = tryLoadFile form form.openFileDialog.FileName world
                let world = clearOtherWorlds world
                form.propertyGrid.SelectedObject <- null
                form.tickingButton.Checked <- false
                world
            | _ -> world)

    let private handleFormUndo (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            match (World.getUserState world).PastWorlds with
            | [] -> world
            | pastWorld :: pastWorlds ->
                let futureWorld = world
                let selectedGroup = (World.getUserState world).SelectedGroup
                let world = World.continueHack selectedGroup pastWorld
                let world =
                    World.updateUserState (fun editorState ->
                        { editorState with PastWorlds = pastWorlds; FutureWorlds = futureWorld :: (World.getUserState futureWorld).FutureWorlds })
                        world
                let world = World.setTickRate 0L world
                refreshFormOnUndoRedo form world
                world)

    let private handleFormRedo (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            match (World.getUserState world).FutureWorlds with
            | [] -> world
            | futureWorld :: futureWorlds ->
                let pastWorld = world
                let selectedGroup = (World.getUserState world).SelectedGroup
                let world = World.continueHack selectedGroup futureWorld
                let world =
                    World.updateUserState (fun editorState ->
                        { editorState with PastWorlds = pastWorld :: (World.getUserState pastWorld).PastWorlds; FutureWorlds = futureWorlds })
                        world
                let world = World.setTickRate 0L world
                refreshFormOnUndoRedo form world
                world)

    let private handleFormInteractivityChanged (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            let tickRate = if form.tickingButton.Checked then 1L else 0L
            let (pastWorld, world) = (world, World.setTickRate tickRate world)
            if tickRate = 1L then pushPastWorld pastWorld world else world)

    let private handleFormCopy (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds -> World.copyToClipboard entityTds.DescribedEntity world; world
            | _ -> trace ^ "Invalid copy operation (likely a code issue in NuEdit)."; world)

    let private handleFormCut (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let world = World.cutToClipboard entityTds.DescribedEntity world
                form.propertyGrid.SelectedObject <- null
                world
            | _ -> trace ^ "Invalid cut operation (likely a code issue in NuEdit)."; world)

    let private handleFormPaste atMouse (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            let world = pushPastWorld world world
            let selectedGroup = (World.getUserState world).SelectedGroup
            let (positionSnap, rotationSnap) = getSnaps form
            let editorState = World.getUserState world
            let (optEntity, world) = World.pasteFromClipboard atMouse editorState.RightClickPosition positionSnap rotationSnap selectedGroup world
            match optEntity with
            | Some entity -> selectEntity form entity world; world
            | None -> world)

    let private handleFormQuickSize (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
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
            | _ -> trace ^ "Invalid quick size operation (likely a code issue in NuEdit)."; world)

    let private handleFormResetCamera (_ : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            World.updateCamera (fun camera -> { camera with EyeCenter = Vector2.Zero }) world)

    let private handleFormReloadAssets (_ : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            let editorState = World.getUserState world
            let targetDir = editorState.TargetDir
            let assetSourceDir = Path.Combine (targetDir, "..\\..")
            match World.tryReloadAssets assetSourceDir targetDir RefinementDir world with
            | Right world -> world
            | Left error ->
                ignore ^
                    MessageBox.Show
                        ("Asset reload error due to: " + error + "'.",
                         "Asset reload error.",
                         MessageBoxButtons.OK,
                         MessageBoxIcon.Error)
                world)

    let private handleFormReloadOverlays (form : NuEditForm) (_ : EventArgs) =
        ignore ^ WorldChangers.Add (fun world ->
            let world = pushPastWorld world world
            let targetDir = (World.getUserState world).TargetDir
            let overlayDir = Path.Combine (targetDir, "..\\..")
            match World.tryReloadOverlays overlayDir targetDir world with
            | Right world ->
                refreshPropertyGrid form world
                world
            | Left error ->
                ignore ^
                    MessageBox.Show
                        ("Overlay reload error due to: " + error + "'.",
                         "Overlay reload error.",
                         MessageBoxButtons.OK,
                         MessageBoxIcon.Error)
                world)

    let private updateEntityDrag (form : NuEditForm) world =
        if not ^ canEditWithMouse form world then
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

    let private updateCameraDrag (_ : NuEditForm) world =
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
    let private updateUndoButton (form : NuEditForm) world =
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty (World.getUserState world).PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not ^ List.isEmpty (World.getUserState world).PastWorlds then
            form.undoButton.Enabled <- true
            form.undoToolStripMenuItem.Enabled <- true

    let private updateRedoButton (form : NuEditForm) world =
        let editorState = World.getUserState world
        if form.redoToolStripMenuItem.Enabled then
            if List.isEmpty editorState.FutureWorlds then
                form.redoButton.Enabled <- false
                form.redoToolStripMenuItem.Enabled <- false
        elif not ^ List.isEmpty editorState.FutureWorlds then
            form.redoButton.Enabled <- true
            form.redoToolStripMenuItem.Enabled <- true

    let private updateEditorWorld form world =
        let worldChangersCopy = List.ofSeq WorldChangers
        WorldChangers.Clear ()
        let world = Seq.fold (fun world worldChanger -> worldChanger world) world worldChangersCopy
        let world = updateEntityDrag form world
        let world = updateCameraDrag form world
        updateUndoButton form world
        updateRedoButton form world
        if form.IsDisposed
        then World.exit world
        else world

    let makeNuPlugin filePath =
        let dirName = Path.GetDirectoryName filePath
        Directory.SetCurrentDirectory dirName
        let assembly = Assembly.LoadFrom filePath
        let assemblyTypes = assembly.GetTypes ()
        let optDispatcherType =
            Array.tryFind
                (fun (aType : Type) -> aType.IsSubclassOf typeof<NuPlugin>)
                assemblyTypes
        match optDispatcherType with
        | Some aType ->
            let nuPlugin = Activator.CreateInstance aType :?> NuPlugin
            (dirName, nuPlugin)
        | None -> (".", NuPlugin ())

    let selectTargetDirAndMakeNuPlugin () =
        use openDialog = new OpenFileDialog ()
        openDialog.Filter <- "Executable Files (*.exe)|*.exe"
        openDialog.Title <- "Select your game's executable file to make its assets and components available in the editor (or cancel for defaults)."
        if openDialog.ShowDialog () = DialogResult.OK
        then makeNuPlugin openDialog.FileName
        else (".", NuPlugin ())

    let createForm () =
        let form = new NuEditForm ()
        form.displayPanel.MaximumSize <- Drawing.Size (Constants.Render.ResolutionX, Constants.Render.ResolutionY)
        form.positionSnapTextBox.Text <- acstring DefaultPositionSnap
        form.rotationSnapTextBox.Text <- acstring DefaultRotationSnap
        form.createDepthTextBox.Text <- acstring DefaultCreationDepth
        form.exitToolStripMenuItem.Click.Add (handleFormExit form)
        form.createDepthPlusButton.Click.Add (handleFormCreateDepthPlusClick form)
        form.createDepthMinusButton.Click.Add (handleFormCreateDepthMinusClick form)
        form.treeView.AfterSelect.Add (handleFormTreeViewNodeSelect form)
        form.createEntityButton.Click.Add (handleFormCreate false form)
        form.createToolStripMenuItem.Click.Add (handleFormCreate false form)
        form.createContextMenuItem.Click.Add (handleFormCreate true form)
        form.deleteEntityButton.Click.Add (handleFormDelete form)
        form.deleteToolStripMenuItem.Click.Add (handleFormDelete form)
        form.deleteContextMenuItem.Click.Add (handleFormDelete form)
        form.saveToolStripMenuItem.Click.Add (handleFormSave form)
        form.openToolStripMenuItem.Click.Add (handleFormOpen form)
        form.undoButton.Click.Add (handleFormUndo form)
        form.undoToolStripMenuItem.Click.Add (handleFormUndo form)
        form.redoButton.Click.Add (handleFormRedo form)
        form.redoToolStripMenuItem.Click.Add (handleFormRedo form)
        form.tickingButton.CheckedChanged.Add (handleFormInteractivityChanged form)
        form.cutToolStripMenuItem.Click.Add (handleFormCut form)
        form.cutContextMenuItem.Click.Add (handleFormCut form)
        form.copyToolStripMenuItem.Click.Add (handleFormCopy form)
        form.copyContextMenuItem.Click.Add (handleFormCopy form)
        form.pasteToolStripMenuItem.Click.Add (handleFormPaste false form)
        form.pasteContextMenuItem.Click.Add (handleFormPaste true form)
        form.quickSizeToolStripButton.Click.Add (handleFormQuickSize form)
        form.resetCameraButton.Click.Add (handleFormResetCamera form)
        form.reloadAssetsButton.Click.Add (handleFormReloadAssets form)
        form.reloadOverlaysButton.Click.Add (handleFormReloadOverlays form)
        form.Show ()
        form

    let tryMakeEditorWorld targetDir sdlDeps form plugin =
        let editorState =
            { TargetDir = targetDir
              RightClickPosition = Vector2.Zero
              DragEntityState = DragEntityNone
              DragCameraState = DragCameraNone
              PastWorlds = []
              FutureWorlds = []
              SelectedGroup = Simulants.DefaultEditorGroup }
        let eitherWorld = World.tryMake false 0L editorState plugin sdlDeps
        match eitherWorld with
        | Right world ->
            let world = World.createScreen typeof<ScreenDispatcher>.Name (Some Simulants.EditorScreen.ScreenName) world |> snd
            let world = World.createGroup typeof<GroupDispatcher>.Name (Some Simulants.DefaultEditorGroup.GroupName) Simulants.EditorScreen world |> snd
            let world = World.setOptSelectedScreen (Some Simulants.EditorScreen) world 
            let world = World.subscribe (handleNuMouseRightDown form) Events.MouseRightDown Simulants.Game world
            let world = World.subscribe (handleNuEntityDragBegin form) Events.MouseLeftDown Simulants.Game world
            let world = World.subscribe (handleNuEntityDragEnd form) Events.MouseLeftUp Simulants.Game world
            let world = World.subscribe (handleNuCameraDragBegin form) Events.MouseCenterDown Simulants.Game world
            let world = World.subscribe (handleNuCameraDragEnd form) Events.MouseCenterUp Simulants.Game world
            let world = subscribeToEntityEvents form world
            Right world
        | Left error -> Left error

    let attemptMakeSdlDeps (form : NuEditForm) =
        let sdlViewConfig = ExistingWindow form.displayPanel.Handle
        let sdlRendererFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
        let sdlConfig =
            { ViewConfig = sdlViewConfig
              ViewW = form.displayPanel.MaximumSize.Width
              ViewH = form.displayPanel.MaximumSize.Height
              RendererFlags = sdlRendererFlags
              AudioChunkSize = Constants.Audio.AudioBufferSizeDefault }
        SdlDeps.attemptMake sdlConfig

    let run5 runWhile sdlDeps (form : NuEditForm) =

        populateCreateComboBox form !RefWorld
        populateTreeViewGroups form !RefWorld
        populateGroupTabs form !RefWorld

        World.runWithoutCleanUp
            runWhile
            (fun world ->
                let world = updateEditorWorld form world
                RefWorld := world
                world)
            (fun world ->
                form.displayPanel.Invalidate ()
                world)
            sdlDeps
            Running
            !RefWorld

    let runFromRepl runWhile sdlDeps form world =
        WorldChangers.Clear ()
        RefWorld := world
        run5 runWhile sdlDeps form

    let run () =
        use form = createForm ()
        let (targetDir, plugin) = selectTargetDirAndMakeNuPlugin ()
        match attemptMakeSdlDeps form with
        | Right sdlDeps ->
            match tryMakeEditorWorld targetDir sdlDeps form plugin with
            | Right world ->
                RefWorld := world
                ignore ^ run5 tautology sdlDeps form
                Constants.Engine.SuccessExitCode
            | Left error -> failwith error
        | Left error -> failwith error