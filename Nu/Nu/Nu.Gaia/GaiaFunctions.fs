// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Gaia
open Nu.Gaia.Design
open SDL2
open OpenTK
open TiledSharp
open Prime
open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open System.Windows.Forms
open System.ComponentModel
open Microsoft.FSharp.Reflection
open Prime
open Nu

[<RequireQualifiedAccess>]
module Gaia =

    // TODO: move these into Constants
    let private DefaultPositionSnap = 8
    let private DefaultRotationSnap = 5
    let private DefaultCreationDepth = 0.0f
    let private CameraSpeed = 4.0f // NOTE: might be nice to be able to configure this just like entity creation depth in the editor
    let private RefinementDir = "Refinement"

    // g'lol'bals!
    let private WorldChangers = WorldChangers ()
    let private RefWorld = ref Unchecked.defaultof<World>

    let addWorldChanger worldChanger =
        ignore ^ WorldChangers.Add worldChanger

    let pushPastWorld pastWorld world =
        World.updateUserState
            (fun editorState -> { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = [] })
            world

    let private getPickableEntities world =
        let selectedGroup = (World.getUserState world).SelectedGroup
        World.proxyEntities selectedGroup world

    let private getSnaps (form : GaiaForm) =
        let positionSnap = snd ^ Int32.TryParse form.positionSnapTextBox.Text
        let rotationSnap = snd ^ Int32.TryParse form.rotationSnapTextBox.Text
        (positionSnap, rotationSnap)
    
    let private getCreationDepth (form : GaiaForm) =
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

    let private addTreeViewNode (form : GaiaForm) (entity : Entity) world =
        let entityCategoryName = getTypeName ^ entity.GetDispatcherNp world
        let treeCategory = form.treeView.Nodes.[entityCategoryName]
        let treeCategoryNodeName = scstring entity.EntityAddress
        if not ^ treeCategory.Nodes.ContainsKey treeCategoryNodeName then
            let treeCategoryNode = TreeNode (Name.getNameStr ^ entity.GetName world)
            treeCategoryNode.Name <- treeCategoryNodeName
            treeCategory.Nodes.Add treeCategoryNode |> ignore
        else () // when changing an entity name, entity will be added twice - once from win forms, once from world

    let private clearTreeViewNodes (form : GaiaForm) =
        form.treeView.Nodes.Clear ()

    let private populateCreateComboBox (form : GaiaForm) world =
        form.createEntityComboBox.Items.Clear ()
        for dispatcherKvp in World.getEntityDispatchers world do
            form.createEntityComboBox.Items.Add dispatcherKvp.Key |> ignore
        form.createEntityComboBox.SelectedIndex <- 0

    let private populateTreeViewGroups (form : GaiaForm) world =
        for dispatcherKvp in World.getEntityDispatchers world do
            let treeGroup = TreeNode dispatcherKvp.Key
            treeGroup.Name <- treeGroup.Text
            form.treeView.Nodes.Add treeGroup |> ignore

    let private populateTreeViewNodes (form : GaiaForm) world =
        let selectedGroup = (World.getUserState world).SelectedGroup
        for entity in World.proxyEntities selectedGroup world do
            addTreeViewNode form entity world

    let private populateGroupTabs (form : GaiaForm) world =
        let groupTabPages = form.groupTabs.TabPages
        groupTabPages.Clear ()
        let groups = World.proxyGroups Simulants.EditorScreen world
        for group in groups do
            let groupNameStr = Name.getNameStr group.GroupName
            groupTabPages.Add (groupNameStr, groupNameStr)

    let private setTreeViewSelectionToPropertyGridSelection (form : GaiaForm) =
        match form.propertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.treeView.Nodes.Find (scstring entityTds.DescribedEntity.EntityAddress, true) with
            | [||] -> form.treeView.SelectedNode <- null
            | nodes ->
                let node = nodes.[0]
                if node.Parent.IsExpanded
                then form.treeView.SelectedNode <- node; node.EnsureVisible ()
                else form.treeView.SelectedNode <- null
        | _ -> form.treeView.SelectedNode <- null

    let private selectEntity (form : GaiaForm) entity world =
        let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = WorldChangers; RefWorld = RefWorld }
        RefWorld := world // must be set for property grid
        form.propertyGrid.SelectedObject <- entityTds

    let private deselectEntity (form : GaiaForm) world =
        RefWorld := world // must be set for property grid
        form.propertyGrid.SelectedObject <- null

    let private refreshPropertyGrid (form : GaiaForm) world =
        match form.propertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            entityTds.RefWorld := world // must be set for property grid
            if World.containsEntity entityTds.DescribedEntity world
            then form.propertyGrid.Refresh ()
            else deselectEntity form world
        | _ -> deselectEntity form world

    let private refreshTreeView (form : GaiaForm) world =
        let treeState = getExpansionState form.treeView
        clearTreeViewNodes form
        populateTreeViewGroups form world
        populateTreeViewNodes form world
        restoreExpansionState form.treeView treeState
        setTreeViewSelectionToPropertyGridSelection form

    let private refreshGroupTabs (form : GaiaForm) world =
        populateGroupTabs form world

    let private refreshFormOnUndoRedo (form : GaiaForm) world =
        form.tickingButton.Checked <- false
        refreshPropertyGrid form world
        refreshTreeView form world
        refreshGroupTabs form world

    let private canEditWithMouse (form : GaiaForm) world =
        World.isTicking world &&
        not form.editWhileInteractiveCheckBox.Checked

    let private tryMousePick (form : GaiaForm) mousePosition world =
        let entities = getPickableEntities world
        let optPicked = World.tryPickEntity mousePosition entities world
        match optPicked with
        | Some entity ->
            selectEntity form entity world
            Some entity
        | None -> None

    let private handleNuEntityAdd (form : GaiaForm) evt world =
        let entity = Entity.proxy ^ atoa evt.Publisher.ParticipantAddress
        addTreeViewNode form entity world
        selectEntity form entity world
        (Cascade, world)

    let private handleNuEntityRemoving (form : GaiaForm) evt world =
        match form.treeView.Nodes.Find (scstring evt.Publisher.ParticipantAddress, true) with
        | [||] -> () // when changing an entity name, entity will be removed twice - once from winforms, once from world
        | treeNodes -> form.treeView.Nodes.Remove treeNodes.[0]
        match form.propertyGrid.SelectedObject with
        | null -> (Cascade, world)
        | :? EntityTypeDescriptorSource as entityTds ->
            if atoa evt.Publisher.ParticipantAddress = entityTds.DescribedEntity.EntityAddress then
                let world = World.updateUserState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                deselectEntity form world
                (Cascade, world)
            else (Cascade, world)
        | _ ->
            Log.trace "Unexpected match failure in Nu.Gaia.Program.handleNuEntityRemoving (probably a bug in Gaia or Nu)."
            (Cascade, world)

    let private handleNuMouseRightDown (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let handled = if World.isTicking world then Cascade else Resolve
        let mousePosition = World.getMousePositionF world
        tryMousePick form mousePosition world |> ignore
        let world = World.updateUserState (fun editorState -> { editorState with RightClickPosition = mousePosition }) world
        (handled, world)

    let private handleNuEntityDragBegin (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        if not ^ canEditWithMouse form world then
            let handled = if World.isTicking world then Cascade else Resolve
            let mousePosition = World.getMousePositionF world
            match tryMousePick form mousePosition world with
            | Some entity ->
                let world = pushPastWorld world world
                let mousePositionWorld = World.mouseToWorld (entity.GetViewType world) mousePosition world
                let dragState = DragEntityPosition (entity.GetPosition world + mousePositionWorld, mousePositionWorld, entity)
                let world = World.updateUserState (fun editorState -> { editorState with DragEntityState = dragState }) world
                (handled, world)
            | None -> (handled, world)
        else (Cascade, world)

    let private handleNuEntityDragEnd (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
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

    let private handleNuCameraDragBegin (_ : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let mousePosition = World.getMousePositionF world
        let mousePositionScreen = World.mouseToScreen mousePosition world
        let dragState = DragCameraPosition (World.getEyeCenter world + mousePositionScreen, mousePositionScreen)
        let world = World.updateUserState (fun editorState -> { editorState with DragCameraState = dragState }) world
        (Resolve, world)

    let private handleNuCameraDragEnd (_ : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        match (World.getUserState world).DragCameraState with
        | DragCameraPosition _ ->
            let world = World.updateUserState (fun editorState -> { editorState with DragCameraState = DragCameraNone }) world
            (Resolve, world)
        | DragCameraNone -> (Resolve, world)

    let private subscribeToEntityEvents form world =
        let selectedGroup = (World.getUserState world).SelectedGroup
        world |>
            World.subscribe5 Constants.SubscriptionKeys.AddEntity (handleNuEntityAdd form) (Events.EntityAdd ->- selectedGroup ->- Events.Any) Simulants.Game |>
            World.subscribe5 Constants.SubscriptionKeys.RemovingEntity (handleNuEntityRemoving form) (Events.EntityRemoving ->- selectedGroup ->- Events.Any) Simulants.Game

    let private unsubscribeFromEntityEvents world =
        world |>
            World.unsubscribe Constants.SubscriptionKeys.AddEntity |>
            World.unsubscribe Constants.SubscriptionKeys.RemovingEntity

    let private trySaveSelectedGroup filePath world =
        let selectedGroup = (World.getUserState world).SelectedGroup
        try World.writeGroupToFile filePath selectedGroup world
        with exn ->
            ignore ^ World.choose world
            ignore ^ MessageBox.Show ("Could not save file due to: " + scstring exn, "File save error", MessageBoxButtons.OK, MessageBoxIcon.Error)

    let private tryLoadSelectedGroup (form : GaiaForm) filePath world =

        try // destroy current group
            let selectedGroup = (World.getUserState world).SelectedGroup
            let world = unsubscribeFromEntityEvents world
            let world = World.destroyGroupImmediate selectedGroup world

            // load and add group
            let world = World.readGroupFromFile filePath (Some selectedGroup.GroupName) Simulants.EditorScreen world |> snd
            let world = subscribeToEntityEvents form world

            // refresh tree view
            refreshTreeView form world
            world

        // handle load failure
        with exn ->
            let world = World.choose world
            ignore ^ MessageBox.Show ("Could not load file due to: " + scstring exn, "File load error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            world

    let private handleFormExit (form : GaiaForm) (_ : EventArgs) =
        form.Close ()

    let private handleFormCreateDepthPlusClick (form : GaiaForm) (_ : EventArgs) =
        let depth = snd ^ Single.TryParse form.createDepthTextBox.Text
        form.createDepthTextBox.Text <- scstring ^ depth + 1.0f

    let private handleFormCreateDepthMinusClick (form : GaiaForm) (_ : EventArgs) =
        let depth = snd ^ Single.TryParse form.createDepthTextBox.Text
        form.createDepthTextBox.Text <- scstring ^ depth - 1.0f

    let private refreshPropertyEditor (form : GaiaForm) =
        match (form.propertyGrid.SelectedObject, form.propertyGrid.SelectedGridItem) with
        | (null, _) | (_, null) ->
            form.propertyEditor.Enabled <- false
            form.propertyNameLabel.Text <- String.Empty
            form.propertyDescriptionTextBox.Text <- String.Empty
            form.propertyValueTextBox.Text <- String.Empty
            form.propertyValueTextBox.EmptyUndoBuffer ()
        | (_, selectedGridItem) ->
            match selectedGridItem.GridItemType with
            | GridItemType.Property ->
                let ty = selectedGridItem.PropertyDescriptor.PropertyType
                let typeConverter = SymbolicConverter ty
                form.propertyEditor.Enabled <- true
                form.propertyNameLabel.Text <- scstring selectedGridItem.Label
                form.propertyDescriptionTextBox.Text <- selectedGridItem.PropertyDescriptor.Description
                if isNotNull selectedGridItem.Value || isNullTrueValue ty then
                    let (keywords0, keywords1) =
                        match selectedGridItem.Label with
                        | "OptOverlayName" ->
                            let overlays = World.getIntrinsicOverlays !RefWorld @ World.getExtrinsicOverlays !RefWorld
                            let overlayNames = List.map (fun overlay -> overlay.OverlayName) overlays
                            (String.Join (" ", overlayNames), "")
                        | "FacetNames" ->
                            let facetNames = !RefWorld |> World.getFacets |> Map.toKeyList
                            (String.Join (" ", facetNames), "")
                        | _ ->
                            match ty.GetCustomAttribute<SyntaxAttribute> true with
                            | null -> ("", "")
                            | syntax -> (syntax.Keywords0, syntax.Keywords1)
                    let selectionStart = form.propertyValueTextBox.SelectionStart
                    let strUnescaped = typeConverter.ConvertToString selectedGridItem.Value
                    let strEscaped = String.escape strUnescaped
                    let strPretty = Symbol.prettyPrint keywords0 strEscaped
                    form.propertyValueTextBox.Text <- strPretty + "\r\n"
                    form.propertyValueTextBox.EmptyUndoBuffer ()
                    form.propertyValueTextBox.Keywords0 <- keywords0
                    form.propertyValueTextBox.Keywords1 <- keywords1
                    form.propertyValueTextBox.SelectionStart <- selectionStart
                    form.propertyValueTextBox.ScrollCaret ()
            | _ ->
                form.propertyEditor.Enabled <- false
                form.propertyNameLabel.Text <- String.Empty
                form.propertyDescriptionTextBox.Text <- String.Empty
                form.propertyValueTextBox.Text <- String.Empty
                form.propertyValueTextBox.EmptyUndoBuffer ()

    let private applyPropertyEditor (form : GaiaForm) =
        match form.propertyGrid.SelectedObject with
        | null -> ()
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.propertyGrid.SelectedGridItem with
            | null -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."
            | selectedGridItem ->
                match selectedGridItem.GridItemType with
                | GridItemType.Property when form.propertyNameLabel.Text = selectedGridItem.Label ->
                    let propertyDescriptor = selectedGridItem.PropertyDescriptor :?> EntityPropertyDescriptor
                    let typeConverter = SymbolicConverter (selectedGridItem.PropertyDescriptor.PropertyType)
                    try form.propertyValueTextBox.EndUndoAction ()
                        let strEscaped = form.propertyValueTextBox.Text.TrimEnd ()
                        let strUnescaped = String.unescape strEscaped
                        let propertyValue = typeConverter.ConvertFromString strUnescaped
                        propertyDescriptor.SetValue (entityTds, propertyValue)
                    with
                    | :? ConversionException as exn ->
                        match exn.OptSymbol with
                        | Some symbol ->
                            match Symbol.tryGetOrigin symbol with
                            | Some origin ->
                                form.propertyValueTextBox.SelectionStart <- int origin.Start.Index
                                form.propertyValueTextBox.SelectionEnd <- int origin.Stop.Index
                            | None -> ()
                        | None -> ()
                        Log.trace ^ "Invalid apply property operation due to: " + scstring exn
                    | exn -> Log.trace ^ "Invalid apply property operation due to: " + scstring exn
                | _ -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."
        | _ -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."

    let private tryReloadAssets (_ : GaiaForm) world =
        let editorState = World.getUserState world
        let targetDir = editorState.TargetDir
        let assetSourceDir = Path.Combine (targetDir, "..\\..")
        World.tryReloadAssets assetSourceDir targetDir RefinementDir world

    let private tryLoadAssetGraph (form : GaiaForm) world =
        match tryReloadAssets form world with
        | Right (assetGraph, world) ->
            let keywords0 = match typeof<AssetGraph>.GetCustomAttribute<SyntaxAttribute> true with null -> "" | syntax -> syntax.Keywords0
            let selectionStart = form.propertyValueTextBox.SelectionStart
            form.assetGraphTextBox.Text <- (Symbol.prettyPrint keywords0 ^ scstring ^ AssetGraph.getPackageDescriptors assetGraph) + "\r\n"
            form.assetGraphTextBox.SelectionStart <- selectionStart
            form.assetGraphTextBox.ScrollCaret ()
            Some world
        | Left error ->
            ignore ^ MessageBox.Show ("Could not load asset graph due to: " + error + "'.", "Failed to load asset graph", MessageBoxButtons.OK, MessageBoxIcon.Error)
            None

    let private trySaveAssetGraph (form : GaiaForm) world =
        let editorState = World.getUserState world
        let assetSourceDir = Path.Combine (editorState.TargetDir, "..\\..")
        let assetGraphFilePath = Path.Combine (assetSourceDir, Constants.Assets.AssetGraphFilePath)
        try let packageDescriptors = scvalue<Map<string, PackageDescriptor>> ^ form.assetGraphTextBox.Text.TrimEnd ()
            let assetGraphKeywords0 = match typeof<AssetGraph>.GetCustomAttribute<SyntaxAttribute> true with null -> "" | syntax -> syntax.Keywords0
            File.WriteAllText (assetGraphFilePath, Symbol.prettyPrint assetGraphKeywords0 ^ scstring packageDescriptors)
            true
        with exn ->
            ignore ^ MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to save asset graph", MessageBoxButtons.OK, MessageBoxIcon.Error)
            false

    let private tryReloadOverlays (_ : GaiaForm) world =
        let targetDir = (World.getUserState world).TargetDir
        let overlayDir = Path.Combine (targetDir, "..\\..")
        World.tryReloadOverlays overlayDir targetDir world

    let private tryLoadOverlayer (form : GaiaForm) world =
        match tryReloadOverlays form world with
        | Right (overlayer, world) ->
            let keywords0 = match typeof<Overlayer>.GetCustomAttribute<SyntaxAttribute> true with null -> "" | syntax -> syntax.Keywords0
            let selectionStart = form.propertyValueTextBox.SelectionStart
            form.overlayerTextBox.Text <- (Symbol.prettyPrint keywords0 ^ scstring ^ Overlayer.getExtrinsicOverlays overlayer) + "\r\n"
            form.overlayerTextBox.SelectionStart <- selectionStart
            form.overlayerTextBox.ScrollCaret ()
            Some world
        | Left error ->
            ignore ^ MessageBox.Show ("Could not reload overlayer due to: " + error + "'.", "Failed to reload overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error)
            None

    let private trySaveOverlayer (form : GaiaForm) world =
        let editorState = World.getUserState world
        let overlayerSourceDir = Path.Combine (editorState.TargetDir, "..\\..")
        let overlayerFilePath = Path.Combine (overlayerSourceDir, Constants.Assets.OverlayerFilePath)
        try let overlays = scvalue<Overlay list> ^ form.overlayerTextBox.Text.TrimEnd ()
            let overlayerKeywords0 = match typeof<Overlayer>.GetCustomAttribute<SyntaxAttribute> true with null -> "" | syntax -> syntax.Keywords0
            File.WriteAllText (overlayerFilePath, Symbol.prettyPrint overlayerKeywords0 ^ scstring overlays)
            true
        with exn ->
            ignore ^ MessageBox.Show ("Could not save overlayer due to: " + scstring exn, "Failed to save overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error)
            false

    let private handleFormPropertyGridSelectedObjectsChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form
        setTreeViewSelectionToPropertyGridSelection form

    let private handleFormPropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormPropertyRefreshClick (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormPropertyApplyClick (form : GaiaForm) (_ : EventArgs) =
        applyPropertyEditor form

    let private handleFormTreeViewNodeSelect (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            if isNotNull form.treeView.SelectedNode then
                let entity = Entity.proxy ^ ftoa ^ Name.make form.treeView.SelectedNode.Name
                match Address.getNames entity.EntityAddress with
                | [_; _; _] ->
                    RefWorld := world // must be set for property grid
                    let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = WorldChangers; RefWorld = RefWorld }
                    form.propertyGrid.SelectedObject <- entityTds
                    world
                | _ -> world // don't have an entity address
            else world

    let private handleFormCreate atMouse (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            try let world = pushPastWorld world world
                let selectedGroup = (World.getUserState world).SelectedGroup
                let (entity, world) = World.createEntity form.createEntityComboBox.Text None None selectedGroup world
                let (positionSnap, rotationSnap) = getSnaps form
                let mousePosition = World.getMousePositionF world
                let entityPosition =
                    if atMouse
                    then World.mouseToWorld (entity.GetViewType world) mousePosition world
                    else World.mouseToWorld (entity.GetViewType world) (World.getEyeSize world * 0.5f) world
                let entityTransform =
                    { Transform.Position = entityPosition
                      Size = entity.GetSize world
                      Rotation = entity.GetRotation world
                      Depth = getCreationDepth form }
                let world = entity.SetTransformSnapped positionSnap rotationSnap entityTransform world
                let world = World.propagateEntityPhysics entity world
                RefWorld := world // must be set for property grid
                let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = WorldChangers; RefWorld = RefWorld }
                form.propertyGrid.SelectedObject <- entityTds
                world
            with exn ->
                let world = World.choose world
                ignore ^ MessageBox.Show (scstring exn, "Could not create Gaia form", MessageBoxButtons.OK, MessageBoxIcon.Error)
                world

    let private handleFormDelete (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let world = pushPastWorld world world
            match form.propertyGrid.SelectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = World.destroyEntity entityTds.DescribedEntity world
                deselectEntity form world
                world
            | _ -> world

    let private handleFormNew (form : GaiaForm) (_ : EventArgs) =
        use groupNameEntryForm = new NameEntryForm ()
        groupNameEntryForm.StartPosition <- FormStartPosition.CenterParent
        groupNameEntryForm.okButton.Click.Add ^ fun _ ->
            addWorldChanger ^ fun world ->
                let world = pushPastWorld world world
                let groupName = groupNameEntryForm.nameTextBox.Text
                try if groupName.Length = 0 then failwith "Group name cannot be empty in Gaia due to WinForms limitations."
                    let world = World.createGroup typeof<GroupDispatcher>.Name None (Some ^ Name.make groupName) Simulants.EditorScreen world |> snd
                    refreshGroupTabs form world
                    form.groupTabs.SelectTab (form.groupTabs.TabPages.IndexOfKey groupName)
                    world
                with exn ->
                    let world = World.choose world
                    ignore ^ MessageBox.Show (scstring exn, "Group creation error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    world
            groupNameEntryForm.Close ()
        groupNameEntryForm.cancelButton.Click.Add (fun _ -> groupNameEntryForm.Close ())
        groupNameEntryForm.ShowDialog form |> ignore

    let private handleFormSave (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let groupNameStr = Name.getNameStr (World.getUserState world).SelectedGroup.GroupName
            form.saveFileDialog.Title <- "Save '" + groupNameStr + "' As"
            form.saveFileDialog.FileName <- String.Empty
            let saveFileResult = form.saveFileDialog.ShowDialog form
            match saveFileResult with
            | DialogResult.OK -> trySaveSelectedGroup form.saveFileDialog.FileName world; world
            | _ -> world

    let private handleFormOpen (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            form.openFileDialog.FileName <- String.Empty
            let openFileResult = form.openFileDialog.ShowDialog form
            match openFileResult with
            | DialogResult.OK ->
                let world = pushPastWorld world world
                let world = tryLoadSelectedGroup form form.openFileDialog.FileName world
                deselectEntity form world
                world
            | _ -> world

    let private handleFormClose (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match form.groupTabs.TabPages.Count with
            | 1 ->
                ignore ^ MessageBox.Show ("Cannot destroy only remaining group.", "Group destruction error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                world
            | _ ->
                let world = pushPastWorld world world
                let group = (World.getUserState world).SelectedGroup
                let world = World.destroyGroupImmediate group world
                deselectEntity form world
                form.groupTabs.TabPages.RemoveByKey ^ Name.getNameStr group.GroupName
                world

    let private handleFormUndo (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
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
                world

    let private handleFormRedo (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
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
                world

    let private handleFormTickingChanged (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let tickRate = if form.tickingButton.Checked then 1L else 0L
            let (pastWorld, world) = (world, World.setTickRate tickRate world)
            if tickRate = 1L then pushPastWorld pastWorld world else world

    let private handleFormResetTickTime (_ : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let (pastWorld, world) = (world, World.resetTickTime world)
            pushPastWorld pastWorld world

    let private handleFormCopy (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds -> World.copyToClipboard entityTds.DescribedEntity world; world
            | _ -> Log.trace ^ "Invalid copy operation (likely a code issue in Gaia)."; world

    let private handleFormCut (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let world = World.cutToClipboard entityTds.DescribedEntity world
                deselectEntity form world
                world
            | _ -> Log.trace ^ "Invalid cut operation (likely a code issue in Gaia)."; world

    let private handleFormPaste atMouse (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let world = pushPastWorld world world
            let selectedGroup = (World.getUserState world).SelectedGroup
            let (positionSnap, rotationSnap) = getSnaps form
            let editorState = World.getUserState world
            let (optEntity, world) = World.pasteFromClipboard atMouse editorState.RightClickPosition positionSnap rotationSnap selectedGroup world
            match optEntity with
            | Some entity -> selectEntity form entity world; world
            | None -> world

    let private handleFormQuickSize (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let entity = entityTds.DescribedEntity
                let world = entity.SetSize (World.getEntityQuickSize entity world) world
                let world = World.propagateEntityPhysics entity world
                entityTds.RefWorld := world // must be set for property grid
                form.propertyGrid.Refresh ()
                world
            | _ -> Log.trace ^ "Invalid quick size operation (likely a code issue in Gaia)."; world

    let private handleFormResetCamera (_ : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            World.setEyeCenter Vector2.Zero world

    let private handleFormReloadAssets (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match tryReloadAssets form world with
            | Right (assetGraph, world) ->
                let assetGraphKeywords0 = match typeof<AssetGraph>.GetCustomAttribute<SyntaxAttribute> true with null -> "" | syntax -> syntax.Keywords0
                form.assetGraphTextBox.Text <- Symbol.prettyPrint assetGraphKeywords0 ^ scstring assetGraph + "\r\n"
                world
            | Left error ->
                ignore ^ MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset reload error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                world

    let private handleFormGroupTabSelected (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let world = unsubscribeFromEntityEvents world
            let world =
                World.updateUserState (fun editorState ->
                    let groupTabs = form.groupTabs
                    let groupTab = groupTabs.SelectedTab
                    { editorState with SelectedGroup = stog Simulants.EditorScreen ^ Name.make groupTab.Text })
                    world
            let world = subscribeToEntityEvents form world
            deselectEntity form world
            refreshPropertyGrid form world
            refreshTreeView form world
            world

    let private handleTraceEventsCheckBoxChanged (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            World.setEventTracing form.traceEventsCheckBox.Checked world

    let private handleApplyEventFilterClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            try let eventFilter = scvalue<EventFilter.Filter> form.eventFilterTextBox.Text
                let eventFilterKeywords0 = match typeof<EventFilter.Filter>.GetCustomAttribute<SyntaxAttribute> true with null -> "" | syntax -> syntax.Keywords0
                let world = World.setEventFilter eventFilter world
                form.eventFilterTextBox.Text <- (Symbol.prettyPrint eventFilterKeywords0 ^ scstring eventFilter) + "\r\n"
                world
            with exn ->
                let world = World.choose world
                ignore ^ MessageBox.Show ("Invalid event filter due to: " + scstring exn, "Invalid event filter", MessageBoxButtons.OK, MessageBoxIcon.Error)
                world

    let private handleResetEventFilterClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let eventFilter = World.getEventFilter world
            let eventFilterStr = scstring eventFilter
            let eventFilterKeywords0 = match typeof<EventFilter.Filter>.GetCustomAttribute<SyntaxAttribute> true with null -> "" | syntax -> syntax.Keywords0
            let eventFilterPretty = Symbol.prettyPrint eventFilterKeywords0 eventFilterStr
            form.eventFilterTextBox.Text <- eventFilterPretty + "\r\n"
            form.eventFilterTextBox.EmptyUndoBuffer ()
            world

    let private handleSaveAssetGraphClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            if trySaveAssetGraph form world then
                match tryReloadAssets form world with
                | Right (_, world) -> world
                | Left error ->
                    ignore ^ MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset reload error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    world
            else World.choose world

    let private handleLoadAssetGraphClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match tryLoadAssetGraph form world with
            | Some world -> world
            | None -> World.choose world

    let private handleSaveOverlayerClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            if trySaveOverlayer form world then
                match tryReloadOverlays form world with
                | Right (_, world) -> world
                | Left error ->
                    ignore ^ MessageBox.Show ("Overlayer reload error due to: " + error + "'.", "Overlayer reload error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    world
            else World.choose world

    let private handleLoadOverlayerClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match tryLoadOverlayer form world with
            | Some world -> world
            | None -> World.choose world

    let private handleRolloutTabSelectedIndexChanged (form : GaiaForm) (args : EventArgs) =
        if form.rolloutTabControl.SelectedTab = form.eventTracingTabPage then
            handleResetEventFilterClick form args
        elif form.rolloutTabControl.SelectedTab = form.assetGraphTabPage then
            handleLoadAssetGraphClick form args
        elif form.rolloutTabControl.SelectedTab = form.overlayerTabPage then
            handleLoadOverlayerClick form args

    let private handleFormClosing (_ : GaiaForm) (args : CancelEventArgs) =
        match MessageBox.Show ("Are you sure you want to close Gaia?", "Close Gaia?", MessageBoxButtons.YesNo) with
        | DialogResult.No -> args.Cancel <- true
        | _ -> ()

    let private updateEntityDrag (form : GaiaForm) world =
        if not ^ canEditWithMouse form world then
            match (World.getUserState world).DragEntityState with
            | DragEntityPosition (pickOffset, mousePositionWorldOrig, entity) ->
                let (positionSnap, _) = getSnaps form
                let mousePosition = World.getMousePositionF world
                let mousePositionWorld = World.mouseToWorld (entity.GetViewType world) mousePosition world
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

    let private updateCameraDrag (_ : GaiaForm) world =
        match (World.getUserState world).DragCameraState with
        | DragCameraPosition (pickOffset, mousePositionScreenOrig) ->
            let mousePosition = World.getMousePositionF world
            let mousePositionScreen = World.mouseToScreen mousePosition world
            let eyeCenter = (pickOffset - mousePositionScreenOrig) + -CameraSpeed * (mousePositionScreen - mousePositionScreenOrig)
            let world = World.setEyeCenter eyeCenter world
            World.updateUserState (fun editorState ->
                { editorState with DragCameraState = DragCameraPosition (pickOffset, mousePositionScreenOrig) })
                world
        | DragCameraNone -> world

    // TODO: remove code duplication with below
    let private updateUndoButton (form : GaiaForm) world =
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty (World.getUserState world).PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not ^ List.isEmpty (World.getUserState world).PastWorlds then
            form.undoButton.Enabled <- true
            form.undoToolStripMenuItem.Enabled <- true

    let private updateRedoButton (form : GaiaForm) world =
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

    /// Attach Gaia to the given world.
    let attachToWorld targetDir form world =
        match World.getUserState world : obj with
        | :? unit ->
            if World.getSelectedScreen world = Simulants.EditorScreen then
                if World.proxyGroups Simulants.EditorScreen world |> Seq.isEmpty |> not then
                    let world = flip World.updateUserState world (fun _ ->
                        { TargetDir = targetDir
                          RightClickPosition = Vector2.Zero
                          DragEntityState = DragEntityNone
                          DragCameraState = DragCameraNone
                          PastWorlds = []
                          FutureWorlds = []
                          SelectedGroup = Simulants.DefaultEditorGroup })
                    let world = World.subscribe (handleNuMouseRightDown form) Events.MouseRightDown Simulants.Game world
                    let world = World.subscribe (handleNuEntityDragBegin form) Events.MouseLeftDown Simulants.Game world
                    let world = World.subscribe (handleNuEntityDragEnd form) Events.MouseLeftUp Simulants.Game world
                    let world = World.subscribe (handleNuCameraDragBegin form) Events.MouseCenterDown Simulants.Game world
                    let world = World.subscribe (handleNuCameraDragEnd form) Events.MouseCenterUp Simulants.Game world
                    subscribeToEntityEvents form world
                else failwith ^ "Cannot attach Gaia to a world with no groups inside the '" + scstring Simulants.EditorScreen + "' screen."
            else failwith ^ "Cannot attach Gaia to a world with a screen selected other than '" + scstring Simulants.EditorScreen + "'."
        | :? EditorState -> world // NOTE: conclude world is already attached
        | _ -> failwith "Cannot attach Gaia to a world that has a user state of a type other than unit or EditorState."

    let private run3 runWhile targetDir sdlDeps (form : GaiaForm) =
        let world = World.choose !RefWorld
        let world = attachToWorld targetDir form world
        populateCreateComboBox form world
        populateTreeViewGroups form world
        populateGroupTabs form world
        form.tickingButton.CheckState <- CheckState.Unchecked
        let world =
            World.runWithoutCleanUp
                runWhile
                (fun world -> let world = updateEditorWorld form world in (RefWorld := world; world))
                (fun world -> form.displayPanel.Invalidate (); world)
                sdlDeps
                Running
                world
        RefWorld := world
        world

    /// Select a target directory for the desired plugin and its assets from the give file path.
    let selectTargetDirAndMakeNuPluginFromFilePath filePath =
        let dirName = Path.GetDirectoryName filePath
        Directory.SetCurrentDirectory dirName
        let assembly = Assembly.LoadFrom filePath
        let assemblyTypes = assembly.GetTypes ()
        let optDispatcherType = Array.tryFind (fun (ty : Type) -> ty.IsSubclassOf typeof<NuPlugin>) assemblyTypes
        match optDispatcherType with
        | Some ty -> let plugin = Activator.CreateInstance ty :?> NuPlugin in (dirName, plugin)
        | None -> (".", NuPlugin ())

    /// Select a target directory for the desired plugin and its assets.
    let selectTargetDirAndMakeNuPlugin () =
        use openDialog = new OpenFileDialog ()
        openDialog.Filter <- "Executable Files (*.exe)|*.exe"
        openDialog.Title <- "Select your game's executable file to make its assets and components available in the editor (or cancel for defaults)"
        if openDialog.ShowDialog () = DialogResult.OK
        then selectTargetDirAndMakeNuPluginFromFilePath openDialog.FileName
        else (".", NuPlugin ())

    /// Create a Gaia form.
    let createForm () =

        // create form
        let form = new GaiaForm ()

        // configure controls
        form.displayPanel.MaximumSize <- Drawing.Size (Constants.Render.ResolutionX, Constants.Render.ResolutionY)
        form.positionSnapTextBox.Text <- scstring DefaultPositionSnap
        form.rotationSnapTextBox.Text <- scstring DefaultRotationSnap
        form.createDepthTextBox.Text <- scstring DefaultCreationDepth

        // sort tree view nodes with a bias against guids
        form.treeView.Sorted <- true
        form.treeView.TreeViewNodeSorter <-
            { new IComparer with
                member this.Compare (left, right) =
                    let leftName = ((left :?> TreeNode).Name.Split [|'/'|]) |> Array.last
                    let rightName = ((right :?> TreeNode).Name.Split [|'/'|]) |> Array.last
                    let leftNameBiased = if isGuid leftName then "~" + leftName else leftName
                    let rightNameBiased = if isGuid rightName then "~" + rightName else rightName
                    String.CompareOrdinal (leftNameBiased, rightNameBiased) }

        // set up events handlers
        form.exitToolStripMenuItem.Click.Add (handleFormExit form)
        form.createDepthPlusButton.Click.Add (handleFormCreateDepthPlusClick form)
        form.createDepthMinusButton.Click.Add (handleFormCreateDepthMinusClick form)
        form.rolloutTabControl.SelectedIndexChanged.Add (handleRolloutTabSelectedIndexChanged form)
        form.propertyGrid.SelectedObjectsChanged.Add (handleFormPropertyGridSelectedObjectsChanged form)
        form.propertyGrid.SelectedGridItemChanged.Add (handleFormPropertyGridSelectedGridItemChanged form)
        form.propertyRefreshButton.Click.Add (handleFormPropertyRefreshClick form)
        form.propertyApplyButton.Click.Add (handleFormPropertyApplyClick form)
        form.traceEventsCheckBox.CheckStateChanged.Add (handleTraceEventsCheckBoxChanged form)
        form.applyEventFilterButton.Click.Add (handleApplyEventFilterClick form)
        form.resetEventFilterButton.Click.Add (handleResetEventFilterClick form)
        form.saveAssetGraphButton.Click.Add (handleSaveAssetGraphClick form)
        form.loadAssetGraphButton.Click.Add (handleLoadAssetGraphClick form)
        form.saveOverlayerButton.Click.Add (handleSaveOverlayerClick form)
        form.loadOverlayerButton.Click.Add (handleLoadOverlayerClick form)
        form.treeView.AfterSelect.Add (handleFormTreeViewNodeSelect form)
        form.createEntityButton.Click.Add (handleFormCreate false form)
        form.createToolStripMenuItem.Click.Add (handleFormCreate false form)
        form.createContextMenuItem.Click.Add (handleFormCreate true form)
        form.deleteEntityButton.Click.Add (handleFormDelete form)
        form.deleteToolStripMenuItem.Click.Add (handleFormDelete form)
        form.deleteContextMenuItem.Click.Add (handleFormDelete form)
        form.newToolStripMenuItem.Click.Add (handleFormNew form)
        form.saveToolStripMenuItem.Click.Add (handleFormSave form)
        form.openToolStripMenuItem.Click.Add (handleFormOpen form)
        form.closeToolStripMenuItem.Click.Add (handleFormClose form)
        form.undoButton.Click.Add (handleFormUndo form)
        form.undoToolStripMenuItem.Click.Add (handleFormUndo form)
        form.redoButton.Click.Add (handleFormRedo form)
        form.redoToolStripMenuItem.Click.Add (handleFormRedo form)
        form.tickingButton.CheckedChanged.Add (handleFormTickingChanged form)
        form.resetTickTime.Click.Add (handleFormResetTickTime form)
        form.cutToolStripMenuItem.Click.Add (handleFormCut form)
        form.cutContextMenuItem.Click.Add (handleFormCut form)
        form.copyToolStripMenuItem.Click.Add (handleFormCopy form)
        form.copyContextMenuItem.Click.Add (handleFormCopy form)
        form.pasteToolStripMenuItem.Click.Add (handleFormPaste false form)
        form.pasteContextMenuItem.Click.Add (handleFormPaste true form)
        form.quickSizeToolStripButton.Click.Add (handleFormQuickSize form)
        form.resetCameraButton.Click.Add (handleFormResetCamera form)
        form.reloadAssetsButton.Click.Add (handleFormReloadAssets form)
        form.groupTabs.Selected.Add (handleFormGroupTabSelected form)
        form.Closing.Add (handleFormClosing form)
        
        // populate event filter keywords
        match typeof<EventFilter.Filter>.GetCustomAttribute<SyntaxAttribute> true with
        | null -> ()
        | syntax ->
            form.eventFilterTextBox.Keywords0 <- syntax.Keywords0
            form.eventFilterTextBox.Keywords1 <- syntax.Keywords1

        // populate asset graph keywords
        match typeof<AssetGraph>.GetCustomAttribute<SyntaxAttribute> true with
        | null -> ()
        | syntax ->
            form.assetGraphTextBox.Keywords0 <- syntax.Keywords0
            form.assetGraphTextBox.Keywords1 <- syntax.Keywords1

        // finally, show form
        form.Show ()
        form

    /// Attempt to make a world for use in the Gaia form.
    /// You can make your own world instead and use the Gaia.attachToWorld instead (so long as the world satisfies said
    /// function's various requirements.
    let attemptMakeWorld plugin sdlDeps =
        let eitherWorld = World.attemptMake false None 0L () plugin sdlDeps
        match eitherWorld with
        | Right world ->
            let world = World.createScreen typeof<ScreenDispatcher>.Name None (Some Simulants.EditorScreen.ScreenName) world |> snd
            let world = World.createGroup typeof<GroupDispatcher>.Name None (Some Simulants.DefaultEditorGroup.GroupName) Simulants.EditorScreen world |> snd
            let world = World.setSelectedScreen Simulants.EditorScreen world
            Right world
        | Left error -> Left error

    /// Attempt to make SdlDeps needed to use in the Gaia form.
    let attemptMakeSdlDeps (form : GaiaForm) =
        let sdlViewConfig = ExistingWindow form.displayPanel.Handle
        let sdlRendererFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
        let sdlConfig =
            { ViewConfig = sdlViewConfig
              ViewW = form.displayPanel.MaximumSize.Width
              ViewH = form.displayPanel.MaximumSize.Height
              RendererFlags = sdlRendererFlags
              AudioChunkSize = Constants.Audio.DefaultBufferSize }
        SdlDeps.attemptMake sdlConfig

    /// Run Gaia from the F# repl.
    let runFromRepl runWhile targetDir sdlDeps form world =
        WorldChangers.Clear ()
        RefWorld := world
        run3 runWhile targetDir sdlDeps form

    /// Run the Gaia in isolation.
    let run () =
        let (targetDir, plugin) = selectTargetDirAndMakeNuPlugin ()
        use form = createForm ()
        match attemptMakeSdlDeps form with
        | Right sdlDeps ->
            match attemptMakeWorld plugin sdlDeps with
            | Right world ->
                RefWorld := world
                run3 tautology targetDir sdlDeps form |> ignore
                Constants.Engine.SuccessExitCode
            | Left error -> failwith error
        | Left error -> failwith error