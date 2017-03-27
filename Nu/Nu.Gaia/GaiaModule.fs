// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu.Gaia
open Nu.Gaia.Design
open SDL2
open OpenTK
open System
open System.IO
open System.Collections
open System.ComponentModel
open System.Linq
open System.Reflection
open System.Windows.Forms
open Prime
open Nu

[<RequireQualifiedAccess>]
module Gaia =

    // globals needed to sync Nu with WinForms
    let private RefWorld = ref Unchecked.defaultof<World>
    let private WorldChangers = WorldChangers ()

    let addWorldChanger worldChanger =
        WorldChangers.Add worldChanger |> ignore

    let pushPastWorld pastWorld world =
        World.updateUserState
            (fun editorState -> { editorState with PastWorlds = pastWorld :: editorState.PastWorlds; FutureWorlds = [] })
            world

    let private getPickableEntities world =
        let selectedLayer = (World.getUserState world).SelectedLayer
        let (entities, world) = World.getEntitiesInView Simulants.EditorScreen world
        let entitiesInLayer = Enumerable.ToList (Enumerable.Where (entities, fun entity -> etol entity = selectedLayer))
        (entitiesInLayer, world)

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
            let treeCategoryNode = TreeNode (entity.GetName world)
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

    let private populateTreeViewLayers (form : GaiaForm) world =
        for dispatcherKvp in World.getEntityDispatchers world do
            let treeNode = TreeNode dispatcherKvp.Key
            treeNode.Name <- treeNode.Text
            form.treeView.Nodes.Add treeNode |> ignore

    let private populateTreeViewNodes (form : GaiaForm) world =
        let selectedLayer = (World.getUserState world).SelectedLayer
        for entity in World.getEntities selectedLayer world do
            addTreeViewNode form entity world

    let private populateLayerTabs (form : GaiaForm) world =

        // add layers imperatively to preserve existing layer tabs 
        let layers = World.getLayers Simulants.EditorScreen world
        let layerTabPages = form.layerTabs.TabPages
        for layer in layers do
            let layerName = layer.LayerName
            if not ^ layerTabPages.ContainsKey layerName then
                layerTabPages.Add (layerName, layerName)
    
        // remove layers imperatively to preserve existing layer tabs 
        for layerTabPage in layerTabPages do
            if Seq.notExists (fun (layer : Layer) -> layer.LayerName = layerTabPage.Name) layers then
                layerTabPages.RemoveByKey layerTabPage.Name

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
            if entityTds.DescribedEntity.GetExists world
            then form.propertyGrid.Refresh ()
            else deselectEntity form world
        | _ -> ()

    let private refreshTreeView (form : GaiaForm) world =
        let treeState = getExpansionState form.treeView
        clearTreeViewNodes form
        populateTreeViewLayers form world
        populateTreeViewNodes form world
        restoreExpansionState form.treeView treeState
        setTreeViewSelectionToPropertyGridSelection form

    let private refreshLayerTabs (form : GaiaForm) world =
        populateLayerTabs form world

    let private refreshFormOnUndoRedo (form : GaiaForm) world =
        form.tickingButton.Checked <- false
        refreshPropertyGrid form world
        refreshTreeView form world
        refreshLayerTabs form world

    let private canEditWithMouse (form : GaiaForm) world =
        World.isTicking world &&
        not form.editWhileInteractiveCheckBox.Checked

    let private tryMousePick (form : GaiaForm) mousePosition world =
        let (entities, world) = getPickableEntities world
        let pickedOpt = World.tryPickEntity mousePosition entities world
        match pickedOpt with
        | Some entity ->
            selectEntity form entity world
            (Some entity, world)
        | None -> (None, world)

    let private handleNuEntityRegister (form : GaiaForm) evt world =
        let entity = Entity (atoa evt.Publisher.ParticipantAddress)
        addTreeViewNode form entity world
        selectEntity form entity world
        (Cascade, world)

    let private handleNuEntityUnregistering (form : GaiaForm) evt world =
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
            Log.trace "Unexpected match failure in Nu.Gaia.Gaia.handleNuEntityUnregistering (probably a bug in Gaia or Nu)."
            (Cascade, world)

    let private handleNuMouseRightDown (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let handled = if World.isTicking world then Cascade else Resolve
        let mousePosition = World.getMousePositionF world
        let (_, world) = tryMousePick form mousePosition world
        let world = World.updateUserState (fun editorState -> { editorState with RightClickPosition = mousePosition }) world
        (handled, world)

    let private handleNuEntityDragBegin (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        if not ^ canEditWithMouse form world then
            let handled = if World.isTicking world then Cascade else Resolve
            let mousePosition = World.getMousePositionF world
            match tryMousePick form mousePosition world with
            | (Some entity, world) ->
                let world = pushPastWorld world world
                let world =
                    World.updateUserState (fun editorState ->
                        let mousePositionWorld = World.mouseToWorld (entity.GetViewType world) mousePosition world
                        let entityPosition =
                            if entity.HasFacet typeof<MountFacet> world && entity.NodeExists world
                            then entity.GetPositionLocal world
                            else entity.GetPosition world
                        { editorState with DragEntityState = DragEntityPosition (entityPosition + mousePositionWorld, mousePositionWorld, entity) })
                        world
                (handled, world)
            | (None, world) -> (handled, world)
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
        let selectedLayer = (World.getUserState world).SelectedLayer
        let world = World.subscribePlus Constants.SubscriptionKeys.RegisterEntity (handleNuEntityRegister form) (Events.EntityRegister ->- selectedLayer ->- Events.Wildcard) Simulants.Game world |> snd
        let world = World.subscribePlus Constants.SubscriptionKeys.UnregisteringEntity (handleNuEntityUnregistering form) (Events.EntityUnregistering ->- selectedLayer ->- Events.Wildcard) Simulants.Game world |> snd
        world

    let private unsubscribeFromEntityEvents world =
        let world = World.unsubscribe Constants.SubscriptionKeys.RegisterEntity world
        let world = World.unsubscribe Constants.SubscriptionKeys.UnregisteringEntity world
        world

    let private trySaveSelectedLayer filePath world =
        let selectedLayer = (World.getUserState world).SelectedLayer
        try World.writeLayerToFile filePath selectedLayer world
        with exn ->
            World.choose world |> ignore
            MessageBox.Show ("Could not save file due to: " + scstring exn, "File save error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore

    let private tryLoadSelectedLayer (form : GaiaForm) filePath world =
        
        // old world in case we need to rewind
        let oldWorld = world

        try // destroy current layer
            let selectedLayer = (World.getUserState world).SelectedLayer
            let world = unsubscribeFromEntityEvents world
            let world = World.destroyLayerImmediate selectedLayer world

            // load and add layer, updating tab and selected layer in the process
            let layerDescriptorStr = File.ReadAllText filePath
            let layerDescriptor = scvalue<LayerDescriptor> layerDescriptorStr
            let layerName = match layerDescriptor.LayerProperties.TryFind "Name" with Some (Atom (name, _)) -> name | _ -> failwithumf ()
            let layer = Simulants.EditorScreen => layerName            
            if not (layer.GetExists world) then
                let (layer, world) = World.readLayer layerDescriptor None Simulants.EditorScreen world
                let layerName = layer.GetName world
                form.layerTabs.SelectedTab.Text <- layerName
                form.layerTabs.SelectedTab.Name <- layerName
                let world = World.updateUserState (fun editorState -> { editorState with SelectedLayer = layer }) world
                let world = subscribeToEntityEvents form world

                // refresh tree view
                refreshTreeView form world
                world
            
            // handle load failure
            else
                let world = World.choose oldWorld
                MessageBox.Show ("Could not load layer file with same name as an existing layer", "File load error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

        // handle load failure
        with exn ->
            let world = World.choose oldWorld
            MessageBox.Show ("Could not load layer file due to: " + scstring exn, "File load error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
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
                form.propertyNameLabel.Text <- selectedGridItem.Label
                form.propertyDescriptionTextBox.Text <- selectedGridItem.PropertyDescriptor.Description
                if isNotNull selectedGridItem.Value || isNullTrueValue ty then
                    let (keywords0, keywords1, prettyPrinter) =
                        match selectedGridItem.Label with
                        | "OverlayNameOpt" ->
                            let overlays = World.getIntrinsicOverlays !RefWorld @ World.getExtrinsicOverlays !RefWorld
                            let overlayNames = List.map (fun overlay -> overlay.OverlayName) overlays
                            (String.concat " " overlayNames, "", PrettyPrinter.defaulted)
                        | "FacetNames" ->
                            let facetNames = !RefWorld |> World.getFacets |> Map.toKeyList
                            (String.concat " " facetNames, "", PrettyPrinter.defaulted)
                        | _ ->
                            let syntax = SyntaxAttribute.getOrDefault ty
                            (syntax.Keywords0, syntax.Keywords1, syntax.PrettyPrinter)
                    let selectionStart = form.propertyValueTextBox.SelectionStart
                    let strUnescaped = typeConverter.ConvertToString selectedGridItem.Value
                    let strEscaped = String.escape strUnescaped
                    let strPretty = PrettyPrinter.prettyPrint strEscaped prettyPrinter
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
                        match exn.SymbolOpt with
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

    let private tryReloadPrelude (_ : GaiaForm) world =
        let editorState = World.getUserState world
        let targetDir = editorState.TargetDir
        let assetSourceDir = Path.Combine (targetDir, "..\\..")
        World.tryReloadPrelude assetSourceDir targetDir world

    let private tryLoadPrelude (form : GaiaForm) world =
        match tryReloadPrelude form world with
        | Right (preludeStr, world) ->
            form.preludeTextBox.Text <- preludeStr + "\r\n"
            world
        | Left (error, world) ->
            MessageBox.Show ("Could not load prelude due to: " + error + "'.", "Failed to load prelude", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            world

    let private trySavePrelude (form : GaiaForm) world =
        let editorState = World.getUserState world
        let preludeSourceDir = Path.Combine (editorState.TargetDir, "..\\..")
        let preludeFilePath = Path.Combine (preludeSourceDir, Assets.PreludeFilePath)
        try let preludeStr = form.preludeTextBox.Text.TrimEnd ()
            File.WriteAllText (preludeFilePath, preludeStr)
            true
        with exn ->
            MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to save asset graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            false

    let private tryReloadAssetGraph (_ : GaiaForm) world =
        let editorState = World.getUserState world
        let targetDir = editorState.TargetDir
        let assetSourceDir = Path.Combine (targetDir, "..\\..")
        World.tryReloadAssetGraph assetSourceDir targetDir Constants.Editor.RefinementDir world

    let private tryLoadAssetGraph (form : GaiaForm) world =
        match tryReloadAssetGraph form world with
        | Right (assetGraph, world) ->
            let selectionStart = form.propertyValueTextBox.SelectionStart
            let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<AssetGraph>).PrettyPrinter
            form.assetGraphTextBox.Text <- PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter + "\r\n"
            form.assetGraphTextBox.SelectionStart <- selectionStart
            form.assetGraphTextBox.ScrollCaret ()
            world
        | Left (error, world) ->
            MessageBox.Show ("Could not load asset graph due to: " + error + "'.", "Failed to load asset graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            world

    let private trySaveAssetGraph (form : GaiaForm) world =
        let editorState = World.getUserState world
        let assetSourceDir = Path.Combine (editorState.TargetDir, "..\\..")
        let assetGraphFilePath = Path.Combine (assetSourceDir, Assets.AssetGraphFilePath)
        try let packageDescriptorsStr = form.assetGraphTextBox.Text.TrimEnd () |> scvalue<Map<string, PackageDescriptor>> |> scstring
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<AssetGraph>).PrettyPrinter
            File.WriteAllText (assetGraphFilePath, PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter)
            true
        with exn ->
            MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to save asset graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            false

    let private tryReloadOverlays (_ : GaiaForm) world =
        let targetDir = (World.getUserState world).TargetDir
        let overlayDir = Path.Combine (targetDir, "..\\..")
        World.tryReloadOverlays overlayDir targetDir world

    let private tryLoadOverlayer (form : GaiaForm) world =
        match tryReloadOverlays form world with
        | Right (overlayer, world) ->
            let selectionStart = form.propertyValueTextBox.SelectionStart
            let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<Overlay>).PrettyPrinter
            form.overlayerTextBox.Text <- PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter + "\r\n"
            form.overlayerTextBox.SelectionStart <- selectionStart
            form.overlayerTextBox.ScrollCaret ()
            world
        | Left (error, world) ->
            MessageBox.Show ("Could not reload overlayer due to: " + error + "'.", "Failed to reload overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            world

    let private trySaveOverlayer (form : GaiaForm) world =
        let editorState = World.getUserState world
        let overlayerSourceDir = Path.Combine (editorState.TargetDir, "..\\..")
        let overlayerFilePath = Path.Combine (overlayerSourceDir, Assets.OverlayerFilePath)
        try let overlays = scvalue<Overlay list> ^ form.overlayerTextBox.Text.TrimEnd ()
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<Overlay>).PrettyPrinter
            File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
            true
        with exn ->
            MessageBox.Show ("Could not save overlayer due to: " + scstring exn, "Failed to save overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
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
            if  isNotNull form.treeView.SelectedNode &&
                form.treeView.SelectedNode.Level = 1 then
                let entity = Entity (Address.makeFromString form.treeView.SelectedNode.Name)
                match Address.getNames entity.EntityAddress with
                | [_; _; _] ->
                    RefWorld := world // must be set for property grid
                    let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = WorldChangers; RefWorld = RefWorld }
                    form.propertyGrid.SelectedObject <- entityTds
                    world
                | _ -> world // don't have an entity address
            else world

    let private handleFormCreateEntity atMouse (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            try let world = pushPastWorld world world
                let selectedLayer = (World.getUserState world).SelectedLayer
                let (entity, world) = World.createEntity5 form.createEntityComboBox.Text (Some form.specializationTextBox.Text) None selectedLayer world
                let (positionSnap, rotationSnap) = getSnaps form
                let mousePosition = World.getMousePositionF world
                let entityPosition =
                    if atMouse
                    then World.mouseToWorld (entity.GetViewType world) mousePosition world
                    else World.mouseToWorld (entity.GetViewType world) (World.getEyeSize world * 0.5f) world
                let entityTransform =
                    { Transform.Position = entityPosition
                      Size = entity.GetQuickSize world
                      Rotation = entity.GetRotation world
                      Depth = getCreationDepth form }
                let world = entity.SetTransformSnapped positionSnap rotationSnap entityTransform world
                let world = entity.PropagatePhysics world
                RefWorld := world // must be set for property grid
                let entityTds = { DescribedEntity = entity; Form = form; WorldChangers = WorldChangers; RefWorld = RefWorld }
                form.propertyGrid.SelectedObject <- entityTds
                world
            with exn ->
                let world = World.choose world
                MessageBox.Show (scstring exn, "Could not create entity", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleFormDeleteEntity (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let world = pushPastWorld world world
            match form.propertyGrid.SelectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = World.destroyEntity entityTds.DescribedEntity world
                deselectEntity form world
                world
            | _ -> world

    let private handleFormNew (form : GaiaForm) (_ : EventArgs) =
        use layerCreationForm = new LayerCreationForm ()
        layerCreationForm.StartPosition <- FormStartPosition.CenterParent
        layerCreationForm.dispatcherTextBox.Text <- typeof<LayerDispatcher>.Name
        layerCreationForm.specializationTextBox.Text <- Constants.Engine.EmptySpecialization
        layerCreationForm.depthTextBox.Text <- scstring 0.0f
        layerCreationForm.okButton.Click.Add ^ fun _ ->
            addWorldChanger ^ fun world ->
                let world = pushPastWorld world world
                let layerName = layerCreationForm.nameTextBox.Text
                let layerDispatcher = layerCreationForm.dispatcherTextBox.Text
                let layerSpecialization = layerCreationForm.specializationTextBox.Text
                try if String.length layerName = 0 then failwith "Layer name cannot be empty in Gaia due to WinForms limitations."
                    let layerDepth = Single.Parse layerCreationForm.depthTextBox.Text
                    let (layer, world) = World.createLayer5 layerDispatcher (Some layerSpecialization) (Some layerName) Simulants.EditorScreen world
                    let world = layer.SetDepth layerDepth world
                    refreshLayerTabs form world
                    form.layerTabs.SelectTab (form.layerTabs.TabPages.IndexOfKey layerName)
                    world
                with exn ->
                    let world = World.choose world
                    MessageBox.Show ("Could not create layer due to: " + scstring exn, "Layer creation error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            layerCreationForm.Close ()
        layerCreationForm.cancelButton.Click.Add (fun _ -> layerCreationForm.Close ())
        layerCreationForm.ShowDialog form |> ignore

    let private handleFormSave (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let layerName = (World.getUserState world).SelectedLayer.LayerName
            form.saveFileDialog.Title <- "Save '" + layerName + "' As"
            form.saveFileDialog.FileName <- String.Empty
            let saveFileResult = form.saveFileDialog.ShowDialog form
            match saveFileResult with
            | DialogResult.OK -> trySaveSelectedLayer form.saveFileDialog.FileName world; world
            | _ -> world

    let private handleFormOpen (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            form.openFileDialog.FileName <- String.Empty
            let openFileResult = form.openFileDialog.ShowDialog form
            match openFileResult with
            | DialogResult.OK ->
                let world = pushPastWorld world world
                let world = tryLoadSelectedLayer form form.openFileDialog.FileName world
                deselectEntity form world
                world
            | _ -> world

    let private handleFormClose (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match form.layerTabs.TabPages.Count with
            | 1 ->
                MessageBox.Show ("Cannot destroy the only remaining layer.", "Layer destruction error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world
            | _ ->
                let world = pushPastWorld world world
                let layer = (World.getUserState world).SelectedLayer
                let world = World.destroyLayerImmediate layer world
                deselectEntity form world
                form.layerTabs.TabPages.RemoveByKey layer.LayerName
                World.updateUserState (fun editorState ->
                    let layerTabs = form.layerTabs
                    let layerTab = layerTabs.SelectedTab
                    { editorState with SelectedLayer = stol Simulants.EditorScreen layerTab.Text })
                    world

    let private handleFormUndo (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match (World.getUserState world).PastWorlds with
            | [] -> world
            | pastWorld :: pastWorlds ->
                let futureWorld = world
                let selectedLayer = (World.getUserState pastWorld).SelectedLayer
                let world = World.continueHack selectedLayer pastWorld
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
                let selectedLayer = (World.getUserState futureWorld).SelectedLayer
                let world = World.continueHack selectedLayer futureWorld
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
            | :? EntityTypeDescriptorSource as entityTds -> World.copyEntityToClipboard entityTds.DescribedEntity world; world
            | _ -> Log.trace ^ "Invalid copy operation (likely a code issue in Gaia)."; world

    let private handleFormCut (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let world = World.cutEntityToClipboard entityTds.DescribedEntity world
                deselectEntity form world
                world
            | _ -> Log.trace ^ "Invalid cut operation (likely a code issue in Gaia)."; world

    let private handleFormPaste atMouse (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let world = pushPastWorld world world
            let selectedLayer = (World.getUserState world).SelectedLayer
            let (positionSnap, rotationSnap) = getSnaps form
            let editorState = World.getUserState world
            let (entityOpt, world) = World.pasteEntityFromClipboard atMouse editorState.RightClickPosition positionSnap rotationSnap selectedLayer world
            match entityOpt with
            | Some entity -> selectEntity form entity world; world
            | None -> world

    let private handleFormQuickSize (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match form.propertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = pushPastWorld world world
                let entity = entityTds.DescribedEntity
                let world = entity.SetSize (entity.GetQuickSize world) world
                let world = entity.PropagatePhysics world
                entityTds.RefWorld := world // must be set for property grid
                form.propertyGrid.Refresh ()
                world
            | _ -> Log.trace ^ "Invalid quick size operation (likely a code issue in Gaia)."; world

    let private handleFormResetCamera (_ : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            World.setEyeCenter Vector2.Zero world

    let private handleFormReloadAssets (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            match tryReloadAssetGraph form world with
            | Right (assetGraph, world) ->
                let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<AssetGraph>).PrettyPrinter
                form.assetGraphTextBox.Text <- PrettyPrinter.prettyPrint (scstring assetGraph) prettyPrinter + "\r\n"
                world
            | Left (error, world) ->
                MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset reload error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleFormLayerTabDeselected (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let world = unsubscribeFromEntityEvents world
            deselectEntity form world
            refreshPropertyGrid form world
            refreshTreeView form world
            world

    let private handleFormLayerTabSelected (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let world =
                World.updateUserState (fun editorState ->
                    let layerTabs = form.layerTabs
                    let layerTab = layerTabs.SelectedTab
                    { editorState with SelectedLayer = stol Simulants.EditorScreen layerTab.Text })
                    world
            let world = subscribeToEntityEvents form world
            refreshPropertyGrid form world
            refreshTreeView form world
            world

    let private handleTraceEventsCheckBoxChanged (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            World.setEventTracing form.traceEventsCheckBox.Checked world

    let private handleApplyEventFilterClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            try let eventFilter = scvalue<EventFilter.Filter> form.eventFilterTextBox.Text
                let world = World.setEventFilter eventFilter world
                let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<EventFilter.Filter>).PrettyPrinter
                form.eventFilterTextBox.Text <- PrettyPrinter.prettyPrint (scstring eventFilter) prettyPrinter + "\r\n"
                world
            with exn ->
                let world = World.choose world
                MessageBox.Show ("Invalid event filter due to: " + scstring exn, "Invalid event filter", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleRefreshEventFilterClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let eventFilter = World.getEventFilter world
            let eventFilterStr = scstring eventFilter
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<EventFilter.Filter>).PrettyPrinter
            let eventFilterPretty = PrettyPrinter.prettyPrint eventFilterStr prettyPrinter
            form.eventFilterTextBox.Text <- eventFilterPretty + "\r\n"
            form.eventFilterTextBox.EmptyUndoBuffer ()
            world

    let private handleSavePreludeClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            if trySavePrelude form world then
                match tryReloadPrelude form world with
                | Right (_, world) -> world
                | Left (error, world) ->
                    MessageBox.Show ("Prelude reload error due to: " + error + "'.", "Prelude reload error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            else World.choose world

    let private handleLoadPreludeClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            tryLoadPrelude form world

    let private handleSaveAssetGraphClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            if trySaveAssetGraph form world then
                match tryReloadAssetGraph form world with
                | Right (_, world) -> world
                | Left (error, world) ->
                    MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset reload error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            else World.choose world

    let private handleLoadAssetGraphClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            tryLoadAssetGraph form world

    let private handleSaveOverlayerClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            if trySaveOverlayer form world then
                match tryReloadOverlays form world with
                | Right (_, world) -> world
                | Left (error, world) ->
                    MessageBox.Show ("Overlayer reload error due to: " + error + "'.", "Overlayer reload error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            else World.choose world

    let private handleLoadOverlayerClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            tryLoadOverlayer form world

    let private handleEvalClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger ^ fun world ->
            let exprsStr =
                if String.notEmpty form.evalInputTextBox.SelectedText
                then form.evalInputTextBox.SelectedText
                else form.evalInputTextBox.Text
            let exprsStr = Symbol.OpenSymbolsStr + "\n" + exprsStr + "\n" + Symbol.CloseSymbolsStr
            try let exprs = scvalue<Scripting.Expr array> exprsStr
                let localFrame = Simulants.Game.GetScriptFrameNp world
                let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<Scripting.Expr>).PrettyPrinter
                let (evaleds, world) = World.evalManyWithLogging exprs localFrame Simulants.Game world
                let evaledStrs = Array.map (fun evaled -> PrettyPrinter.prettyPrint (scstring evaled) prettyPrinter) evaleds
                let evaledsStr = String.concat "\n" evaledStrs
                form.evalOutputTextBox.ReadOnly <- false
                form.evalOutputTextBox.Text <-
                    if String.notEmpty form.evalOutputTextBox.Text
                    then form.evalOutputTextBox.Text + evaledsStr + "\n"
                    else evaledsStr + "\n"
                form.evalOutputTextBox.GotoPosition form.evalOutputTextBox.Text.Length
                form.evalOutputTextBox.ReadOnly <- true
                world
            with exn -> Log.debug ("Could not evaluate input due to: " + scstring exn); world

    let private handleClearOutputClick (form : GaiaForm) (_ : EventArgs) =
        form.evalOutputTextBox.Text <- String.Empty

    let private handleCreateEntityComboBoxSelectedIndexChanged (form : GaiaForm) (_ : EventArgs) =
        form.specializationTextBox.Text <- Constants.Engine.EmptySpecialization

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
                let entityPositionSnapped = Math.snap2F positionSnap entityPosition
                let world =
                    if entity.HasFacet typeof<MountFacet> world && entity.NodeExists world
                    then entity.SetPositionLocal entityPositionSnapped world
                    else entity.SetPosition entityPositionSnapped world
                let world = entity.PropagatePhysics world
                let world =
                    World.updateUserState (fun editorState ->
                        { editorState with DragEntityState = DragEntityPosition (pickOffset, mousePositionWorldOrig, entity) })
                        world
                // NOTE: disabled the following line to fix perf issue caused by refreshing the property grid every frame
                // form.propertyGrid.Refresh ()
                world
            | DragEntityRotation _ -> world
            | DragEntityNone -> world
        else world

    let private updateCameraDrag (_ : GaiaForm) world =
        match (World.getUserState world).DragCameraState with
        | DragCameraPosition (pickOffset, mousePositionScreenOrig) ->
            let mousePosition = World.getMousePositionF world
            let mousePositionScreen = World.mouseToScreen mousePosition world
            let eyeCenter = (pickOffset - mousePositionScreenOrig) + -Constants.Editor.CameraSpeed * (mousePositionScreen - mousePositionScreenOrig)
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
                if World.getLayers Simulants.EditorScreen world |> Seq.isEmpty |> not then
                    let world = flip World.updateUserState world (fun _ ->
                        { TargetDir = targetDir
                          RightClickPosition = Vector2.Zero
                          DragEntityState = DragEntityNone
                          DragCameraState = DragCameraNone
                          PastWorlds = []
                          FutureWorlds = []
                          SelectedLayer = Simulants.DefaultEditorLayer })
                    let world = World.subscribePlus (makeGuid ()) (handleNuMouseRightDown form) Events.MouseRightDown Simulants.Game world |> snd
                    let world = World.subscribePlus (makeGuid ()) (handleNuEntityDragBegin form) Events.MouseLeftDown Simulants.Game world |> snd
                    let world = World.subscribePlus (makeGuid ()) (handleNuEntityDragEnd form) Events.MouseLeftUp Simulants.Game world |> snd
                    let world = World.subscribePlus (makeGuid ()) (handleNuCameraDragBegin form) Events.MouseCenterDown Simulants.Game world |> snd
                    let world = World.subscribePlus (makeGuid ()) (handleNuCameraDragEnd form) Events.MouseCenterUp Simulants.Game world |> snd
                    subscribeToEntityEvents form world
                else failwith ^ "Cannot attach Gaia to a world with no layers inside the '" + scstring Simulants.EditorScreen + "' screen."
            else failwith ^ "Cannot attach Gaia to a world with a screen selected other than '" + scstring Simulants.EditorScreen + "'."
        | :? EditorState -> world // NOTE: conclude world is already attached
        | _ -> failwith "Cannot attach Gaia to a world that has a user state of a type other than unit or EditorState."

    let rec private tryRun3 runWhile sdlDeps (form : GaiaForm) =
        try World.runWithoutCleanUp
                runWhile
                (fun world -> let world = updateEditorWorld form world in (RefWorld := world; world))
                (fun world -> form.displayPanel.Invalidate (); world)
                sdlDeps
                Running
                !RefWorld |>
                ignore
        with exn ->
            match MessageBox.Show
                ("Unexpected exception due to: " + scstring exn + "\nWould you like to undo the last operator to try to keep Gaia running?",
                 "Unexpected Exception",
                 MessageBoxButtons.YesNo,
                 MessageBoxIcon.Error) with
            | DialogResult.Yes ->
                form.undoButton.PerformClick ()
                RefWorld := World.choose !RefWorld
                tryRun3 runWhile sdlDeps form
            | _ -> RefWorld := World.choose !RefWorld

    let private run3 runWhile targetDir sdlDeps (form : GaiaForm) =
        RefWorld := attachToWorld targetDir form !RefWorld
        populateCreateComboBox form !RefWorld
        populateTreeViewLayers form !RefWorld
        populateLayerTabs form !RefWorld
        form.tickingButton.CheckState <- CheckState.Unchecked
        tryRun3 runWhile sdlDeps (form : GaiaForm)

    /// Select a target directory for the desired plugin and its assets from the give file path.
    let selectTargetDirAndMakeNuPluginFromFilePath filePath =
        let dirName = Path.GetDirectoryName filePath
        Directory.SetCurrentDirectory dirName
        let assembly = Assembly.LoadFrom filePath
        let assemblyTypes = assembly.GetTypes ()
        let dispatcherTypeOpt = Array.tryFind (fun (ty : Type) -> ty.IsSubclassOf typeof<NuPlugin>) assemblyTypes
        match dispatcherTypeOpt with
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
        form.positionSnapTextBox.Text <- scstring Constants.Editor.DefaultPositionSnap
        form.rotationSnapTextBox.Text <- scstring Constants.Editor.DefaultRotationSnap
        form.createDepthTextBox.Text <- scstring Constants.Editor.DefaultCreationDepth
        form.specializationTextBox.Text <- Constants.Engine.EmptySpecialization

        // sort tree view nodes with a bias against guids
        form.treeView.Sorted <- true
        form.treeView.TreeViewNodeSorter <-
            { new IComparer with
                member this.Compare (left, right) =
                    let leftName = ((left :?> TreeNode).Name.Split Constants.Address.Separator) |> Array.last
                    let rightName = ((right :?> TreeNode).Name.Split Constants.Address.Separator) |> Array.last
                    let leftNameBiased = if String.isGuid leftName then "~" + leftName else leftName
                    let rightNameBiased = if String.isGuid rightName then "~" + rightName else rightName
                    String.CompareOrdinal (leftNameBiased, rightNameBiased) }

        // set up events handlers
        form.exitToolStripMenuItem.Click.Add (handleFormExit form)
        form.createDepthPlusButton.Click.Add (handleFormCreateDepthPlusClick form)
        form.createDepthMinusButton.Click.Add (handleFormCreateDepthMinusClick form)
        form.propertyGrid.SelectedObjectsChanged.Add (handleFormPropertyGridSelectedObjectsChanged form)
        form.propertyGrid.SelectedGridItemChanged.Add (handleFormPropertyGridSelectedGridItemChanged form)
        form.propertyRefreshButton.Click.Add (handleFormPropertyRefreshClick form)
        form.propertyApplyButton.Click.Add (handleFormPropertyApplyClick form)
        form.traceEventsCheckBox.CheckStateChanged.Add (handleTraceEventsCheckBoxChanged form)
        form.applyEventFilterButton.Click.Add (handleApplyEventFilterClick form)
        form.refreshEventFilterButton.Click.Add (handleRefreshEventFilterClick form)
        form.savePreludeButton.Click.Add (handleSavePreludeClick form)
        form.loadPreludeButton.Click.Add (handleLoadPreludeClick form)
        form.saveAssetGraphButton.Click.Add (handleSaveAssetGraphClick form)
        form.loadAssetGraphButton.Click.Add (handleLoadAssetGraphClick form)
        form.saveOverlayerButton.Click.Add (handleSaveOverlayerClick form)
        form.loadOverlayerButton.Click.Add (handleLoadOverlayerClick form)
        form.treeView.AfterSelect.Add (handleFormTreeViewNodeSelect form)
        form.createEntityButton.Click.Add (handleFormCreateEntity false form)
        form.createToolStripMenuItem.Click.Add (handleFormCreateEntity false form)
        form.createContextMenuItem.Click.Add (handleFormCreateEntity true form)
        form.deleteEntityButton.Click.Add (handleFormDeleteEntity form)
        form.deleteToolStripMenuItem.Click.Add (handleFormDeleteEntity form)
        form.deleteContextMenuItem.Click.Add (handleFormDeleteEntity form)
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
        form.layerTabs.Deselected.Add (handleFormLayerTabDeselected form)
        form.layerTabs.Selected.Add (handleFormLayerTabSelected form)
        form.evalButton.Click.Add (handleEvalClick form)
        form.clearOutputButton.Click.Add (handleClearOutputClick form)
        form.createEntityComboBox.SelectedIndexChanged.Add (handleCreateEntityComboBoxSelectedIndexChanged form)
        form.Closing.Add (handleFormClosing form)
        
        // populate event filter keywords
        match typeof<EventFilter.Filter>.GetCustomAttribute<SyntaxAttribute> true with
        | null -> ()
        | syntax ->
            form.eventFilterTextBox.Keywords0 <- syntax.Keywords0
            form.eventFilterTextBox.Keywords1 <- syntax.Keywords1

        // populate evaluator and prelude keywords
        match typeof<Scripting.Expr>.GetCustomAttribute<SyntaxAttribute> true with
        | null -> ()
        | syntax ->
            form.evalInputTextBox.Keywords0 <- syntax.Keywords0
            form.evalInputTextBox.Keywords1 <- syntax.Keywords1
            form.evalOutputTextBox.Keywords0 <- syntax.Keywords0
            form.evalOutputTextBox.Keywords1 <- syntax.Keywords1
            form.preludeTextBox.Keywords0 <- syntax.Keywords0
            form.preludeTextBox.Keywords1 <- syntax.Keywords1

        // populate asset graph keywords
        match typeof<AssetGraph>.GetCustomAttribute<SyntaxAttribute> true with
        | null -> ()
        | syntax ->
            form.assetGraphTextBox.Keywords0 <- syntax.Keywords0
            form.assetGraphTextBox.Keywords1 <- syntax.Keywords1

        // populate overlayer keywords
        match typeof<Overlay>.GetCustomAttribute<SyntaxAttribute> true with
        | null -> ()
        | syntax ->
            form.overlayerTextBox.Keywords0 <- syntax.Keywords0
            form.overlayerTextBox.Keywords1 <- syntax.Keywords1

        // populate rollout tab texts
        handleRefreshEventFilterClick form (EventArgs ())
        handleLoadPreludeClick form (EventArgs ())
        handleLoadAssetGraphClick form (EventArgs ())
        handleLoadOverlayerClick form (EventArgs ())

        // finally, show form
        form.Show ()
        form

    /// Attempt to make a world for use in the Gaia form.
    /// You can make your own world instead and use the Gaia.attachToWorld instead (so long as the world satisfies said
    /// function's various requirements.
    let attemptMakeWorld plugin sdlDeps =
        let worldEir = World.attemptMake false None 0L () plugin sdlDeps
        match worldEir with
        | Right world ->
            let world = World.createScreen None (Some Simulants.EditorScreen.ScreenName) world |> snd
            let world = World.createLayer None (Some Simulants.DefaultEditorLayer.LayerName) Simulants.EditorScreen world |> snd
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

    /// Run Gaia from the F# evaluator.
    let runFromRepl runWhile targetDir sdlDeps form world =
        RefWorld := world
        run3 runWhile targetDir sdlDeps form
        !RefWorld

    /// Run Gaia in isolation.
    let run () =
        let (targetDir, plugin) = selectTargetDirAndMakeNuPlugin ()
        use form = createForm ()
        match attemptMakeSdlDeps form with
        | Right sdlDeps ->
            match attemptMakeWorld plugin sdlDeps with
            | Right world ->
                RefWorld := world
                let _ = run3 tautology targetDir sdlDeps form
                Constants.Engine.SuccessExitCode
            | Left error -> failwith error
        | Left error -> failwith error