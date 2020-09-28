// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open System.IO
open System.Collections
open System.ComponentModel
open System.Linq
open System.Reflection
open System.Runtime.InteropServices
open System.Windows.Forms
open FSharp.Reflection
open Prime
open Nu
open Nu.Gaia
open Nu.Gaia.Design

[<RequireQualifiedAccess>]
module Gaia =

    let mutable private propertyPickButtonClickHandler =
        Unchecked.defaultof<EventHandler>

    let addWorldChanger worldChanger =
        Globals.WorldChangers.Add worldChanger |> ignore

    let private getEditorState world =
        World.getKeyedValue<EditorState> Globals.EditorGuid world

    let private updateEditorState updater world =
        World.updateKeyedValue<EditorState> updater Globals.EditorGuid world

    let private getPickableEntities world =
        let selectedLayer = (getEditorState world).SelectedLayer
        let (entities, world) = World.getEntitiesInView2 world
        let entitiesInLayer = Enumerable.ToList (Enumerable.Where (entities, fun entity -> entity.Parent = selectedLayer))
        (entitiesInLayer, world)

    let private getSnaps (form : GaiaForm) =
        let positionSnap = snd (Int32.TryParse form.positionSnapTextBox.Text)
        let rotationSnap = snd (Int32.TryParse form.rotationSnapTextBox.Text)
        (positionSnap, rotationSnap)
    
    let private getCreationDepth (form : GaiaForm) =
        snd (Single.TryParse form.createDepthTextBox.Text)

    let private getExpansionState (treeView : TreeView) =
        let nodeStates =
            Seq.fold (fun state (node : TreeNode) ->
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

    let private addEntityTreeViewNode (entity : Entity) (form : GaiaForm) world =
        let entityGroupName = getTypeName (entity.GetDispatcher world)
        let treeGroup = form.entityTreeView.Nodes.[entityGroupName]
        let treeGroupNodeName = scstring entity.EntityAddress
        if treeGroup.Nodes.ContainsKey treeGroupNodeName
        then () // when changing an entity name, entity will be added twice - once from win forms, once from world
        else
            let treeGroupNode = TreeNode entity.Name
            treeGroupNode.Name <- treeGroupNodeName
            treeGroup.Nodes.Add treeGroupNode |> ignore

    let private removeEntityTreeViewNode (entity : Simulant) (form : GaiaForm) (_ : World) =
        match form.entityTreeView.Nodes.Find (scstring entity.SimulantAddress, true) with
        | [||] -> () // when changing an entity name, entity will be removed twice - once from winforms, once from world
        | treeNodes -> form.entityTreeView.Nodes.Remove treeNodes.[0]

    let private setEntityTreeViewSelectionToEntityPropertyGridSelection (form : GaiaForm) =
        match form.entityPropertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.entityTreeView.Nodes.Find (scstring entityTds.DescribedEntity.EntityAddress, true) with
            | [||] -> form.entityTreeView.SelectedNode <- null
            | nodes ->
                let node = nodes.[0]
                if node.Parent.IsExpanded then
                    form.entityTreeView.SelectedNode <- node
                    node.EnsureVisible ()
                else form.entityTreeView.SelectedNode <- null
        | _ -> form.entityTreeView.SelectedNode <- null

    let private setHierarchyTreeViewSelectionToEntityPropertyGridSelection (form : GaiaForm) =
        match form.entityPropertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.hierarchyTreeView.Nodes.Find (scstring entityTds.DescribedEntity.EntityAddress, true) with
            | [||] -> form.entityTreeView.SelectedNode <- null
            | entityNodes ->
                let entityNode = entityNodes.[0]
                form.hierarchyTreeView.SelectedNode <- entityNode
                entityNode.EnsureVisible ()
        | _ -> form.hierarchyTreeView.SelectedNode <- null

    let private refreshOverlayComboBox (form : GaiaForm) world =
        form.overlayComboBox.Items.Clear ()
        form.overlayComboBox.Items.Add "(Default Overlay)" |> ignore
        form.overlayComboBox.Items.Add "(Routed Overlay)" |> ignore
        form.overlayComboBox.Items.Add "(No Overlay)" |> ignore
        for overlay in World.getExtrinsicOverlays world do
            form.overlayComboBox.Items.Add overlay.OverlayName |> ignore
        form.overlayComboBox.SelectedIndex <- 0

    let private refreshCreateComboBox (form : GaiaForm) world =
        form.createEntityComboBox.Items.Clear ()
        for dispatcherKvp in World.getEntityDispatchers world do
            form.createEntityComboBox.Items.Add dispatcherKvp.Key |> ignore
        form.createEntityComboBox.SelectedIndex <- 0

    let private refreshEntityTreeView (form : GaiaForm) world =
        let treeState = getExpansionState form.entityTreeView
        form.entityTreeView.Nodes.Clear ()
        for dispatcherKvp in World.getEntityDispatchers world do
            let treeNode = TreeNode dispatcherKvp.Key
            treeNode.Name <- treeNode.Text
            form.entityTreeView.Nodes.Add treeNode |> ignore
        let selectedLayer = (getEditorState world).SelectedLayer
        for entity in World.getEntities selectedLayer world do
            addEntityTreeViewNode entity form world
        restoreExpansionState form.entityTreeView treeState
        setEntityTreeViewSelectionToEntityPropertyGridSelection form

    let private collectHierarchyTreeNodes (form : GaiaForm) (_ : World) =
        let rec collect (node : TreeNode) =
            seq {
                yield node
                yield! node.Nodes.Cast<_> ()
                for child in node.Nodes do
                    yield! collect child }
        let result =
            seq {
                for node in form.hierarchyTreeView.Nodes do
                    yield node
                    yield!
                        if node.Nodes.Count <> 0
                        then collect node.Nodes.[0].Parent
                        else Seq.empty }
        Array.ofSeq result

    let private tryFindHierarchyTreeNode name (form : GaiaForm) (world : World) =
        let nodes = collectHierarchyTreeNodes form world
        Array.tryFind (fun (node : TreeNode) -> node.Name = name) nodes

    let private containsHierarchyTreeNode name (form : GaiaForm) (world : World) =
        Option.isSome (tryFindHierarchyTreeNode name form world)

    let private addHierarchyTreeEntityNode (entity : Entity) (form : GaiaForm) world =
        let entityNodeKey = scstring entity
        let entityNode = TreeNode entity.Name
        let layerNodeKey = scstring entity.Parent
        let layerNode = form.hierarchyTreeView.Nodes.[layerNodeKey]
        entityNode.Name <- entityNodeKey
        if entity.Has<NodeFacet> world then
            match entity.GetParentNodeOpt world with
            | Some relation ->
                let entityParent = resolve entity relation
                let entityParentNodeKey = scstring entityParent
                match tryFindHierarchyTreeNode entityParentNodeKey form world with
                | Some node -> node.Nodes.Add entityNode |> ignore
                | None -> failwithumf ()
            | None -> layerNode.Nodes.Add entityNode |> ignore
        else layerNode.Nodes.Add entityNode |> ignore

    let private removeHierarchyTreeEntityNode (entity : Entity) (form : GaiaForm) world =
        let entityNodeKey = scstring entity
        match tryFindHierarchyTreeNode entityNodeKey form world with
        | Some node -> node.Remove ()
        | None -> failwithumf ()

    let private refreshHierarchyTreeView (form : GaiaForm) world =
        // TODO: this code causes severe performance issues. To unfuck performance, we will probably have to find
        // a way to update the hierarchy tree without a complete rebuild of it - IE, updating it in-place and
        // imperatively.
        let treeState = getExpansionState form.hierarchyTreeView
        form.hierarchyTreeView.Nodes.Clear ()
        let layers = World.getLayers Simulants.DefaultScreen world
        for layer in layers do
            let layerNode = TreeNode layer.Name
            layerNode.Name <- scstring layer
            form.hierarchyTreeView.Nodes.Add layerNode |> ignore
            let entities = World.getEntities layer world
            for entity in entities do
                let mutable parentNode = layerNode
                for entity in entity.GetChildNodes world @ [entity] do
                    let entityNodeKey = scstring entity
                    if not (parentNode.Nodes.ContainsKey entityNodeKey) then
                        let entityNode = TreeNode entity.Name
                        entityNode.Name <- entityNodeKey
                        parentNode.Nodes.Add entityNode |> ignore
                        parentNode <- entityNode
                    else parentNode <- parentNode.Nodes.[entityNodeKey]
        restoreExpansionState form.hierarchyTreeView treeState
        setHierarchyTreeViewSelectionToEntityPropertyGridSelection form

    let private refreshLayerTabs (form : GaiaForm) world =

        // add layers imperatively to preserve existing layer tabs
        // NOTE: adding layers in reverse works better when opening anew
        let layers = World.getLayers Simulants.DefaultScreen world
        let layerTabPages = form.layerTabControl.TabPages
        for layer in Seq.rev layers do
            let layerName = layer.Name
            if not (layerTabPages.ContainsKey layerName) then
                layerTabPages.Add (layerName, layerName)
    
        // remove layers imperatively to preserve existing layer tabs 
        for layerTabPage in layerTabPages do
            if Seq.notExists (fun (layer : Layer) -> layer.Name = layerTabPage.Name) layers then
                layerTabPages.RemoveByKey layerTabPage.Name

    let private selectEntity entity (form : GaiaForm) world =
        Globals.World <- world // must be set for property grid
        let entityTds = { DescribedEntity = entity; Form = form }
        form.entityPropertyGrid.SelectedObject <- entityTds
        form.propertyTabControl.SelectTab 0 // show entity properties

    let private deselectEntity (form : GaiaForm) world =
        Globals.World <- world // must be set for property grid
        form.entityPropertyGrid.SelectedObject <- null

    let private refreshEntityPropertyGrid (form : GaiaForm) world =
        match form.entityPropertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            Globals.World <- world // must be set for property grid
            if entityTds.DescribedEntity.Exists world
            then form.entityPropertyGrid.Refresh ()
            else deselectEntity form world
        | _ -> ()

    let private selectLayer layer (form : GaiaForm) world =
        let layerTds = { DescribedLayer = layer; Form = form }
        Globals.World <- world // must be set for property grid
        form.layerPropertyGrid.SelectedObject <- layerTds

    let private deselectLayer (form : GaiaForm) world =
        Globals.World <- world // must be set for property grid
        form.layerPropertyGrid.SelectedObject <- null

    let private refreshLayerPropertyGrid (form : GaiaForm) world =
        match form.layerPropertyGrid.SelectedObject with
        | :? LayerTypeDescriptorSource as layerTds ->
            Globals.World <- world // must be set for property grid
            if layerTds.DescribedLayer.Exists world
            then form.layerPropertyGrid.Refresh ()
            else deselectLayer form world
        | _ -> ()

    let private refreshFormOnUndoRedo (form : GaiaForm) world =
        form.tickingButton.Checked <- false
        refreshEntityPropertyGrid form world
        refreshLayerPropertyGrid form world
        refreshLayerTabs form world
        refreshEntityTreeView form world
        refreshHierarchyTreeView form world

    let private canEditWithMouse (form : GaiaForm) world =
        World.isTicking world &&
        not form.editWhileInteractiveCheckBox.Checked

    let private tryMousePickInner (form : GaiaForm) mousePosition world =
        let (entities, world) = getPickableEntities world
        let pickedOpt = World.tryPickEntity mousePosition entities world
        match pickedOpt with
        | Some entity ->
            selectEntity entity form world
            (Some entity, world)
        | None -> (None, world)

    let private tryMousePick mousePosition (form : GaiaForm) world =
        match form.entityPropertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            let entity = entityTds.DescribedEntity
            let mousePositionWorld = World.mouseToWorld (entity.GetAbsolute world) mousePosition world
            if Math.isPointInBounds mousePositionWorld (entity.GetBounds world)
            then (Some entity, world)
            else tryMousePickInner form mousePosition world
        | _ -> tryMousePickInner form mousePosition world

    let private handleNuChangeParentNodeOpt form evt world =
        let entity = Entity (atoa evt.Publisher.SimulantAddress)
        removeHierarchyTreeEntityNode entity form world
        addHierarchyTreeEntityNode entity form world
        (Cascade, world)

    let private handleNuEntityRegister (form : GaiaForm) evt world =
        let entity = Entity (atoa evt.Publisher.SimulantAddress)
        addEntityTreeViewNode entity form world
        addHierarchyTreeEntityNode entity form world
        (Cascade, world)

    let private handleNuEntityUnregistering (form : GaiaForm) evt world =
        let entity = Entity (atoa evt.Publisher.SimulantAddress)
        removeEntityTreeViewNode evt.Publisher form world
        removeHierarchyTreeEntityNode entity form world
        match form.entityPropertyGrid.SelectedObject with
        | null -> (Cascade, world)
        | :? EntityTypeDescriptorSource as entityTds ->
            if atoa evt.Publisher.SimulantAddress = entityTds.DescribedEntity.EntityAddress then
                let world = updateEditorState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                deselectEntity form world
                (Cascade, world)
            else (Cascade, world)
        | _ -> failwithumf ()

    let private handleNuMouseRightDown (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let handled = if World.isTicking world then Cascade else Resolve
        let mousePosition = World.getMousePositionF world
        let (_, world) = tryMousePick mousePosition form world
        let world = updateEditorState (fun editorState -> { editorState with RightClickPosition = mousePosition }) world
        (handled, world)

    let private handleNuEntityDragBegin (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        if not (canEditWithMouse form world) then
            let handled = if World.isTicking world then Cascade else Resolve
            let mousePosition = World.getMousePositionF world
            match tryMousePick mousePosition form world with
            | (Some entity, world) ->
                Globals.pushPastWorld world
                let world = entity.Diverge world
                let world =
                    updateEditorState (fun editorState ->
                        let mousePositionWorld = World.mouseToWorld (entity.GetAbsolute world) mousePosition world
                        let entityPosition =
                            if entity.Has<NodeFacet> world && entity.ParentNodeExists world
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
            match (getEditorState world).DragEntityState with
            | DragEntityPosition _
            | DragEntityRotation _ ->
                let world = updateEditorState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                form.entityPropertyGrid.Refresh ()
                (handled, world)
            | DragEntityNone -> (Resolve, world)

    let private handleNuCameraDragBegin (_ : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let mousePosition = World.getMousePositionF world
        let mousePositionScreen = World.mouseToScreen mousePosition world
        let dragState = DragCameraPosition (World.getEyeCenter world + mousePositionScreen, mousePositionScreen)
        let world = updateEditorState (fun editorState -> { editorState with DragCameraState = dragState }) world
        (Resolve, world)

    let private handleNuCameraDragEnd (_ : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        match (getEditorState world).DragCameraState with
        | DragCameraPosition _ ->
            let world = updateEditorState (fun editorState -> { editorState with DragCameraState = DragCameraNone }) world
            (Resolve, world)
        | DragCameraNone -> (Resolve, world)

    let private monitorEntityEvents (layer : Layer) form world =
        let world = World.monitor (handleNuChangeParentNodeOpt form) (Events.Change Property? ParentNodeOpt --> layer --> Events.Wildcard) layer world
        let world = World.monitor (handleNuEntityRegister form) (Events.Register --> layer --> Events.Wildcard) layer world
        let world = World.monitor (handleNuEntityUnregistering form) (Events.Unregistering --> layer --> Events.Wildcard) layer world
        world

    let private trySaveSelectedLayer filePath world =
        let oldWorld = world
        let selectedLayer = (getEditorState world).SelectedLayer
        try World.writeLayerToFile filePath selectedLayer world
        with exn ->
            World.choose oldWorld |> ignore
            MessageBox.Show ("Could not save file due to: " + scstring exn, "File save error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore

    let private tryLoadSelectedLayer (form : GaiaForm) filePath world =

        // old world in case we need to rewind
        let oldWorld = world

        try // destroy current layer
            let selectedLayer = (getEditorState world).SelectedLayer
            let world = World.destroyLayerImmediate selectedLayer world

            // load and add layer, updating tab and selected layer in the process
            let layerDescriptorStr = File.ReadAllText filePath
            let layerDescriptor = scvalue<LayerDescriptor> layerDescriptorStr
            let layerName = match layerDescriptor.LayerProperties.TryFind "Name" with Some (Atom (name, _)) -> name | _ -> failwithumf ()
            let layer = Simulants.DefaultScreen / layerName
            if not (layer.Exists world) then
                let (layer, world) = World.readLayer layerDescriptor None Simulants.DefaultScreen world
                form.layerTabControl.SelectedTab.Text <- layer.Name
                form.layerTabControl.SelectedTab.Name <- layer.Name
                let world = updateEditorState (fun editorState -> { editorState with SelectedLayer = layer }) world
                let world = monitorEntityEvents layer form world

                // refresh tree views
                refreshEntityTreeView form world
                refreshHierarchyTreeView form world
                (Some layer, world)
            
            // handle load failure
            else
                let world = World.choose oldWorld
                MessageBox.Show ("Could not load layer file with same name as an existing layer", "File load error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                (None, world)

        // handle load failure
        with exn ->
            let world = World.choose oldWorld
            MessageBox.Show ("Could not load layer file due to: " + scstring exn, "File load error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            (None, world)

    let private handleFormExit (form : GaiaForm) (_ : EventArgs) =
        form.Close ()

    let private handleFormCreateDepthPlusClick (form : GaiaForm) (_ : EventArgs) =
        let depth = snd (Single.TryParse form.createDepthTextBox.Text)
        form.createDepthTextBox.Text <- scstring (depth + 1.0f)

    let private handleFormCreateDepthMinusClick (form : GaiaForm) (_ : EventArgs) =
        let depth = snd (Single.TryParse form.createDepthTextBox.Text)
        form.createDepthTextBox.Text <- scstring (depth - 1.0f)

    let private handlePropertyPickAsset (form : GaiaForm) world =
        use assetPicker = new AssetPicker ()
        let assetMap = World.getAssetMap world
        for assetTag in assetMap do
            let node = assetPicker.assetTreeView.Nodes.Add assetTag.Key
            for assetName in assetTag.Value do
                node.Nodes.Add assetName |> ignore
        assetPicker.assetTreeView.DoubleClick.Add (fun _ -> assetPicker.DialogResult <- DialogResult.OK)
        assetPicker.okButton.Click.Add (fun _ -> assetPicker.DialogResult <- DialogResult.OK)
        assetPicker.cancelButton.Click.Add (fun _ -> assetPicker.Close ())
        assetPicker.searchTextBox.TextChanged.Add(fun _ ->
            assetPicker.assetTreeView.Nodes.Clear ()
            for assetTag in assetMap do
                let node = assetPicker.assetTreeView.Nodes.Add assetTag.Key
                for assetName in assetTag.Value do
                    if assetName.Contains assetPicker.searchTextBox.Text then
                        node.Nodes.Add assetName |> ignore
            assetPicker.assetTreeView.ExpandAll ())
        match assetPicker.ShowDialog () with
        | DialogResult.OK ->
            match assetPicker.assetTreeView.SelectedNode with
            | null -> world
            | selectedNode ->
                match selectedNode.Parent with
                | null -> world
                | selectedNodeParent ->
                    let assetTag = (AssetTag.make<obj> selectedNodeParent.Text selectedNode.Text)
                    form.propertyValueTextBoxText <- scstring assetTag
                    form.applyPropertyButton.PerformClick ()
                    world
        | _ -> world

    let private handlePropertyPickParentNode (propertyDescriptor : System.ComponentModel.PropertyDescriptor) (entityTds : EntityTypeDescriptorSource) (form : GaiaForm) world =
        use entityPicker = new EntityPicker ()
        let selectedLayer = (getEditorState world).SelectedLayer
        let entityNames =
            World.getEntities selectedLayer world |>
            Seq.filter (fun entity -> entity.Has<NodeFacet> world) |>
            Seq.filter (fun entity -> not (Gen.isName entity.Name)) |>
            Seq.map (fun entity -> entity.Name) |>
            flip Seq.append [Constants.Editor.NonePick] |>
            Seq.toArray
        entityPicker.entityListBox.Items.AddRange (Array.map box entityNames)
        entityPicker.entityListBox.DoubleClick.Add (fun _ -> entityPicker.DialogResult <- DialogResult.OK)
        entityPicker.okButton.Click.Add (fun _ -> entityPicker.DialogResult <- DialogResult.OK)
        entityPicker.cancelButton.Click.Add (fun _ -> entityPicker.Close ())
        entityPicker.searchTextBox.TextChanged.Add(fun _ ->
            entityPicker.entityListBox.Items.Clear ()
            for name in entityNames do
                if name.Contains entityPicker.searchTextBox.Text || name = Constants.Editor.NonePick then
                    entityPicker.entityListBox.Items.Add name |> ignore)
        match entityPicker.ShowDialog () with
        | DialogResult.OK ->
            match entityPicker.entityListBox.SelectedItem with
            | :? string as parentEntityName ->
                match parentEntityName with
                | Constants.Editor.NonePick -> entityTds.DescribedEntity.SetParentNodeOptWithAdjustment None world
                | _ ->
                    let parentRelation = Relation.makeFromString ("?/?/" + parentEntityName)
                    form.propertyValueTextBoxText <- scstring parentRelation
                    if propertyDescriptor.Name = "ParentNodeOpt"
                    then entityTds.DescribedEntity.SetParentNodeOptWithAdjustment (Some parentRelation) world
                    else (form.applyPropertyButton.PerformClick (); world)
            | _ -> world
        | _ -> world

    let private handlePropertyPickButton (propertyDescriptor : System.ComponentModel.PropertyDescriptor) entityTds form world =
        if propertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag> then handlePropertyPickAsset form world
        elif propertyDescriptor.PropertyType = typeof<Entity Relation option> then handlePropertyPickParentNode propertyDescriptor entityTds form world
        else world

    // TODO: factor away some of the code duplication in this function!
    let private refreshPropertyEditor (form : GaiaForm) =
        if form.propertyTabControl.SelectedIndex = 0 then
            match (form.entityPropertyGrid.SelectedObject, form.entityPropertyGrid.SelectedGridItem) with
            | (null, _) | (_, null) ->
                form.propertyEditor.Enabled <- false
                form.propertyNameLabel.Text <- String.Empty
                form.propertyDescriptionTextBox.Text <- String.Empty
                form.propertyValueTextBoxText <- String.Empty
                form.propertyValueTextBox.EmptyUndoBuffer ()
            | (selectedEntityTds, selectedGridItem) ->
                let selectedEntityTds = selectedEntityTds :?> EntityTypeDescriptorSource // unbox manually
                match selectedGridItem.GridItemType with
                | GridItemType.Property ->
                    let ty = selectedGridItem.PropertyDescriptor.PropertyType
                    let typeConverter = SymbolicConverter (false, None, ty)
                    form.propertyEditor.Enabled <- not selectedGridItem.PropertyDescriptor.IsReadOnly
                    form.propertyNameLabel.Text <- selectedGridItem.Label
                    form.propertyDescriptionTextBox.Text <- selectedGridItem.PropertyDescriptor.Description
                    if ty <> typeof<ComputedProperty> && (notNull selectedGridItem.Value || FSharpType.isNullTrueValue ty) then
                        let (keywords0, keywords1, prettyPrinter) =
                            match selectedGridItem.Label with
                            | "OverlayNameOpt" ->
                                let overlays = World.getIntrinsicOverlays Globals.World @ World.getExtrinsicOverlays Globals.World
                                let overlayNames = List.map (fun overlay -> overlay.OverlayName) overlays
                                (String.concat " " overlayNames, "", PrettyPrinter.defaulted)
                            | "FacetNames" ->
                                let facetNames = Globals.World |> World.getFacets |> Map.toKeyList
                                (String.concat " " facetNames, "", PrettyPrinter.defaulted)
                            | _ ->
                                let syntax = SyntaxAttribute.getOrDefault ty
                                let keywords0 =
                                    if ty = typeof<Scripting.Expr>
                                    then syntax.Keywords0 + " " + WorldBindings.BindingKeywords
                                    else syntax.Keywords0
                                (keywords0, syntax.Keywords1, syntax.PrettyPrinter)
                        let selectionStart = form.propertyValueTextBox.SelectionStart
                        let strUnescaped = typeConverter.ConvertToString selectedGridItem.Value
                        let strEscaped = String.escape strUnescaped
                        let strPretty = PrettyPrinter.prettyPrint strEscaped prettyPrinter
                        form.propertyValueTextBoxText <- strPretty + "\n"
                        form.propertyValueTextBox.EmptyUndoBuffer ()
                        form.propertyValueTextBox.Keywords0 <- keywords0
                        form.propertyValueTextBox.Keywords1 <- keywords1
                        form.propertyValueTextBox.SelectionStart <- selectionStart
                        form.propertyValueTextBox.ScrollCaret ()
                        form.pickPropertyButton.Visible <-
                            selectedGridItem.PropertyDescriptor.PropertyType = typeof<Image AssetTag> ||
                            selectedGridItem.PropertyDescriptor.PropertyType = typeof<Entity Relation option>
                        form.pickPropertyButton.Click.RemoveHandler propertyPickButtonClickHandler
                        propertyPickButtonClickHandler <- EventHandler (fun _ _ -> addWorldChanger $ handlePropertyPickButton selectedGridItem.PropertyDescriptor selectedEntityTds form)
                        form.pickPropertyButton.Click.AddHandler propertyPickButtonClickHandler
                | _ ->
                    form.propertyEditor.Enabled <- false
                    form.propertyNameLabel.Text <- String.Empty
                    form.propertyDescriptionTextBox.Text <- String.Empty
                    form.propertyValueTextBoxText <- String.Empty
                    form.propertyValueTextBox.EmptyUndoBuffer ()
                    form.pickPropertyButton.Visible <- false
                    form.pickPropertyButton.Click.RemoveHandler propertyPickButtonClickHandler
        else // assume layer
            match (form.layerPropertyGrid.SelectedObject, form.layerPropertyGrid.SelectedGridItem) with
            | (null, _) | (_, null) ->
                form.propertyEditor.Enabled <- false
                form.propertyNameLabel.Text <- String.Empty
                form.propertyDescriptionTextBox.Text <- String.Empty
                form.propertyValueTextBoxText <- String.Empty
                form.propertyValueTextBox.EmptyUndoBuffer ()
            | (_, selectedGridItem) ->
                match selectedGridItem.GridItemType with
                | GridItemType.Property ->
                    let ty = selectedGridItem.PropertyDescriptor.PropertyType
                    let typeConverter = SymbolicConverter (false, None, ty)
                    form.propertyEditor.Enabled <- true
                    form.propertyNameLabel.Text <- selectedGridItem.Label
                    form.propertyDescriptionTextBox.Text <- selectedGridItem.PropertyDescriptor.Description
                    if ty <> typeof<ComputedProperty> && (notNull selectedGridItem.Value || FSharpType.isNullTrueValue ty) then
                        let (keywords0, keywords1, prettyPrinter) =
                            let syntax = SyntaxAttribute.getOrDefault ty
                            let keywords0 =
                                if ty = typeof<Scripting.Expr>
                                then syntax.Keywords0 + " " + WorldBindings.BindingKeywords
                                else syntax.Keywords0
                            (keywords0, syntax.Keywords1, syntax.PrettyPrinter)
                        let selectionStart = form.propertyValueTextBox.SelectionStart
                        let strUnescaped = typeConverter.ConvertToString selectedGridItem.Value
                        let strEscaped = String.escape strUnescaped
                        let strPretty = PrettyPrinter.prettyPrint strEscaped prettyPrinter
                        form.propertyValueTextBoxText <- strPretty + "\n"
                        form.propertyValueTextBox.EmptyUndoBuffer ()
                        form.propertyValueTextBox.Keywords0 <- keywords0
                        form.propertyValueTextBox.Keywords1 <- keywords1
                        form.propertyValueTextBox.SelectionStart <- selectionStart
                        form.propertyValueTextBox.ScrollCaret ()
                        form.pickPropertyButton.Visible <- false
                | _ ->
                    form.propertyEditor.Enabled <- false
                    form.propertyNameLabel.Text <- String.Empty
                    form.propertyDescriptionTextBox.Text <- String.Empty
                    form.propertyValueTextBoxText <- String.Empty
                    form.propertyValueTextBox.EmptyUndoBuffer ()
                    form.pickPropertyButton.Visible <- false

    let private refreshEntityPropertyDesigner (form : GaiaForm) =
        form.entityPropertyDesigner.Enabled <- notNull form.entityPropertyGrid.SelectedObject

    // TODO: factor away some of the code duplication in this function!
    let private applyPropertyEditor (form : GaiaForm) =
        match form.entityPropertyGrid.SelectedObject with
        | null -> ()
        | :? EntityTypeDescriptorSource as entityTds ->
            match form.entityPropertyGrid.SelectedGridItem with
            | null -> failwithumf ()
            | selectedGridItem ->
                match selectedGridItem.GridItemType with
                | GridItemType.Property when form.propertyNameLabel.Text = selectedGridItem.Label ->
                    let propertyDescriptor = selectedGridItem.PropertyDescriptor :?> EntityPropertyDescriptor
                    let typeConverter = SymbolicConverter (false, None, propertyDescriptor.PropertyType)
                    try form.propertyValueTextBox.EndUndoAction ()
                        let strEscaped = form.propertyValueTextBox.Text.TrimEnd ()
                        let strUnescaped = String.unescape strEscaped
                        let propertyValue = typeConverter.ConvertFromString strUnescaped
                        propertyDescriptor.SetValue (entityTds, propertyValue)
                    with
                    | :? ConversionException as exn ->
                        match exn.SymbolOpt with
                        | Some symbol ->
                            match Symbol.getOriginOpt symbol with
                            | Some origin ->
                                form.propertyValueTextBox.SelectionStart <- int origin.Start.Index
                                form.propertyValueTextBox.SelectionEnd <- int origin.Stop.Index
                            | None -> ()
                        | None -> ()
                        Log.info ("Invalid apply property operation due to: " + scstring exn)
                    | exn -> Log.info ("Invalid apply property operation due to: " + scstring exn)
                | _ -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."
        | :? LayerTypeDescriptorSource as layerTds ->
            match form.layerPropertyGrid.SelectedGridItem with
            | null -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."
            | selectedGridItem ->
                match selectedGridItem.GridItemType with
                | GridItemType.Property when form.propertyNameLabel.Text = selectedGridItem.Label ->
                    let propertyDescriptor = selectedGridItem.PropertyDescriptor :?> LayerPropertyDescriptor
                    let typeConverter = SymbolicConverter (false, None, selectedGridItem.PropertyDescriptor.PropertyType)
                    try form.propertyValueTextBox.EndUndoAction ()
                        let strEscaped = form.propertyValueTextBox.Text.TrimEnd ()
                        let strUnescaped = String.unescape strEscaped
                        let propertyValue = typeConverter.ConvertFromString strUnescaped
                        propertyDescriptor.SetValue (layerTds, propertyValue)
                    with
                    | :? ConversionException as exn ->
                        match exn.SymbolOpt with
                        | Some symbol ->
                            match Symbol.getOriginOpt symbol with
                            | Some origin ->
                                form.propertyValueTextBox.SelectionStart <- int origin.Start.Index
                                form.propertyValueTextBox.SelectionEnd <- int origin.Stop.Index
                            | None -> ()
                        | None -> ()
                        Log.info ("Invalid apply property operation due to: " + scstring exn)
                    | exn -> Log.info ("Invalid apply property operation due to: " + scstring exn)
                | _ -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."
        | _ -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."

    let private tryReloadPrelude (_ : GaiaForm) world =
        let editorState = getEditorState world
        let targetDir = editorState.TargetDir
        let assetSourceDir = targetDir + "/../.."
        World.tryReloadPrelude assetSourceDir targetDir world

    let private tryLoadPrelude (form : GaiaForm) world =
        match tryReloadPrelude form world with
        | (Right preludeStr, world) ->
            form.preludeTextBox.Text <- preludeStr + "\n"
            world
        | (Left error, world) ->
            MessageBox.Show ("Could not load prelude due to: " + error + "'.", "Failed to load prelude", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            world

    let private trySavePrelude (form : GaiaForm) world =
        let oldWorld = world
        let editorState = getEditorState world
        let preludeSourceDir = editorState.TargetDir + "/../.."
        let preludeFilePath = preludeSourceDir + "/" + Assets.PreludeFilePath
        try let preludeStr = form.preludeTextBox.Text.TrimEnd ()
            File.WriteAllText (preludeFilePath, preludeStr)
            (true, world)
        with exn ->
            MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to save asset graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            (false, World.choose oldWorld)

    let private tryReloadAssetGraph (_ : GaiaForm) world =
        let editorState = getEditorState world
        let targetDir = editorState.TargetDir
        let assetSourceDir = targetDir + "/../.."
        World.tryReloadAssetGraph assetSourceDir targetDir Constants.Editor.RefinementDir world

    let private tryLoadAssetGraph (form : GaiaForm) world =
        match tryReloadAssetGraph form world with
        | (Right assetGraph, world) ->
            let selectionStart = form.assetGraphTextBox.SelectionStart
            let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<AssetGraph>).PrettyPrinter
            form.assetGraphTextBox.Text <- PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter + "\n"
            form.assetGraphTextBox.SelectionStart <- selectionStart
            form.assetGraphTextBox.ScrollCaret ()
            world
        | (Left error, world) ->
            MessageBox.Show ("Could not load asset graph due to: " + error + "'.", "Failed to load asset graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            world

    let private trySaveAssetGraph (form : GaiaForm) world =
        let oldWorld = world
        let editorState = getEditorState world
        let assetSourceDir = editorState.TargetDir + "/../.."
        let assetGraphFilePath = assetSourceDir + "/" + Assets.AssetGraphFilePath
        try let packageDescriptorsStr = form.assetGraphTextBox.Text.TrimEnd () |> scvalue<Map<string, PackageDescriptor>> |> scstring
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<AssetGraph>).PrettyPrinter
            File.WriteAllText (assetGraphFilePath, PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter)
            (true, world)
        with exn ->
            MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to save asset graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            (false, World.choose oldWorld)

    let private tryReloadOverlays form world =
        let targetDir = (getEditorState world).TargetDir
        let overlayDir = targetDir + "/../.."
        match World.tryReloadOverlays overlayDir targetDir world with
        | (Right overlayer, world) ->
            refreshOverlayComboBox form world
            (Right overlayer, world)
        | (Left error, world) -> (Left error, world)

    let private tryLoadOverlayer (form : GaiaForm) world =
        match tryReloadOverlays form world with
        | (Right overlayer, world) ->
            let selectionStart = form.overlayerTextBox.SelectionStart
            let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<Overlay>).PrettyPrinter
            form.overlayerTextBox.Text <- PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter + "\n"
            form.overlayerTextBox.SelectionStart <- selectionStart
            form.overlayerTextBox.ScrollCaret ()
            world
        | (Left error, world) ->
            MessageBox.Show ("Could not reload overlayer due to: " + error + "'.", "Failed to reload overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            world

    let private trySaveOverlayer (form : GaiaForm) world =
        let oldWorld = world
        let editorState = getEditorState world
        let overlayerSourceDir = editorState.TargetDir + "/../.."
        let overlayerFilePath = overlayerSourceDir + "/" + Assets.OverlayerFilePath
        try let overlays = scvalue<Overlay list> (form.overlayerTextBox.Text.TrimEnd ())
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<Overlay>).PrettyPrinter
            File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
            (true, world)
        with exn ->
            MessageBox.Show ("Could not save overlayer due to: " + scstring exn, "Failed to save overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            (false, oldWorld)

    let private handleFormEntityPropertyGridSelectedObjectsChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form
        refreshEntityPropertyDesigner form
        setEntityTreeViewSelectionToEntityPropertyGridSelection form
        setHierarchyTreeViewSelectionToEntityPropertyGridSelection form

    let private handleFormEntityPropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormLayerPropertyGridSelectedObjectsChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormLayerPropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handlePropertyTabControlSelectedIndexChanged (form : GaiaForm) (_ : EventArgs) =
        if form.propertyTabControl.SelectedIndex = 0
        then refreshEntityPropertyGrid form Globals.World
        else refreshLayerPropertyGrid form Globals.World

    let private handleFormPropertyRefreshClick (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormPropertyApplyClick (form : GaiaForm) (_ : EventArgs) =
        applyPropertyEditor form

    let private handleFormEntityTreeViewNodeSelect (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            if  notNull form.entityTreeView.SelectedNode &&
                form.entityTreeView.SelectedNode.Level = 1 then
                let address = Address.makeFromString form.entityTreeView.SelectedNode.Name
                match Address.getNames address with
                | [|_; _; _|] ->
                    let entity = Entity address
                    selectEntity entity form world
                    world
                | _ -> world // don't have an entity address
            else world

    let private handleFormHierarchyTreeViewNodeSelect (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            if notNull form.hierarchyTreeView.SelectedNode then
                let address = Address.makeFromString form.hierarchyTreeView.SelectedNode.Name
                match Address.getNames address with
                | [|_; _|] ->

                    // select the layer of the selected entity
                    let layer = Layer (atoa address)
                    let layerTabIndex = form.layerTabControl.TabPages.IndexOfKey layer.Name
                    form.layerTabControl.SelectTab layerTabIndex
                    world

                | [|_; _; _|] ->

                    // get a handle to the selected entity
                    let entity = Entity (atoa address)

                    // select the layer of the selected entity
                    let layer = entity.Parent
                    let layerTabIndex = form.layerTabControl.TabPages.IndexOfKey layer.Name
                    form.layerTabControl.SelectTab layerTabIndex

                    // select the entity in the property grid
                    selectEntity entity form world
                    world

                | _ -> world // don't have an entity address
            else world

    let private handleFormCreateEntity atMouse (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let oldWorld = world
            try Globals.pushPastWorld world
                let selectedLayer = (getEditorState world).SelectedLayer
                let dispatcherName = form.createEntityComboBox.Text
                let overlayNameDescriptor =
                    match form.overlayComboBox.Text with
                    | "(Default Overlay)" -> DefaultOverlay
                    | "(Routed Overlay)" -> RoutedOverlay
                    | "(No Overlay)" -> NoOverlay
                    | overlayName -> ExplicitOverlay overlayName
                let (entity, world) = World.createEntity5 dispatcherName None overlayNameDescriptor selectedLayer world
                let (positionSnap, rotationSnap) = getSnaps form
                let mousePosition = World.getMousePositionF world
                let entityPosition =
                    if atMouse
                    then World.mouseToWorld (entity.GetAbsolute world) mousePosition world
                    else World.mouseToWorld (entity.GetAbsolute world) (World.getEyeSize world * 0.5f) world
                let entityTransform =
                    { Position = entityPosition
                      Size = entity.GetQuickSize world
                      Rotation = entity.GetRotation world
                      Depth = getCreationDepth form
                      Flags = entity.GetFlags world
                      RefCount = 0 }
                let world = entity.SetTransformSnapped positionSnap rotationSnap entityTransform world
                selectEntity entity form world
                world
            with exn ->
                let world = World.choose oldWorld
                MessageBox.Show (scstring exn, "Could not create entity", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleFormDeleteEntity (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            Globals.pushPastWorld world
            match form.entityPropertyGrid.SelectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                let world = World.destroyEntity entityTds.DescribedEntity world
                deselectEntity form world
                world
            | _ -> world

    let private handleFormNew (form : GaiaForm) (_ : EventArgs) =
        use layerCreationForm = new LayerCreationForm ()
        layerCreationForm.StartPosition <- FormStartPosition.CenterParent
        layerCreationForm.dispatcherTextBox.Text <- typeof<LayerDispatcher>.Name
        layerCreationForm.okButton.Click.Add $ fun _ ->
            addWorldChanger $ fun world ->
                let oldWorld = world
                Globals.pushPastWorld world
                let layerName = layerCreationForm.nameTextBox.Text
                let layerDispatcherName = layerCreationForm.dispatcherTextBox.Text
                try if String.length layerName = 0 then failwith "Layer name cannot be empty in Gaia due to WinForms limitations."
                    let world = World.createLayer4 layerDispatcherName (Some layerName) Simulants.DefaultScreen world |> snd
                    refreshLayerTabs form world
                    refreshHierarchyTreeView form world
                    deselectEntity form world
                    form.layerTabControl.SelectTab (form.layerTabControl.TabPages.IndexOfKey layerName)
                    world
                with exn ->
                    let world = World.choose oldWorld
                    MessageBox.Show ("Could not create layer due to: " + scstring exn, "Layer creation error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            layerCreationForm.Close ()
        layerCreationForm.cancelButton.Click.Add (fun _ -> layerCreationForm.Close ())
        layerCreationForm.ShowDialog form |> ignore

    let private handleFormSave saveAs (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let layer = (getEditorState world).SelectedLayer
            form.saveFileDialog.Title <- "Save '" + layer.Name + "' As"
            match Map.tryFind layer.LayerAddress (getEditorState world).FilePaths with
            | Some filePath -> form.saveFileDialog.FileName <- filePath
            | None -> form.saveFileDialog.FileName <- String.Empty
            if saveAs || String.IsNullOrWhiteSpace form.saveFileDialog.FileName then
                match form.saveFileDialog.ShowDialog form with
                | DialogResult.OK ->
                    let filePath = form.saveFileDialog.FileName
                    trySaveSelectedLayer filePath world
                    updateEditorState (fun value ->
                        let filePaths = Map.add layer.LayerAddress filePath value.FilePaths
                        { value with FilePaths = filePaths })
                        world
                | _ -> world
            else trySaveSelectedLayer form.saveFileDialog.FileName world; world

    let private handleFormOpen (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            form.openFileDialog.FileName <- String.Empty
            match form.openFileDialog.ShowDialog form with
            | DialogResult.OK ->
                Globals.pushPastWorld world
                let filePath = form.openFileDialog.FileName
                match tryLoadSelectedLayer form filePath world with
                | (Some layer, world) ->
                    let world =
                        updateEditorState (fun value ->
                            let filePaths = Map.add layer.LayerAddress filePath value.FilePaths
                            { value with FilePaths = filePaths })
                            world
                    deselectEntity form world // currently selected entity may be gone if loading into an existing layer
                    world
                | (None, world) -> world
            | _ -> world

    let private handleFormClose (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match form.layerTabControl.TabPages.Count with
            | 1 ->
                MessageBox.Show ("Cannot close the only remaining layer.", "Layer close error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world
            | _ ->
                Globals.pushPastWorld world
                let layer = (getEditorState world).SelectedLayer
                let world = World.destroyLayerImmediate layer world
                deselectEntity form world
                form.layerTabControl.TabPages.RemoveByKey layer.Name
                updateEditorState (fun editorState ->
                    let layerTabControl = form.layerTabControl
                    let layerTab = layerTabControl.SelectedTab
                    { editorState with
                        SelectedLayer = Simulants.DefaultScreen / layerTab.Text
                        FilePaths = Map.remove layer.LayerAddress editorState.FilePaths })
                    world

    let private handleFormUndo (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match Globals.PastWorlds with
            | [] -> world
            | pastWorld :: pastWorlds ->
                let futureWorld = World.shelve world
                let world = World.unshelve pastWorld
                Globals.PastWorlds <- pastWorlds
                Globals.FutureWorlds <- futureWorld :: Globals.FutureWorlds
                let world = World.setTickRate 0L world
                refreshFormOnUndoRedo form world
                world

    let private handleFormRedo (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match Globals.FutureWorlds with
            | [] -> world
            | futureWorld :: futureWorlds ->
                let pastWorld = World.shelve world
                let world = World.unshelve futureWorld
                Globals.PastWorlds <- pastWorld :: Globals.PastWorlds
                Globals.FutureWorlds <- futureWorlds
                let world = World.setTickRate 0L world
                refreshFormOnUndoRedo form world
                world

    let private handleFormTickingChanged (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let tickRate = if form.tickingButton.Checked then 1L else 0L
            let (pastWorld, world) = (world, World.setTickRate tickRate world)
            if tickRate = 1L then Globals.pushPastWorld pastWorld
            world

    let private handleFormResetTickTime (_ : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let (pastWorld, world) = (world, World.resetTickTime world)
            Globals.pushPastWorld pastWorld
            world

    let private handleFormIncTickTime (_ : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let (pastWorld, world) = (world, World.incTickTime world)
            Globals.pushPastWorld pastWorld
            world

    let private handleFormDecTickTime (_ : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let (pastWorld, world) = (world, World.decTickTime world)
            Globals.pushPastWorld pastWorld
            world

    let private handleFormCopy (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds -> World.copyEntityToClipboard entityTds.DescribedEntity world; world
            | _ -> failwithumf ()

    let private handleFormCut (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                Globals.pushPastWorld world
                let world = World.cutEntityToClipboard entityTds.DescribedEntity world
                deselectEntity form world
                world
            | _ -> failwithumf ()

    let private handleFormPaste atMouse (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            Globals.pushPastWorld world
            let selectedLayer = (getEditorState world).SelectedLayer
            let (positionSnap, rotationSnap) = getSnaps form
            let editorState = getEditorState world
            let (entityOpt, world) = World.pasteEntityFromClipboard atMouse editorState.RightClickPosition positionSnap rotationSnap selectedLayer world
            match entityOpt with
            | Some entity -> selectEntity entity form world; world
            | None -> world

    let private handleFormQuickSize (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = entityTds.DescribedEntity
                Globals.pushPastWorld world
                let world = entity.Diverge world
                let world = entity.SetSize (entity.GetQuickSize world) world
                Globals.World <- world // must be set for property grid
                form.entityPropertyGrid.Refresh ()
                world
            | _ -> failwithumf ()

    let private handleFormResetCamera (_ : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            World.setEyeCenter Vector2.Zero world

    let private handleFormReloadAssets (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match tryReloadAssetGraph form world with
            | (Right assetGraph, world) ->
                let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<AssetGraph>).PrettyPrinter
                form.assetGraphTextBox.Text <- PrettyPrinter.prettyPrint (scstring assetGraph) prettyPrinter + "\n"
                world
            | (Left error, world) ->
                MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset reload error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleFormLayerTabDeselected (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            deselectEntity form world
            refreshEntityTreeView form world
            refreshEntityPropertyGrid form world
            refreshLayerPropertyGrid form world
            world

    let private handleFormLayerTabSelected (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let selectedLayer =
                let layerTabControl = form.layerTabControl
                let layerTab = layerTabControl.SelectedTab
                Simulants.DefaultScreen / layerTab.Text
            let world = updateEditorState (fun editorState -> { editorState with SelectedLayer = selectedLayer}) world
            refreshEntityTreeView form world
            refreshEntityPropertyGrid form world
            selectLayer selectedLayer form world
            world

    let private handleTraceEventsCheckBoxChanged (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let eventTracerOpt =
                if form.traceEventsCheckBox.Checked
                then Some (Log.remark "Event")
                else None
            World.setEventTracerOpt eventTracerOpt world

    let private handleApplyEventFilterClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let oldWorld = world
            try let eventFilter = scvalue<EventFilter.Filter> form.eventFilterTextBox.Text
                let world = World.setEventFilter eventFilter world
                let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<EventFilter.Filter>).PrettyPrinter
                form.eventFilterTextBox.Text <- PrettyPrinter.prettyPrint (scstring eventFilter) prettyPrinter + "\n"
                world
            with exn ->
                let world = World.choose oldWorld
                MessageBox.Show ("Invalid event filter due to: " + scstring exn, "Invalid event filter", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleRefreshEventFilterClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let eventFilter = World.getEventFilter world
            let eventFilterStr = scstring eventFilter
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<EventFilter.Filter>).PrettyPrinter
            let eventFilterPretty = PrettyPrinter.prettyPrint eventFilterStr prettyPrinter
            form.eventFilterTextBox.Text <- eventFilterPretty + "\n"
            world

    let private handleSavePreludeClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match trySavePrelude form world with
            | (true, world) ->
                match tryReloadPrelude form world with
                | (Right _, world) -> world
                | (Left error, world) ->
                    MessageBox.Show ("Prelude reload error due to: " + error + "'.", "Prelude reload error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            | (false, world) -> world

    let private handleLoadPreludeClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            tryLoadPrelude form world

    let private handleSaveAssetGraphClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match trySaveAssetGraph form world with
            | (true, world) ->
                match tryReloadAssetGraph form world with
                | (Right _, world) -> world
                | (Left error, world) ->
                    MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset reload error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            | (false, world) -> world

    let private handleLoadAssetGraphClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            tryLoadAssetGraph form world

    let private handleSaveOverlayerClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match trySaveOverlayer form world with
            | (true, world) ->
                match tryReloadOverlays form world with
                | (Right _, world) -> world
                | (Left error, world) ->
                    MessageBox.Show ("Overlayer reload error due to: " + error + "'.", "Overlayer reload error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            | (false, world) -> world

    let private handleLoadOverlayerClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            tryLoadOverlayer form world

    let private handleEvalClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let exprsStr =
                if String.notEmpty form.evalInputTextBox.SelectedText
                then form.evalInputTextBox.SelectedText
                else form.evalInputTextBox.Text
            let exprsStr = Symbol.OpenSymbolsStr + "\n" + exprsStr + "\n" + Symbol.CloseSymbolsStr
            try let exprs = scvalue<Scripting.Expr array> exprsStr
                let layer = (getEditorState world).SelectedLayer
                let (selectedSimulant, localFrame) =
                    match form.entityPropertyGrid.SelectedObject with
                    | :?
                        EntityTypeDescriptorSource as entityTds when
                        form.propertyTabControl.SelectedTab <> form.layerTabPage ->
                        let entity = entityTds.DescribedEntity
                        (entity :> Simulant, entity.GetScriptFrame world)
                    | _ -> (layer :> Simulant, layer.GetScriptFrame world)
                let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<Scripting.Expr>).PrettyPrinter
                let struct (evaleds, world) = World.evalManyWithLogging exprs localFrame selectedSimulant world
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
        form.overlayComboBox.SelectedIndex <- 0

    let private handleEntityDesignerPropertyAddClick defaulting (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let propertyTypeText = form.entityDesignerPropertyTypeComboBox.Text
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = entityTds.DescribedEntity
                match Array.toList (propertyTypeText.Split ([|" : "|], StringSplitOptions.None)) with
                | [] ->
                    MessageBox.Show
                        ("Could not add designer property '" + propertyTypeText + "'. Please provide an evaluatable expression or choose one from the list.",
                         "Invalid Designer Property",
                         MessageBoxButtons.OK) |>
                        ignore
                    world
                | exprStr :: _ ->
                    try let expr = scvalue<Scripting.Expr> exprStr
                        let selectedLayer = (getEditorState world).SelectedLayer
                        let localFrame = selectedLayer.GetScriptFrame world
                        let struct (evaled, world) = World.evalWithLogging expr localFrame selectedLayer world
                        match Scripting.Expr.toFSharpTypeOpt evaled with
                        | Some dt ->
                            let dvOpt =
                                if defaulting
                                then dt.TryGetDefaultValue ()
                                else ScriptingSystem.tryExport dt evaled world
                            match dvOpt with
                            | Some dv ->
                                let propertyName = form.entityDesignerPropertyNameTextBox.Text
                                let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                                let nonPersistent = Reflection.isPropertyNonPersistentByName propertyName
                                let dp = { DesignerType = dt; DesignerValue = dv }
                                let property = { PropertyType = typeof<DesignerProperty>; PropertyValue = dp }
                                let world = entity.AttachProperty propertyName alwaysPublish nonPersistent property world
                                Globals.World <- world // must be set for property grid
                                form.entityPropertyGrid.Refresh ()
                                world
                            | None ->
                                MessageBox.Show
                                    ("Could not add default value for designer property '" + propertyTypeText + "'. ",
                                     "Invalid Designer Property",
                                     MessageBoxButtons.OK) |>
                                    ignore
                                world
                        | None ->
                            MessageBox.Show
                                ("Could not add designer property '" + propertyTypeText + "'. " +
                                 "Expression does not articulate enough information to infer a static F# type. " +
                                 "Please provide a fully articulated expression or choose one from the list.",
                                 "Invalid Designer Property",
                                 MessageBoxButtons.OK) |>
                                ignore
                            world
                    with _ ->
                        MessageBox.Show
                            ("Could not add designer property '" + propertyTypeText + "'. Please provide an evaluatable expression or choose one from the list.",
                             "Invalid Designer Property",
                             MessageBoxButtons.OK) |>
                            ignore
                        world
            | _ -> world

    let private handleEntityDesignerPropertyRemoveClick (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = entityTds.DescribedEntity
                let world = entity.DetachProperty form.entityDesignerPropertyNameTextBox.Text world
                Globals.World <- world // must be set for property grid
                form.entityPropertyGrid.Refresh ()
                world
            | _ -> world

    let private handleKeyboardInput key isKeyFromKeyableControl (form : GaiaForm) world =
        if form :> Form = Form.ActiveForm then
            if Keys.F5 = key then form.tickingButton.PerformClick ()
            if Keys.Control = Control.ModifierKeys && Keys.Q = key then handleFormQuickSize form (EventArgs ())
            if Keys.Control = Control.ModifierKeys && Keys.N = key then handleFormNew form (EventArgs ())
            if Keys.Control = Control.ModifierKeys && Keys.O = key then handleFormOpen form (EventArgs ())
            if Keys.Control = Control.ModifierKeys && Keys.S = key then handleFormSave false form (EventArgs ())
            if Keys.Control = Control.ModifierKeys && Keys.D0 = key then handleFormResetTickTime form (EventArgs ())
            if Keys.Control = Control.ModifierKeys && (Keys.OemMinus = key || Keys.Add = key) then handleFormIncTickTime form ( EventArgs ())
            if Keys.Control = Control.ModifierKeys && (Keys.Oemplus = key || Keys.Subtract = key) then handleFormDecTickTime form (EventArgs ())
            if Keys.Alt = Control.ModifierKeys && (Keys.A = key || Keys.Enter = key) then
                match form.rolloutTabControl.SelectedTab.Name with
                | "propertyEditorTabPage" -> form.applyPropertyButton.PerformClick ()
                | "preludeTabPage" -> form.applyPreludeButton.PerformClick ()
                | "assetGraphTabPage" -> form.applyAssetGraphButton.PerformClick ()
                | "overlayerTabPage" -> form.applyOverlayerButton.PerformClick ()
                | "eventTracingTabPage" -> form.applyEventFilterButton.PerformClick ()
                | _ -> ()
            if Keys.Alt = Control.ModifierKeys && Keys.D = key then
                match form.rolloutTabControl.SelectedTab.Name with
                | "propertyEditorTabPage" -> form.discardPropertyButton.PerformClick ()
                | "preludeTabPage" -> form.discardPreludeButton.PerformClick ()
                | "assetGraphTabPage" -> form.discardAssetGraphButton.PerformClick ()
                | "overlayerTabPage" -> form.discardOverlayerButton.PerformClick ()
                | "eventTracingTabPage" -> form.discardEventFilterButton.PerformClick ()
                | _ -> ()
            if Keys.Alt = Control.ModifierKeys && (Keys.P = key || Keys.Enter = key) then (form.rolloutTabControl.SelectTab "propertyEditorTabPage"; form.propertyValueTextBox.Select (); form.propertyValueTextBox.SelectAll ())
            if Keys.Alt = Control.ModifierKeys && Keys.E = key then (form.rolloutTabControl.SelectTab "evaluatorTabPage"; form.evalInputTextBox.Select ())
            if Keys.Alt = Control.ModifierKeys && Keys.K = key && form.rolloutTabControl.SelectedTab.Name = "propertyEditorTabPage" then form.pickPropertyButton.PerformClick ()
            if Keys.Alt = Control.ModifierKeys && Keys.T = key && form.rolloutTabControl.SelectedTab.Name = "eventTracingTabPage" then form.traceEventsCheckBox.Checked <- not form.traceEventsCheckBox.Checked
            if Keys.Alt = Control.ModifierKeys && Keys.V = key && form.rolloutTabControl.SelectedTab.Name = "evaluatorTabPage" then form.evalButton.PerformClick ()
            if Keys.Alt = Control.ModifierKeys && Keys.L = key && form.rolloutTabControl.SelectedTab.Name = "evaluatorTabPage" then form.evalLineButton.PerformClick ()
            if not isKeyFromKeyableControl then
                if Keys.Control = Control.ModifierKeys && Keys.A = key then handleFormSave true form (EventArgs ())
                if Keys.Control = Control.ModifierKeys && Keys.Z = key then handleFormUndo form (EventArgs ())
                if Keys.Control = Control.ModifierKeys && Keys.Y = key then handleFormRedo form (EventArgs ())
                if Keys.Control = Control.ModifierKeys && Keys.E = key then handleFormCreateEntity false form (EventArgs ())
                if Keys.Control = Control.ModifierKeys && Keys.D = key then handleFormDeleteEntity form (EventArgs ())
                if Keys.Control = Control.ModifierKeys && Keys.X = key then handleFormCut form (EventArgs ())
                if Keys.Control = Control.ModifierKeys && Keys.C = key then handleFormCopy form (EventArgs ())
                if Keys.Control = Control.ModifierKeys && Keys.V = key then handleFormPaste false form (EventArgs ())
                if Keys.Delete = key then handleFormDeleteEntity form (EventArgs ())
                if Keys.Escape = key then deselectEntity form world

    let private handleFormClosing (_ : GaiaForm) (args : CancelEventArgs) =
        match MessageBox.Show ("Are you sure you want to close Gaia?", "Close Gaia?", MessageBoxButtons.OKCancel) with
        | DialogResult.Cancel -> args.Cancel <- true
        | _ -> ()

    let private updateEntityDrag (form : GaiaForm) world =
        if not (canEditWithMouse form world) then
            match (getEditorState world).DragEntityState with
            | DragEntityPosition (pickOffset, mousePositionWorldOrig, entity) ->
                // NOTE: in https://github.com/bryanedds/Nu/issues/272, we found that we need to check for an entity's
                // existence here because it could be deleted right as the drag operation begins if the delete button
                // is held during selection
                if entity.Exists world then
                    let (positionSnap, _) = getSnaps form
                    let mousePosition = World.getMousePositionF world
                    let mousePositionWorld = World.mouseToWorld (entity.GetAbsolute world) mousePosition world
                    let entityPosition = (pickOffset - mousePositionWorldOrig) + (mousePositionWorld - mousePositionWorldOrig)
                    let entityPositionSnapped = Math.snap2F positionSnap entityPosition
                    let world =
                        if entity.Has<NodeFacet> world && entity.ParentNodeExists world
                        then entity.SetPositionLocal entityPositionSnapped world
                        else entity.SetPosition entityPositionSnapped world
                    let world =
                        updateEditorState (fun editorState ->
                            { editorState with DragEntityState = DragEntityPosition (pickOffset, mousePositionWorldOrig, entity) })
                            world
                    // NOTE: disabled the following line to fix perf issue caused by refreshing the property grid every frame
                    // form.entityPropertyGrid.Refresh ()
                    world
                else world
            | DragEntityRotation _ -> world
            | DragEntityNone -> world
        else world

    let private updateCameraDrag (_ : GaiaForm) world =
        match (getEditorState world).DragCameraState with
        | DragCameraPosition (pickOffset, mousePositionScreenOrig) ->
            let mousePosition = World.getMousePositionF world
            let mousePositionScreen = World.mouseToScreen mousePosition world
            let eyeCenter = (pickOffset - mousePositionScreenOrig) + -Constants.Editor.CameraSpeed * (mousePositionScreen - mousePositionScreenOrig)
            let world = World.setEyeCenter eyeCenter world
            updateEditorState (fun editorState ->
                { editorState with DragCameraState = DragCameraPosition (pickOffset, mousePositionScreenOrig) })
                world
        | DragCameraNone -> world

    // TODO: remove code duplication with below
    let private updateUndoButton (form : GaiaForm) =
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty Globals.PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not (List.isEmpty Globals.PastWorlds) then
            form.undoButton.Enabled <- true
            form.undoToolStripMenuItem.Enabled <- true

    let private updateRedoButton (form : GaiaForm) =
        if form.redoToolStripMenuItem.Enabled then
            if List.isEmpty Globals.FutureWorlds then
                form.redoButton.Enabled <- false
                form.redoToolStripMenuItem.Enabled <- false
        elif not (List.isEmpty Globals.FutureWorlds) then
            form.redoButton.Enabled <- true
            form.redoToolStripMenuItem.Enabled <- true

    let private updateEditorWorld form world =
        let worldChangersCopy = List.ofSeq Globals.WorldChangers
        Globals.WorldChangers.Clear ()
        let world = List.fold (fun world worldChanger -> worldChanger world) world worldChangersCopy
        let world = updateEntityDrag form world
        let world = updateCameraDrag form world
        updateUndoButton form
        updateRedoButton form
        if not form.propertyValueTextBox.Focused &&
           not form.applyPropertyButton.Focused &&
           not form.IsClosing then
           refreshPropertyEditor form
        if form.IsDisposed
        then World.exit world
        else world

    /// Attach Gaia to the given world.
    let attachToWorld targetDir form world =
        if World.getSelectedScreen world = Simulants.DefaultScreen then
            let layers = World.getLayers Simulants.DefaultScreen world |> Seq.toList
            let (defaultLayer, world) =
                match layers with
                | defaultLayer :: _ ->
                    match World.tryGetKeyedValue<EditorState> Globals.EditorGuid world with
                    | None ->
                        let editorState =
                            { TargetDir = targetDir
                              RightClickPosition = Vector2.Zero
                              DragEntityState = DragEntityNone
                              DragCameraState = DragCameraNone
                              SelectedLayer = defaultLayer
                              FilePaths = Map.empty }
                        let world = World.addKeyedValue Globals.EditorGuid editorState world
                        let world = World.subscribe (handleNuMouseRightDown form) Events.MouseRightDown Simulants.Game world
                        let world = World.subscribe (handleNuEntityDragBegin form) Events.MouseLeftDown Simulants.Game world
                        let world = World.subscribe (handleNuEntityDragEnd form) Events.MouseLeftUp Simulants.Game world
                        let world = World.subscribe (handleNuCameraDragBegin form) Events.MouseCenterDown Simulants.Game world
                        let world = World.subscribe (handleNuCameraDragEnd form) Events.MouseCenterUp Simulants.Game world
                        (defaultLayer, world)
                    | Some _ -> (defaultLayer, world) // NOTE: conclude world is already attached
                | [] -> failwith ("Cannot attach Gaia to a world with no layers inside the '" + scstring Simulants.DefaultScreen + "' screen.")
            let world = List.fold (fun world layer -> monitorEntityEvents layer form world) world layers
            (defaultLayer, world)
        else failwith ("Cannot attach Gaia to a world with a screen selected other than '" + scstring Simulants.DefaultScreen + "'.")

    let rec private tryRun3 runWhile sdlDeps (form : GaiaForm) =
        try World.runWithoutCleanUp
                runWhile
                (fun world -> let world = updateEditorWorld form world in (Globals.World <- world; world))
                (fun world -> form.displayPanel.Invalidate (); world)
                sdlDeps
                Running
                None
                None
                Globals.World |>
                ignore
        with exn ->
            match MessageBox.Show
                ("Unexpected exception due to: " + scstring exn + "\nWould you like to undo the last operator to try to keep Gaia running?",
                 "Unexpected Exception",
                 MessageBoxButtons.YesNo,
                 MessageBoxIcon.Error) with
            | DialogResult.Yes ->
                form.undoButton.PerformClick ()
                Globals.World <- World.choose Globals.World
                tryRun3 runWhile sdlDeps form
            | _ -> Globals.World <- World.choose Globals.World

    let private run3 runWhile targetDir sdlDeps (form : GaiaForm) =
        let (defaultLayer, world) = attachToWorld targetDir form Globals.World
        Globals.World <- world
        refreshOverlayComboBox form Globals.World
        refreshCreateComboBox form Globals.World
        refreshEntityTreeView form Globals.World
        refreshHierarchyTreeView form Globals.World
        refreshLayerTabs form Globals.World
        selectLayer defaultLayer form Globals.World
        form.tickingButton.CheckState <- CheckState.Unchecked
        form.add_LowLevelKeyboardHook (fun nCode wParam lParam ->
            let WM_KEYDOWN = 0x0100
            let WM_SYSKEYDOWN = 0x0104
            if nCode >= 0 && (wParam = IntPtr WM_KEYDOWN || wParam = IntPtr WM_SYSKEYDOWN) then
                let key = lParam |> Marshal.ReadInt32 |> enum<Keys> 
                match form.GetFocusedControl () with
                | :? ToolStripDropDown
                // | :? ToolStripComboBox wtf?
                | :? TextBox
                | :? SymbolicTextBox -> handleKeyboardInput key true form Globals.World
                | _ -> handleKeyboardInput key false form Globals.World
            GaiaForm.CallNextHookEx (form.HookId, nCode, wParam, lParam)) |> ignore
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
        let savedState =
            try scvalue (File.ReadAllText Constants.Editor.SavedStateFilePath)
            with _ -> { BinaryFilePath = ""; UseGameplayScreen = false }
        use startForm = new StartForm ()
        startForm.binaryFilePathText.Text <- savedState.BinaryFilePath
        startForm.useGameplayScreenCheckBox.Checked <- savedState.UseGameplayScreen
        if  startForm.ShowDialog () = DialogResult.OK &&
            not (String.IsNullOrWhiteSpace (startForm.binaryFilePathText.Text)) then
            let savedState =
                { BinaryFilePath = startForm.binaryFilePathText.Text
                  UseGameplayScreen = startForm.useGameplayScreenCheckBox.Checked }
            try File.WriteAllText (Constants.Editor.SavedStateFilePath, (scstring savedState))
            with _ -> Log.info "Could not save editor state."
            let (targetDir, plugIn) = selectTargetDirAndMakeNuPluginFromFilePath startForm.binaryFilePathText.Text
            (savedState, targetDir, plugIn)
        else (savedState, ".", NuPlugin ())

    /// Create a Gaia form.
    let createForm () =

        // create form
        let form = new GaiaForm ()

        // configure controls
        form.displayPanel.MaximumSize <- Drawing.Size (Constants.Render.ResolutionX, Constants.Render.ResolutionY)
        form.positionSnapTextBox.Text <- scstring Constants.Editor.DefaultPositionSnap
        form.rotationSnapTextBox.Text <- scstring Constants.Editor.DefaultRotationSnap
        form.createDepthTextBox.Text <- scstring Constants.Editor.DefaultCreationDepth

        // build tree view sorter
        let treeViewSorter =
            { new IComparer with
                member this.Compare (left, right) =
                    let leftName = ((left :?> TreeNode).Name.Split Constants.Address.Separator) |> Array.last
                    let rightName = ((right :?> TreeNode).Name.Split Constants.Address.Separator) |> Array.last
                    let leftNameBiased = if Gen.isName leftName then "~" + leftName else leftName
                    let rightNameBiased = if Gen.isName rightName then "~" + rightName else rightName
                    String.CompareOrdinal (leftNameBiased, rightNameBiased) }

        // sort entity tree view nodes with a bias against guids
        form.entityTreeView.Sorted <- true
        form.entityTreeView.TreeViewNodeSorter <- treeViewSorter

        // same for hierarchy tree view...
        form.hierarchyTreeView.Sorted <- true
        form.hierarchyTreeView.TreeViewNodeSorter <- treeViewSorter

        // set up events handlers
        form.exitToolStripMenuItem.Click.Add (handleFormExit form)
        form.createDepthPlusButton.Click.Add (handleFormCreateDepthPlusClick form)
        form.createDepthMinusButton.Click.Add (handleFormCreateDepthMinusClick form)
        form.entityPropertyGrid.SelectedObjectsChanged.Add (handleFormEntityPropertyGridSelectedObjectsChanged form)
        form.entityPropertyGrid.SelectedGridItemChanged.Add (handleFormEntityPropertyGridSelectedGridItemChanged form)
        form.layerPropertyGrid.SelectedObjectsChanged.Add (handleFormLayerPropertyGridSelectedObjectsChanged form)
        form.layerPropertyGrid.SelectedGridItemChanged.Add (handleFormLayerPropertyGridSelectedGridItemChanged form)
        form.propertyTabControl.SelectedIndexChanged.Add (handlePropertyTabControlSelectedIndexChanged form)
        form.discardPropertyButton.Click.Add (handleFormPropertyRefreshClick form)
        form.applyPropertyButton.Click.Add (handleFormPropertyApplyClick form)
        form.traceEventsCheckBox.CheckStateChanged.Add (handleTraceEventsCheckBoxChanged form)
        form.applyEventFilterButton.Click.Add (handleApplyEventFilterClick form)
        form.discardEventFilterButton.Click.Add (handleRefreshEventFilterClick form)
        form.applyPreludeButton.Click.Add (handleSavePreludeClick form)
        form.discardPreludeButton.Click.Add (handleLoadPreludeClick form)
        form.applyAssetGraphButton.Click.Add (handleSaveAssetGraphClick form)
        form.discardAssetGraphButton.Click.Add (handleLoadAssetGraphClick form)
        form.applyOverlayerButton.Click.Add (handleSaveOverlayerClick form)
        form.discardOverlayerButton.Click.Add (handleLoadOverlayerClick form)
        form.entityTreeView.AfterSelect.Add (handleFormEntityTreeViewNodeSelect form)
        form.hierarchyTreeView.AfterSelect.Add (handleFormHierarchyTreeViewNodeSelect form)
        form.createEntityButton.Click.Add (handleFormCreateEntity false form)
        form.createToolStripMenuItem.Click.Add (handleFormCreateEntity false form)
        form.createContextMenuItem.Click.Add (handleFormCreateEntity true form)
        form.deleteEntityButton.Click.Add (handleFormDeleteEntity form)
        form.deleteToolStripMenuItem.Click.Add (handleFormDeleteEntity form)
        form.quickSizeToolStripMenuItem.Click.Add (handleFormQuickSize form)
        form.startStopTickingToolStripMenuItem.Click.Add (fun _ -> form.tickingButton.PerformClick ())
        form.deleteContextMenuItem.Click.Add (handleFormDeleteEntity form)
        form.newLayerToolStripMenuItem.Click.Add (handleFormNew form)
        form.saveLayerToolStripMenuItem.Click.Add (handleFormSave false form)
        form.saveLayerAsToolStripMenuItem.Click.Add (handleFormSave true form)
        form.openLayerToolStripMenuItem.Click.Add (handleFormOpen form)
        form.closeLayerToolStripMenuItem.Click.Add (handleFormClose form)
        form.undoButton.Click.Add (handleFormUndo form)
        form.undoToolStripMenuItem.Click.Add (handleFormUndo form)
        form.redoButton.Click.Add (handleFormRedo form)
        form.redoToolStripMenuItem.Click.Add (handleFormRedo form)
        form.tickingButton.CheckedChanged.Add (handleFormTickingChanged form)
        form.resetTickTime.Click.Add (handleFormResetTickTime form)
        form.incTickTime.Click.Add (handleFormIncTickTime form)
        form.decTickTime.Click.Add (handleFormDecTickTime form)
        form.cutToolStripMenuItem.Click.Add (handleFormCut form)
        form.cutContextMenuItem.Click.Add (handleFormCut form)
        form.copyToolStripMenuItem.Click.Add (handleFormCopy form)
        form.copyContextMenuItem.Click.Add (handleFormCopy form)
        form.pasteToolStripMenuItem.Click.Add (handleFormPaste false form)
        form.pasteContextMenuItem.Click.Add (handleFormPaste true form)
        form.quickSizeToolStripButton.Click.Add (handleFormQuickSize form)
        form.resetCameraButton.Click.Add (handleFormResetCamera form)
        form.reloadAssetsButton.Click.Add (handleFormReloadAssets form)
        form.layerTabControl.Deselected.Add (handleFormLayerTabDeselected form)
        form.layerTabControl.Selected.Add (handleFormLayerTabSelected form)
        form.evalButton.Click.Add (handleEvalClick form)
        form.clearOutputButton.Click.Add (handleClearOutputClick form)
        form.createEntityComboBox.SelectedIndexChanged.Add (handleCreateEntityComboBoxSelectedIndexChanged form)
        form.entityDesignerPropertyAddButton.Click.Add (handleEntityDesignerPropertyAddClick false form)
        form.entityDesignerPropertyDefaultButton.Click.Add (handleEntityDesignerPropertyAddClick true form)
        form.entityDesignerPropertyRemoveButton.Click.Add (handleEntityDesignerPropertyRemoveClick form)
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
            form.evalInputTextBox.Keywords0 <- syntax.Keywords0 + " " + WorldBindings.BindingKeywords
            form.evalInputTextBox.Keywords1 <- syntax.Keywords1
            form.evalOutputTextBox.Keywords0 <- syntax.Keywords0 + " " + WorldBindings.BindingKeywords
            form.evalOutputTextBox.Keywords1 <- syntax.Keywords1
            form.preludeTextBox.Keywords0 <- syntax.Keywords0 + " " + WorldBindings.BindingKeywords
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

        // populate entity designer property types
        form.entityDesignerPropertyTypeComboBox.Items.Add ("false : bool") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("true : bool") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("0 : int") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("0L : int64") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("0f : single") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("0d : double") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("\"\" : string") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[v2 0f 0f] : Vector2") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[v4 0f 0f 0f 0f] : Vector4") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[v2i 0 0] : Vector2i") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[some 0] : int option") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[right 0] : Either<int, obj>") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[list 0] : int list") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[ring 0] : int Set") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[table [0 \"\"]] : Map<int, string>") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[tuple 0 0] : int * int") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[record AssetTag [PackageName \"Default\"] [AssetName \"Image\"]] : AssetTag") |> ignore

        // clear undo buffers
        form.eventFilterTextBox.EmptyUndoBuffer ()
        form.preludeTextBox.EmptyUndoBuffer ()
        form.assetGraphTextBox.EmptyUndoBuffer ()
        form.overlayerTextBox.EmptyUndoBuffer ()

        // finally, show and activate form
        form.Show ()
        form.Activate ()
        form

    /// Attempt to make a world for use in the Gaia form.
    /// You can make your own world instead and use the Gaia.attachToWorld instead (so long as the world satisfies said
    /// function's various requirements.
    let tryMakeWorld useGameplayScreen sdlDeps worldConfig (plugin : NuPlugin) =
        let worldEir = World.tryMake sdlDeps worldConfig plugin
        match worldEir with
        | Right world ->
            let world =
                World.setEventFilter
                    (EventFilter.NotAny [EventFilter.Pattern (Rexpr "Update", []); EventFilter.Pattern (Rexpr "Mouse/Move", [])])
                    world
            let screenDispatcher =
                if useGameplayScreen
                then plugin.GetEditorScreenDispatcher ()
                else typeof<ScreenDispatcher>
            let (screen, world) =
                World.createScreen3 screenDispatcher.Name (Some Simulants.DefaultScreen.Name) world
            let world =
                if Seq.isEmpty (World.getLayers screen world)
                then World.createLayer (Some "Layer") screen world |> snd
                else world
            let world = World.selectScreen screen world
            Right world
        | Left error -> Left error

    /// Attempt to make SdlDeps needed to use in the Gaia form.
    let tryMakeSdlDeps (form : GaiaForm) =
        let sdlViewConfig = ExistingWindow form.displayPanel.Handle
        let sdlConfig =
            { ViewConfig = sdlViewConfig
              ViewW = form.displayPanel.MaximumSize.Width
              ViewH = form.displayPanel.MaximumSize.Height
              RendererFlags = Constants.Render.DefaultRendererFlags
              AudioChunkSize = Constants.Audio.DefaultBufferSize }
        SdlDeps.attemptMake sdlConfig

    /// Run Gaia from the F# evaluator.
    let runFromRepl runWhile targetDir sdlDeps form world =
        Globals.World <- world
        run3 runWhile targetDir sdlDeps form
        Globals.World

    /// Run Gaia in isolation.
    let run worldConfig =
        let (savedState, targetDir, plugin) = selectTargetDirAndMakeNuPlugin ()
        use form = createForm ()
        Globals.Form <- form
        match tryMakeSdlDeps form with
        | Right sdlDeps ->
            match tryMakeWorld savedState.UseGameplayScreen sdlDeps worldConfig plugin with
            | Right world ->
                Globals.World <- world
                let _ = run3 tautology targetDir sdlDeps form
                Constants.Engine.SuccessExitCode
            | Left error -> failwith error
        | Left error -> failwith error

    let init nuConfig =
        Nu.init nuConfig
        Globals.SelectEntity <- selectEntity