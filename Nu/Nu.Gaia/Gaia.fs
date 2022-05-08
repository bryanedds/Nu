// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open System.IO
open System.Collections
open System.ComponentModel
open System.Drawing
open System.Linq
open System.Numerics
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
        let selectedGroup = (getEditorState world).SelectedGroup
        let (entities, world) = World.getEntitiesInView2d world
        let entitiesInGroup =
            Enumerable.ToList
                (Enumerable.Where
                    (entities, fun entity -> entity.GetVisible world && entity.Group = selectedGroup))
        (entitiesInGroup, world)

    let private getSnaps (form : GaiaForm) =
        let positionSnap = snd (Int32.TryParse form.positionSnapTextBox.Text)
        let rotationSnap = snd (Int32.TryParse form.rotationSnapTextBox.Text)
        (positionSnap, rotationSnap)
    
    let private getCreationElevation (form : GaiaForm) =
        snd (Single.TryParse form.createElevationTextBox.Text)

    let private generateEntityName dispatcherName world =
        let selectedGroup = (getEditorState world).SelectedGroup
        let name = Gen.nameForEditor dispatcherName
        let mutable entity = Entity (selectedGroup.GroupAddress <-- ntoa name)
        while entity.Exists world do
            let name = Gen.nameForEditor dispatcherName
            entity <- Entity (selectedGroup.GroupAddress <-- ntoa name)
        entity.Name

    let private refreshOverlayComboBox (form : GaiaForm) world =
        form.overlayComboBox.Items.Clear ()
        form.overlayComboBox.Items.Add "(Default Overlay)" |> ignore
        form.overlayComboBox.Items.Add "(Routed Overlay)" |> ignore
        form.overlayComboBox.Items.Add "(No Overlay)" |> ignore
        let overlays = world |> World.getOverlays |> Map.toValueList
        for overlay in overlays do
            form.overlayComboBox.Items.Add overlay.OverlayName |> ignore
        form.overlayComboBox.SelectedIndex <- 0

    let private refreshCreateComboBox (form : GaiaForm) world =
        form.createEntityComboBox.Items.Clear ()
        for dispatcherKvp in World.getEntityDispatchers world do
            form.createEntityComboBox.Items.Add dispatcherKvp.Key |> ignore
        form.createEntityComboBox.SelectedIndex <- 0

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

    let private refreshHierarchyTreeView (_ : GaiaForm) (_ : World) =
        addWorldChanger $ fun world ->
            updateEditorState (fun state -> { state with RefreshHierarchyViewRequested = true }) world

    let private refreshHierarchyTreeViewImpl (form : GaiaForm) world =
        // TODO: this code causes severe performance issues. To unfuck performance, we will probably have to find
        // a way to update the hierarchy tree without a complete rebuild of it - IE, updating it in-place and
        // imperatively.
        let treeNodesState = form.hierarchyTreeView.GetExpandedNodesState ()
        form.hierarchyTreeView.Nodes.Clear ()
        let selectedGroup = (getEditorState world).SelectedGroup
        let groupNode = TreeNode selectedGroup.Name
        groupNode.Name <- Constants.Editor.GroupNodeKey
        form.hierarchyTreeView.Nodes.Add groupNode |> ignore
        let entities = World.getEntitiesFlattened selectedGroup world
        for entity in entities do
            let mutable namesUsed = [||]
            let mutable parentNode = groupNode
            for name in entity.Surnames do
                namesUsed <- Array.add name namesUsed
                let childNodeKey = namesUsed |> rtoa |> string
                if not (parentNode.Nodes.ContainsKey childNodeKey) then
                    let childNode = TreeNode name
                    childNode.Name <- childNodeKey
                    parentNode.Nodes.Add childNode |> ignore
                    parentNode <- childNode
                else parentNode <- parentNode.Nodes.[childNodeKey]
        form.hierarchyTreeView.RestoreExpandedNodesState treeNodesState
        groupNode.Expand () // root node is always expanded
        updateEditorState (fun state -> { state with RefreshHierarchyViewRequested = false }) world

    let private refreshGroupTabs (form : GaiaForm) world =

        // add groups imperatively to preserve existing group tabs
        let groups = World.getGroups Globals.Screen world
        let groupTabPages = form.groupTabControl.TabPages
        for group in groups do
            let groupName = group.Name
            if not (groupTabPages.ContainsKey groupName) then
                groupTabPages.Add (groupName, groupName)
    
        // remove groups imperatively to preserve existing group tabs 
        for groupTabPage in groupTabPages do
            if Seq.notExists (fun (group : Group) -> group.Name = groupTabPage.Name) groups then
                groupTabPages.RemoveByKey groupTabPage.Name

    let private selectEntity entity (form : GaiaForm) world =
        Globals.World <- world // must be set for property grid
        let entityTds = { DescribedEntity = entity; Form = form }
        let previousGridItem = form.entityPropertyGrid.SelectedGridItem
        form.entityPropertyGrid.SelectedObject <- entityTds
        let gridItems = form.entityPropertyGrid.SelectedGridItem.Parent.Parent.GridItems.["\rScene Properties"].GridItems
        if notNull previousGridItem then
            match Seq.tryFind (fun (gridItem : GridItem) -> gridItem.Label = previousGridItem.Label) (enumerable gridItems) with
            | Some gridItem -> form.entityPropertyGrid.SelectedGridItem <- gridItem
            | None -> if entity.GetModelGeneric<obj> world <> box () then form.entityPropertyGrid.SelectedGridItem <- gridItems.["Model"]
        elif entity.GetModelGeneric<obj> world <> box () then form.entityPropertyGrid.SelectedGridItem <- gridItems.["Model"]
        form.entityIgnorePropertyBindingsCheckBox.Checked <- entityTds.DescribedEntity.GetIgnorePropertyBindings world
        form.propertyTabControl.SelectTab 0 // show entity properties

    let private deselectEntity (form : GaiaForm) world =
        Globals.World <- world // must be set for property grid
        form.entityIgnorePropertyBindingsCheckBox.Checked <- false
        form.entityPropertyGrid.SelectedObject <- null

    let private refreshEntityPropertyGrid (form : GaiaForm) world =
        match form.entityPropertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            Globals.World <- world // must be set for property grid
            if entityTds.DescribedEntity.Exists world then
                form.entityPropertyGrid.Refresh ()
                form.entityIgnorePropertyBindingsCheckBox.Checked <- entityTds.DescribedEntity.GetIgnorePropertyBindings world
            else deselectEntity form world
        | _ -> ()

    let private selectGroup group (form : GaiaForm) world =
        let groupTds = { DescribedGroup = group; Form = form }
        Globals.World <- world // must be set for property grid
        form.groupPropertyGrid.SelectedObject <- groupTds

    let private deselectGroup (form : GaiaForm) world =
        Globals.World <- world // must be set for property grid
        form.groupPropertyGrid.SelectedObject <- null

    let private refreshGroupPropertyGrid (form : GaiaForm) world =
        match form.groupPropertyGrid.SelectedObject with
        | :? GroupTypeDescriptorSource as groupTds ->
            Globals.World <- world // must be set for property grid
            if groupTds.DescribedGroup.Exists world then
                //form.groupIgnorePropertyBindingsCheckBox.Checked <- groupTds.DescribedGroup.GetIgnorePropertyBindings world
                form.groupPropertyGrid.Refresh ()
            else deselectGroup form world
        | _ -> ()

    let private refreshFormOnUndoRedo (form : GaiaForm) world =
        form.advancingButton.Checked <- false
        refreshEntityPropertyGrid form world
        refreshGroupPropertyGrid form world
        refreshGroupTabs form world
        refreshHierarchyTreeView form world

    let private canEditWithMouse (form : GaiaForm) world =
        World.isAdvancing world &&
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
            let mousePositionWorld = World.mouseToWorld2d (entity.GetAbsolute world) mousePosition world
            if Math.isPointInBounds2d mousePositionWorld (entity.GetPerimeterOriented world).Box2
            then (Some entity, world)
            else tryMousePickInner form mousePosition world
        | _ -> tryMousePickInner form mousePosition world

    let private handleNuGroupLifeCycle (form : GaiaForm) (_ : Event<LifeCycleData, Screen>) world =
        Globals.World <- world // handle re-entry
        refreshGroupTabs form world
        refreshHierarchyTreeView form world
        (Cascade, world)

    let private handleNuEntityLifeCycle (form : GaiaForm) (evt : Event<LifeCycleData, Screen>) world =
        Globals.World <- world // handle re-entry
        match evt.Data with
        | RegisterData _ ->
            refreshHierarchyTreeView form world
            (Cascade, world)
        | UnregisteringData simulant ->
            let nodeKey = scstring (rtoa (simulant :?> Entity).Surnames)
            for node in collectHierarchyTreeNodes form world do
                if node.Name = nodeKey then
                    node.Remove ()
            match form.entityPropertyGrid.SelectedObject with
            | null -> (Cascade, world)
            | :? EntityTypeDescriptorSource as entityTds ->
                if atoa evt.Publisher.SimulantAddress = entityTds.DescribedEntity.EntityAddress then
                    let world = updateEditorState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                    deselectEntity form world
                    (Cascade, world)
                else (Cascade, world)
            | _ -> failwithumf ()
        | MountOptChangeData _ ->
            refreshHierarchyTreeView form world
            (Cascade, world)

    let private handleNuMouseRightDown (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let handled = if World.isAdvancing world then Cascade else Resolve
        let mousePosition = World.getMousePosition2d world
        let (_, world) = tryMousePick mousePosition form world
        let world = updateEditorState (fun editorState -> { editorState with RightClickPosition = mousePosition }) world
        (handled, world)

    let private handleNuEntityDragBegin (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        if not (canEditWithMouse form world) then
            let handled = if World.isAdvancing world then Cascade else Resolve
            let mousePosition = World.getMousePosition2d world
            match tryMousePick mousePosition form world with
            | (Some entity, world) ->
                Globals.pushPastWorld world
                let world =
                    updateEditorState (fun editorState ->
                        let mousePositionWorld = World.mouseToWorld2d (entity.GetAbsolute world) mousePosition world
                        let entityPosition =
                            if entity.MountExists world
                            then entity.GetPositionLocal world
                            else entity.GetPosition world
                        { editorState with DragEntityState = DragEntityPosition (entityPosition.V2 + mousePositionWorld, mousePositionWorld, entity) })
                        world
                (handled, world)
            | (None, world) -> (handled, world)
        else (Cascade, world)

    let private handleNuEntityDragEnd (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        if canEditWithMouse form world then (Cascade, world)
        else
            let handled = if World.isAdvancing world then Cascade else Resolve
            match (getEditorState world).DragEntityState with
            | DragEntityPosition _
            | DragEntityRotation _ ->
                let world = updateEditorState (fun editorState -> { editorState with DragEntityState = DragEntityNone }) world
                form.entityPropertyGrid.Refresh ()
                (handled, world)
            | DragEntityNone -> (Resolve, world)

    let private handleNuCameraDragBegin (_ : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let mousePosition = World.getMousePosition2d world
        let mousePositionScreen = World.mouseToScreen2d mousePosition world
        let dragState = DragCameraPosition (World.getEyePosition2d world + mousePositionScreen, mousePositionScreen)
        let world = updateEditorState (fun editorState -> { editorState with DragCameraState = dragState }) world
        (Resolve, world)

    let private handleNuCameraDragEnd (_ : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        match (getEditorState world).DragCameraState with
        | DragCameraPosition _ ->
            let world = updateEditorState (fun editorState -> { editorState with DragCameraState = DragCameraNone }) world
            (Resolve, world)
        | DragCameraNone -> (Resolve, world)

    let private monitorLifeCycleEvents form world =
        let world = World.monitor (handleNuEntityLifeCycle form) (Events.LifeCycle (nameof Entity)) Globals.Screen world
        let world = World.monitor (handleNuGroupLifeCycle form) (Events.LifeCycle (nameof Group)) Globals.Screen world
        world

    let private trySaveSelectedGroup filePath world =
        let oldWorld = world
        let selectedGroup = (getEditorState world).SelectedGroup
        try World.writeGroupToFile filePath selectedGroup world
        with exn ->
            World.choose oldWorld |> ignore
            MessageBox.Show ("Could not save file due to: " + scstring exn, "File save error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore

    let private tryLoadSelectedGroup (form : GaiaForm) filePath world =

        // old world in case we need to rewind
        let oldWorld = world

        try // destroy current group
            let selectedGroup = (getEditorState world).SelectedGroup
            let world = World.destroyGroupImmediate selectedGroup world

            // load and add group, updating tab and selected group in the process
            let groupDescriptorStr = File.ReadAllText filePath
            let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
            let groupName = match groupDescriptor.GroupProperties.TryFind "Name" with Some (Atom (name, _)) -> name | _ -> failwithumf ()
            let group = Globals.Screen / groupName
            if not (group.Exists world) then
                let (group, world) = World.readGroup groupDescriptor None Globals.Screen world
                form.groupTabControl.SelectedTab.Text <- group.Name
                form.groupTabControl.SelectedTab.Name <- group.Name
                let world = updateEditorState (fun editorState -> { editorState with SelectedGroup = group }) world

                // refresh hierarchy view
                refreshHierarchyTreeView form world
                (Some group, world)
            
            // handle load failure
            else
                let world = World.choose oldWorld
                MessageBox.Show ("Could not load group file with same name as an existing group", "File load error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                (None, world)

        // handle load failure
        with exn ->
            let world = World.choose oldWorld
            MessageBox.Show ("Could not load group file due to: " + scstring exn, "File load error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            (None, world)

    let private handleFormExit (form : GaiaForm) (_ : EventArgs) =
        form.Close ()

    let private handleFormCreateElevationPlusClick (form : GaiaForm) (_ : EventArgs) =
        let elevation = snd (Single.TryParse form.createElevationTextBox.Text)
        form.createElevationTextBox.Text <- scstring (elevation + 1.0f)

    let private handleFormCreateElevationMinusClick (form : GaiaForm) (_ : EventArgs) =
        let elevation = snd (Single.TryParse form.createElevationTextBox.Text)
        form.createElevationTextBox.Text <- scstring (elevation - 1.0f)

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
        let selectedGroup = (getEditorState world).SelectedGroup
        let surnamesStrs =
            World.getEntitiesFlattened selectedGroup world |>
            Seq.filter (fun entity -> not (Gen.isName entity.Name)) |>
            Seq.map (fun entity -> entity.Surnames |> Address.makeFromArray |> string) |>
            flip Seq.append [Constants.Editor.NonePick] |>
            Seq.toArray
        entityPicker.entityListBox.Items.AddRange (Array.map box surnamesStrs)
        entityPicker.entityListBox.DoubleClick.Add (fun _ -> entityPicker.DialogResult <- DialogResult.OK)
        entityPicker.okButton.Click.Add (fun _ -> entityPicker.DialogResult <- DialogResult.OK)
        entityPicker.cancelButton.Click.Add (fun _ -> entityPicker.Close ())
        entityPicker.searchTextBox.TextChanged.Add(fun _ ->
            entityPicker.entityListBox.Items.Clear ()
            for namesStr in surnamesStrs do
                if namesStr.Contains entityPicker.searchTextBox.Text || namesStr = Constants.Editor.NonePick then
                    entityPicker.entityListBox.Items.Add namesStr |> ignore)
        match entityPicker.ShowDialog () with
        | DialogResult.OK ->
            match entityPicker.entityListBox.SelectedItem with
            | :? string as parentSurnamesStr ->
                match parentSurnamesStr with
                | Constants.Editor.NonePick ->
                    entityTds.DescribedEntity.SetMountOptWithAdjustment None world
                | _ ->
                    let entity = entityTds.DescribedEntity
                    let parent = Entity (string entity.Group.GroupAddress + Constants.Address.SeparatorStr + parentSurnamesStr)
                    let parentRelation = Relation.relate entity.EntityAddress parent.EntityAddress
                    if parentRelation <> Relation.makeCurrent () then
                        form.propertyValueTextBoxText <- scstring parentRelation
                        if propertyDescriptor.Name = "MountOpt"
                        then entity.SetMountOptWithAdjustment (Some parentRelation) world
                        else form.applyPropertyButton.PerformClick (); world
                    else form.applyPropertyButton.PerformClick (); world
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
                                let overlays = World.getOverlays Globals.World
                                let overlayNames = overlays |> Map.toValueList |> List.map (fun overlay -> overlay.OverlayName)
                                (String.concat " " overlayNames, "", PrettyPrinter.defaultPrinter)
                            | "FacetNames" ->
                                let facetNames = Globals.World |> World.getFacets |> Map.toKeyList
                                (String.concat " " facetNames, "", PrettyPrinter.defaultPrinter)
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
                            selectedGridItem.PropertyDescriptor.PropertyType = typeof<Entity Relation option> ||
                            (selectedGridItem.PropertyDescriptor.PropertyType.IsGenericType &&
                             selectedGridItem.PropertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>)
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
        else // assume group
            match (form.groupPropertyGrid.SelectedObject, form.groupPropertyGrid.SelectedGridItem) with
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
        | :? GroupTypeDescriptorSource as groupTds ->
            match form.groupPropertyGrid.SelectedGridItem with
            | null -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."
            | selectedGridItem ->
                match selectedGridItem.GridItemType with
                | GridItemType.Property when form.propertyNameLabel.Text = selectedGridItem.Label ->
                    let propertyDescriptor = selectedGridItem.PropertyDescriptor :?> GroupPropertyDescriptor
                    let typeConverter = SymbolicConverter (false, None, selectedGridItem.PropertyDescriptor.PropertyType)
                    try form.propertyValueTextBox.EndUndoAction ()
                        let strEscaped = form.propertyValueTextBox.Text.TrimEnd ()
                        let strUnescaped = String.unescape strEscaped
                        let propertyValue = typeConverter.ConvertFromString strUnescaped
                        propertyDescriptor.SetValue (groupTds, propertyValue)
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
        let preludeFilePath = preludeSourceDir + "/" + Assets.Global.PreludeFilePath
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
        World.tryReloadAssetGraph assetSourceDir targetDir Constants.Engine.RefinementDir world

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
        let assetGraphFilePath = assetSourceDir + "/" + Assets.Global.AssetGraphFilePath
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
        let overlayerFilePath = overlayerSourceDir + "/" + Assets.Global.OverlayerFilePath
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

    let private handleFormEntityPropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormGroupPropertyGridSelectedObjectsChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormGroupPropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handlePropertyTabControlSelectedIndexChanged (form : GaiaForm) (_ : EventArgs) =
        if form.propertyTabControl.SelectedIndex = 0
        then refreshEntityPropertyGrid form Globals.World
        else refreshGroupPropertyGrid form Globals.World

    let private handleFormPropertyRefreshClick (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormPropertyApplyClick (form : GaiaForm) (_ : EventArgs) =
        applyPropertyEditor form

    let private handleFormHierarchyTreeViewItemDrag (form : GaiaForm) (e : ItemDragEventArgs) =
        if e.Button = MouseButtons.Left then
            form.DoDragDrop (e.Item, DragDropEffects.Move) |> ignore

    let private handleFormHierarchyTreeViewDragEnter (_ : GaiaForm) (e : DragEventArgs) =
        e.Effect <- e.AllowedEffect

    let private handleFormHierarchyTreeViewDragOver (form : GaiaForm) (e : DragEventArgs) =
        let targetPoint = form.hierarchyTreeView.PointToClient (Point (e.X, e.Y))
        form.hierarchyTreeView.SelectedNode <- form.hierarchyTreeView.GetNodeAt targetPoint

    let private handleFormHierarchyTreeViewDragDrop (form : GaiaForm) (e : DragEventArgs) =

        // TODO: lift this out.
        let rec containsNode (source : TreeNode) (target : TreeNode) =
            if isNull target.Parent then false
            elif target.Parent = source then true
            else containsNode source target.Parent

        addWorldChanger $ fun world ->
            Globals.pushPastWorld world
            let targetPoint = form.hierarchyTreeView.PointToClient (Point (e.X, e.Y))
            let targetNodeOpt = form.hierarchyTreeView.GetNodeAt targetPoint
            let draggedNode = e.Data.GetData typeof<TreeNode> :?> TreeNode
            if draggedNode <> targetNodeOpt && notNull targetNodeOpt && not (containsNode draggedNode targetNodeOpt) then
                let selectedGroup = (getEditorState world).SelectedGroup
                let source = Entity (selectedGroup.GroupAddress <-- Address.makeFromString draggedNode.Name)
                let (mountToParent, target) =
                    if targetNodeOpt.Name = Constants.Editor.GroupNodeKey
                    then (false, Group selectedGroup.GroupAddress / source.Name)
                    else (true, Entity (selectedGroup.GroupAddress <-- Address.makeFromString targetNodeOpt.Name) / source.Name)
                let mountOpt = if mountToParent then Some (Relation.makeParent ()) else None
                let world = World.renameEntityImmediate source target world
                target.SetMountOptWithAdjustment mountOpt world
            else world

    let private handleFormHierarchyTreeViewNodeSelect (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            if notNull form.hierarchyTreeView.SelectedNode then
                let nodeKey = form.hierarchyTreeView.SelectedNode.Name
                if nodeKey <> Constants.Editor.GroupNodeKey then
                    let address = Address.makeFromString nodeKey
                    let entity = Entity ((getEditorState world).SelectedGroup.GroupAddress <-- atoa address)
                    if entity.Exists world then selectEntity entity form world
                    world
                else world
            else world

    let private handleFormCreateEntity atMouse (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let oldWorld = world
            try Globals.pushPastWorld world
                let selectedGroup = (getEditorState world).SelectedGroup
                let dispatcherName = form.createEntityComboBox.Text
                let overlayNameDescriptor =
                    match form.overlayComboBox.Text with
                    | "(Default Overlay)" -> DefaultOverlay
                    | "(Routed Overlay)" -> RoutedOverlay
                    | "(No Overlay)" -> NoOverlay
                    | overlayName -> ExplicitOverlay overlayName
                let name = generateEntityName dispatcherName world
                let (entity, world) = World.createEntity5 dispatcherName (Some [|name|]) overlayNameDescriptor selectedGroup world
                let (positionSnap, rotationSnap) = getSnaps form
                let mousePosition = World.getMousePosition2d world
                let entityPosition =
                    if atMouse
                    then World.mouseToWorld2d (entity.GetAbsolute world) mousePosition world
                    else World.mouseToWorld2d (entity.GetAbsolute world) (World.getEyeSize2d world * 0.5f) world
                let mutable entityTransform = entity.GetTransform world
                entityTransform.Position <- entityPosition.V3
                entityTransform.Size <- entity.GetQuickSize world
                entityTransform.Elevation <- getCreationElevation form
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
        use groupCreationForm = new GroupCreationForm ()
        groupCreationForm.StartPosition <- FormStartPosition.CenterParent
        groupCreationForm.dispatcherTextBox.Text <- typeof<GroupDispatcher>.Name
        groupCreationForm.okButton.Click.Add $ fun _ ->
            addWorldChanger $ fun world ->
                let oldWorld = world
                Globals.pushPastWorld world
                let groupName = groupCreationForm.nameTextBox.Text
                let groupDispatcherName = groupCreationForm.dispatcherTextBox.Text
                try if String.length groupName = 0 then failwith "Group name cannot be empty in Gaia due to WinForms limitations."
                    let world = World.createGroup4 groupDispatcherName (Some groupName) Globals.Screen world |> snd
                    refreshGroupTabs form world
                    refreshHierarchyTreeView form world
                    deselectEntity form world
                    form.groupTabControl.SelectTab (form.groupTabControl.TabPages.IndexOfKey groupName)
                    world
                with exn ->
                    let world = World.choose oldWorld
                    MessageBox.Show ("Could not create group due to: " + scstring exn, "Group creation error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            groupCreationForm.Close ()
        groupCreationForm.cancelButton.Click.Add (fun _ -> groupCreationForm.Close ())
        groupCreationForm.ShowDialog form |> ignore

    let private handleFormSave saveAs (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let group = (getEditorState world).SelectedGroup
            form.saveFileDialog.Title <- "Save '" + group.Name + "' As"
            match Map.tryFind group.GroupAddress (getEditorState world).FilePaths with
            | Some filePath -> form.saveFileDialog.FileName <- filePath
            | None -> form.saveFileDialog.FileName <- String.Empty
            if saveAs || String.IsNullOrWhiteSpace form.saveFileDialog.FileName then
                match form.saveFileDialog.ShowDialog form with
                | DialogResult.OK ->
                    let filePath = form.saveFileDialog.FileName
                    trySaveSelectedGroup filePath world
                    updateEditorState (fun state ->
                        let filePaths = Map.add group.GroupAddress filePath state.FilePaths
                        { state with FilePaths = filePaths })
                        world
                | _ -> world
            else trySaveSelectedGroup form.saveFileDialog.FileName world; world

    let private handleFormOpen (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            form.openFileDialog.FileName <- String.Empty
            match form.openFileDialog.ShowDialog form with
            | DialogResult.OK ->
                Globals.pushPastWorld world
                let filePath = form.openFileDialog.FileName
                match tryLoadSelectedGroup form filePath world with
                | (Some group, world) ->
                    let world =
                        updateEditorState (fun state ->
                            let filePaths = Map.add group.GroupAddress filePath state.FilePaths
                            { state with FilePaths = filePaths })
                            world
                    deselectEntity form world // currently selected entity may be gone if loading into an existing group
                    world
                | (None, world) -> world
            | _ -> world

    let private handleFormClose (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match form.groupTabControl.TabPages.Count with
            | 1 ->
                MessageBox.Show ("Cannot close the only remaining group.", "Group close error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world
            | _ ->
                Globals.pushPastWorld world
                let group = (getEditorState world).SelectedGroup
                let world = World.destroyGroupImmediate group world
                deselectEntity form world
                form.groupTabControl.TabPages.RemoveByKey group.Name
                updateEditorState (fun editorState ->
                    let groupTabControl = form.groupTabControl
                    let groupTab = groupTabControl.SelectedTab
                    { editorState with
                        SelectedGroup = Globals.Screen / groupTab.Text
                        FilePaths = Map.remove group.GroupAddress editorState.FilePaths })
                    world

    let private handleFormUndo (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            if not (World.getImperative world) then
                match Globals.PastWorlds with
                | pastWorld :: pastWorlds ->
                    let futureWorld = World.shelve world
                    let world = World.unshelve pastWorld
                    Globals.PastWorlds <- pastWorlds
                    Globals.FutureWorlds <- futureWorld :: Globals.FutureWorlds
                    let world = World.setUpdateRate 0L world
                    refreshFormOnUndoRedo form world
                    world
                | [] -> world
            else world

    let private handleFormRedo (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            if not (World.getImperative world) then
                match Globals.FutureWorlds with
                | futureWorld :: futureWorlds ->
                    let pastWorld = World.shelve world
                    let world = World.unshelve futureWorld
                    Globals.PastWorlds <- pastWorld :: Globals.PastWorlds
                    Globals.FutureWorlds <- futureWorlds
                    let world = World.setUpdateRate 0L world
                    refreshFormOnUndoRedo form world
                    world
                | [] -> world
            else world

    let private handleFormAdvancingChanged (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let updateRate = if form.advancingButton.Checked then 1L else 0L
            let (pastWorld, world) = (world, World.setUpdateRate updateRate world)
            if updateRate <> 0L then
                form.displayPanel.Focus () |> ignore
                Globals.pushPastWorld pastWorld
            world

    let private handleFormSongPlayback (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            if form.songPlaybackButton.Checked
            then World.setMasterSongVolume 1.0f world
            else World.setMasterSongVolume 0.0f world

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
            let selectedGroup = (getEditorState world).SelectedGroup
            let (positionSnap, rotationSnap) = getSnaps form
            let editorState = getEditorState world
            let surnamesOpt = World.tryGetEntityDispatcherNameOnClipboard world |> Option.map (flip generateEntityName world) |> Option.map Array.singleton
            let (entityOpt, world) = World.pasteEntityFromClipboard atMouse editorState.RightClickPosition positionSnap rotationSnap surnamesOpt selectedGroup world
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
                let world = entity.SetSize (entity.GetQuickSize world) world
                Globals.World <- world // must be set for property grid
                form.entityPropertyGrid.Refresh ()
                world
            | _ -> failwithumf ()

    let private handleFormResetCamera (_ : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            World.setEyePosition2d v2Zero world

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

    let private handleFormGroupTabDeselected (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            deselectEntity form world
            refreshEntityPropertyGrid form world
            refreshGroupPropertyGrid form world
            world

    let private handleFormGroupTabSelected (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            let selectedGroup =
                let groupTabControl = form.groupTabControl
                let groupTab = groupTabControl.SelectedTab
                Globals.Screen / groupTab.Text
            let world = updateEditorState (fun editorState -> { editorState with SelectedGroup = selectedGroup}) world
            refreshEntityPropertyGrid form world
            refreshHierarchyTreeView form world
            selectGroup selectedGroup form world
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
                let group = (getEditorState world).SelectedGroup
                let (selectedSimulant, localFrame) =
                    match form.entityPropertyGrid.SelectedObject with
                    | :?
                        EntityTypeDescriptorSource as entityTds when
                        form.propertyTabControl.SelectedTab <> form.groupTabPage ->
                        let entity = entityTds.DescribedEntity
                        (entity :> Simulant, entity.GetScriptFrame world)
                    | _ -> (group :> Simulant, group.GetScriptFrame world)
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
                        let selectedGroup = (getEditorState world).SelectedGroup
                        let localFrame = selectedGroup.GetScriptFrame world
                        let struct (evaled, world) = World.evalWithLogging expr localFrame selectedGroup world
                        match Scripting.Expr.toFSharpTypeOpt evaled with
                        | Some dt ->
                            let dvOpt =
                                if defaulting
                                then dt.TryGetDefaultValue ()
                                else ScriptingSystem.tryExport dt evaled world
                            match dvOpt with
                            | Some dv ->
                                let propertyName = form.entityDesignerPropertyNameTextBox.Text
                                let dp = { DesignerType = dt; DesignerValue = dv }
                                let property = { PropertyType = typeof<DesignerProperty>; PropertyValue = dp }
                                let world = entity.AttachProperty propertyName property world
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

    let private handleEntityIgnorePropertyBindingsChanged (form : GaiaForm) (_ : EventArgs) =
        addWorldChanger $ fun world ->
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = entityTds.DescribedEntity
                let world = entity.SetIgnorePropertyBindings form.entityIgnorePropertyBindingsCheckBox.Checked world
                Globals.World <- world // must be set for property grid
                form.entityPropertyGrid.Refresh ()
                world
            | _ -> world

    let private handleKeyboardInput key isKeyFromKeyableControl (form : GaiaForm) world =
        if Form.ActiveForm = (form :> Form) then
            if Keys.F5 = key then form.advancingButton.PerformClick ()
            if Keys.Control = Control.ModifierKeys && Keys.Q = key then handleFormQuickSize form (EventArgs ())
            if Keys.Control = Control.ModifierKeys && Keys.N = key then handleFormNew form (EventArgs ())
            if Keys.Control = Control.ModifierKeys && Keys.O = key then handleFormOpen form (EventArgs ())
            if Keys.Control = Control.ModifierKeys && Keys.S = key then handleFormSave false form (EventArgs ())
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
                    let mousePosition = World.getMousePosition2d world
                    let mousePositionWorld = World.mouseToWorld2d (entity.GetAbsolute world) mousePosition world
                    let entityPosition = (pickOffset - mousePositionWorldOrig) + (mousePositionWorld - mousePositionWorldOrig)
                    let entityPositionSnapped = Math.snapF3d positionSnap entityPosition.V3
                    let world =
                        if entity.MountExists world
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
            let mousePosition = World.getMousePosition2d world
            let mousePositionScreen = World.mouseToScreen2d mousePosition world
            let eyePosition = (pickOffset - mousePositionScreenOrig) + -Constants.Editor.CameraSpeed * (mousePositionScreen - mousePositionScreenOrig)
            let world = World.setEyePosition2d eyePosition world
            updateEditorState (fun editorState ->
                { editorState with DragCameraState = DragCameraPosition (pickOffset, mousePositionScreenOrig) })
                world
        | DragCameraNone -> world

    // TODO: remove code duplication with below
    let private updateUndoButton (form : GaiaForm) world =
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty Globals.PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not (List.isEmpty Globals.PastWorlds) then
            form.undoButton.Enabled <- not (World.getImperative world)
            form.undoToolStripMenuItem.Enabled <- not (World.getImperative world)

    let private updateRedoButton (form : GaiaForm) world =
        if form.redoToolStripMenuItem.Enabled then
            if List.isEmpty Globals.FutureWorlds then
                form.redoButton.Enabled <- false
                form.redoToolStripMenuItem.Enabled <- false
        elif not (List.isEmpty Globals.FutureWorlds) then
            form.redoButton.Enabled <- not (World.getImperative world)
            form.redoToolStripMenuItem.Enabled <- not (World.getImperative world)

    let private updateEditorWorld (form : GaiaForm) world =
        let worldChangersCopy = List.ofSeq Globals.WorldChangers
        Globals.WorldChangers.Clear ()
        let world = List.fold (fun world worldChanger -> worldChanger world) world worldChangersCopy
        let world =
            if (getEditorState world).RefreshHierarchyViewRequested
            then refreshHierarchyTreeViewImpl form world
            else world
        let world = updateEntityDrag form world
        let world = updateCameraDrag form world
        updateUndoButton form world
        updateRedoButton form world
        if  Form.ActiveForm = (form :> Form) &&
            not form.propertyValueTextBox.Focused &&
            not form.applyPropertyButton.Focused &&
            not form.IsClosing then
            refreshPropertyEditor form
        if form.IsDisposed
        then World.exit world
        else world

    /// Attach Gaia to the given world.
    let attachToWorld targetDir form world =
        if World.getSelectedScreen world = Globals.Screen then
            let groups = World.getGroups Globals.Screen world |> Seq.toList
            let (defaultGroup, world) =
                match groups with
                | defaultGroup :: _ ->
                    match World.tryGetKeyedValue<EditorState> Globals.EditorGuid world with
                    | None ->
                        let editorState =
                            { TargetDir = targetDir
                              RightClickPosition = Vector2.Zero
                              DragEntityState = DragEntityNone
                              DragCameraState = DragCameraNone
                              SelectedGroup = defaultGroup
                              FilePaths = Map.empty
                              RefreshHierarchyViewRequested = false }
                        let world = World.addKeyedValue Globals.EditorGuid editorState world
                        let world = World.subscribe (handleNuMouseRightDown form) Events.MouseRightDown Simulants.Game world
                        let world = World.subscribe (handleNuEntityDragBegin form) Events.MouseLeftDown Simulants.Game world
                        let world = World.subscribe (handleNuEntityDragEnd form) Events.MouseLeftUp Simulants.Game world
                        let world = World.subscribe (handleNuCameraDragBegin form) Events.MouseCenterDown Simulants.Game world
                        let world = World.subscribe (handleNuCameraDragEnd form) Events.MouseCenterUp Simulants.Game world
                        (defaultGroup, world)
                    | Some _ -> (defaultGroup, world) // NOTE: conclude world is already attached
                | [] -> failwith ("Cannot attach Gaia to a world with no groups inside the '" + scstring Globals.Screen + "' screen.")
            let world = monitorLifeCycleEvents form world
            (defaultGroup, world)
        else failwith ("Cannot attach Gaia to a world with a screen selected other than '" + scstring Globals.Screen + "'.")

    let rec private tryRun3 runWhile sdlDeps (form : GaiaForm) =
        try World.runWithoutCleanUp
                runWhile
                (fun world -> let world = updateEditorWorld form world in (Globals.World <- world; world))
                (fun world -> Application.DoEvents (); world)
                sdlDeps Live None None Globals.World |>
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
        let (defaultGroup, world) = attachToWorld targetDir form Globals.World
        let world = World.setMasterSongVolume 0.0f world // no song playback in editor by default
        Globals.World <- world
        refreshOverlayComboBox form Globals.World
        refreshCreateComboBox form Globals.World
        refreshGroupTabs form Globals.World
        refreshHierarchyTreeView form Globals.World
        selectGroup defaultGroup form Globals.World
        form.advancingButton.CheckState <- CheckState.Unchecked
        form.songPlaybackButton.CheckState <- if World.getMasterSongVolume world = 0.0f then CheckState.Unchecked else CheckState.Checked
        form.displayPanel.Focus () |> ignore // keeps user from having to manually click on displayPanel to interact
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
        let assemblyTypes =
            try let assembly = Assembly.Load (File.ReadAllBytes filePath)
                assembly.GetTypes ()
            with _ ->
                Log.info "Could not load assembly dependencies when utilitzing Assembly.Load (.NET does not look in the assembly's directory for dependencies for some reason). Using non-shadow assemply load instead."
                let assembly = Assembly.LoadFrom filePath
                assembly.GetTypes ()
        let dispatcherTypeOpt = Array.tryFind (fun (ty : Type) -> ty.IsSubclassOf typeof<NuPlugin>) assemblyTypes
        match dispatcherTypeOpt with
        | Some ty -> let plugin = Activator.CreateInstance ty :?> NuPlugin in (dirName, plugin)
        | None -> (".", NuPlugin ())

    /// Select a target directory for the desired plugin and its assets.
    let selectTargetDirAndMakeNuPlugin () =
        let savedState =
            try scvalue (File.ReadAllText Constants.Editor.SavedStateFilePath)
            with _ -> { BinaryFilePath = ""; UseGameplayScreen = false; UseImperativeExecution = false }
        use startForm = new StartForm ()
        startForm.binaryFilePathText.Text <- savedState.BinaryFilePath
        startForm.useGameplayScreenCheckBox.Checked <- savedState.UseGameplayScreen
        startForm.useImperativeExecutionCheckBox.Checked <- savedState.UseImperativeExecution
        if  startForm.ShowDialog () = DialogResult.OK &&
            not (String.IsNullOrWhiteSpace (startForm.binaryFilePathText.Text)) then
            let savedState =
                { BinaryFilePath = startForm.binaryFilePathText.Text
                  UseGameplayScreen = startForm.useGameplayScreenCheckBox.Checked
                  UseImperativeExecution = startForm.useImperativeExecutionCheckBox.Checked }
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
        form.positionSnapTextBox.Text <- scstring Constants.Editor.PositionSnapDefault
        form.rotationSnapTextBox.Text <- scstring Constants.Editor.RotationSnapDefault
        form.createElevationTextBox.Text <- scstring Constants.Editor.CreationElevationDefault

        // build tree view sorter
        let treeViewSorter =
            { new IComparer with
                member this.Compare (left, right) =
                    let world = Globals.World
                    let selectedGroup = (getEditorState world).SelectedGroup
                    let leftEntity = Entity (Array.append selectedGroup.GroupAddress.Names ((left :?> TreeNode).Name.Split Constants.Address.Separator))
                    let rightEntity = Entity (Array.append selectedGroup.GroupAddress.Names ((right :?> TreeNode).Name.Split Constants.Address.Separator))
                    if leftEntity.Exists world && rightEntity.Exists world
                    then (leftEntity.GetOrder world).CompareTo (rightEntity.GetOrder world)
                    else 0 }

        // same for hierarchy tree view...
        form.hierarchyTreeView.AllowDrop <- true // TODO: configure this in designer instead.
        form.hierarchyTreeView.Sorted <- true
        form.hierarchyTreeView.TreeViewNodeSorter <- treeViewSorter

        // set up events handlers
        form.exitToolStripMenuItem.Click.Add (handleFormExit form)
        form.createElevationPlusButton.Click.Add (handleFormCreateElevationPlusClick form)
        form.createElevationMinusButton.Click.Add (handleFormCreateElevationMinusClick form)
        form.entityPropertyGrid.SelectedObjectsChanged.Add (handleFormEntityPropertyGridSelectedObjectsChanged form)
        form.entityPropertyGrid.SelectedGridItemChanged.Add (handleFormEntityPropertyGridSelectedGridItemChanged form)
        form.groupPropertyGrid.SelectedObjectsChanged.Add (handleFormGroupPropertyGridSelectedObjectsChanged form)
        form.groupPropertyGrid.SelectedGridItemChanged.Add (handleFormGroupPropertyGridSelectedGridItemChanged form)
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
        form.hierarchyTreeView.AfterSelect.Add (handleFormHierarchyTreeViewNodeSelect form)
        form.hierarchyTreeView.ItemDrag.Add (handleFormHierarchyTreeViewItemDrag form)
        form.hierarchyTreeView.DragEnter.Add (handleFormHierarchyTreeViewDragEnter form)
        form.hierarchyTreeView.DragOver.Add (handleFormHierarchyTreeViewDragOver form)
        form.hierarchyTreeView.DragDrop.Add (handleFormHierarchyTreeViewDragDrop form)
        form.createEntityButton.Click.Add (handleFormCreateEntity false form)
        form.createToolStripMenuItem.Click.Add (handleFormCreateEntity false form)
        form.createContextMenuItem.Click.Add (handleFormCreateEntity true form)
        form.deleteEntityButton.Click.Add (handleFormDeleteEntity form)
        form.deleteToolStripMenuItem.Click.Add (handleFormDeleteEntity form)
        form.quickSizeToolStripMenuItem.Click.Add (handleFormQuickSize form)
        form.startStopAdvancingToolStripMenuItem.Click.Add (fun _ -> form.advancingButton.PerformClick ())
        form.deleteContextMenuItem.Click.Add (handleFormDeleteEntity form)
        form.newGroupToolStripMenuItem.Click.Add (handleFormNew form)
        form.saveGroupToolStripMenuItem.Click.Add (handleFormSave false form)
        form.saveGroupAsToolStripMenuItem.Click.Add (handleFormSave true form)
        form.openGroupToolStripMenuItem.Click.Add (handleFormOpen form)
        form.closeGroupToolStripMenuItem.Click.Add (handleFormClose form)
        form.undoButton.Click.Add (handleFormUndo form)
        form.undoToolStripMenuItem.Click.Add (handleFormUndo form)
        form.redoButton.Click.Add (handleFormRedo form)
        form.redoToolStripMenuItem.Click.Add (handleFormRedo form)
        form.advancingButton.CheckedChanged.Add (handleFormAdvancingChanged form)
        form.songPlaybackButton.Click.Add (handleFormSongPlayback form)
        form.cutToolStripMenuItem.Click.Add (handleFormCut form)
        form.cutContextMenuItem.Click.Add (handleFormCut form)
        form.copyToolStripMenuItem.Click.Add (handleFormCopy form)
        form.copyContextMenuItem.Click.Add (handleFormCopy form)
        form.pasteToolStripMenuItem.Click.Add (handleFormPaste false form)
        form.pasteContextMenuItem.Click.Add (handleFormPaste true form)
        form.quickSizeToolStripButton.Click.Add (handleFormQuickSize form)
        form.resetCameraButton.Click.Add (handleFormResetCamera form)
        form.reloadAssetsButton.Click.Add (handleFormReloadAssets form)
        form.groupTabControl.Deselected.Add (handleFormGroupTabDeselected form)
        form.groupTabControl.Selected.Add (handleFormGroupTabSelected form)
        form.evalButton.Click.Add (handleEvalClick form)
        form.clearOutputButton.Click.Add (handleClearOutputClick form)
        form.createEntityComboBox.SelectedIndexChanged.Add (handleCreateEntityComboBoxSelectedIndexChanged form)
        form.entityDesignerPropertyAddButton.Click.Add (handleEntityDesignerPropertyAddClick false form)
        form.entityDesignerPropertyDefaultButton.Click.Add (handleEntityDesignerPropertyAddClick true form)
        form.entityDesignerPropertyRemoveButton.Click.Add (handleEntityDesignerPropertyRemoveClick form)
        form.entityIgnorePropertyBindingsCheckBox.CheckedChanged.Add (handleEntityIgnorePropertyBindingsChanged form)
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
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[v4i 0 0 0 0] : Vector4i") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[quat 0 0 0 1] : Quaternion") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[some 0] : int option") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[right 0] : Either<int, obj>") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[list 0] : int list") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[ring 0] : int Set") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[table [0 \"\"]] : Map<int, string>") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[tuple 0 0] : int * int") |> ignore
        form.entityDesignerPropertyTypeComboBox.Items.Add ("[record AssetTag [PackageName \"Default\"] [AssetName \"Image\"]] : AssetTag") |> ignore

#if !DEBUG
        // disable entity ignore property bindings in non-DEBUG mode.
        form.entityIgnorePropertyBindingsCheckBox.Enabled <- false
#endif

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
        match World.tryMake sdlDeps worldConfig plugin with
        | Right world ->
            let world =
                World.setEventFilter
                    (EventFilter.NotAny [EventFilter.Pattern (Rexpr "Update", []); EventFilter.Pattern (Rexpr "Mouse/Move", [])])
                    world
            let (screen, screenDispatcher) =
                if useGameplayScreen
                then plugin.EditorConfig
                else (Simulants.DefaultScreen, typeof<ScreenDispatcher>)
            Globals.Screen <- screen
            let (screen, world) =
                if not (screen.Exists world)
                then World.createScreen3 screenDispatcher.Name (Some screen.Name) world
                else (screen, world)
            let world =
                if Seq.isEmpty (World.getGroups screen world)
                then World.createGroup (Some "Group") screen world |> snd
                else world
            let world = World.selectScreen IdlingState screen world
            Right world
        | Left error -> Left error

    /// Attempt to make Gaia's SDL dependencies.
    let tryMakeSdlDeps (form : GaiaForm) =
        let sdlConfig =
            { ViewConfig = ExistingWindow { WfglWindow = form.displayPanel.Handle; WfglSwapBuffers = fun () -> form.displayPanel.Invalidate () }
              ViewW = Constants.Render.ResolutionX
              ViewH = Constants.Render.ResolutionY
              RendererFlags = Constants.Render.RendererFlagsDefault
              AudioChunkSize = Constants.Audio.BufferSizeDefault }
        match SdlDeps.tryMake sdlConfig with
        | Left msg -> Left msg
        | Right sdlDeps -> Right (sdlConfig, sdlDeps)

    /// Run Gaia from the F# evaluator.
    let runFromRepl runWhile targetDir sdlDeps form world =
        Globals.World <- world
        run3 runWhile targetDir sdlDeps form
        Globals.World

    /// Run Gaia in isolation.
    let run nuConfig =
        let (savedState, targetDir, plugin) = selectTargetDirAndMakeNuPlugin ()
        use form = createForm ()
        Globals.Form <- form
        match tryMakeSdlDeps form with
        | Right (sdlConfig, sdlDeps) ->
            let worldConfig =
                { Imperative = savedState.UseImperativeExecution
                  StandAlone = false
                  UpdateRate = 0L
                  NuConfig = nuConfig
                  SdlConfig = sdlConfig }
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
