// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.ComponentModel
open System.Drawing
open System.IO
open System.Linq
open System.Numerics
open System.Reflection
open System.Runtime.InteropServices
open System.Windows.Forms
open FSharp.Compiler.Interactive
open FSharp.Reflection
open Prime
open Nu
open Nu.Gaia
open Nu.Gaia.Design

[<RequireQualifiedAccess>]
module Gaia =

    // uses global variables for state because Gaia relies on Nu.Gaia.Globals to interoperate Nu and WinForms
    let mutable internal propertyPickButtonClickHandler = EventHandler (fun _ _ -> ())
    let mutable internal refreshHierarchyViewRequested = false // HACK: make sure hierarchy view isn't updated more than once per frame.
    let mutable internal rightClickPosition = v2Zero
    let mutable internal dragEntityState = DragEntityInactive
    let mutable internal dragEyeState = DragEyeInactive
    let mutable internal otherSnaps = (Constants.Editor.Position3dSnapDefault, Constants.Editor.Degrees3dSnapDefault, Constants.Editor.Scale3dSnapDefault)
    let mutable internal filePaths = Map.empty<Group Address, string>
    let mutable internal targetDir = "."
    let mutable internal selectedScreen = Screen "Screen" // TODO: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable internal selectedGroup = selectedScreen / "Group"

    let private getPickableEntities2d world =
        let (entities, world) = World.getEntitiesInView2d (HashSet ()) world
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible world) |> Seq.toArray
        (entitiesInGroup, world)

    let private getPickableEntities3d world =
        let (entities, world) = World.getEntitiesInView3d (HashSet ()) world
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible world) |> Seq.toArray
        (entitiesInGroup, world)

    let private getSnaps (form : GaiaForm) =
        let positionSnap = snd (Single.TryParse form.positionSnapTextBox.Text)
        let degreesSnap = snd (Single.TryParse form.degreesSnapTextBox.Text)
        let scaleSnap = snd (Single.TryParse form.scaleSnapTextBox.Text)
        (positionSnap, degreesSnap, scaleSnap)
    
    let private getCreationElevation (form : GaiaForm) =
        snd (Single.TryParse form.createElevationTextBox.Text)

    let rec private generateEntityName3 dispatcherName existingEntityNames world =
        let mutable name = Gen.nameForEditor dispatcherName
        if Set.contains name existingEntityNames
        then generateEntityName3 dispatcherName existingEntityNames world
        else name

    let private generateEntityName dispatcherName selectedGroup world =
        let existingEntityNames =
            World.getEntitiesFlattened selectedGroup world |>
            Seq.map (fun entity -> entity.Name) |>
            Set.ofSeq
        generateEntityName3 dispatcherName existingEntityNames world

    let private clearSelections (form : GaiaForm) =
        form.entityPropertyGrid.SelectedObject <- null
        form.propertyTabControl.SelectedIndex <- 3
        form.propertyEditor.Enabled <- false
        form.propertyNameLabel.Text <- String.Empty

    let private refreshSelections (form : GaiaForm) world =
        clearSelections form
        form.groupPropertyGrid.SelectedObject <- { DescribedGroup = Seq.head (World.getGroups selectedScreen world); Form = form }
        form.screenPropertyGrid.SelectedObject <- { DescribedScreen = selectedScreen; Form = form }
        form.gamePropertyGrid.SelectedObject <- { DescribedGame = Simulants.Game; Form = form }

    let private refreshEditModes (form : GaiaForm) world =
        let editModes = World.getEditModes world
        form.editModeComboBox.Items.Clear ()
        for (modeName, _) in editModes.Pairs do
            form.editModeComboBox.Items.Add modeName |> ignore
        if form.editModeComboBox.Items.Count <> 0 then
            form.editModeComboBox.Enabled <- true
        else form.editModeComboBox.Enabled <- false

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

    let private tryGetHierarchyTreeNode name (form : GaiaForm) (world : World) =
        let nodes = collectHierarchyTreeNodes form world
        Array.tryFind (fun (node : TreeNode) -> node.Name = name) nodes

    let private containsHierarchyTreeNode name (form : GaiaForm) (world : World) =
        Option.isSome (tryGetHierarchyTreeNode name form world)

    let private refreshHierarchyTreeView (_ : GaiaForm) (_ : World) =
        refreshHierarchyViewRequested <- true

    let private refreshHierarchyTreeViewImmediate (form : GaiaForm) world =
        // TODO: this code can cause severe performance issues. To unfuck performance, we will probably have to find a
        // way to update the hierarchy tree without a complete rebuild of it - IE, updating it in-place and
        // imperatively.
        Globals.World <- world // must be set for property grid
        let treeNodesState = form.hierarchyTreeView.GetExpandedNodesState ()
        form.hierarchyTreeView.Nodes.Clear ()
        let entities =
            World.getEntitiesFlattened selectedGroup world |>
            Seq.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity)) |>
            Array.ofSeq |>
            Array.sortBy fst |>
            Array.map snd
        for entity in entities do
            let mutable namesUsed = [||]
            let mutable parentNodeOpt = Option<TreeNode>.None
            for name in entity.Surnames do
                namesUsed <- Array.add name namesUsed
                let childNodeKey = namesUsed |> rtoa |> string
                match parentNodeOpt with
                | None ->
                    if not (form.hierarchyTreeView.Nodes.ContainsKey childNodeKey) then
                        let childNode = TreeNode name
                        childNode.Name <- childNodeKey
                        form.hierarchyTreeView.Nodes.Add childNode |> ignore
                        parentNodeOpt <- Some childNode
                    else parentNodeOpt <- Some form.hierarchyTreeView.Nodes.[childNodeKey]
                | Some parentNode ->
                    if not (parentNode.Nodes.ContainsKey childNodeKey) then
                        let childNode = TreeNode name
                        childNode.Name <- childNodeKey
                        parentNode.Nodes.Add childNode |> ignore
                        parentNodeOpt <- Some childNode
                    else parentNodeOpt <- Some parentNode.Nodes.[childNodeKey]
        form.hierarchyTreeView.RestoreExpandedNodesState treeNodesState
        refreshHierarchyViewRequested <- false
        world

    let private refreshGroupTabs (form : GaiaForm) world =

        // add groups imperatively to preserve existing group tabs
        let groups = World.getGroups selectedScreen world
        let groupTabPages = form.groupTabControl.TabPages
        for group in groups do
            let groupName = group.Name
            if not (groupTabPages.ContainsKey groupName) then
                groupTabPages.Add (groupName, groupName)
    
        // remove groups imperatively to preserve existing group tabs 
        for groupTabPage in groupTabPages do
            if Seq.notExists (fun (group : Group) -> group.Name = groupTabPage.Name) groups then
                groupTabPages.RemoveByKey groupTabPage.Name

    // TODO: factor out the duplicated node finding code.
    let private tryShowSelectedEntityInHierarchy (form : GaiaForm) =
        let pathOpt =
            match form.entityPropertyGrid.SelectedObject with
            | null -> None
            | :? EntityTypeDescriptorSource as entityTds -> entityTds.DescribedEntity.Surnames |> String.join Constants.Address.SeparatorStr |> Some
            | _ -> None
        match pathOpt with
        | Some path ->
            let nodeOpt = form.hierarchyTreeView.TryGetNodeFromPath path
            if notNull nodeOpt then
                form.hierarchyTreeView.SelectedNode <- nodeOpt
        | None -> ()

    let private tryShowSelectedEntityInHierarchyIfVisible (form : GaiaForm) =
        let pathOpt =
            match form.entityPropertyGrid.SelectedObject with
            | null -> None
            | :? EntityTypeDescriptorSource as entityTds -> entityTds.DescribedEntity.Surnames |> String.join Constants.Address.SeparatorStr |> Some
            | _ -> None
        match pathOpt with
        | Some path ->
            let nodeOpt = form.hierarchyTreeView.TryGetNodeFromPath path
            if notNull nodeOpt && nodeOpt.IsVisible then
                form.hierarchyTreeView.SelectedNode <- nodeOpt
            else form.hierarchyTreeView.SelectedNode <- null
        | None -> ()

    let private tryShowSelectedEntityInDisplay (form : GaiaForm) world =
        match form.entityPropertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            let entity = entityTds.DescribedEntity
            let world =
                if not (entity.GetAbsolute world) then
                    if entity.GetIs2d world
                    then World.setEyeCenter2d (entity.GetCenter world).V2 world
                    else
                        let eyeRotation = World.getEyeRotation3d world
                        let eyeCenterOffset = Vector3.Transform (Constants.Engine.EyeCenter3dOffset, eyeRotation)
                        World.setEyeCenter3d (entity.GetPosition world + eyeCenterOffset) world
                else world
            world
        | _ -> world

    let private selectEntity entity (form : GaiaForm) world =

        // select entity in property grid
        Globals.World <- world // must be set for property grid
        let entityTds = { DescribedEntity = entity; Form = form }
        let previousGridItem = form.entityPropertyGrid.SelectedGridItem
        form.entityPropertyGrid.SelectedObject <- entityTds

        // show previous grid item or model if available
        let gridItems = dictPlus StringComparer.Ordinal []
        let gridEntry = form.entityPropertyGrid.SelectedGridItem.Parent
        let gridEntry = if notNull gridEntry.Parent then gridEntry.Parent else gridEntry
        for gridItem in gridEntry.GridItems do
            for gridItem in gridItem.GridItems do
                gridItems.Add (gridItem.Label, gridItem)
        if notNull previousGridItem then
            match gridItems.TryGetValue previousGridItem.Label with
            | (true, gridItem) -> form.entityPropertyGrid.SelectedGridItem <- gridItem
            | (false, _) -> if entity.GetModelGeneric<obj> world <> box () then form.entityPropertyGrid.SelectedGridItem <- gridItems.[Constants.Engine.ModelPropertyName]
        elif entity.GetModelGeneric<obj> world <> box () then
            form.entityPropertyGrid.SelectedGridItem <- gridItems.[Constants.Engine.ModelPropertyName]

        // show entity property tab
        form.propertyTabControl.SelectTab 3

    let private deselectEntity (form : GaiaForm) world =
        Globals.World <- world // must be set for property grid
        form.entityPropertyGrid.SelectedObject <- null
        form.hierarchyTreeView.SelectedNode <- null

    let private refreshEntityPropertyGrid (form : GaiaForm) world =
        match form.entityPropertyGrid.SelectedObject with
        | :? EntityTypeDescriptorSource as entityTds ->
            Globals.World <- world // must be set for property grid
            if entityTds.DescribedEntity.Exists world
            then form.entityPropertyGrid.Refresh ()
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
            if groupTds.DescribedGroup.Exists world
            then form.groupPropertyGrid.Refresh ()
            else deselectGroup form world
        | _ -> ()

    let private refreshScreenPropertyGrid (form : GaiaForm) world =
        match form.screenPropertyGrid.SelectedObject with
        | :? ScreenTypeDescriptorSource as screenTds ->
            Globals.World <- world // must be set for property grid
            if screenTds.DescribedScreen.Exists world then
                form.screenPropertyGrid.Refresh ()
        | _ -> ()

    let private refreshGamePropertyGrid (form : GaiaForm) world =
        match form.gamePropertyGrid.SelectedObject with
        | :? GameTypeDescriptorSource ->
            Globals.World <- world // must be set for property grid
            form.screenPropertyGrid.Refresh ()
        | _ -> ()

    let private refreshFormOnUndoRedo (form : GaiaForm) world =
        refreshEntityPropertyGrid form world
        refreshGroupPropertyGrid form world
        refreshScreenPropertyGrid form world
        refreshGamePropertyGrid form world
        refreshGroupTabs form world
        refreshHierarchyTreeView form world
        form.runButton.Checked <- World.getAdvancing world

    let private canEditWithMouse (form : GaiaForm) world =
        World.getAdvancing world &&
        not form.liveEditCheckBox.Checked

    let private tryMousePick mousePosition (form : GaiaForm) world =
        let (entities2d, world) = getPickableEntities2d world
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d world
        match pickedOpt with
        | Some entity ->
            selectEntity entity form world
            tryShowSelectedEntityInHierarchyIfVisible form
            (Some (0.0f, entity), world)
        | None ->
            let (entities3d, world) = getPickableEntities3d world
            let pickedOpt = World.tryPickEntity3d mousePosition entities3d world
            match pickedOpt with
            | Some (intersection, entity) ->
                selectEntity entity form world
                tryShowSelectedEntityInHierarchyIfVisible form
                (Some (intersection, entity), world)
            | None -> (None, world)

    let private handleNuEntityLifeCycle (form : GaiaForm) (evt : Event<LifeCycleData, Game>) world =
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
                    dragEntityState <- DragEntityInactive
                    deselectEntity form world
                    (Cascade, world)
                else (Cascade, world)
            | _ -> failwithumf ()
        | MountOptChangeData _ ->
            refreshHierarchyTreeView form world
            (Cascade, world)

    let private handleNuGroupLifeCycle (form : GaiaForm) (_ : Event<LifeCycleData, Game>) world =
        Globals.World <- world // handle re-entry
        refreshGroupTabs form world
        refreshHierarchyTreeView form world
        (Cascade, world)

    let private handleNuSelectedScreenOptChange (form : GaiaForm) (evt : Event<ChangeData, Game>) world =
        Globals.World <- world // handle re-entry
        match evt.Data.Value :?> Screen option with
        | Some screen ->
            let groups = World.getGroups screen world
            let (group, world) =
                match Seq.tryHead groups with
                | None -> World.createGroup (Some "Group") screen world
                | Some group -> (group, world)
            selectedGroup <- group
            selectedScreen <- screen
            refreshGroupTabs form world
            refreshHierarchyTreeView form world
            refreshSelections form world
            (Cascade, world)
        | None ->
            // just keep current group selection and screen if no screen selected
            (Cascade, world)

    let private handleNuMouseRightDown (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let handled = if World.getAdvancing world then Cascade else Resolve
        let mousePosition = World.getMousePosition world
        let (_, world) = tryMousePick mousePosition form world
        rightClickPosition <- mousePosition
        (handled, world)

    let private handleNuEntityDragBegin (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        if not (canEditWithMouse form world) then
            let handled = if World.getAdvancing world then Cascade else Resolve
            let mousePosition = World.getMousePosition world
            match tryMousePick mousePosition form world with
            | (Some (_, entity), world) ->
                let world = Globals.pushPastWorld world
                if World.isKeyboardShiftDown world then
                    if entity.GetIs2d world then
                        let viewport = World.getViewport world
                        let eyeCenter = World.getEyeCenter2d world
                        let eyeSize = World.getEyeSize2d world
                        let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                        let entityDegrees = if entity.MountExists world then entity.GetDegreesLocal world else entity.GetDegrees world
                        dragEntityState <- DragEntityRotation2d (DateTimeOffset.Now, mousePositionWorld, entityDegrees.Z + mousePositionWorld.Y, entity)
                        (handled, world)
                    else
                        let viewport = World.getViewport world
                        let eyeCenter = World.getEyeCenter2d world
                        let eyeSize = World.getEyeSize2d world
                        let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                        let entityDegrees = if entity.MountExists world then entity.GetDegreesLocal world else entity.GetDegrees world
                        let (entityDegree, entityAxis) =
                            match (form.constrainXButton.Checked, form.constrainYButton.Checked, form.constrainZButton.Checked) with
                            | (true, false, false) -> (entityDegrees.X, v3Right)
                            | (false, true, false) -> (entityDegrees.Y, v3Up)
                            | (false, false, true) -> (entityDegrees.Z, v3Back)
                            | (_, _, _) -> (entityDegrees.Y, v3Up)
                        dragEntityState <- DragEntityRotation3d (DateTimeOffset.Now, mousePositionWorld, entityDegree + mousePositionWorld.Y, entityAxis, entity)
                        (handled, world)
                else
                    if entity.GetIs2d world then
                        let viewport = World.getViewport world
                        let eyeCenter = World.getEyeCenter2d world
                        let eyeSize = World.getEyeSize2d world
                        let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                        let entityPosition = entity.GetPosition world
                        dragEntityState <- DragEntityPosition2d (DateTimeOffset.Now, mousePositionWorld, entityPosition.V2 + mousePositionWorld, entity)
                        (handled, world)
                    else
                        let viewport = World.getViewport world
                        let eyeCenter = World.getEyeCenter3d world
                        let eyeRotation = World.getEyeRotation3d world
                        let mouseRayWorld = viewport.MouseToWorld3d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeRotation)
                        let entityPosition = entity.GetPosition world
                        let entityPlane = plane3 entityPosition (Vector3.Transform (v3Forward, World.getEyeRotation3d world))
                        let intersectionOpt = mouseRayWorld.Intersection entityPlane
                        if intersectionOpt.HasValue then
                            let entityDragOffset = intersectionOpt.Value - entityPosition
                            dragEntityState <- DragEntityPosition3d (DateTimeOffset.Now, entityDragOffset, entityPlane, entity)
                        (handled, world)
            | (None, world) -> (handled, world)
        else (Cascade, world)

    let private handleNuEntityDragEnd (form : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        if canEditWithMouse form world then (Cascade, world)
        else
            let handled = if World.getAdvancing world then Cascade else Resolve
            match dragEntityState with
            | DragEntityPosition2d _ | DragEntityRotation2d _ | DragEntityPosition3d _ | DragEntityRotation3d _ ->
                dragEntityState <- DragEntityInactive
                form.entityPropertyGrid.Refresh ()
                (handled, world)
            | DragEntityInactive -> (Resolve, world)

    let private handleNuEyeDragBegin (_ : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        let mousePositionScreen = World.getMousePosition2dScreen world
        let dragState = DragEyeCenter2d (World.getEyeCenter2d world + mousePositionScreen, mousePositionScreen)
        dragEyeState <- dragState
        (Resolve, world)

    let private handleNuEyeDragEnd (_ : GaiaForm) (_ : Event<MouseButtonData, Game>) world =
        match dragEyeState with
        | DragEyeCenter2d _ ->
            dragEyeState <- DragEyeInactive
            (Resolve, world)
        | DragEyeInactive -> (Resolve, world)

    let private handleNuUpdate (form : GaiaForm) (_ : Event<unit, Game>) world =
        if not form.runButton.Checked || form.liveEditCheckBox.Checked then
            let position = World.getEyeCenter3d world
            let rotation = World.getEyeRotation3d world
            let moveSpeed =
                if World.isKeyboardShiftDown world then 0.02f
                elif World.isKeyboardKeyDown KeyboardKey.Return world then 0.5f
                else 0.12f
            let turnSpeed =
                if World.isKeyboardShiftDown world then 0.025f
                else 0.05f
            let world =
                if World.isKeyboardKeyDown KeyboardKey.W world
                then World.setEyeCenter3d (position + Vector3.Transform (v3Forward, rotation) * moveSpeed) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.S world
                then World.setEyeCenter3d (position + Vector3.Transform (v3Back, rotation) * moveSpeed) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.A world
                then World.setEyeCenter3d (position + Vector3.Transform (v3Left, rotation) * moveSpeed) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.D world
                then World.setEyeCenter3d (position + Vector3.Transform (v3Right, rotation) * moveSpeed) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.Q world
                then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Right, turnSpeed)) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.E world
                then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Left, turnSpeed)) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.Up world
                then World.setEyeCenter3d (position + Vector3.Transform (v3Up, rotation) * moveSpeed) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.Down world
                then World.setEyeCenter3d (position + Vector3.Transform (v3Down, rotation) * moveSpeed) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.Left world
                then World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Up, turnSpeed) * rotation) world
                else world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.Right world
                then World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Down, turnSpeed) * rotation) world
                else world
            (Cascade, world)
        else (Cascade, world)

    let private handleNuRender (form : GaiaForm) (_ : Event<unit, Game>) world =

        // render lights of the selected group in play
        let (entities, world) = World.getLightsInPlay3d (HashSet ()) world
        let lightsInGroup =
            entities |>
            Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetLight world) |>
            Seq.toArray
        let world =
            Array.fold (fun world (light : Entity) ->
                World.enqueueRenderMessage3d
                    (RenderStaticModel
                        { Absolute = false
                          ModelMatrix = light.GetAffineMatrix world
                          Presence = Prominent
                          InsetOpt = None
                          MaterialProperties = MaterialProperties.defaultProperties
                          RenderType = ForwardRenderType (0.0f, Single.MinValue / 2.0f)
                          StaticModel = Assets.Default.LightbulbModel })
                    world)
                world lightsInGroup

        // render selection highlights
        let world =
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = entityTds.DescribedEntity
                let absolute = entity.GetAbsolute world
                let bounds = entity.GetHighlightBounds world
                if entity.GetIs2d world then
                    let elevation = Single.MaxValue
                    let transform = Transform.makePerimeter bounds v3Zero elevation absolute false
                    let image = Assets.Default.HighlightImage
                    World.enqueueRenderMessage2d
                        (LayeredOperation2d
                            { Elevation = elevation
                              Horizon = bounds.Bottom.Y
                              AssetTag = AssetTag.generalize image
                              RenderOperation2d =
                                RenderSprite
                                    { Transform = transform
                                      InsetOpt = ValueNone
                                      Image = image
                                      Color = Color.One
                                      Blend = Transparent
                                      Emission = Color.Zero
                                      Flip = FlipNone }})
                        world
                else
                    let mutable boundsMatrix = Matrix4x4.CreateScale (bounds.Size + v3Dup 0.01f) // slightly bigger to eye to prevent z-fighting with selected entity
                    boundsMatrix.Translation <- bounds.Center
                    World.enqueueRenderMessage3d
                        (RenderStaticModel
                            { Absolute = absolute
                              ModelMatrix = boundsMatrix
                              Presence = Prominent
                              InsetOpt = None
                              MaterialProperties = MaterialProperties.defaultProperties
                              RenderType = ForwardRenderType (0.0f, Single.MinValue)
                              StaticModel = Assets.Default.HighlightModel })
                        world
            | _ -> world

        // fin
        (Cascade, world)
    let private trySaveSelectedGroup filePath world =
        let oldWorld = world
        try World.writeGroupToFile filePath selectedGroup world
        with exn ->
            World.choose oldWorld |> ignore
            MessageBox.Show ("Could not save file due to: " + scstring exn, "File Save Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore

    let private tryLoadSelectedGroup (form : GaiaForm) filePath world =

        // old world in case we need to rewind
        let oldWorld = world

        try // try to destroy current group
            if not (selectedGroup.GetProtected world) then
                let world = World.destroyGroupImmediate selectedGroup world

                // load and add group, updating tab and selected group in the process
                let groupDescriptorStr = File.ReadAllText filePath
                let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                let groupName =
                    match groupDescriptor.GroupProperties.TryFind Constants.Engine.NamePropertyName with
                    | Some (Atom (name, _)) -> name
                    | _ -> failwithumf ()
                let group = selectedScreen / groupName
                if not (group.Exists world) then
                    let (group, world) = World.readGroup groupDescriptor None selectedScreen world
                    form.groupTabControl.SelectedTab.Text <- group.Name
                    form.groupTabControl.SelectedTab.Name <- group.Name
                    selectedGroup <- group

                    // refresh hierarchy view
                    refreshHierarchyTreeView form world
                    (Some group, world)
            
                // handle load failure
                else
                    let world = World.choose oldWorld
                    MessageBox.Show ("Could not load group file with same name as an existing group", "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    (None, world)

            else
                MessageBox.Show ("Cannot load into a protected simulant (such as a group created by the Elmish API).", "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                (None, world)

        // handle load failure
        with exn ->
            let world = World.choose oldWorld
            MessageBox.Show ("Could not load group file due to: " + scstring exn, "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
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
        let assets = Metadata.getDiscoveredAssets ()
        for package in assets do
            let node = assetPicker.assetTreeView.Nodes.Add package.Key
            for assetName in package.Value do
                node.Nodes.Add assetName |> ignore
        assetPicker.assetTreeView.DoubleClick.Add (fun _ -> assetPicker.DialogResult <- DialogResult.OK)
        assetPicker.okButton.Click.Add (fun _ -> assetPicker.DialogResult <- DialogResult.OK)
        assetPicker.cancelButton.Click.Add (fun _ -> assetPicker.Close ())
        assetPicker.searchTextBox.TextChanged.Add(fun _ ->
            assetPicker.assetTreeView.Nodes.Clear ()
            for package in assets do
                let node = assetPicker.assetTreeView.Nodes.Add package.Key
                for assetName in package.Value do
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
                    form.propertyValueTextBox.Text <- scstring assetTag
                    form.applyPropertyButton.PerformClick ()
                    world
        | _ -> world

    let private handlePropertyPickParentNode (propertyDescriptor : System.ComponentModel.PropertyDescriptor) (entityTds : EntityTypeDescriptorSource) (form : GaiaForm) world =
        use entityPicker = new EntityPicker ()
        let surnamesStrs =
            World.getEntitiesFlattened selectedGroup world |>
            Seq.filter (fun entity -> not (Gen.isNameGenerated entity.Name)) |>
            Seq.map (fun entity -> entity.Surnames |> rtoa |> string) |>
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
                        form.propertyValueTextBox.Text <- scstring parentRelation
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

    let private refreshPropertyEditor4 isEntity (propertyGrid : PropertyGrid) (form : GaiaForm) world =
        match (propertyGrid.SelectedObject, propertyGrid.SelectedGridItem) with
        | (null, _) | (_, null) ->
            form.propertyEditor.Enabled <- false
            form.propertyNameLabel.Text <- String.Empty
            form.propertyDescriptionTextBox.Text <- String.Empty
            form.propertyValueTextBox.Text <- String.Empty
        | (selectedObject, selectedGridItem) ->
            match selectedGridItem.GridItemType with
            | GridItemType.Property ->
                let ty = selectedGridItem.PropertyDescriptor.PropertyType
                let typeConverter = SymbolicConverter (false, None, ty)
                form.propertyEditor.Enabled <- not selectedGridItem.PropertyDescriptor.IsReadOnly
                form.propertyNameLabel.Text <- selectedGridItem.Label
                form.propertyDescriptionTextBox.Text <- selectedGridItem.PropertyDescriptor.Description
                if ty <> typeof<ComputedProperty> && (notNull selectedGridItem.Value || FSharpType.isNullTrueValue ty) then
                    let (_, _, prettyPrinter) =
                        match (isEntity, selectedGridItem.Label) with
                        | (true, Constants.Engine.OverlayNameOptPropertyName) ->
                            let overlays = World.getOverlays world
                            let overlayNames = overlays |> Map.toValueList |> List.map (fun overlay -> overlay.OverlayName)
                            (String.concat " " overlayNames, "", PrettyPrinter.defaultPrinter)
                        | (true, Constants.Engine.FacetNamesPropertyName) ->
                            let facetNames = world |> World.getFacets |> Map.toKeyList
                            (String.concat " " facetNames, "", PrettyPrinter.defaultPrinter)
                        | (_, _) ->
                            let syntax = SyntaxAttribute.defaultValue ty
                            let keywords0 =
                                if ty = typeof<Scripting.Expr>
                                then syntax.Keywords0 + " " + WorldBindings.BindingKeywords
                                else syntax.Keywords0
                            (keywords0, syntax.Keywords1, syntax.PrettyPrinter)
                    let selectionStart = form.propertyValueTextBox.SelectionStart
                    let strUnescaped = typeConverter.ConvertToString selectedGridItem.Value
                    let strEscaped = String.escape strUnescaped
                    let strPretty = PrettyPrinter.prettyPrint strEscaped prettyPrinter
                    form.propertyValueTextBox.Text <- strPretty.Replace ("\n", "\r\n")
                    form.propertyValueTextBox.SelectionStart <- selectionStart
                    form.pickPropertyButton.Visible <-
                        selectedGridItem.PropertyDescriptor.PropertyType = typeof<Entity Relation option> ||
                        (selectedGridItem.PropertyDescriptor.PropertyType.IsGenericType &&
                         selectedGridItem.PropertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>)
                    form.pickPropertyButton.Click.RemoveHandler propertyPickButtonClickHandler
                    propertyPickButtonClickHandler <- EventHandler (fun _ _ ->
                        if isEntity then
                            let selectedEntityTds = selectedObject :?> EntityTypeDescriptorSource
                            let handler = handlePropertyPickButton selectedGridItem.PropertyDescriptor selectedEntityTds form
                            Globals.nextPreUpdate handler)
                    form.pickPropertyButton.Click.AddHandler propertyPickButtonClickHandler
            | _ ->
                form.propertyEditor.Enabled <- false
                form.propertyNameLabel.Text <- String.Empty
                form.propertyDescriptionTextBox.Text <- String.Empty
                form.propertyValueTextBox.Text <- String.Empty
                form.pickPropertyButton.Visible <- false
                form.pickPropertyButton.Click.RemoveHandler propertyPickButtonClickHandler

    let private applyPropertyEditor2 (propertyGrid : PropertyGrid) (form : GaiaForm) =
        match (propertyGrid.SelectedObject, propertyGrid.SelectedGridItem) with
        | (null, _) -> ()
        | (_, null) -> failwithumf ()
        | (selectedObject, selectedGridItem) ->
            match selectedGridItem.GridItemType with
            | GridItemType.Property when form.propertyNameLabel.Text = selectedGridItem.Label ->
                let propertyDescriptor = selectedGridItem.PropertyDescriptor
                let typeConverter = SymbolicConverter (false, None, propertyDescriptor.PropertyType)
                try let strEscaped = form.propertyValueTextBox.Text.TrimEnd ()
                    let strUnescaped = String.unescape strEscaped
                    let propertyValue = typeConverter.ConvertFromString strUnescaped
                    propertyDescriptor.SetValue (selectedObject, propertyValue)
                with
                | :? ConversionException as exn ->
                    match exn.SymbolOpt with
                    | Some symbol ->
                        match Symbol.getOriginOpt symbol with
                        | ValueSome origin ->
                            form.propertyValueTextBox.SelectionStart <- int origin.Start.Index
                            form.propertyValueTextBox.Focus () |> ignore<bool>
                        | ValueNone -> ()
                    | None -> ()
                    Log.info ("Invalid apply property operation due to: " + scstring exn)
                | exn -> Log.info ("Invalid apply property operation due to: " + scstring exn)
            | _ -> Log.trace "Invalid apply property operation (likely a code issue in Gaia)."

    let private refreshPropertyEditor (form : GaiaForm) =
        let world = Globals.World // handle re-entry
        match form.propertyTabControl.SelectedIndex with
        | 0 -> refreshPropertyEditor4 false form.gamePropertyGrid form world
        | 1 -> refreshPropertyEditor4 false form.screenPropertyGrid form world
        | 2 -> refreshPropertyEditor4 false form.groupPropertyGrid form world
        | 3 -> refreshPropertyEditor4 true form.entityPropertyGrid form world
        | _ -> failwithumf ()

    let private applyPropertyEditor (form : GaiaForm) =
        match form.propertyTabControl.SelectedIndex with
        | 0 -> applyPropertyEditor2 form.gamePropertyGrid form
        | 1 -> applyPropertyEditor2 form.screenPropertyGrid form
        | 2 -> applyPropertyEditor2 form.groupPropertyGrid form
        | 3 -> applyPropertyEditor2 form.entityPropertyGrid form
        | _ -> failwithumf ()

    let private populatePreludeTextBox (form : GaiaForm) =
        match World.tryReadPrelude () with
        | Right (preludeStr, _) ->
            form.preludeTextBox.Text <- preludeStr.Replace ("\n", "\r\n")
        | Left error ->
            MessageBox.Show ("Could not read prelude due to: " + error + "'.", "Failed to Read Prelude", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore

    let private tryReloadPrelude (_ : GaiaForm) world =
        let assetSourceDir = targetDir + "/../../.."
        World.tryReloadPrelude assetSourceDir targetDir world

    let private trySavePrelude (form : GaiaForm) world =
        let oldWorld = world
        let preludeSourceDir = targetDir + "/../../.."
        let preludeFilePath = preludeSourceDir + "/" + Assets.Global.PreludeFilePath
        try let preludeStr = form.preludeTextBox.Text.TrimEnd ()
            File.WriteAllText (preludeFilePath, preludeStr)
            (true, world)
        with exn ->
            MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to Save Asset Graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            (false, World.choose oldWorld)

    let private populateAssetGraphTextBox (form : GaiaForm) =
        match AssetGraph.tryMakeFromFile (targetDir + "/" + Assets.Global.AssetGraphFilePath) with
        | Right assetGraph ->
            let selectionStart = form.assetGraphTextBox.SelectionStart
            let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            let packageDescriptorsPretty = PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
            form.assetGraphTextBox.Text <- packageDescriptorsPretty.Replace ("\n", "\r\n")
            form.assetGraphTextBox.SelectionStart <- selectionStart
        | Left error ->
            MessageBox.Show ("Could not read asset graph due to: " + error + "'.", "Failed to Read Asset Graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore

    let private tryReloadAssetGraph (_ : GaiaForm) world =
        let assetSourceDir = targetDir + "/../../.."
        World.tryReloadAssetGraph assetSourceDir targetDir Constants.Engine.RefinementDir world

    let private tryLoadAssetGraph (form : GaiaForm) world =
        match tryReloadAssetGraph form world with
        | (Right assetGraph, world) ->
            let selectionStart = form.assetGraphTextBox.SelectionStart
            let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            let packageDescriptorsPretty = PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
            form.assetGraphTextBox.Text <- packageDescriptorsPretty.Replace ("\n", "\r\n")
            form.assetGraphTextBox.SelectionStart <- selectionStart
            world
        | (Left error, world) ->
            MessageBox.Show ("Could not load asset graph due to: " + error + "'.", "Failed to Load Asset Graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            world

    let private trySaveAssetGraph (form : GaiaForm) world =
        let oldWorld = world
        let assetSourceDir = targetDir + "/../../.."
        let assetGraphFilePath = assetSourceDir + "/" + Assets.Global.AssetGraphFilePath
        try let packageDescriptorsStr = form.assetGraphTextBox.Text.TrimEnd () |> scvalue<Map<string, PackageDescriptor>> |> scstring
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            File.WriteAllText (assetGraphFilePath, PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter)
            (true, world)
        with exn ->
            MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to Save Asset Graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            (false, World.choose oldWorld)

    let private populateOverlayerTextBox (form : GaiaForm) =
        let overlayerFilePath = targetDir + "/" + Assets.Global.OverlayerFilePath
        match Overlayer.tryMakeFromFile [] overlayerFilePath with
        | Right overlayer ->
            let selectionStart = form.overlayerTextBox.SelectionStart
            let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
            let extrinsicOverlaysPretty = PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
            form.overlayerTextBox.Text <- extrinsicOverlaysPretty.Replace ("\n", "\r\n")
            form.overlayerTextBox.SelectionStart <- selectionStart
        | Left error ->
            MessageBox.Show ("Could not read overlayer due to: " + error + "'.", "Failed to Read Overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore

    let private tryReloadOverlayer form world =
        let overlayerDir = targetDir + "/../../.."
        match World.tryReloadOverlayer overlayerDir targetDir world with
        | (Right overlayer, world) ->
            refreshOverlayComboBox form world
            (Right overlayer, world)
        | (Left error, world) -> (Left error, world)

    let private tryLoadOverlayer (form : GaiaForm) world =
        match tryReloadOverlayer form world with
        | (Right overlayer, world) ->
            let selectionStart = form.overlayerTextBox.SelectionStart
            let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
            let extrinsicOverlaysPretty = PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
            form.overlayerTextBox.Text <- extrinsicOverlaysPretty.Replace ("\n", "\r\n")
            form.overlayerTextBox.SelectionStart <- selectionStart
            world
        | (Left error, world) ->
            MessageBox.Show ("Could not reload overlayer due to: " + error + "'.", "Failed to Reload Overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            world

    let private trySaveOverlayer (form : GaiaForm) world =
        let oldWorld = world
        let overlayerSourceDir = targetDir + "/../../.."
        let overlayerFilePath = overlayerSourceDir + "/" + Assets.Global.OverlayerFilePath
        try let overlays = scvalue<Overlay list> (form.overlayerTextBox.Text.TrimEnd ())
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
            File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
            (true, world)
        with exn ->
            MessageBox.Show ("Could not save overlayer due to: " + scstring exn, "Failed to Save Overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            (false, oldWorld)

    let private populateEventFilterTextBox (form : GaiaForm) =
        let eventFilterStr = scstring Constants.Editor.EventFilter
        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<EventFilter>).PrettyPrinter
        let eventFilterPretty = PrettyPrinter.prettyPrint eventFilterStr prettyPrinter
        form.eventFilterTextBox.Text <- eventFilterPretty.Replace ("\n", "\r\n")

    let private handleFormEntityPropertyGridSelectedObjectsChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormEntityPropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormGroupPropertyGridSelectedObjectsChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormGroupPropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormScreenPropertyGridSelectedObjectsChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormScreenPropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormGamePropertyGridSelectedObjectsChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormGamePropertyGridSelectedGridItemChanged (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handlePropertyTabControlSelectedIndexChanged (form : GaiaForm) (_ : EventArgs) =
        let world = Globals.World // handle re-entry
        match form.propertyTabControl.SelectedIndex with
        | 0 -> refreshGamePropertyGrid form world
        | 1 -> refreshScreenPropertyGrid form world
        | 2 -> refreshGroupPropertyGrid form world
        | 3 -> refreshEntityPropertyGrid form world
        | _ -> failwithumf ()

    let private handleFormPropertyRefreshClick (form : GaiaForm) (_ : EventArgs) =
        refreshPropertyEditor form

    let private handleFormPropertyApplyClick (form : GaiaForm) (_ : EventArgs) =
        applyPropertyEditor form

    let private handleFormHierarchyTreeViewItemDrag (form : GaiaForm) (args : ItemDragEventArgs) =
        if args.Button = MouseButtons.Left then
            form.DoDragDrop (args.Item, DragDropEffects.Move) |> ignore

    let private handleFormHierarchyTreeViewDragEnter (_ : GaiaForm) (args : DragEventArgs) =
        args.Effect <- args.AllowedEffect

    let private handleFormHierarchyTreeViewDragDrop (form : GaiaForm) (args : DragEventArgs) =

        // TODO: lift this out.
        let rec containsNode (source : TreeNode) (target : TreeNode) =
            if isNull target.Parent then false
            elif target.Parent = source then true
            else containsNode source target.Parent

        Globals.nextPreUpdate $ fun world ->
            let world = Globals.pushPastWorld world
            let targetPoint = form.hierarchyTreeView.PointToClient (Point (args.X, args.Y))
            let targetNodeOpt = form.hierarchyTreeView.GetNodeAt targetPoint
            let draggedNode = args.Data.GetData typeof<TreeNode> :?> TreeNode
            let source = Entity (selectedGroup.GroupAddress <-- Address.makeFromString draggedNode.Name)
            if not (source.GetProtected world) then
                if isNull targetNodeOpt then
                    let source' = Entity (selectedGroup.GroupAddress <-- Address.makeFromString source.Name)
                    let world = source.SetMountOptWithAdjustment None world
                    let world = World.renameEntityImmediate source source' world
                    let world = refreshHierarchyTreeViewImmediate form world
                    selectEntity source' form world
                    tryShowSelectedEntityInHierarchy form
                    world
                elif draggedNode <> targetNodeOpt && not (containsNode draggedNode targetNodeOpt) then
                    if args.KeyState &&& 32 = 0 then // alt not pressed
                        let source' = Entity (selectedGroup.GroupAddress <-- Address.makeFromString targetNodeOpt.Name) / source.Name
                        let mount = Relation.makeParent ()
                        let world = World.renameEntityImmediate source source' world
                        let world = source'.SetMountOptWithAdjustment (Some mount) world
                        let world = refreshHierarchyTreeViewImmediate form world
                        selectEntity source' form world
                        tryShowSelectedEntityInHierarchy form
                        world
                    else // alt pressed
                        let next = Entity (selectedGroup.GroupAddress <-- Address.makeFromString targetNodeOpt.Name)
                        let previousOpt = World.tryGetPreviousEntity next world
                        let parentOpt = match next.Parent with :? Entity as parent -> Some parent | _ -> None
                        let mountOpt = match parentOpt with Some _ -> Some (Relation.makeParent ()) | None -> None
                        let source' = match parentOpt with Some parent -> parent / source.Name | None -> selectedGroup / source.Name
                        let world = World.insertEntityOrder source previousOpt next world
                        let world = World.renameEntityImmediate source source' world
                        let world = source'.SetMountOptWithAdjustment mountOpt world
                        let world = refreshHierarchyTreeViewImmediate form world
                        selectEntity source' form world
                        tryShowSelectedEntityInHierarchy form
                        world
                else world
            else
                MessageBox.Show ("Cannot relocate a protected simulant (such as an entity created by the Elmish API).", "Protected Elmish Simulant", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleFormHierarchyTreeViewCollapseClick (form : GaiaForm) (_ : EventArgs) =
        form.hierarchyTreeView.CollapseAll ()

    let private handleFormHierarchyTreeViewNodeSelect (form : GaiaForm) (evt : TreeViewEventArgs) =
        Globals.nextPreUpdate $ fun world ->
            if notNull form.hierarchyTreeView.SelectedNode then
                let nodeKey = form.hierarchyTreeView.SelectedNode.Name
                let address = Address.makeFromString nodeKey
                let entity = Entity (selectedGroup.GroupAddress <-- atoa address)
                if  entity.Exists world &&
                    evt.Action <> TreeViewAction.Unknown then
                    selectEntity entity form world
                world
            else world

    let private handleFormHierarchyTreeViewClick (form : GaiaForm) (evt : TreeNodeMouseClickEventArgs) =
        if evt.Button = MouseButtons.Right then
            form.hierarchyTreeView.SelectedNode <- evt.Node

    let private handleFormHierarchyTreeViewDoubleClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            tryShowSelectedEntityInDisplay form world

    let private handleFormCreateEntity atMouse inHierarchy (dispatcherNameOpt : string option) (form : GaiaForm) (_ : EventArgs) =
        form.displayPanel.Focus () |> ignore<bool>
        Globals.nextPreUpdate $ fun world ->
            let oldWorld = world
            try let world = Globals.pushPastWorld world
                let dispatcherName =
                    match dispatcherNameOpt with
                    | Some dispatcherName -> dispatcherName
                    | None -> form.createEntityComboBox.Text
                let overlayDescriptor =
                    match form.overlayComboBox.Text with
                    | "(Default Overlay)" -> DefaultOverlay
                    | "(Routed Overlay)" -> RoutedOverlay
                    | "(No Overlay)" -> NoOverlay
                    | overlayName -> ExplicitOverlay overlayName
                let name = generateEntityName dispatcherName selectedGroup world
                let surnames =
                    match form.entityPropertyGrid.SelectedObject with
                    | null -> [|name|]
                    | :? GroupTypeDescriptorSource -> [|name|]
                    | :? EntityTypeDescriptorSource as entityTds when inHierarchy ->
                        let parent = entityTds.DescribedEntity
                        Array.add name parent.Surnames
                    | _ -> [|name|]
                let (entity, world) = World.createEntity5 dispatcherName overlayDescriptor (Some surnames) selectedGroup world
                let (positionSnap, degreesSnap, scaleSnap) = getSnaps form
                let viewport = World.getViewport world
                let mousePosition = World.getMousePosition world
                let mutable entityTransform = entity.GetTransform world
                let world =
                    if entity.GetIs2d world then
                        let eyeCenter = World.getEyeCenter2d world
                        let eyeSize = World.getEyeSize2d world
                        let entityPosition =
                            if atMouse
                            then viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                            else viewport.MouseToWorld2d (entity.GetAbsolute world, World.getEyeSize2d world * 0.5f, eyeCenter, eyeSize)
                        entityTransform.Position <- entityPosition.V3
                        entityTransform.Size <- entity.GetQuickSize world
                        entityTransform.Elevation <- getCreationElevation form
                        if not form.snap3dButton.Checked
                        then entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform world
                        else entity.SetTransform entityTransform world
                    else
                        let eyeCenter = World.getEyeCenter3d world
                        let eyeRotation = World.getEyeRotation3d world
                        let entityPosition =
                            if atMouse then
                                let ray = viewport.MouseToWorld3d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeRotation)
                                let forward = Vector3.Transform (v3Forward, eyeRotation)
                                let plane = plane3 (eyeCenter + forward * Constants.Engine.EyeCenter3dOffset.Z) -forward
                                (ray.Intersection plane).Value
                            else eyeCenter + Vector3.Transform (v3Forward, eyeRotation) * Constants.Engine.EyeCenter3dOffset.Z
                        entityTransform.Position <- entityPosition
                        entityTransform.Size <- entity.GetQuickSize world
                        if form.snap3dButton.Checked
                        then entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform world
                        else entity.SetTransform entityTransform world
                let world =
                    if inHierarchy
                    then entity.SetMountOptWithAdjustment (Some (Relation.makeParent ())) world
                    else world
                let world =
                    match entity.TryGetProperty (nameof entity.ProbeBounds) world with
                    | Some property when property.PropertyType = typeof<Box3> ->
                        let bounds =
                            box3
                                (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f + entity.GetPosition world)
                                (v3Dup Constants.Render.LightProbeSizeDefault)
                        entity.SetProbeBounds bounds world
                    | Some _ | None -> world
                let world = refreshHierarchyTreeViewImmediate form world
                selectEntity entity form world
                tryShowSelectedEntityInHierarchy form
                world
            with exn ->
                let world = World.choose oldWorld
                MessageBox.Show (scstring exn, "Could Not Create Entity", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleFormDeleteEntity (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let world = Globals.pushPastWorld world
            match form.entityPropertyGrid.SelectedObject with
            | :? EntityTypeDescriptorSource as entityTds ->
                if not (entityTds.DescribedEntity.GetProtected world) then
                    let world = World.destroyEntity entityTds.DescribedEntity world
                    deselectEntity form world
                    world
                else
                    MessageBox.Show ("Cannot destroy a protected simulant (such as an entity created by the Elmish API).", "Protected Elmish Simulant", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            | _ -> world

    let private handleFormSelectInHierarchy (form : GaiaForm) (_ : EventArgs) =
        tryShowSelectedEntityInHierarchy form

    let private handleFormNew (form : GaiaForm) (_ : EventArgs) =
        use groupCreationForm = new GroupCreationForm ()
        groupCreationForm.StartPosition <- FormStartPosition.CenterParent
        groupCreationForm.dispatcherTextBox.Text <- typeof<GroupDispatcher>.Name
        groupCreationForm.okButton.Click.Add $ fun _ ->
            Globals.nextPreUpdate $ fun world ->
                let oldWorld = world
                let world = Globals.pushPastWorld world
                let groupName = groupCreationForm.nameTextBox.Text
                let groupDispatcherName = groupCreationForm.dispatcherTextBox.Text
                try if String.length groupName = 0 then failwith "Group name cannot be empty in Gaia due to WinForms limitations."
                    let world = World.createGroup4 groupDispatcherName (Some groupName) selectedScreen world |> snd
                    refreshGroupTabs form world
                    refreshHierarchyTreeView form world
                    deselectEntity form world
                    form.groupTabControl.SelectTab (form.groupTabControl.TabPages.IndexOfKey groupName)
                    world
                with exn ->
                    let world = World.choose oldWorld
                    MessageBox.Show ("Could not create group due to: " + scstring exn, "Group Creation Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            groupCreationForm.Close ()
        groupCreationForm.cancelButton.Click.Add (fun _ -> groupCreationForm.Close ())
        groupCreationForm.ShowDialog form |> ignore

    let private handleFormSave saveAs (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let group = selectedGroup
            form.saveFileDialog.Title <- "Save '" + group.Name + "' As"
            match Map.tryFind group.GroupAddress filePaths with
            | Some filePath -> form.saveFileDialog.FileName <- filePath
            | None -> form.saveFileDialog.FileName <- String.Empty
            if saveAs || String.IsNullOrWhiteSpace form.saveFileDialog.FileName then
                match form.saveFileDialog.ShowDialog form with
                | DialogResult.OK ->
                    let filePath = form.saveFileDialog.FileName
                    trySaveSelectedGroup filePath world
                    filePaths <- Map.add group.GroupAddress filePath filePaths
                    world
                | _ -> world
            else trySaveSelectedGroup form.saveFileDialog.FileName world; world

    let private handleFormOpen (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            form.openFileDialog.FileName <- String.Empty
            match form.openFileDialog.ShowDialog form with
            | DialogResult.OK ->
                let world = Globals.pushPastWorld world
                let filePath = form.openFileDialog.FileName
                match tryLoadSelectedGroup form filePath world with
                | (Some group, world) ->
                    filePaths <- Map.add group.GroupAddress filePath filePaths
                    deselectEntity form world // currently selected entity may be gone if loading into an existing group
                    world
                | (None, world) -> world
            | _ -> world

    let private handleFormClose (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            match form.groupTabControl.TabPages.Count with
            | 1 ->
                MessageBox.Show ("Cannot close the only remaining group.", "Group Close Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world
            | _ ->
                let world = Globals.pushPastWorld world
                let group = selectedGroup
                if not (group.GetProtected world) then
                    let world = World.destroyGroupImmediate group world
                    deselectEntity form world
                    form.groupTabControl.TabPages.RemoveByKey group.Name
                    let groupTabControl = form.groupTabControl
                    let groupTab = groupTabControl.SelectedTab
                    selectedGroup <- selectedScreen / groupTab.Text
                    filePaths <- Map.remove group.GroupAddress filePaths
                    world
                else
                    MessageBox.Show ("Cannot close a protected group (such as one created by the Elmish API).", "Protected Elmish Simulant", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world

    let private handleFormUndo (form : GaiaForm) (_ : EventArgs) =
        Globals.nextUpdate $ fun world ->
            match Globals.tryUndo world with
            | (true, world) -> refreshFormOnUndoRedo form world; world
            | (false, world) -> world

    let private handleFormRedo (form : GaiaForm) (_ : EventArgs) =
        Globals.nextUpdate $ fun world ->
            match Globals.tryRedo world with
            | (true, world) -> refreshFormOnUndoRedo form world; world
            | (false, world) -> world

    let private handleFormRunButtonClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let advancing = form.runButton.Checked
            let world =
                if advancing then
                    form.displayPanel.Focus () |> ignore
                    Globals.pushPastWorld world
                else world
            World.setAdvancing advancing world

    let private handleFormSongPlayback (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            if form.songPlaybackButton.Checked
            then World.setMasterSongVolume 1.0f world
            else World.setMasterSongVolume 0.0f world

    let private handleFormCopy (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds -> World.copyEntityToClipboard entityTds.DescribedEntity world; world
            | _ -> failwithumf ()

    let private handleFormCut (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let world = Globals.pushPastWorld world
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                if not (entityTds.DescribedEntity.GetProtected world) then
                    let world = Globals.pushPastWorld world
                    let world = World.cutEntityToClipboard entityTds.DescribedEntity world
                    deselectEntity form world
                    world
                else
                    MessageBox.Show ("Cannot cut a protected simulant (such as an entity created by the Elmish API).", "Protected Elmish Simulant", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            | _ -> failwithumf ()

    let private handleFormPaste atMouse (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let world = Globals.pushPastWorld world
            let surnamesOpt =
                World.tryGetEntityDispatcherNameOnClipboard world |>
                Option.map (fun dispatcherName -> generateEntityName dispatcherName selectedGroup world) |>
                Option.map Array.singleton
            let snapsEir = getSnaps form |> if form.snap3dButton.Checked then Right else Left
            let (entityOpt, world) = World.pasteEntityFromClipboard atMouse rightClickPosition snapsEir surnamesOpt selectedGroup world
            match entityOpt with
            | Some entity ->
                selectEntity entity form world
                tryShowSelectedEntityInHierarchy form
                world
            | None -> world

    let private handleFormQuickSize (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let world = Globals.pushPastWorld world
            match form.entityPropertyGrid.SelectedObject with
            | null -> world
            | :? EntityTypeDescriptorSource as entityTds ->
                let entity = entityTds.DescribedEntity
                let world = Globals.pushPastWorld world
                let world = entity.SetSize (entity.GetQuickSize world) world
                Globals.World <- world // must be set for property grid
                form.entityPropertyGrid.Refresh ()
                world
            | _ -> failwithumf ()

    let private handleFormSnap3d (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let (positionSnap, degreesSnap, scaleSnap) = otherSnaps
            let otherSnaps' =
                (snd (Single.TryParse form.positionSnapTextBox.Text),
                 snd (Single.TryParse form.degreesSnapTextBox.Text),
                 snd (Single.TryParse form.scaleSnapTextBox.Text))
            otherSnaps <- otherSnaps'
            form.positionSnapTextBox.Text <- scstring positionSnap
            form.degreesSnapTextBox.Text <- scstring degreesSnap
            form.scaleSnapTextBox.Text <- scstring scaleSnap
            world

    let private handleFormResetEye (_ : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let world = Globals.pushPastWorld world
            let world = World.setEyeCenter2d v2Zero world
            let world = World.setEyeCenter3d Constants.Engine.EyeCenter3dDefault world
            let world = World.setEyeRotation3d quatIdentity world
            world

    let private handleFormSelectEditMode (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let editModes = World.getEditModes world
            match editModes.TryGetValue (form.editModeComboBox.SelectedItem :?> string) with
            | (true, callback) ->
                form.displayPanel.Focus () |> ignore
                let world = Globals.pushPastWorld world
                callback world
            | (false, _) -> world

    let private handleFormReloadAssets (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            match tryReloadAssetGraph form world with
            | (Right assetGraph, world) ->
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                let assetGraphPretty = PrettyPrinter.prettyPrint (scstring assetGraph) prettyPrinter
                form.assetGraphTextBox.Text <- assetGraphPretty.Replace ("\n", "\r\n")
                world
            | (Left error, world) ->
                MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset Reload Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleFormReloadCode form (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let world = Globals.pushPastWorld world
            clearSelections form // keep old type information from sticking around in re-painting property editors
            let workingDirPath = targetDir + "/../../.."
            Log.info ("Inspecting directory " + workingDirPath + " for F# code...")
            try match Array.ofSeq (Directory.EnumerateFiles (workingDirPath, "*.fsproj")) with
                | [||] -> Log.trace ("Unable to find fsproj file in '" + workingDirPath + "'."); world
                | fsprojFilePaths ->
                    let buildName =
#if DEBUG
                        "Debug"
#else
                        "Release"
#endif
                    let fsprojFilePath = fsprojFilePaths.[0]
                    Log.info ("Inspecting code for F# project '" + fsprojFilePath + "'...")
                    let fsprojFileLines = File.ReadAllLines fsprojFilePath
                    let fsprojNugetPaths = // imagine manually parsing an xml file...
                        fsprojFileLines |>
                        Array.map (fun line -> line.Trim ()) |>
                        Array.filter (fun line -> line.Contains "PackageReference") |>
                        Array.map (fun line -> line.Replace ("<PackageReference Include=", "nuget: ")) |>
                        Array.map (fun line -> line.Replace (" Version=", ", ")) |>
                        Array.map (fun line -> line.Replace ("/>", "")) |>
                        Array.map (fun line -> line.Replace ("\"", "")) |>
                        Array.map (fun line -> line.Trim ())
                    let fsprojDllFilePaths =
                        fsprojFileLines |>
                        Array.map (fun line -> line.Trim ()) |>
                        Array.filter (fun line -> line.Contains "HintPath" && line.Contains ".dll") |>
                        Array.map (fun line -> line.Replace ("<HintPath>", "")) |>
                        Array.map (fun line -> line.Replace ("</HintPath>", "")) |>
                        Array.map (fun line -> line.Replace ("=", "")) |>
                        Array.map (fun line -> line.Replace ("\"", "")) |>
                        Array.map (fun line -> line.Replace ("\\", "/")) |>
                        Array.map (fun line -> line.Trim ())
                    let fsprojProjectLines = // TODO: see if we can pull these from the fsproj as well...
                        ["#r \"../../../../../Nu/Nu.Math/bin/" + buildName + "/netstandard2.0/Nu.Math.dll\""
                         "#r \"../../../../../Nu/Nu.Pipe/bin/" + buildName + "/net7.0/Nu.Pipe.dll\""
                         "#r \"../../../../../Nu/Nu/bin/" + buildName + "/net7.0/Nu.dll\""]
                    let fsprojFsFilePaths =
                        fsprojFileLines |>
                        Array.map (fun line -> line.Trim ()) |>
                        Array.filter (fun line -> line.Contains "Compile Include" && line.Contains ".fs") |>
                        Array.filter (fun line -> line.Contains "Compile Include" && not (line.Contains "Program.fs")) |>
                        Array.map (fun line -> line.Replace ("<Compile Include", "")) |>
                        Array.map (fun line -> line.Replace ("/>", "")) |>
                        Array.map (fun line -> line.Replace ("=", "")) |>
                        Array.map (fun line -> line.Replace ("\"", "")) |>
                        Array.map (fun line -> line.Replace ("\\", "/")) |>
                        Array.map (fun line -> line.Trim ())
                    let fsxFileString =
                        String.Join ("\n", Array.map (fun (nugetPath : string) -> "#r \"" + nugetPath + "\"") fsprojNugetPaths) + "\n" +
                        String.Join ("\n", Array.map (fun (filePath : string) -> "#r \"../../../" + filePath + "\"") fsprojDllFilePaths) + "\n" +
                        String.Join ("\n", fsprojProjectLines) + "\n" +
                        String.Join ("\n", Array.map (fun (filePath : string) -> "#load \"../../../" + filePath + "\"") fsprojFsFilePaths)
                    let fsProjectNoWarn = "--nowarn:FS9;FS1178;FS3391;FS3536" // TODO: P1: pull these from fsproj!
                    Log.info ("Compiling code via generated F# script:\n" + fsxFileString)
                    let defaultArgs = [|"fsi.exe"; "--debug+"; "--debug:full"; "--optimize-"; "--tailcalls-"; "--multiemit+"; "--gui-"; fsProjectNoWarn|]
                    use errorStream = new StringWriter ()
                    use inStream = new StringReader ""
                    use outStream = new StringWriter ()
                    let fsiConfig = Shell.FsiEvaluationSession.GetDefaultConfiguration ()
                    use session = Shell.FsiEvaluationSession.Create (fsiConfig, defaultArgs, inStream, outStream, errorStream)
                    try session.EvalInteraction fsxFileString
                        let error = string errorStream
                        if error.Length > 0
                        then Log.info ("Code compiled with the following warnings (these may disable debugging of reloaded code):\n" + error)
                        else Log.info "Code compiled with no warnings."
                        Log.info "Updating code..."
                        let world = World.updateLateBindings session.DynamicAssemblies world
                        Log.info "Code updated."
                        world
                    with _ ->
                        let error = string errorStream
                        Log.trace ("Failed to compile code due to (see full output in the console):\n" + error)
                        World.choose world
            with exn ->
                Log.trace ("Failed to inspect for F# code due to: " + scstring exn)
                World.choose world

    let private handleFormReloadAll (form : GaiaForm) (args : EventArgs) =
        handleFormReloadAssets form args
        handleFormReloadCode form args

    let private handleFormGroupTabDeselected (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            deselectEntity form world
            refreshEntityPropertyGrid form world
            refreshGroupPropertyGrid form world
            world

    let private handleFormGroupTabSelected (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let selectedGroup' =
                let groupTabControl = form.groupTabControl
                let groupTab = groupTabControl.SelectedTab
                selectedScreen / groupTab.Text
            selectedGroup <- selectedGroup'
            refreshEntityPropertyGrid form world
            refreshHierarchyTreeView form world
            selectGroup selectedGroup' form world
            world

    let private handleTraceEventsCheckBoxChanged (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let eventTracerOpt =
                if form.traceEventsCheckBox.Checked
                then Some (Log.remark "Event")
                else None
            World.setEventTracerOpt eventTracerOpt world

    let private handleApplyEventFilterClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let oldWorld = world
            try let eventFilter = scvalue<EventFilter> form.eventFilterTextBox.Text
                let world = World.setEventFilter eventFilter world
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<EventFilter>).PrettyPrinter
                let eventFilterPretty = PrettyPrinter.prettyPrint (scstring eventFilter) prettyPrinter
                form.eventFilterTextBox.Text <- eventFilterPretty.Replace ("\n", "\r\n")
                world
            with exn ->
                let world = World.choose oldWorld
                MessageBox.Show ("Invalid event filter due to: " + scstring exn, "Invalid Event Filter", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleRefreshEventFilterClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let eventFilter = World.getEventFilter world
            let eventFilterStr = scstring eventFilter
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<EventFilter>).PrettyPrinter
            let eventFilterPretty = PrettyPrinter.prettyPrint eventFilterStr prettyPrinter
            form.eventFilterTextBox.Text <- eventFilterPretty.Replace ("\n", "\r\n")
            world

    let private handleSavePreludeClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            match trySavePrelude form world with
            | (true, world) ->
                match tryReloadPrelude form world with
                | (Right _, world) -> world
                | (Left error, world) ->
                    MessageBox.Show ("Prelude reload error due to: " + error + "'.", "Prelude Reload Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            | (false, world) -> world

    let private handleLoadPreludeClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            match tryReloadPrelude form world with
            | (Right preludeStr, world) ->
                form.preludeTextBox.Text <- preludeStr.Replace ("\n", "\r\n")
                world
            | (Left error, world) ->
                MessageBox.Show ("Could not load prelude due to: " + error + "'.", "Failed to Load Prelude", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                world

    let private handleSaveAssetGraphClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            match trySaveAssetGraph form world with
            | (true, world) ->
                match tryReloadAssetGraph form world with
                | (Right _, world) -> world
                | (Left error, world) ->
                    MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset Reload Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            | (false, world) -> world

    let private handleLoadAssetGraphClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            tryLoadAssetGraph form world

    let private handleSaveOverlayerClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            match trySaveOverlayer form world with
            | (true, world) ->
                match tryReloadOverlayer form world with
                | (Right _, world) -> world
                | (Left error, world) ->
                    MessageBox.Show ("Overlayer reload error due to: " + error + "'.", "Overlayer Reload Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    world
            | (false, world) -> world

    let private handleLoadOverlayerClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            tryLoadOverlayer form world

    let private handleEvalClick (form : GaiaForm) (_ : EventArgs) =
        Globals.nextPreUpdate $ fun world ->
            let exprsStr =
                if String.notEmpty form.evalInputTextBox.SelectedText
                then form.evalInputTextBox.SelectedText
                else form.evalInputTextBox.Text
            let exprsStr = Symbol.OpenSymbolsStr + "\n" + exprsStr + "\n" + Symbol.CloseSymbolsStr
            try let exprs = scvalue<Scripting.Expr array> exprsStr
                let group = selectedGroup
                let (selectedSimulant, localFrame) =
                    match form.entityPropertyGrid.SelectedObject with
                    | :?
                        EntityTypeDescriptorSource as entityTds when
                        form.propertyTabControl.SelectedTab <> form.groupTabPage ->
                        let entity = entityTds.DescribedEntity
                        (entity :> Simulant, entity.GetScriptFrame world)
                    | _ -> (group :> Simulant, group.GetScriptFrame world)
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Scripting.Expr>).PrettyPrinter
                let struct (evaleds, world) = World.evalManyWithLogging exprs localFrame selectedSimulant world
                let evaledStrs = Array.map (fun evaled -> PrettyPrinter.prettyPrint (scstring evaled) prettyPrinter) evaleds
                let evaledsStr = String.concat "\n" evaledStrs
                form.evalOutputTextBox.ReadOnly <- false
                form.evalOutputTextBox.Text <-
                    if String.notEmpty form.evalOutputTextBox.Text
                    then form.evalOutputTextBox.Text + evaledsStr.Replace ("\n", "\r\n")
                    else evaledsStr.Replace ("\n", "\r\n")
                form.evalOutputTextBox.ReadOnly <- true
                world
            with exn -> Log.debug ("Could not evaluate input due to: " + scstring exn); world

    let private handleClearOutputClick (form : GaiaForm) (_ : EventArgs) =
        form.evalOutputTextBox.Text <- String.Empty

    let private handleCreateEntityComboBoxSelectedIndexChanged (form : GaiaForm) (_ : EventArgs) =
        form.overlayComboBox.SelectedIndex <- 0

    let private handleKeyboardInput key (form : GaiaForm) world =
        if Form.ActiveForm = (form :> Form) then
            if Keys.Enter = key then
                if  form.createEntityComboBox.Focused &&
                    not form.createEntityComboBox.DroppedDown then
                    handleFormCreateEntity false false None form (EventArgs ())
            if Keys.Escape = key then
                if form.FocusedControl = form.displayPanel then deselectEntity form world
                form.displayPanel.Focus () |> ignore<bool>
            if Keys.F5 = key then form.runButton.PerformClick ()
            if Keys.D3 = key && Keys.None = Control.ModifierKeys then form.snap3dButton.Checked <- form.snap3dButton.Checked
            if Keys.Q = key && Keys.Control = Control.ModifierKeys then handleFormQuickSize form (EventArgs ())
            if Keys.N = key && Keys.Control = Control.ModifierKeys then handleFormNew form (EventArgs ())
            if Keys.O = key && Keys.Control = Control.ModifierKeys then handleFormOpen form (EventArgs ())
            if Keys.S = key && Keys.Control = Control.ModifierKeys then handleFormSave false form (EventArgs ())
            if (Keys.A = key || Keys.Enter = key) && Keys.Alt = Control.ModifierKeys then
                match form.rolloutTabControl.SelectedTab.Name with
                | "propertyEditorTabPage" -> form.applyPropertyButton.PerformClick ()
                | "preludeTabPage" -> form.applyPreludeButton.PerformClick ()
                | "assetGraphTabPage" -> form.applyAssetGraphButton.PerformClick ()
                | "overlayerTabPage" -> form.applyOverlayerButton.PerformClick ()
                | "eventTracingTabPage" -> form.applyEventFilterButton.PerformClick ()
                | _ -> ()
            if Keys.D = key && Keys.Alt = Control.ModifierKeys then
                match form.rolloutTabControl.SelectedTab.Name with
                | "propertyEditorTabPage" -> form.discardPropertyButton.PerformClick ()
                | "preludeTabPage" -> form.discardPreludeButton.PerformClick ()
                | "assetGraphTabPage" -> form.discardAssetGraphButton.PerformClick ()
                | "overlayerTabPage" -> form.discardOverlayerButton.PerformClick ()
                | "eventTracingTabPage" -> form.discardEventFilterButton.PerformClick ()
                | _ -> ()
            if (Keys.P = key || Keys.Enter = key) && Keys.Alt = Control.ModifierKeys then (form.rolloutTabControl.SelectTab "propertyEditorTabPage"; form.propertyValueTextBox.Select (); form.propertyValueTextBox.SelectAll ())
            if Keys.E = key && Keys.Alt = Control.ModifierKeys then (form.rolloutTabControl.SelectTab "evaluatorTabPage"; form.evalInputTextBox.Select ())
            if Keys.K = key && Keys.Alt = Control.ModifierKeys && form.rolloutTabControl.SelectedTab.Name = "propertyEditorTabPage" then form.pickPropertyButton.PerformClick ()
            if Keys.T = key && Keys.Alt = Control.ModifierKeys && form.rolloutTabControl.SelectedTab.Name = "eventTracingTabPage" then form.traceEventsCheckBox.Checked <- not form.traceEventsCheckBox.Checked
            if Keys.V = key && Keys.Alt = Control.ModifierKeys && form.rolloutTabControl.SelectedTab.Name = "evaluatorTabPage" then form.evalButton.PerformClick ()
            if Keys.L = key && Keys.Alt = Control.ModifierKeys && form.rolloutTabControl.SelectedTab.Name = "evaluatorTabPage" then form.evalLineButton.PerformClick ()
            match form.FocusedControl  with
            | :? ToolStripDropDown | :? TextBox -> ()
            // | :? ToolStripComboBox -> () // doesn't implement Control... not sure how to match.
            | _ ->
                if Keys.X = key && Keys.None = Control.ModifierKeys then form.constrainXButton.Checked <- not form.constrainXButton.Checked
                if Keys.Y = key && Keys.None = Control.ModifierKeys then form.constrainYButton.Checked <- not form.constrainYButton.Checked
                if Keys.Z = key && Keys.None = Control.ModifierKeys then form.constrainZButton.Checked <- not form.constrainZButton.Checked
                if Keys.C = key && Keys.None = Control.ModifierKeys then
                    form.constrainXButton.Checked <- false
                    form.constrainYButton.Checked <- false
                    form.constrainZButton.Checked <- false
                if Keys.A = key && Keys.Control = Control.ModifierKeys then handleFormSave true form (EventArgs ())
                if Keys.Z = key && Keys.Control = Control.ModifierKeys then handleFormUndo form (EventArgs ())
                if Keys.Y = key && Keys.Control = Control.ModifierKeys then handleFormRedo form (EventArgs ())
                if Keys.E = key && Keys.Control = Control.ModifierKeys then handleFormCreateEntity false false None form (EventArgs ())
                if Keys.D = key && Keys.Control = Control.ModifierKeys then handleFormDeleteEntity form (EventArgs ())
                if Keys.X = key && Keys.Control = Control.ModifierKeys then handleFormCut form (EventArgs ())
                if Keys.C = key && Keys.Control = Control.ModifierKeys then handleFormCopy form (EventArgs ())
                if Keys.V = key && Keys.Control = Control.ModifierKeys then handleFormPaste false form (EventArgs ())
                if Keys.Delete = key then handleFormDeleteEntity form (EventArgs ())

    let private handleFormClosing (_ : GaiaForm) (args : CancelEventArgs) =
        match MessageBox.Show ("Are you sure you want to close Gaia?", "Close Gaia?", MessageBoxButtons.OKCancel) with
        | DialogResult.Cancel -> args.Cancel <- true
        | _ -> ()

    let private updateEntityDrag (form : GaiaForm) world =

        if not (canEditWithMouse form world) then
            match dragEntityState with
            | DragEntityPosition2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityPosition = (entityDragOffset - mousePositionWorldOriginal) + (mousePositionWorld - mousePositionWorldOriginal)
                    let entityPositionSnapped =
                        if not form.snap3dButton.Checked
                        then Math.snapF3d (Triple.fst (getSnaps form)) entityPosition.V3
                        else entityPosition.V3
                    let entityPosition = entity.GetPosition world
                    let entityPositionDelta = entityPositionSnapped - entityPosition
                    let entityPositionConstrained =
                        match (form.constrainXButton.Checked, form.constrainYButton.Checked, form.constrainZButton.Checked) with
                        | (true, false, _) -> entityPosition + entityPositionDelta * v3Right
                        | (false, true, _) -> entityPosition + entityPositionDelta * v3Up
                        | (_, _, _) -> entityPosition + entityPositionDelta
                    let world =
                        match Option.bind (tryResolve entity) (entity.GetMountOpt world) with
                        | Some parent ->
                            let entityPositionLocal = Vector3.Transform (entityPositionConstrained, parent.GetAffineMatrix world |> Matrix4x4.Inverse)
                            entity.SetPositionLocal entityPositionLocal world
                        | None ->
                            entity.SetPosition entityPositionConstrained world
                    let world =
                        if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                            Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                            let world = entity.SetLinearVelocity v3Zero world
                            let world = entity.SetAngularVelocity v3Zero world
                            world
                        else world
                    // NOTE: disabled the following line to fix perf issue caused by refreshing the property grid every frame
                    // form.entityPropertyGrid.Refresh ()
                    world
                else world

            | DragEntityRotation2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityDegree = (entityDragOffset - mousePositionWorldOriginal.Y) + (mousePositionWorld.Y - mousePositionWorldOriginal.Y)
                    let entityDegreeSnapped =
                        if not form.snap3dButton.Checked
                        then Math.snapF (Triple.snd (getSnaps form)) entityDegree
                        else entityDegree
                    let entityDegree = (entity.GetDegreesLocal world).Z
                    let world =
                        if entity.MountExists world then
                            let entityDegreeDelta = entityDegreeSnapped - entityDegree
                            let entityDegreeLocal = entityDegree + entityDegreeDelta
                            entity.SetDegreesLocal (v3 0.0f 0.0f entityDegreeLocal) world
                        else
                            let entityDegreeDelta = entityDegreeSnapped - entityDegree
                            let entityDegree = entityDegree + entityDegreeDelta
                            entity.SetDegrees (v3 0.0f 0.0f entityDegree) world
                    let world =
                        if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                            Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                            let world = entity.SetLinearVelocity v3Zero world
                            let world = entity.SetAngularVelocity v3Zero world
                            world
                        else world
                    // NOTE: disabled the following line to fix perf issue caused by refreshing the property grid every frame
                    // form.entityPropertyGrid.Refresh ()
                    world
                else world

            | DragEntityPosition3d (time, entityDragOffset, entityPlane, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mouseRayWorld = World.getMouseRay3dWorld (entity.GetAbsolute world) world
                    let intersectionOpt = mouseRayWorld.Intersection entityPlane
                    if intersectionOpt.HasValue then
                        let entityPosition = intersectionOpt.Value - entityDragOffset
                        let entityPositionSnapped =
                            if form.snap3dButton.Checked
                            then Math.snapF3d (Triple.fst (getSnaps form)) entityPosition
                            else entityPosition
                        let entityPosition = entity.GetPosition world
                        let entityPositionDelta = entityPositionSnapped - entityPosition
                        let entityPositionConstrained =
                            match (form.constrainXButton.Checked, form.constrainYButton.Checked, form.constrainZButton.Checked) with
                            | (true, false, false) -> entityPosition + entityPositionDelta * v3Right
                            | (false, true, false) -> entityPosition + entityPositionDelta * v3Up
                            | (false, false, true) -> entityPosition + entityPositionDelta * v3Back
                            | (true, true, false) -> entityPosition + entityPositionDelta * (v3Right + v3Up)
                            | (false, true, true) -> entityPosition + entityPositionDelta * (v3Up + v3Back)
                            | (_, _, _) -> entityPosition + entityPositionDelta
                        let world =
                            match Option.bind (tryResolve entity) (entity.GetMountOpt world) with
                            | Some parent ->
                                let entityPositionLocal = Vector3.Transform (entityPositionConstrained, parent.GetAffineMatrix world |> Matrix4x4.Inverse)
                                entity.SetPositionLocal entityPositionLocal world
                            | None ->
                                entity.SetPosition entityPositionConstrained world
                        let world =
                            if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                                Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                                let world = entity.SetLinearVelocity v3Zero world
                                let world = entity.SetAngularVelocity v3Zero world
                                world
                            else world
                        // NOTE: disabled the following line to fix perf issue caused by refreshing the property grid every frame
                        // form.entityPropertyGrid.Refresh ()
                        world
                    else world
                else world

            | DragEntityRotation3d (time, mousePositionWorldOriginal, entityDragOffset, entityDragAxis, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityDegree = (entityDragOffset - mousePositionWorldOriginal.Y) + (mousePositionWorld.Y - mousePositionWorldOriginal.Y)
                    let entityDegreeSnapped =
                        if form.snap3dButton.Checked
                        then Math.snapF (Triple.snd (getSnaps form)) entityDegree
                        else entityDegree
                    let world =
                        if entity.MountExists world then
                            let entityDegreesLocal = entity.GetDegreesLocal world
                            let entityDegreeLocal = (entityDegreesLocal * entityDragAxis).Magnitude
                            let entityDegreeLocalDelta = entityDegreeSnapped - entityDegreeLocal
                            let entityDegreeLocal = entityDegreeLocal + entityDegreeLocalDelta
                            let entityDegreesLocal = entityDegreeLocal * entityDragAxis + entityDegreesLocal * (v3One - entityDragAxis)
                            entity.SetDegreesLocal entityDegreesLocal world
                        else
                            let entityDegrees = entity.GetDegrees world
                            let entityDegree = (entityDegrees * entityDragAxis).Magnitude
                            let entityDegreeDelta = entityDegreeSnapped - entityDegree
                            let entityDegree = entityDegree + entityDegreeDelta
                            let entityDegrees = entityDegree * entityDragAxis + entityDegrees * (v3One - entityDragAxis)
                            entity.SetDegrees entityDegrees world
                    let world =
                        if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                            Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                            let world = entity.SetLinearVelocity v3Zero world
                            let world = entity.SetAngularVelocity v3Zero world
                            world
                        else world
                    // NOTE: disabled the following line to fix perf issue caused by refreshing the property grid every frame
                    // form.entityPropertyGrid.Refresh ()
                    world
                else world
            | DragEntityInactive -> world
        else world

    let private updateEyeDrag (_ : GaiaForm) world =
        match dragEyeState with
        | DragEyeCenter2d (entityDragOffset, mousePositionScreenOrig) ->
            let mousePositionScreen = World.getMousePosition2dScreen world
            let eyeCenter = (entityDragOffset - mousePositionScreenOrig) + -Constants.Editor.EyeSpeed * (mousePositionScreen - mousePositionScreenOrig)
            let world = World.setEyeCenter2d eyeCenter world
            dragEyeState <- DragEyeCenter2d (entityDragOffset, mousePositionScreenOrig)
            world
        | DragEyeInactive -> world

    let private updateUndoButton (form : GaiaForm) world =
        if form.undoToolStripMenuItem.Enabled then
            if not (Globals.canUndo ()) then
                form.undoToolStripMenuItem.Enabled <- false
        elif Globals.canUndo () then
            form.undoToolStripMenuItem.Enabled <- not (World.getImperative world)

    let private updateRedoButton (form : GaiaForm) world =
        if form.redoToolStripMenuItem.Enabled then
            if not (Globals.canRedo ()) then
                form.redoToolStripMenuItem.Enabled <- false
        elif Globals.canRedo () then
            form.redoToolStripMenuItem.Enabled <- not (World.getImperative world)

    let private preUpdateEditorWorld (form : GaiaForm) world =
        let world = Globals.processPreUpdaters world
        let world =
            if refreshHierarchyViewRequested
            then refreshHierarchyTreeViewImmediate form world
            else world
        let world = updateEntityDrag form world
        let world = updateEyeDrag form world
        if  Form.ActiveForm = (form :> Form) &&
            not form.propertyValueTextBox.Focused &&
            not form.applyPropertyButton.Focused &&
            not form.IsClosing then
            refreshPropertyEditor form
        if form.IsDisposed
        then World.exit world
        else world

    let private perUpdateEditorWorld (form : GaiaForm) world =
        let world = Globals.processUpdaters world
        let world =
            if refreshHierarchyViewRequested
            then refreshHierarchyTreeViewImmediate form world
            else world
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

    let private postUpdateEditorWorld (_ : GaiaForm) world =
        Application.DoEvents ()
        world

    let rec private tryRun (form : GaiaForm) =
        try World.runWithoutCleanUp
                tautology
                (fun world -> let world = preUpdateEditorWorld form world in (Globals.World <- world; world))
                (fun world -> let world = perUpdateEditorWorld form world in (Globals.World <- world; world))
                (fun world -> let world = postUpdateEditorWorld form world in (Globals.World <- world; world))
                id
                Live true Globals.World |>
                ignore
        with exn ->
            match MessageBox.Show
                ("Unexpected exception due to: " + scstring exn + "\nWould you like to undo the last operation to try to keep Gaia running?",
                 "Unexpected Exception",
                 MessageBoxButtons.YesNo,
                 MessageBoxIcon.Error) with
            | DialogResult.Yes ->
                form.undoToolStripMenuItem.PerformClick ()
                Globals.World <- World.choose Globals.World
                tryRun form
            | _ -> Globals.World <- World.choose Globals.World

    let private refreshCreateContextMenuItemChildren atMouse inHierarchy (createContextMenuItem : ToolStripMenuItem) (form : GaiaForm) world =
        createContextMenuItem.DropDownItems.Clear ()
        let item = createContextMenuItem.DropDownItems.Add "Selected"
        item.Click.Add (handleFormCreateEntity atMouse inHierarchy None form)
        createContextMenuItem.DropDownItems.Add "-" |> ignore
        for dispatcherKvp in World.getEntityDispatchers world do
            let dispatcherName = dispatcherKvp.Key
            let item = createContextMenuItem.DropDownItems.Add dispatcherName
            item.Click.Add (handleFormCreateEntity atMouse inHierarchy (Some dispatcherName) form)

    let private run3 editModeOpt screen (form : GaiaForm) =
        Globals.World <- World.subscribe (handleNuMouseRightDown form) Events.MouseRightDown Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuEntityDragBegin form) Events.MouseLeftDown Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuEntityDragEnd form) Events.MouseLeftUp Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuEyeDragBegin form) Events.MouseMiddleDown Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuEyeDragEnd form) Events.MouseMiddleUp Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuUpdate form) Events.Update Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuRender form) Events.Render Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuEntityLifeCycle form) (Events.LifeCycle (nameof Entity)) Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuGroupLifeCycle form) (Events.LifeCycle (nameof Group)) Simulants.Game Globals.World
        Globals.World <- World.subscribe (handleNuSelectedScreenOptChange form) Simulants.Game.SelectedScreenOpt.ChangeEvent Simulants.Game Globals.World
        Globals.World <- World.setMasterSongVolume 0.0f Globals.World // no song playback in editor by default
        refreshEditModes form Globals.World
        refreshOverlayComboBox form Globals.World
        refreshCreateComboBox form Globals.World
        refreshCreateContextMenuItemChildren true false form.createContextMenuItem form Globals.World
        refreshCreateContextMenuItemChildren false true form.createInHierarchyContextMenuItem form Globals.World
        refreshGroupTabs form Globals.World
        refreshHierarchyTreeView form Globals.World
        selectGroup selectedGroup form Globals.World
        match editModeOpt with Some editMode -> form.editModeComboBox.SelectedItem <- editMode | None -> ()
        form.screenPropertyGrid.SelectedObject <- { DescribedScreen = screen; Form = form }
        form.gamePropertyGrid.SelectedObject <- { DescribedGame = Simulants.Game; Form = form }
        form.runButton.CheckState <- CheckState.Unchecked
        form.songPlaybackButton.CheckState <- if World.getMasterSongVolume Globals.World = 0.0f then CheckState.Unchecked else CheckState.Checked
        form.editModeComboBox.SelectedIndexChanged.Add (handleFormSelectEditMode form) // this event is hooked up later than normal to avoid invocation when edit mode is initially selected
        form.displayPanel.Focus () |> ignore // keeps user from having to manually click on displayPanel to interact
        form.add_LowLevelKeyboardHook (fun nCode wParam lParam ->
            let WM_KEYDOWN = 0x0100
            let WM_SYSKEYDOWN = 0x0104
            if nCode >= 0 && (wParam = IntPtr WM_KEYDOWN || wParam = IntPtr WM_SYSKEYDOWN) then
                let key = lParam |> Marshal.ReadInt32 |> enum<Keys>
                handleKeyboardInput key form Globals.World
            GaiaForm.CallNextHookEx (form.HookId, nCode, wParam, lParam)) |> ignore
        tryRun (form : GaiaForm)

    /// Attempt to select a target directory for the desired plugin and its assets from the give file path.
    let trySelectTargetDirAndMakeNuPluginFromFilePathOpt filePathOpt =
        let filePathAndDirNameAndTypesOpt =
            if not (String.IsNullOrWhiteSpace filePathOpt) then
                let filePath = filePathOpt
                try let dirName = Path.GetDirectoryName filePath
                    try Directory.SetCurrentDirectory dirName
                        let assembly = Assembly.Load (File.ReadAllBytes filePath)
                        Right (Some (filePath, dirName, assembly.GetTypes ()))
                    with _ ->
                        let assembly = Assembly.LoadFrom filePath
                        Right (Some (filePath, dirName, assembly.GetTypes ()))
                with _ -> Left ()
            else Right None
        match filePathAndDirNameAndTypesOpt with
        | Right (Some (filePath, dirName, types)) ->
            let pluginTypeOpt = Array.tryFind (fun (ty : Type) -> ty.IsSubclassOf typeof<NuPlugin>) types
            match pluginTypeOpt with
            | Some ty ->
                let plugin = Activator.CreateInstance ty :?> NuPlugin
                Right (Some (filePath, dirName, plugin))
            | None -> Left ()
        | Right None -> Right None
        | Left () -> Left ()

    /// Select a target directory for the desired plugin and its assets.
    let selectNuPlugin gaiaPlugin =
        let savedState =
            try if File.Exists Constants.Editor.SavedStateFilePath
                then scvalue (File.ReadAllText Constants.Editor.SavedStateFilePath)
                else SavedState.defaultState
            with _ -> SavedState.defaultState
        let savedStateDirectory = Directory.GetCurrentDirectory ()
        use startForm = new StartForm ()
        startForm.binaryFilePathText.TextChanged.Add (fun _ ->
            match trySelectTargetDirAndMakeNuPluginFromFilePathOpt startForm.binaryFilePathText.Text with
            | Right (Some (_, _, plugin)) ->
                startForm.modeComboBox.Items.Clear ()
                for kvp in plugin.EditModes do
                   startForm.modeComboBox.Items.Add (kvp.Key) |> ignore
                if startForm.modeComboBox.Items.Count <> 0 then
                    startForm.modeComboBox.SelectedIndex <- 0
                    startForm.modeComboBox.Enabled <- true
                else startForm.modeComboBox.Enabled <- false
            | Right None | Left () ->
                startForm.modeComboBox.Items.Clear ()
                startForm.modeComboBox.Enabled <- false
                Directory.SetCurrentDirectory AppContext.BaseDirectory)
        startForm.binaryFilePathText.Text <- savedState.BinaryFilePath
        match savedState.EditModeOpt with
        | Some mode ->
            for i in 0 .. startForm.modeComboBox.Items.Count - 1 do
                if startForm.modeComboBox.Items.[i] = box mode then
                    startForm.modeComboBox.SelectedIndex <- i
        | None -> ()
        startForm.useImperativeExecutionCheckBox.Checked <- savedState.UseImperativeExecution
        if startForm.ShowDialog () = DialogResult.OK then
            let savedState =
                { BinaryFilePath = startForm.binaryFilePathText.Text
                  EditModeOpt = if String.IsNullOrWhiteSpace startForm.modeComboBox.Text then None else Some startForm.modeComboBox.Text
                  UseImperativeExecution = startForm.useImperativeExecutionCheckBox.Checked }
            let (targetDir, plugin) =
                match trySelectTargetDirAndMakeNuPluginFromFilePathOpt startForm.binaryFilePathText.Text with
                | Right (Some (filePath, targetDir, plugin)) ->
                    Constants.Override.fromAppConfig filePath
                    try File.WriteAllText (savedStateDirectory + "/" + Constants.Editor.SavedStateFilePath, scstring savedState)
                    with _ -> Log.info "Could not save editor state."
                    (targetDir, plugin)
                | Right None ->
                    try File.WriteAllText (savedStateDirectory + "/" + Constants.Editor.SavedStateFilePath, scstring savedState)
                    with _ -> Log.info "Could not save editor state."
                    (".", gaiaPlugin)
                | Left () ->
                    if not (String.IsNullOrWhiteSpace startForm.binaryFilePathText.Text) then
                        Log.trace ("Invalid Nu Assembly: " + startForm.binaryFilePathText.Text)
                    (".", gaiaPlugin)
            (savedState, targetDir, plugin)
        else
            Directory.SetCurrentDirectory savedStateDirectory
            (SavedState.defaultState, ".", gaiaPlugin)

    /// Create a Gaia form.
    let createForm () =

        // create form
        let form = new GaiaForm ()

        // configure controls
        form.displayPanel.MaximumSize <- Drawing.Size (Constants.Render.ResolutionX, Constants.Render.ResolutionY)
        form.positionSnapTextBox.Text <- scstring Constants.Editor.Position2dSnapDefault
        form.degreesSnapTextBox.Text <- scstring Constants.Editor.Degrees2dSnapDefault
        form.scaleSnapTextBox.Text <- scstring Constants.Editor.Scale2dSnapDefault
        form.createElevationTextBox.Text <- scstring Constants.Editor.CreationElevationDefault
        form.propertyTabControl.SelectTab 3

        // same for hierarchy tree view...
        form.hierarchyTreeView.NodeMouseClick.Add (fun (args : TreeNodeMouseClickEventArgs) ->
            Globals.nextPreUpdate $ fun world ->
                if args.Button = MouseButtons.Right then
                    let nodeKey = args.Node.Name
                    let address = Address.makeFromString nodeKey
                    let entity = Entity (selectedGroup.GroupAddress <-- atoa address)
                    if entity.Exists world then
                        selectEntity entity form world
                        form.hierarchyContextMenuStrip.Show ()
                world)
        form.hierarchyTreeView.KeyDown.Add (fun args ->
            if uint args.Modifiers &&& uint Keys.Alt <> 0u && (args.KeyCode = Keys.Up || args.KeyCode = Keys.Down) then
                args.Handled <- true
                Globals.nextPreUpdate $ fun world ->
                    match form.entityPropertyGrid.SelectedObject with
                    | :? EntityTypeDescriptorSource as entityTds ->
                        let entity = entityTds.DescribedEntity
                        let peerOpt =
                            if args.KeyCode = Keys.Up then World.tryGetPreviousEntity entity world
                            elif args.KeyCode = Keys.Down then World.tryGetNextEntity entity world
                            else None
                        match peerOpt with
                        | Some peer ->
                            if not (entity.GetProtected world) && not (peer.GetProtected world) then
                                let world = World.swapEntityOrders entity peer world
                                refreshHierarchyTreeView form world
                                world
                            else
                                MessageBox.Show ("Cannot reorder a protected simulant (such as an entity created by the Elmish API).", "Protected Elmish Simulant", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                                world
                        | None -> world
                    | _ -> world)

        // set up events handlers
        form.exitToolStripMenuItem.Click.Add (handleFormExit form)
        form.createElevationPlusButton.Click.Add (handleFormCreateElevationPlusClick form)
        form.createElevationMinusButton.Click.Add (handleFormCreateElevationMinusClick form)
        form.entityPropertyGrid.SelectedObjectsChanged.Add (handleFormEntityPropertyGridSelectedObjectsChanged form)
        form.entityPropertyGrid.SelectedGridItemChanged.Add (handleFormEntityPropertyGridSelectedGridItemChanged form)
        form.groupPropertyGrid.SelectedObjectsChanged.Add (handleFormGroupPropertyGridSelectedObjectsChanged form)
        form.groupPropertyGrid.SelectedGridItemChanged.Add (handleFormGroupPropertyGridSelectedGridItemChanged form)
        form.screenPropertyGrid.SelectedObjectsChanged.Add (handleFormScreenPropertyGridSelectedObjectsChanged form)
        form.screenPropertyGrid.SelectedGridItemChanged.Add (handleFormScreenPropertyGridSelectedGridItemChanged form)
        form.gamePropertyGrid.SelectedObjectsChanged.Add (handleFormGamePropertyGridSelectedObjectsChanged form)
        form.gamePropertyGrid.SelectedGridItemChanged.Add (handleFormGamePropertyGridSelectedGridItemChanged form)
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
        form.hierarchySelectButton.Click.Add (handleFormSelectInHierarchy form)
        form.hierarchyCollapseButton.Click.Add (handleFormHierarchyTreeViewCollapseClick form)
        form.hierarchyTreeView.AfterSelect.Add (handleFormHierarchyTreeViewNodeSelect form)
        form.hierarchyTreeView.NodeMouseClick.Add (handleFormHierarchyTreeViewClick form)
        form.hierarchyTreeView.DoubleClick.Add (handleFormHierarchyTreeViewDoubleClick form)
        form.hierarchyTreeView.ItemDrag.Add (handleFormHierarchyTreeViewItemDrag form)
        form.hierarchyTreeView.DragEnter.Add (handleFormHierarchyTreeViewDragEnter form)
        form.hierarchyTreeView.DragDrop.Add (handleFormHierarchyTreeViewDragDrop form)
        form.createEntityButton.Click.Add (handleFormCreateEntity false false None form)
        form.createToolStripMenuItem.Click.Add (handleFormCreateEntity false false None form)
        form.quickSizeToolStripMenuItem.Click.Add (handleFormQuickSize form)
        form.startStopAdvancingToolStripMenuItem.Click.Add (fun _ -> form.runButton.PerformClick ())
        form.deleteContextMenuItem.Click.Add (handleFormDeleteEntity form)
        form.deleteToolStripMenuItem.Click.Add (handleFormDeleteEntity form)
        form.deleteInHierachyContextMenuItem.Click.Add (handleFormDeleteEntity form)
        form.selectInHierarchyContextMenuItem.Click.Add (handleFormSelectInHierarchy form)
        form.newGroupToolStripMenuItem.Click.Add (handleFormNew form)
        form.saveGroupToolStripMenuItem.Click.Add (handleFormSave false form)
        form.saveGroupAsToolStripMenuItem.Click.Add (handleFormSave true form)
        form.openGroupToolStripMenuItem.Click.Add (handleFormOpen form)
        form.closeGroupToolStripMenuItem.Click.Add (handleFormClose form)
        form.undoToolStripMenuItem.Click.Add (handleFormUndo form)
        form.redoToolStripMenuItem.Click.Add (handleFormRedo form)
        form.runButton.Click.Add (handleFormRunButtonClick form)
        form.songPlaybackButton.Click.Add (handleFormSongPlayback form)
        form.cutToolStripMenuItem.Click.Add (handleFormCut form)
        form.cutContextMenuItem.Click.Add (handleFormCut form)
        form.copyToolStripMenuItem.Click.Add (handleFormCopy form)
        form.copyContextMenuItem.Click.Add (handleFormCopy form)
        form.pasteToolStripMenuItem.Click.Add (handleFormPaste false form)
        form.pasteContextMenuItem.Click.Add (handleFormPaste true form)
        form.quickSizeToolStripButton.Click.Add (handleFormQuickSize form)
        form.snap3dButton.Click.Add (handleFormSnap3d form)
        form.resetEyeButton.Click.Add (handleFormResetEye form)
        form.editModeComboBox.KeyDown.Add (fun evt -> evt.Handled <- true) // disable key events for edit mode combo box
        form.reloadAssetsButton.Click.Add (handleFormReloadAssets form)
        form.reloadCodeButton.Click.Add (handleFormReloadCode form)
        form.reloadAllButton.Click.Add (handleFormReloadAll form)
        form.groupTabControl.Deselected.Add (handleFormGroupTabDeselected form)
        form.groupTabControl.Selected.Add (handleFormGroupTabSelected form)
        form.evalButton.Click.Add (handleEvalClick form)
        form.clearOutputButton.Click.Add (handleClearOutputClick form)
        form.createEntityComboBox.SelectedIndexChanged.Add (handleCreateEntityComboBoxSelectedIndexChanged form)
        form.Closing.Add (handleFormClosing form)

        // populate rollout tab text boxes
        populateAssetGraphTextBox form
        populateOverlayerTextBox form
        populatePreludeTextBox form
        populateEventFilterTextBox form

        // finally, show and activate form
        form.Show ()
        form.Activate ()
        form

    /// Attempt to make a world for use in the Gaia form.
    /// You can make your own world instead and use the Gaia.attachToWorld instead (so long as the world satisfies said
    /// function's various requirements.
    let tryMakeWorld sdlDeps worldConfig (plugin : NuPlugin) =

        // attempt to make the world
        match World.tryMake sdlDeps worldConfig plugin with
        | Right world ->

            // initialize event filter as not to flood the log
            let world = World.setEventFilter Constants.Editor.EventFilter world

            // apply any selected mode
            let world =
                match worldConfig.ModeOpt with
                | Some mode ->
                    match plugin.EditModes.TryGetValue mode with
                    | (true, modeFn) -> modeFn world
                    | (false, _) -> world
                | None -> world

            // figure out which screen to use
            let (screen, world) =
                match World.getDesiredScreen world with
                | Desire screen -> (screen, world)
                | DesireNone ->
                    let (screen, world) = World.createScreen (Some "Screen") world
                    let world = World.setDesiredScreen (Desire screen) world
                    (screen, world)
                | DesireIgnore ->
                    let (screen, world) = World.createScreen (Some "Screen") world
                    let world = World.setSelectedScreen screen world
                    (screen, world)

            // create default group if no group exists
            let world =
                if Seq.isEmpty (World.getGroups screen world)
                then World.createGroup (Some "Group") screen world |> snd
                else world

            // proceed directly to idle state
            let world = World.selectScreen (IdlingState world.GameTime) screen world
            Right (screen, world)

        // error
        | Left error -> Left error

    /// Attempt to make Gaia's SDL dependencies.
    let tryMakeSdlDeps (form : GaiaForm) =
        let sdlConfig =
            { ViewConfig = ExistingWindow form.displayPanel
              ViewW = Constants.Render.ResolutionX
              ViewH = Constants.Render.ResolutionY
              AudioChunkSize = Constants.Audio.BufferSizeDefault }
        match SdlDeps.tryMake sdlConfig with
        | Left msg -> Left msg
        | Right sdlDeps -> Right (sdlConfig, sdlDeps)

    /// Initialize Gaia.
    let init nuConfig =
        Nu.init nuConfig
        Globals.init selectEntity

    /// Run Gaia.
    let run nuConfig gaiaPlugin =
        let (savedState, targetDir', plugin) = selectNuPlugin gaiaPlugin
        use form = createForm ()
        match tryMakeSdlDeps form with
        | Right (sdlConfig, sdlDeps) ->
            let worldConfig =
                { Imperative = savedState.UseImperativeExecution
                  Advancing = false
                  ModeOpt = savedState.EditModeOpt
                  NuConfig = nuConfig
                  SdlConfig = sdlConfig }
            match tryMakeWorld sdlDeps worldConfig plugin with
            | Right (screen, world) ->
                Globals.Form <- form
                Globals.World <- world
                targetDir <- targetDir'
                selectedScreen <- screen
                selectedGroup <- Nu.World.getGroups screen world |> Seq.head
                let _ = run3 savedState.EditModeOpt screen form
                Constants.Engine.ExitCodeSuccess
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
        | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure