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
open FSharp.Compiler.Interactive
open FSharp.Reflection
open Prime
open ImGuiNET
open Nu
open Nu.Gaia

[<RequireQualifiedAccess>]
module Gaia =

    // uses global variables for state because Gaia relies on Nu.Gaia.Globals to interoperate Nu and WinForms
    let mutable private rightClickPosition = v2Zero
    let mutable private dragEntityState = DragEntityInactive
    let mutable private dragEyeState = DragEyeInactive
    let mutable private snaps2d = (Constants.Editor.Position2dSnapDefault, Constants.Editor.Degrees2dSnapDefault, Constants.Editor.Scale2dSnapDefault)
    let mutable private snaps3d = (Constants.Editor.Position3dSnapDefault, Constants.Editor.Degrees3dSnapDefault, Constants.Editor.Scale3dSnapDefault)
    let mutable private propertyFocusedOpt = None
    let mutable private snaps2dSelected = true
    let mutable private filePaths = Map.empty<Group Address, string>
    let mutable private targetDir = "."
    let mutable private selectedScreen = Screen "Screen" // TODO: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable private selectedGroup = selectedScreen / "Group"
    let mutable private selectedEntityTdsOpt = None
    let mutable private creationDispatcherName = null // this will be initialized on start
    let mutable private creationOverlayName = "(Default Overlay)"
    let mutable private creationElevation = 0.0f
    let mutable private assetViewerSearchStr = ""
    let mutable private assetPickerSearchStr = ""
    let mutable private showAssetPicker = false
    let mutable private showInspector = false
    let mutable private darkTheme = false // TODO: load this from config

    let private getSnaps () =
        if snaps2dSelected
        then snaps2d
        else snaps3d

    let private getPickableEntities2d () =
        let (entities, world) = World.getEntitiesInView2d (HashSet ()) Globals.World
        let world = Globals.World <- world
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible Globals.World) |> Seq.toArray
        entitiesInGroup

    let private getPickableEntities3d () =
        let (entities, world) = World.getEntitiesInView3d (HashSet ()) Globals.World
        let world = Globals.World <- world
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible Globals.World) |> Seq.toArray
        entitiesInGroup

    let rec private generateEntityName3 dispatcherName existingEntityNames =
        let mutable name = Gen.nameForEditor dispatcherName
        if Set.contains name existingEntityNames
        then generateEntityName3 dispatcherName existingEntityNames
        else name

    let private generateEntityName dispatcherName selectedGroup =
        let existingEntityNames =
            World.getEntitiesFlattened selectedGroup Globals.World |>
            Seq.map (fun entity -> entity.Name) |>
            Set.ofSeq
        generateEntityName3 dispatcherName existingEntityNames

    let private canEditWithMouse () =
        World.getAdvancing Globals.World

    let private tryMousePick mousePosition =
        let entities2d = getPickableEntities2d ()
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d Globals.World
        match pickedOpt with
        | Some entity ->
            selectedEntityTdsOpt <- Some { DescribedEntity = entity }
            //DUMMY
            //tryShowSelectedEntityInHierarchyIfVisible form
            Some (0.0f, entity)
        | None ->
            let entities3d = getPickableEntities3d ()
            let pickedOpt = World.tryPickEntity3d mousePosition entities3d Globals.World
            match pickedOpt with
            | Some (intersection, entity) ->
                selectedEntityTdsOpt <- Some { DescribedEntity = entity }
                //DUMMY
                //tryShowSelectedEntityInHierarchyIfVisible form
                Some (intersection, entity)
            | None -> None

    let private handleNuSelectedScreenOptChange (evt : Event<ChangeData, Game>) world =
        let world = Globals.World <- world
        match evt.Data.Value :?> Screen option with
        | Some screen ->
            let groups = World.getGroups screen Globals.World
            let group =
                match Seq.tryHead groups with
                | Some group -> group
                | None ->
                    let (group, world) = World.createGroup (Some "Group") screen Globals.World
                    let world = Globals.World <- world
                    group
            selectedGroup <- group
            selectedScreen <- screen
            (Cascade, Globals.World)
        | None ->
            // just keep current group selection and screen if no screen selected
            (Cascade, Globals.World)

    let private handleNuMouseRightDown (_ : Event<MouseButtonData, Game>) world =
        let world = Globals.World <- world
        let handled = if World.getAdvancing Globals.World then Cascade else Resolve
        let mousePosition = World.getMousePosition Globals.World
        let _ = tryMousePick mousePosition
        rightClickPosition <- mousePosition
        (handled, Globals.World)

    let private handleNuEntityDragBegin (_ : Event<MouseButtonData, Game>) world =
        let world = Globals.World <- world
        if not (canEditWithMouse ()) then
            let handled = if World.getAdvancing Globals.World then Cascade else Resolve
            let mousePosition = World.getMousePosition Globals.World
            match tryMousePick mousePosition with
            | Some (_, entity) ->
                Globals.pushPastWorld ()
                if World.isKeyboardShiftDown Globals.World then
                    if entity.GetIs2d Globals.World then
                        let viewport = World.getViewport Globals.World
                        let eyeCenter = World.getEyeCenter2d Globals.World
                        let eyeSize = World.getEyeSize2d Globals.World
                        let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute Globals.World, mousePosition, eyeCenter, eyeSize)
                        let entityDegrees = if entity.MountExists Globals.World then entity.GetDegreesLocal Globals.World else entity.GetDegrees Globals.World
                        dragEntityState <- DragEntityRotation2d (DateTimeOffset.Now, mousePositionWorld, entityDegrees.Z + mousePositionWorld.Y, entity)
                        (handled, Globals.World)
                    else
                        let viewport = World.getViewport Globals.World
                        let eyeCenter = World.getEyeCenter2d Globals.World
                        let eyeSize = World.getEyeSize2d Globals.World
                        let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute Globals.World, mousePosition, eyeCenter, eyeSize)
                        let entityDegrees = if entity.MountExists Globals.World then entity.GetDegreesLocal Globals.World else entity.GetDegrees Globals.World
                        let (entityDegree, entityAxis) = (entityDegrees.Y, v3Up)
                        dragEntityState <- DragEntityRotation3d (DateTimeOffset.Now, mousePositionWorld, entityDegree + mousePositionWorld.Y, entityAxis, entity)
                        (handled, Globals.World)
                else
                    if entity.GetIs2d Globals.World then
                        let viewport = World.getViewport Globals.World
                        let eyeCenter = World.getEyeCenter2d Globals.World
                        let eyeSize = World.getEyeSize2d Globals.World
                        let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute Globals.World, mousePosition, eyeCenter, eyeSize)
                        let entityPosition = entity.GetPosition Globals.World
                        dragEntityState <- DragEntityPosition2d (DateTimeOffset.Now, mousePositionWorld, entityPosition.V2 + mousePositionWorld, entity)
                        (handled, Globals.World)
                    else
                        let viewport = World.getViewport Globals.World
                        let eyeCenter = World.getEyeCenter3d Globals.World
                        let eyeRotation = World.getEyeRotation3d Globals.World
                        let mouseRayWorld = viewport.MouseToWorld3d (entity.GetAbsolute Globals.World, mousePosition, eyeCenter, eyeRotation)
                        let entityPosition = entity.GetPosition Globals.World
                        let entityPlane = plane3 entityPosition (Vector3.Transform (v3Forward, World.getEyeRotation3d Globals.World))
                        let intersectionOpt = mouseRayWorld.Intersection entityPlane
                        if intersectionOpt.HasValue then
                            let entityDragOffset = intersectionOpt.Value - entityPosition
                            dragEntityState <- DragEntityPosition3d (DateTimeOffset.Now, entityDragOffset, entityPlane, entity)
                        (handled, Globals.World)
            | None -> (handled, Globals.World)
        else (Cascade, Globals.World)

    let private handleNuEntityDragEnd (_ : Event<MouseButtonData, Game>) world =
        let world = Globals.World <- world
        if canEditWithMouse () then (Cascade, Globals.World)
        else
            let handled = if World.getAdvancing Globals.World then Cascade else Resolve
            match dragEntityState with
            | DragEntityPosition2d _ | DragEntityRotation2d _ | DragEntityPosition3d _ | DragEntityRotation3d _ ->
                dragEntityState <- DragEntityInactive
                (handled, Globals.World)
            | DragEntityInactive -> (Resolve, Globals.World)

    let private handleNuEyeDragBegin (_ : Event<MouseButtonData, Game>) world =
        let world = Globals.World <- world
        let mousePositionScreen = World.getMousePosition2dScreen Globals.World
        let dragState = DragEyeCenter2d (World.getEyeCenter2d Globals.World + mousePositionScreen, mousePositionScreen)
        dragEyeState <- dragState
        (Resolve, Globals.World)

    let private handleNuEyeDragEnd (_ : Event<MouseButtonData, Game>) world =
        let world = Globals.World <- world
        match dragEyeState with
        | DragEyeCenter2d _ ->
            dragEyeState <- DragEyeInactive
            (Resolve, Globals.World)
        | DragEyeInactive -> (Resolve, Globals.World)

    let private handleNuUpdate (_ : Event<unit, Game>) world =
        let world = Globals.World <- world
        if World.getHalted Globals.World then
            let position = World.getEyeCenter3d Globals.World
            let rotation = World.getEyeRotation3d Globals.World
            let moveSpeed =
                if World.isKeyboardShiftDown Globals.World then 0.02f
                elif World.isKeyboardKeyDown KeyboardKey.Return Globals.World then 0.5f
                else 0.12f
            let turnSpeed =
                if World.isKeyboardShiftDown Globals.World then 0.025f
                else 0.05f
            if World.isKeyboardKeyDown KeyboardKey.W Globals.World then
                Globals.World <- World.setEyeCenter3d (position + Vector3.Transform (v3Forward, rotation) * moveSpeed) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.S Globals.World then
                Globals.World <- World.setEyeCenter3d (position + Vector3.Transform (v3Back, rotation) * moveSpeed) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.A Globals.World then
                Globals.World <- World.setEyeCenter3d (position + Vector3.Transform (v3Left, rotation) * moveSpeed) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.D Globals.World then
                Globals.World <- World.setEyeCenter3d (position + Vector3.Transform (v3Right, rotation) * moveSpeed) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.Q Globals.World then
                Globals.World <- World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Right, turnSpeed)) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.E Globals.World then
                Globals.World <- World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Left, turnSpeed)) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.Up Globals.World then
                Globals.World <- World.setEyeCenter3d (position + Vector3.Transform (v3Up, rotation) * moveSpeed) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.Down Globals.World then
                Globals.World <- World.setEyeCenter3d (position + Vector3.Transform (v3Down, rotation) * moveSpeed) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.Left Globals.World then
                Globals.World <- World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Up, turnSpeed) * rotation) Globals.World
            if World.isKeyboardKeyDown KeyboardKey.Right Globals.World then
                Globals.World <- World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Down, turnSpeed) * rotation) Globals.World
            (Cascade, Globals.World)
        else (Cascade, Globals.World)

    let private handleNuRender (_ : Event<unit, Game>) world =

        // render lights of the selected group in play
        let world = Globals.World <- world
        let (entities, world) = World.getLightsInPlay3d (HashSet ()) Globals.World
        let world = Globals.World <- world
        let lightsInGroup =
            entities |>
            Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetLight Globals.World) |>
            Seq.toArray
        for light in lightsInGroup do
            Globals.World <-
                World.enqueueRenderMessage3d
                    (RenderStaticModel
                        { Absolute = false
                          ModelMatrix = light.GetAffineMatrix Globals.World
                          Presence = Prominent
                          InsetOpt = None
                          MaterialProperties = MaterialProperties.defaultProperties
                          RenderType = ForwardRenderType (0.0f, Single.MinValue / 2.0f)
                          StaticModel = Assets.Default.LightbulbModel })
                    Globals.World

        // render selection highlights
        match selectedEntityTdsOpt with
        | Some entityTds ->
            let entity = entityTds.DescribedEntity
            let absolute = entity.GetAbsolute Globals.World
            let bounds = entity.GetHighlightBounds Globals.World
            if entity.GetIs2d Globals.World then
                let elevation = Single.MaxValue
                let transform = Transform.makePerimeter bounds v3Zero elevation absolute false
                let image = Assets.Default.HighlightImage
                Globals.World <-
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
                        Globals.World
            else
                let mutable boundsMatrix = Matrix4x4.CreateScale (bounds.Size + v3Dup 0.01f) // slightly bigger to eye to prevent z-fighting with selected entity
                boundsMatrix.Translation <- bounds.Center
                Globals.World <-
                    World.enqueueRenderMessage3d
                        (RenderStaticModel
                            { Absolute = absolute
                              ModelMatrix = boundsMatrix
                              Presence = Prominent
                              InsetOpt = None
                              MaterialProperties = MaterialProperties.defaultProperties
                              RenderType = ForwardRenderType (0.0f, Single.MinValue)
                              StaticModel = Assets.Default.HighlightModel })
                        Globals.World
        | None -> ()

        // fin
        (Cascade, Globals.World)

    let private trySaveSelectedGroup filePath =
        try World.writeGroupToFile filePath selectedGroup Globals.World
        with exn ->
            //DUMMY
            //MessageBox.Show ("Could not save file due to: " + scstring exn, "File Save Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            ()

    let private tryLoadSelectedGroup filePath =

        // old world in case we need to rewind
        let oldWorld = Globals.World

        try // try to destroy current group
            if not (selectedGroup.GetProtected Globals.World) then
                Globals.World <- World.destroyGroupImmediate selectedGroup Globals.World

                // load and add group, updating tab and selected group in the process
                let groupDescriptorStr = File.ReadAllText filePath
                let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                let groupName =
                    match groupDescriptor.GroupProperties.TryFind Constants.Engine.NamePropertyName with
                    | Some (Atom (name, _)) -> name
                    | _ -> failwithumf ()
                let group = selectedScreen / groupName
                if not (group.Exists Globals.World) then
                    let (group, world) = World.readGroup groupDescriptor None selectedScreen Globals.World
                    let world = Globals.World <- world
                    selectedGroup <- group
                    Some group
            
                // handle load failure
                else
                    Globals.World <- World.choose oldWorld
                    //DUMMY
                    //MessageBox.Show ("Could not load group file with same name as an existing group", "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    None

            else
                //DUMMY
                //MessageBox.Show ("Cannot load into a protected simulant (such as a group created by the Elmish API).", "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                None

        // handle load failure
        with exn ->
            Globals.World <- World.choose oldWorld
            //DUMMY
            //MessageBox.Show ("Could not load group file due to: " + scstring exn, "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            None

    (*let private handlePropertyPickAsset (form : GaiaForm) world =
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
        else world*)

    (*let private refreshPropertyEditor (form : GaiaForm) =
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
            tryShowSelectedEntityInDisplay form world*)

    let private createEntity atMouse inHierarchy (dispatcherNameOpt : string option) =
        Globals.pushPastWorld ()
        let dispatcherName =
            match dispatcherNameOpt with
            | Some dispatcherName -> dispatcherName
            | None -> creationDispatcherName
        let overlayDescriptor =
            match creationOverlayName with
            | "(Default Overlay)" -> DefaultOverlay
            | "(Routed Overlay)" -> RoutedOverlay
            | "(No Overlay)" -> NoOverlay
            | overlayName -> ExplicitOverlay overlayName
        let name = generateEntityName dispatcherName selectedGroup
        let surnames =
            match selectedEntityTdsOpt with
            | Some entityTds when inHierarchy ->
                let parent = entityTds.DescribedEntity
                Array.add name parent.Surnames
            | Some _ | None -> [|name|]
        let (entity, world) = World.createEntity5 dispatcherName overlayDescriptor (Some surnames) selectedGroup Globals.World
        let world = Globals.World <- world
        let (positionSnap, degreesSnap, scaleSnap) = getSnaps ()
        let viewport = World.getViewport Globals.World
        let mousePosition = World.getMousePosition Globals.World
        let mutable entityTransform = entity.GetTransform Globals.World
        if entity.GetIs2d Globals.World then
            let eyeCenter = World.getEyeCenter2d Globals.World
            let eyeSize = World.getEyeSize2d Globals.World
            let entityPosition =
                if atMouse
                then viewport.MouseToWorld2d (entity.GetAbsolute Globals.World, mousePosition, eyeCenter, eyeSize)
                else viewport.MouseToWorld2d (entity.GetAbsolute Globals.World, World.getEyeSize2d Globals.World, eyeCenter, eyeSize)
            entityTransform.Position <- entityPosition.V3
            entityTransform.Size <- entity.GetQuickSize Globals.World
            entityTransform.Elevation <- creationElevation
            if snaps2dSelected
            then Globals.World <- entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform Globals.World
            else Globals.World <- entity.SetTransform entityTransform Globals.World
        else
            let eyeCenter = World.getEyeCenter3d Globals.World
            let eyeRotation = World.getEyeRotation3d Globals.World
            let entityPosition =
                if atMouse then
                    let ray = viewport.MouseToWorld3d (entity.GetAbsolute Globals.World, mousePosition, eyeCenter, eyeRotation)
                    let forward = Vector3.Transform (v3Forward, eyeRotation)
                    let plane = plane3 (eyeCenter + forward * Constants.Engine.EyeCenter3dOffset.Z) -forward
                    (ray.Intersection plane).Value
                else eyeCenter + Vector3.Transform (v3Forward, eyeRotation) * Constants.Engine.EyeCenter3dOffset.Z
            entityTransform.Position <- entityPosition
            entityTransform.Size <- entity.GetQuickSize Globals.World
            if not snaps2dSelected
            then Globals.World <- entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform Globals.World
            else Globals.World <- entity.SetTransform entityTransform Globals.World
        if inHierarchy then
            Globals.World <- entity.SetMountOptWithAdjustment (Some (Relation.makeParent ())) Globals.World
        match entity.TryGetProperty (nameof entity.ProbeBounds) Globals.World with
        | Some property when property.PropertyType = typeof<Box3> ->
            let bounds =
                box3
                    (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f + entity.GetPosition Globals.World)
                    (v3Dup Constants.Render.LightProbeSizeDefault)
            Globals.World <- entity.SetProbeBounds bounds Globals.World
        | Some _ | None -> ()
        selectedEntityTdsOpt <- Some { DescribedEntity = entity }
        //DUMMY
        //tryShowSelectedEntityInHierarchy form

    (*let private handleFormDeleteEntity (form : GaiaForm) (_ : EventArgs) =
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
        | _ -> ()*)

    let private updateEntityDrag () =

        if not (canEditWithMouse ()) then
            match dragEntityState with
            | DragEntityPosition2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists Globals.World && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute Globals.World) Globals.World
                    let entityPosition = (entityDragOffset - mousePositionWorldOriginal) + (mousePositionWorld - mousePositionWorldOriginal)
                    let entityPositionSnapped =
                        if snaps2dSelected
                        then Math.snapF3d (Triple.fst (getSnaps ())) entityPosition.V3
                        else entityPosition.V3
                    let entityPosition = entity.GetPosition Globals.World
                    let entityPositionDelta = entityPositionSnapped - entityPosition
                    let entityPositionConstrained = entityPosition + entityPositionDelta
                    match Option.bind (tryResolve entity) (entity.GetMountOpt Globals.World) with
                    | Some parent ->
                        let entityPositionLocal = Vector3.Transform (entityPositionConstrained, parent.GetAffineMatrix Globals.World |> Matrix4x4.Inverse)
                        Globals.World <- entity.SetPositionLocal entityPositionLocal Globals.World
                    | None ->
                        Globals.World <- entity.SetPosition entityPositionConstrained Globals.World
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" Globals.World) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" Globals.World) then
                        Globals.World <- entity.SetLinearVelocity v3Zero Globals.World
                        Globals.World <- entity.SetAngularVelocity v3Zero Globals.World

            | DragEntityRotation2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists Globals.World && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute Globals.World) Globals.World
                    let entityDegree = (entityDragOffset - mousePositionWorldOriginal.Y) + (mousePositionWorld.Y - mousePositionWorldOriginal.Y)
                    let entityDegreeSnapped =
                        if snaps2dSelected
                        then Math.snapF (Triple.snd (getSnaps ())) entityDegree
                        else entityDegree
                    let entityDegree = (entity.GetDegreesLocal Globals.World).Z
                    if entity.MountExists Globals.World then
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegreeLocal = entityDegree + entityDegreeDelta
                        Globals.World <- entity.SetDegreesLocal (v3 0.0f 0.0f entityDegreeLocal) Globals.World
                    else
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegree = entityDegree + entityDegreeDelta
                        Globals.World <- entity.SetDegrees (v3 0.0f 0.0f entityDegree) Globals.World
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" Globals.World) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" Globals.World) then
                        Globals.World <- entity.SetLinearVelocity v3Zero Globals.World
                        Globals.World <- entity.SetAngularVelocity v3Zero Globals.World

            | DragEntityPosition3d (time, entityDragOffset, entityPlane, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists Globals.World && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mouseRayWorld = World.getMouseRay3dWorld (entity.GetAbsolute Globals.World) Globals.World
                    let intersectionOpt = mouseRayWorld.Intersection entityPlane
                    if intersectionOpt.HasValue then
                        let entityPosition = intersectionOpt.Value - entityDragOffset
                        let entityPositionSnapped =
                            if not snaps2dSelected
                            then Math.snapF3d (Triple.fst (getSnaps ())) entityPosition
                            else entityPosition
                        let entityPosition = entity.GetPosition Globals.World
                        let entityPositionDelta = entityPositionSnapped - entityPosition
                        let entityPositionConstrained = entityPosition + entityPositionDelta
                        match Option.bind (tryResolve entity) (entity.GetMountOpt Globals.World) with
                        | Some parent ->
                            let entityPositionLocal = Vector3.Transform (entityPositionConstrained, parent.GetAffineMatrix Globals.World |> Matrix4x4.Inverse)
                            Globals.World <- entity.SetPositionLocal entityPositionLocal Globals.World
                        | None ->
                            Globals.World <- entity.SetPosition entityPositionConstrained Globals.World
                        if  Option.isSome (entity.TryGetProperty "LinearVelocity" Globals.World) &&
                            Option.isSome (entity.TryGetProperty "AngularVelocity" Globals.World) then
                            Globals.World <- entity.SetLinearVelocity v3Zero Globals.World
                            Globals.World <- entity.SetAngularVelocity v3Zero Globals.World

            | DragEntityRotation3d (time, mousePositionWorldOriginal, entityDragOffset, entityDragAxis, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists Globals.World && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute Globals.World) Globals.World
                    let entityDegree = (entityDragOffset - mousePositionWorldOriginal.Y) + (mousePositionWorld.Y - mousePositionWorldOriginal.Y)
                    let entityDegreeSnapped =
                        if not snaps2dSelected
                        then Math.snapF (Triple.snd (getSnaps ())) entityDegree
                        else entityDegree                    
                    if entity.MountExists Globals.World then
                        let entityDegreesLocal = entity.GetDegreesLocal Globals.World
                        let entityDegreeLocal = (entityDegreesLocal * entityDragAxis).Magnitude
                        let entityDegreeLocalDelta = entityDegreeSnapped - entityDegreeLocal
                        let entityDegreeLocal = entityDegreeLocal + entityDegreeLocalDelta
                        let entityDegreesLocal = entityDegreeLocal * entityDragAxis + entityDegreesLocal * (v3One - entityDragAxis)
                        Globals.World <- entity.SetDegreesLocal entityDegreesLocal Globals.World
                    else
                        let entityDegrees = entity.GetDegrees Globals.World
                        let entityDegree = (entityDegrees * entityDragAxis).Magnitude
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegree = entityDegree + entityDegreeDelta
                        let entityDegrees = entityDegree * entityDragAxis + entityDegrees * (v3One - entityDragAxis)
                        Globals.World <- entity.SetDegrees entityDegrees Globals.World
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" Globals.World) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" Globals.World) then
                        Globals.World <- entity.SetLinearVelocity v3Zero Globals.World
                        Globals.World <- entity.SetAngularVelocity v3Zero Globals.World
            | DragEntityInactive -> ()

    (*let private updateEyeDrag (_ : GaiaForm) world =
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
        world*)

    let imGuiProcess world =

        // TODO: figure out some sort of exception handling strategy for Gaia interaction.

        let world = Globals.World <- world

        updateEntityDrag ()

        ImGui.DockSpaceOverViewport (ImGui.GetMainViewport (), ImGuiDockNodeFlags.PassthruCentralNode) |> ignore<uint>

        if ImGui.Begin ("Gaia", ImGuiWindowFlags.MenuBar) then
            if ImGui.BeginMenuBar () then
                if ImGui.BeginMenu "File" then
                    if ImGui.MenuItem ("New Group", "Ctrl+N") then ()
                    if ImGui.MenuItem ("Open Group", "Ctrl+O") then ()
                    if ImGui.MenuItem ("Save Group", "Ctrl+S") then ()
                    if ImGui.MenuItem ("Save Group as...", "Ctrl+A") then ()
                    if ImGui.MenuItem "Close Group" then ()
                    ImGui.Separator ()
                    if ImGui.MenuItem "Exit" then Globals.World <- World.exit Globals.World
                    ImGui.EndMenu ()
                if ImGui.BeginMenu "Edit" then
                    if ImGui.MenuItem ("Undo", "Ctrl+Z") then ()
                    if ImGui.MenuItem ("Redo", "Ctrl+Y") then ()
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Cut", "Ctrl+X") then ()
                    if ImGui.MenuItem ("Copy", "Ctrl+C") then ()
                    if ImGui.MenuItem ("Paste", "Ctrl+V") then ()
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Create", "Ctrl+E") then ()
                    if ImGui.MenuItem ("Delete", "Ctrl+D") then ()
                    if ImGui.MenuItem ("Quick Size", "Ctrl+Q") then ()
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Run / Pause", "F5") then ()
                    ImGui.EndMenu ()
                ImGui.EndMenuBar ()
            if ImGui.Button "Create" then createEntity false false None
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 150.0f
            let creationDispatcherNames = World.getEntityDispatchers Globals.World |> Map.toKeyArray
            if ImGui.BeginCombo ("##creationDispatcherName", creationDispatcherName) then
                for dispatcherName in creationDispatcherNames do
                    if ImGui.Selectable dispatcherName then
                        creationDispatcherName <- dispatcherName
                ImGui.EndCombo ()
            ImGui.SameLine ()
            ImGui.Text "w/ Overlay"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 150.0f
            let overlayNames = Array.append [|"(Default Overlay)"; "(Routed Overlay)"; "(No Overlay)"|] (World.getOverlays Globals.World |> Map.toKeyArray)
            if ImGui.BeginCombo ("##creationOverlayName", creationOverlayName) then
                for overlayName in overlayNames do
                    if ImGui.Selectable overlayName then
                        creationDispatcherName <- overlayName
                ImGui.EndCombo ()
            ImGui.SameLine ()
            ImGui.Text "@ Elevation"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 50.0f
            ImGui.DragFloat ("##creationElevation", &creationElevation) |> ignore<bool>
            ImGui.SameLine ()
            if ImGui.Button "Quick Size" then ()
            ImGui.SameLine ()
            if World.getHalted Globals.World then
                if ImGui.Button "<Run>" then
                    Globals.pushPastWorld ()
                    Globals.World <- World.setAdvancing true Globals.World
            else
                if ImGui.Button "Pause" then
                    Globals.pushPastWorld ()
                    Globals.World <- World.setAdvancing false Globals.World
            ImGui.SameLine ()
            ImGui.Text "Inspector"
            ImGui.SameLine ()
            ImGui.Checkbox ("##showInspector", &showInspector) |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "Dark"
            ImGui.SameLine ()
            if ImGui.Checkbox ("##darkTheme", &darkTheme) then
                if darkTheme
                then ImGui.StyleColorsDark ()
                else ImGui.StyleColorsLight ()
            ImGui.SameLine ()
            ImGui.End ()

        if ImGui.Begin "Hierarchy" then
            let entities =
                World.getEntitiesSovereign selectedGroup Globals.World |>
                Seq.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder Globals.World), entity)) |>
                Array.ofSeq |>
                Array.sortBy fst |>
                Array.map snd
            for entity in entities do
                ImGui.TreeNode entity.Name |> ignore<bool>
                for child in entity.GetChildren Globals.World do
                    ImGui.Indent ()
                    ImGui.TreeNode child.Name |> ignore<bool>
                    ImGui.Unindent ()
            ImGui.End ()

        // TODO: implement in order of priority -
        //
        //  option & voption with custom checkbox header
        //  Enums
        //  AssetTag w/ picking
        //  RenderStyle
        //  Substance
        //  SymbolicCompression
        //  TmxMap
        //  LightType
        //  MaterialProperties
        //
        //  Layout
        //  CollisionMask
        //  CollisionCategories
        //  CollisionDetection
        //  BodyShape
        //  JointDevice
        //  DateTimeOffset?
        //  Flag Enums
        if ImGui.Begin "Properties" then
            match selectedEntityTdsOpt with
            | Some entityTds ->
                let entity = entityTds.DescribedEntity
                let makePropertyDescriptor = fun (epv, tcas) -> (EntityPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
                let properties = PropertyDescriptor.getPropertyDescriptors<EntityState> makePropertyDescriptor (Some (entity, Globals.World))
                for property in properties do
                    let ty = property.PropertyType
                    let converter = SymbolicConverter ty
                    let value = property.GetValue entityTds
                    let valueStr = converter.ConvertToString value
                    match value with
                    | :? bool as b -> let mutable b' = b in if ImGui.Checkbox (property.Name, &b') then property.SetValue (entityTds, b')
                    | :? int8 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.Name, &i') then property.SetValue (entityTds, int8 i')
                    | :? uint8 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.Name, &i') then property.SetValue (entityTds, uint8 i')
                    | :? int16 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.Name, &i') then property.SetValue (entityTds, int16 i')
                    | :? uint16 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.Name, &i') then property.SetValue (entityTds, uint16 i')
                    | :? int32 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.Name, &i') then property.SetValue (entityTds, int32 i')
                    | :? uint32 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.Name, &i') then property.SetValue (entityTds, uint32 i')
                    | :? int64 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.Name, &i') then property.SetValue (entityTds, int64 i')
                    | :? uint64 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.Name, &i') then property.SetValue (entityTds, uint64 i')
                    | :? single as f -> let mutable f' = single f in if ImGui.DragFloat (property.Name, &f') then property.SetValue (entityTds, single f')
                    | :? double as f -> let mutable f' = single f in if ImGui.DragFloat (property.Name, &f') then property.SetValue (entityTds, double f')
                    | :? Vector2 as v -> let mutable v' = v in if ImGui.DragFloat2 (property.Name, &v') then property.SetValue (entityTds, v')
                    | :? Vector3 as v -> let mutable v' = v in if ImGui.DragFloat3 (property.Name, &v') then property.SetValue (entityTds, v')
                    | :? Vector4 as v -> let mutable v' = v in if ImGui.DragFloat4 (property.Name, &v') then property.SetValue (entityTds, v')
                    | :? Vector2i as v -> let mutable v' = v in if ImGui.DragInt2 (property.Name, &v'.X) then property.SetValue (entityTds, v')
                    | :? Vector3i as v -> let mutable v' = v in if ImGui.DragInt3 (property.Name, &v'.X) then property.SetValue (entityTds, v')
                    | :? Vector4i as v -> let mutable v' = v in if ImGui.DragInt4 (property.Name, &v'.X) then property.SetValue (entityTds, v')
                    | :? Box2 as b ->
                        ImGui.Text property.Name
                        let mutable min = v2 b.Min.X b.Min.Y
                        let mutable size = v2 b.Size.X b.Size.Y
                        ImGui.Indent ()
                        if  ImGui.DragFloat2 ("Min", &min) ||
                            ImGui.DragFloat2 ("Size", &size) then
                            let b' = box2 min size
                            property.SetValue (entityTds, b')
                        ImGui.Unindent ()
                    | :? Box3 as b ->
                        ImGui.Text property.Name
                        let mutable min = v3 b.Min.X b.Min.Y b.Min.Z
                        let mutable size = v3 b.Size.X b.Size.Y b.Size.Z
                        ImGui.Indent ()
                        if  ImGui.DragFloat3 ("Min", &min) ||
                            ImGui.DragFloat3 ("Size", &size) then
                            let b' = box3 min size
                            property.SetValue (entityTds, b')
                        ImGui.Unindent ()
                    | :? Box2i as b ->
                        ImGui.Text property.Name
                        let mutable min = v2i b.Min.X b.Min.Y
                        let mutable size = v2i b.Size.X b.Size.Y
                        ImGui.Indent ()
                        if  ImGui.DragInt2 ("Min", &min.X) ||
                            ImGui.DragInt2 ("Size", &size.X) then
                            let b' = box2i min size
                            property.SetValue (entityTds, b')
                        ImGui.Unindent ()
                    | :? Quaternion as q ->
                        let mutable v = v4 q.X q.Y q.Z q.W
                        if ImGui.DragFloat4 (property.Name, &v) then
                            let q' = quat v.X v.Y v.Z v.W
                            property.SetValue (entityTds, q')
                    | :? Color as c ->
                        let mutable v = v4 c.R c.G c.B c.A
                        if ImGui.ColorEdit4 (property.Name, &v) then
                            let c' = color v.X v.Y v.Z v.W
                            property.SetValue (entityTds, c')
                    | _ ->
                        let mutable combo = false
                        if FSharpType.IsUnion ty then
                            let cases = FSharpType.GetUnionCases ty
                            if Array.forall (fun (case : UnionCaseInfo) -> Array.isEmpty (case.GetFields ())) cases then
                                combo <- true
                                let caseNames = Array.map (fun (case : UnionCaseInfo) -> case.Name) cases
                                let (unionCaseInfo, _) = FSharpValue.GetUnionFields (value, ty)
                                let mutable tag = unionCaseInfo.Tag
                                if ImGui.Combo (property.Name, &tag, caseNames, caseNames.Length) then
                                    let value' = FSharpValue.MakeUnion (cases.[tag], [||])
                                    property.SetValue (entityTds, value')
                        if not combo then
                            let mutable valueStr' = valueStr
                            if ImGui.InputText (property.Name, &valueStr', 131072u) then
                                try let value' = converter.ConvertFromString valueStr'
                                    property.SetValue (entityTds, value')
                                with _ -> ()
                    if ImGui.IsItemFocused () then propertyFocusedOpt <- Some property
            | None -> ()
            ImGui.End ()

        if ImGui.Begin "Property Editor" then
            match selectedEntityTdsOpt with
            | Some entityTds ->
                match propertyFocusedOpt with
                | Some property when property.PropertyType <> typeof<ComputedProperty> ->
                    let typeConverter = SymbolicConverter (false, None, property.PropertyType)
                    match property.GetValue entityTds with
                    | null -> ()
                    | propertyValue ->
                        ImGui.Text property.Name
                        ImGui.SameLine ()
                        ImGui.Text ":"
                        ImGui.SameLine ()
                        ImGui.Text property.Description
                        let propertyValueUnescaped = typeConverter.ConvertToString propertyValue
                        let propertyValueEscaped = String.escape propertyValueUnescaped
                        if  property.PropertyType = typeof<Entity Relation option> ||
                            (property.PropertyType.IsGenericType && property.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>) then
                            ImGui.SameLine ()
                            if ImGui.Button "Pick" then showAssetPicker <- true
                        let mutable propertyValuePretty = PrettyPrinter.prettyPrint propertyValueEscaped PrettyPrinter.defaultPrinter
                        if ImGui.InputTextMultiline ("##propertyValuePretty", &propertyValuePretty, 131072u, v2 -1.0f -1.0f) then
                            try let propertyValueEscaped = propertyValuePretty
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = typeConverter.ConvertFromString propertyValueUnescaped
                                property.SetValue (entityTds, propertyValue)
                            with
                            | :? (*Parse*)Exception // TODO: use ParseException once Prime is updated.
                            | :? ConversionException -> ()
                | Some _ | None -> ()
            | None -> ()
            ImGui.End ()

        if ImGui.Begin "Asset Viewer" then
            ImGui.Text "Search:"
            ImGui.SameLine ()
            ImGui.InputTextWithHint ("##searchString", "[enter search text]", &assetViewerSearchStr, 4096u) |> ignore<bool>
            let assets = Metadata.getDiscoveredAssets ()
            for package in assets do
                if ImGui.TreeNode package.Key then
                    for assetName in package.Value do
                        if (assetName.ToLowerInvariant ()).Contains (assetViewerSearchStr.ToLowerInvariant ()) then
                            if ImGui.TreeNodeEx (assetName, ImGuiTreeNodeFlags.Leaf) then
                                ImGui.TreePop ()
                    ImGui.TreePop ()
            ImGui.End ()

        if showAssetPicker then
            let title = "Choose an Asset..."
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &showAssetPicker) then
                ImGui.Text "Search:"
                ImGui.SameLine ()
                ImGui.InputTextWithHint ("##searchString", "[enter search text]", &assetPickerSearchStr, 4096u) |> ignore<bool>
                let assets = Metadata.getDiscoveredAssets ()
                for package in assets do
                    if ImGui.TreeNode package.Key then
                        for assetName in package.Value do
                            if (assetName.ToLowerInvariant ()).Contains (assetPickerSearchStr.ToLowerInvariant ()) then
                                if ImGui.TreeNodeEx (assetName, ImGuiTreeNodeFlags.Leaf) then
                                    if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                                        match selectedEntityTdsOpt with
                                        | Some entityTds ->
                                            match propertyFocusedOpt with
                                            | Some property when property.PropertyType <> typeof<ComputedProperty> ->
                                                let typeConverter = SymbolicConverter (false, None, property.PropertyType)
                                                let propertyValueStr = "[" + package.Key + " " + assetName + "]"
                                                let propertyValue = typeConverter.ConvertFromString propertyValueStr
                                                property.SetValue (entityTds, propertyValue)
                                            | Some _ | None -> ()
                                        | None -> ()
                                        showAssetPicker <- false
                                    ImGui.TreePop ()
                        ImGui.TreePop ()
                ImGui.EndPopup ()
            if ImGui.IsKeyPressed ImGuiKey.Escape then showAssetPicker <- false

        if showInspector then
            ImGui.ShowStackToolWindow ()

        Globals.World

    let rec private tryRun () =
        try Globals.World <- World.runWithoutCleanUp tautology id id id imGuiProcess Live true Globals.World
        with exn ->
            //DUMMY
            //match MessageBox.Show
            //    ("Unexpected exception due to: " + scstring exn + "\nWould you like to undo the last operation to try to keep Gaia running?",
            //     "Unexpected Exception",
            //     MessageBoxButtons.YesNo,
            //     MessageBoxIcon.Error) with
            //| DialogResult.Yes ->
            //    form.undoToolStripMenuItem.PerformClick ()
            //    Globals.World <- World.choose Globals.World
            //    tryRun form
            //| _ -> Globals.World <- World.choose Globals.World
            ()

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
        let (targetDir, plugin) =
            match trySelectTargetDirAndMakeNuPluginFromFilePathOpt savedState.AssemblyFilePath with
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
                if not (String.IsNullOrWhiteSpace savedState.AssemblyFilePath) then
                    Log.trace ("Invalid Nu Assembly: " + savedState.AssemblyFilePath)
                (".", gaiaPlugin)
        (savedState, targetDir, plugin)

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
    let tryMakeSdlDeps () =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        match SdlDeps.tryMake sdlConfig with
        | Left msg -> Left msg
        | Right sdlDeps -> Right (sdlConfig, sdlDeps)

    /// Run Gaia.
    let run nuConfig gaiaPlugin =
        let (savedState, targetDir', plugin) = selectNuPlugin gaiaPlugin
        match tryMakeSdlDeps () with
        | Right (sdlConfig, sdlDeps) ->
            let worldConfig =
                { Imperative = savedState.UseImperativeExecution
                  Advancing = false
                  ModeOpt = savedState.EditModeOpt
                  NuConfig = nuConfig
                  SdlConfig = sdlConfig }
            match tryMakeWorld sdlDeps worldConfig plugin with
            | Right (screen, world) ->
                let world = Globals.World <- world
                targetDir <- targetDir'
                selectedScreen <- screen
                selectedGroup <- Nu.World.getGroups screen Globals.World |> Seq.head
                creationDispatcherName <- Nu.World.getEntityDispatchers Globals.World |> Seq.head |> fun kvp -> kvp.Key
                Globals.World <- World.subscribe handleNuMouseRightDown Events.MouseRightDown Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuEntityDragBegin Events.MouseLeftDown Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuEntityDragEnd Events.MouseLeftUp Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuEyeDragBegin Events.MouseCenterDown Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuEyeDragEnd Events.MouseCenterUp Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuUpdate Events.Update Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuRender Events.Render Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuSelectedScreenOptChange Simulants.Game.SelectedScreenOpt.ChangeEvent Simulants.Game Globals.World
                Globals.World <- World.setMasterSongVolume 0.0f Globals.World // no song playback in editor by default
                tryRun ()
                Constants.Engine.ExitCodeSuccess
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
        | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure