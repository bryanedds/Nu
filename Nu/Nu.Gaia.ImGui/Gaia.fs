// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Reflection
open FSharp.Compiler.Interactive
open FSharp.NativeInterop
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
    let mutable private selectedEntityOpt = None
    let mutable private newGroupDispatcherName = nameof GroupDispatcher
    let mutable private newEntityDispatcherName = null // this will be initialized on start
    let mutable private newEntityOverlayName = "(Default Overlay)"
    let mutable private newEntityElevation = 0.0f
    let mutable private assetViewerSearchStr = ""
    let mutable private assetPickerSearchStr = ""
    let mutable private showAssetPicker = false
    let mutable private showInspector = false
    let mutable private showNewGroupDialog = false
    let mutable private showOpenGroupDialog = false
    let mutable private showSaveGroupDialog = false
    let mutable private editWhileAdvancing = false
    let mutable private lightTheme = false // TODO: load this from config
    let mutable private newGroupName = nameof Group
    let mutable private groupFilePath = ""
    let mutable private dragDropPayloadOpt = None
    let mutable private assetGraphStr = null // this will be initialized on start
    let mutable private overlayerStr = null // this will be initialized on start
    let mutable private lightMappingConfig = { LightMappingEnabled = true }
    let mutable private ssaoConfig =
        { SsaoEnabled = true
          SsaoIntensity = Constants.Render.SsaoIntensityDefault
          SsaoBias = Constants.Render.SsaoBiasDefault
          SsaoRadius = Constants.Render.SsaoRadiusDefault
          SsaoSampleCount = Constants.Render.SsaoSampleCountDefault }
    let mutable private messageBoxOpt = Option<string>.None

    let private imGuiMessage message =
        messageBoxOpt <- Some message

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

    let private generateEntityName dispatcherName =
        let existingEntityNames =
            World.getEntitiesFlattened selectedGroup Globals.World |>
            Seq.map (fun entity -> entity.Name) |>
            Set.ofSeq
        generateEntityName3 dispatcherName existingEntityNames

    let private canEditWithMouse () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureMouse) && (World.getHalted Globals.World || editWhileAdvancing)

    let private canEditWithKeyboard () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureKeyboard) && (World.getHalted Globals.World || editWhileAdvancing)

    let private tryMousePick mousePosition =
        let entities2d = getPickableEntities2d ()
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d Globals.World
        match pickedOpt with
        | Some entity ->
            selectedEntityOpt <- Some entity
            //DUMMY
            //tryShowSelectedEntityInHierarchyIfVisible form
            Some (0.0f, entity)
        | None ->
            let entities3d = getPickableEntities3d ()
            let pickedOpt = World.tryPickEntity3d mousePosition entities3d Globals.World
            match pickedOpt with
            | Some (intersection, entity) ->
                selectedEntityOpt <- Some entity
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
        if canEditWithMouse () then
            let handling = if World.getAdvancing Globals.World then Cascade else Resolve
            let mousePosition = World.getMousePosition Globals.World
            let _ = tryMousePick mousePosition
            rightClickPosition <- mousePosition
            (handling, Globals.World)
        else (Resolve, Globals.World)

    let private handleNuEntityDragBegin (_ : Event<MouseButtonData, Game>) world =
        let world = Globals.World <- world
        if canEditWithMouse () then
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
        if canEditWithMouse () then
            let handled = if World.getAdvancing Globals.World then Cascade else Resolve
            match dragEntityState with
            | DragEntityPosition2d _ | DragEntityRotation2d _ | DragEntityPosition3d _ | DragEntityRotation3d _ ->
                dragEntityState <- DragEntityInactive
                (handled, Globals.World)
            | DragEntityInactive -> (Resolve, Globals.World)
        else (Cascade, Globals.World)

    let private handleNuEyeDragBegin (_ : Event<MouseButtonData, Game>) world =
        let world = Globals.World <- world
        if canEditWithMouse () then
            let mousePositionScreen = World.getMousePosition2dScreen Globals.World
            let dragState = DragEyeCenter2d (World.getEyeCenter2d Globals.World + mousePositionScreen, mousePositionScreen)
            dragEyeState <- dragState
            (Resolve, Globals.World)
        else (Resolve, Globals.World)

    let private handleNuEyeDragEnd (_ : Event<MouseButtonData, Game>) world =
        let world = Globals.World <- world
        if canEditWithMouse () then
            match dragEyeState with
            | DragEyeCenter2d _ ->
                dragEyeState <- DragEyeInactive
                (Resolve, Globals.World)
            | DragEyeInactive -> (Resolve, Globals.World)
        else (Resolve, Globals.World)

    let private handleNuUpdate (_ : Event<unit, Game>) world =
        let world = Globals.World <- world
        if canEditWithKeyboard () then
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
        match selectedEntityOpt with
        | Some entity when entity.Exists Globals.World ->
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
        | Some _ | None -> ()

        // fin
        (Cascade, Globals.World)

    let private trySaveSelectedGroup filePath =
        try World.writeGroupToFile filePath selectedGroup Globals.World
            filePaths <- Map.add selectedGroup.GroupAddress groupFilePath filePaths
            true
        with exn ->
            //DUMMY
            //MessageBox.Show ("Could not save file due to: " + scstring exn, "File Save Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            false

    let private tryUndo () =
        if Globals.tryUndo () then
            match selectedEntityOpt with
            | Some entity when not (entity.Exists Globals.World) ->
                selectedEntityOpt <- None
                true
            | Some _ | None -> false
        else false

    let private tryRedo () =
        if Globals.tryRedo () then
            match selectedEntityOpt with
            | Some entity when not (entity.Exists Globals.World) ->
                selectedEntityOpt <- None
                true
            | Some _ | None -> false
        else false

    let private tryLoadSelectedGroup filePath =

        // ensure group isn't protected
        if not (selectedGroup.GetProtected Globals.World) then

            // attempt to load group descriptor
            let groupAndDescriptorOpt =
                try let groupDescriptorStr = File.ReadAllText filePath
                    let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                    let groupName =
                        match groupDescriptor.GroupProperties.TryFind Constants.Engine.NamePropertyName with
                        | Some (Atom (name, _)) -> name
                        | _ -> failwithumf ()
                    Right (selectedScreen / groupName, groupDescriptor)
                with exn ->
                    Left exn

            // attempt to load group
            match groupAndDescriptorOpt with
            | Right (group, groupDescriptor) ->
                let oldWorld = Globals.World
                try
                    if group.Exists Globals.World then Globals.World <- World.destroyGroupImmediate selectedGroup Globals.World
                    let (group, world) = World.readGroup groupDescriptor None selectedScreen Globals.World
                    let world = Globals.World <- world
                    selectedGroup <- group
                    match selectedEntityOpt with
                    | Some entity when not (entity.Exists Globals.World) -> selectedEntityOpt <- None
                    | Some _ | None -> ()
                    filePaths <- Map.add group.GroupAddress groupFilePath filePaths
                    true
                with exn ->
                    Globals.World <- World.choose oldWorld
                    //DUMMY
                    //MessageBox.Show ("Could not load group file due to: " + scstring exn, "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    false

            | Left exn ->
                //DUMMY
                //MessageBox.Show ("Could not load group file due to: " + scstring exn, "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                false

        else
            //DUMMY
            //MessageBox.Show ("Cannot load into a protected simulant (such as a group created by the Elmish API).", "File Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            false

    let private createEntity atMouse inHierarchy (dispatcherNameOpt : string option) =
        Globals.pushPastWorld ()
        let dispatcherName =
            match dispatcherNameOpt with
            | Some dispatcherName -> dispatcherName
            | None -> newEntityDispatcherName
        let overlayDescriptor =
            match newEntityOverlayName with
            | "(Default Overlay)" -> DefaultOverlay
            | "(Routed Overlay)" -> RoutedOverlay
            | "(No Overlay)" -> NoOverlay
            | overlayName -> ExplicitOverlay overlayName
        let name = generateEntityName dispatcherName
        let surnames =
            match selectedEntityOpt with
            | Some entity when entity.Exists Globals.World && inHierarchy -> Array.add name entity.Surnames
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
            entityTransform.Elevation <- newEntityElevation
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
        selectedEntityOpt <- Some entity
        //DUMMY
        //tryShowSelectedEntityInHierarchy form

    let private tryQuickSizeSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists Globals.World ->
            Globals.pushPastWorld ()
            Globals.World <- entity.SetSize (entity.GetQuickSize Globals.World) Globals.World
            true
        | Some _ | None -> false

    let private tryDeleteSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists Globals.World ->
            if not (entity.GetProtected Globals.World) then
                Globals.pushPastWorld ()
                Globals.World <- World.destroyEntity entity Globals.World
                true
            else
                //DUMMY
                //MessageBox.Show ("Cannot destroy a protected simulant (such as an entity created by the Elmish API).", "Protected Elmish Simulant", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                false
        | Some _ | None -> false

    let private tryCutSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists Globals.World ->
            if not (entity.GetProtected Globals.World) then
                Globals.pushPastWorld ()
                selectedEntityOpt <- None
                Globals.World <- World.cutEntityToClipboard entity Globals.World
                true
            else
                //DUMMY
                //MessageBox.Show ("Cannot cut a protected simulant (such as an entity created by the Elmish API).", "Protected Elmish Simulant", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                false
        | Some _ | None -> false

    let private tryCopySelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists Globals.World ->
            World.copyEntityToClipboard entity Globals.World
            true
        | Some _ | None -> false

    let private tryPaste atMouse =
        Globals.pushPastWorld ()
        let surnamesOpt =
            World.tryGetEntityDispatcherNameOnClipboard Globals.World |>
            Option.map (fun dispatcherName -> generateEntityName dispatcherName) |>
            Option.map Array.singleton
        let snapsEir = if snaps2dSelected then Left snaps2d else Right snaps3d
        let (entityOpt, world) = World.pasteEntityFromClipboard atMouse rightClickPosition snapsEir surnamesOpt selectedGroup Globals.World
        let world = Globals.World <- world
        match entityOpt with
        | Some entity ->
            selectedEntityOpt <- Some entity
            //DUMMY
            //tryShowSelectedEntityInHierarchy form
            true
        | None -> false

    let private tryReloadAssets () =
        let assetSourceDir = targetDir + "/../../.."
        match World.tryReloadAssetGraph assetSourceDir targetDir Constants.Engine.RefinementDir Globals.World with
        | (Right assetGraph, world) ->
            let world = Globals.World <- world
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            assetGraphStr <- PrettyPrinter.prettyPrint (scstring assetGraph) prettyPrinter
        | (Left error, world) ->
            let world = Globals.World <- world
            //DUMMY
            //MessageBox.Show ("Asset reload error due to: " + error + "'.", "Asset Reload Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            ()

    let private tryReloadCode () =
        if World.getAllowCodeReload Globals.World then
            Globals.pushPastWorld ()
            let oldWorld = Globals.World
            selectedEntityOpt <- None // NOTE: makes sure old dispatcher doesn't hang around in old cached entity state.
            let workingDirPath = targetDir + "/../../.."
            Log.info ("Inspecting directory " + workingDirPath + " for F# code...")
            try match Array.ofSeq (Directory.EnumerateFiles (workingDirPath, "*.fsproj")) with
                | [||] -> Log.trace ("Unable to find fsproj file in '" + workingDirPath + "'.")
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
                        Globals.World <- World.updateLateBindings session.DynamicAssemblies Globals.World
                        Log.info "Code updated."
                    with _ ->
                        let error = string errorStream
                        Log.trace ("Failed to compile code due to (see full output in the console):\n" + error)
                        Globals.World <- World.choose oldWorld
            with exn ->
                Log.trace ("Failed to inspect for F# code due to: " + scstring exn)
                Globals.World <- World.choose oldWorld
        else imGuiMessage "Code reloading not allowed by current plugin. This is likely because you're using the GaiaPlugin which doesn't allow it."

    let private tryReloadAll () =
        tryReloadAssets ()
        tryReloadCode ()

    let private resetEye () =
        Globals.World <- World.setEyeCenter2d v2Zero Globals.World
        Globals.World <- World.setEyeCenter3d Constants.Engine.EyeCenter3dDefault Globals.World
        Globals.World <- World.setEyeRotation3d quatIdentity Globals.World

    let private toggleAdvancing () =
        let advancing = World.getAdvancing Globals.World
        if not advancing then Globals.pushPastWorld ()
        Globals.World <- World.setAdvancing (not advancing) Globals.World

    let private updateEyeDrag () =
        match dragEyeState with
        | DragEyeCenter2d (entityDragOffset, mousePositionScreenOrig) ->
            let mousePositionScreen = World.getMousePosition2dScreen Globals.World
            let eyeCenter = (entityDragOffset - mousePositionScreenOrig) + -Constants.Editor.EyeSpeed * (mousePositionScreen - mousePositionScreenOrig)
            Globals.World <- World.setEyeCenter2d eyeCenter Globals.World
            dragEyeState <- DragEyeCenter2d (entityDragOffset, mousePositionScreenOrig)
        | DragEyeInactive -> ()

    let private updateEntityDrag () =

        if canEditWithMouse () then
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

    let rec imGuiEntityHierarchy (entity : Entity) =
        let children = Globals.World |> entity.GetChildren |> Seq.toArray
        if ImGui.TreeNodeEx (entity.Name, if Array.notEmpty children then ImGuiTreeNodeFlags.None else ImGuiTreeNodeFlags.Leaf) then
            if ImGui.IsMouseClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                selectedEntityOpt <- Some entity
            if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                if not (entity.GetAbsolute Globals.World) then
                    if entity.GetIs2d Globals.World then
                        Globals.World <- World.setEyeCenter2d (entity.GetCenter Globals.World).V2 Globals.World
                    else
                        let eyeRotation = World.getEyeRotation3d Globals.World
                        let eyeCenterOffset = Vector3.Transform (Constants.Engine.EyeCenter3dOffset, eyeRotation)
                        Globals.World <- World.setEyeCenter3d (entity.GetPosition Globals.World + eyeCenterOffset) Globals.World
            if ImGui.BeginDragDropSource () then
                let entityAddressStr = scstring entity.EntityAddress
                dragDropPayloadOpt <- Some entityAddressStr
                ImGui.Text entity.Name
                ImGui.SetDragDropPayload ("Entity", IntPtr.Zero, 0u) |> ignore<bool>
                ImGui.EndDragDropSource ()
            if ImGui.BeginDragDropTarget () then
                if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr) then
                    match dragDropPayloadOpt with
                    | Some payload ->
                        let sourceEntityAddressStr = payload
                        let sourceEntity = Entity sourceEntityAddressStr
                        if not (sourceEntity.GetProtected Globals.World) then
                            if ImGui.IsAltPressed () then
                                let next = Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                                let previousOpt = World.tryGetPreviousEntity next Globals.World
                                let parentOpt = match next.Parent with :? Entity as parent -> Some parent | _ -> None
                                let mountOpt = match parentOpt with Some _ -> Some (Relation.makeParent ()) | None -> None
                                let sourceEntity' = match parentOpt with Some parent -> parent / sourceEntity.Name | None -> selectedGroup / sourceEntity.Name
                                Globals.World <- World.insertEntityOrder sourceEntity previousOpt next Globals.World
                                Globals.World <- World.renameEntityImmediate sourceEntity sourceEntity' Globals.World
                                Globals.World <- sourceEntity'.SetMountOptWithAdjustment mountOpt Globals.World
                                selectedEntityOpt <- Some sourceEntity'
                                //DUMMY
                                //tryShowSelectedEntityInHierarchy form
                            else
                                let sourceEntity' = Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames) / sourceEntity.Name
                                let mount = Relation.makeParent ()
                                Globals.World <- World.renameEntityImmediate sourceEntity sourceEntity' Globals.World
                                Globals.World <- sourceEntity'.SetMountOptWithAdjustment (Some mount) Globals.World
                                selectedEntityOpt <- Some sourceEntity'
                                //DUMMY
                                //ImGui.SetItemOpt ()
                                //DUMMY
                                //tryShowSelectedEntityInHierarchy form
                        else
                            //DUMMY
                            //MessageBox.Show ("Cannot relocate a protected simulant (such as an entity created by the Elmish API).", "Protected Elmish Simulant", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                            ()
                    | None -> ()
            for child in children do imGuiEntityHierarchy child
            ImGui.TreePop ()

    let imGuiProcess world =

        // TODO: figure out some sort of exception handling strategy for Gaia interaction.

        let world = Globals.World <- world

        updateEyeDrag ()

        updateEntityDrag ()

        let io = ImGui.GetIO ()
        if ImGui.IsKeyPressed ImGuiKey.F5 then toggleAdvancing ()
        if ImGui.IsKeyPressed ImGuiKey.Q && ImGui.IsCtrlPressed () then tryQuickSizeSelectedEntity () |> ignore<bool>
        if ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlPressed () then showNewGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlPressed () then showOpenGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlPressed () then showSaveGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.D && ImGui.IsCtrlPressed () then tryDeleteSelectedEntity () |> ignore<bool>
        if ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlPressed () then createEntity false false None
        if not (io.WantCaptureKeyboard) then
            if ImGui.IsKeyPressed ImGuiKey.A && ImGui.IsCtrlPressed () then showSaveGroupDialog <- true
            if ImGui.IsKeyPressed ImGuiKey.Z && ImGui.IsCtrlPressed () then tryUndo () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.Y && ImGui.IsCtrlPressed () then tryRedo () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.X && ImGui.IsCtrlPressed () then tryCutSelectedEntity () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlPressed () then tryCopySelectedEntity () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.V && ImGui.IsCtrlPressed () then tryPaste false |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.Delete then tryDeleteSelectedEntity () |> ignore<bool>

        ImGui.DockSpaceOverViewport (ImGui.GetMainViewport (), ImGuiDockNodeFlags.PassthruCentralNode) |> ignore<uint>

        if ImGui.Begin ("Gaia", ImGuiWindowFlags.MenuBar) then
            if ImGui.BeginMenuBar () then
                if ImGui.BeginMenu "File" then
                    if ImGui.MenuItem ("New Group", "Ctrl+N") then
                        showNewGroupDialog <- true
                    if ImGui.MenuItem ("Open Group", "Ctrl+O") then
                        showOpenGroupDialog <- true
                    if ImGui.MenuItem ("Save Group", "Ctrl+S") then
                        match Map.tryFind selectedGroup.GroupAddress filePaths with
                        | Some groupFilePath -> trySaveSelectedGroup groupFilePath |> ignore<bool>
                        | None -> showSaveGroupDialog <- true
                    if ImGui.MenuItem ("Save Group as...", "Ctrl+A") then
                        match Map.tryFind selectedGroup.GroupAddress filePaths with
                        | Some filePath -> groupFilePath <- filePath
                        | None -> groupFilePath <- ""
                        showSaveGroupDialog <- true
                    if ImGui.MenuItem "Close Group" then
                        let groups = Globals.World |> World.getGroups selectedScreen |> Set.ofSeq
                        if not (selectedGroup.GetProtected Globals.World) && Set.count groups > 1 then
                            Globals.pushPastWorld ()
                            let groupsRemaining = Set.remove selectedGroup groups
                            selectedEntityOpt <- None
                            Globals.World <- World.destroyGroupImmediate selectedGroup Globals.World
                            filePaths <- Map.remove selectedGroup.GroupAddress filePaths
                            selectedGroup <- Seq.head groupsRemaining
                    ImGui.Separator ()
                    if ImGui.MenuItem "Exit" then Globals.World <- World.exit Globals.World
                    ImGui.EndMenu ()
                if ImGui.BeginMenu "Edit" then
                    if ImGui.MenuItem ("Undo", "Ctrl+Z") then tryUndo () |> ignore<bool>
                    if ImGui.MenuItem ("Redo", "Ctrl+Y") then tryRedo () |> ignore<bool>
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Cut", "Ctrl+X") then tryCutSelectedEntity () |> ignore<bool>
                    if ImGui.MenuItem ("Copy", "Ctrl+C") then tryCopySelectedEntity () |> ignore<bool>
                    if ImGui.MenuItem ("Paste", "Ctrl+V") then tryPaste false |> ignore<bool>
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Create", "Ctrl+Enter") then createEntity false false None
                    if ImGui.MenuItem ("Delete", "Delete") then tryDeleteSelectedEntity () |> ignore<bool>
                    if ImGui.MenuItem ("Quick Size", "Ctrl+Q") then tryQuickSizeSelectedEntity () |> ignore<bool>
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Run/Pause", "F5") then toggleAdvancing ()
                    ImGui.EndMenu ()
                ImGui.EndMenuBar ()
            ImGui.Text "Entity:"
            ImGui.SameLine ()
            if ImGui.Button "Create" then createEntity false false None
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 150.0f
            let newEntityDispatcherNames = World.getEntityDispatchers Globals.World |> Map.toKeyArray
            if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                for dispatcherName in newEntityDispatcherNames do
                    if ImGui.Selectable dispatcherName then
                        newEntityDispatcherName <- dispatcherName
                ImGui.EndCombo ()
            ImGui.SameLine ()
            ImGui.Text "w/ Overlay"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 150.0f
            let overlayNames = Array.append [|"(Default Overlay)"; "(Routed Overlay)"; "(No Overlay)"|] (World.getOverlays Globals.World |> Map.toKeyArray)
            if ImGui.BeginCombo ("##newEntityOverlayName", newEntityOverlayName) then
                for overlayName in overlayNames do
                    if ImGui.Selectable overlayName then
                        newEntityDispatcherName <- overlayName
                ImGui.EndCombo ()
            ImGui.SameLine ()
            ImGui.Text "@ Elevation"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 50.0f
            ImGui.DragFloat ("##newEntityElevation", &newEntityElevation, 0.05f, Single.MinValue, Single.MaxValue, "%2.2f") |> ignore<bool>
            ImGui.SameLine ()
            if ImGui.Button "Quick Size" then tryQuickSizeSelectedEntity () |> ignore<bool>
            ImGui.SameLine ()
            if ImGui.Button "Delete" then tryDeleteSelectedEntity () |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            if World.getHalted Globals.World then
                if ImGui.Button "*Run*" then
                    Globals.pushPastWorld ()
                    Globals.World <- World.setAdvancing true Globals.World
            else
                if ImGui.Button "Pause" then
                    Globals.World <- World.setAdvancing false Globals.World
                ImGui.SameLine ()
                ImGui.Checkbox ("Edit", &editWhileAdvancing) |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Snap:"
            ImGui.SameLine ()
            ImGui.Text "2d"
            ImGui.SameLine ()
            ImGui.Checkbox ("##snaps2dSelected", &snaps2dSelected) |> ignore<bool>
            ImGui.SameLine ()
            let mutable (p, d, s) = if snaps2dSelected then snaps2d else snaps3d
            ImGui.Text "Pos"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 36.0f
            ImGui.DragFloat ("##p", &p, (if snaps2dSelected then 0.1f else 0.01f), 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "Deg"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 36.0f
            ImGui.DragFloat ("##d", &d, 0.1f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "Scl"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 36.0f
            ImGui.DragFloat ("##s", &s, 0.01f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
            ImGui.SameLine ()
            if snaps2dSelected then snaps2d <- (p, d, s) else snaps3d <- (p, d, s)
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Eye:"
            ImGui.SameLine ()
            if ImGui.Button "Reset" then resetEye ()
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Reload:"
            ImGui.SameLine ()
            if ImGui.Button "Assets" then tryReloadAssets ()
            ImGui.SameLine ()
            if ImGui.Button "Code" then tryReloadCode ()
            ImGui.SameLine ()
            if ImGui.Button "All" then tryReloadAll ()
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Inspector"
            ImGui.SameLine ()
            ImGui.Checkbox ("##showInspector", &showInspector) |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "Light"
            ImGui.SameLine ()
            if ImGui.Checkbox ("##darkTheme", &lightTheme) then
                if lightTheme
                then ImGui.StyleColorsLight ()
                else ImGui.StyleColorsDark ()
            ImGui.SameLine ()
            ImGui.End ()

        if ImGui.Begin "Hierarchy" then
            let groups = World.getGroups selectedScreen Globals.World
            let mutable selectedGroupName = selectedGroup.Name
            if ImGui.BeginCombo ("##selectedGroupName", selectedGroupName) then
                for group in groups do
                    if ImGui.Selectable group.Name then
                        selectedEntityOpt <- None
                        selectedGroup <- group
                ImGui.EndCombo ()
            if ImGui.BeginDragDropTarget () then
                if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr) then
                    match dragDropPayloadOpt with
                    | Some payload ->
                        let sourceEntityAddressStr = payload
                        let sourceEntity = Entity sourceEntityAddressStr
                        if not (sourceEntity.GetProtected Globals.World) then
                            let sourceEntity' = Entity (selectedGroup.GroupAddress <-- Address.makeFromName sourceEntity.Name)
                            Globals.World <- sourceEntity.SetMountOptWithAdjustment None Globals.World
                            Globals.World <- World.renameEntityImmediate sourceEntity sourceEntity' Globals.World
                            selectedEntityOpt <- Some sourceEntity'
                            //DUMMY
                            //tryShowSelectedEntityInHierarchy form
                    | None -> ()
            let entities =
                World.getEntitiesSovereign selectedGroup Globals.World |>
                Seq.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder Globals.World), entity)) |>
                Array.ofSeq |>
                Array.sortBy fst |>
                Array.map snd
            for entity in entities do
                imGuiEntityHierarchy entity
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
        //
        if ImGui.Begin "Properties" then
            match selectedEntityOpt with
            | Some entity when entity.Exists Globals.World ->
                let entityTds = { DescribedEntity = entity }
                let makePropertyDescriptor = fun (epv, tcas) -> (EntityPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
                let properties = PropertyDescriptor.getPropertyDescriptors<EntityState> makePropertyDescriptor (Some (entity, Globals.World))
                for property in properties do
                    let ty = property.PropertyType
                    let converter = SymbolicConverter ty
                    let isPropertyAssetTag = property.PropertyType.IsGenericType && property.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>
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
                    | _ when isPropertyAssetTag ->
                        let mutable valueStr' = valueStr
                        if ImGui.InputText (property.Name, &valueStr', 4096u) then
                            try let value' = converter.ConvertFromString valueStr'
                                property.SetValue (entityTds, value')
                            with
                            | :? (*Parse*)Exception // TODO: use ParseException once Prime is updated.
                            | :? ConversionException -> ()
                        if ImGui.BeginDragDropTarget () then
                            if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                match dragDropPayloadOpt with
                                | Some payload ->
                                    try let propertyValueEscaped = payload
                                        let propertyValueUnescaped = String.unescape propertyValueEscaped
                                        let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                        property.SetValue (entityTds, propertyValue)
                                    with
                                    | :? (*Parse*)Exception // TODO: use ParseException once Prime is updated.
                                    | :? ConversionException -> ()
                                | None -> ()
                            ImGui.EndDragDropTarget ()
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
                                with
                                | :? (*Parse*)Exception // TODO: use ParseException once Prime is updated.
                                | :? ConversionException -> ()
                    if ImGui.IsItemFocused () then propertyFocusedOpt <- Some property
            | Some _ | None -> ()
            ImGui.End ()

        if ImGui.Begin "Property Editor" then
            match selectedEntityOpt with
            | Some entity when entity.Exists Globals.World ->
                let entityTds = { DescribedEntity = entity }
                match propertyFocusedOpt with
                | Some property when property.PropertyType <> typeof<ComputedProperty> ->
                    let converter = SymbolicConverter (false, None, property.PropertyType)
                    match property.GetValue entityTds with
                    | null -> ()
                    | propertyValue ->
                        ImGui.Text property.Name
                        ImGui.SameLine ()
                        ImGui.Text ":"
                        ImGui.SameLine ()
                        ImGui.Text property.Description
                        let propertyValueUnescaped = converter.ConvertToString propertyValue
                        let propertyValueEscaped = String.escape propertyValueUnescaped
                        let isPropertyAssetTag = property.PropertyType.IsGenericType && property.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>
                        if isPropertyAssetTag then
                            ImGui.SameLine ()
                            if ImGui.Button "Pick" then showAssetPicker <- true
                        let mutable propertyValuePretty = PrettyPrinter.prettyPrint propertyValueEscaped PrettyPrinter.defaultPrinter
                        if ImGui.InputTextMultiline ("##propertyValuePretty", &propertyValuePretty, 131072u, v2 -1.0f -1.0f) then
                            try let propertyValueEscaped = propertyValuePretty
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                property.SetValue (entityTds, propertyValue)
                            with
                            | :? (*Parse*)Exception // TODO: use ParseException once Prime is updated.
                            | :? ConversionException -> ()
                        if isPropertyAssetTag then
                            if ImGui.BeginDragDropTarget () then
                                if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                    match dragDropPayloadOpt with
                                    | Some payload ->
                                        try let propertyValueEscaped = payload
                                            let propertyValueUnescaped = String.unescape propertyValueEscaped
                                            let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                            property.SetValue (entityTds, propertyValue)
                                        with
                                        | :? (*Parse*)Exception // TODO: use ParseException once Prime is updated.
                                        | :? ConversionException -> ()
                                    | None -> ()
                                ImGui.EndDragDropTarget ()
                | Some _ | None -> ()
            | Some _ | None -> ()
            ImGui.End ()

        if ImGui.Begin "Asset Viewer" then
            ImGui.Text "Search:"
            ImGui.SameLine ()
            ImGui.InputTextWithHint ("##assetViewerSearchStr", "[enter search text]", &assetViewerSearchStr, 4096u) |> ignore<bool>
            let assets = Metadata.getDiscoveredAssets ()
            for package in assets do
                if ImGui.TreeNode package.Key then
                    for assetName in package.Value do
                        if (assetName.ToLowerInvariant ()).Contains (assetViewerSearchStr.ToLowerInvariant ()) then
                            ImGui.TreeNodeEx (assetName, ImGuiTreeNodeFlags.Leaf) |> ignore<bool>
                            if ImGui.BeginDragDropSource () then
                                let assetTagStr = "[" + package.Key + " " + assetName + "]"
                                dragDropPayloadOpt <- Some assetTagStr
                                ImGui.Text assetTagStr
                                ImGui.SetDragDropPayload ("Asset", IntPtr.Zero, 0u) |> ignore<bool>
                                ImGui.EndDragDropSource ()
                            ImGui.TreePop ()
                    ImGui.TreePop ()
            ImGui.End ()

        if ImGui.Begin "Asset Graph" then
            if ImGui.Button "Save" then
                let assetSourceDir = targetDir + "/../../.."
                let assetGraphFilePath = assetSourceDir + "/" + Assets.Global.AssetGraphFilePath
                try let packageDescriptorsStr = assetGraphStr |> scvalue<Map<string, PackageDescriptor>> |> scstring
                    let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                    File.WriteAllText (assetGraphFilePath, PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter)
                with exn ->
                    //DUMMY
                    //MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to Save Asset Graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    ()
            ImGui.SameLine ()
            if ImGui.Button "Load" then
                match AssetGraph.tryMakeFromFile (targetDir + "/" + Assets.Global.AssetGraphFilePath) with
                | Right assetGraph ->
                    let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
                    let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                    assetGraphStr <- PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
                | Left errer ->
                    //DUMMY
                    //MessageBox.Show ("Could not read asset graph due to: " + error + "'.", "Failed to Read Asset Graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    ()
            ImGui.InputTextMultiline ("##assetGraphStr", &assetGraphStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
            ImGui.End ()

        if ImGui.Begin "Overlayer" then
            if ImGui.Button "Save" then
                let overlayerSourceDir = targetDir + "/../../.."
                let overlayerFilePath = overlayerSourceDir + "/" + Assets.Global.AssetGraphFilePath
                try let overlays = scvalue<Overlay list> overlayerStr
                    let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                    File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
                with exn ->
                    //DUMMY
                    //MessageBox.Show ("Could not save asset graph due to: " + scstring exn, "Failed to Save Asset Graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    ()
            ImGui.SameLine ()
            if ImGui.Button "Load" then
                let overlayerFilePath = targetDir + "/" + Assets.Global.OverlayerFilePath
                match Overlayer.tryMakeFromFile [] overlayerFilePath with
                | Right overlayer ->
                    let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
                    let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                    overlayerStr <- PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
                | Left error ->
                    //DUMMY
                    //MessageBox.Show ("Could not read overlayer due to: " + error + "'.", "Failed to Read Overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                    ()
            ImGui.InputTextMultiline ("##overlayerStr", &overlayerStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
            ImGui.End ()

        if ImGui.Begin "Event Tracing" then
            let mutable traceEvents = Globals.World |> World.getEventTracerOpt |> Option.isSome
            if ImGui.Checkbox ("Trace Events", &traceEvents) then
                Globals.World <- World.setEventTracerOpt (if traceEvents then Some (Log.remark "Event") else None) Globals.World
            let eventFilter = World.getEventFilter Globals.World
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<EventFilter>).PrettyPrinter
            let mutable eventFilterStr = PrettyPrinter.prettyPrint (scstring eventFilter) prettyPrinter
            if ImGui.InputTextMultiline ("##eventFilterStr", &eventFilterStr, 131072u, v2 -1.0f -1.0f) then
                try let eventFilter = scvalue<EventFilter> eventFilterStr
                    Globals.World <- World.setEventFilter eventFilter Globals.World
                with _ -> ()
            ImGui.End ()

        if ImGui.Begin "Audio Player" then
            ImGui.Text "Master Sound Volume"
            let mutable masterSoundVolume = World.getMasterSoundVolume Globals.World
            if ImGui.SliderFloat ("##masterSoundVolume", &masterSoundVolume, 0.0f, 1.0f, "", ImGuiSliderFlags.Logarithmic) then Globals.World <- World.setMasterSoundVolume masterSoundVolume Globals.World
            ImGui.SameLine ()
            ImGui.Text (string masterSoundVolume)
            ImGui.Text "Master Song Volume"
            let mutable masterSongVolume = World.getMasterSongVolume Globals.World
            if ImGui.SliderFloat ("##masterSongVolume", &masterSongVolume, 0.0f, 1.0f, "", ImGuiSliderFlags.Logarithmic) then Globals.World <- World.setMasterSongVolume masterSongVolume Globals.World
            ImGui.SameLine ()
            ImGui.Text (string masterSongVolume)
            ImGui.End ()

        if ImGui.Begin "Renderer" then
            ImGui.Text "Light-Mapping (local light mapping)"
            let mutable lightMappingEnabled = lightMappingConfig.LightMappingEnabled
            ImGui.Checkbox ("Light-Mapping Enabled", &lightMappingEnabled) |> ignore<bool>
            lightMappingConfig <- { LightMappingEnabled = lightMappingEnabled }
            Globals.World <- World.enqueueRenderMessage3d (ConfigureLightMapping lightMappingConfig) Globals.World
            ImGui.Text "Ssao (screen-space ambient occlusion)"
            let mutable ssaoEnabled = ssaoConfig.SsaoEnabled
            let mutable ssaoIntensity = ssaoConfig.SsaoIntensity
            let mutable ssaoBias = ssaoConfig.SsaoBias
            let mutable ssaoRadius = ssaoConfig.SsaoRadius
            let mutable ssaoSampleCount = ssaoConfig.SsaoSampleCount
            ImGui.Checkbox ("Ssao Enabled", &ssaoEnabled) |> ignore<bool>
            if ssaoEnabled then
                ImGui.SliderFloat ("Ssao Intensity", &ssaoIntensity, 0.0f, 4.0f) |> ignore<bool>
                ImGui.SliderFloat ("Ssao Bias", &ssaoBias, 0.0f, 0.1f) |> ignore<bool>
                ImGui.SliderFloat ("Ssao Radius", &ssaoRadius, 0.0f, 1.0f) |> ignore<bool>
                ImGui.SliderInt ("Ssao Sample Count", &ssaoSampleCount, 0, 256) |> ignore<bool>
            ssaoConfig <-
                { SsaoEnabled = ssaoEnabled
                  SsaoIntensity = ssaoIntensity
                  SsaoBias = ssaoBias
                  SsaoRadius = ssaoRadius
                  SsaoSampleCount = ssaoSampleCount }
            Globals.World <- World.enqueueRenderMessage3d (ConfigureSsao ssaoConfig) Globals.World
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
                                        match selectedEntityOpt with
                                        | Some entity when entity.Exists Globals.World ->
                                            let entityTds = { DescribedEntity = entity }
                                            match propertyFocusedOpt with
                                            | Some property when property.PropertyType <> typeof<ComputedProperty> ->
                                                let converter = SymbolicConverter (false, None, property.PropertyType)
                                                let propertyValueStr = "[" + package.Key + " " + assetName + "]"
                                                let propertyValue = converter.ConvertFromString propertyValueStr
                                                property.SetValue (entityTds, propertyValue)
                                            | Some _ | None -> ()
                                        | Some _ | None -> ()
                                        showAssetPicker <- false
                                    ImGui.TreePop ()
                        ImGui.TreePop ()
                ImGui.EndPopup ()
            if ImGui.IsKeyPressed ImGuiKey.Escape then showAssetPicker <- false

        if showNewGroupDialog then
            let title = "Create a group..."
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &showNewGroupDialog) then
                ImGui.Text "Group Name:"
                ImGui.SameLine ()
                ImGui.InputTextWithHint ("##newGroupName", "[enter group name]", &newGroupName, 4096u) |> ignore<bool>
                let groupDispatcherNames = World.getGroupDispatchers Globals.World |> Map.toKeyArray
                if ImGui.BeginCombo ("##newGroupDispatcherName", newGroupDispatcherName) then
                    for dispatcherName in groupDispatcherNames do
                        if ImGui.Selectable dispatcherName then
                            newGroupDispatcherName <- dispatcherName
                    ImGui.EndCombo ()
                let newGroup = selectedScreen / newGroupName
                if (ImGui.Button "Create" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty newGroupName && not (newGroup.Exists Globals.World) then
                    let oldWorld = Globals.World
                    try Globals.World <- World.createGroup4 newGroupDispatcherName (Some newGroupName) selectedScreen Globals.World |> snd
                        selectedGroup <- newGroup
                        showNewGroupDialog <- false
                    with exn ->
                        Globals.World <- World.choose oldWorld
                        //DUMMY
                        //MessageBox.Show ("Could not create group due to: " + scstring exn, "Group Creation Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                        ()
                if ImGui.IsKeyPressed ImGuiKey.Escape then showNewGroupDialog <- false

        if showOpenGroupDialog then
            let title = "Choose a nugroup file..."
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &showOpenGroupDialog) then
                ImGui.Text "File Path:"
                ImGui.SameLine ()
                ImGui.InputTextWithHint ("##groupFilePath", "[enter file path]", &groupFilePath, 4096u) |> ignore<bool>
                if (ImGui.Button "Open" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty groupFilePath then
                    Globals.pushPastWorld ()
                    showOpenGroupDialog <- not (tryLoadSelectedGroup groupFilePath)
                if ImGui.IsKeyPressed ImGuiKey.Escape then showOpenGroupDialog <- false

        if showSaveGroupDialog then
            let title = "Save a nugroup file..."
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &showSaveGroupDialog) then
                ImGui.Text "File Path:"
                ImGui.SameLine ()
                ImGui.InputTextWithHint ("##groupFilePath", "[enter file path]", &groupFilePath, 4096u) |> ignore<bool>
                if (ImGui.Button "Save" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty groupFilePath then
                    Globals.pushPastWorld ()
                    showSaveGroupDialog <- not (trySaveSelectedGroup groupFilePath)
            if ImGui.IsKeyPressed ImGuiKey.Escape then showSaveGroupDialog <- false

        if showInspector then
            ImGui.ShowStackToolWindow ()

        match messageBoxOpt with
        | Some messageBox ->
            let title = "Message!"
            let mutable showing = true
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &showing) then
                ImGui.TextWrapped messageBox
                if ImGui.Button "Okay" then showing <- false
                ImGui.EndPopup ()
            if not showing then messageBoxOpt <- None
        | None -> ()

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
                newEntityDispatcherName <- Nu.World.getEntityDispatchers Globals.World |> Seq.head |> fun kvp -> kvp.Key
                assetGraphStr <-
                    match AssetGraph.tryMakeFromFile (targetDir + "/" + Assets.Global.AssetGraphFilePath) with
                    | Right assetGraph ->
                        let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
                        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                        PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
                    | Left error ->
                        //DUMMY
                        //MessageBox.Show ("Could not read asset graph due to: " + error + "'.", "Failed to Read Asset Graph", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                        ""
                overlayerStr <-
                    let overlayerFilePath = targetDir + "/" + Assets.Global.OverlayerFilePath
                    match Overlayer.tryMakeFromFile [] overlayerFilePath with
                    | Right overlayer ->
                        let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
                        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                        PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
                    | Left error ->
                        //DUMMY
                        //MessageBox.Show ("Could not read overlayer due to: " + error + "'.", "Failed to Read Overlayer", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                        ""
                Globals.World <- World.subscribe handleNuMouseRightDown Events.MouseRightDown Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuEntityDragBegin Events.MouseLeftDown Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuEntityDragEnd Events.MouseLeftUp Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuEyeDragBegin Events.MouseMiddleDown Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuEyeDragEnd Events.MouseMiddleUp Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuUpdate Events.Update Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuRender Events.Render Simulants.Game Globals.World
                Globals.World <- World.subscribe handleNuSelectedScreenOptChange Simulants.Game.SelectedScreenOpt.ChangeEvent Simulants.Game Globals.World
                Globals.World <- World.setMasterSongVolume 0.0f Globals.World // no song playback in editor by default
                tryRun ()
                Constants.Engine.ExitCodeSuccess
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
        | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure