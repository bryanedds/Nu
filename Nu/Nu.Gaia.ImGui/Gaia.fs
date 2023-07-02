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
open ImGuizmoNET
open Nu
open Nu.Gaia

[<RequireQualifiedAccess>]
module Gaia =

    let mutable private world = Unchecked.defaultof<World> // this will be initialized on start
    let mutable private rightClickPosition = v2Zero
    let mutable private dragEntityState = DragEntityInactive
    let mutable private dragEyeState = DragEyeInactive
    let mutable private snaps2dSelected = true
    let mutable private snaps2d = (Constants.Editor.Position2dSnapDefault, Constants.Editor.Degrees2dSnapDefault, Constants.Editor.Scale2dSnapDefault)
    let mutable private snaps3d = (Constants.Editor.Position3dSnapDefault, Constants.Editor.Degrees3dSnapDefault, Constants.Editor.Scale3dSnapDefault)
    let mutable private fullScreen = false
    let mutable private propertyDescriptorFocusedOpt = None
    let mutable private filePaths = Map.empty<Group Address, string>
    let mutable private targetDir = "."
    let mutable private selectedScreen = Screen "Screen" // TODO: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable private selectedGroup = selectedScreen / "Group"
    let mutable private selectedEntityOpt = Option<Entity>.None
    let mutable private newGroupDispatcherName = nameof GroupDispatcher
    let mutable private newEntityDispatcherName = null // this will be initialized on start
    let mutable private newEntityOverlayName = "(Default Overlay)"
    let mutable private newEntityElevation = 0.0f
    let mutable private assetViewerSearchStr = ""
    let mutable private assetPickerSearchStr = ""
    let mutable private showContextMenu = false
    let mutable private showAssetPicker = false
    let mutable private showInspector = false
    let mutable private showNewGroupDialog = false
    let mutable private showOpenGroupDialog = false
    let mutable private showSaveGroupDialog = false
    let mutable private editWhileAdvancing = false
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
    let mutable private manipulating = false
    let mutable private messageBoxOpt = Option<string>.None
    let mutable private worldsPast = []
    let mutable private worldsFuture = []

    let private imGuiMessage message =
        messageBoxOpt <- Some message

    let snapshot () =
        world <- Nu.World.shelve world
        worldsPast <- world :: worldsPast
        worldsFuture <- []

    let canUndo () =
        List.notEmpty worldsPast

    let canRedo () =
        List.notEmpty worldsPast

    let private tryUndo () =
        if
            (if not (Nu.World.getImperative world) then
                match worldsPast with
                | worldPast :: worldsPast' ->
                    let worldFuture = Nu.World.shelve world
                    world <- Nu.World.unshelve worldPast
                    worldsPast <- worldsPast'
                    worldsFuture <- worldFuture :: worldsFuture
                    true
                | [] -> false
             else false) then
            match selectedEntityOpt with
            | Some entity when not (entity.Exists world) ->
                selectedEntityOpt <- None
                true
            | Some _ | None -> false
        else false

    let private tryRedo () =
        if
            (if not (Nu.World.getImperative world) then
                match worldsFuture with
                | worldFuture :: worldsFuture' ->
                    let worldPast = Nu.World.shelve world
                    world <- Nu.World.unshelve worldFuture
                    worldsPast <- worldPast :: worldsPast
                    worldsFuture <- worldsFuture'
                    true
                | [] -> false
             else false) then
            match selectedEntityOpt with
            | Some entity when not (entity.Exists world) ->
                selectedEntityOpt <- None
                true
            | Some _ | None -> false
        else false

    let private getSnaps () =
        if snaps2dSelected
        then snaps2d
        else snaps3d

    let private getPickableEntities2d () =
        let (entities, wtemp) = World.getEntitiesInView2d (HashSet ()) world in world <- wtemp
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible world) |> Seq.toArray
        entitiesInGroup

    let private getPickableEntities3d () =
        let (entities, wtemp) = World.getEntitiesInView3d (HashSet ()) world in world <- wtemp
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible world) |> Seq.toArray
        entitiesInGroup

    let rec private generateEntityName3 dispatcherName existingEntityNames =
        let mutable name = Gen.nameForEditor dispatcherName
        if Set.contains name existingEntityNames
        then generateEntityName3 dispatcherName existingEntityNames
        else name

    let private generateEntityName dispatcherName =
        let existingEntityNames =
            World.getEntitiesFlattened selectedGroup world |>
            Seq.map (fun entity -> entity.Name) |>
            Set.ofSeq
        generateEntityName3 dispatcherName existingEntityNames

    let private canEditWithMouse () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureMouse) && (World.getHalted world || editWhileAdvancing)

    let private canEditWithKeyboard () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureKeyboard) && (World.getHalted world || editWhileAdvancing)

    let private tryMousePick mousePosition =
        let entities2d = getPickableEntities2d ()
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d world
        match pickedOpt with
        | Some entity ->
            selectedEntityOpt <- Some entity
            //DUMMY
            //tryShowSelectedEntityInHierarchyIfVisible form
            Some (0.0f, entity)
        | None ->
            let entities3d = getPickableEntities3d ()
            let pickedOpt = World.tryPickEntity3d mousePosition entities3d world
            match pickedOpt with
            | Some (intersection, entity) ->
                selectedEntityOpt <- Some entity
                //DUMMY
                //tryShowSelectedEntityInHierarchyIfVisible form
                Some (intersection, entity)
            | None -> None

    let private handleNuSelectedScreenOptChange (evt : Event<ChangeData, Game>) wtemp =
        world <- wtemp
        match evt.Data.Value :?> Screen option with
        | Some screen ->
            let groups = World.getGroups screen world
            let group =
                match Seq.tryHead groups with
                | Some group -> group
                | None ->
                    let (group, wtemp) = World.createGroup (Some "Group") screen world in world <- wtemp
                    group
            selectedGroup <- group
            selectedScreen <- screen
            (Cascade, world)
        | None ->
            // just keep current group selection and screen if no screen selected
            (Cascade, world)

    let private handleNuEntityContext (_ : Event<MouseButtonData, Game>) wtemp =
        world <- wtemp
        if canEditWithMouse () then
            let handling = if World.getAdvancing world then Cascade else Resolve
            let mousePosition = World.getMousePosition world
            let _ = tryMousePick mousePosition
            rightClickPosition <- mousePosition
            showContextMenu <- true
            (handling, world)
        else (Resolve, world)

    let private handleNuEntityDragBegin (_ : Event<MouseButtonData, Game>) wtemp =
        world <- wtemp
        if canEditWithMouse () then
            let handled = if World.getAdvancing world then Cascade else Resolve
            let mousePosition = World.getMousePosition world
            match tryMousePick mousePosition with
            | Some (_, entity) ->
                if entity.GetIs2d world then
                    snapshot ()
                    if World.isKeyboardShiftDown world then
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
                        let entityPosition = entity.GetPosition world
                        dragEntityState <- DragEntityPosition2d (DateTimeOffset.Now, mousePositionWorld, entityPosition.V2 + mousePositionWorld, entity)
                        (handled, world)
                else (handled, world)
            | None -> (handled, world)
        else (Cascade, world)

    let private handleNuEntityDragEnd (_ : Event<MouseButtonData, Game>) wtemp =
        world <- wtemp
        if canEditWithMouse () then
            let handled = if World.getAdvancing world then Cascade else Resolve
            match dragEntityState with
            | DragEntityPosition2d _ | DragEntityRotation2d _ ->
                dragEntityState <- DragEntityInactive
                (handled, world)
            | DragEntityInactive -> (Resolve, world)
        else (Cascade, world)

    let private handleNuEyeDragBegin (_ : Event<MouseButtonData, Game>) wtemp =
        world <- wtemp
        if canEditWithMouse () then
            let mousePositionScreen = World.getMousePosition2dScreen world
            let dragState = DragEyeCenter2d (World.getEyeCenter2d world + mousePositionScreen, mousePositionScreen)
            dragEyeState <- dragState
            (Resolve, world)
        else (Resolve, world)

    let private handleNuEyeDragEnd (_ : Event<MouseButtonData, Game>) wtemp =
        world <- wtemp
        if canEditWithMouse () then
            match dragEyeState with
            | DragEyeCenter2d _ ->
                dragEyeState <- DragEyeInactive
                (Resolve, world)
            | DragEyeInactive -> (Resolve, world)
        else (Resolve, world)

    let private handleNuUpdate (_ : Event<unit, Game>) wtemp =
        world <- wtemp
        if canEditWithKeyboard () then
            let position = World.getEyeCenter3d world
            let rotation = World.getEyeRotation3d world
            let moveSpeed =
                if World.isKeyboardShiftDown world then 0.02f
                elif World.isKeyboardKeyDown KeyboardKey.Return world then 0.5f
                else 0.12f
            let turnSpeed =
                if World.isKeyboardShiftDown world then 0.025f
                else 0.05f
            if World.isKeyboardKeyDown KeyboardKey.W world then
                world <- World.setEyeCenter3d (position + Vector3.Transform (v3Forward, rotation) * moveSpeed) world
            if World.isKeyboardKeyDown KeyboardKey.S world then
                world <- World.setEyeCenter3d (position + Vector3.Transform (v3Back, rotation) * moveSpeed) world
            if World.isKeyboardKeyDown KeyboardKey.A world then
                world <- World.setEyeCenter3d (position + Vector3.Transform (v3Left, rotation) * moveSpeed) world
            if World.isKeyboardKeyDown KeyboardKey.D world then
                world <- World.setEyeCenter3d (position + Vector3.Transform (v3Right, rotation) * moveSpeed) world
            if World.isKeyboardKeyDown KeyboardKey.Q world then
                world <- World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Right, turnSpeed)) world
            if World.isKeyboardKeyDown KeyboardKey.E world then
                world <- World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Left, turnSpeed)) world
            if World.isKeyboardKeyDown KeyboardKey.Up world then
                world <- World.setEyeCenter3d (position + Vector3.Transform (v3Up, rotation) * moveSpeed) world
            if World.isKeyboardKeyDown KeyboardKey.Down world then
                world <- World.setEyeCenter3d (position + Vector3.Transform (v3Down, rotation) * moveSpeed) world
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                world <- World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Up, turnSpeed) * rotation) world
            if World.isKeyboardKeyDown KeyboardKey.Right world then
                world <- World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Down, turnSpeed) * rotation) world
            (Cascade, world)
        else (Cascade, world)

    let private handleNuRender (_ : Event<unit, Game>) wtemp =

        // render lights of the selected group in play
        world <- wtemp
        let (entities, wtemp) = World.getLightsInPlay3d (HashSet ()) world in world <- wtemp
        let lightsInGroup =
            entities |>
            Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetLight world) |>
            Seq.toArray
        for light in lightsInGroup do
            world <-
                World.enqueueRenderMessage3d
                    (RenderStaticModel
                        { Absolute = false
                          ModelMatrix = light.GetAffineMatrix world
                          Presence = Prominent
                          InsetOpt = None
                          MaterialProperties = MaterialProperties.defaultProperties
                          RenderType = ForwardRenderType (0.0f, Single.MinValue / 2.0f)
                          StaticModel = Assets.Default.LightbulbModel })
                    world

        // render selection highlights
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            let absolute = entity.GetAbsolute world
            let bounds = entity.GetHighlightBounds world
            if entity.GetIs2d world then
                let elevation = Single.MaxValue
                let transform = Transform.makePerimeter bounds v3Zero elevation absolute false
                let image = Assets.Default.HighlightImage
                world <-
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
            elif ImGui.IsCtrlPressed () then
                let mutable boundsMatrix = Matrix4x4.CreateScale (bounds.Size + v3Dup 0.01f) // slightly bigger to eye to prevent z-fighting with selected entity
                boundsMatrix.Translation <- bounds.Center
                world <-
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
        | Some _ | None -> ()

        // fin
        (Cascade, world)

    let private trySaveSelectedGroup filePath =
        try World.writeGroupToFile filePath selectedGroup world
            filePaths <- Map.add selectedGroup.GroupAddress groupFilePath filePaths
            true
        with exn ->
            messageBoxOpt <- Some ("Could not save file due to: " + scstring exn)
            false

    let private tryLoadSelectedGroup filePath =

        // ensure group isn't protected
        if not (selectedGroup.GetProtected world) then

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
                let oldWorld = world
                try
                    if group.Exists world then world <- World.destroyGroupImmediate selectedGroup world
                    let (group, wtemp) = World.readGroup groupDescriptor None selectedScreen world in world <- wtemp
                    selectedGroup <- group
                    match selectedEntityOpt with
                    | Some entity when not (entity.Exists world) -> selectedEntityOpt <- None
                    | Some _ | None -> ()
                    filePaths <- Map.add group.GroupAddress groupFilePath filePaths
                    true
                with exn ->
                    world <- World.choose oldWorld
                    messageBoxOpt <- Some ("Could not load group file due to: " + scstring exn)
                    false

            // error
            | Left exn ->
                messageBoxOpt <- Some ("Could not load group file due to: " + scstring exn)
                false

        // error
        else
            messageBoxOpt <- Some "Cannot load into a protected simulant (such as a group created by the Elmish API)."
            false

    let private createEntity atMouse inHierarchy =
        snapshot ()
        let dispatcherName = newEntityDispatcherName
        let overlayNameDescriptor =
            match newEntityOverlayName with
            | "(Default Overlay)" -> DefaultOverlay
            | "(Routed Overlay)" -> RoutedOverlay
            | "(No Overlay)" -> NoOverlay
            | overlayName -> ExplicitOverlay overlayName
        let name = generateEntityName dispatcherName
        let surnames =
            match selectedEntityOpt with
            | Some entity when entity.Exists world && inHierarchy -> Array.add name entity.Surnames
            | Some _ | None -> [|name|]
        let (entity, wtemp) = World.createEntity5 dispatcherName overlayNameDescriptor (Some surnames) selectedGroup world in world <- wtemp
        let (positionSnap, degreesSnap, scaleSnap) = getSnaps ()
        let viewport = World.getViewport world
        let mousePosition = World.getMousePosition world
        let mutable entityTransform = entity.GetTransform world
        if entity.GetIs2d world then
            let eyeCenter = World.getEyeCenter2d world
            let eyeSize = World.getEyeSize2d world
            let entityPosition =
                if atMouse
                then viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                else viewport.MouseToWorld2d (entity.GetAbsolute world, World.getEyeSize2d world, eyeCenter, eyeSize)
            entityTransform.Position <- entityPosition.V3
            entityTransform.Size <- entity.GetQuickSize world
            entityTransform.Elevation <- newEntityElevation
            if snaps2dSelected
            then world <- entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform world
            else world <- entity.SetTransform entityTransform world
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
            if not snaps2dSelected
            then world <- entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform world
            else world <- entity.SetTransform entityTransform world
        if inHierarchy then
            world <- entity.SetMountOptWithAdjustment (Some (Relation.makeParent ())) world
        match entity.TryGetProperty (nameof entity.ProbeBounds) world with
        | Some property when property.PropertyType = typeof<Box3> ->
            let bounds =
                box3
                    (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f + entity.GetPosition world)
                    (v3Dup Constants.Render.LightProbeSizeDefault)
            world <- entity.SetProbeBounds bounds world
        | Some _ | None -> ()
        selectedEntityOpt <- Some entity
        //DUMMY
        //tryShowSelectedEntityInHierarchy form

    let private tryQuickSizeSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            snapshot ()
            world <- entity.SetSize (entity.GetQuickSize world) world
            true
        | Some _ | None -> false

    let private tryDeleteSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            if not (entity.GetProtected world) then
                snapshot ()
                world <- World.destroyEntity entity world
                true
            else
                messageBoxOpt <- Some "Cannot destroy a protected simulant (such as an entity created by the Elmish API)."
                false
        | Some _ | None -> false

    let private tryCutSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            if not (entity.GetProtected world) then
                snapshot ()
                selectedEntityOpt <- None
                world <- World.cutEntityToClipboard entity world
                true
            else
                messageBoxOpt <- Some "Cannot cut a protected simulant (such as an entity created by the Elmish API)."
                false
        | Some _ | None -> false

    let private tryCopySelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            World.copyEntityToClipboard entity world
            true
        | Some _ | None -> false

    let private tryPaste atMouse =
        snapshot ()
        let surnamesOpt =
            World.tryGetEntityDispatcherNameOnClipboard world |>
            Option.map (fun dispatcherName -> generateEntityName dispatcherName) |>
            Option.map Array.singleton
        let snapsEir = if snaps2dSelected then Left snaps2d else Right snaps3d
        let (entityOpt, wtemp) = World.pasteEntityFromClipboard atMouse rightClickPosition snapsEir surnamesOpt selectedGroup world in world <- wtemp
        match entityOpt with
        | Some entity ->
            selectedEntityOpt <- Some entity
            //DUMMY
            //tryShowSelectedEntityInHierarchy form
            true
        | None -> false

    let private tryReloadAssets () =
        let assetSourceDir = targetDir + "/../../.."
        match World.tryReloadAssetGraph assetSourceDir targetDir Constants.Engine.RefinementDir world with
        | (Right assetGraph, wtemp) ->
            world <- wtemp
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            assetGraphStr <- PrettyPrinter.prettyPrint (scstring assetGraph) prettyPrinter
        | (Left error, wtemp) ->
            world <- wtemp
            messageBoxOpt <- Some ("Asset reload error due to: " + error + "'.")
            ()

    let private tryReloadCode () =
        if World.getAllowCodeReload world then
            snapshot ()
            let oldWorld = world
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
                        world <- World.updateLateBindings session.DynamicAssemblies world
                        Log.info "Code updated."
                    with _ ->
                        let error = string errorStream
                        Log.trace ("Failed to compile code due to (see full output in the console):\n" + error)
                        world <- World.choose oldWorld
            with exn ->
                Log.trace ("Failed to inspect for F# code due to: " + scstring exn)
                world <- World.choose oldWorld
        else imGuiMessage "Code reloading not allowed by current plugin. This is likely because you're using the GaiaPlugin which doesn't allow it."

    let private tryReloadAll () =
        tryReloadAssets ()
        tryReloadCode ()

    let private resetEye () =
        world <- World.setEyeCenter2d v2Zero world
        world <- World.setEyeCenter3d Constants.Engine.EyeCenter3dDefault world
        world <- World.setEyeRotation3d quatIdentity world

    let private toggleAdvancing () =
        let advancing = World.getAdvancing world
        if not advancing then snapshot ()
        world <- World.setAdvancing (not advancing) world

    let private updateEyeDrag () =
        match dragEyeState with
        | DragEyeCenter2d (entityDragOffset, mousePositionScreenOrig) ->
            let mousePositionScreen = World.getMousePosition2dScreen world
            let eyeCenter = (entityDragOffset - mousePositionScreenOrig) + -Constants.Editor.EyeSpeed * (mousePositionScreen - mousePositionScreenOrig)
            world <- World.setEyeCenter2d eyeCenter world
            dragEyeState <- DragEyeCenter2d (entityDragOffset, mousePositionScreenOrig)
        | DragEyeInactive -> ()

    let private updateEntityDrag () =
        if canEditWithMouse () then
            match dragEntityState with
            | DragEntityPosition2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityPosition = (entityDragOffset - mousePositionWorldOriginal) + (mousePositionWorld - mousePositionWorldOriginal)
                    let entityPositionSnapped =
                        if snaps2dSelected
                        then Math.snapF3d (Triple.fst (getSnaps ())) entityPosition.V3
                        else entityPosition.V3
                    let entityPosition = entity.GetPosition world
                    let entityPositionDelta = entityPositionSnapped - entityPosition
                    let entityPositionConstrained = entityPosition + entityPositionDelta
                    match Option.bind (tryResolve entity) (entity.GetMountOpt world) with
                    | Some parent ->
                        let entityPositionLocal = Vector3.Transform (entityPositionConstrained, parent.GetAffineMatrix world |> Matrix4x4.Inverse)
                        world <- entity.SetPositionLocal entityPositionLocal world
                    | None ->
                        world <- entity.SetPosition entityPositionConstrained world
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                        world <- entity.SetLinearVelocity v3Zero world
                        world <- entity.SetAngularVelocity v3Zero world
            | DragEntityRotation2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Editor.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityDegree = (entityDragOffset - mousePositionWorldOriginal.Y) + (mousePositionWorld.Y - mousePositionWorldOriginal.Y)
                    let entityDegreeSnapped =
                        if snaps2dSelected
                        then Math.snapF (Triple.snd (getSnaps ())) entityDegree
                        else entityDegree
                    let entityDegree = (entity.GetDegreesLocal world).Z
                    if entity.MountExists world then
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegreeLocal = entityDegree + entityDegreeDelta
                        world <- entity.SetDegreesLocal (v3 0.0f 0.0f entityDegreeLocal) world
                    else
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegree = entityDegree + entityDegreeDelta
                        world <- entity.SetDegrees (v3 0.0f 0.0f entityDegree) world
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                        world <- entity.SetLinearVelocity v3Zero world
                        world <- entity.SetAngularVelocity v3Zero world
            | DragEntityInactive -> ()

    let rec imGuiEntityHierarchy (entity : Entity) =
        let children = world |> entity.GetChildren |> Seq.toArray
        if ImGui.TreeNodeEx (entity.Name, if Array.notEmpty children then ImGuiTreeNodeFlags.None else ImGuiTreeNodeFlags.Leaf) then
            if ImGui.IsMouseClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                selectedEntityOpt <- Some entity
            if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                if not (entity.GetAbsolute world) then
                    if entity.GetIs2d world then
                        world <- World.setEyeCenter2d (entity.GetCenter world).V2 world
                    else
                        let eyeRotation = World.getEyeRotation3d world
                        let eyeCenterOffset = Vector3.Transform (Constants.Engine.EyeCenter3dOffset, eyeRotation)
                        world <- World.setEyeCenter3d (entity.GetPosition world + eyeCenterOffset) world
            if ImGui.BeginPopupContextItem () then
                selectedEntityOpt <- Some entity
                if ImGui.MenuItem "Cut" then tryCutSelectedEntity () |> ignore<bool>
                if ImGui.MenuItem "Copy" then tryCopySelectedEntity () |> ignore<bool>
                ImGui.Separator ()
                if ImGui.MenuItem "Delete" then tryDeleteSelectedEntity () |> ignore<bool>
                ImGui.EndPopup ()
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
                        if not (sourceEntity.GetProtected world) then
                            if ImGui.IsAltPressed () then
                                let next = Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                                let previousOpt = World.tryGetPreviousEntity next world
                                let parentOpt = match next.Parent with :? Entity as parent -> Some parent | _ -> None
                                let mountOpt = match parentOpt with Some _ -> Some (Relation.makeParent ()) | None -> None
                                let sourceEntity' = match parentOpt with Some parent -> parent / sourceEntity.Name | None -> selectedGroup / sourceEntity.Name
                                world <- World.insertEntityOrder sourceEntity previousOpt next world
                                world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                                world <- sourceEntity'.SetMountOptWithAdjustment mountOpt world
                                selectedEntityOpt <- Some sourceEntity'
                                //DUMMY
                                //tryShowSelectedEntityInHierarchy form
                            else
                                let sourceEntity' = Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames) / sourceEntity.Name
                                let mount = Relation.makeParent ()
                                world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                                world <- sourceEntity'.SetMountOptWithAdjustment (Some mount) world
                                selectedEntityOpt <- Some sourceEntity'
                                //DUMMY
                                //ImGui.SetItemOpt ()
                                //DUMMY
                                //tryShowSelectedEntityInHierarchy form
                        else messageBoxOpt <- Some "Cannot relocate a protected simulant (such as an entity created by the Elmish API)."
                    | None -> ()
            for child in children do imGuiEntityHierarchy child
            ImGui.TreePop ()

    let imGuiGetEntityProperty propertyDescriptor entity =
        EntityPropertyDescriptor.getValue propertyDescriptor entity world

    let imGuiSetEntityProperty (value : obj) propertyDescriptor entity =
        match value with
        | :? Color -> () // NOTE: color editor is draggable, which means it causes an unbounded number of snapshots currently.
        | _ -> snapshot ()
        match EntityPropertyDescriptor.trySetValue value propertyDescriptor entity world with
        | Right wtemp -> world <- wtemp
        | Left (error, wtemp) -> messageBoxOpt <- Some error; world <- wtemp

    let imGuiProcess wtemp =

        // TODO: figure out some sort of exception handling strategy for Gaia interaction.

        world <- wtemp

        updateEyeDrag ()

        updateEntityDrag ()

        let io = ImGui.GetIO ()
        if ImGui.IsKeyPressed ImGuiKey.F5 then toggleAdvancing ()
        if ImGui.IsKeyPressed ImGuiKey.F6 then editWhileAdvancing <- not editWhileAdvancing
        if ImGui.IsKeyPressed ImGuiKey.F11 then fullScreen <- not fullScreen
        if ImGui.IsKeyPressed ImGuiKey.Q && ImGui.IsCtrlPressed () then tryQuickSizeSelectedEntity () |> ignore<bool>
        if ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlPressed () then showNewGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlPressed () then showOpenGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlPressed () then showSaveGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.D && ImGui.IsCtrlPressed () then tryDeleteSelectedEntity () |> ignore<bool>
        if ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlPressed () then createEntity false false
        if not (io.WantCaptureKeyboard) then
            if ImGui.IsKeyPressed ImGuiKey.A && ImGui.IsCtrlPressed () then showSaveGroupDialog <- true
            if ImGui.IsKeyPressed ImGuiKey.Z && ImGui.IsCtrlPressed () then tryUndo () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.Y && ImGui.IsCtrlPressed () then tryRedo () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.X && ImGui.IsCtrlPressed () then tryCutSelectedEntity () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlPressed () then tryCopySelectedEntity () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.V && ImGui.IsCtrlPressed () then tryPaste false |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.Delete then tryDeleteSelectedEntity () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.Escape then selectedEntityOpt <- None

        ImGui.DockSpaceOverViewport (ImGui.GetMainViewport (), ImGuiDockNodeFlags.PassthruCentralNode) |> ignore<uint>

        ImGui.SetNextWindowPos v2Zero
        ImGui.SetNextWindowSize io.DisplaySize
        if ImGui.Begin ("Panel", ImGuiWindowFlags.NoBackground ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoInputs) then
            match selectedEntityOpt with
            | Some entity when entity.Exists world && entity.GetIs3d world ->
                let viewport = Constants.Render.Viewport
                let viewMatrix = viewport.View3d (entity.GetAbsolute world, World.getEyeCenter3d world, World.getEyeRotation3d world)
                let view = viewMatrix.ToArray ()
                let projection = (viewport.Projection3d Constants.Render.NearPlaneDistanceEnclosed Constants.Render.FarPlaneDistanceOmnipresent).ToArray ()
                let affineMatrix = (entity.GetAffineMatrix world).ToArray ()
                let operation =
                    if ImGui.IsShiftPressed () then OPERATION.SCALE
                    elif ImGui.IsAltPressed () then OPERATION.ROTATE
                    else OPERATION.TRANSLATE
                ImGuizmo.SetOrthographic false
                ImGuizmo.SetRect (0.0f, 0.0f, io.DisplaySize.X, io.DisplaySize.Y)
                ImGuizmo.SetDrawlist () // NOTE: I guess this goes right before Manipulate?
                if ImGuizmo.Manipulate (&view.[0], &projection.[0], operation, MODE.WORLD, &affineMatrix.[0]) then
                    if not manipulating && ImGui.IsMouseDown ImGuiMouseButton.Left then
                        snapshot ()
                        manipulating <- true
                    let affineMatrix' = Matrix4x4.CreateFromArray affineMatrix
                    let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                    if Matrix4x4.Decompose (affineMatrix', &scale, &rotation, &position) then
                        let (p, d, s) = if not snaps2dSelected then snaps3d else (0.0f, 0.0f, 0.0f)
                        world <- entity.SetScale (Math.snapF3d s scale) world
                        world <- entity.SetRotation rotation world
                        world <- entity.SetPosition (Math.snapF3d p position) world
                if ImGui.IsMouseReleased ImGuiMouseButton.Left then manipulating <- false
            | Some _ | None -> ()
            ImGui.End ()

        if not fullScreen then

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
                            let groups = world |> World.getGroups selectedScreen |> Set.ofSeq
                            if not (selectedGroup.GetProtected world) && Set.count groups > 1 then
                                snapshot ()
                                let groupsRemaining = Set.remove selectedGroup groups
                                selectedEntityOpt <- None
                                world <- World.destroyGroupImmediate selectedGroup world
                                filePaths <- Map.remove selectedGroup.GroupAddress filePaths
                                selectedGroup <- Seq.head groupsRemaining
                        ImGui.Separator ()
                        if ImGui.MenuItem "Exit" then world <- World.exit world
                        ImGui.EndMenu ()
                    if ImGui.BeginMenu "Edit" then
                        if ImGui.MenuItem ("Undo", "Ctrl+Z") then tryUndo () |> ignore<bool>
                        if ImGui.MenuItem ("Redo", "Ctrl+Y") then tryRedo () |> ignore<bool>
                        ImGui.Separator ()
                        if ImGui.MenuItem ("Cut", "Ctrl+X") then tryCutSelectedEntity () |> ignore<bool>
                        if ImGui.MenuItem ("Copy", "Ctrl+C") then tryCopySelectedEntity () |> ignore<bool>
                        if ImGui.MenuItem ("Paste", "Ctrl+V") then tryPaste false |> ignore<bool>
                        ImGui.Separator ()
                        if ImGui.MenuItem ("Create", "Ctrl+Enter") then createEntity false false
                        if ImGui.MenuItem ("Delete", "Delete") then tryDeleteSelectedEntity () |> ignore<bool>
                        if ImGui.MenuItem ("Quick Size", "Ctrl+Q") then tryQuickSizeSelectedEntity () |> ignore<bool>
                        ImGui.Separator ()
                        if ImGui.MenuItem ("Run/Pause", "F5") then toggleAdvancing ()
                        ImGui.EndMenu ()
                    ImGui.EndMenuBar ()
                if ImGui.Button "Create" then createEntity false false
                ImGui.SameLine ()
                ImGui.SetNextItemWidth 250.0f
                if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                    for dispatcherName in (World.getEntityDispatchers world).Keys do
                        if ImGui.Selectable (dispatcherName, strEq dispatcherName newEntityDispatcherName) then
                            newEntityDispatcherName <- dispatcherName
                    ImGui.EndCombo ()
                ImGui.SameLine ()
                ImGui.Text "w/ Overlay"
                ImGui.SameLine ()
                ImGui.SetNextItemWidth 200.0f
                let overlayNames = Array.append [|"(Default Overlay)"; "(Routed Overlay)"; "(No Overlay)"|] (World.getOverlays world |> Map.toKeyArray)
                if ImGui.BeginCombo ("##newEntityOverlayName", newEntityOverlayName) then
                    for overlayName in overlayNames do
                        if ImGui.Selectable (overlayName, strEq overlayName newEntityOverlayName) then
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
                if World.getHalted world then
                    if ImGui.Button "*Run*" then
                        snapshot ()
                        world <- World.setAdvancing true world
                else
                    if ImGui.Button "Pause" then
                        world <- World.setAdvancing false world
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
                if snaps2dSelected
                then ImGui.DragFloat ("##d", &d, 0.1f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
                else ImGui.DragFloat ("##d", &d, 0.0f, 0.0f, 0.0f, "%2.2f") |> ignore<bool> // unchangable 3d rotation
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
                ImGui.Text "Full (F11)"
                ImGui.SameLine ()
                ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                ImGui.End ()

            if ImGui.Begin "Hierarchy" then
                let groups = World.getGroups selectedScreen world
                let mutable selectedGroupName = selectedGroup.Name
                if ImGui.BeginCombo ("##selectedGroupName", selectedGroupName) then
                    for group in groups do
                        if ImGui.Selectable (group.Name, strEq group.Name selectedGroupName) then
                            selectedEntityOpt <- None
                            selectedGroup <- group
                    ImGui.EndCombo ()
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr) then
                        match dragDropPayloadOpt with
                        | Some payload ->
                            let sourceEntityAddressStr = payload
                            let sourceEntity = Entity sourceEntityAddressStr
                            if not (sourceEntity.GetProtected world) then
                                let sourceEntity' = Entity (selectedGroup.GroupAddress <-- Address.makeFromName sourceEntity.Name)
                                world <- sourceEntity.SetMountOptWithAdjustment None world
                                world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                                selectedEntityOpt <- Some sourceEntity'
                                //DUMMY
                                //tryShowSelectedEntityInHierarchy form
                        | None -> ()
                let entities =
                    World.getEntitiesSovereign selectedGroup world |>
                    Seq.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity)) |>
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
            //  AssetTag wtemp/ picking
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
                | Some entity when entity.Exists world ->
                    let propertyDescriptors = EntityPropertyDescriptor.getPropertyDescriptors entity world
                    for propertyDescriptor in propertyDescriptors do
                        let ty = propertyDescriptor.PropertyType
                        let converter = SymbolicConverter ty
                        let isPropertyAssetTag = propertyDescriptor.PropertyType.IsGenericType && propertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>
                        let value = EntityPropertyDescriptor.getValue propertyDescriptor entity world
                        let valueStr = converter.ConvertToString value
                        match value with
                        | :? bool as b -> let mutable b' = b in if ImGui.Checkbox (propertyDescriptor.PropertyName, &b') then imGuiSetEntityProperty b' propertyDescriptor entity
                        | :? int8 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then imGuiSetEntityProperty (int8 i') propertyDescriptor entity
                        | :? uint8 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then imGuiSetEntityProperty (uint8 i') propertyDescriptor entity
                        | :? int16 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then imGuiSetEntityProperty (int16 i') propertyDescriptor entity
                        | :? uint16 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then imGuiSetEntityProperty (uint16 i') propertyDescriptor entity
                        | :? int32 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then imGuiSetEntityProperty (int32 i') propertyDescriptor entity
                        | :? uint32 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then imGuiSetEntityProperty (uint32 i') propertyDescriptor entity
                        | :? int64 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then imGuiSetEntityProperty (int64 i') propertyDescriptor entity
                        | :? uint64 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then imGuiSetEntityProperty (uint64 i') propertyDescriptor entity
                        | :? single as f -> let mutable f' = single f in if ImGui.InputFloat (propertyDescriptor.PropertyName, &f') then imGuiSetEntityProperty (single f') propertyDescriptor entity
                        | :? double as f -> let mutable f' = single f in if ImGui.InputFloat (propertyDescriptor.PropertyName, &f') then imGuiSetEntityProperty (double f') propertyDescriptor entity
                        | :? Vector2 as v -> let mutable v' = v in if ImGui.InputFloat2 (propertyDescriptor.PropertyName, &v') then imGuiSetEntityProperty v' propertyDescriptor entity
                        | :? Vector3 as v -> let mutable v' = v in if ImGui.InputFloat3 (propertyDescriptor.PropertyName, &v') then imGuiSetEntityProperty v' propertyDescriptor entity
                        | :? Vector4 as v -> let mutable v' = v in if ImGui.InputFloat4 (propertyDescriptor.PropertyName, &v') then imGuiSetEntityProperty v' propertyDescriptor entity
                        | :? Vector2i as v -> let mutable v' = v in if ImGui.InputInt2 (propertyDescriptor.PropertyName, &v'.X) then imGuiSetEntityProperty v' propertyDescriptor entity
                        | :? Vector3i as v -> let mutable v' = v in if ImGui.InputInt3 (propertyDescriptor.PropertyName, &v'.X) then imGuiSetEntityProperty v' propertyDescriptor entity
                        | :? Vector4i as v -> let mutable v' = v in if ImGui.InputInt4 (propertyDescriptor.PropertyName, &v'.X) then imGuiSetEntityProperty v' propertyDescriptor entity
                        | :? Box2 as b ->
                            ImGui.Text propertyDescriptor.PropertyName
                            let mutable min = v2 b.Min.X b.Min.Y
                            let mutable size = v2 b.Size.X b.Size.Y
                            ImGui.Indent ()
                            if  ImGui.InputFloat2 ("Min", &min) ||
                                ImGui.InputFloat2 ("Size", &size) then
                                let b' = box2 min size
                                imGuiSetEntityProperty b' propertyDescriptor entity
                            ImGui.Unindent ()
                        | :? Box3 as b ->
                            ImGui.Text propertyDescriptor.PropertyName
                            let mutable min = v3 b.Min.X b.Min.Y b.Min.Z
                            let mutable size = v3 b.Size.X b.Size.Y b.Size.Z
                            ImGui.Indent ()
                            if  ImGui.InputFloat3 ("Min", &min) ||
                                ImGui.InputFloat3 ("Size", &size) then
                                let b' = box3 min size
                                imGuiSetEntityProperty b' propertyDescriptor entity
                            ImGui.Unindent ()
                        | :? Box2i as b ->
                            ImGui.Text propertyDescriptor.PropertyName
                            let mutable min = v2i b.Min.X b.Min.Y
                            let mutable size = v2i b.Size.X b.Size.Y
                            ImGui.Indent ()
                            if  ImGui.InputInt2 ("Min", &min.X) ||
                                ImGui.InputInt2 ("Size", &size.X) then
                                let b' = box2i min size
                                imGuiSetEntityProperty b' propertyDescriptor entity
                            ImGui.Unindent ()
                        | :? Quaternion as q ->
                            let mutable v = v4 q.X q.Y q.Z q.W
                            if ImGui.InputFloat4 (propertyDescriptor.PropertyName, &v) then
                                let q' = quat v.X v.Y v.Z v.W
                                imGuiSetEntityProperty q' propertyDescriptor entity
                        | :? Color as c ->
                            let mutable v = v4 c.R c.G c.B c.A
                            if ImGui.ColorEdit4 (propertyDescriptor.PropertyName, &v) then
                                let c' = color v.X v.Y v.Z v.W
                                imGuiSetEntityProperty c' propertyDescriptor entity
                        | _ when isPropertyAssetTag ->
                            let mutable valueStr' = valueStr
                            if ImGui.InputText (propertyDescriptor.PropertyName, &valueStr', 4096u) then
                                try let value' = converter.ConvertFromString valueStr'
                                    imGuiSetEntityProperty value' propertyDescriptor entity
                                with
                                | :? ParseException // TODO: use ParseException once Prime is updated.
                                | :? ConversionException -> ()
                            if ImGui.BeginDragDropTarget () then
                                if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                    match dragDropPayloadOpt with
                                    | Some payload ->
                                        try let propertyValueEscaped = payload
                                            let propertyValueUnescaped = String.unescape propertyValueEscaped
                                            let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                            imGuiSetEntityProperty propertyValue propertyDescriptor entity
                                        with
                                        | :? ParseException // TODO: use ParseException once Prime is updated.
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
                                    if ImGui.Combo (propertyDescriptor.PropertyName, &tag, caseNames, caseNames.Length) then
                                        let value' = FSharpValue.MakeUnion (cases.[tag], [||])
                                        imGuiSetEntityProperty value' propertyDescriptor entity
                            if not combo then
                                let mutable valueStr' = valueStr
                                if ImGui.InputText (propertyDescriptor.PropertyName, &valueStr', 131072u) then
                                    try let value' = converter.ConvertFromString valueStr'
                                        imGuiSetEntityProperty value' propertyDescriptor entity
                                    with
                                    | :? ParseException // TODO: use ParseException once Prime is updated.
                                    | :? ConversionException -> ()
                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some propertyDescriptor
                | Some _ | None -> ()
                ImGui.End ()

            if ImGui.Begin "Property Editor" then
                match selectedEntityOpt with
                | Some entity when entity.Exists world ->
                    match propertyDescriptorFocusedOpt with
                    | Some propertyDescriptor when propertyDescriptor.PropertyType <> typeof<ComputedProperty> ->
                        let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType)
                        match imGuiGetEntityProperty propertyDescriptor entity with
                        | null -> ()
                        | propertyValue ->
                            ImGui.Text propertyDescriptor.PropertyName
                            ImGui.SameLine ()
                            ImGui.Text ":"
                            ImGui.SameLine ()
                            ImGui.Text (Reflection.getSimplifiedTypeNameHack propertyDescriptor.PropertyType)
                            let propertyValueUnescaped = converter.ConvertToString propertyValue
                            let propertyValueEscaped = String.escape propertyValueUnescaped
                            let isPropertyAssetTag = propertyDescriptor.PropertyType.IsGenericType && propertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>
                            if isPropertyAssetTag then
                                ImGui.SameLine ()
                                if ImGui.Button "Pick" then showAssetPicker <- true
                            let mutable propertyValuePretty = PrettyPrinter.prettyPrint propertyValueEscaped PrettyPrinter.defaultPrinter
                            if ImGui.InputTextMultiline ("##propertyValuePretty", &propertyValuePretty, 131072u, v2 -1.0f -1.0f) then
                                try let propertyValueEscaped = propertyValuePretty
                                    let propertyValueUnescaped = String.unescape propertyValueEscaped
                                    let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                    imGuiSetEntityProperty propertyValue propertyDescriptor entity
                                with
                                | :? ParseException // TODO: use ParseException once Prime is updated.
                                | :? ConversionException -> ()
                            if isPropertyAssetTag then
                                if ImGui.BeginDragDropTarget () then
                                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                        match dragDropPayloadOpt with
                                        | Some payload ->
                                            try let propertyValueEscaped = payload
                                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                                let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                                imGuiSetEntityProperty propertyValue propertyDescriptor entity
                                            with
                                            | :? ParseException // TODO: use ParseException once Prime is updated.
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
                    with exn ->messageBoxOpt <- Some ("Could not save asset graph due to: " + scstring exn)
                ImGui.SameLine ()
                if ImGui.Button "Load" then
                    match AssetGraph.tryMakeFromFile (targetDir + "/" + Assets.Global.AssetGraphFilePath) with
                    | Right assetGraph ->
                        let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
                        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                        assetGraphStr <- PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
                    | Left error ->messageBoxOpt <- Some ("Could not read asset graph due to: " + error + "'.")
                ImGui.InputTextMultiline ("##assetGraphStr", &assetGraphStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
                ImGui.End ()

            if ImGui.Begin "Overlayer" then
                if ImGui.Button "Save" then
                    let overlayerSourceDir = targetDir + "/../../.."
                    let overlayerFilePath = overlayerSourceDir + "/" + Assets.Global.AssetGraphFilePath
                    try let overlays = scvalue<Overlay list> overlayerStr
                        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                        File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
                    with exn ->messageBoxOpt <- Some ("Could not save asset graph due to: " + scstring exn)
                ImGui.SameLine ()
                if ImGui.Button "Load" then
                    let overlayerFilePath = targetDir + "/" + Assets.Global.OverlayerFilePath
                    match Overlayer.tryMakeFromFile [] overlayerFilePath with
                    | Right overlayer ->
                        let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
                        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                        overlayerStr <- PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
                    | Left error ->messageBoxOpt <- Some ("Could not read overlayer due to: " + error + "'.")
                ImGui.InputTextMultiline ("##overlayerStr", &overlayerStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
                ImGui.End ()

            if ImGui.Begin "Event Tracing" then
                let mutable traceEvents = world |> World.getEventTracerOpt |> Option.isSome
                if ImGui.Checkbox ("Trace Events", &traceEvents) then
                    world <- World.setEventTracerOpt (if traceEvents then Some (Log.remark "Event") else None) world
                let eventFilter = World.getEventFilter world
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<EventFilter>).PrettyPrinter
                let mutable eventFilterStr = PrettyPrinter.prettyPrint (scstring eventFilter) prettyPrinter
                if ImGui.InputTextMultiline ("##eventFilterStr", &eventFilterStr, 131072u, v2 -1.0f -1.0f) then
                    try let eventFilter = scvalue<EventFilter> eventFilterStr
                        world <- World.setEventFilter eventFilter world
                    with _ -> ()
                ImGui.End ()

            if ImGui.Begin "Audio Player" then
                ImGui.Text "Master Sound Volume"
                let mutable masterSoundVolume = World.getMasterSoundVolume world
                if ImGui.SliderFloat ("##masterSoundVolume", &masterSoundVolume, 0.0f, 1.0f, "", ImGuiSliderFlags.Logarithmic) then world <- World.setMasterSoundVolume masterSoundVolume world
                ImGui.SameLine ()
                ImGui.Text (string masterSoundVolume)
                ImGui.Text "Master Song Volume"
                let mutable masterSongVolume = World.getMasterSongVolume world
                if ImGui.SliderFloat ("##masterSongVolume", &masterSongVolume, 0.0f, 1.0f, "", ImGuiSliderFlags.Logarithmic) then world <- World.setMasterSongVolume masterSongVolume world
                ImGui.SameLine ()
                ImGui.Text (string masterSongVolume)
                ImGui.End ()

            if ImGui.Begin "Renderer" then
                ImGui.Text "Light-Mapping (local light mapping)"
                let mutable lightMappingEnabled = lightMappingConfig.LightMappingEnabled
                ImGui.Checkbox ("Light-Mapping Enabled", &lightMappingEnabled) |> ignore<bool>
                lightMappingConfig <- { LightMappingEnabled = lightMappingEnabled }
                world <- World.enqueueRenderMessage3d (ConfigureLightMapping lightMappingConfig) world
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
                world <- World.enqueueRenderMessage3d (ConfigureSsao ssaoConfig) world
                ImGui.End ()

        else
            if ImGui.Begin "Full Screen Enabled" then
                ImGui.Text "Full Screen (F11)"
                ImGui.SameLine ()
                ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                ImGui.End ()

        if showContextMenu then
            ImGui.SetNextWindowPos rightClickPosition
            ImGui.SetNextWindowSize (v2 250.0f 135.0f)
            if ImGui.Begin ("ContextMenu", ImGuiWindowFlags.NoTitleBar) then
                if ImGui.Button "Create" then createEntity true false; showContextMenu <- false
                ImGui.SameLine ()
                ImGui.SetNextItemWidth -1.0f
                if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                    for dispatcherName in (World.getEntityDispatchers world).Keys do
                        if ImGui.Selectable (dispatcherName, strEq dispatcherName newEntityDispatcherName) then
                            newEntityDispatcherName <- dispatcherName
                            createEntity true false
                            showContextMenu <- false
                    ImGui.EndCombo ()
                if ImGui.Button "Delete" then tryDeleteSelectedEntity () |> ignore<bool>; showContextMenu <- false
                if  ImGui.IsMouseClicked ImGuiMouseButton.Right ||
                    ImGui.IsKeyPressed ImGuiKey.Escape then
                    showContextMenu <- false
                ImGui.Separator ()
                if ImGui.Button "Cut" then tryCutSelectedEntity () |> ignore<bool>; showContextMenu <- false
                if ImGui.Button "Copy" then tryCopySelectedEntity () |> ignore<bool>; showContextMenu <- false
                if ImGui.Button "Paste" then tryPaste true |> ignore<bool>; showContextMenu <- false
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
                                        | Some entity when entity.Exists world ->
                                            match propertyDescriptorFocusedOpt with
                                            | Some propertyDescriptor when propertyDescriptor.PropertyType <> typeof<ComputedProperty> ->
                                                let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType)
                                                let propertyValueStr = "[" + package.Key + " " + assetName + "]"
                                                let propertyValue = converter.ConvertFromString propertyValueStr
                                                imGuiSetEntityProperty propertyValue propertyDescriptor entity
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
                if ImGui.BeginCombo ("##newGroupDispatcherName", newGroupDispatcherName) then
                    for dispatcherName in (World.getGroupDispatchers world).Keys do
                        if ImGui.Selectable (dispatcherName, strEq dispatcherName newGroupDispatcherName) then
                            newGroupDispatcherName <- dispatcherName
                    ImGui.EndCombo ()
                let newGroup = selectedScreen / newGroupName
                if (ImGui.Button "Create" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty newGroupName && not (newGroup.Exists world) then
                    let oldWorld = world
                    try world <- World.createGroup4 newGroupDispatcherName (Some newGroupName) selectedScreen world |> snd
                        selectedGroup <- newGroup
                        showNewGroupDialog <- false
                    with exn ->
                        world <- World.choose oldWorld
                        messageBoxOpt <- Some ("Could not create group due to: " + scstring exn)
                if ImGui.IsKeyPressed ImGuiKey.Escape then showNewGroupDialog <- false

        if showOpenGroupDialog then
            let title = "Choose a nugroup file..."
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &showOpenGroupDialog) then
                ImGui.Text "File Path:"
                ImGui.SameLine ()
                ImGui.InputTextWithHint ("##groupFilePath", "[enter file path]", &groupFilePath, 4096u) |> ignore<bool>
                if (ImGui.Button "Open" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty groupFilePath then
                    snapshot ()
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
                    snapshot ()
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

        world

    let rec private runWithCleanUp targetDir' screen wtemp =
        world <- wtemp
        targetDir <- targetDir'
        selectedScreen <- screen
        selectedGroup <- Nu.World.getGroups screen world |> Seq.head
        newEntityDispatcherName <- Nu.World.getEntityDispatchers world |> Seq.head |> fun kvp -> kvp.Key
        assetGraphStr <-
            match AssetGraph.tryMakeFromFile (targetDir + "/" + Assets.Global.AssetGraphFilePath) with
            | Right assetGraph ->
                let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
            | Left error ->
                messageBoxOpt <- Some ("Could not read asset graph due to: " + error + "'.")
                ""
        overlayerStr <-
            let overlayerFilePath = targetDir + "/" + Assets.Global.OverlayerFilePath
            match Overlayer.tryMakeFromFile [] overlayerFilePath with
            | Right overlayer ->
                let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
            | Left error ->
                messageBoxOpt <- Some ("Could not read overlayer due to: " + error + "'.")
                ""
        let result =
            try World.runWithCleanUp tautology id id id imGuiProcess Live true world
            with exn ->
                //DUMMY
                //match MessageBox.Show
                //    ("Unexpected exception due to: " + scstring exn + "\nWould you like to undo the last operation to try to keep Gaia running?",
                //     "Unexpected Exception",
                //     MessageBoxButtons.YesNo,
                //     MessageBoxIcon.Error) with
                //| DialogResult.Yes ->
                //    form.undoToolStripMenuItem.PerformClick ()
                //    WORLD <- World.choose WORLD
                //    runWithCleanUp form
                //| _ -> WORLD <- World.choose WORLD
                0
        world <- Unchecked.defaultof<_>
        result

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
        let (savedState, targetDir, plugin) = selectNuPlugin gaiaPlugin
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
                let world = World.subscribe handleNuEntityContext Events.MouseRightUp Simulants.Game world
                let world = World.subscribe handleNuEntityDragBegin Events.MouseLeftDown Simulants.Game world
                let world = World.subscribe handleNuEntityDragEnd Events.MouseLeftUp Simulants.Game world
                let world = World.subscribe handleNuEyeDragBegin Events.MouseMiddleDown Simulants.Game world
                let world = World.subscribe handleNuEyeDragEnd Events.MouseMiddleUp Simulants.Game world
                let world = World.subscribe handleNuUpdate Events.Update Simulants.Game world
                let world = World.subscribe handleNuRender Events.Render Simulants.Game world
                let world = World.subscribe handleNuSelectedScreenOptChange Simulants.Game.SelectedScreenOpt.ChangeEvent Simulants.Game world
                let world = World.setMasterSongVolume 0.0f world // no song playback in editor by default
                runWithCleanUp targetDir screen world
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
        | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure