// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Reflection
open FSharp.Compiler.Interactive
open FSharp.NativeInterop
open FSharp.Reflection
open ImGuiNET
open ImGuizmoNET
open Prime
open Nu
open Nu.Gaia

///////////////////////////////////
// TODO:
//
// Group renaming.
// Collapse / Expand all in Hierarchy and Assets.
// Box3 viewport editing (w/ snapping).
// Refresh all probes button.
// Traditional close w/ Alt+F4 as well as confirmation dialog.
// View guizmo.
// Paste in hierarchy.
// Add Prelude script.
// Scripting console window.
// Hierarchical Static toggle (similar to Unity).
// Show Selected expands hierarchy as needed.
// Try to figure out how to snapshot only on first property interaction.
// File explorer dialog.
// Double-click in overlay selected and show entity in hierarchy.
// Multi-selection?
//
// Custom properties in order of priority:
//
//  option & voption with custom checkbox header
//  Enums
//  AssetTag w/ picking
//  RenderStyle
//  Substance
//  SymbolicCompression
//  TmxMap
//  LightType
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
///////////////////////////////////

[<RequireQualifiedAccess>]
module Gaia =

    (* World States - mutable to accomodatae ImGui's intended authoring style *)

    let mutable private world = Unchecked.defaultof<World> // this will be initialized on start
    let mutable private worldsPast = []
    let mutable private worldsFuture = []

    (* Active Editing States *)

    let mutable private manipulationActive = false
    let mutable private manipulationOperation = OPERATION.TRANSLATE
    let mutable private showSelectedEntity = false
    let mutable private rightClickPosition = v2Zero
    let mutable private propertyDescriptorFocusedOpt = None
    let mutable private newProjectName = "MyGame"
    let mutable private dragDropPayloadOpt = None
    let mutable private dragEntityState = DragEntityInactive
    let mutable private dragEyeState = DragEyeInactive
    let mutable private selectedScreen = Screen "Screen" // TODO: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable private selectedGroup = selectedScreen / "Group"
    let mutable private selectedEntityOpt = Option<Entity>.None

    (* Configuration States *)

    let mutable private fullScreen = false
    let mutable private editWhileAdvancing = false
    let mutable private snaps2dSelected = true
    let mutable private snaps2d = (Constants.Editor.Position2dSnapDefault, Constants.Editor.Degrees2dSnapDefault, Constants.Editor.Scale2dSnapDefault)
    let mutable private snaps3d = (Constants.Editor.Position3dSnapDefault, Constants.Editor.Degrees3dSnapDefault, Constants.Editor.Scale3dSnapDefault)
    let mutable private newGroupDispatcherName = nameof GroupDispatcher
    let mutable private newEntityDispatcherName = null // this will be initialized on start
    let mutable private newEntityOverlayName = "(Default Overlay)"
    let mutable private newEntityElevation = 0.0f
    let mutable private newGroupName = ""
    let mutable private assetViewerSearchStr = ""
    let mutable private assetPickerSearchStr = ""
    let mutable private lightMappingConfig = { LightMappingEnabled = true }
    let mutable private ssaoConfig =
        { SsaoEnabled = true
          SsaoIntensity = Constants.Render.SsaoIntensityDefault
          SsaoBias = Constants.Render.SsaoBiasDefault
          SsaoRadius = Constants.Render.SsaoRadiusDefault
          SsaoSampleCount = Constants.Render.SsaoSampleCountDefault }

    (* Project States *)

    let mutable private targetDir = "."
    let mutable private gameDllPath = ""
    let mutable private projectEditMode = "Title"
    let mutable private projectImperativeExecution = false
    let mutable private assetGraphStr = null // this will be initialized on start
    let mutable private overlayerStr = null // this will be initialized on start
    let mutable private groupFilePaths = Map.empty<Group Address, string>
    let mutable private groupFilePath = ""

    (* Modal Activity States *)

    let mutable private messageBoxOpt = Option<string>.None
    let mutable private recoverableExceptionOpt = Option<Exception * World>.None
    let mutable private showEntityContextMenu = false
    let mutable private showAssetPickerDialog = false
    let mutable private showNewProjectDialog = false
    let mutable private showOpenProjectDialog = false
    let mutable private showNewGroupDialog = false
    let mutable private showOpenGroupDialog = false
    let mutable private showSaveGroupDialog = false
    let mutable private showInspector = false

    (* Initial imgui.ini File Content *)

    let private ImGuiIniFileStr = """
[Window][Debug##Default]
Pos=60,60
Size=400,400
Collapsed=0

[Window][DockSpaceViewport_11111111]
Pos=0,0
Size=1920,1080
Collapsed=0

[Window][Panel]
Size=1920,1080
Collapsed=0

[Window][Gaia]
Pos=0,0
Size=1920,54
Collapsed=0
DockId=0x00000002,0

[Window][Hierarchy]
Pos=0,57
Size=174,1023
Collapsed=0
DockId=0x0000000B,0

[Window][Properties]
Pos=1574,58
Size=346,1022
Collapsed=0
DockId=0x00000006,0

[Window][Property Editor]
Pos=284,874
Size=712,206
Collapsed=0
DockId=0x00000001,0

[Window][Asset Viewer]
Pos=0,56
Size=282,1024
Collapsed=0
DockId=0x0000000C,1

[Window][Asset Graph]
Pos=998,874
Size=624,206
Collapsed=0
DockId=0x00000009,2

[Window][Overlayer]
Pos=998,874
Size=624,206
Collapsed=0
DockId=0x00000009,3

[Window][Event Tracing]
Pos=998,874
Size=624,206
Collapsed=0
DockId=0x00000009,4

[Window][Audio Player]
Pos=998,874
Size=624,206
Collapsed=0
DockId=0x00000009,0

[Window][Renderer]
Pos=998,874
Size=624,206
Collapsed=0
DockId=0x00000009,1

[Window][Choose a project .dll and configuration...]
Pos=754,478
Size=411,123
Collapsed=0

[Window][Choose a project .dll... EDITOR RESTART REQUIRED!]
Pos=754,478
Size=411,123
Collapsed=0

[Window][Create a group...]
Pos=715,469
Size=482,128
Collapsed=0

[Window][Full Screen Enabled]
Pos=20,23
Size=162,54
Collapsed=0

[Window][Choose a nugroup file...]
Pos=756,500
Size=403,84
Collapsed=0

[Window][Choose a project .dll... *MANUAL RESTART REQUIRED!*]
Pos=754,478
Size=411,123
Collapsed=0

[Window][Message!]
Pos=827,398
Size=360,182
Collapsed=0

[Window][Viewport]
Pos=0,0
Size=1920,1080
Collapsed=0

[Window][Entity Properties]
Pos=1624,56
Size=296,1024
Collapsed=0
DockId=0x0000000E,3

[Window][Group Properties]
Pos=1624,56
Size=296,1024
Collapsed=0
DockId=0x0000000E,2

[Window][Screen Properties]
Pos=1624,56
Size=296,1024
Collapsed=0
DockId=0x0000000E,1

[Window][Game Properties]
Pos=1624,56
Size=296,1024
Collapsed=0
DockId=0x0000000E,0

[Window][Entity Hierarchy]
Pos=0,56
Size=282,1024
Collapsed=0
DockId=0x0000000C,0

[Window][Choose a project .dll... *EDITOR RESTART REQUIRED!*]
Pos=662,475
Size=592,125
Collapsed=0

[Window][Create Nu Project... *EDITOR RESTART REQUIRED!*]
Pos=661,488
Size=621,105
Collapsed=0

[Window][Save a nugroup file...]
Pos=665,470
Size=598,134
Collapsed=0

[Window][Choose an Asset...]
Pos=796,323
Size=336,458
Collapsed=0

[Docking][Data]
DockSpace             ID=0x8B93E3BD Window=0xA787BDB4 Pos=0,0 Size=1920,1080 Split=Y
  DockNode            ID=0x00000002 Parent=0x8B93E3BD SizeRef=1920,54 HiddenTabBar=1 Selected=0x48908BE7
  DockNode            ID=0x0000000F Parent=0x8B93E3BD SizeRef=1920,1024 Split=X
    DockNode          ID=0x0000000D Parent=0x0000000F SizeRef=1622,1080 Split=X
      DockNode        ID=0x00000007 Parent=0x0000000D SizeRef=282,1080 Split=X Selected=0x29EABFBD
        DockNode      ID=0x0000000B Parent=0x00000007 SizeRef=174,1022 Selected=0x29EABFBD
        DockNode      ID=0x0000000C Parent=0x00000007 SizeRef=171,1022 Selected=0xAE464409
      DockNode        ID=0x00000008 Parent=0x0000000D SizeRef=1338,1080 Split=X
        DockNode      ID=0x00000005 Parent=0x00000008 SizeRef=1223,979 Split=Y
          DockNode    ID=0x00000004 Parent=0x00000005 SizeRef=1678,816 CentralNode=1
          DockNode    ID=0x00000003 Parent=0x00000005 SizeRef=1678,206 Split=X Selected=0xD4E24632
            DockNode  ID=0x00000001 Parent=0x00000003 SizeRef=712,205 Selected=0x61D81DE4
            DockNode  ID=0x00000009 Parent=0x00000003 SizeRef=624,205 Selected=0xD4E24632
        DockNode      ID=0x00000006 Parent=0x00000008 SizeRef=346,979 Selected=0x199AB496
    DockNode          ID=0x0000000E Parent=0x0000000F SizeRef=296,1080 Selected=0xD5116FF8

"""

    (* Prelude Functions *)

    let private canEditWithMouse () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureMouse) && (World.getHalted world || editWhileAdvancing)

    let private canEditWithKeyboard () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureKeyboard) && (World.getHalted world || editWhileAdvancing)

    let private selectScreen screen =
        if screen <> selectedScreen then
            ImGui.SetWindowFocus null
            selectedScreen <- screen

    let private selectGroup group =
        if group <> selectedGroup then
            ImGui.SetWindowFocus null
            selectedGroup <- group

    let private selectEntityOpt entityOpt =

        // HACK: in order to keep the property of one simulant from copied to another simulant when the selected
        // simulant is changed, we have to move focus away from the property windows. We chose to focus on the
        // "Entity Hierarchy" window in order to avoid disrupting drag and drop when selecting a different entity
        // in it.
        if entityOpt <> selectedEntityOpt then
            ImGui.SetWindowFocus "Entity Hierarchy"

        // actually set the selection
        selectedEntityOpt <- entityOpt

    let private snapshot () =
        world <- Nu.World.shelve world
        worldsPast <- world :: worldsPast
        worldsFuture <- []

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
                selectEntityOpt None
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
                selectEntityOpt None
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

    let private tryMousePick mousePosition =
        let entities2d = getPickableEntities2d ()
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d world
        match pickedOpt with
        | Some entity ->
            selectEntityOpt (Some entity)
            Some (0.0f, entity)
        | None ->
            let entities3d = getPickableEntities3d ()
            let pickedOpt = World.tryPickEntity3d mousePosition entities3d world
            match pickedOpt with
            | Some (intersection, entity) ->
                selectEntityOpt (Some entity)
                Some (intersection, entity)
            | None -> None

    (* Nu Event Handlers *)

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
            selectEntityOpt None
            selectGroup group
            selectScreen screen
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
            showEntityContextMenu <- true
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

    (* Editor Commands *)

    let private getProperty propertyDescriptor simulant =
        SimulantPropertyDescriptor.getValue propertyDescriptor simulant world

    let private setPropertyWithoutUndo (value : obj) propertyDescriptor simulant =
        match SimulantPropertyDescriptor.trySetValue value propertyDescriptor simulant world with
        | Right wtemp -> world <- wtemp
        | Left (error, wtemp) -> messageBoxOpt <- Some error; world <- wtemp

    let private setProperty (value : obj) propertyDescriptor simulant =
        snapshot ()
        setPropertyWithoutUndo value propertyDescriptor simulant

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
        selectEntityOpt (Some entity)
        showSelectedEntity <- true

    let private trySaveSelectedGroup filePath =
        try World.writeGroupToFile filePath selectedGroup world
            groupFilePaths <- Map.add selectedGroup.GroupAddress groupFilePath groupFilePaths
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
                try if group.Exists world then
                        world <- World.destroyGroupImmediate selectedGroup world
                    let (group, wtemp) = World.readGroup groupDescriptor None selectedScreen world in world <- wtemp
                    selectGroup group
                    match selectedEntityOpt with
                    | Some entity when not (entity.Exists world) -> selectEntityOpt None
                    | Some _ | None -> ()
                    groupFilePaths <- Map.add group.GroupAddress groupFilePath groupFilePaths
                    groupFilePath <- ""
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

    let private tryQuickSizeSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            snapshot ()
            world <- entity.SetSize (entity.GetQuickSize world) world
            true
        | Some _ | None -> false

    let private tryReorderSelectedEntity up =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            let peerOpt =
                if up
                then World.tryGetPreviousEntity entity world
                else World.tryGetNextEntity entity world
            match peerOpt with
            | Some peer ->
                if not (entity.GetProtected world) && not (peer.GetProtected world)
                then world <- World.swapEntityOrders entity peer world
                else messageBoxOpt <- Some "Cannot reorder a protected simulant (such as an entity created by the Elmish API)."
            | None -> ()
        | Some _ | None -> ()

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
                selectEntityOpt None
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
            selectEntityOpt (Some entity)
            showSelectedEntity <- true
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
            selectEntityOpt None // NOTE: makes sure old dispatcher doesn't hang around in old cached entity state.
            let workingDirPath = targetDir + "/../../.."
            Log.info ("Inspecting directory " + workingDirPath + " for F# code...")
            try match Array.ofSeq (Directory.EnumerateFiles (workingDirPath, "*.fsproj")) with
                | [||] -> Log.trace ("Unable to find fsproj file in '" + workingDirPath + "'.")
                | fsprojFilePaths ->
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
                        ["#r \"../../../../../Nu/Nu.Math/bin/" + Constants.Editor.BuildName + "/netstandard2.0/Nu.Math.dll\""
                         "#r \"../../../../../Nu/Nu.Pipe/bin/" + Constants.Editor.BuildName + "/net7.0/Nu.Pipe.dll\""
                         "#r \"../../../../../Nu/Nu/bin/" + Constants.Editor.BuildName + "/net7.0/Nu.dll\""]
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
        else messageBoxOpt <- Some "Code reloading not allowed by current plugin. This is likely because you're using the GaiaPlugin which doesn't allow it."

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

    let private trySelectTargetDirAndMakeNuPluginFromFilePathOpt filePathOpt =
        let gaiaDir = Directory.GetCurrentDirectory ()
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
                with _ ->
                    Directory.SetCurrentDirectory gaiaDir
                    Left ()
            else Right None
        match filePathAndDirNameAndTypesOpt with
        | Right (Some (filePath, dirName, types)) ->
            let pluginTypeOpt = Array.tryFind (fun (ty : Type) -> ty.IsSubclassOf typeof<NuPlugin>) types
            match pluginTypeOpt with
            | Some ty ->
                let plugin = Activator.CreateInstance ty :?> NuPlugin
                Right (Some (filePath, dirName, plugin))
            | None ->
                Directory.SetCurrentDirectory gaiaDir
                Left ()
        | Right None -> Right None
        | Left () -> Left ()

    let private selectNuPlugin gaiaPlugin =
        let savedState =
            try if File.Exists Constants.Editor.SavedStateFilePath
                then scvalue (File.ReadAllText Constants.Editor.SavedStateFilePath)
                else SavedState.defaultState
            with _ -> SavedState.defaultState
        let gaiaDirectory = Directory.GetCurrentDirectory ()
        match trySelectTargetDirAndMakeNuPluginFromFilePathOpt savedState.ProjectFilePath with
        | Right (Some (filePath, targetDir, plugin)) ->
            Constants.Override.fromAppConfig filePath
            try File.WriteAllText (gaiaDirectory + "/" + Constants.Editor.SavedStateFilePath, scstring savedState)
            with _ -> Log.info "Could not save editor state."
            (savedState, targetDir, plugin)
        | Right None ->
            try File.WriteAllText (gaiaDirectory + "/" + Constants.Editor.SavedStateFilePath, scstring savedState)
            with _ -> Log.info "Could not save editor state."
            (savedState, ".", gaiaPlugin)
        | Left () ->
            if not (String.IsNullOrWhiteSpace savedState.ProjectFilePath) then
                Log.trace ("Invalid Nu Assembly: " + savedState.ProjectFilePath)
            (SavedState.defaultState, ".", gaiaPlugin)

    (* ImGui Callback Functions *)

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

    let private updateHotkeys () =
        let io = ImGui.GetIO ()
        if ImGui.IsKeyPressed ImGuiKey.F5 then toggleAdvancing ()
        if ImGui.IsKeyPressed ImGuiKey.F6 then editWhileAdvancing <- not editWhileAdvancing
        if ImGui.IsKeyPressed ImGuiKey.F11 then fullScreen <- not fullScreen
        if ImGui.IsKeyPressed ImGuiKey.Q && ImGui.IsCtrlPressed () then tryQuickSizeSelectedEntity () |> ignore<bool>
        if ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlPressed () then showNewGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlPressed () then showOpenGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlPressed () then showSaveGroupDialog <- true
        if ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlPressed () then createEntity false false
        if ImGui.IsKeyPressed ImGuiKey.UpArrow && ImGui.IsAltPressed () then tryReorderSelectedEntity true
        if ImGui.IsKeyPressed ImGuiKey.DownArrow && ImGui.IsAltPressed () then tryReorderSelectedEntity false
        if not (io.WantCaptureKeyboard) then
            if ImGui.IsKeyPressed ImGuiKey.Z && ImGui.IsCtrlPressed () then tryUndo () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.Y && ImGui.IsCtrlPressed () then tryRedo () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.X && ImGui.IsCtrlPressed () then tryCutSelectedEntity () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlPressed () then tryCopySelectedEntity () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.V && ImGui.IsCtrlPressed () then tryPaste false |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.Delete then tryDeleteSelectedEntity () |> ignore<bool>
            if ImGui.IsKeyPressed ImGuiKey.Escape then selectEntityOpt None

    let rec private imGuiEntityHierarchy (entity : Entity) =
        let children = world |> entity.GetChildren |> Seq.toArray
        let selected = match selectedEntityOpt with Some selectedEntity -> entity = selectedEntity | None -> false
        let treeNodeFlags =
            (if selected then ImGuiTreeNodeFlags.Selected else ImGuiTreeNodeFlags.None) |||
            (if Array.isEmpty children then ImGuiTreeNodeFlags.Leaf else ImGuiTreeNodeFlags.None) |||
            ImGuiTreeNodeFlags.SpanAvailWidth ||| ImGuiTreeNodeFlags.OpenOnArrow
        let expanded = ImGui.TreeNodeEx (entity.Name, treeNodeFlags)
        if showSelectedEntity && selectedEntityOpt = Some entity then
            ImGui.SetScrollHereY ()
            showSelectedEntity <- false
        if ImGui.IsMouseClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            selectEntityOpt (Some entity)
        if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            if not (entity.GetAbsolute world) then
                if entity.GetIs2d world then
                    world <- World.setEyeCenter2d (entity.GetCenter world).V2 world
                else
                    let eyeRotation = World.getEyeRotation3d world
                    let eyeCenterOffset = Vector3.Transform (Constants.Engine.EyeCenter3dOffset, eyeRotation)
                    world <- World.setEyeCenter3d (entity.GetPosition world + eyeCenterOffset) world
        if ImGui.BeginPopupContextItem () then
            selectEntityOpt (Some entity)
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
                            selectEntityOpt (Some sourceEntity')
                            showSelectedEntity <- true
                        else
                            let sourceEntity' = Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames) / sourceEntity.Name
                            let mount = Relation.makeParent ()
                            world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                            world <- sourceEntity'.SetMountOptWithAdjustment (Some mount) world
                            selectEntityOpt (Some sourceEntity')
                            showSelectedEntity <- true
                    else messageBoxOpt <- Some "Cannot relocate a protected simulant (such as an entity created by the Elmish API)."
                | None -> ()
        if expanded then
            for child in children do imGuiEntityHierarchy child
            ImGui.TreePop ()

    let private imGuiEditProperties (simulant : Simulant) =
        let mutable simulant = simulant
        let propertyDescriptors = world |> SimulantPropertyDescriptor.getPropertyDescriptors simulant |> Array.ofList
        let propertyDescriptorses = propertyDescriptors |> Array.groupBy EntityPropertyDescriptor.getCategory |> Map.ofSeq
        for (propertyCategory, propertyDescriptors) in propertyDescriptorses.Pairs do
            if ImGui.CollapsingHeader (propertyCategory, ImGuiTreeNodeFlags.DefaultOpen ||| ImGuiTreeNodeFlags.OpenOnArrow) then
                let propertyDescriptors =
                    propertyDescriptors |>
                    Array.filter (fun pd -> SimulantPropertyDescriptor.getEditable pd simulant) |>
                    Array.sortBy (fun pd -> pd.PropertyName)
                for propertyDescriptor in propertyDescriptors do
                    if propertyDescriptor.PropertyName = Constants.Engine.NamePropertyName then
                        match simulant with
                        | :? Entity as entity ->
                            // NOTE: name edit properties can't be replaced.
                            let mutable name = entity.Name
                            let flags = if entity.GetProtected world then ImGuiInputTextFlags.ReadOnly else ImGuiInputTextFlags.None
                            if ImGui.InputText ("Name", &name, 4096u, flags) then
                                setProperty name propertyDescriptor simulant
                                let entity' = Entity (Array.add name entity.Parent.SimulantAddress.Names)
                                selectEntityOpt (Some entity')
                                simulant <- entity'
                        | :? Group as group ->
                            // NOTE: only edit entities names for now.
                            let mutable name = group.Name
                            ImGui.InputText ("Name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                        | :? Screen as screen ->
                            // NOTE: only edit entities names for now.
                            let mutable name = screen.Name
                            ImGui.InputText ("Name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                        | _ -> ()
                        // NOTE: don't edit names in property editor for now.
                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- None
                    else
                        let mutable replaced = false
                        let replaceProperty =
                            ReplaceProperty
                                { Snapshot = fun world -> snapshot (); world
                                  IndicateReplaced = fun world -> replaced <- true; world
                                  PropertyDescriptor = propertyDescriptor }
                        world <- World.edit replaceProperty simulant world
                        if not replaced then
                            let ty = propertyDescriptor.PropertyType
                            let converter = SymbolicConverter ty
                            let isPropertyAssetTag = propertyDescriptor.PropertyType.IsGenericType && propertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>
                            let value = SimulantPropertyDescriptor.getValue propertyDescriptor simulant world
                            let valueStr = converter.ConvertToString value
                            match value with
                            | :? Frustum -> () // TODO: implement FrustumConverter.
                            | :? bool as b -> let mutable b' = b in if ImGui.Checkbox (propertyDescriptor.PropertyName, &b') then setProperty b' propertyDescriptor simulant
                            | :? int8 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then setProperty (int8 i') propertyDescriptor simulant
                            | :? uint8 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then setProperty (uint8 i') propertyDescriptor simulant
                            | :? int16 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then setProperty (int16 i') propertyDescriptor simulant
                            | :? uint16 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then setProperty (uint16 i') propertyDescriptor simulant
                            | :? int32 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then setProperty (int32 i') propertyDescriptor simulant
                            | :? uint32 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then setProperty (uint32 i') propertyDescriptor simulant
                            | :? int64 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then setProperty (int64 i') propertyDescriptor simulant
                            | :? uint64 as i -> let mutable i' = int32 i in if ImGui.InputInt (propertyDescriptor.PropertyName, &i') then setProperty (uint64 i') propertyDescriptor simulant
                            | :? single as f -> let mutable f' = single f in if ImGui.InputFloat (propertyDescriptor.PropertyName, &f') then setProperty (single f') propertyDescriptor simulant
                            | :? double as f -> let mutable f' = single f in if ImGui.InputFloat (propertyDescriptor.PropertyName, &f') then setProperty (double f') propertyDescriptor simulant
                            | :? Vector2 as v -> let mutable v' = v in if ImGui.InputFloat2 (propertyDescriptor.PropertyName, &v') then setProperty v' propertyDescriptor simulant
                            | :? Vector3 as v -> let mutable v' = v in if ImGui.InputFloat3 (propertyDescriptor.PropertyName, &v') then setProperty v' propertyDescriptor simulant
                            | :? Vector4 as v -> let mutable v' = v in if ImGui.InputFloat4 (propertyDescriptor.PropertyName, &v') then setProperty v' propertyDescriptor simulant
                            | :? Vector2i as v -> let mutable v' = v in if ImGui.InputInt2 (propertyDescriptor.PropertyName, &v'.X) then setProperty v' propertyDescriptor simulant
                            | :? Vector3i as v -> let mutable v' = v in if ImGui.InputInt3 (propertyDescriptor.PropertyName, &v'.X) then setProperty v' propertyDescriptor simulant
                            | :? Vector4i as v -> let mutable v' = v in if ImGui.InputInt4 (propertyDescriptor.PropertyName, &v'.X) then setProperty v' propertyDescriptor simulant
                            | :? Box2 as b ->
                                ImGui.Text propertyDescriptor.PropertyName
                                let mutable min = v2 b.Min.X b.Min.Y
                                let mutable size = v2 b.Size.X b.Size.Y
                                ImGui.Indent ()
                                if  ImGui.InputFloat2 ("Min", &min) ||
                                    ImGui.InputFloat2 ("Size", &size) then
                                    let b' = box2 min size
                                    setProperty b' propertyDescriptor simulant
                                ImGui.Unindent ()
                            | :? Box3 as b ->
                                ImGui.Text propertyDescriptor.PropertyName
                                let mutable min = v3 b.Min.X b.Min.Y b.Min.Z
                                let mutable size = v3 b.Size.X b.Size.Y b.Size.Z
                                ImGui.Indent ()
                                if  ImGui.InputFloat3 ("Min", &min) ||
                                    ImGui.InputFloat3 ("Size", &size) then
                                    let b' = box3 min size
                                    setProperty b' propertyDescriptor simulant
                                ImGui.Unindent ()
                            | :? Box2i as b ->
                                ImGui.Text propertyDescriptor.PropertyName
                                let mutable min = v2i b.Min.X b.Min.Y
                                let mutable size = v2i b.Size.X b.Size.Y
                                ImGui.Indent ()
                                if  ImGui.InputInt2 ("Min", &min.X) ||
                                    ImGui.InputInt2 ("Size", &size.X) then
                                    let b' = box2i min size
                                    setProperty b' propertyDescriptor simulant
                                ImGui.Unindent ()
                            | :? Quaternion as q ->
                                let mutable v = v4 q.X q.Y q.Z q.W
                                if ImGui.InputFloat4 (propertyDescriptor.PropertyName, &v) then
                                    let q' = quat v.X v.Y v.Z v.W
                                    setProperty q' propertyDescriptor simulant
                            | :? Color as c ->
                                let mutable v = v4 c.R c.G c.B c.A
                                if ImGui.ColorEdit4 (propertyDescriptor.PropertyName, &v) then
                                    let c' = color v.X v.Y v.Z v.W
                                    setPropertyWithoutUndo c' propertyDescriptor simulant
                            | :? MaterialProperties as mp ->

                                let mutable isSome = ValueOption.isSome mp.AlbedoOpt
                                if ImGui.Checkbox ((if isSome then "##mpAlbedoIsSome" else "AlbedoOpt"), &isSome) then
                                    if isSome
                                    then setProperty { mp with AlbedoOpt = ValueSome Constants.Render.AlbedoDefault } propertyDescriptor simulant
                                    else setProperty { mp with AlbedoOpt = ValueNone } propertyDescriptor simulant
                                else
                                    match mp.AlbedoOpt with
                                    | ValueSome albedo ->
                                        let mutable v = v4 albedo.R albedo.G albedo.B albedo.A
                                        ImGui.SameLine ()
                                        if ImGui.ColorEdit4 ("AlbedoOpt", &v) then setProperty { mp with AlbedoOpt = ValueSome (color v.X v.Y v.Z v.W) } propertyDescriptor simulant
                                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)
                                    | ValueNone -> ()
                                if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)

                                let mutable isSome = ValueOption.isSome mp.MetallicOpt
                                if ImGui.Checkbox ((if isSome then "##mpMetallicIsSome" else "MetallicOpt"), &isSome) then
                                    if isSome
                                    then setProperty { mp with MetallicOpt = ValueSome Constants.Render.MetallicDefault } propertyDescriptor simulant
                                    else setProperty { mp with MetallicOpt = ValueNone } propertyDescriptor simulant
                                else
                                    match mp.MetallicOpt with
                                    | ValueSome metallic ->
                                        let mutable metallic = metallic
                                        ImGui.SameLine ()
                                        if ImGui.DragFloat ("MetallicOpt", &metallic, 0.005f, 0.0f, 10.0f) then setProperty { mp with MetallicOpt = ValueSome metallic } propertyDescriptor simulant
                                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)
                                    | ValueNone -> ()
                                if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)

                                let mutable isSome = ValueOption.isSome mp.RoughnessOpt
                                if ImGui.Checkbox ((if isSome then "##mpRoughnessIsSome" else "RoughnessOpt"), &isSome) then
                                    if isSome
                                    then setProperty { mp with RoughnessOpt = ValueSome Constants.Render.RoughnessDefault } propertyDescriptor simulant
                                    else setProperty { mp with RoughnessOpt = ValueNone } propertyDescriptor simulant
                                else
                                    match mp.RoughnessOpt with
                                    | ValueSome roughness ->
                                        let mutable roughness = roughness
                                        ImGui.SameLine ()
                                        if ImGui.DragFloat ("RoughnessOpt", &roughness, 0.005f, 0.0f, 10.0f) then setProperty { mp with RoughnessOpt = ValueSome roughness } propertyDescriptor simulant
                                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)
                                    | ValueNone -> ()
                                if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)

                                let mutable isSome = ValueOption.isSome mp.EmissionOpt
                                if ImGui.Checkbox ((if isSome then "##mpEmissionIsSome" else "EmissionOpt"), &isSome) then
                                    if isSome
                                    then setProperty { mp with EmissionOpt = ValueSome Constants.Render.EmissionDefault } propertyDescriptor simulant
                                    else setProperty { mp with EmissionOpt = ValueNone } propertyDescriptor simulant
                                else
                                    match mp.EmissionOpt with
                                    | ValueSome emission ->
                                        let mutable emission = emission
                                        ImGui.SameLine ()
                                        if ImGui.DragFloat ("EmissionOpt", &emission, 0.005f, 0.0f, 10.0f) then setProperty { mp with EmissionOpt = ValueSome emission } propertyDescriptor simulant
                                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)
                                    | ValueNone -> ()
                                if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)

                                let mutable isSome = ValueOption.isSome mp.HeightOpt
                                if ImGui.Checkbox ((if isSome then "##mpHeightIsSome" else "HeightOpt"), &isSome) then
                                    if isSome
                                    then setProperty { mp with HeightOpt = ValueSome Constants.Render.HeightDefault } propertyDescriptor simulant
                                    else setProperty { mp with HeightOpt = ValueNone } propertyDescriptor simulant
                                else
                                    match mp.HeightOpt with
                                    | ValueSome height ->
                                        let mutable height = height
                                        ImGui.SameLine ()
                                        if ImGui.DragFloat ("HeightOpt", &height, 0.005f, 0.0f, 10.0f) then setProperty { mp with HeightOpt = ValueSome height } propertyDescriptor simulant
                                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)
                                    | ValueNone -> ()
                                if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)

                                let mutable isSome = ValueOption.isSome mp.InvertRoughnessOpt
                                if ImGui.Checkbox ((if isSome then "##mpInvertRoughnessIsSome" else "InvertRoughnessOpt"), &isSome) then
                                    if isSome
                                    then setProperty { mp with InvertRoughnessOpt = ValueSome Constants.Render.InvertRoughnessDefault } propertyDescriptor simulant
                                    else setProperty { mp with InvertRoughnessOpt = ValueNone } propertyDescriptor simulant
                                else
                                    match mp.InvertRoughnessOpt with
                                    | ValueSome invertRoughness ->
                                        let mutable invertRoughness = invertRoughness
                                        ImGui.SameLine ()
                                        if ImGui.Checkbox ("InvertRoughnessOpt", &invertRoughness) then setProperty { mp with InvertRoughnessOpt = ValueSome invertRoughness } propertyDescriptor simulant
                                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)
                                    | ValueNone -> ()
                                if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)

                            | _ when isPropertyAssetTag ->
                                let mutable valueStr' = valueStr
                                if ImGui.InputText (propertyDescriptor.PropertyName, &valueStr', 4096u) then
                                    try let value' = converter.ConvertFromString valueStr'
                                        setProperty value' propertyDescriptor simulant
                                    with :? ParseException | :? ConversionException -> ()
                                if ImGui.BeginDragDropTarget () then
                                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                        match dragDropPayloadOpt with
                                        | Some payload ->
                                            try let propertyValueEscaped = payload
                                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                                let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                                setProperty propertyValue propertyDescriptor simulant
                                            with :? ParseException | :? ConversionException -> ()
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
                                            setProperty value' propertyDescriptor simulant
                                if not combo then
                                    let mutable valueStr' = valueStr
                                    if ImGui.InputText (propertyDescriptor.PropertyName, &valueStr', 131072u) then
                                        try let value' = converter.ConvertFromString valueStr'
                                            setProperty value' propertyDescriptor simulant
                                        with :? ParseException | :? ConversionException -> ()
                        if ImGui.IsItemFocused () then propertyDescriptorFocusedOpt <- Some (propertyDescriptor, simulant)
        world <- World.edit AppendProperties simulant world

    let private imGuiProcess wtemp =

        // transfer to world mutation mode
        world <- wtemp

        // enable global docking
        ImGui.DockSpaceOverViewport (ImGui.GetMainViewport (), ImGuiDockNodeFlags.PassthruCentralNode) |> ignore<uint>

        // attempt to proceed with normal operation
        match recoverableExceptionOpt with
        | None ->

            // use a generalized exception process
            try

                // process non-widget specific input
                updateEyeDrag ()
                updateEntityDrag ()
                updateHotkeys ()

                // viewport interaction
                let io = ImGui.GetIO ()
                ImGui.SetNextWindowPos v2Zero
                ImGui.SetNextWindowSize io.DisplaySize
                if ImGui.Begin ("Viewport", ImGuiWindowFlags.NoBackground ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoInputs) then
                    match selectedEntityOpt with
                    | Some entity when entity.Exists world && entity.GetIs3d world ->
                        let viewport = Constants.Render.Viewport
                        let viewMatrix = viewport.View3d (entity.GetAbsolute world, World.getEyeCenter3d world, World.getEyeRotation3d world)
                        let view = viewMatrix.ToArray ()
                        let projectionMatrix = viewport.Projection3d Constants.Render.NearPlaneDistanceEnclosed Constants.Render.FarPlaneDistanceOmnipresent
                        let projection = projectionMatrix.ToArray ()
                        let affineMatrix = entity.GetAffineMatrix world
                        let affine = affineMatrix.ToArray ()
                        ImGuizmo.SetOrthographic false
                        ImGuizmo.SetRect (0.0f, 0.0f, io.DisplaySize.X, io.DisplaySize.Y)
                        ImGuizmo.SetDrawlist () // NOTE: I guess this goes right before Manipulate?
                        if not manipulationActive then
                            if ImGui.IsShiftPressed () then manipulationOperation <- OPERATION.SCALE
                            elif ImGui.IsAltPressed () then manipulationOperation <- OPERATION.ROTATE
                            else manipulationOperation <- OPERATION.TRANSLATE
                        if ImGuizmo.Manipulate (&view.[0], &projection.[0], manipulationOperation, MODE.WORLD, &affine.[0]) then
                            if not manipulationActive && ImGui.IsMouseDown ImGuiMouseButton.Left then
                                snapshot ()
                                manipulationActive <- true
                            let affine' = Matrix4x4.CreateFromArray affine
                            let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                            if Matrix4x4.Decompose (affine', &scale, &rotation, &position) then
                                let (p, _, s) = if not snaps2dSelected then snaps3d else (0.0f, 0.0f, 0.0f)
                                scale <- Math.snapF3d s scale
                                if scale.X < 0.01f then scale.X <- 0.01f
                                if scale.Y < 0.01f then scale.Y <- 0.01f
                                if scale.Z < 0.01f then scale.Z <- 0.01f
                                position <- Math.snapF3d p position
                                match manipulationOperation with
                                | OPERATION.SCALE -> world <- entity.SetScale scale world
                                | OPERATION.ROTATE -> world <- entity.SetRotation rotation world
                                | OPERATION.TRANSLATE -> world <- entity.SetPosition position world
                                | _ -> () // nothing to do
                            if World.getAdvancing world then
                                match entity.TryGetProperty (nameof entity.LinearVelocity) world with
                                | Some property when property.PropertyType = typeof<Vector3> -> world <- entity.SetLinearVelocity v3Zero world
                                | Some _ | None -> ()
                                match entity.TryGetProperty (nameof entity.AngularVelocity) world with
                                | Some property when property.PropertyType = typeof<Vector3> -> world <- entity.SetAngularVelocity v3Zero world
                                | Some _ | None -> ()
                        let operation =
                            OverlayViewport
                                { Snapshot = fun world -> snapshot (); world
                                  ViewportView = viewMatrix
                                  ViewportProjection = projectionMatrix
                                  ViewportBounds = box2 v2Zero io.DisplaySize }
                        world <- World.editEntity operation entity world
                        if ImGui.IsMouseReleased ImGuiMouseButton.Left then manipulationActive <- false
                    | Some _ | None -> ()
                    ImGui.End ()

                // show all windows when out in full-screen mode
                if not fullScreen then

                    // main menu window
                    if ImGui.Begin ("Gaia", ImGuiWindowFlags.MenuBar) then
                        if ImGui.BeginMenuBar () then
                            if ImGui.BeginMenu "File" then
                                if ImGui.MenuItem ("New Project") then
                                    showNewProjectDialog <- true
                                if ImGui.MenuItem ("Open Project") then
                                    showOpenProjectDialog <- true
                                ImGui.Separator ()
                                if ImGui.MenuItem ("New Group", "Ctrl+N") then
                                    showNewGroupDialog <- true
                                if ImGui.MenuItem ("Open Group", "Ctrl+O") then
                                    showOpenGroupDialog <- true
                                if ImGui.MenuItem ("Save Group", "Ctrl+S") then
                                    match Map.tryFind selectedGroup.GroupAddress groupFilePaths with
                                    | Some filePath -> groupFilePath <- filePath
                                    | None -> groupFilePath <- ""
                                    showSaveGroupDialog <- true
                                if ImGui.MenuItem "Close Group" then
                                    let groups = world |> World.getGroups selectedScreen |> Set.ofSeq
                                    if not (selectedGroup.GetProtected world) && Set.count groups > 1 then
                                        snapshot ()
                                        let groupsRemaining = Set.remove selectedGroup groups
                                        selectEntityOpt None
                                        world <- World.destroyGroupImmediate selectedGroup world
                                        groupFilePaths <- Map.remove selectedGroup.GroupAddress groupFilePaths
                                        selectGroup (Seq.head groupsRemaining)
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
                        ImGui.SetNextItemWidth 200.0f
                        if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                            for dispatcherName in (World.getEntityDispatchers world).Keys do
                                if ImGui.Selectable (dispatcherName, strEq dispatcherName newEntityDispatcherName) then
                                    newEntityDispatcherName <- dispatcherName
                            ImGui.EndCombo ()
                        ImGui.SameLine ()
                        ImGui.Text "w/ Overlay"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 150.0f
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
                        ImGui.Text "Mode:"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 150.0f
                        if ImGui.BeginCombo ("##projectEditMode", projectEditMode) then
                            let editModes = World.getEditModes world
                            for editMode in editModes do
                                if ImGui.Selectable (editMode.Key, strEq editMode.Key projectEditMode) then
                                    projectEditMode <- editMode.Key
                                    snapshot () // snapshot before mode change
                                    selectEntityOpt None
                                    world <- editMode.Value world
                                    snapshot () // snapshot before after change
                            ImGui.EndCombo ()
                        ImGui.SameLine ()
                        ImGui.Text "Full (F11)"
                        ImGui.SameLine ()
                        ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                        ImGui.End ()

                    // asset viewer window
                    if ImGui.Begin "Asset Viewer" then
                        ImGui.SetNextItemWidth -1.0f
                        ImGui.InputTextWithHint ("##assetViewerSearchStr", "[enter search text]", &assetViewerSearchStr, 4096u) |> ignore<bool>
                        let assets = Metadata.getDiscoveredAssets ()
                        for package in assets do
                            let flags = ImGuiTreeNodeFlags.SpanAvailWidth ||| ImGuiTreeNodeFlags.OpenOnArrow
                            if ImGui.TreeNodeEx (package.Key, flags) then
                                for assetName in package.Value do
                                    if (assetName.ToLowerInvariant ()).Contains (assetViewerSearchStr.ToLowerInvariant ()) then
                                        ImGui.TreeNodeEx (assetName, flags ||| ImGuiTreeNodeFlags.Leaf) |> ignore<bool>
                                        if ImGui.BeginDragDropSource () then
                                            let assetTagStr = "[" + package.Key + " " + assetName + "]"
                                            dragDropPayloadOpt <- Some assetTagStr
                                            ImGui.Text assetTagStr
                                            ImGui.SetDragDropPayload ("Asset", IntPtr.Zero, 0u) |> ignore<bool>
                                            ImGui.EndDragDropSource ()
                                        ImGui.TreePop ()
                                ImGui.TreePop ()
                        ImGui.End ()

                    // entity hierarchy window
                    if ImGui.Begin "Entity Hierarchy" then
                        if ImGui.Button "Show Selected" then showSelectedEntity <- true
                        let groups = World.getGroups selectedScreen world
                        let mutable selectedGroupName = selectedGroup.Name
                        ImGui.SetNextItemWidth -1.0f
                        if ImGui.BeginCombo ("##selectedGroupName", selectedGroupName) then
                            for group in groups do
                                if ImGui.Selectable (group.Name, strEq group.Name selectedGroupName) then
                                    selectEntityOpt None
                                    selectGroup group
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
                                        selectEntityOpt (Some sourceEntity')
                                        showSelectedEntity <- true
                                | None -> ()
                        let entities =
                            World.getEntitiesSovereign selectedGroup world |> // TODO: P1: see if we can optimize entity hierarchy queries!
                            Seq.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity)) |>
                            Array.ofSeq |>
                            Array.sortBy fst |>
                            Array.map snd
                        for entity in entities do
                            imGuiEntityHierarchy entity
                        ImGui.End ()

                    // game properties window
                    if ImGui.Begin "Game Properties" then
                        imGuiEditProperties Simulants.Game
                        ImGui.End ()

                    // screen properties window
                    if ImGui.Begin "Screen Properties" then
                        imGuiEditProperties selectedScreen
                        ImGui.End ()

                    // group properties window
                    if ImGui.Begin "Group Properties" then
                        imGuiEditProperties selectedGroup
                        ImGui.End ()

                    // entity properties window
                    if ImGui.Begin "Entity Properties" then
                        match selectedEntityOpt with
                        | Some entity when entity.Exists world -> imGuiEditProperties entity
                        | Some _ | None -> ()
                        ImGui.End ()

                    // property editor window
                    if ImGui.Begin "Property Editor" then
                        match propertyDescriptorFocusedOpt with
                        | Some (propertyDescriptor, simulant) when
                            World.getExists simulant world &&
                            propertyDescriptor.PropertyType <> typeof<ComputedProperty> ->
                            let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType)
                            let propertyValue = getProperty propertyDescriptor simulant
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
                                if ImGui.Button "Pick" then showAssetPickerDialog <- true
                            let mutable propertyValuePretty = PrettyPrinter.prettyPrint propertyValueEscaped PrettyPrinter.defaultPrinter
                            if ImGui.InputTextMultiline ("##propertyValuePretty", &propertyValuePretty, 131072u, v2 -1.0f -1.0f) then
                                try let propertyValueEscaped = propertyValuePretty
                                    let propertyValueUnescaped = String.unescape propertyValueEscaped
                                    let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                    setProperty propertyValue propertyDescriptor simulant
                                with :? ParseException | :? ConversionException -> ()
                            if isPropertyAssetTag then
                                if ImGui.BeginDragDropTarget () then
                                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                        match dragDropPayloadOpt with
                                        | Some payload ->
                                            try let propertyValueEscaped = payload
                                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                                let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                                setProperty propertyValue propertyDescriptor simulant
                                            with :? ParseException | :? ConversionException -> ()
                                        | None -> ()
                                    ImGui.EndDragDropTarget ()
                        | Some _ | None -> ()
                        ImGui.End ()

                    // asset graph window
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

                    // overlayer window
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

                    // event tracing window
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

                    // audio player window
                    if ImGui.Begin "Audio Player" then
                        ImGui.Text "Master Sound Volume"
                        let mutable masterSoundVolume = World.getMasterSoundVolume world
                        if ImGui.SliderFloat ("##masterSoundVolume", &masterSoundVolume, 0.0f, 1.0f) then world <- World.setMasterSoundVolume masterSoundVolume world
                        ImGui.SameLine ()
                        ImGui.Text (string masterSoundVolume)
                        ImGui.Text "Master Song Volume"
                        let mutable masterSongVolume = World.getMasterSongVolume world
                        if ImGui.SliderFloat ("##masterSongVolume", &masterSongVolume, 0.0f, 1.0f) then world <- World.setMasterSongVolume masterSongVolume world
                        ImGui.SameLine ()
                        ImGui.Text (string masterSongVolume)
                        ImGui.End ()

                    // renderer window
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

                // in full-screen mode, just show full-screen short cut window
                else
                    if ImGui.Begin "Full Screen Enabled" then
                        ImGui.Text "Full Screen (F11)"
                        ImGui.SameLine ()
                        ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                        ImGui.End ()

                // entity context menu
                if showEntityContextMenu then
                    ImGui.SetNextWindowPos rightClickPosition
                    ImGui.SetNextWindowSize (v2 250.0f 135.0f)
                    if ImGui.Begin ("ContextMenu", ImGuiWindowFlags.NoTitleBar) then
                        if ImGui.Button "Create" then createEntity true false; showEntityContextMenu <- false
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth -1.0f
                        if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                            for dispatcherName in (World.getEntityDispatchers world).Keys do
                                if ImGui.Selectable (dispatcherName, strEq dispatcherName newEntityDispatcherName) then
                                    newEntityDispatcherName <- dispatcherName
                                    createEntity true false
                                    showEntityContextMenu <- false
                            ImGui.EndCombo ()
                        if ImGui.Button "Delete" then tryDeleteSelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if  ImGui.IsMouseClicked ImGuiMouseButton.Right ||
                            ImGui.IsKeyPressed ImGuiKey.Escape then
                            showEntityContextMenu <- false
                        ImGui.Separator ()
                        if ImGui.Button "Cut" then tryCutSelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if ImGui.Button "Copy" then tryCopySelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if ImGui.Button "Paste" then tryPaste true |> ignore<bool>; showEntityContextMenu <- false
                        ImGui.End ()

                // asset picker dialog
                if showAssetPickerDialog then
                    let title = "Choose an Asset..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal (title, &showAssetPickerDialog) then
                        ImGui.SetNextItemWidth -1.0f
                        ImGui.InputTextWithHint ("##searchString", "[enter search text]", &assetPickerSearchStr, 4096u) |> ignore<bool>
                        let assets = Metadata.getDiscoveredAssets ()
                        for package in assets do
                            let flags = ImGuiTreeNodeFlags.SpanAvailWidth ||| ImGuiTreeNodeFlags.OpenOnArrow
                            if ImGui.TreeNodeEx (package.Key, flags) then
                                for assetName in package.Value do
                                    if (assetName.ToLowerInvariant ()).Contains (assetPickerSearchStr.ToLowerInvariant ()) then
                                        if ImGui.TreeNodeEx (assetName, flags ||| ImGuiTreeNodeFlags.Leaf) then
                                            if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                                                match propertyDescriptorFocusedOpt with
                                                | Some (propertyDescriptor, simulant) when
                                                    World.getExists simulant world &&
                                                    propertyDescriptor.PropertyType <> typeof<ComputedProperty> ->
                                                    let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType)
                                                    let propertyValueStr = "[" + package.Key + " " + assetName + "]"
                                                    let propertyValue = converter.ConvertFromString propertyValueStr
                                                    setProperty propertyValue propertyDescriptor simulant
                                                | Some _ | None -> ()
                                                showAssetPickerDialog <- false
                                            ImGui.TreePop ()
                                ImGui.TreePop ()
                        ImGui.EndPopup ()
                    if ImGui.IsKeyPressed ImGuiKey.Escape then showAssetPickerDialog <- false

                // new project dialog
                if showNewProjectDialog then

                    // ensure template directory exists
                    let programDir = Reflection.Assembly.GetEntryAssembly().Location |> Path.GetDirectoryName 
                    let slnDir = programDir + "/../../../../.." |> Path.Simplify
                    let templateDir = programDir + "/../../../../Nu.Template" |> Path.Simplify
                    if Directory.Exists templateDir then

                        // prompt user to create new project
                        if ImGui.Begin "Create Nu Project... *EDITOR RESTART REQUIRED!*" then
                            ImGui.Text "Project Name"
                            ImGui.SameLine ()
                            ImGui.InputText ("##newProjectName", &newProjectName, 4096u) |> ignore<bool>
                            newProjectName <- newProjectName.Replace(" ", "").Replace("\t", "").Replace(".", "")
                            let templateIdentifier = templateDir.Replace("/", "\\") // this is what dotnet knows the template as for uninstall...
                            let templateFileName = "Nu.Template.fsproj"
                            let projectsDir = programDir + "/../../../../../Projects" |> Path.Simplify
                            let newProjectDir = projectsDir + "/" + newProjectName |> Path.Simplify
                            let newProjectDll = newProjectDir + "/bin/" + Constants.Editor.BuildName + "/net7.0/" + newProjectName + ".dll"
                            let newFileName = newProjectName + ".fsproj"
                            let newProject = newProjectDir + "/" + newFileName |> Path.Simplify
                            let validName = Array.notExists (fun char -> newProjectName.Contains (string char)) (Path.GetInvalidPathChars ())
                            if not validName then ImGui.Text "Invalid project name!"
                            let validDirectory = not (Directory.Exists newProjectDir)
                            if not validDirectory then ImGui.Text "Project already exists!"
                            if validName && validDirectory && (ImGui.Button "Create" || ImGui.IsKeyPressed ImGuiKey.Enter) then

                                // attempt to create project files
                                try Log.info ("Creating project '" + newProjectName + "' in '" + projectsDir + "'...")

                                    // install nu template
                                    Directory.SetCurrentDirectory templateDir
                                    Process.Start("dotnet", "new uninstall \"" + templateIdentifier + "\"").WaitForExit()
                                    Process.Start("dotnet", "new install ./").WaitForExit()

                                    // instantiate nu template
                                    Directory.SetCurrentDirectory projectsDir
                                    Directory.CreateDirectory newProjectName |> ignore<DirectoryInfo>
                                    Directory.SetCurrentDirectory newProjectDir
                                    Process.Start("dotnet", "new nu-game --force").WaitForExit()

                                    // rename project file
                                    File.Copy (templateFileName, newFileName, true)
                                    File.Delete templateFileName

                                    // substitute project guid in project file
                                    let projectGuid = Gen.id
                                    let projectGuidStr = projectGuid.ToString().ToUpperInvariant()
                                    let newProjectStr = File.ReadAllText newProject
                                    let newProjectStr = newProjectStr.Replace("4DBBAA23-56BA-43CB-AB63-C45D5FC1016F", projectGuidStr)
                                    File.WriteAllText (newProject, newProjectStr)

                                    // add project to sln file
                                    Directory.SetCurrentDirectory slnDir
                                    let slnLines = "Nu.sln" |> File.ReadAllLines |> Array.toList
                                    let insertionIndex = List.findIndexBack ((=) "\tEndProjectSection") slnLines
                                    let slnLines = 
                                        List.take insertionIndex slnLines @
                                        ["\t\t{" + projectGuidStr + "} = {" + projectGuidStr + "}"] @
                                        List.skip insertionIndex slnLines
                                    let insertionIndex = List.findIndex ((=) "Global") slnLines
                                    let slnLines =
                                        List.take insertionIndex slnLines @
                                        ["Project(\"{6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705}\") = \"" + newProjectName + "\", \"Projects\\" + newProjectName + "\\" + newProjectName + ".fsproj\", \"{" + projectGuidStr + "}\""
                                         "EndProject"] @
                                        List.skip insertionIndex slnLines
                                    let insertionIndex = List.findIndex ((=) "\tGlobalSection(SolutionProperties) = preSolution") slnLines - 1
                                    let slnLines =
                                        List.take insertionIndex slnLines @
                                        ["\t\t{" + projectGuidStr + "}.Debug|Any CPU.ActiveCfg = Debug|Any CPU"
                                         "\t\t{" + projectGuidStr + "}.Debug|Any CPU.Build.0 = Debug|Any CPU"
                                         "\t\t{" + projectGuidStr + "}.Release|Any CPU.ActiveCfg = Release|Any CPU"
                                         "\t\t{" + projectGuidStr + "}.Release|Any CPU.Build.0 = Release|x64"] @
                                        List.skip insertionIndex slnLines
                                    let insertionIndex = List.findIndex ((=) "\tGlobalSection(ExtensibilityGlobals) = postSolution") slnLines - 1
                                    let slnLines =
                                        List.take insertionIndex slnLines @
                                        ["\t\t{" + projectGuidStr + "} = {E3C4D6E1-0572-4D80-84A9-8001C21372D3}"] @
                                        List.skip insertionIndex slnLines
                                    File.WriteAllLines ("Nu.sln", List.toArray slnLines)
                                    Log.info ("Project '" + newProjectName + "'" + "created.")

                                    // configure editor to open new project then exit
                                    let savedState =
                                        { ProjectFilePath = newProjectDll
                                          EditModeOpt = Some "Title"
                                          UseImperativeExecution = projectImperativeExecution }
                                    let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                    let gaiaDirectory = Path.GetDirectoryName gaiaFilePath
                                    try File.WriteAllText (gaiaDirectory + "/" + Constants.Editor.SavedStateFilePath, scstring savedState)
                                        Directory.SetCurrentDirectory gaiaDirectory
                                        world <- World.exit world
                                    with _ -> Log.trace "Could not save editor state and open new project."

                                    // close dialog
                                    showNewProjectDialog <- false
                                    newProjectName <- "MyGame"

                                // log failure
                                with exn -> Log.trace ("Failed to create new project '" + newProjectName + "' due to: " + scstring exn)

                            // escape to cancel
                            if ImGui.IsKeyPressed ImGuiKey.Escape then
                                showNewProjectDialog <- false
                                newProjectName <- "MyGame"

                            // fin
                            ImGui.End ()

                    // template project missing
                    else
                        Log.trace "Template project is missing; new project cannot be generated."
                        showNewProjectDialog <- false

                // open project dialog
                if showOpenProjectDialog then
                    let title = "Choose a project .dll... *EDITOR RESTART REQUIRED!*"
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal (title, &showOpenProjectDialog) then
                        ImGui.Text "Game Assembly Path:"
                        ImGui.SameLine ()
                        ImGui.InputTextWithHint ("##gameDllFilePath", "[enter game .dll path]", &gameDllPath, 4096u) |> ignore<bool>
                        ImGui.Text "Edit Mode:"
                        ImGui.SameLine ()
                        ImGui.InputText ("##projectGameMode", &projectEditMode, 4096u) |> ignore<bool>
                        ImGui.Checkbox ("Use Imperative Execution (faster, but no Undo / Redo)", &projectImperativeExecution) |> ignore<bool>
                        if  (ImGui.Button "Open" || ImGui.IsKeyPressed ImGuiKey.Enter) &&
                            String.notEmpty gameDllPath &&
                            File.Exists gameDllPath then
                            showOpenProjectDialog <- false
                            let savedState =
                                { ProjectFilePath = gameDllPath
                                  EditModeOpt = Some projectEditMode
                                  UseImperativeExecution = projectImperativeExecution }
                            let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                            let gaiaDirectory = Path.GetDirectoryName gaiaFilePath
                            try File.WriteAllText (gaiaDirectory + "/" + Constants.Editor.SavedStateFilePath, scstring savedState)
                                Directory.SetCurrentDirectory gaiaDirectory
                                world <- World.exit world
                            with _ -> Log.info "Could not save editor state and open project."
                        if ImGui.IsKeyPressed ImGuiKey.Escape then showOpenProjectDialog <- false

                // new group dialog
                if showNewGroupDialog then
                    let title = "Create a group..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal (title, &showNewGroupDialog) then
                        ImGui.Text "Group Name:"
                        ImGui.SameLine ()
                        ImGui.InputTextWithHint ("##newGroupName", "[enter group name]", &newGroupName, 4096u) |> ignore<bool>
                        let newGroup = selectedScreen / newGroupName
                        if ImGui.BeginCombo ("##newGroupDispatcherName", newGroupDispatcherName) then
                            for dispatcherName in (World.getGroupDispatchers world).Keys do
                                if ImGui.Selectable (dispatcherName, strEq dispatcherName newGroupDispatcherName) then
                                    newGroupDispatcherName <- dispatcherName
                            ImGui.EndCombo ()
                        if (ImGui.Button "Create" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty newGroupName && not (newGroup.Exists world) then
                            let oldWorld = world
                            try world <- World.createGroup4 newGroupDispatcherName (Some newGroupName) selectedScreen world |> snd
                                selectGroup newGroup
                                showNewGroupDialog <- false
                                newGroupName <- ""
                            with exn ->
                                world <- World.choose oldWorld
                                messageBoxOpt <- Some ("Could not create group due to: " + scstring exn)
                        if ImGui.IsKeyPressed ImGuiKey.Escape then showNewGroupDialog <- false

                // open group dialog
                if showOpenGroupDialog then
                    let title = "Choose a nugroup file..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal (title, &showOpenGroupDialog) then
                        ImGui.Text "File Path:"
                        ImGui.SameLine ()
                        ImGui.InputTextWithHint ("##groupFilePath", "[enter file path]", &groupFilePath, 4096u) |> ignore<bool>
                        if  (ImGui.Button "Open" || ImGui.IsKeyPressed ImGuiKey.Enter) &&
                            String.notEmpty groupFilePath &&
                            File.Exists groupFilePath then
                            snapshot ()
                            showOpenGroupDialog <- not (tryLoadSelectedGroup groupFilePath)
                        if ImGui.IsKeyPressed ImGuiKey.Escape then showOpenGroupDialog <- false

                // save group dialog
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

                // message box dialog
                match messageBoxOpt with
                | Some messageBox ->
                    let title = "Message!"
                    let mutable showing = true
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal (title, &showing) then
                        ImGui.TextWrapped messageBox
                        if ImGui.Button "Okay" || ImGui.IsKeyPressed ImGuiKey.Enter || ImGui.IsKeyPressed ImGuiKey.Escape then showing <- false
                        ImGui.EndPopup ()
                    if not showing then messageBoxOpt <- None
                | None -> ()

                // imgui inspector window
                if showInspector then
                    ImGui.ShowStackToolWindow ()

                // fin
                world

            // propagate exception to dialog
            with exn ->
                recoverableExceptionOpt <- Some (exn, wtemp)
                world

        // exception handling dialog
        | Some (exn, oldWorld) ->
            let title = "Unexpected Exception!"
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &showOpenProjectDialog) then
                ImGui.Text "Exception text:"
                ImGui.TextWrapped (scstring exn)
                ImGui.Text "How would you like to handle this exception?"
                if ImGui.Button "Ignore exception and revert to old world." then
                    world <- oldWorld
                    recoverableExceptionOpt <- None
                if ImGui.Button "Ignore exception and process with current world." then
                    recoverableExceptionOpt <- None
                if ImGui.Button "Exit the editor." then
                    world <- World.exit world
                ImGui.EndPopup ()
            world

    let rec private runWithCleanUp savedState targetDir' screen wtemp =
        world <- wtemp
        targetDir <- targetDir'
        gameDllPath <- savedState.ProjectFilePath
        projectEditMode <- match savedState.EditModeOpt with Some m -> m | None -> ""
        projectImperativeExecution <- savedState.UseImperativeExecution
        selectScreen screen
        selectGroup (Nu.World.getGroups screen world |> Seq.head)
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
        let result = World.runWithCleanUp tautology id id id imGuiProcess Live true world
        world <- Unchecked.defaultof<_>
        result

    (* Public API *)

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
                let imguiIniFilePath = targetDir + "/imgui.ini"
                if not (File.Exists imguiIniFilePath) then
                    File.WriteAllText (imguiIniFilePath, ImGuiIniFileStr)
                runWithCleanUp savedState targetDir screen world
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
        | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure