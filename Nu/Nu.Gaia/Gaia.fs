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
open Microsoft.FSharp.Core
open ImGuiNET
open ImGuizmoNET
open Prime
open Nu

//////////////////////////////////////////////////////////////////////////////////////
// TODO:                                                                            //
// Log Output window.                                                               //
// Perhaps look up (Value)Some-constructed default property values from overlayer.  //
// Custom properties in order of priority:                                          //
//  NormalOpt (for terrain)                                                         //
//  Enums                                                                           //
//  Layout                                                                          //
//  CollisionMask                                                                   //
//  CollisionCategories                                                             //
//  CollisionDetection                                                              //
//  BodyShape                                                                       //
//  JointDevice                                                                     //
//  DateTimeOffset?                                                                 //
//  SymbolicCompression                                                             //
//  Flag Enums                                                                      //
//////////////////////////////////////////////////////////////////////////////////////

[<RequireQualifiedAccess>]
module Gaia =

    (* World States - mutable to accomodatae ImGui's intended authoring style *)

    let mutable private world = Unchecked.defaultof<World> // this will be initialized on start
    let mutable private worldsPast = []
    let mutable private worldsFuture = []

    (* Active Editing States *)

    let mutable private manipulationActive = false
    let mutable private manipulationOperation = OPERATION.TRANSLATE
    let mutable private expandEntityHierarchy = false
    let mutable private collapseEntityHierarchy = false
    let mutable private showSelectedEntity = false
    let mutable private rightClickPosition = v2Zero
    let mutable private focusedPropertyDescriptorOpt = Option<PropertyDescriptor * Simulant>.None
    let mutable private propertyEditorFocusRequested = false
    let mutable private entityHierarchySearchRequested = false
    let mutable private assetViewerSearchRequested = false
    let mutable private propertyValueStrPrevious = ""
    let mutable private dragDropPayloadOpt = None
    let mutable private dragEntityState = DragEntityInactive
    let mutable private dragEyeState = DragEyeInactive
    let mutable private selectedScreen = Game / "Screen" // TODO: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable private selectedGroup = selectedScreen / "Group"
    let mutable private selectedEntityOpt = Option<Entity>.None
    let mutable private openProjectFilePath = null // this will be initialized on start
    let mutable private openProjectEditMode = "Title"
    let mutable private openProjectImperativeExecution = false
    let mutable private newProjectName = "MyGame"
    let mutable private newGroupDispatcherName = nameof GroupDispatcher
    let mutable private newEntityDispatcherName = null // this will be initialized on start
    let mutable private newEntityOverlayName = "(Default Overlay)"
    let mutable private newEntityParentOpt = Option<Entity>.None
    let mutable private newEntityElevation = 0.0f
    let mutable private newEntityDistance = 2.0f
    let mutable private newGroupName = ""
    let mutable private groupRename = ""
    let mutable private entityRename = ""
    let mutable private desiredEye2dCenter = v2Zero
    let mutable private desiredEye3dCenter = v3Zero
    let mutable private desiredEye3dRotation = quatIdentity
    let mutable private eyeChangedElsewhere = false
    let mutable private fpsStartDateTime = DateTimeOffset.Now
    let mutable private fpsStartUpdateTime = 0L

    (* Configuration States *)

    let mutable private fullScreen = false
    let mutable private editWhileAdvancing = false
    let mutable private snaps2dSelected = true
    let mutable private snaps2d = Constants.Gaia.Snaps2dDefault
    let mutable private snaps3d = Constants.Gaia.Snaps3dDefault
    let mutable private snapDrag = 0.1f
    let mutable private alternativeEyeTravelInput = false
    let mutable private entityHierarchySearchStr = ""
    let mutable private assetViewerSearchStr = ""
    let mutable private lightingConfig =
        { LightCutoffMargin = Constants.Render.LightCutoffMarginDefault
          LightShadowBiasAcne = Constants.Render.LightShadowBiasAcneDefault
          LightShadowBiasBleed = Constants.Render.LightShadowBiasBleedDefault
          LightMappingEnabled = Constants.Render.LightMappingEnabledDefault }
    let mutable private ssaoConfig =
        { SsaoEnabled = Constants.Render.SsaoEnabledDefault
          SsaoIntensity = Constants.Render.SsaoIntensityDefault
          SsaoBias = Constants.Render.SsaoBiasDefault
          SsaoRadius = Constants.Render.SsaoRadiusDefault
          SsaoDistanceMax = Constants.Render.SsaoDistanceMaxDefault
          SsaoSampleCount = Constants.Render.SsaoSampleCountDefault }

    (* Project States *)

    let mutable private targetDir = "."
    let mutable private projectDllPath = ""
    let mutable private projectFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private projectEditMode = ""
    let mutable private projectImperativeExecution = false
    let mutable private groupFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private groupFilePaths = Map.empty<Group Address, string>
    let mutable private assetGraphStr = null // this will be initialized on start
    let mutable private overlayerStr = null // this will be initialized on start

    (* Modal Activity States *)

    let mutable private messageBoxOpt = Option<string>.None
    let mutable private recoverableExceptionOpt = Option<Exception * World>.None
    let mutable private showEntityContextMenu = false
    let mutable private showNewProjectDialog = false
    let mutable private showOpenProjectDialog = false
    let mutable private showOpenProjectFileDialog = false
    let mutable private showCloseProjectDialog = false
    let mutable private showNewGroupDialog = false
    let mutable private showOpenGroupDialog = false
    let mutable private showSaveGroupDialog = false
    let mutable private showRenameGroupDialog = false
    let mutable private showRenameEntityDialog = false
    let mutable private showConfirmExitDialog = false
    let mutable private showRestartDialog = false
    let mutable private showInspector = false
    let mutable private reloadAssetsRequested = 0
    let mutable private reloadCodeRequested = 0
    let mutable private reloadAllRequested = 0
    let modal () =
        messageBoxOpt.IsSome ||
        recoverableExceptionOpt.IsSome ||
        showEntityContextMenu ||
        showNewProjectDialog ||
        showOpenProjectDialog ||
        showOpenProjectFileDialog ||
        showCloseProjectDialog ||
        showNewGroupDialog ||
        showOpenGroupDialog ||
        showSaveGroupDialog ||
        showRenameGroupDialog ||
        showRenameEntityDialog ||
        showConfirmExitDialog ||
        showRestartDialog ||
        reloadAssetsRequested <> 0 ||
        reloadCodeRequested <> 0 ||
        reloadAllRequested <> 0

    (* Memoization *)
    let mutable toSymbolMemo = new ForgetfulDictionary<struct (Type * obj), Symbol> (HashIdentity.FromFunctions hash objEq)
    let mutable ofSymbolMemo = new ForgetfulDictionary<struct (Type * Symbol), obj> (HashIdentity.Structural)

    (* Initial imgui.ini File Content *)

    let private ImGuiIniFileStr = """
[Window][Gaia]
Pos=0,0
Size=1920,54
Collapsed=0
DockId=0x00000002,0

[Window][Edit Overlayer]
Pos=284,854
Size=677,226
Collapsed=0
DockId=0x00000001,2

[Window][Edit Asset Graph]
Pos=284,854
Size=677,226
Collapsed=0
DockId=0x00000001,1

[Window][Edit Property]
Pos=284,854
Size=677,226
Collapsed=0
DockId=0x00000001,0

[Window][Metrics]
Pos=963,854
Size=659,226
Collapsed=0
DockId=0x00000009,5

[Window][Event Tracing]
Pos=963,854
Size=659,226
Collapsed=0
DockId=0x00000009,4

[Window][Renderer]
Pos=963,854
Size=659,226
Collapsed=0
DockId=0x00000009,3

[Window][Audio Player]
Pos=963,854
Size=659,226
Collapsed=0
DockId=0x00000009,2

[Window][Editor]
Pos=963,854
Size=659,226
Collapsed=0
DockId=0x00000009,1

[Window][Asset Viewer]
Pos=963,854
Size=659,226
Collapsed=0
DockId=0x00000009,0

[Window][Full Screen Enabled]
Pos=20,23
Size=162,54
Collapsed=0

[Window][Message!]
Pos=827,398
Size=360,182
Collapsed=0

[Window][Unhandled Exception!]
Pos=608,366
Size=694,406
Collapsed=0

[Window][Are you okay with exiting Gaia?]
Pos=836,504
Size=248,72
Collapsed=0

[Window][ContextMenu]
Pos=853,391
Size=250,135
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

[Window][Create Nu Project... *EDITOR RESTART REQUIRED!*]
Pos=661,488
Size=621,105
Collapsed=0

[Window][Choose a project .dll... *EDITOR RESTART REQUIRED!*]
Pos=628,476
Size=674,128
Collapsed=0

[Window][Create a group...]
Pos=715,469
Size=482,128
Collapsed=0

[Window][Choose a nugroup file...]
Pos=602,352
Size=677,399
Collapsed=0

[Window][Save a nugroup file...]
Pos=613,358
Size=664,399
Collapsed=0

[Window][Choose an Asset...]
Pos=796,323
Size=336,458
Collapsed=0

[Window][Rename group...]
Pos=734,514
Size=444,94
Collapsed=0

[Window][Rename entity...]
Pos=734,514
Size=444,94
Collapsed=0

[Window][Reloading assets...]
Pos=700,500
Size=520,110
Collapsed=0

[Window][Reloading code...]
Pos=700,500
Size=520,110
Collapsed=0

[Window][Reloading assets and code...]
Pos=700,500
Size=520,110
Collapsed=0

[Window][DockSpaceViewport_11111111]
Pos=0,0
Size=1920,1080
Collapsed=0

[Window][Debug##Default]
Pos=60,60
Size=400,400
Collapsed=0

[Window][Close project... *EDITOR RESTART REQUIRED!*]
Pos=769,504
Size=381,71
Collapsed=0

[Window][Editor restart required.]
Pos=671,504
Size=577,71
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
          DockNode    ID=0x00000004 Parent=0x00000005 SizeRef=1678,796 CentralNode=1
          DockNode    ID=0x00000003 Parent=0x00000005 SizeRef=1678,226 Split=X Selected=0xD4E24632
            DockNode  ID=0x00000001 Parent=0x00000003 SizeRef=677,205 Selected=0x9CF3CB04
            DockNode  ID=0x00000009 Parent=0x00000003 SizeRef=659,205 Selected=0xD92922EC
        DockNode      ID=0x00000006 Parent=0x00000008 SizeRef=346,979 Selected=0x199AB496
    DockNode          ID=0x0000000E Parent=0x0000000F SizeRef=296,1080 Selected=0xD5116FF8

"""

    (* Prelude Functions *)

    let private canEditWithMouse () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureMouseGlobal) && (world.Halted || editWhileAdvancing)

    let private canEditWithKeyboard () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureKeyboardGlobal) && (world.Halted || editWhileAdvancing)

    let private snapshot () =
        worldsPast <- world :: worldsPast
        worldsFuture <- []

    let private makeGaiaState projectDllPath editModeOpt freshlyLoaded : GaiaState =
        GaiaState.make
            projectDllPath editModeOpt freshlyLoaded openProjectImperativeExecution editWhileAdvancing
            desiredEye2dCenter desiredEye3dCenter desiredEye3dRotation (World.getMasterSoundVolume world) (World.getMasterSongVolume world)            
            snaps2dSelected snaps2d snaps3d newEntityElevation newEntityDistance alternativeEyeTravelInput

    let private printGaiaState gaiaState =
        PrettyPrinter.prettyPrintSymbol (valueToSymbol gaiaState) PrettyPrinter.defaultPrinter

    let private containsProperty propertyDescriptor simulant =
        SimulantPropertyDescriptor.containsPropertyDescriptor propertyDescriptor simulant world

    let private getPropertyValue propertyDescriptor simulant =
        SimulantPropertyDescriptor.getValue propertyDescriptor simulant world

    let private setPropertyValueWithoutUndo (value : obj) propertyDescriptor simulant =
        match SimulantPropertyDescriptor.trySetValue value propertyDescriptor simulant world with
        | Right wtemp -> world <- wtemp
        | Left (error, wtemp) -> messageBoxOpt <- Some error; world <- wtemp

    let private setPropertyValueIgnoreError (value : obj) propertyDescriptor simulant =
        snapshot ()
        match SimulantPropertyDescriptor.trySetValue value propertyDescriptor simulant world with
        | Right wtemp -> world <- wtemp
        | Left (_, wtemp) -> world <- wtemp

    let private setPropertyValue (value : obj) propertyDescriptor simulant =
        if  not (ImGui.IsMouseDragging ImGuiMouseButton.Left) ||
            not (ImGui.IsMouseDraggingContinued ImGuiMouseButton.Left) then
            snapshot ()
        setPropertyValueWithoutUndo value propertyDescriptor simulant

    let private selectScreen screen =
        if screen <> selectedScreen then
            ImGui.SetWindowFocus null
            newEntityParentOpt <- None
            selectedScreen <- screen

    let private selectGroup group =
        if group <> selectedGroup then
            ImGui.SetWindowFocus null
            newEntityParentOpt <- None
            selectedGroup <- group

    let private selectGroupInitial screen =
        let groups = World.getGroups screen world
        let group =
            match Seq.tryFind (fun (group : Group) -> group.Name = "Scene") groups with // NOTE: try to get the Scene group since it's more likely to be the group the user wants to edit.
            | Some group -> group
            | None ->
                match Seq.tryHead groups with
                | Some group -> group
                | None ->
                    let (group, wtemp) = World.createGroup (Some "Group") screen world in world <- wtemp
                    group
        selectGroup group

    let private selectEntityOpt entityOpt =

        if entityOpt <> selectedEntityOpt then

            // try to focus on same entity property
            match focusedPropertyDescriptorOpt with
            | Some (propertyDescriptor, :? Entity) ->
                match entityOpt with
                | Some entity when containsProperty propertyDescriptor entity ->
                    focusedPropertyDescriptorOpt <- Some (propertyDescriptor, entity)
                | Some _ | None -> focusedPropertyDescriptorOpt <- None
            | Some _ | None -> ()

            // HACK: in order to keep the property of one simulant from being copied to another when the selected
            // simulant is changed, we have to move focus away from the property windows. We chose to focus on the
            // "Entity Hierarchy" window in order to avoid disrupting drag and drop when selecting a different entity
            // in it. Then if there is no entity selected, we'll select the viewport instead
            ImGui.SetWindowFocus "Entity Hierarchy"
            if entityOpt.IsNone then ImGui.SetWindowFocus "Viewport"

        // actually set the selection
        selectedEntityOpt <- entityOpt

    let private deselectEntity () =
        focusedPropertyDescriptorOpt <- None
        selectEntityOpt None

    let private tryUndo () =
        if
            (if not (Nu.World.getImperative world) then
                match worldsPast with
                | worldPast :: worldsPast' ->
                    let worldFuture = world
                    world <- Nu.World.switch worldPast
                    worldsPast <- worldsPast'
                    worldsFuture <- worldFuture :: worldsFuture
                    true
                | [] -> false
             else false) then
            propertyValueStrPrevious <- ""
            selectScreen (World.getSelectedScreen world)
            if not (selectedGroup.Exists world) || not (selectedGroup.Selected world) then
                let group = Seq.head (World.getGroups selectedScreen world)
                selectGroup group
            match selectedEntityOpt with
            | Some entity when not (entity.Exists world) || entity.Group <> selectedGroup -> selectEntityOpt None
            | Some _ | None -> ()
            world <- World.setEye2dCenter desiredEye2dCenter world
            world <- World.setEye3dCenter desiredEye3dCenter world
            world <- World.setEye3dRotation desiredEye3dRotation world
            true
        else false

    let private tryRedo () =
        if
            (if not (Nu.World.getImperative world) then
                match worldsFuture with
                | worldFuture :: worldsFuture' ->
                    let worldPast = world
                    world <- Nu.World.switch worldFuture
                    worldsPast <- worldPast :: worldsPast
                    worldsFuture <- worldsFuture'
                    true
                | [] -> false
             else false) then
            propertyValueStrPrevious <- ""
            selectScreen (World.getSelectedScreen world)
            if not (selectedGroup.Exists world) || not (selectedGroup.Selected world) then
                let group = Seq.head (World.getGroups selectedScreen world)
                selectGroup group
            match selectedEntityOpt with
            | Some entity when not (entity.Exists world) || entity.Group <> selectedGroup -> selectEntityOpt None
            | Some _ | None -> ()
            world <- World.setEye2dCenter desiredEye2dCenter world
            world <- World.setEye3dCenter desiredEye3dCenter world
            world <- World.setEye3dRotation desiredEye3dRotation world
            true
        else false

    let private freezeEntities () =
        let groups = World.getGroups selectedScreen world
        let freezers =
            groups |>
            Seq.map (fun group -> World.getEntitiesFlattened group world) |>
            Seq.concat |>
            Seq.filter (fun entity -> entity.Has<FreezerFacet> world)
        for freezer in freezers do
            world <- freezer.SetFrozen true world

    let private thawEntities () =
        let groups = World.getGroups selectedScreen world
        let freezers =
            groups |>
            Seq.map (fun group -> World.getEntitiesFlattened group world) |>
            Seq.concat |>
            Seq.filter (fun entity -> entity.Has<FreezerFacet> world)
        for freezer in freezers do
            world <- freezer.SetFrozen false world

    let private rerenderLightMaps () =
        let groups = World.getGroups selectedScreen world
        let lightProbes =
            groups |>
            Seq.map (fun group -> World.getEntitiesFlattened group world) |>
            Seq.concat |>
            Seq.filter (fun entity -> entity.Has<LightProbe3dFacet> world)
        for lightProbe in lightProbes do
            world <- lightProbe.SetProbeStale true world

    let private getSnaps () =
        if snaps2dSelected
        then snaps2d
        else snaps3d

    let private getPickCandidates2d () =
        let (entities, wtemp) = World.getEntities2dInView (HashSet ()) world in world <- wtemp
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible world) |> Seq.toArray
        entitiesInGroup

    let private getPickCandidates3d () =
        let (entities, wtemp) = World.getEntities3dInView (HashSet ()) world in world <- wtemp
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible world) |> Seq.toArray
        entitiesInGroup

    let private tryMousePick mousePosition =
        let entities2d = getPickCandidates2d ()
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d world
        match pickedOpt with
        | Some entity ->
            selectEntityOpt (Some entity)
            Some (0.0f, entity)
        | None ->
            let entities3d = getPickCandidates3d ()
            let pickedOpt = World.tryPickEntity3d mousePosition entities3d world
            match pickedOpt with
            | Some (intersection, entity) ->
                selectEntityOpt (Some entity)
                Some (intersection, entity)
            | None -> None

    let private tryPickName names =
        let io = ImGui.GetIO ()
        io.SwallowKeyboard ()
        let namesMatching =
            [|for key in int ImGuiKey.A .. int ImGuiKey.Z do
                if ImGui.IsKeyReleased (enum key) then
                    let chr = char (key - int ImGuiKey.A + 97)
                    names |>
                    Seq.filter (fun (name : string) -> name.Length > 0 && Char.ToLowerInvariant name.[0] = chr) |>
                    Seq.tryHead|]
        namesMatching |>
        Array.definitize |>
        Array.tryHead

    let private searchAssetViewer () =
        ImGui.SetWindowFocus "Asset Viewer"
        assetViewerSearchRequested <- true

    let private searchEntityHierarchy () =
        ImGui.SetWindowFocus "Entity Hierarchy"
        entityHierarchySearchRequested <- true

    let private revertOpenProjectState () =
        openProjectFilePath <- projectDllPath
        openProjectEditMode <- projectEditMode
        openProjectImperativeExecution <- world.Imperative

    (* Nu Event Handlers *)

    let private handleNuMouseButton (_ : Event<MouseButtonData, Game>) wtemp =
        world <- wtemp
        if canEditWithMouse ()
        then (Resolve, world)
        else (Cascade, world)

    let private handleNuSelectedScreenOptChange (evt : Event<ChangeData, Game>) wtemp =
        world <- wtemp
        match evt.Data.Value :?> Screen option with
        | Some screen ->
            selectScreen screen
            selectGroupInitial screen
            selectEntityOpt None
            (Cascade, world)
        | None ->
            // just keep current group selection and screen if no screen selected
            (Cascade, world)

    let private imGuiRender wtemp =

        // render light probes of the selected group in light box and view frustum
        world <- wtemp
        let lightBox = World.getLight3dBox world
        let viewFrustum = World.getEye3dFrustumView world
        let (entities, wtemp) = World.getLightProbes3dInBox lightBox (HashSet ()) world in world <- wtemp
        let lightProbeModels =
            entities |>
            Seq.filter (fun entity -> entity.Group = selectedGroup && viewFrustum.Intersects (entity.GetBounds world)) |>
            Seq.map (fun light -> (light.GetAffineMatrix world, Omnipresent, None, MaterialProperties.defaultProperties)) |>
            SList.ofSeq
        if SList.notEmpty lightProbeModels then
            World.enqueueRenderMessage3d
                (RenderStaticModels
                    { Absolute = false
                      StaticModels = lightProbeModels
                      StaticModel = Assets.Default.LightProbeModel
                      RenderType = DeferredRenderType
                      RenderPass = NormalPass false })
                world

        // render lights of the selected group in play
        let (entities, wtemp) = World.getLights3dInBox lightBox (HashSet ()) world in world <- wtemp
        let lightModels =
            entities |>
            Seq.filter (fun entity -> entity.Group = selectedGroup && viewFrustum.Intersects (entity.GetBounds world)) |>
            Seq.map (fun light -> (light.GetAffineMatrix world, Omnipresent, None, MaterialProperties.defaultProperties)) |>
            SList.ofSeq
        if SList.notEmpty lightModels then
            World.enqueueRenderMessage3d
                (RenderStaticModels
                    { Absolute = false
                      StaticModels = lightModels
                      StaticModel = Assets.Default.LightbulbModel
                      RenderType = DeferredRenderType
                      RenderPass = NormalPass false })
                world

        // render selection highlights
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            if entity.GetIs2d world then
                let absolute = entity.GetAbsolute world
                let bounds = entity.GetBounds world
                let elevation = Single.MaxValue
                let transform = Transform.makePerimeter bounds v3Zero elevation absolute false
                let image = Assets.Default.HighlightImage
                World.enqueueRenderMessage2d
                    (LayeredOperation2d
                        { Elevation = elevation
                          Horizon = bounds.Bottom.Y
                          AssetTag = image
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
                let absolute = entity.GetAbsolute world
                let bounds = entity.GetBounds world
                let mutable boundsMatrix = Matrix4x4.CreateScale (bounds.Size + v3Dup 0.01f) // slightly bigger to eye to prevent z-fighting with selected entity
                boundsMatrix.Translation <- bounds.Center
                World.enqueueRenderMessage3d
                    (RenderStaticModel
                        { Absolute = absolute
                          ModelMatrix = boundsMatrix
                          Presence = Omnipresent
                          InsetOpt = None
                          MaterialProperties = MaterialProperties.defaultProperties
                          StaticModel = Assets.Default.HighlightModel
                          RenderType = ForwardRenderType (0.0f, Single.MinValue)
                          RenderPass = NormalPass false })
                    world
        | Some _ | None -> ()

        // fin
        world

    (* Editor Commands *)

    let private createEntity atMouse inHierarchy =
        snapshot ()
        let dispatcherName = newEntityDispatcherName
        let overlayNameDescriptor =
            match newEntityOverlayName with
            | "(Default Overlay)" -> DefaultOverlay
            | "(Routed Overlay)" -> RoutedOverlay
            | "(No Overlay)" -> NoOverlay
            | overlayName -> ExplicitOverlay overlayName
        let name = World.generateEntitySequentialName dispatcherName selectedGroup world
        let surnames =
            match selectedEntityOpt with
            | Some entity ->
                if inHierarchy then
                    if entity.Exists world then Array.add name entity.Surnames
                    else [|name|]
                else
                    match newEntityParentOpt with
                    | Some newEntityParent when newEntityParent.Exists world -> Array.add name newEntityParent.Surnames
                    | Some _ | None -> [|name|]
            | None -> [|name|]
        let (entity, wtemp) = World.createEntity5 dispatcherName overlayNameDescriptor (Some surnames) selectedGroup world in world <- wtemp
        let (positionSnap, degreesSnap, scaleSnap) = getSnaps ()
        let viewport = World.getViewport world
        let mutable entityTransform = entity.GetTransform world
        if entity.GetIs2d world then
            let eyeCenter = World.getEye2dCenter world
            let eyeSize = World.getEye2dSize world
            let entityPosition =
                if atMouse
                then viewport.MouseToWorld2d (entity.GetAbsolute world, rightClickPosition, eyeCenter, eyeSize)
                else viewport.MouseToWorld2d (entity.GetAbsolute world, World.getEye2dSize world, eyeCenter, eyeSize)
            let attributes = entity.GetAttributesInferred world
            entityTransform.Position <- entityPosition.V3
            entityTransform.Size <- attributes.SizeInferred
            entityTransform.Offset <- attributes.OffsetInferred
            entityTransform.Elevation <- newEntityElevation
            if snaps2dSelected && ImGui.IsCtrlUp ()
            then world <- entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform world
            else world <- entity.SetTransform entityTransform world
        else
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let entityPosition =
                if atMouse then
                    let ray = viewport.MouseToWorld3d (entity.GetAbsolute world, rightClickPosition, eyeCenter, eyeRotation)
                    let forward = eyeRotation.Forward
                    let plane = plane3 (eyeCenter + forward * newEntityDistance) -forward
                    (ray.Intersection plane).Value
                else eyeCenter + Vector3.Transform (v3Forward, eyeRotation) * newEntityDistance
            let attributes = entity.GetAttributesInferred world
            entityTransform.Position <- entityPosition
            entityTransform.Size <- attributes.SizeInferred
            entityTransform.Offset <- attributes.OffsetInferred
            if not snaps2dSelected && ImGui.IsCtrlUp ()
            then world <- entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform world
            else world <- entity.SetTransform entityTransform world
        if surnames.Length > 1 then
            world <- entity.SetMountOptWithAdjustment (Some (Relation.makeParent ())) world
        match entity.TryGetProperty (nameof entity.ProbeBounds) world with
        | Some property when property.PropertyType = typeof<Box3> ->
            world <- entity.ResetProbeBounds world
        | Some _ | None -> ()
        selectEntityOpt (Some entity)
        ImGui.SetWindowFocus "Viewport"
        showSelectedEntity <- true

    let private trySaveSelectedGroup filePath =
        try World.writeGroupToFile filePath selectedGroup world
            try let deploymentPath = PathF.Combine (targetDir, PathF.GetRelativePath(targetDir, filePath).Replace("../", ""))
                if Directory.Exists (PathF.GetDirectoryName deploymentPath) then
                    File.Copy (filePath, deploymentPath, true)
            with exn ->
                messageBoxOpt <- Some ("Could not deploy file due to: " + scstring exn)
            groupFilePaths <- Map.add selectedGroup.GroupAddress groupFileDialogState.FilePath groupFilePaths
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
                let worldOld = world
                try if group.Exists world then
                        world <- World.destroyGroupImmediate selectedGroup world
                    let (group, wtemp) = World.readGroup groupDescriptor None selectedScreen world in world <- wtemp
                    selectGroup group
                    match selectedEntityOpt with
                    | Some entity when not (entity.Exists world) || entity.Group <> selectedGroup -> selectEntityOpt None
                    | Some _ | None -> ()
                    groupFilePaths <- Map.add group.GroupAddress groupFileDialogState.FilePath groupFilePaths
                    groupFileDialogState.FileName <- ""
                    true
                with exn ->
                    world <- World.switch worldOld
                    messageBoxOpt <- Some ("Could not load group file due to: " + scstring exn)
                    false

            // error
            | Left exn ->
                messageBoxOpt <- Some ("Could not load group file due to: " + scstring exn)
                false

        // error
        else
            messageBoxOpt <- Some "Cannot load into a protected simulant (such as a group created by the MMCC API)."
            false

    let private tryAutoBoundsSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            snapshot ()
            world <- entity.AutoBounds world
            true
        | Some _ | None -> false

    let private tryReorderSelectedEntity up =
        if String.IsNullOrWhiteSpace entityHierarchySearchStr then
            match selectedEntityOpt with
            | Some entity when entity.Exists world ->
                let peerOpt =
                    if up
                    then World.tryGetPreviousEntity entity world
                    else World.tryGetNextEntity entity world
                match peerOpt with
                | Some peer ->
                    if not (entity.GetProtected world) && not (peer.GetProtected world) then
                        snapshot ()
                        world <- World.swapEntityOrders entity peer world
                    else messageBoxOpt <- Some "Cannot reorder a protected simulant (such as an entity created by the MMCC API)."
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
                messageBoxOpt <- Some "Cannot destroy a protected simulant (such as an entity created by the MMCC API)."
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
                messageBoxOpt <- Some "Cannot cut a protected simulant (such as an entity created by the MMCC API)."
                false
        | Some _ | None -> false

    let private tryCopySelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            World.copyEntityToClipboard entity world
            true
        | Some _ | None -> false

    let private tryPaste atMouse parentOpt =
        snapshot ()
        let snapsEir = if snaps2dSelected then Left snaps2d else Right snaps3d
        let parent = match parentOpt with Some parent -> parent | None -> selectedGroup :> Simulant
        let (entityOpt, wtemp) = World.pasteEntityFromClipboard atMouse newEntityDistance rightClickPosition snapsEir parent world in world <- wtemp
        match entityOpt with
        | Some entity ->
            selectEntityOpt (Some entity)
            ImGui.SetWindowFocus "Viewport"
            showSelectedEntity <- true
            true
        | None -> false

    let private trySetSelectedEntityFamilyStatic static_ =
        let rec setEntityFamilyStatic static_ (entity : Entity) =
            world <- entity.SetStatic static_ world
            for entity in entity.GetChildren world do
                setEntityFamilyStatic static_ entity
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            snapshot ()
            setEntityFamilyStatic static_ entity
        | Some _ | None -> ()

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
            let worldOld = world
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
                        Array.filter (fun line -> not (line.Contains "PackageReference Update=")) |>
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
                        Array.map (fun line -> PathF.Normalize line) |>
                        Array.map (fun line -> line.Trim ())
                    let fsprojProjectLines = // TODO: see if we can pull these from the fsproj as well...
                        ["#r \"../../../../../Nu/Nu.Math/bin/" + Constants.Gaia.BuildName + "/netstandard2.0/Nu.Math.dll\""
                         "#r \"../../../../../Nu/Nu.Pipe/bin/" + Constants.Gaia.BuildName + "/net7.0/Nu.Pipe.dll\""
                         "#r \"../../../../../Nu/Nu/bin/" + Constants.Gaia.BuildName + "/net7.0/Nu.dll\""]
                    let fsprojFsFilePaths =
                        fsprojFileLines |>
                        Array.map (fun line -> line.Trim ()) |>
                        Array.filter (fun line -> line.Contains "Compile Include" && line.Contains ".fs") |>
                        Array.filter (fun line -> line.Contains "Compile Include" && not (line.Contains "Program.fs")) |>
                        Array.map (fun line -> line.Replace ("<Compile Include", "")) |>
                        Array.map (fun line -> line.Replace ("/>", "")) |>
                        Array.map (fun line -> line.Replace ("=", "")) |>
                        Array.map (fun line -> line.Replace ("\"", "")) |>
                        Array.map (fun line -> PathF.Normalize line) |>
                        Array.map (fun line -> line.Trim ())
                    let fsxFileString =
                        String.Join ("\n", Array.map (fun (nugetPath : string) -> "#r \"" + nugetPath + "\"") fsprojNugetPaths) + "\n" +
                        String.Join ("\n", Array.map (fun (filePath : string) -> "#r \"../../../" + filePath + "\"") fsprojDllFilePaths) + "\n" +
                        String.Join ("\n", fsprojProjectLines) + "\n" +
                        String.Join ("\n", Array.map (fun (filePath : string) -> "#load \"../../../" + filePath + "\"") fsprojFsFilePaths)
                    let fsProjectNoWarn = "--nowarn:FS9;FS1178;FS3391;FS3536;FS3560" // TODO: pull these from fsproj!
                    // TODO: add warnings as errors, too?
                    Log.info ("Compiling code via generated F# script:\n" + fsxFileString)
                    let defaultArgs = [|"fsi.exe"; "--debug+"; "--debug:full"; "--optimize-"; "--tailcalls-"; "--multiemit+"; "--gui-"; fsProjectNoWarn|] // TODO: see can we use --wwarnon as well.
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
                        focusedPropertyDescriptorOpt <- None // drop any reference to old property type
                        world <- World.updateLateBindings session.DynamicAssemblies world // replace references to old types
                        Log.info "Code updated."
                    with _ ->
                        let error = string errorStream
                        Log.info ("Failed to compile code due to (see full output in the console):\n" + error)
                        world <- World.switch worldOld
            with exn ->
                Log.trace ("Failed to inspect for F# code due to: " + scstring exn)
                world <- World.switch worldOld
        else messageBoxOpt <- Some "Code reloading not allowed by current plugin. This is likely because you're using the GaiaPlugin which doesn't allow it."

    let private tryReloadAll () =
        tryReloadAssets ()
        tryReloadCode ()

    let private resetEye () =
        desiredEye2dCenter <- v2Zero
        desiredEye3dCenter <- Constants.Engine.Eye3dCenterDefault
        desiredEye3dRotation <- quatIdentity

    let private toggleAdvancing () =
        if not world.Advancing then snapshot ()
        world <- World.setAdvancing (not world.Advancing) world

    let private trySelectTargetDirAndMakeNuPluginFromFilePathOpt filePathOpt =
        let gaiaDir = Directory.GetCurrentDirectory ()
        let filePathAndDirNameAndTypesOpt =
            if not (String.IsNullOrWhiteSpace filePathOpt) then
                let filePath = filePathOpt
                try let dirName = PathF.GetDirectoryName filePath
                    try Directory.SetCurrentDirectory dirName
                        let assembly = Assembly.Load (File.ReadAllBytes filePath)
                        Right (Some (filePath, dirName, assembly.GetTypes ()))
                    with _ ->
                        let assembly = Assembly.LoadFrom filePath
                        Right (Some (filePath, dirName, assembly.GetTypes ()))
                with exn ->
                    Log.info ("Failed to load Nu game project from '" + filePath + "' due to: " + scstring exn)
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
        let gaiaState =
            try if File.Exists Constants.Gaia.StateFilePath
                then scvalue (File.ReadAllText Constants.Gaia.StateFilePath)
                else GaiaState.defaultState
            with _ -> GaiaState.defaultState
        let gaiaDirectory = Directory.GetCurrentDirectory ()
        match trySelectTargetDirAndMakeNuPluginFromFilePathOpt gaiaState.ProjectDllPath with
        | Right (Some (filePath, targetDir, plugin)) ->
            Constants.Override.fromAppConfig filePath
            try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
            with _ -> Log.info "Could not save gaia state."
            (gaiaState, targetDir, plugin)
        | Right None ->
            try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
            with _ -> Log.info "Could not save gaia state."
            (gaiaState, ".", gaiaPlugin)
        | Left () ->
            if not (String.IsNullOrWhiteSpace gaiaState.ProjectDllPath) then
                Log.trace ("Invalid Nu Assembly: " + gaiaState.ProjectDllPath)
            (GaiaState.defaultState, ".", gaiaPlugin)

    let private tryMakeWorld sdlDeps worldConfig (plugin : NuPlugin) =

        // attempt to make the world
        match World.tryMake sdlDeps worldConfig plugin with
        | Right world ->

            // initialize event filter as not to flood the log
            let world = World.setEventFilter Constants.Gaia.EventFilter world

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
                match Game.GetDesiredScreen world with
                | Desire screen -> (screen, world)
                | DesireNone ->
                    let (screen, world) = World.createScreen (Some "Screen") world
                    let world = Game.SetDesiredScreen (Desire screen) world
                    (screen, world)
                | DesireIgnore ->
                    let (screen, world) = World.createScreen (Some "Screen") world
                    let world = World.setSelectedScreen screen world
                    (screen, world)

            // proceed directly to idle state
            let world = World.selectScreen (IdlingState world.GameTime) screen world
            Right (screen, world)

        // error
        | Left error -> Left error

    let private tryMakeSdlDeps () =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        match SdlDeps.tryMake sdlConfig with
        | Left msg -> Left msg
        | Right sdlDeps -> Right (sdlConfig, sdlDeps)

    (* ImGui Callback Functions *)

    let private updateEntityContext () =
        if canEditWithMouse () then
            if ImGui.IsMouseReleased ImGuiMouseButton.Right then
                let mousePosition = World.getMousePosition world
                let _ = tryMousePick mousePosition
                rightClickPosition <- mousePosition
                showEntityContextMenu <- true

    let private updateEntityDrag () =

        if canEditWithMouse () then

            if ImGui.IsMouseClicked ImGuiMouseButton.Left then
                let mousePosition = World.getMousePosition world
                match tryMousePick mousePosition with
                | Some (_, entity) ->
                    if entity.GetIs2d world then
                        snapshot ()
                        if World.isKeyboardAltDown world then
                            let viewport = World.getViewport world
                            let eyeCenter = World.getEye2dCenter world
                            let eyeSize = World.getEye2dSize world
                            let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                            let entityDegrees = if entity.MountExists world then entity.GetDegreesLocal world else entity.GetDegrees world
                            dragEntityState <- DragEntityRotation2d (DateTimeOffset.Now, mousePositionWorld, entityDegrees.Z + mousePositionWorld.Y, entity)
                        else
                            let entity =
                                if  World.isKeyboardCtrlDown world &&
                                    tryCopySelectedEntity () &&
                                    tryPaste (PasteAt (entity.GetPosition world)) (Option.map cast newEntityParentOpt) then
                                    Option.defaultValue entity selectedEntityOpt
                                else entity
                            let viewport = World.getViewport world
                            let eyeCenter = World.getEye2dCenter world
                            let eyeSize = World.getEye2dSize world
                            let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                            let entityPosition = entity.GetPosition world
                            dragEntityState <- DragEntityPosition2d (DateTimeOffset.Now, mousePositionWorld, entityPosition.V2 + mousePositionWorld, entity)
                | None -> ()

            match dragEntityState with
            | DragEntityPosition2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Gaia.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityPosition = (entityDragOffset - mousePositionWorldOriginal) + (mousePositionWorld - mousePositionWorldOriginal)
                    let entityPositionSnapped =
                        if snaps2dSelected && ImGui.IsCtrlUp ()
                        then Math.SnapF3d (Triple.fst (getSnaps ())) entityPosition.V3
                        else entityPosition.V3
                    let entityPosition = entity.GetPosition world
                    let entityPositionDelta = entityPositionSnapped - entityPosition
                    let entityPositionConstrained = entityPosition + entityPositionDelta
                    match Option.bind (tryResolve entity) (entity.GetMountOpt world) with
                    | Some parent ->
                        let entityPositionLocal = Vector3.Transform (entityPositionConstrained, (parent.GetAffineMatrix world).Inverted)
                        world <- entity.SetPositionLocal entityPositionLocal world
                    | None ->
                        world <- entity.SetPosition entityPositionConstrained world
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                        world <- entity.SetLinearVelocity v3Zero world
                        world <- entity.SetAngularVelocity v3Zero world
            | DragEntityRotation2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Gaia.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityDegree = (entityDragOffset - mousePositionWorldOriginal.Y) + (mousePositionWorld.Y - mousePositionWorldOriginal.Y)
                    let entityDegreeSnapped =
                        if snaps2dSelected && ImGui.IsCtrlUp ()
                        then Math.SnapF (Triple.snd (getSnaps ())) entityDegree
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

        if ImGui.IsMouseReleased ImGuiMouseButton.Left then
            match dragEntityState with
            | DragEntityPosition2d _ | DragEntityRotation2d _ -> dragEntityState <- DragEntityInactive
            | DragEntityInactive -> ()

    let private updateEyeDrag () =

        if canEditWithMouse () then

            if ImGui.IsMouseClicked ImGuiMouseButton.Middle then
                let mousePositionScreen = World.getMousePosition2dScreen world
                let dragState = DragEye2dCenter (World.getEye2dCenter world + mousePositionScreen, mousePositionScreen)
                dragEyeState <- dragState

            match dragEyeState with
            | DragEye2dCenter (entityDragOffset, mousePositionScreenOrig) ->
                let mousePositionScreen = World.getMousePosition2dScreen world
                desiredEye2dCenter <- (entityDragOffset - mousePositionScreenOrig) + -Constants.Gaia.EyeSpeed * (mousePositionScreen - mousePositionScreenOrig)
                dragEyeState <- DragEye2dCenter (entityDragOffset, mousePositionScreenOrig)
            | DragEyeInactive -> ()

        if ImGui.IsMouseReleased ImGuiMouseButton.Middle then
            match dragEyeState with
            | DragEye2dCenter _ -> dragEyeState <- DragEyeInactive
            | DragEyeInactive -> ()

    let private updateEyeTravel () =
        if canEditWithKeyboard () then
            let position = World.getEye3dCenter world
            let rotation = World.getEye3dRotation world
            let moveSpeed =
                if ImGui.IsKeyDown ImGuiKey.Enter && ImGui.IsShiftDown () then 5.0f
                elif ImGui.IsKeyDown ImGuiKey.Enter then 0.5f
                elif ImGui.IsShiftDown () then 0.02f
                else 0.12f
            let turnSpeed =
                if ImGui.IsShiftDown () && not (ImGui.IsKeyDown ImGuiKey.Enter) then 0.025f
                else 0.05f
            if ImGui.IsKeyDown ImGuiKey.W && ImGui.IsCtrlUp () then
                desiredEye3dCenter <- position + Vector3.Transform (v3Forward, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.S && ImGui.IsCtrlUp () then
                desiredEye3dCenter <- position + Vector3.Transform (v3Back, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.A && ImGui.IsCtrlUp () then
                desiredEye3dCenter <- position + Vector3.Transform (v3Left, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.D && ImGui.IsCtrlUp () then
                desiredEye3dCenter <- position + Vector3.Transform (v3Right, rotation) * moveSpeed
            if ImGui.IsKeyDown (if alternativeEyeTravelInput then ImGuiKey.UpArrow else ImGuiKey.E) && ImGui.IsCtrlUp () then
                let rotation' = rotation * Quaternion.CreateFromAxisAngle (v3Right, turnSpeed)
                if Vector3.Dot (rotation'.Forward, v3Up) < 0.995f then desiredEye3dRotation <- rotation'
            if ImGui.IsKeyDown (if alternativeEyeTravelInput then ImGuiKey.DownArrow else ImGuiKey.Q) && ImGui.IsCtrlUp () then
                let rotation' = rotation * Quaternion.CreateFromAxisAngle (v3Left, turnSpeed)
                if Vector3.Dot (rotation'.Forward, v3Down) < 0.995f then desiredEye3dRotation <- rotation'
            if ImGui.IsKeyDown (if alternativeEyeTravelInput then ImGuiKey.E else ImGuiKey.UpArrow) && ImGui.IsAltUp () then
                desiredEye3dCenter <- position + Vector3.Transform (v3Up, rotation) * moveSpeed
            if ImGui.IsKeyDown (if alternativeEyeTravelInput then ImGuiKey.Q else ImGuiKey.DownArrow) && ImGui.IsAltUp () then
                desiredEye3dCenter <- position + Vector3.Transform (v3Down, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.LeftArrow && ImGui.IsAltUp () then
                desiredEye3dRotation <- Quaternion.CreateFromAxisAngle (v3Up, turnSpeed) * rotation
            if ImGui.IsKeyDown ImGuiKey.RightArrow && ImGui.IsAltUp () then
                desiredEye3dRotation <- Quaternion.CreateFromAxisAngle (v3Down, turnSpeed) * rotation

    let private updateHotkeys entityHierarchyFocused =
        if not (modal ()) then
            if ImGui.IsKeyPressed ImGuiKey.F2 && selectedEntityOpt.IsSome && not (selectedEntityOpt.Value.GetProtected world) then showRenameEntityDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F3 then snaps2dSelected <- not snaps2dSelected
            elif ImGui.IsKeyPressed ImGuiKey.F4 && ImGui.IsAltDown () then showConfirmExitDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F5 then toggleAdvancing ()
            elif ImGui.IsKeyPressed ImGuiKey.F6 then editWhileAdvancing <- not editWhileAdvancing
            elif ImGui.IsKeyPressed ImGuiKey.F8 then reloadAssetsRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F9 then reloadCodeRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F11 then fullScreen <- not fullScreen
            elif ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlDown () && ImGui.IsShiftUp () then showNewGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftUp () then showOpenGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlDown () && ImGui.IsShiftUp () then showSaveGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.B && ImGui.IsCtrlDown () && ImGui.IsShiftUp () then tryAutoBoundsSelectedEntity () |> ignore<bool>
            elif ImGui.IsKeyPressed ImGuiKey.R && ImGui.IsCtrlDown () && ImGui.IsShiftUp () then reloadAllRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F && ImGui.IsCtrlDown () && ImGui.IsShiftUp () then searchEntityHierarchy ()
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftDown () then showOpenProjectDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F && ImGui.IsCtrlDown () && ImGui.IsShiftDown () then freezeEntities ()
            elif ImGui.IsKeyPressed ImGuiKey.T && ImGui.IsCtrlDown () && ImGui.IsShiftDown () then thawEntities ()
            elif ImGui.IsKeyPressed ImGuiKey.R && ImGui.IsCtrlDown () && ImGui.IsShiftDown () then rerenderLightMaps ()
            elif ImGui.IsKeyPressed ImGuiKey.UpArrow && ImGui.IsAltDown () then tryReorderSelectedEntity true
            elif ImGui.IsKeyPressed ImGuiKey.DownArrow && ImGui.IsAltDown () then tryReorderSelectedEntity false
            elif not (ImGui.GetIO ()).WantCaptureKeyboardGlobal || entityHierarchyFocused then
                if ImGui.IsKeyPressed ImGuiKey.Z && ImGui.IsCtrlDown () then tryUndo () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Y && ImGui.IsCtrlDown () then tryRedo () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.X && ImGui.IsCtrlDown () then tryCutSelectedEntity () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlDown () then tryCopySelectedEntity () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.V && ImGui.IsCtrlDown () then tryPaste PasteAtLook (Option.map cast newEntityParentOpt) |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlDown () then createEntity false false
                elif ImGui.IsKeyPressed ImGuiKey.Delete then tryDeleteSelectedEntity () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Escape then
                    if not (String.IsNullOrWhiteSpace entityHierarchySearchStr)
                    then entityHierarchySearchStr <- ""
                    else deselectEntity ()

    let private imGuiEntity branch searchActive (entity : Entity) =
        let selected = match selectedEntityOpt with Some selectedEntity -> entity = selectedEntity | None -> false
        let treeNodeFlags =
            (if selected then ImGuiTreeNodeFlags.Selected else ImGuiTreeNodeFlags.None) |||
            (if not branch || searchActive then ImGuiTreeNodeFlags.Leaf else ImGuiTreeNodeFlags.None) |||
            (if newEntityParentOpt = Some entity && DateTimeOffset.Now.Millisecond / 400 % 2 = 0 then ImGuiTreeNodeFlags.Bullet else ImGuiTreeNodeFlags.None) |||
            ImGuiTreeNodeFlags.OpenOnArrow
        if not searchActive then
            if expandEntityHierarchy then ImGui.SetNextItemOpen true
            if collapseEntityHierarchy then ImGui.SetNextItemOpen false
        match selectedEntityOpt with
        | Some selectedEntity when showSelectedEntity ->
            let relation = relate entity selectedEntity
            if  Array.notExists (fun t -> t = Parent || t = Current) relation.Links &&
                relation.Links.Length > 0 then
                ImGui.SetNextItemOpen true
        | Some _ | None -> ()
        let expanded = ImGui.TreeNodeEx (entity.Name, treeNodeFlags)
        if showSelectedEntity && Some entity = selectedEntityOpt then
            ImGui.SetScrollHereY 0.5f
            showSelectedEntity <- false
        if ImGui.IsKeyPressed ImGuiKey.Space && ImGui.IsItemFocused () && ImGui.IsWindowFocused () then
            selectEntityOpt (Some entity)
        if ImGui.IsMouseReleased ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            selectEntityOpt (Some entity)
        if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            if not (entity.GetAbsolute world) then
                if entity.GetIs2d world then
                    desiredEye2dCenter <- (entity.GetPerimeterCenter world).V2
                else
                    let eyeRotation = World.getEye3dRotation world
                    let eyeCenterOffset = Vector3.Transform (v3Back * newEntityDistance, eyeRotation)
                    desiredEye3dCenter <- entity.GetPosition world + eyeCenterOffset
        let popupContextItemTitle = "##popupContextItem"
        let mutable openPopupContextItemWhenUnselected = false
        if ImGui.BeginPopupContextItem popupContextItemTitle then
            if ImGui.IsMouseReleased ImGuiMouseButton.Right then openPopupContextItemWhenUnselected <- true
            selectEntityOpt (Some entity)
            if ImGui.MenuItem "Create" then createEntity false true
            if ImGui.MenuItem "Delete" then tryDeleteSelectedEntity () |> ignore<bool>
            ImGui.Separator ()
            if ImGui.MenuItem "Cut" then tryCutSelectedEntity () |> ignore<bool>
            if ImGui.MenuItem "Copy" then tryCopySelectedEntity () |> ignore<bool>
            if ImGui.MenuItem "Paste" then tryPaste PasteAtLook (Some entity) |> ignore<bool>
            ImGui.Separator ()
            if newEntityParentOpt = Some entity then
                if ImGui.MenuItem "Reset Creation Parent" then newEntityParentOpt <- None; showEntityContextMenu <- false
            else
                if ImGui.MenuItem "Set as Creation Parent" then newEntityParentOpt <- selectedEntityOpt; showEntityContextMenu <- false
            ImGui.Separator ()
            match selectedEntityOpt with
            | Some entity when entity.Exists world ->
                if entity.GetStatic world
                then if ImGui.MenuItem "Make Entity Family Non-Static" then trySetSelectedEntityFamilyStatic false
                else if ImGui.MenuItem "Make Entity Family Static" then trySetSelectedEntityFamilyStatic true
            | Some _ | None -> ()
            ImGui.EndPopup ()
        if openPopupContextItemWhenUnselected then
            ImGui.OpenPopup popupContextItemTitle
        if ImGui.BeginDragDropSource () then
            let entityAddressStr = entity.EntityAddress |> scstring |> Symbol.distill
            dragDropPayloadOpt <- Some entityAddressStr
            ImGui.Text entity.Name
            ImGui.SetDragDropPayload ("Entity", IntPtr.Zero, 0u) |> ignore<bool>
            ImGui.EndDragDropSource ()
        if ImGui.BeginDragDropTarget () then
            if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr) then
                match dragDropPayloadOpt with
                | Some payload ->
                    let sourceEntityAddressStr = payload
                    let sourceEntity = Nu.Entity sourceEntityAddressStr
                    if not (sourceEntity.GetProtected world) then
                        if ImGui.IsAltDown () then
                            let next = Nu.Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                            let previousOpt = World.tryGetPreviousEntity next world
                            let parentOpt = match next.Parent with :? Entity as parent -> Some parent | _ -> None
                            if not ((scstring parentOpt).Contains (scstring sourceEntity)) then
                                let mountOpt = match parentOpt with Some _ -> Some (Relation.makeParent ()) | None -> None
                                let sourceEntity' = match parentOpt with Some parent -> parent / sourceEntity.Name | None -> selectedGroup / sourceEntity.Name
                                if not (sourceEntity'.Exists world) then
                                    snapshot ()
                                    world <- World.insertEntityOrder sourceEntity previousOpt next world
                                    world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                                    world <- sourceEntity'.SetMountOptWithAdjustment mountOpt world
                                    if newEntityParentOpt = Some sourceEntity then newEntityParentOpt <- Some sourceEntity'
                                    selectEntityOpt (Some sourceEntity')
                                    showSelectedEntity <- true
                                else messageBoxOpt <- Some "Cannot reparent an entity where the parent entity contains a child with the same name."
                        else
                            let parent = Nu.Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                            let sourceEntity' = parent / sourceEntity.Name
                            if not ((scstring parent).Contains (scstring sourceEntity)) then
                                if not (sourceEntity'.Exists world) then
                                    snapshot ()
                                    world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                                    world <- sourceEntity'.SetMountOptWithAdjustment (Some (Relation.makeParent ())) world
                                    if newEntityParentOpt = Some sourceEntity then newEntityParentOpt <- Some sourceEntity'
                                    selectEntityOpt (Some sourceEntity')
                                    showSelectedEntity <- true
                                else messageBoxOpt <- Some "Cannot reparent an entity where the parent entity contains a child with the same name."
                    else messageBoxOpt <- Some "Cannot relocate a protected simulant (such as an entity created by the MMCC API)."
                | None -> ()
        if entity.Has<FreezerFacet> world then
            let frozen = entity.GetFrozen world
            let (text, color) = if frozen then ("Thaw", Color.CornflowerBlue) else ("Freeze", Color.DarkRed)
            ImGui.SameLine ()
            ImGui.Separator ()
            ImGui.SameLine ()
            ImGui.PushStyleColor (ImGuiCol.Button, color.Abgr)
            ImGui.PushID ("##frozen" + scstring entity)
            if ImGui.SmallButton text then
                snapshot ()
                world <- entity.SetFrozen (not frozen) world
            ImGui.PopID ()
            ImGui.PopStyleColor ()
        expanded

    let rec private imGuiEntityHierarchy (entity : Entity) =
        if entity.Exists world then // NOTE: entity may have been moved during this process.
            let searchActive =
                not (String.IsNullOrWhiteSpace entityHierarchySearchStr)
            let visible =
                not searchActive || entity.Name.ToLowerInvariant().Contains (entityHierarchySearchStr.ToLowerInvariant ())
            let expanded =
                if visible then
                    let branch = entity.HasChildren world
                    imGuiEntity branch searchActive entity
                else false
            if expanded || searchActive then
                let children =
                    entity.GetChildren world |>
                    Array.ofSeq |>
                    Array.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity)) |>
                    Array.sortBy fst |>
                    Array.map snd
                for child in children do
                    imGuiEntityHierarchy child
                if visible then
                    ImGui.TreePop ()

    let private imGuiEditMaterialPropertiesProperty mp propertyDescriptor simulant =

        // edit albedo
        let mutable isSome = ValueOption.isSome mp.AlbedoOpt
        if ImGui.Checkbox ("##mpAlbedoIsSome", &isSome) then
            if isSome
            then setPropertyValue { mp with AlbedoOpt = ValueSome Constants.Render.AlbedoDefault } propertyDescriptor simulant
            else setPropertyValue { mp with AlbedoOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.AlbedoOpt with
            | ValueSome albedo ->
                let mutable v = v4 albedo.R albedo.G albedo.B albedo.A
                ImGui.SameLine ()
                if ImGui.ColorEdit4 ("##mpAlbedo", &v) then setPropertyValue { mp with AlbedoOpt = ValueSome (color v.X v.Y v.Z v.W) } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "AlbedoOpt"

        // edit roughness
        let mutable isSome = ValueOption.isSome mp.RoughnessOpt
        if ImGui.Checkbox ("##mpRoughnessIsSome", &isSome) then
            if isSome
            then setPropertyValue { mp with RoughnessOpt = ValueSome Constants.Render.RoughnessDefault } propertyDescriptor simulant
            else setPropertyValue { mp with RoughnessOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.RoughnessOpt with
            | ValueSome roughness ->
                let mutable roughness = roughness
                ImGui.SameLine ()
                if ImGui.SliderFloat ("##mpRoughness", &roughness, 0.0f, 10.0f) then setPropertyValue { mp with RoughnessOpt = ValueSome roughness } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "RoughnessOpt"

        // edit metallic
        let mutable isSome = ValueOption.isSome mp.MetallicOpt
        if ImGui.Checkbox ("##mpMetallicIsSome", &isSome) then
            if isSome
            then setPropertyValue { mp with MetallicOpt = ValueSome Constants.Render.MetallicDefault } propertyDescriptor simulant
            else setPropertyValue { mp with MetallicOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.MetallicOpt with
            | ValueSome metallic ->
                let mutable metallic = metallic
                ImGui.SameLine ()
                if ImGui.SliderFloat ("##mpMetallic", &metallic, 0.0f, 10.0f) then setPropertyValue { mp with MetallicOpt = ValueSome metallic } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "MetallicOpt"

        // edit ambient occlusion
        let mutable isSome = ValueOption.isSome mp.AmbientOcclusionOpt
        if ImGui.Checkbox ("##mpAmbientOcclusionIsSome", &isSome) then
            if isSome
            then setPropertyValue { mp with AmbientOcclusionOpt = ValueSome Constants.Render.AmbientOcclusionDefault } propertyDescriptor simulant
            else setPropertyValue { mp with AmbientOcclusionOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.AmbientOcclusionOpt with
            | ValueSome ambientOcclusion ->
                let mutable ambientOcclusion = ambientOcclusion
                ImGui.SameLine ()
                if ImGui.SliderFloat ("##mpAmbientOcclusion", &ambientOcclusion, 0.0f, 10.0f) then setPropertyValue { mp with AmbientOcclusionOpt = ValueSome ambientOcclusion } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "AmbientOcclusionOpt"

        // edit emission
        let mutable isSome = ValueOption.isSome mp.EmissionOpt
        if ImGui.Checkbox ("##mpEmissionIsSome", &isSome) then
            if isSome
            then setPropertyValue { mp with EmissionOpt = ValueSome Constants.Render.EmissionDefault } propertyDescriptor simulant
            else setPropertyValue { mp with EmissionOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.EmissionOpt with
            | ValueSome emission ->
                let mutable emission = emission
                ImGui.SameLine ()
                if ImGui.SliderFloat ("##mpEmission", &emission, 0.0f, 10.0f) then setPropertyValue { mp with EmissionOpt = ValueSome emission } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "EmissionOpt"

        // edit height
        let mutable isSome = ValueOption.isSome mp.HeightOpt
        if ImGui.Checkbox ("##mpHeightIsSome", &isSome) then
            if isSome
            then setPropertyValue { mp with HeightOpt = ValueSome Constants.Render.HeightDefault } propertyDescriptor simulant
            else setPropertyValue { mp with HeightOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.HeightOpt with
            | ValueSome height ->
                let mutable height = height
                ImGui.SameLine ()
                if ImGui.SliderFloat ("##mpHeight", &height, 0.0f, 10.0f) then setPropertyValue { mp with HeightOpt = ValueSome height } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "HeightOpt"

        // edit ignore light maps
        let mutable isSome = ValueOption.isSome mp.IgnoreLightMapsOpt
        if ImGui.Checkbox ("##mpIgnoreLightMapsIsSome", &isSome) then
            if isSome
            then setPropertyValue { mp with IgnoreLightMapsOpt = ValueSome false } propertyDescriptor simulant
            else setPropertyValue { mp with IgnoreLightMapsOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.IgnoreLightMapsOpt with
            | ValueSome ignoreLightMaps ->
                let mutable ignoreLightMaps = ignoreLightMaps
                ImGui.SameLine ()
                if ImGui.Checkbox ("##mpIgnoreLightMaps", &ignoreLightMaps) then setPropertyValue { mp with IgnoreLightMapsOpt = ValueSome ignoreLightMaps } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "IgnoreLightMapsOpt"

        // edit opaque distance
        let mutable isSome = ValueOption.isSome mp.OpaqueDistanceOpt
        if ImGui.Checkbox ("##mpOpaqueDistanceIsSome", &isSome) then
            if isSome
            then setPropertyValue { mp with OpaqueDistanceOpt = ValueSome Constants.Render.OpaqueDistanceDefault } propertyDescriptor simulant
            else setPropertyValue { mp with OpaqueDistanceOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.OpaqueDistanceOpt with
            | ValueSome opaqueDistance ->
                let mutable opaqueDistance = opaqueDistance
                ImGui.SameLine ()
                if ImGui.InputFloat ("##mpOpaqueDistance", &opaqueDistance) then setPropertyValue { mp with OpaqueDistanceOpt = ValueSome opaqueDistance } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "OpaqueDistanceOpt"

    let private imGuiEditMaterialProperty m propertyDescriptor simulant =

        // edit albedo image
        let mutable isSome = ValueOption.isSome m.AlbedoImageOpt
        if ImGui.Checkbox ("##matAlbedoImageIsSome", &isSome) then
            if isSome
            then setPropertyValue { m with AlbedoImageOpt = ValueSome Assets.Default.MaterialAlbedo } propertyDescriptor simulant
            else setPropertyValue { m with AlbedoImageOpt = ValueNone } propertyDescriptor simulant
        else
            match m.AlbedoImageOpt with
            | ValueSome albedoImage ->
                let mutable propertyValueStr = scstring albedoImage
                ImGui.SameLine ()
                if ImGui.InputText ("##matAlbedoImage", &propertyValueStr, 4096u) then
                    let worldsPast' = worldsPast
                    try let propertyValue = scvalue propertyValueStr
                        setPropertyValue { m with AlbedoImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                    with _ ->
                        worldsPast <- worldsPast'
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                        match dragDropPayloadOpt with
                        | Some payload ->
                            let worldsPast' = worldsPast
                            try let propertyValueEscaped = payload
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = scvalue propertyValueUnescaped
                                setPropertyValue { m with AlbedoImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                            with _ ->
                                worldsPast <- worldsPast'
                        | None -> ()
                    ImGui.EndDragDropTarget ()
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                ImGui.SameLine ()
                ImGui.PushID ("##matAlbedoImagePick")
                if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                ImGui.PopID ()
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "AlbedoImageOpt"

        // edit roughness image
        let mutable isSome = ValueOption.isSome m.RoughnessImageOpt
        if ImGui.Checkbox ("##matRoughnessImageIsSome", &isSome) then
            if isSome
            then setPropertyValue { m with RoughnessImageOpt = ValueSome Assets.Default.MaterialRoughness } propertyDescriptor simulant
            else setPropertyValue { m with RoughnessImageOpt = ValueNone } propertyDescriptor simulant
        else
            match m.RoughnessImageOpt with
            | ValueSome roughnessImage ->
                let mutable propertyValueStr = scstring roughnessImage
                ImGui.SameLine ()
                if ImGui.InputText ("##matRoughnessImage", &propertyValueStr, 4096u) then
                    let worldsPast' = worldsPast
                    try let propertyValue = scvalue propertyValueStr
                        setPropertyValue { m with RoughnessImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                    with _ ->
                        worldsPast <- worldsPast'
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                        match dragDropPayloadOpt with
                        | Some payload ->
                            let worldsPast' = worldsPast
                            try let propertyValueEscaped = payload
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = scvalue propertyValueUnescaped
                                setPropertyValue { m with RoughnessImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                            with _ ->
                                worldsPast <- worldsPast'
                        | None -> ()
                    ImGui.EndDragDropTarget ()
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                ImGui.SameLine ()
                ImGui.PushID ("##matRoughnessImagePick")
                if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                ImGui.PopID ()
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "RoughnessImageOpt"

        // edit metallic image
        let mutable isSome = ValueOption.isSome m.MetallicImageOpt
        if ImGui.Checkbox ("##matMetallicImageIsSome", &isSome) then
            if isSome
            then setPropertyValue { m with MetallicImageOpt = ValueSome Assets.Default.MaterialMetallic } propertyDescriptor simulant
            else setPropertyValue { m with MetallicImageOpt = ValueNone } propertyDescriptor simulant
        else
            match m.MetallicImageOpt with
            | ValueSome metallicImage ->
                let mutable propertyValueStr = scstring metallicImage
                ImGui.SameLine ()
                if ImGui.InputText ("##matMetallicImage", &propertyValueStr, 4096u) then
                    let worldsPast' = worldsPast
                    try let propertyValue = scvalue propertyValueStr
                        setPropertyValue { m with MetallicImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                    with _ ->
                        worldsPast <- worldsPast'
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                        match dragDropPayloadOpt with
                        | Some payload ->
                            let worldsPast' = worldsPast
                            try let propertyValueEscaped = payload
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = scvalue propertyValueUnescaped
                                setPropertyValue { m with MetallicImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                            with _ ->
                                worldsPast <- worldsPast'
                        | None -> ()
                    ImGui.EndDragDropTarget ()
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                ImGui.SameLine ()
                ImGui.PushID ("##matMetallicImagePick")
                if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                ImGui.PopID ()
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "MetallicImageOpt"

        // edit ambient occlusion image
        let mutable isSome = ValueOption.isSome m.AmbientOcclusionImageOpt
        if ImGui.Checkbox ("##matAmbientOcclusionImageIsSome", &isSome) then
            if isSome
            then setPropertyValue { m with AmbientOcclusionImageOpt = ValueSome Assets.Default.MaterialAmbientOcclusion } propertyDescriptor simulant
            else setPropertyValue { m with AmbientOcclusionImageOpt = ValueNone } propertyDescriptor simulant
        else
            match m.AmbientOcclusionImageOpt with
            | ValueSome ambientOcclusionImage ->
                let mutable propertyValueStr = scstring ambientOcclusionImage
                ImGui.SameLine ()
                if ImGui.InputText ("##matAmbientOcclusionImage", &propertyValueStr, 4096u) then
                    let worldsPast' = worldsPast
                    try let propertyValue = scvalue propertyValueStr
                        setPropertyValue { m with AmbientOcclusionImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                    with _ ->
                        worldsPast <- worldsPast'
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                        match dragDropPayloadOpt with
                        | Some payload ->
                            let worldsPast' = worldsPast
                            try let propertyValueEscaped = payload
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = scvalue propertyValueUnescaped
                                setPropertyValue { m with AmbientOcclusionImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                            with _ ->
                                worldsPast <- worldsPast'
                        | None -> ()
                    ImGui.EndDragDropTarget ()
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                ImGui.SameLine ()
                ImGui.PushID ("##matAmbientOcclusionImagePick")
                if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                ImGui.PopID ()
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "AmbientOcclusionImageOpt"

        // edit emission image
        let mutable isSome = ValueOption.isSome m.EmissionImageOpt
        if ImGui.Checkbox ("##matEmissionImageIsSome", &isSome) then
            if isSome
            then setPropertyValue { m with EmissionImageOpt = ValueSome Assets.Default.MaterialEmission } propertyDescriptor simulant
            else setPropertyValue { m with EmissionImageOpt = ValueNone } propertyDescriptor simulant
        else
            match m.EmissionImageOpt with
            | ValueSome emissionImage ->
                let mutable propertyValueStr = scstring emissionImage
                ImGui.SameLine ()
                if ImGui.InputText ("##matEmissionImage", &propertyValueStr, 4096u) then
                    let worldsPast' = worldsPast
                    try let propertyValue = scvalue propertyValueStr
                        setPropertyValue { m with EmissionImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                    with _ ->
                        worldsPast <- worldsPast'
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                        match dragDropPayloadOpt with
                        | Some payload ->
                            let worldsPast' = worldsPast
                            try let propertyValueEscaped = payload
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = scvalue propertyValueUnescaped
                                setPropertyValue { m with EmissionImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                            with _ ->
                                worldsPast <- worldsPast'
                        | None -> ()
                    ImGui.EndDragDropTarget ()
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                ImGui.SameLine ()
                ImGui.PushID ("##matEmissionImagePick")
                if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                ImGui.PopID ()
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "EmissionImageOpt"

        // edit normal image
        let mutable isSome = ValueOption.isSome m.NormalImageOpt
        if ImGui.Checkbox ("##matNormalImageIsSome", &isSome) then
            if isSome
            then setPropertyValue { m with NormalImageOpt = ValueSome Assets.Default.MaterialNormal } propertyDescriptor simulant
            else setPropertyValue { m with NormalImageOpt = ValueNone } propertyDescriptor simulant
        else
            match m.NormalImageOpt with
            | ValueSome normalImage ->
                let mutable propertyValueStr = scstring normalImage
                ImGui.SameLine ()
                if ImGui.InputText ("##matNormalImage", &propertyValueStr, 4096u) then
                    let worldsPast' = worldsPast
                    try let propertyValue = scvalue propertyValueStr
                        setPropertyValue { m with NormalImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                    with _ ->
                        worldsPast <- worldsPast'
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                        match dragDropPayloadOpt with
                        | Some payload ->
                            let worldsPast' = worldsPast
                            try let propertyValueEscaped = payload
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = scvalue propertyValueUnescaped
                                setPropertyValue { m with NormalImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                            with _ ->
                                worldsPast <- worldsPast'
                        | None -> ()
                    ImGui.EndDragDropTarget ()
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                ImGui.SameLine ()
                ImGui.PushID ("##matNormalImagePick")
                if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                ImGui.PopID ()
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "NormalImageOpt"

        // edit height image
        let mutable isSome = ValueOption.isSome m.HeightImageOpt
        if ImGui.Checkbox ("##matHeightImageIsSome", &isSome) then
            if isSome
            then setPropertyValue { m with HeightImageOpt = ValueSome Assets.Default.MaterialHeight } propertyDescriptor simulant
            else setPropertyValue { m with HeightImageOpt = ValueNone } propertyDescriptor simulant
        else
            match m.HeightImageOpt with
            | ValueSome heightImage ->
                let mutable propertyValueStr = scstring heightImage
                ImGui.SameLine ()
                if ImGui.InputText ("##matHeightImage", &propertyValueStr, 4096u) then
                    let worldsPast' = worldsPast
                    try let propertyValue = scvalue propertyValueStr
                        setPropertyValue { m with HeightImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                    with _ ->
                        worldsPast <- worldsPast'
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                        match dragDropPayloadOpt with
                        | Some payload ->
                            let worldsPast' = worldsPast
                            try let propertyValueEscaped = payload
                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                let propertyValue = scvalue propertyValueUnescaped
                                setPropertyValue { m with HeightImageOpt = ValueSome propertyValue } propertyDescriptor simulant
                            with _ ->
                                worldsPast <- worldsPast'
                        | None -> ()
                    ImGui.EndDragDropTarget ()
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                ImGui.SameLine ()
                ImGui.PushID ("##matHeightImagePick")
                if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                ImGui.PopID ()
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
        ImGui.SameLine ()
        ImGui.Text "HeightImageOpt"

        // edit two-sided
        let mutable isSome = ValueOption.isSome m.TwoSidedOpt
        if ImGui.Checkbox ("##matTwoSidedIsSome", &isSome) then
            if isSome
            then setPropertyValue { m with TwoSidedOpt = ValueSome false } propertyDescriptor simulant
            else setPropertyValue { m with TwoSidedOpt = ValueNone } propertyDescriptor simulant
        else
            match m.TwoSidedOpt with
            | ValueSome twoSided ->
                let mutable twoSided = twoSided
                ImGui.SameLine ()
                if ImGui.Checkbox ("##matTwoSided", &twoSided) then setPropertyValue { m with TwoSidedOpt = ValueSome twoSided } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        ImGui.SameLine ()
        ImGui.Text "TwoSidedOpt"
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)

    let rec private imGuiEditProperty (getProperty : PropertyDescriptor -> Simulant -> obj) (setProperty : obj -> PropertyDescriptor -> Simulant -> unit) (focusProperty : unit -> unit) (propertyLabelPrefix : string) (propertyDescriptor : PropertyDescriptor) (simulant : Simulant) =
        let ty = propertyDescriptor.PropertyType
        let name = propertyDescriptor.PropertyName
        let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType, toSymbolMemo, ofSymbolMemo)
        let propertyValue = getProperty propertyDescriptor simulant
        let propertyValueStr = converter.ConvertToString propertyValue
        match propertyValue with
        | :? Frustum -> () // TODO: P1: implement FrustumConverter.
        | :? bool as b -> let mutable b' = b in if ImGui.Checkbox (name, &b') then setProperty b' propertyDescriptor simulant
        | :? int8 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (int8 i') propertyDescriptor simulant
        | :? uint8 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (uint8 i') propertyDescriptor simulant
        | :? int16 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (int16 i') propertyDescriptor simulant
        | :? uint16 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (uint16 i') propertyDescriptor simulant
        | :? int32 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (int32 i') propertyDescriptor simulant
        | :? uint32 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (uint32 i') propertyDescriptor simulant
        | :? int64 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (int64 i') propertyDescriptor simulant
        | :? uint64 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (uint64 i') propertyDescriptor simulant
        | :? single as f -> let mutable f' = single f in if ImGui.DragFloat (name, &f', snapDrag) then setProperty (single f') propertyDescriptor simulant
        | :? double as f -> let mutable f' = single f in if ImGui.DragFloat (name, &f', snapDrag) then setProperty (double f') propertyDescriptor simulant
        | :? Vector2 as v -> let mutable v' = v in if ImGui.DragFloat2 (name, &v', snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector3 as v -> let mutable v' = v in if ImGui.DragFloat3 (name, &v', snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector4 as v -> let mutable v' = v in if ImGui.DragFloat4 (name, &v', snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector2i as v -> let mutable v' = v in if ImGui.DragInt2 (name, &v'.X, snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector3i as v -> let mutable v' = v in if ImGui.DragInt3 (name, &v'.X, snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector4i as v -> let mutable v' = v in if ImGui.DragInt4 (name, &v'.X, snapDrag) then setProperty v' propertyDescriptor simulant
        | :? MaterialProperties as mp -> imGuiEditMaterialPropertiesProperty mp propertyDescriptor simulant
        | :? Material as m -> imGuiEditMaterialProperty m propertyDescriptor simulant
        | :? Box2 as b ->
            ImGui.Text name
            let mutable min = v2 b.Min.X b.Min.Y
            let mutable size = v2 b.Size.X b.Size.Y
            ImGui.Indent ()
            let minChanged = ImGui.DragFloat2 (propertyLabelPrefix + "Min via " + name, &min, snapDrag)
            focusProperty ()
            let sizeChanged = ImGui.DragFloat2 (propertyLabelPrefix + "Size via " + name, &size, snapDrag)
            if minChanged || sizeChanged then
                let b' = box2 min size
                setProperty b' propertyDescriptor simulant
            ImGui.Unindent ()
        | :? Box3 as b ->
            ImGui.Text name
            let mutable min = v3 b.Min.X b.Min.Y b.Min.Z
            let mutable size = v3 b.Size.X b.Size.Y b.Size.Z
            ImGui.Indent ()
            let minChanged = ImGui.DragFloat3 (propertyLabelPrefix + "Min via " + name, &min, snapDrag)
            focusProperty ()
            let sizeChanged = ImGui.DragFloat3 (propertyLabelPrefix + "Size via " + name, &size, snapDrag)
            if minChanged || sizeChanged then
                let b' = box3 min size
                setProperty b' propertyDescriptor simulant
            ImGui.Unindent ()
        | :? Box2i as b ->
            ImGui.Text name
            let mutable min = v2i b.Min.X b.Min.Y
            let mutable size = v2i b.Size.X b.Size.Y
            ImGui.Indent ()
            let minChanged = ImGui.DragInt2 (propertyLabelPrefix + "Min via " + name, &min.X, snapDrag)
            focusProperty ()
            let sizeChanged = ImGui.DragInt2 (propertyLabelPrefix + "Size via " + name, &size.X, snapDrag)
            if minChanged || sizeChanged then
                let b' = box2i min size
                setProperty b' propertyDescriptor simulant
            ImGui.Unindent ()
        | :? Quaternion as q ->
            let mutable v = v4 q.X q.Y q.Z q.W
            if ImGui.DragFloat4 (name, &v, snapDrag) then
                let q' = quat v.X v.Y v.Z v.W
                setProperty q' propertyDescriptor simulant
        | :? Color as c ->
            let mutable v = v4 c.R c.G c.B c.A
            if ImGui.ColorEdit4 (name, &v) then
                let c' = color v.X v.Y v.Z v.W
                setPropertyValue c' propertyDescriptor simulant
        | :? RenderStyle as style ->
            let mutable index = match style with Deferred -> 0 | Forward _ -> 1
            let (changed, style) =
                if ImGui.Combo (name, &index, [|nameof Deferred; nameof Forward|], 2)
                then (true, match index with 0 -> Deferred | 1 -> Forward (0.0f, 0.0f) | _ -> failwithumf ())
                else (false, style)
            focusProperty ()
            let (changed, style) =
                match index with
                | 0 -> (changed, style)
                | 1 ->
                    match style with
                    | Deferred -> failwithumf ()
                    | Forward (subsort, sort) ->
                        let mutable (subsort, sort) = (subsort, sort)
                        ImGui.Indent ()
                        let subsortChanged = ImGui.DragFloat ("Subsort via " + name, &subsort, snapDrag)
                        focusProperty ()
                        let sortChanged = ImGui.DragFloat ("Sort via " + name, &sort, snapDrag)
                        ImGui.Unindent ()
                        (changed || subsortChanged || sortChanged, Forward (subsort, sort))
                | _ -> failwithumf ()
            if changed then setProperty style propertyDescriptor simulant
        | :? LightType as light ->
            let mutable index = match light with PointLight -> 0 | DirectionalLight -> 1 | SpotLight _ -> 2
            let (changed, light) =
                if ImGui.Combo (name, &index, [|nameof PointLight; nameof DirectionalLight; nameof SpotLight|], 3)
                then (true, match index with 0 -> PointLight | 1 -> DirectionalLight | 2 -> SpotLight (0.9f, 1.0f) | _ -> failwithumf ())
                else (false, light)
            focusProperty ()
            let (changed, light) =
                match index with
                | 0 -> (changed, light)
                | 1 -> (changed, light)
                | 2 ->
                    match light with
                    | PointLight -> failwithumf ()
                    | DirectionalLight -> failwithumf ()
                    | SpotLight (innerCone, outerCone) ->
                        let mutable (innerCone, outerCone) = (innerCone, outerCone)
                        ImGui.Indent ()
                        let innerConeChanged = ImGui.DragFloat ("InnerCone via " + name, &innerCone, snapDrag)
                        focusProperty ()
                        let outerConeChanged = ImGui.DragFloat ("OuterCone via " + name, &outerCone, snapDrag)
                        ImGui.Unindent ()
                        (changed || innerConeChanged || outerConeChanged, SpotLight (innerCone, outerCone))
                | _ -> failwithumf ()
            if changed then setProperty light propertyDescriptor simulant
        | :? Substance as substance ->
            let mutable scalar = match substance with Mass m -> m | Density d -> d
            let changed = ImGui.DragFloat ("##scalar via " + name, &scalar, snapDrag)
            focusProperty ()
            let mutable index = match substance with Mass _ -> 0 | Density _ -> 1
            ImGui.SameLine ()
            let changed = ImGui.Combo (name, &index, [|nameof Mass; nameof Density|], 2) || changed
            if changed then
                let substance = match index with 0 -> Mass scalar | 1 -> Density scalar | _ -> failwithumf ()
                setProperty substance propertyDescriptor simulant
        | _ ->
            let mutable combo = false
            if FSharpType.IsUnion ty then
                let cases = FSharpType.GetUnionCases ty
                if Array.forall (fun (case : UnionCaseInfo) -> Array.isEmpty (case.GetFields ())) cases then
                    combo <- true
                    let caseNames = Array.map (fun (case : UnionCaseInfo) -> case.Name) cases
                    let (unionCaseInfo, _) = FSharpValue.GetUnionFields (propertyValue, ty)
                    let mutable tag = unionCaseInfo.Tag
                    if ImGui.Combo (name, &tag, caseNames, caseNames.Length) then
                        let value' = FSharpValue.MakeUnion (cases.[tag], [||])
                        setProperty value' propertyDescriptor simulant
            if not combo then
                if ty.IsGenericType &&
                   ty.GetGenericTypeDefinition () = typedefof<_ option> &&
                   ty.GenericTypeArguments.[0] <> typedefof<_ option> &&
                   ty.GenericTypeArguments.[0] <> typeof<MaterialProperties> &&
                   ty.GenericTypeArguments.[0] <> typeof<Material> &&
                   (ty.GenericTypeArguments.[0].IsValueType && ty.GenericTypeArguments.[0].Name <> typedefof<_ AssetTag>.Name ||
                    ty.GenericTypeArguments.[0] = typeof<string> ||
                    ty.GenericTypeArguments.[0] |> FSharpType.isNullTrueValue) then
                    let mutable isSome = ty.GetProperty("IsSome").GetValue(null, [|propertyValue|]) :?> bool
                    if ImGui.Checkbox ((if isSome then "##" else "") + name, &isSome) then
                        if isSome then
                            if ty.GenericTypeArguments.[0].IsValueType then
                                if ty.GenericTypeArguments.[0].Name = typedefof<_ AssetTag>.Name
                                then setProperty (Activator.CreateInstance (ty, [|Activator.CreateInstance (ty.GenericTypeArguments.[0], [|""; ""|])|])) propertyDescriptor simulant
                                else setProperty (Activator.CreateInstance (ty, [|Activator.CreateInstance ty.GenericTypeArguments.[0]|])) propertyDescriptor simulant
                            elif ty.GenericTypeArguments.[0] = typeof<string> then
                                setProperty (Activator.CreateInstance (ty, [|""|])) propertyDescriptor simulant
                            elif FSharpType.isNullTrueValue ty.GenericTypeArguments.[0] then
                                setProperty (Activator.CreateInstance (ty, [|null|])) propertyDescriptor simulant
                            else ()
                        else setProperty None propertyDescriptor simulant
                    focusProperty ()
                    if isSome then
                        ImGui.SameLine ()
                        let getProperty = fun _ simulant -> let opt = getProperty propertyDescriptor simulant in ty.GetProperty("Value").GetValue(opt, [||])
                        let setProperty = fun value _ simulant -> setProperty (Activator.CreateInstance (ty, [|value|])) propertyDescriptor simulant
                        let propertyDescriptor = { propertyDescriptor with PropertyType = ty.GenericTypeArguments.[0] }
                        imGuiEditProperty getProperty setProperty focusProperty (name + ".") propertyDescriptor simulant
                elif ty.IsGenericType &&
                     ty.GetGenericTypeDefinition () = typedefof<_ voption> &&
                     ty.GenericTypeArguments.[0] <> typedefof<_ voption> &&
                     ty.GenericTypeArguments.[0] <> typeof<MaterialProperties> &&
                     ty.GenericTypeArguments.[0] <> typeof<Material> &&
                     (ty.GenericTypeArguments.[0].IsValueType && ty.GenericTypeArguments.[0].Name <> typedefof<_ AssetTag>.Name ||
                      ty.GenericTypeArguments.[0] = typeof<string> ||
                      ty.GenericTypeArguments.[0] |> FSharpType.isNullTrueValue) then
                    let mutable isSome = ty.GetProperty("IsSome").GetValue(null, [|propertyValue|]) :?> bool
                    if ImGui.Checkbox ((if isSome then "##" else "") + name, &isSome) then
                        if isSome then
                            if ty.GenericTypeArguments.[0].IsValueType then
                                setProperty (Activator.CreateInstance (ty, [|Activator.CreateInstance ty.GenericTypeArguments.[0]|])) propertyDescriptor simulant
                            elif ty.GenericTypeArguments.[0] = typeof<string> then
                                setProperty (Activator.CreateInstance (ty, [|""|])) propertyDescriptor simulant
                            elif FSharpType.isNullTrueValue ty.GenericTypeArguments.[0] then
                                setProperty (Activator.CreateInstance (ty, [|null|])) propertyDescriptor simulant
                            else
                                failwithumf ()
                        else setProperty ValueNone propertyDescriptor simulant
                    focusProperty ()
                    if isSome then
                        ImGui.SameLine ()
                        let getProperty = fun _ simulant -> let opt = getProperty propertyDescriptor simulant in ty.GetProperty("Value").GetValue(opt, [||])
                        let setProperty = fun value _ simulant -> setProperty (Activator.CreateInstance (ty, [|value|])) propertyDescriptor simulant
                        let propertyDescriptor = { propertyDescriptor with PropertyType = ty.GenericTypeArguments.[0] }
                        imGuiEditProperty getProperty setProperty focusProperty (name + ".") propertyDescriptor simulant
                elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ AssetTag> then
                    let mutable propertyValueStr = propertyValueStr
                    if ImGui.InputText ("##text" + name, &propertyValueStr, 4096u) then
                        let worldsPast' = worldsPast
                        try let propertyValue = converter.ConvertFromString propertyValueStr
                            setProperty propertyValue propertyDescriptor simulant
                        with _ ->
                            worldsPast <- worldsPast'
                    focusProperty ()
                    if ImGui.BeginDragDropTarget () then
                        if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                            match dragDropPayloadOpt with
                            | Some payload ->
                                let worldsPast' = worldsPast
                                try let propertyValueEscaped = payload
                                    let propertyValueUnescaped = String.unescape propertyValueEscaped
                                    let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                    setProperty propertyValue propertyDescriptor simulant
                                with _ ->
                                    worldsPast <- worldsPast'
                            | None -> ()
                        ImGui.EndDragDropTarget ()
                    ImGui.SameLine ()
                    ImGui.PushID ("##pickAsset" + name)
                    if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                    focusProperty ()
                    ImGui.PopID ()
                    ImGui.SameLine ()
                    ImGui.Text name
                elif name = Constants.Engine.FacetNamesPropertyName && ty = typeof<string Set> then
                    let facetNameEmpty = "(Empty)"
                    let facetNamesValue = scvalue<string Set> propertyValueStr
                    let facetNamesSelectable = world |> World.getFacets |> Map.toKeyArray |> Array.append [|facetNameEmpty|]
                    let mutable facetNamesValue' = Set.empty
                    let mutable changed = false
                    ImGui.Indent ()
                    for i in 0 .. facetNamesValue.Count do
                        let last = i = facetNamesValue.Count
                        let mutable facetName = if not last then Seq.item i facetNamesValue else facetNameEmpty
                        if ImGui.BeginCombo ("##" + name + string i, facetName) then
                            let facetNameSelectablePicked = tryPickName facetNamesSelectable
                            for facetNameSelectable in facetNamesSelectable do
                                if Some facetNameSelectable = facetNameSelectablePicked then ImGui.SetScrollHereY -0.2f
                                if ImGui.Selectable (facetNameSelectable, strEq facetName newEntityDispatcherName) then
                                    facetName <- facetNameSelectable
                                    changed <- true
                            ImGui.EndCombo ()
                        if not last then focusProperty ()
                        if facetName <> facetNameEmpty then facetNamesValue' <- Set.add facetName facetNamesValue'
                    ImGui.Unindent ()
                    if changed then setPropertyValueIgnoreError facetNamesValue' propertyDescriptor simulant
                else
                    let mutable propertyValueStr = propertyValueStr
                    if ImGui.InputText (name, &propertyValueStr, 131072u) && propertyValueStr <> propertyValueStrPrevious then
                        let worldsPast' = worldsPast
                        try let propertyValue = converter.ConvertFromString propertyValueStr
                            setProperty propertyValue propertyDescriptor simulant
                        with _ ->
                            worldsPast <- worldsPast'
                        propertyValueStrPrevious <- propertyValueStr
        focusProperty ()

    let private imGuiEditProperties (simulant : Simulant) =
        let mutable simulant = simulant
        let propertyDescriptors = world |> SimulantPropertyDescriptor.getPropertyDescriptors simulant |> Array.ofList
        let propertyDescriptorses = propertyDescriptors |> Array.groupBy EntityPropertyDescriptor.getCategory |> Map.ofSeq
        for (propertyCategory, propertyDescriptors) in propertyDescriptorses.Pairs do
            if ImGui.CollapsingHeader (propertyCategory, ImGuiTreeNodeFlags.DefaultOpen ||| ImGuiTreeNodeFlags.OpenOnArrow) then
                let propertyDescriptors =
                    propertyDescriptors |>
                    Array.filter (fun pd -> SimulantPropertyDescriptor.getEditable pd simulant) |>
                    Array.sortBy (fun pd ->
                        match pd.PropertyName with
                        | Constants.Engine.NamePropertyName -> "!00" // put Name first
                        | Constants.Engine.ModelPropertyName -> "!01" // put Model second
                        | Constants.Engine.MountOptPropertyName -> "!02" // and so on...
                        | Constants.Engine.OverlayNameOptPropertyName -> "!03"
                        | nameof Entity.Position -> "!04"
                        | nameof Entity.PositionLocal -> "!05"
                        | nameof Entity.Degrees -> "!06"
                        | nameof Entity.DegreesLocal -> "!07"
                        | nameof Entity.Scale -> "!08"
                        | nameof Entity.ScaleLocal -> "!09"
                        | nameof Entity.Size -> "!10"
                        | nameof Entity.Offset -> "!11"
                        | nameof Entity.Overflow -> "!12"
                        | name -> name)
                for propertyDescriptor in propertyDescriptors do
                    if containsProperty propertyDescriptor simulant then
                        if propertyDescriptor.PropertyName = Constants.Engine.NamePropertyName then // NOTE: name edit properties can't be replaced.
                            match simulant with
                            | :? Screen as screen ->
                                // NOTE: can't edit screen names for now.
                                let mutable name = screen.Name
                                ImGui.InputText ("Name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                            | :? Group as group ->
                                let mutable name = group.Name
                                ImGui.InputText ("##name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                if not (group.GetProtected world) then
                                    if ImGui.Button "Rename" then
                                        showRenameGroupDialog <- true
                                else ImGui.Text "Name"
                            | :? Entity as entity ->
                                let mutable name = entity.Name
                                ImGui.InputText ("##name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                if not (entity.GetProtected world) then
                                    if ImGui.Button "Rename" then
                                        showRenameEntityDialog <- true
                                else ImGui.Text "Name"
                            | _ -> ()
                            if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- None
                        elif propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                            let mutable clickToEditModel = "*click to edit*"
                            ImGui.InputText ("Model", &clickToEditModel, 256u) |> ignore<bool>
                            if ImGui.IsItemFocused () then
                                focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                                propertyEditorFocusRequested <- true
                        else
                            let focusProperty = fun () -> if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                            let mutable replaced = false
                            let replaceProperty =
                                ReplaceProperty
                                    { Snapshot = fun world -> snapshot (); world
                                      FocusProperty = fun world -> focusProperty (); world
                                      IndicateReplaced = fun world -> replaced <- true; world
                                      PropertyDescriptor = propertyDescriptor }
                            world <- World.edit replaceProperty simulant world
                            if not replaced then
                                imGuiEditProperty getPropertyValue setPropertyValue focusProperty "" propertyDescriptor simulant
        let appendProperties =
            { Snapshot = fun world -> snapshot (); world
              UnfocusProperty = fun world -> focusedPropertyDescriptorOpt <- None; world }
        world <- World.edit (AppendProperties appendProperties) simulant world

    let private imGuiProcess wtemp =

        // store old world
        let worldOld = wtemp

        // transfer to world mutation mode
        world <- worldOld

        // detect if eyes were changed somewhere other than in the editor (such as in gameplay code)
        if  World.getEye2dCenter world <> desiredEye2dCenter ||
            World.getEye3dCenter world <> desiredEye3dCenter ||
            World.getEye3dRotation world <> desiredEye3dRotation then
            eyeChangedElsewhere <- true

        // enable global docking
        ImGui.DockSpaceOverViewport (ImGui.GetMainViewport (), ImGuiDockNodeFlags.PassthruCentralNode) |> ignore<uint>

        // attempt to proceed with normal operation
        match recoverableExceptionOpt with
        | None ->

            // use a generalized exception process
            try

                // track state for hot key input
                let mutable entityHierarchyFocused = false

                // viewport interaction
                let io = ImGui.GetIO ()
                ImGui.SetNextWindowPos v2Zero
                ImGui.SetNextWindowSize io.DisplaySize
                if ImGui.IsKeyReleased ImGuiKey.Escape && not (modal ()) then ImGui.SetNextWindowFocus ()
                if ImGui.Begin ("Viewport", ImGuiWindowFlags.NoBackground ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoInputs ||| ImGuiWindowFlags.NoNav) then

                    // user-defined viewport manipulation
                    let viewport = Constants.Render.Viewport
                    let projectionMatrix = viewport.Projection3d Constants.Render.NearPlaneDistanceInterior Constants.Render.FarPlaneDistanceOmnipresent
                    let projection = projectionMatrix.ToArray ()
                    match selectedEntityOpt with
                    | Some entity when entity.Exists world && entity.GetIs3d world ->
                        let viewMatrix =
                            viewport.View3d (entity.GetAbsolute world, World.getEye3dCenter world, World.getEye3dRotation world)
                        let operation =
                            OverlayViewport
                                { Snapshot = fun world -> snapshot (); world
                                  ViewportView = viewMatrix
                                  ViewportProjection = projectionMatrix
                                  ViewportBounds = box2 v2Zero io.DisplaySize }
                        world <- World.editEntity operation entity world
                    | Some _ | None -> ()

                    // light probe bounds manipulation
                    match selectedEntityOpt with
                    | Some entity when entity.Exists world && entity.Has<LightProbe3dFacet> world ->
                        let mutable lightProbeBounds = entity.GetProbeBounds world
                        let manipulationResult =
                            ImGuizmo.ManipulateBox3
                                (World.getEye3dCenter world,
                                 World.getEye3dRotation world,
                                 World.getEye3dFrustumView world,
                                 entity.GetAbsolute world,
                                 (if not snaps2dSelected && ImGui.IsCtrlUp () then Triple.fst snaps3d else 0.0f),
                                 &lightProbeBounds)
                        match manipulationResult with
                        | ImGuiEditActive started ->
                            if started then snapshot ()
                            world <- entity.SetProbeBounds lightProbeBounds world
                        | ImGuiEditInactive -> ()
                    | Some _ | None -> ()

                    // guizmo manipulation
                    ImGuizmo.SetOrthographic false
                    ImGuizmo.SetRect (0.0f, 0.0f, io.DisplaySize.X, io.DisplaySize.Y)
                    ImGuizmo.SetDrawlist (ImGui.GetBackgroundDrawList ())
                    match selectedEntityOpt with
                    | Some entity when entity.Exists world && entity.GetIs3d world && not io.WantCaptureMouseLocal ->
                        let viewMatrix = viewport.View3d (entity.GetAbsolute world, World.getEye3dCenter world, World.getEye3dRotation world)
                        let view = viewMatrix.ToArray ()
                        let affineMatrix = entity.GetAffineMatrix world
                        let affine = affineMatrix.ToArray ()
                        let (p, r, s) =
                            if not snaps2dSelected && ImGui.IsCtrlUp ()
                            then snaps3d
                            else (0.0f, 0.0f, 0.0f)
                        let mutable copying = false
                        if not manipulationActive then
                            if ImGui.IsShiftDown () then manipulationOperation <- OPERATION.SCALE
                            elif ImGui.IsAltDown () then manipulationOperation <- OPERATION.ROTATE
                            elif ImGui.IsKeyDown ImGuiKey.X then manipulationOperation <- OPERATION.ROTATE_X
                            elif ImGui.IsKeyDown ImGuiKey.Y then manipulationOperation <- OPERATION.ROTATE_Y
                            elif ImGui.IsKeyDown ImGuiKey.Z then manipulationOperation <- OPERATION.ROTATE_Z
                            elif ImGui.IsCtrlDown () then  manipulationOperation <- OPERATION.TRANSLATE; copying <- true
                            else manipulationOperation <- OPERATION.TRANSLATE
                        let mutable snap =
                            match manipulationOperation with
                            | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> r
                            | _ -> 0.0f // NOTE: doing other snapping ourselves since I don't like guizmo's implementation.
                        let deltaMatrix = m4Identity.ToArray ()
                        let manipulationResult =
                            if snap = 0.0f
                            then ImGuizmo.Manipulate (&view.[0], &projection.[0], manipulationOperation, MODE.WORLD, &affine.[0], &deltaMatrix.[0])
                            else ImGuizmo.Manipulate (&view.[0], &projection.[0], manipulationOperation, MODE.WORLD, &affine.[0], &deltaMatrix.[0], &snap)
                        if manipulationResult then
                            if not manipulationActive && ImGui.IsMouseDown ImGuiMouseButton.Left then
                                snapshot ()
                                manipulationActive <- true
                            let affine' = Matrix4x4.CreateFromArray affine
                            let mutable (position, rotation, degrees, scale) = (v3Zero, quatIdentity, v3Zero, v3One)
                            if Matrix4x4.Decompose (affine', &scale, &rotation, &position) then
                                position <- Math.SnapF3d p position
                                rotation <- rotation.Normalized // try to avoid weird angle combinations
                                let rollPitchYaw = rotation.RollPitchYaw
                                degrees.X <- Math.RadiansToDegrees rollPitchYaw.X
                                degrees.Y <- Math.RadiansToDegrees rollPitchYaw.Y
                                degrees.Z <- Math.RadiansToDegrees rollPitchYaw.Z
                                degrees <- if degrees.X = 180.0f && degrees.Z = 180.0f then v3 0.0f (180.0f - degrees.Y) 0.0f else degrees
                                degrees <- v3 degrees.X (if degrees.Y > 180.0f then degrees.Y - 360.0f else degrees.Y) degrees.Z
                                degrees <- v3 degrees.X (if degrees.Y < -180.0f then degrees.Y + 360.0f else degrees.Y) degrees.Z
                                scale <- Math.SnapF3d s scale
                                if scale.X < 0.01f then scale.X <- 0.01f
                                if scale.Y < 0.01f then scale.Y <- 0.01f
                                if scale.Z < 0.01f then scale.Z <- 0.01f
                            let entity =
                                if  copying &&
                                    tryCopySelectedEntity () &&
                                    tryPaste (PasteAt position) (Option.map cast newEntityParentOpt) then
                                    Option.defaultValue entity selectedEntityOpt
                                else entity
                            match Option.bind (tryResolve entity) (entity.GetMountOpt world) with
                            | Some mount ->
                                let mountAffineMatrixInverse = (mount.GetAffineMatrix world).Inverted
                                let positionLocal = Vector3.Transform (position, mountAffineMatrixInverse)
                                let mountRotationInverse = (mount.GetRotation world).Inverted
                                let rotationLocal = mountRotationInverse * rotation
                                let rollPitchYawLocal = rotationLocal.RollPitchYaw
                                let mutable degreesLocal = v3Zero
                                degreesLocal.X <- Math.RadiansToDegrees rollPitchYawLocal.X
                                degreesLocal.Y <- Math.RadiansToDegrees rollPitchYawLocal.Y
                                degreesLocal.Z <- Math.RadiansToDegrees rollPitchYawLocal.Z
                                degreesLocal <- if degreesLocal.X = 180.0f && degreesLocal.Z = 180.0f then v3 0.0f (180.0f - degreesLocal.Y) 0.0f else degreesLocal
                                degreesLocal <- v3 degreesLocal.X (if degreesLocal.Y > 180.0f then degreesLocal.Y - 360.0f else degreesLocal.Y) degreesLocal.Z
                                degreesLocal <- v3 degreesLocal.X (if degreesLocal.Y < -180.0f then degreesLocal.Y + 360.0f else degreesLocal.Y) degreesLocal.Z
                                let mountScaleInverse = v3One / mount.GetScale world
                                let scaleLocal = mountScaleInverse * scale
                                match manipulationOperation with
                                | OPERATION.TRANSLATE -> world <- entity.SetPositionLocal positionLocal world
                                | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> world <- entity.SetDegreesLocal degreesLocal world
                                | OPERATION.SCALE -> world <- entity.SetScaleLocal scaleLocal world
                                | _ -> () // nothing to do
                            | None ->
                                match manipulationOperation with
                                | OPERATION.TRANSLATE -> world <- entity.SetPosition position world
                                | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> world <- entity.SetDegrees degrees world
                                | OPERATION.SCALE -> world <- entity.SetScale scale world
                                | _ -> () // nothing to do
                            if world.Advancing then
                                match entity.TryGetProperty (nameof entity.LinearVelocity) world with
                                | Some property when property.PropertyType = typeof<Vector3> -> world <- entity.SetLinearVelocity v3Zero world
                                | Some _ | None -> ()
                                match entity.TryGetProperty (nameof entity.AngularVelocity) world with
                                | Some property when property.PropertyType = typeof<Vector3> -> world <- entity.SetAngularVelocity v3Zero world
                                | Some _ | None -> ()
                        if ImGui.IsMouseReleased ImGuiMouseButton.Left then
                            if manipulationActive then
                                do (ImGuizmo.Enable false; ImGuizmo.Enable true) // HACK: forces imguizmo to end manipulation when mouse is release over an imgui window.
                                match Option.bind (tryResolve entity) (entity.GetMountOpt world) with
                                | Some _ ->
                                    match manipulationOperation with
                                    | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z when r <> 0.0f ->
                                        let degrees = Math.SnapDegree3d r (entity.GetDegreesLocal world)
                                        world <- entity.SetDegreesLocal degrees world
                                    | _ -> ()
                                | None ->
                                    match manipulationOperation with
                                    | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z when r <> 0.0f ->
                                        let degrees = Math.SnapDegree3d r (entity.GetDegrees world)
                                        world <- entity.SetDegrees degrees world
                                    | _ -> ()
                                manipulationOperation <- OPERATION.TRANSLATE
                                manipulationActive <- false
                    | Some _ | None -> ()

                    // view manipulation
                    // NOTE: this code is the current failed attempt to integrate ImGuizmo view manipulation as reported here - https://github.com/CedricGuillemet/ImGuizmo/issues/304
                    //if not io.WantCaptureMouseMinus then
                    //    let eyeCenter = (World.getEye3dCenter world |> Matrix4x4.CreateTranslation).ToArray ()
                    //    let eyeRotation = (World.getEye3dRotation world |> Matrix4x4.CreateFromQuaternion).ToArray ()
                    //    let eyeScale = m4Identity.ToArray ()
                    //    let view = m4Identity.ToArray ()
                    //    ImGuizmo.RecomposeMatrixFromComponents (&eyeCenter.[0], &eyeRotation.[0], &eyeScale.[0], &view.[0])
                    //    ImGuizmo.ViewManipulate (&view.[0], 1.0f, v2 1400.0f 100.0f, v2 150.0f 150.0f, uint 0x10101010)
                    //    ImGuizmo.DecomposeMatrixToComponents (&view.[0], &eyeCenter.[0], &eyeRotation.[0], &eyeScale.[0])
                    //    desiredEye3dCenter <- (eyeCenter |> Matrix4x4.CreateFromArray).Translation
                    //    desiredEye3dRotation <- (eyeRotation |> Matrix4x4.CreateFromArray |> Quaternion.CreateFromRotationMatrix)

                    // fin
                    ImGui.End ()

                // show all windows when out in full-screen mode
                if not fullScreen then

                    // main menu window
                    if ImGui.Begin ("Gaia", ImGuiWindowFlags.MenuBar ||| ImGuiWindowFlags.NoNav) then
                        if ImGui.BeginMenuBar () then
                            if ImGui.BeginMenu "Project" then
                                if ImGui.MenuItem "New Project" then showNewProjectDialog <- true
                                if ImGui.MenuItem "Open Project" then showOpenProjectDialog <- true
                                if ImGui.MenuItem "Close Project" then showCloseProjectDialog <- true
                                ImGui.Separator ()
                                if ImGui.MenuItem "Exit" then showConfirmExitDialog <- true
                                ImGui.EndMenu ()
                            if ImGui.BeginMenu "Group" then
                                if ImGui.MenuItem ("New Group", "Ctrl+N") then showNewGroupDialog <- true
                                if ImGui.MenuItem ("Open Group", "Ctrl+O") then showOpenGroupDialog <- true
                                if ImGui.MenuItem ("Save Group", "Ctrl+S") then
                                    match Map.tryFind selectedGroup.GroupAddress groupFilePaths with
                                    | Some filePath -> groupFileDialogState.FilePath <- filePath
                                    | None -> groupFileDialogState.FileName <- ""
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
                                    else messageBoxOpt <- Some "Cannot close protected or only group."
                                ImGui.EndMenu ()
                            if ImGui.BeginMenu "Entity" then
                                if ImGui.MenuItem ("Cut Entity", "Ctrl+X") then tryCutSelectedEntity () |> ignore<bool>
                                if ImGui.MenuItem ("Copy Entity", "Ctrl+C") then tryCopySelectedEntity () |> ignore<bool>
                                if ImGui.MenuItem ("Paste Entity", "Ctrl+V") then tryPaste PasteAtLook (Option.map cast newEntityParentOpt) |> ignore<bool>
                                ImGui.Separator ()
                                if ImGui.MenuItem ("Create Entity", "Ctrl+Enter") then createEntity false false
                                if ImGui.MenuItem ("Delete Entity", "Delete") then tryDeleteSelectedEntity () |> ignore<bool>
                                if ImGui.MenuItem ("Auto Bounds", "Ctrl+B") then tryAutoBoundsSelectedEntity () |> ignore<bool>
                                ImGui.EndMenu ()
                            if ImGui.BeginMenu "Edit" then
                                if ImGui.MenuItem ("Undo", "Ctrl+Z") then tryUndo () |> ignore<bool>
                                if ImGui.MenuItem ("Redo", "Ctrl+Y") then tryRedo () |> ignore<bool>
                                ImGui.Separator ()
                                if ImGui.MenuItem ("Freeze Entities", "Ctrl+Shift+F") then freezeEntities ()
                                if ImGui.MenuItem ("Thaw Entities", "Ctrl+Shift+T") then freezeEntities ()
                                if ImGui.MenuItem ("Re-render Light Maps", "Ctrl+Shift+R") then rerenderLightMaps ()
                                ImGui.Separator ()
                                if not world.Advancing
                                then if ImGui.MenuItem ("Advance", "F5") then toggleAdvancing ()
                                else if ImGui.MenuItem ("Halt", "F5") then toggleAdvancing ()
                                if editWhileAdvancing
                                then if ImGui.MenuItem ("Disable Edit while Advancing", "F6") then editWhileAdvancing <- false
                                else if ImGui.MenuItem ("Enable Edit while Advancing", "F6") then editWhileAdvancing <- true
                                ImGui.Separator ()
                                if ImGui.MenuItem ("Reload Assets", "F8") then reloadAssetsRequested <- 1
                                if ImGui.MenuItem ("Reload Code", "F9") then reloadCodeRequested <- 1
                                if ImGui.MenuItem ("Reload All", "Ctrl+R") then reloadAllRequested <- 1
                                ImGui.EndMenu ()
                            ImGui.EndMenuBar ()
                        if ImGui.Button "Create" then createEntity false false
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 200.0f
                        if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                            let dispatcherNames = (World.getEntityDispatchers world).Keys
                            let dispatcherNamePicked = tryPickName dispatcherNames
                            for dispatcherName in dispatcherNames do
                                if Some dispatcherName = dispatcherNamePicked then ImGui.SetScrollHereY -0.2f
                                if ImGui.Selectable (dispatcherName, strEq dispatcherName newEntityDispatcherName) then
                                    newEntityDispatcherName <- dispatcherName
                            ImGui.EndCombo ()
                        ImGui.SameLine ()
                        ImGui.Text "w/ Overlay"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 150.0f
                        let overlayNames = Array.append [|"(Default Overlay)"; "(Routed Overlay)"; "(No Overlay)"|] (World.getOverlays world |> Map.toKeyArray)
                        if ImGui.BeginCombo ("##newEntityOverlayName", newEntityOverlayName) then
                            let overlayNamePicked = tryPickName overlayNames
                            for overlayName in overlayNames do
                                if Some overlayName = overlayNamePicked then ImGui.SetScrollHereY -0.2f
                                if ImGui.Selectable (overlayName, strEq overlayName newEntityOverlayName) then
                                    newEntityOverlayName <- overlayName
                            ImGui.EndCombo ()
                        ImGui.SameLine ()
                        if ImGui.Button "Auto Bounds" then tryAutoBoundsSelectedEntity () |> ignore<bool>
                        ImGui.SameLine ()
                        if ImGui.Button "Delete" then tryDeleteSelectedEntity () |> ignore<bool>
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        if world.Halted then
                            if ImGui.Button "Advance (F5)" then toggleAdvancing ()
                        else
                            if ImGui.Button "Halt (F5)" then toggleAdvancing ()
                            ImGui.SameLine ()
                            ImGui.Checkbox ("Edit", &editWhileAdvancing) |> ignore<bool>
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        ImGui.Text "Eye:"
                        ImGui.SameLine ()
                        if ImGui.Button "Reset" then resetEye ()
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            let mutable eye2dCenter = World.getEye2dCenter world
                            let mutable eye3dCenter = World.getEye3dCenter world
                            let mutable eye3dDegrees = Math.RadiansToDegrees3d (World.getEye3dRotation world).RollPitchYaw
                            ImGui.InputFloat2 ("Eye 2d Center", &eye2dCenter, "%3.3f", ImGuiInputTextFlags.ReadOnly) |> ignore
                            ImGui.InputFloat3 ("Eye 3d Center", &eye3dCenter, "%3.3f", ImGuiInputTextFlags.ReadOnly) |> ignore
                            ImGui.InputFloat3 ("Eye 3d Degrees", &eye3dDegrees, "%3.3f", ImGuiInputTextFlags.ReadOnly) |> ignore
                            ImGui.EndTooltip ()
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        ImGui.Text "Reload:"
                        ImGui.SameLine ()
                        if ImGui.Button "Assets" then reloadAssetsRequested <- 1
                        ImGui.SameLine ()
                        if ImGui.Button "Code" then reloadCodeRequested <- 1
                        ImGui.SameLine ()
                        if ImGui.Button "All" then reloadAllRequested <- 1
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        ImGui.Text "Mode:"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 130.0f
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
                        if ImGui.Button "Thaw" then thawEntities ()
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            ImGui.Text "Thaw all frozen entities. (Ctrl+Shift+T)"
                            ImGui.EndTooltip ()
                        ImGui.SameLine ()
                        if ImGui.Button "Freeze" then freezeEntities ()
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            ImGui.Text "Freeze all thawed entities. (Ctrl+Shift+F)"
                            ImGui.EndTooltip ()
                        ImGui.SameLine ()
                        if ImGui.Button "Relight" then rerenderLightMaps ()
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            ImGui.Text "Re-render all light maps. (Ctrl+Shift+R)"
                            ImGui.EndTooltip ()
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        ImGui.Text "Full Screen"
                        ImGui.SameLine ()
                        ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            ImGui.Text "Toggle full screen view (F11 to toggle)."
                            ImGui.EndTooltip ()
                        ImGui.End ()

                    // entity hierarchy window
                    if ImGui.Begin "Entity Hierarchy" then
                        
                        // allow defocus of entity hierarchy?
                        entityHierarchyFocused <- ImGui.IsWindowFocused ()

                        // hierarchy operations
                        if ImGui.Button "Collapse All" then
                            collapseEntityHierarchy <- true
                            ImGui.SetWindowFocus "Viewport"
                        ImGui.SameLine ()
                        if ImGui.Button "Expand All" then
                            expandEntityHierarchy <- true
                            ImGui.SetWindowFocus "Viewport"
                        ImGui.SameLine ()
                        if ImGui.Button "Show Entity" then
                            showSelectedEntity <- true
                            ImGui.SetWindowFocus "Viewport"

                        // entity search
                        if entityHierarchySearchRequested then
                            ImGui.SetKeyboardFocusHere ()
                            entityHierarchySearchStr <- ""
                            entityHierarchySearchRequested <- false
                        ImGui.SetNextItemWidth -1.0f
                        ImGui.InputTextWithHint ("##entityHierarchySearchStr", "[enter search text]", &entityHierarchySearchStr, 4096u) |> ignore<bool>

                        // creation parent display
                        match newEntityParentOpt with
                        | Some newEntityParent when newEntityParent.Exists world ->
                            let creationParentStr = scstring (Address.skip 2 newEntityParent.EntityAddress)
                            if ImGui.Button creationParentStr then newEntityParentOpt <- None
                            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                                ImGui.Text (creationParentStr + " (click to reset)")
                                ImGui.EndTooltip ()
                        | Some _ | None ->
                            ImGui.Button (scstring (Address.skip 2 selectedGroup.GroupAddress)) |> ignore<bool>
                            newEntityParentOpt <- None
                        ImGui.SameLine ()
                        ImGui.Text "(creation parent)"

                        // group selection
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
                                    let sourceEntity = Nu.Entity sourceEntityAddressStr
                                    if not (sourceEntity.GetProtected world) then
                                        let sourceEntity' = Nu.Entity (selectedGroup.GroupAddress <-- Address.makeFromName sourceEntity.Name)
                                        if not (sourceEntity'.Exists world) then
                                            world <- sourceEntity.SetMountOptWithAdjustment None world
                                            world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                                            if newEntityParentOpt = Some sourceEntity then newEntityParentOpt <- Some sourceEntity'
                                            selectEntityOpt (Some sourceEntity')
                                            showSelectedEntity <- true
                                        else messageBoxOpt <- Some "Cannot unparent an entity when there exists another unparented entity with the same name."
                                | None -> ()
                        
                        // entity editing
                        let entities =
                            World.getEntitiesSovereign selectedGroup world |>
                            Array.ofSeq |>
                            Array.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity)) |>
                            Array.sortBy fst |>
                            Array.map snd
                        for entity in entities do
                            imGuiEntityHierarchy entity
                        ImGui.End ()

                    // allow defocus of entity hierarchy?
                    else entityHierarchyFocused <- false
                    expandEntityHierarchy <- false
                    collapseEntityHierarchy <- false

                    // game properties window
                    if ImGui.Begin ("Game Properties", ImGuiWindowFlags.NoNav) then
                        imGuiEditProperties Game
                        ImGui.End ()

                    // screen properties window
                    if ImGui.Begin ("Screen Properties", ImGuiWindowFlags.NoNav) then
                        imGuiEditProperties selectedScreen
                        ImGui.End ()

                    // group properties window
                    if ImGui.Begin ("Group Properties", ImGuiWindowFlags.NoNav) then
                        imGuiEditProperties selectedGroup
                        ImGui.End ()

                    // entity properties window
                    if ImGui.Begin ("Entity Properties", ImGuiWindowFlags.NoNav) then
                        match selectedEntityOpt with
                        | Some entity when entity.Exists world -> imGuiEditProperties entity
                        | Some _ | None -> ()
                        ImGui.End ()

                    // edit overlayer window
                    if ImGui.Begin ("Edit Overlayer", ImGuiWindowFlags.NoNav) then
                        if ImGui.Button "Save" then
                            let overlayerSourceDir = targetDir + "/../../.."
                            let overlayerFilePath = overlayerSourceDir + "/" + Assets.Global.AssetGraphFilePath
                            try let overlays = scvalue<Overlay list> overlayerStr
                                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                                File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
                            with exn -> messageBoxOpt <- Some ("Could not save asset graph due to: " + scstring exn)
                        ImGui.SameLine ()
                        if ImGui.Button "Load" then
                            let overlayerFilePath = targetDir + "/" + Assets.Global.OverlayerFilePath
                            match Overlayer.tryMakeFromFile [] overlayerFilePath with
                            | Right overlayer ->
                                let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
                                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                                overlayerStr <- PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
                            | Left error -> messageBoxOpt <- Some ("Could not read overlayer due to: " + error + "'.")
                        ImGui.InputTextMultiline ("##overlayerStr", &overlayerStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
                        ImGui.End ()

                    // edit asset graph window
                    if ImGui.Begin ("Edit Asset Graph", ImGuiWindowFlags.NoNav) then
                        if ImGui.Button "Save" then
                            let assetSourceDir = targetDir + "/../../.."
                            let assetGraphFilePath = assetSourceDir + "/" + Assets.Global.AssetGraphFilePath
                            try let packageDescriptorsStr = assetGraphStr |> scvalue<Map<string, PackageDescriptor>> |> scstring
                                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                                File.WriteAllText (assetGraphFilePath, PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter)
                            with exn -> messageBoxOpt <- Some ("Could not save asset graph due to: " + scstring exn)
                        ImGui.SameLine ()
                        if ImGui.Button "Load" then
                            match AssetGraph.tryMakeFromFile (targetDir + "/" + Assets.Global.AssetGraphFilePath) with
                            | Right assetGraph ->
                                let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
                                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                                assetGraphStr <- PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
                            | Left error -> messageBoxOpt <- Some ("Could not read asset graph due to: " + error + "'.")
                        ImGui.InputTextMultiline ("##assetGraphStr", &assetGraphStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
                        ImGui.End ()

                    // edit property window
                    if propertyEditorFocusRequested then ImGui.SetNextWindowFocus ()
                    if ImGui.Begin ("Edit Property", ImGuiWindowFlags.NoNav) then
                        match focusedPropertyDescriptorOpt with
                        | Some (propertyDescriptor, simulant) when
                            World.getExists simulant world &&
                            propertyDescriptor.PropertyType <> typeof<ComputedProperty> ->
                            toSymbolMemo.Evict Constants.Gaia.PropertyValueStrMemoEvictionAge
                            ofSymbolMemo.Evict Constants.Gaia.PropertyValueStrMemoEvictionAge
                            let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType, toSymbolMemo, ofSymbolMemo)
                            let propertyValueUntruncated = getPropertyValue propertyDescriptor simulant
                            let propertyValue =
                                if propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                                    match World.tryTruncateModel propertyValueUntruncated simulant world with
                                    | Some truncatedValue -> truncatedValue
                                    | None -> propertyValueUntruncated
                                else propertyValueUntruncated
                            ImGui.Text propertyDescriptor.PropertyName
                            ImGui.SameLine ()
                            ImGui.Text ":"
                            ImGui.SameLine ()
                            ImGui.Text (Reflection.getSimplifiedTypeNameHack propertyDescriptor.PropertyType)
                            let propertyValueSymbol = converter.ConvertTo (propertyValue, typeof<Symbol>) :?> Symbol
                            let mutable propertyValueStr = PrettyPrinter.prettyPrintSymbol propertyValueSymbol PrettyPrinter.defaultPrinter
                            let isPropertyAssetTag = propertyDescriptor.PropertyType.IsGenericType && propertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>
                            if  isPropertyAssetTag then
                                ImGui.SameLine ()
                                if ImGui.Button "Pick" then searchAssetViewer ()
                            if  propertyEditorFocusRequested then
                                ImGui.SetKeyboardFocusHere ()
                                propertyEditorFocusRequested <- false
                            if  propertyDescriptor.PropertyName = Constants.Engine.FacetNamesPropertyName &&
                                propertyDescriptor.PropertyType = typeof<string Set> then
                                ImGui.InputTextMultiline ("##propertyValuePretty", &propertyValueStr, 4096u, v2 -1.0f -1.0f, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                            elif ImGui.InputTextMultiline ("##propertyValuePretty", &propertyValueStr, 131072u, v2 -1.0f -1.0f) && propertyValueStr <> propertyValueStrPrevious then
                                let worldsPast' = worldsPast
                                try let propertyValueEscaped = propertyValueStr
                                    let propertyValueUnescaped = String.unescape propertyValueEscaped
                                    let propertyValueTruncated = converter.ConvertFromString propertyValueUnescaped
                                    let propertyValue =
                                        if propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                                            match World.tryUntruncateModel propertyValueTruncated simulant world with
                                            | Some truncatedValue -> truncatedValue
                                            | None -> propertyValueTruncated
                                        else propertyValueTruncated
                                    setPropertyValue propertyValue propertyDescriptor simulant
                                with _ ->
                                    worldsPast <- worldsPast'
                                propertyValueStrPrevious <- propertyValueStr
                            if isPropertyAssetTag then
                                if ImGui.BeginDragDropTarget () then
                                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                        match dragDropPayloadOpt with
                                        | Some payload ->
                                            let worldsPast' = worldsPast
                                            try let propertyValueEscaped = payload
                                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                                let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                                setPropertyValue propertyValue propertyDescriptor simulant
                                            with _ ->
                                                worldsPast <- worldsPast'
                                        | None -> ()
                                    ImGui.EndDragDropTarget ()
                        | Some _ | None -> ()
                        ImGui.End ()

                    // matrics window
                    if ImGui.Begin ("Metrics", ImGuiWindowFlags.NoNav) then
                        ImGui.Text "Fps:"
                        ImGui.SameLine ()
                        let currentDateTime = DateTimeOffset.Now
                        let elapsedDateTime = currentDateTime - fpsStartDateTime
                        if elapsedDateTime.TotalSeconds >= 5.0 then
                            fpsStartUpdateTime <- world.UpdateTime
                            fpsStartDateTime <- currentDateTime
                        let elapsedDateTime = currentDateTime - fpsStartDateTime
                        let time = double (world.UpdateTime - fpsStartUpdateTime)
                        let frames = time / elapsedDateTime.TotalSeconds
                        ImGui.Text (if not (Double.IsNaN frames) then String.Format ("{0:f2}", frames) else "0.00")
                        ImGui.Text "Draw Call Count:"
                        ImGui.SameLine ()
                        ImGui.Text (string (OpenGL.Hl.GetDrawCallCount ()))
                        ImGui.Text "Draw Instance Count:"
                        ImGui.SameLine ()
                        ImGui.Text (string (OpenGL.Hl.GetDrawInstanceCount ()))
                        ImGui.End ()

                    // event tracing window
                    if ImGui.Begin ("Event Tracing", ImGuiWindowFlags.NoNav) then
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

                    // renderer window
                    if ImGui.Begin ("Renderer", ImGuiWindowFlags.NoNav) then

                        // configure lighting
                        ImGui.Text "Lighting"
                        let mutable lightingChanged = false
                        let mutable lightCutoffMargin = lightingConfig.LightCutoffMargin
                        let mutable lightShadowBiasAcneStr = lightingConfig.LightShadowBiasAcne.ToString "0.00000000"
                        let mutable lightShadowBiasBleed = lightingConfig.LightShadowBiasBleed
                        let mutable lightMappingEnabled = lightingConfig.LightMappingEnabled
                        lightingChanged <- ImGui.SliderFloat ("Light Cutoff Margin", &lightCutoffMargin, 0.0f, 1.0f) || lightingChanged
                        lightingChanged <- ImGui.InputText ("Light Shadow Bias Acne", &lightShadowBiasAcneStr, 4096u) || lightingChanged
                        lightingChanged <- ImGui.SliderFloat ("Light Shadow Bias Bleed", &lightShadowBiasBleed, 0.0f, 1.0f) || lightingChanged
                        lightingChanged <- ImGui.Checkbox ("Light Mapping Enabled", &lightMappingEnabled) || lightingChanged
                        if lightingChanged then
                            lightingConfig <-
                                { LightCutoffMargin = lightCutoffMargin
                                  LightShadowBiasAcne = match Single.TryParse lightShadowBiasAcneStr with (true, s) -> s | (false, _) -> lightingConfig.LightShadowBiasAcne
                                  LightShadowBiasBleed = lightShadowBiasBleed
                                  LightMappingEnabled = lightMappingEnabled }
                            World.enqueueRenderMessage3d (ConfigureLighting lightingConfig) world

                        // configure ssao
                        ImGui.Text "Ssao (screen-space ambient occlusion)"
                        let mutable ssaoChanged = false
                        let mutable ssaoEnabled = ssaoConfig.SsaoEnabled
                        let mutable ssaoIntensity = ssaoConfig.SsaoIntensity
                        let mutable ssaoBias = ssaoConfig.SsaoBias
                        let mutable ssaoRadius = ssaoConfig.SsaoRadius
                        let mutable ssaoDistanceMax = ssaoConfig.SsaoDistanceMax
                        let mutable ssaoSampleCount = ssaoConfig.SsaoSampleCount
                        ssaoChanged <- ImGui.Checkbox ("Ssao Enabled", &ssaoEnabled) || ssaoChanged
                        if ssaoEnabled then
                            ssaoChanged <- ImGui.SliderFloat ("Ssao Intensity", &ssaoIntensity, 0.0f, 10.0f) || ssaoChanged
                            ssaoChanged <- ImGui.SliderFloat ("Ssao Bias", &ssaoBias, 0.0f, 0.1f) || ssaoChanged
                            ssaoChanged <- ImGui.SliderFloat ("Ssao Radius", &ssaoRadius, 0.0f, 1.0f) || ssaoChanged
                            ssaoChanged <- ImGui.SliderFloat ("Ssao Distance Max", &ssaoDistanceMax, 0.0f, 1.0f) || ssaoChanged
                            ssaoChanged <- ImGui.SliderInt ("Ssao Sample Count", &ssaoSampleCount, 0, Constants.Render.SsaoSampleCountMax) || ssaoChanged
                        if ssaoChanged then
                            ssaoConfig <-
                                { SsaoEnabled = ssaoEnabled
                                  SsaoIntensity = ssaoIntensity
                                  SsaoBias = ssaoBias
                                  SsaoRadius = ssaoRadius
                                  SsaoDistanceMax = ssaoDistanceMax
                                  SsaoSampleCount = ssaoSampleCount }
                            World.enqueueRenderMessage3d (ConfigureSsao ssaoConfig) world

                        // fin
                        ImGui.End ()

                    // audio player window
                    if ImGui.Begin ("Audio Player", ImGuiWindowFlags.NoNav) then
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

                    // editor window
                    if ImGui.Begin ("Editor", ImGuiWindowFlags.NoNav) then
                        ImGui.Text "Transform Snapping"
                        ImGui.SetNextItemWidth 50.0f
                        let mutable index = if snaps2dSelected then 0 else 1
                        if ImGui.Combo ("##snapsSelection", &index, [|"2d"; "3d"|], 2) then
                            match index with
                            | 0 -> snaps2dSelected <- true
                            | _ -> snaps2dSelected <- false
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            ImGui.Text "Use 2d or 3d snapping (F3 to swap mode)."
                            ImGui.EndTooltip ()
                        ImGui.SameLine ()
                        let mutable (p, d, s) = if snaps2dSelected then snaps2d else snaps3d
                        ImGui.Text "Pos"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 50.0f
                        ImGui.DragFloat ("##p", &p, (if snaps2dSelected then 0.1f else 0.01f), 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
                        ImGui.SameLine ()
                        ImGui.Text "Deg"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 50.0f
                        if snaps2dSelected
                        then ImGui.DragFloat ("##d", &d, 0.1f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
                        else ImGui.DragFloat ("##d", &d, 0.0f, 0.0f, 0.0f, "%2.2f") |> ignore<bool> // unchangable 3d rotation
                        ImGui.SameLine ()
                        ImGui.Text "Scl"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 50.0f
                        ImGui.DragFloat ("##s", &s, 0.01f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
                        if snaps2dSelected then snaps2d <- (p, d, s) else snaps3d <- (p, d, s)
                        ImGui.Text "Creation Elevation (2d)"
                        ImGui.DragFloat ("##newEntityElevation", &newEntityElevation, snapDrag, Single.MinValue, Single.MaxValue, "%2.2f") |> ignore<bool>
                        ImGui.Text "Creation Distance (3d)"
                        ImGui.DragFloat ("##newEntityDistance", &newEntityDistance, snapDrag, 0.5f, Single.MaxValue, "%2.2f") |> ignore<bool>
                        ImGui.Text "Input"
                        ImGui.Checkbox ("Alternative Eye Travel Input", &alternativeEyeTravelInput) |> ignore<bool>
                        ImGui.End ()

                    // asset viewer window
                    if ImGui.Begin "Asset Viewer" then
                        if assetViewerSearchRequested then
                            ImGui.SetKeyboardFocusHere ()
                            assetViewerSearchStr <- ""
                            assetViewerSearchRequested <- false
                        ImGui.SetNextItemWidth -1.0f
                        let searchActivePrevious = not (String.IsNullOrWhiteSpace assetViewerSearchStr)
                        ImGui.InputTextWithHint ("##assetViewerSearchStr", "[enter search text]", &assetViewerSearchStr, 4096u) |> ignore<bool>
                        let searchActiveCurrent = not (String.IsNullOrWhiteSpace assetViewerSearchStr)
                        let searchDeactivated = searchActivePrevious && not searchActiveCurrent
                        let assets = Metadata.getDiscoveredAssets ()
                        for package in assets do
                            let flags = ImGuiTreeNodeFlags.SpanAvailWidth ||| ImGuiTreeNodeFlags.OpenOnArrow
                            if searchActiveCurrent then ImGui.SetNextItemOpen true
                            if searchDeactivated then ImGui.SetNextItemOpen false
                            if ImGui.TreeNodeEx (package.Key, flags) then
                                for assetName in package.Value do
                                    if (assetName.ToLowerInvariant ()).Contains (assetViewerSearchStr.ToLowerInvariant ()) then
                                        ImGui.TreeNodeEx (assetName, flags ||| ImGuiTreeNodeFlags.Leaf) |> ignore<bool>
                                        if ImGui.BeginDragDropSource () then
                                            let packageNameText = if Symbol.shouldBeExplicit package.Key then String.surround "\"" package.Key else package.Key
                                            let assetNameText = if Symbol.shouldBeExplicit assetName then String.surround "\"" assetName else assetName
                                            let assetTagStr = "[" + packageNameText + " " + assetNameText + "]"
                                            dragDropPayloadOpt <- Some assetTagStr
                                            ImGui.Text assetTagStr
                                            ImGui.SetDragDropPayload ("Asset", IntPtr.Zero, 0u) |> ignore<bool>
                                            ImGui.EndDragDropSource ()
                                        ImGui.TreePop ()
                                ImGui.TreePop ()
                        ImGui.End ()

                // in full-screen mode, just show full-screen short cut window
                else
                    if ImGui.Begin ("Full Screen Enabled", ImGuiWindowFlags.NoNav) then
                        ImGui.Text "Full Screen (F11)"
                        ImGui.SameLine ()
                        ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            ImGui.Text "Toggle full screen view (F11 to toggle)."
                            ImGui.EndTooltip ()
                        ImGui.End ()

                // if message box not shown, may show another popup
                match messageBoxOpt with
                | None ->

                    // new project dialog
                    if showNewProjectDialog then

                        // ensure template directory exists
                        let programDir = PathF.GetDirectoryName (Reflection.Assembly.GetEntryAssembly().Location)
                        let slnDir = PathF.GetFullPath (programDir + "/../../../../..")
                        let templateDir = PathF.GetFullPath (programDir + "/../../../../Nu.Template")
                        if Directory.Exists templateDir then

                            // prompt user to create new project
                            let title = "Create Nu Project... *EDITOR RESTART REQUIRED!*"
                            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                            if ImGui.BeginPopupModal (title, &showNewProjectDialog) then
                                ImGui.Text "Project Name"
                                ImGui.SameLine ()
                                ImGui.InputText ("##newProjectName", &newProjectName, 4096u) |> ignore<bool>
                                newProjectName <- newProjectName.Replace(" ", "").Replace("\t", "").Replace(".", "")
                                let templateIdentifier = PathF.Denormalize templateDir // this is what dotnet knows the template as for uninstall...
                                let templateFileName = "Nu.Template.fsproj"
                                let projectsDir = PathF.GetFullPath (programDir + "/../../../../../Projects")
                                let newProjectDir = PathF.GetFullPath (projectsDir + "/" + newProjectName)
                                let newProjectDllPath = newProjectDir + "/bin/" + Constants.Gaia.BuildName + "/net7.0/" + newProjectName + ".dll"
                                let newFileName = newProjectName + ".fsproj"
                                let newProject = PathF.GetFullPath (newProjectDir + "/" + newFileName)
                                let validName = not (String.IsNullOrWhiteSpace newProjectName) && Array.notExists (fun char -> newProjectName.Contains (string char)) (PathF.GetInvalidPathChars ())
                                if not validName then ImGui.Text "Invalid project name!"
                                let validDirectory = not (Directory.Exists newProjectDir)
                                if not validDirectory then ImGui.Text "Project already exists!"
                                if validName && validDirectory && (ImGui.Button "Create" || ImGui.IsKeyReleased ImGuiKey.Enter) then

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
                                             "\t\t{" + projectGuidStr + "}.Release|Any CPU.Build.0 = Release|Any CPU"] @
                                            List.skip insertionIndex slnLines
                                        let insertionIndex = List.findIndex ((=) "\tGlobalSection(ExtensibilityGlobals) = postSolution") slnLines - 1
                                        let slnLines =
                                            List.take insertionIndex slnLines @
                                            ["\t\t{" + projectGuidStr + "} = {E3C4D6E1-0572-4D80-84A9-8001C21372D3}"] @
                                            List.skip insertionIndex slnLines
                                        File.WriteAllLines ("Nu.sln", List.toArray slnLines)
                                        Log.info ("Project '" + newProjectName + "'" + "created.")

                                        // configure editor to open new project then exit
                                        let gaiaState = makeGaiaState newProjectDllPath (Some "Title") true
                                        let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                        let gaiaDirectory = PathF.GetDirectoryName gaiaFilePath
                                        try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                                            Directory.SetCurrentDirectory gaiaDirectory
                                            showRestartDialog <- true
                                        with _ -> Log.trace "Could not save gaia state and open new project."

                                        // close dialog
                                        showNewProjectDialog <- false
                                        newProjectName <- "MyGame"

                                    // log failure
                                    with exn -> Log.trace ("Failed to create new project '" + newProjectName + "' due to: " + scstring exn)

                                // escape to cancel
                                if ImGui.IsKeyReleased ImGuiKey.Escape then
                                    showNewProjectDialog <- false
                                    newProjectName <- "MyGame"

                                // fin
                                ImGui.EndPopup ()

                        // template project missing
                        else
                            Log.trace "Template project is missing; new project cannot be generated."
                            showNewProjectDialog <- false

                    // open project dialog
                    if showOpenProjectDialog && not showOpenProjectFileDialog then
                        let title = "Choose a project .dll... *EDITOR RESTART REQUIRED!*"
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showOpenProjectDialog) then
                            ImGui.Text "Game Assembly Path:"
                            ImGui.SameLine ()
                            ImGui.InputTextWithHint ("##openProjectFilePath", "[enter game .dll path]", &openProjectFilePath, 4096u) |> ignore<bool>
                            ImGui.SameLine ()
                            if ImGui.Button "..." then showOpenProjectFileDialog <- true
                            ImGui.Text "Edit Mode:"
                            ImGui.SameLine ()
                            ImGui.InputText ("##openProjectEditMode", &openProjectEditMode, 4096u) |> ignore<bool>
                            ImGui.Checkbox ("Use Imperative Execution (faster, but no Undo / Redo)", &openProjectImperativeExecution) |> ignore<bool>
                            if  (ImGui.Button "Open" || ImGui.IsKeyReleased ImGuiKey.Enter) &&
                                String.notEmpty openProjectFilePath &&
                                File.Exists openProjectFilePath then
                                showOpenProjectDialog <- false
                                let gaiaState = makeGaiaState openProjectFilePath (Some openProjectEditMode) true
                                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                let gaiaDirectory = PathF.GetDirectoryName gaiaFilePath
                                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                                    Directory.SetCurrentDirectory gaiaDirectory
                                    showRestartDialog <- true
                                with _ ->
                                    revertOpenProjectState ()
                                    Log.info "Could not save editor state and open project."
                            if ImGui.IsKeyReleased ImGuiKey.Escape then
                                revertOpenProjectState ()
                                showOpenProjectDialog <- false
                            ImGui.EndPopup ()

                    // open project file dialog
                    elif showOpenProjectFileDialog then
                        projectFileDialogState.Title <- "Choose a game .dll..."
                        projectFileDialogState.FilePattern <- "*.dll"
                        projectFileDialogState.FileDialogType <- ImGuiFileDialogType.Open
                        if ImGui.FileDialog (&showOpenProjectFileDialog, projectFileDialogState) then
                            openProjectFilePath <- projectFileDialogState.FilePath

                    // close project dialog
                    if showCloseProjectDialog then
                        let title = "Close project... *EDITOR RESTART REQUIRED!*"
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showCloseProjectDialog) then
                            ImGui.Text "Close the project and use Gaia in its default state?"
                            if ImGui.Button "Okay" || ImGui.IsKeyReleased ImGuiKey.Enter then
                                showCloseProjectDialog <- false
                                let gaiaState = GaiaState.defaultState
                                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                let gaiaDirectory = PathF.GetDirectoryName gaiaFilePath
                                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                                    Directory.SetCurrentDirectory gaiaDirectory
                                    showRestartDialog <- true
                                with _ -> Log.info "Could not clear editor state and close project."
                            if ImGui.IsKeyReleased ImGuiKey.Escape then showCloseProjectDialog <- false
                            ImGui.EndPopup ()

                    // new group dialog
                    if showNewGroupDialog then
                        let title = "Create a group..."
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showNewGroupDialog) then
                            ImGui.Text "Group Name:"
                            ImGui.SameLine ()
                            ImGui.SetKeyboardFocusHere ()
                            ImGui.InputTextWithHint ("##newGroupName", "[enter group name]", &newGroupName, 4096u) |> ignore<bool>
                            let newGroup = selectedScreen / newGroupName
                            if ImGui.BeginCombo ("##newGroupDispatcherName", newGroupDispatcherName) then
                                let dispatcherNames = (World.getGroupDispatchers world).Keys
                                let dispatcherNamePicked = tryPickName dispatcherNames
                                for dispatcherName in dispatcherNames do
                                    if Some dispatcherName = dispatcherNamePicked then ImGui.SetScrollHereY -0.2f
                                    if ImGui.Selectable (dispatcherName, strEq dispatcherName newGroupDispatcherName) then
                                        newGroupDispatcherName <- dispatcherName
                                ImGui.EndCombo ()
                            if (ImGui.Button "Create" || ImGui.IsKeyReleased ImGuiKey.Enter) && String.notEmpty newGroupName && Address.validName newGroupName && not (newGroup.Exists world) then
                                let worldOld = world
                                try world <- World.createGroup4 newGroupDispatcherName (Some newGroupName) selectedScreen world |> snd
                                    selectEntityOpt None
                                    selectGroup newGroup
                                    showNewGroupDialog <- false
                                    newGroupName <- ""
                                with exn ->
                                    world <- World.switch worldOld
                                    messageBoxOpt <- Some ("Could not create group due to: " + scstring exn)
                            if ImGui.IsKeyReleased ImGuiKey.Escape then showNewGroupDialog <- false
                            ImGui.EndPopup ()

                    // open group dialog
                    if showOpenGroupDialog then
                        groupFileDialogState.Title <- "Choose a nugroup file..."
                        groupFileDialogState.FilePattern <- "*.nugroup"
                        groupFileDialogState.FileDialogType <- ImGuiFileDialogType.Open
                        if ImGui.FileDialog (&showOpenGroupDialog, groupFileDialogState) then
                            snapshot ()
                            showOpenGroupDialog <- not (tryLoadSelectedGroup groupFileDialogState.FilePath)

                    // save group dialog
                    if showSaveGroupDialog then
                        groupFileDialogState.Title <- "Save a nugroup file..."
                        groupFileDialogState.FilePattern <- "*.nugroup"
                        groupFileDialogState.FileDialogType <- ImGuiFileDialogType.Save
                        if ImGui.FileDialog (&showSaveGroupDialog, groupFileDialogState) then
                            snapshot ()
                            if not (PathF.HasExtension groupFileDialogState.FilePath) then groupFileDialogState.FilePath <- groupFileDialogState.FilePath + ".nugroup"
                            showSaveGroupDialog <- not (trySaveSelectedGroup groupFileDialogState.FilePath)

                    // rename group dialog
                    if showRenameGroupDialog then
                        match selectedGroup with
                        | group when group.Exists world ->
                            let title = "Rename group..."
                            let opening = not (ImGui.IsPopupOpen title)
                            if opening then ImGui.OpenPopup title
                            if ImGui.BeginPopupModal (title, &showRenameGroupDialog) then
                                ImGui.Text "Group Name:"
                                ImGui.SameLine ()
                                if opening then
                                    ImGui.SetKeyboardFocusHere ()
                                    groupRename <- group.Name
                                ImGui.InputTextWithHint ("##groupName", "[enter group name]", &groupRename, 4096u) |> ignore<bool>
                                let group' = group.Screen / groupRename
                                if (ImGui.Button "Apply" || ImGui.IsKeyReleased ImGuiKey.Enter) && String.notEmpty groupRename && Address.validName groupRename && not (group'.Exists world) then
                                    snapshot ()
                                    world <- World.renameGroupImmediate group group' world
                                    selectGroup group'
                                    showRenameGroupDialog <- false
                                if ImGui.IsKeyReleased ImGuiKey.Escape then showRenameGroupDialog <- false
                                ImGui.EndPopup ()
                        | _ -> showRenameGroupDialog <- false

                    // rename entity dialog
                    if showRenameEntityDialog then
                        match selectedEntityOpt with
                        | Some entity when entity.Exists world ->
                            let title = "Rename entity..."
                            let opening = not (ImGui.IsPopupOpen title)
                            if opening then ImGui.OpenPopup title
                            if ImGui.BeginPopupModal (title, &showRenameEntityDialog) then
                                ImGui.Text "Entity Name:"
                                ImGui.SameLine ()
                                if opening then
                                    ImGui.SetKeyboardFocusHere ()
                                    entityRename <- entity.Name
                                ImGui.InputTextWithHint ("##entityRename", "[enter entity name]", &entityRename, 4096u) |> ignore<bool>
                                let entity' = Nu.Entity (Array.add entityRename entity.Parent.SimulantAddress.Names)
                                if (ImGui.Button "Apply" || ImGui.IsKeyReleased ImGuiKey.Enter) && String.notEmpty entityRename && Address.validName entityRename && not (entity'.Exists world) then
                                    snapshot ()
                                    world <- World.renameEntityImmediate entity entity' world
                                    selectedEntityOpt <- Some entity'
                                    showRenameEntityDialog <- false
                                if ImGui.IsKeyReleased ImGuiKey.Escape then showRenameEntityDialog <- false
                                ImGui.EndPopup ()
                        | Some _ | None -> showRenameEntityDialog <- false

                    // confirm exit dialog
                    if showConfirmExitDialog then
                        let title = "Are you okay with exiting Gaia?"
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showConfirmExitDialog) then
                            ImGui.Text "Any unsaved changes will be lost."
                            if ImGui.Button "Okay" || ImGui.IsKeyReleased ImGuiKey.Enter then
                                let gaiaState = makeGaiaState projectDllPath (Some projectEditMode) false
                                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                let gaiaDirectory = PathF.GetDirectoryName gaiaFilePath
                                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                                    Directory.SetCurrentDirectory gaiaDirectory
                                with _ -> Log.trace "Could not save gaia state."
                                world <- World.exit world
                            ImGui.SameLine ()
                            if ImGui.Button "Cancel" || ImGui.IsKeyReleased ImGuiKey.Escape then showConfirmExitDialog <- false
                            ImGui.EndPopup ()

                    // restart dialog
                    if showRestartDialog then
                        let title = "Editor restart required."
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal title then
                            ImGui.Text "Gaia will apply your configuration changes and exit. Restart Gaia after exiting."
                            if ImGui.Button "Okay" || ImGui.IsKeyPressed ImGuiKey.Enter then // HACK: checking key pressed event so that previous ui's key release won't bypass this.
                                world <- World.exit world
                            ImGui.EndPopup ()

                // message box dialog
                | Some messageBox ->
                    let title = "Message!"
                    let mutable showing = true
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal (title, &showing) then
                        ImGui.TextWrapped messageBox
                        if ImGui.Button "Okay" || ImGui.IsKeyReleased ImGuiKey.Enter || ImGui.IsKeyReleased ImGuiKey.Escape then showing <- false
                        if not showing then messageBoxOpt <- None
                        ImGui.EndPopup ()

                // entity context menu
                if showEntityContextMenu then
                    ImGui.SetNextWindowPos rightClickPosition
                    ImGui.SetNextWindowSize (v2 280.0f 185.0f)
                    if ImGui.Begin ("ContextMenu", ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoResize) then
                        if ImGui.Button "Create" then
                            createEntity true false
                            showEntityContextMenu <- false
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth -1.0f
                        if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                            let dispatcherNames = (World.getEntityDispatchers world).Keys
                            let dispatcherNamePicked = tryPickName dispatcherNames
                            for dispatcherName in dispatcherNames do
                                if Some dispatcherName = dispatcherNamePicked then ImGui.SetScrollHereY -0.2f
                                if ImGui.Selectable (dispatcherName, strEq dispatcherName newEntityDispatcherName) then
                                    newEntityDispatcherName <- dispatcherName
                                    createEntity true false
                                    showEntityContextMenu <- false
                            ImGui.EndCombo ()
                        if ImGui.Button "Delete" then tryDeleteSelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if  ImGui.IsMouseReleased ImGuiMouseButton.Right ||
                            ImGui.IsKeyReleased ImGuiKey.Escape then
                            showEntityContextMenu <- false
                        ImGui.Separator ()
                        if ImGui.Button "Cut" then tryCutSelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if ImGui.Button "Copy" then tryCopySelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if ImGui.Button "Paste" then tryPaste PasteAtMouse (Option.map cast newEntityParentOpt) |> ignore<bool>; showEntityContextMenu <- false
                        ImGui.Separator ()
                        if ImGui.Button "Set as Creation Parent" then newEntityParentOpt <- selectedEntityOpt; showEntityContextMenu <- false
                        ImGui.Separator ()
                        if ImGui.Button "Show in Hierarchy" then showSelectedEntity <- true; showEntityContextMenu <- false
                        ImGui.End ()

                // imgui inspector window
                if showInspector then
                    ImGui.ShowStackToolWindow ()

                // process non-widget mouse input and hotkeys
                updateEyeDrag ()
                updateEyeTravel ()
                updateEntityContext ()
                updateEntityDrag ()
                updateHotkeys entityHierarchyFocused

                // reloading assets dialog
                if reloadAssetsRequested > 0 then
                    let title = "Reloading assets..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal title then
                        ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
                        ImGui.EndPopup ()
                    reloadAssetsRequested <- inc reloadAssetsRequested
                    if reloadAssetsRequested = 4 then // NOTE: takes multiple frames to see dialog.
                        tryReloadAssets ()
                        reloadAssetsRequested <- 0

                // reloading code dialog
                if reloadCodeRequested > 0 then
                    let title = "Reloading code..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal title then
                        ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
                        ImGui.EndPopup ()
                    reloadCodeRequested <- inc reloadCodeRequested
                    if reloadCodeRequested = 4 then // NOTE: takes multiple frames to see dialog.
                        tryReloadCode ()
                        reloadCodeRequested <- 0

                // reloading assets and code dialog
                if reloadAllRequested > 0 then
                    let title = "Reloading assets and code..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal title then
                        ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
                        ImGui.EndPopup ()
                    reloadAllRequested <- inc reloadAllRequested
                    if reloadAllRequested = 4 then // NOTE: takes multiple frames to see dialog.
                        tryReloadAll ()
                        reloadAllRequested <- 0

            // propagate exception to dialog
            with exn ->
                recoverableExceptionOpt <- Some (exn, worldOld)

        // exception handling dialog
        | Some (exn, worldOld) ->
            let title = "Unhandled Exception!"
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal title then
                ImGui.Text "Exception text:"
                ImGui.TextWrapped (scstring exn)
                ImGui.Text "How would you like to handle this exception?"
                if ImGui.Button "Ignore exception and revert to old world." then
                    world <- World.switch worldOld
                    recoverableExceptionOpt <- None
                if ImGui.Button "Ignore exception and proceed with current world." then
                    recoverableExceptionOpt <- None
                if ImGui.Button "Exit the editor." then
                    showConfirmExitDialog <- true
                    recoverableExceptionOpt <- None
                ImGui.EndPopup ()

        // fin
        world

    let private imGuiPostProcess wtemp =

        // override local desired eye changes if eye was changed elsewhere
        let mutable world = wtemp
        if eyeChangedElsewhere then
            desiredEye2dCenter <- World.getEye2dCenter world
            desiredEye3dCenter <- World.getEye3dCenter world
            desiredEye3dRotation <- World.getEye3dRotation world
            eyeChangedElsewhere <- false
        else
            world <- World.setEye2dCenter desiredEye2dCenter world
            world <- World.setEye3dCenter desiredEye3dCenter world
            world <- World.setEye3dRotation desiredEye3dRotation world
        world

    let rec private runWithCleanUp gaiaState targetDir_ screen wtemp =
        world <- wtemp
        openProjectFilePath <- gaiaState.ProjectDllPath
        openProjectImperativeExecution <- gaiaState.ProjectImperativeExecution
        snaps2dSelected <- gaiaState.Snaps2dSelected
        snaps2d <- gaiaState.Snaps2d
        snaps3d <- gaiaState.Snaps3d
        newEntityElevation <- gaiaState.CreationElevation
        newEntityDistance <- gaiaState.CreationDistance
        alternativeEyeTravelInput <- gaiaState.AlternativeEyeTravelInput
        if not gaiaState.ProjectFreshlyLoaded then
            editWhileAdvancing <- gaiaState.EditWhileAdvancing
            desiredEye2dCenter <- gaiaState.DesiredEye2dCenter
            desiredEye3dCenter <- gaiaState.DesiredEye3dCenter
            desiredEye3dRotation <- gaiaState.DesiredEye3dRotation
            world <- World.setEye2dCenter desiredEye2dCenter world
            world <- World.setEye3dCenter desiredEye3dCenter world
            world <- World.setEye3dRotation desiredEye3dRotation world
            world <- World.setMasterSoundVolume gaiaState.MasterSoundVolume world
            world <- World.setMasterSongVolume gaiaState.MasterSongVolume world
        targetDir <- targetDir_
        projectDllPath <- openProjectFilePath
        projectFileDialogState <- ImGuiFileDialogState targetDir
        projectEditMode <- Option.defaultValue "" gaiaState.ProjectEditModeOpt
        projectImperativeExecution <- openProjectImperativeExecution
        groupFileDialogState <- ImGuiFileDialogState (targetDir + "/../../..")
        selectScreen screen
        selectGroupInitial screen
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
        let result = World.runWithCleanUp tautology imGuiPostProcess id imGuiRender imGuiProcess imGuiPostProcess Live true world
        world <- Unchecked.defaultof<_>
        result

    (* Public API *)

    /// Run Gaia.
    let run gaiaPlugin =

        // discover the desired nu plugin for editing
        let (gaiaState, targetDir, plugin) = selectNuPlugin gaiaPlugin

        // ensure imgui ini file exists and was created by Gaia before initialising imgui
        let imguiIniFilePath = targetDir + "/imgui.ini"
        if  not (File.Exists imguiIniFilePath) ||
            (File.ReadAllLines imguiIniFilePath).[0] <> "[Window][Gaia]" then
            File.WriteAllText (imguiIniFilePath, ImGuiIniFileStr)

        // attempt to create SDL dependencies
        match tryMakeSdlDeps () with
        | Right (sdlConfig, sdlDeps) ->

            // attempt to create the world
            let worldConfig =
                { Imperative = gaiaState.ProjectImperativeExecution
                  Accompanied = true
                  Advancing = false
                  ModeOpt = gaiaState.ProjectEditModeOpt
                  SdlConfig = sdlConfig }
            match tryMakeWorld sdlDeps worldConfig plugin with
            | Right (screen, world) ->

                // subscribe to events related to editing
                let world = World.subscribe handleNuMouseButton Game.MouseLeftDownEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseLeftUpEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseMiddleDownEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseMiddleUpEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseRightDownEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseRightUpEvent Game world
                let world = World.subscribe handleNuSelectedScreenOptChange Game.SelectedScreenOpt.ChangeEvent Game world
                
                // run the world
                runWithCleanUp gaiaState targetDir screen world
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
        | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure