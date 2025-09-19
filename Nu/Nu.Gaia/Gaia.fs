// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Reflection
open System.Text
open FSharp.Compiler.Interactive
open FSharp.NativeInterop
open FSharp.Reflection
open Microsoft.FSharp.Core
open ImGuiNET
open ImGuizmoNET
open ImPlotNET
open JoltPhysicsSharp
open Prime
open Nu
open Nu.Gaia

//////////////////////////////////////////////////////////////////////////////////////
// TODO:                                                                            //
// Perhaps look up (Value)Some-constructed default property values from overlayer.  //
// Custom properties in order of priority:                                          //
//  Enums                                                                           //
//  Flag Enums                                                                      //
//  CollisionMask                                                                   //
//  CollisionCategories                                                             //
//  CollisionDetection                                                              //
//  BodyShape                                                                       //
//  BodyJoint                                                                       //
//  BlendMaterial                                                                   //
//  TerrainMaterial                                                                 //
//  DateTimeOffset?                                                                 //
//  SymbolicCompression                                                             //
//////////////////////////////////////////////////////////////////////////////////////

[<RequireQualifiedAccess>]
module Gaia =

    (* Active Editing States *)

    let mutable private Pasts = [] : (SnapshotType * WorldState) list
    let mutable private Futures = [] : (SnapshotType * WorldState) list
    let mutable private SelectedWindowOpt = Option<string>.None
    let mutable private SelectedWindowRestoreRequested = 0
    let mutable private EntityPropertiesFocusRequested = false
    let mutable private TimelineChanged = false
    let mutable private ManipulationActive = false
    let mutable private ManipulationOperation = OPERATION.TRANSLATE
    let mutable private ExpandEntityHierarchy = false
    let mutable private CollapseEntityHierarchy = false
    let mutable private ShowSelectedEntity = false
    let mutable private RightClickPosition = v2Zero
    let mutable private PropertyFocusedOpt = Option<PropertyDescriptor * Simulant>.None
    let mutable private PropertyEditorFocusRequested = false
    let mutable private EntityHierarchySearchRequested = false
    let mutable private AssetViewerSearchRequested = false
    let mutable private DragDropPayloadOpt = None
    let mutable private DragEntityState = DragEntityInactive
    let mutable private DragEyeState = DragEyeInactive
    let mutable private SelectedScreen = Game / "Screen" // TODO: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable private SelectedGroup = SelectedScreen / "Group" // use the default group
    let mutable private SelectedEntityOpt = Option<Entity>.None
    let mutable private OpenProjectFilePath = null // this will be initialized on start
    let mutable private OpenProjectEditMode = "Title"
    let mutable private OpenProjectImperativeExecution = false
    let mutable private CloseProjectImperativeExecution = false
    let mutable private NewProjectName = "My Game"
    let mutable private NewProjectType = "ImSim Game"
    let mutable private NewGroupDispatcherName = nameof GroupDispatcher
    let mutable private NewEntityDispatcherName = null // this will be initialized on start
    let mutable private NewEntityOverlayName = "(Default Overlay)"
    let mutable private NewEntityParentOpt = Option<Entity>.None
    let mutable private NewEntityElevation = 0.0f
    let mutable private NewEntityDistance = 2.0f
    let mutable private NewGroupName = ""
    let mutable private GroupRename = ""
    let mutable private EntityRename = ""
    let mutable private DesiredEye2dCenter = v2Zero
    let mutable private DesiredEye3dCenter = v3Zero
    let mutable private DesiredEye3dRotation = quatIdentity
    let mutable private EyeChangedElsewhere = false
    let mutable private FpsStartDateTime = DateTimeOffset.Now
    let mutable private FpsStartUpdateTime = 0L
    let mutable private InteractiveNeedsInitialization = true
    let mutable private InteractiveInputFocusRequested = false
    let mutable private InteractiveInputStr = ""
    let mutable private InteractiveOutputStr = ""
    let mutable private LogStr = ""

    (* Configuration States *)

    let mutable private FullScreen = false
    let mutable private CaptureMode = false
    let mutable private EditWhileAdvancing = false
    let mutable private Snaps2dSelected = true
    let mutable private Snaps2d = Constants.Gaia.Snaps2dDefault
    let mutable private Snaps3d = Constants.Gaia.Snaps3dDefault
    let mutable private SnapDrag = 0.1f
    let mutable private AlternativeEyeTravelInput = false
    let mutable private PhysicsDebugRendering2d = false
    let mutable private PhysicsDebugRendering3d = false
    let mutable private ImGuiDebugWindow = false
    let mutable private EntityHierarchySearchStr = ""
    let mutable private PropagationSourcesSearchStr = ""
    let mutable private AssetViewerSearchStr = ""

    (* Project States *)

    let mutable private TargetDir = "."
    let mutable private ProjectDllPath = ""
    let mutable private ProjectFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private ProjectEditMode = ""
    let mutable private ProjectImperativeExecution = false
    let mutable private GroupFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private GroupFilePaths = Map.empty<Group Address, string>
    let mutable private EntityFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private EntityFilePaths = Map.empty<Entity Address, string>
    let mutable private AssetGraphStr = null // this will be initialized on start
    let mutable private OverlayerStr = null // this will be initialized on start

    (* Metrics States *)

    let private TimingCapacity = 200
    let private TimingsArray = Array.zeroCreate<single> TimingCapacity
    let private GcTimings = Queue (Array.zeroCreate<single> TimingCapacity)
    let private MiscTimings = Queue (Array.zeroCreate<single> TimingCapacity)
    let private PhysicsTimings = Queue (Array.zeroCreate<single> TimingCapacity)
    let private UpdateTimings = Queue (Array.zeroCreate<single> TimingCapacity)
    let private RenderMessagesTimings = Queue (Array.zeroCreate<single> TimingCapacity)
    let private ImGuiTimings = Queue (Array.zeroCreate<single> TimingCapacity)
    let private MainThreadTimings = Queue (Array.zeroCreate<single> TimingCapacity)
    let private FrameTimings = Queue (Array.zeroCreate<single> TimingCapacity)

    (* Modal Activity States *)

    let mutable private MessageBoxOpt = Option<string>.None
    let mutable private RecoverableExceptionOpt = Option<Exception>.None
    let mutable private ShowEntityContextMenu = false
    let mutable private ShowNewProjectDialog = false
    let mutable private ShowOpenProjectDialog = false
    let mutable private ShowOpenProjectFileDialog = false
    let mutable private ShowCloseProjectDialog = false
    let mutable private ShowNewGroupDialog = false
    let mutable private ShowOpenGroupDialog = false
    let mutable private ShowSaveGroupDialog = false
    let mutable private ShowRenameGroupDialog = false
    let mutable private ShowOpenEntityDialog = false
    let mutable private ShowSaveEntityDialog = false
    let mutable private ShowRenameEntityDialog = false
    let mutable private ShowDeleteEntityDialog = false
    let mutable private ShowCutEntityDialog = false
    let mutable private ShowConfirmExitDialog = false
    let mutable private ShowRestartDialog = false
    let mutable private ReloadAssetsRequested = 0
    let mutable private ReloadCodeRequested = 0
    let mutable private ReloadAllRequested = 0
    let modal () =
        MessageBoxOpt.IsSome ||
        RecoverableExceptionOpt.IsSome ||
        ShowEntityContextMenu ||
        ShowNewProjectDialog ||
        ShowOpenProjectDialog ||
        ShowOpenProjectFileDialog ||
        ShowCloseProjectDialog ||
        ShowNewGroupDialog ||
        ShowOpenGroupDialog ||
        ShowSaveGroupDialog ||
        ShowRenameGroupDialog ||
        ShowOpenEntityDialog ||
        ShowSaveEntityDialog ||
        ShowRenameEntityDialog ||
        ShowDeleteEntityDialog ||
        ShowCutEntityDialog ||
        ShowConfirmExitDialog ||
        ShowRestartDialog ||
        ReloadAssetsRequested <> 0 ||
        ReloadCodeRequested <> 0 ||
        ReloadAllRequested <> 0

    (* Memoization *)

    let ToSymbolMemo = ForgetfulDictionary<struct (Type * obj), Symbol> (HashIdentity.FromFunctions hash objEq)
    let OfSymbolMemo = ForgetfulDictionary<struct (Type * Symbol), obj> HashIdentity.Structural

    (* Fsi Session *)

    let FsProjectWarnOn = "--warnon:FS1182"
    let FsProjectNoWarn = "--nowarn:FS0009;FS0052;FS1178;FS3391;FS3536;FS3560"
    let FsiArgs = [|"fsi.exe"; "--debug+"; "--debug:full"; "--define:DEBUG"; "--optimize-"; "--tailcalls-"; "--multiemit+"; "--gui-"; "--nologo"; FsProjectWarnOn; FsProjectNoWarn|]
    let FsiConfig = Shell.FsiEvaluationSession.GetDefaultConfiguration ()
    let private FsiErrorStream = new StringWriter ()
    let private FsiInStream = new StringReader ""
    let private FsiOutStream = new StringWriter ()
    let mutable private FsiSession = Unchecked.defaultof<Shell.FsiEvaluationSession>

    (* Initial imgui.ini File Content *)

    let private ImGuiIniFileStr = """
[Window][Gaia]
Pos=0,0
Size=1280,54
Collapsed=0
DockId=0x0000000F,0

[Window][Propagation Sources]
Pos=290,484
Size=335,236
Collapsed=0
DockId=0x0000000E,3

[Window][Edit Overlayer]
Pos=290,484
Size=335,236
Collapsed=0
DockId=0x0000000E,2

[Window][Edit Asset Graph]
Pos=290,484
Size=335,236
Collapsed=0
DockId=0x0000000E,1

[Window][Edit Property]
Pos=290,484
Size=335,236
Collapsed=0
DockId=0x0000000E,0

[Window][Log]
Pos=627,484
Size=356,236
Collapsed=0
DockId=0x00000002,7

[Window][Metrics]
Pos=627,484
Size=356,236
Collapsed=0
DockId=0x00000002,6

[Window][Interactive]
Pos=627,484
Size=356,236
Collapsed=0
DockId=0x00000002,5

[Window][Event Tracing]
Pos=627,484
Size=356,236
Collapsed=0
DockId=0x00000002,4

[Window][Renderer]
Pos=627,484
Size=356,236
Collapsed=0
DockId=0x00000002,3

[Window][Audio Player]
Pos=627,484
Size=356,236
Collapsed=0
DockId=0x00000002,2

[Window][Editor]
Pos=627,484
Size=356,236
Collapsed=0
DockId=0x00000002,1

[Window][Asset Viewer]
Pos=627,484
Size=356,236
Collapsed=0
DockId=0x00000002,0

[Window][Full Screen Enabled]
Pos=20,23
Size=169,77
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
Pos=516,324
Size=247,71
Collapsed=0

[Window][ContextMenu]
Pos=853,391
Size=250,135
Collapsed=0

[Window][Viewport]
Pos=0,0
Size=1280,720
Collapsed=0

[Window][Entity Hierarchy]
Pos=0,56
Size=288,426
Collapsed=0
DockId=0x00000009,0

[Window][Timeline]
Pos=0,484
Size=288,236
Collapsed=0
DockId=0x0000000A,0

[Window][Entity Properties]
Pos=985,56
Size=295,664
Collapsed=0
DockId=0x00000001,3

[Window][Group Properties]
Pos=985,56
Size=295,664
Collapsed=0
DockId=0x00000001,2

[Window][Screen Properties]
Pos=985,56
Size=295,664
Collapsed=0
DockId=0x00000001,1

[Window][Game Properties]
Pos=985,56
Size=295,664
Collapsed=0
DockId=0x00000001,0

[Window][Dear ImGui Debug Log]
Pos=563,268
Size=418,212
Collapsed=0

[Window][Create Nu Project... *EDITOR RESTART REQUIRED!*]
Pos=699,495
Size=524,138
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
Pos=587,326
Size=747,398
Collapsed=0

[Window][Save a nugroup file...]
Pos=613,358
Size=664,399
Collapsed=0

[Window][Choose an Asset...]
Pos=796,323
Size=336,458
Collapsed=0

[Window][Choose a nuentity file...]
Pos=602,352
Size=677,399
Collapsed=0

[Window][Save a nuentity file...]
Pos=613,358
Size=664,399
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
Size=1920,1080
Collapsed=0

[Window][Debug##Default]
Pos=60,60
Size=400,400
Collapsed=0

[Window][Close project... *EDITOR RESTART REQUIRED!*]
Pos=716,510
Size=463,94
Collapsed=0

[Window][Editor restart required.]
Pos=671,504
Size=577,71
Collapsed=0

[Window][WindowOverViewport_11111111]
Pos=0,0
Size=1280,720
Collapsed=0

[Window][Context Menu]
Pos=1377,435
Size=280,323
Collapsed=0

[Docking][Data]
DockSpace           ID=0x7C6B3D9B Window=0xA87D555D Pos=0,0 Size=1280,720 Split=Y
  DockNode          ID=0x0000000F Parent=0x7C6B3D9B SizeRef=1920,54 HiddenTabBar=1 Selected=0x48908BE7
  DockNode          ID=0x00000010 Parent=0x7C6B3D9B SizeRef=1920,1024 Split=X
    DockNode        ID=0x0000000C Parent=0x00000010 SizeRef=983,1080 Split=X
      DockNode      ID=0x00000004 Parent=0x0000000C SizeRef=288,1080 Split=Y
        DockNode    ID=0x00000009 Parent=0x00000004 SizeRef=284,426 Selected=0xAE464409
        DockNode    ID=0x0000000A Parent=0x00000004 SizeRef=284,236 Selected=0x0F18B61B
      DockNode      ID=0x0000000B Parent=0x0000000C SizeRef=693,1080 Split=Y
        DockNode    ID=0x0000000D Parent=0x0000000B SizeRef=1327,426 CentralNode=1 Selected=0xB80FFB58
        DockNode    ID=0x00000003 Parent=0x0000000B SizeRef=1327,236 Split=X Selected=0xB205577F
          DockNode  ID=0x0000000E Parent=0x00000003 SizeRef=335,205 Selected=0x9CF3CB04
          DockNode  ID=0x00000002 Parent=0x00000003 SizeRef=356,205 Selected=0xD92922EC
    DockNode        ID=0x00000001 Parent=0x00000010 SizeRef=295,1080 Selected=0xD5116FF8

"""

    (* Prelude Functions *)

    let private truncateLog () =
        if LogStr.Length > Constants.Gaia.LogCharactersMax then
            let cutPoint = LogStr.IndexOf ('\n', LogStr.Length - Constants.Gaia.LogCharactersMax) |> inc
            LogStr <- "...\n" + LogStr.[cutPoint ..]

    let private concatLog (str : string) =
        truncateLog ()
        LogStr <- LogStr + str

    let private shouldSwallowMouseButton (world : World) =
        let io = ImGui.GetIO ()
        not io.WantCaptureMouseGlobal &&
        (world.Halted || EditWhileAdvancing)

    let private canEditWithMouse (world : World) =
        let io = ImGui.GetIO ()
        not CaptureMode &&
        not io.WantCaptureMouseGlobal &&
        (world.Halted || EditWhileAdvancing)

    let private canEditWithKeyboard (world : World) =
        let io = ImGui.GetIO ()
        not io.WantCaptureKeyboardGlobal &&
        (world.Halted || EditWhileAdvancing)

    let private snapshot snapshotType (world : World) =
        Pasts <- (snapshotType, world.CurrentState) :: Pasts
        Futures <- []
        TimelineChanged <- true

    let private makeGaiaState projectDllPath editModeOpt freshlyLoaded world : GaiaState =
        GaiaState.make
            projectDllPath editModeOpt freshlyLoaded OpenProjectImperativeExecution EditWhileAdvancing
            DesiredEye2dCenter DesiredEye3dCenter DesiredEye3dRotation (World.getMasterSoundVolume world) (World.getMasterSongVolume world)            
            Snaps2dSelected Snaps2d Snaps3d NewEntityElevation NewEntityDistance AlternativeEyeTravelInput

    let private printGaiaState gaiaState =
        PrettyPrinter.prettyPrintSymbol (valueToSymbol gaiaState) PrettyPrinter.defaultPrinter

    let private containsProperty propertyName simulant world =
        SimulantPropertyDescriptor.containsPropertyDescriptor propertyName simulant world

    let private getPropertyValue propertyDescriptor simulant world =
        SimulantPropertyDescriptor.getValue propertyDescriptor simulant world

    let private setPropertyValueWithoutUndo (value : obj) propertyDescriptor simulant world =
        match SimulantPropertyDescriptor.trySetValue value propertyDescriptor simulant world with
        | Right () -> ()
        | Left error -> MessageBoxOpt <- Some error

    let private setPropertyValueIgnoreError (value : obj) propertyDescriptor simulant world =
        snapshot (ChangeProperty (None, propertyDescriptor.PropertyName)) world
        match SimulantPropertyDescriptor.trySetValue value propertyDescriptor simulant world with
        | Right () -> ()
        | Left _ -> ()

    let private setPropertyValue skipSnapshot (value : obj) propertyDescriptor simulant world =
        let skipSnapshot =
            skipSnapshot ||
            match Pasts with
            | (ChangeProperty (mouseLeftIdOpt, _), _) :: _ -> mouseLeftIdOpt = Some ImGui.MouseLeftId
            | _ -> false
        if not skipSnapshot then
            snapshot (ChangeProperty (Some ImGui.MouseLeftId, propertyDescriptor.PropertyName)) world
        setPropertyValueWithoutUndo value propertyDescriptor simulant world

    let private createSceneGroup screen world =
        World.createGroup (Some "Scene") screen world

    let private selectScreen show screen =
        if screen <> SelectedScreen then
            if show then
                ImGui.SetWindowFocus "Screen Properties" // make sure group properties are showing
                ImGui.SetWindowFocus null
            NewEntityParentOpt <- None
            SelectedScreen <- screen

    let private selectGroup show group =
        if group <> SelectedGroup then
            if show then
                ImGui.SetWindowFocus "Group Properties" // make sure group properties are showing
                ImGui.SetWindowFocus null
            NewEntityParentOpt <- None
            SelectedGroup <- group

    let private selectGroupInitial screen world =
        let groups = World.getGroups screen world
        let group =
            match Seq.tryHead groups with
            | Some group -> group
            | None -> createSceneGroup screen world
        selectGroup false group

    let rec private focusPropertyOpt targetOpt world =
        match targetOpt with // special case for selecting property of non-entity to force deselection of entity
        | Some (_, simulant : Simulant) when not (simulant :? Entity) -> selectEntityOpt None world
        | Some _ | None -> ()
        PropertyFocusedOpt <- targetOpt

    and private selectEntityOpt entityOpt world =

        // adjust editing state as needed
        if entityOpt <> SelectedEntityOpt then

            // try to focus on same entity property
            match PropertyFocusedOpt with
            | Some (propertyDescriptor, :? Entity) ->
                match entityOpt with
                | Some entity ->
                    match world |> EntityPropertyDescriptor.getPropertyDescriptors entity |> Seq.filter (fun pd -> pd.PropertyName = propertyDescriptor.PropertyName) |> Seq.tryHead with
                    | Some propertyDescriptor -> focusPropertyOpt (Some (propertyDescriptor, entity)) world
                    | None -> focusPropertyOpt None world
                | Some _ | None -> focusPropertyOpt None world
            | Some _ -> focusPropertyOpt None world
            | None -> ()

            // adjust focus and manipulation / drag state accordingly
            match entityOpt with
            | Some _ -> EntityPropertiesFocusRequested <- true
            | None -> DragEntityState <- DragEntityInactive

            // actually set the selection
            SelectedEntityOpt <- entityOpt

    let private setFullScreen fullScreen world =
        ignore<World> world // not yet used for anything here
        if FullScreen && not fullScreen then SelectedWindowRestoreRequested <- 1
        FullScreen <- fullScreen
        if not FullScreen then CaptureMode <- false

    let private setCaptureMode captureMode world =
        CaptureMode <- captureMode
        if CaptureMode then
            selectEntityOpt None world
            setFullScreen true world
        else setFullScreen false world

    let private tryUndo (world : World) =
        if  not (World.getImperative world) &&
            match Pasts with
            | past :: pasts' ->
                let future = (fst past, world.CurrentState)
                World.switch (snd past) world
                Pasts <- pasts'
                Futures <- future :: Futures
                TimelineChanged <- true
                true
            | [] -> false
        then
            focusPropertyOpt None world
            selectScreen false (World.getSelectedScreen world)
            if not (SelectedGroup.GetExists world) || not (SelectedGroup.GetSelected world) then
                let group = Seq.head (World.getGroups SelectedScreen world)
                selectGroup false group
            match SelectedEntityOpt with
            | Some entity when not (entity.GetExists world) || entity.Group <> SelectedGroup -> selectEntityOpt None world
            | Some _ | None -> ()
            World.setEye2dCenter DesiredEye2dCenter world
            World.setEye3dCenter DesiredEye3dCenter world
            World.setEye3dRotation DesiredEye3dRotation world
            true
        else false

    let private tryRedo world =
        if  not (World.getImperative world) &&
            match Futures with
            | future :: futures' ->
                let past = (fst future, world.CurrentState)
                World.switch (snd future) world
                Pasts <- past :: Pasts
                Futures <- futures'
                TimelineChanged <- true
                true
            | [] -> false
        then
            focusPropertyOpt None world
            selectScreen false (World.getSelectedScreen world)
            if not (SelectedGroup.GetExists world) || not (SelectedGroup.GetSelected world) then
                let group = Seq.head (World.getGroups SelectedScreen world)
                selectGroup false group
            match SelectedEntityOpt with
            | Some entity when not (entity.GetExists world) || entity.Group <> SelectedGroup -> selectEntityOpt None world
            | Some _ | None -> ()
            World.setEye2dCenter DesiredEye2dCenter world
            World.setEye3dCenter DesiredEye3dCenter world
            World.setEye3dRotation DesiredEye3dRotation world
            true
        else false

    let private freezeEntities world =
        snapshot FreezeEntities world
        World.getGroups SelectedScreen world
        |> Seq.map (fun group -> World.getEntities group world)
        |> Seq.concat
        |> Seq.filter (fun entity -> entity.Has<Freezer3dFacet> world)
        |> Seq.filter (fun entity -> not (entity.GetIgnoreGlobalFreezerCommands world))
        |> Seq.iter (fun freezer -> freezer.SetFrozen true world)

    let private thawEntities world =
        snapshot ThawEntities world
        World.getGroups SelectedScreen world
        |> Seq.map (fun group -> World.getEntities group world)
        |> Seq.concat
        |> Seq.filter (fun entity -> entity.Has<Freezer3dFacet> world)
        |> Seq.filter (fun entity -> not (entity.GetIgnoreGlobalFreezerCommands world))
        |> Seq.iter (fun freezer -> freezer.SetFrozen false world)

    let private synchronizeNav world =
        // TODO: sync nav 2d when it's available.
        snapshot SynchronizeNav world
        let navFilePathOpt =
            let nav3d = SelectedScreen.GetNav3d world
            match nav3d.Nav3dMeshOpt with
            | Some (navFilePathOpt, _, _, _) -> navFilePathOpt
            | None -> None
        World.synchronizeNav3d true navFilePathOpt SelectedScreen world

    let private rerenderLightMaps world =
        World.getGroups SelectedScreen world
        |> Seq.map (fun group -> World.getEntities group world)
        |> Seq.concat
        |> Seq.filter (fun entity -> entity.Has<LightProbe3dFacet> world)
        |> Seq.iter (fun lightProbe -> lightProbe.SetProbeStale true world)

    let private tryMoveSelectedEntityToOrigin skipSnapshot world =
        match SelectedEntityOpt with
        | Some selectedEntity ->
            if not skipSnapshot then snapshot MoveEntityToOrigin world
            match Option.bind (flip tryResolve selectedEntity) (selectedEntity.GetMountOpt world) with
            | Some _ -> selectedEntity.SetPositionLocal v3Zero world
            | None -> selectedEntity.SetPosition v3Zero world
            true
        | None -> false

    let private getSnaps () =
        if Snaps2dSelected
        then Snaps2d
        else Snaps3d

    let private getPickCandidates2d world =
        let entities = World.getEntities2dInView (HashSet (QuadelementEqualityComparer ())) world
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = SelectedGroup && entity.GetVisible world) |> Seq.toArray
        entitiesInGroup

    let private getPickCandidates3d world =
        let entities = World.getEntities3dInView (HashSet (OctelementEqualityComparer ())) world
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = SelectedGroup && entity.GetVisible world) |> Seq.toArray
        entitiesInGroup

    let private tryMousePick mousePosition world =
        let entities2d = getPickCandidates2d world
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d world
        match pickedOpt with
        | Some entity ->
            selectEntityOpt (Some entity) world
            Some (0.0f, entity)
        | None ->
            let entities3d = getPickCandidates3d world
            let pickedOpt = World.tryPickEntity3d mousePosition entities3d world
            match pickedOpt with
            | Some (intersection, entity) ->
                selectEntityOpt (Some entity) world
                Some (intersection, entity)
            | None -> None

    let private tryPickName names =
        let io = ImGui.GetIO ()
        io.SwallowKeyboard ()
        [|for key in int ImGuiKey.A .. int ImGuiKey.Z do
            if ImGui.IsKeyReleased (enum key) then
                let chr = char (key - int ImGuiKey.A + 97)
                names
                |> Seq.filter (fun (name : string) -> name.Length > 0 && Char.ToLowerInvariant name.[0] = chr)
                |> Seq.tryHead|]
        |> Array.definitize
        |> Array.tryHead

    let private searchAssetViewer () =
        ImGui.SetWindowFocus "Asset Viewer"
        AssetViewerSearchRequested <- true

    let private searchEntityHierarchy () =
        ImGui.SetWindowFocus "Entity Hierarchy"
        EntityHierarchySearchRequested <- true

    let private revertOpenProjectState (world : World) =
        OpenProjectFilePath <- ProjectDllPath
        OpenProjectEditMode <- ProjectEditMode
        OpenProjectImperativeExecution <- world.Imperative

    let private resolveAssemblyAt (dirPath : string) (args : ResolveEventArgs) =
        let assemblyName = AssemblyName args.Name
        let assemblyFilePath = dirPath + "/" + assemblyName.Name + ".dll"
        if File.Exists assemblyFilePath
        then Assembly.LoadFrom assemblyFilePath
        else null

    // NOTE: this function isn't used, but it is kept around as it's a good tool to surface memory leaks deep in large libs like FSI.
    let private scanAndNullifyFields (root : obj) (targetType : Type) =
        let bindingFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
        let visited = HashSet ()
        let rec scan (obj : obj) =
            if notNull obj then
                let objType = obj.GetType ()
                if not objType.IsValueType && (try not (visited.Contains(obj)) with _ -> false) then
                    visited.Add obj |> ignore
                    let fields = objType.GetFields bindingFlags
                    for field in fields do
                        if not field.FieldType.IsValueType && targetType = field.FieldType 
                        then field.SetValue (obj, null)
                        else
                            let fieldValue = field.GetValue obj
                            scan fieldValue
        scan root

    let private makeContext focusPropertyOpt unfocusPropertyOpt =
        { Snapshot = snapshot
          FocusProperty = match focusPropertyOpt with Some focus -> focus | None -> fun () -> ()
          UnfocusProperty = match unfocusPropertyOpt with Some unfocus -> unfocus | None -> fun () -> ()
          SearchAssetViewer = fun () -> searchAssetViewer ()
          DragDropPayloadOpt = DragDropPayloadOpt
          SnapDrag = SnapDrag
          SelectedScreen = SelectedScreen
          SelectedGroup = SelectedGroup
          SelectedEntityOpt = SelectedEntityOpt
          ToSymbolMemo = ToSymbolMemo
          OfSymbolMemo = OfSymbolMemo }

    (* Nu Event Handling Functions *)

    let private handleNuMouseButton (_ : Event<MouseButtonData, Game>) world =
        if shouldSwallowMouseButton world then Resolve else Cascade

    let private handleNuLifeCycleGroup (evt : Event<LifeCycleEventData, Game>) world =
        match evt.Data with
        | UnregisteringData simulant ->
            if SelectedGroup :> Simulant = simulant then
                let groups = World.getGroups SelectedScreen world
                if Seq.isEmpty groups then
                    let group = createSceneGroup SelectedScreen world // create gui group if no group remains
                    SelectedGroup <- group
                else
                    SelectedGroup <- Seq.head groups
            elif (match SelectedEntityOpt with Some entity -> entity :> Simulant = simulant | None -> false) then
                SelectedEntityOpt <- None
        | _ -> ()
        Cascade

    let private handleNuSelectedScreenOptChange (evt : Event<ChangeData, Game>) world =
        match evt.Data.Value :?> Screen option with
        | Some screen ->
            selectScreen true screen
            selectGroupInitial screen world
            selectEntityOpt None world
            focusPropertyOpt None world
            Cascade
        | None -> Cascade // just keep current group selection and screen if no screen selected

    let private handleNuExitRequest _ (_ : World) =
        ShowConfirmExitDialog <- true
        Cascade

    (* Editor Command Functions *)

    let private createRestorePoint world =
        World.playSound Constants.Audio.SoundVolumeDefault Assets.Default.Sound world
        if world.Advancing then
            World.setAdvancing false world
            snapshot RestorePoint world
            World.setAdvancing true world
        else snapshot RestorePoint world

    let private inductEntity atMouse (entity : Entity) (world : World)=
        let (positionSnap, _, _) = getSnaps ()
        let mutable entityTransform = entity.GetTransform world
        if entity.GetIs2d world then
            let absolute = entity.GetAbsolute world
            let entityPosition =
                if atMouse then Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize RightClickPosition world.RasterViewport
                elif not absolute then world.Eye2dCenter
                else v2Zero
            let attributes = entity.GetAttributesInferred world
            entityTransform.Position <- entityPosition.V3
            entityTransform.Size <- attributes.SizeInferred
            entityTransform.Offset <- attributes.OffsetInferred
            entityTransform.Elevation <- NewEntityElevation
            if Snaps2dSelected && ImGui.IsCtrlUp ()
            then entity.SetTransformPositionSnapped positionSnap entityTransform world
            else entity.SetTransform entityTransform world
        else
            let eyeCenter = world.Eye3dCenter
            let eyeRotation = world.Eye3dRotation
            let eyeFieldOfView = world.Eye3dFieldOfView
            let entityPosition =
                if atMouse then
                    let ray = Viewport.mouseToWorld3d eyeCenter eyeRotation eyeFieldOfView RightClickPosition world.RasterViewport
                    let forward = eyeRotation.Forward
                    let plane = plane3 (eyeCenter + forward * NewEntityDistance) -forward
                    (ray.Intersection plane).Value
                else eyeCenter + v3Forward.Transform eyeRotation * NewEntityDistance
            let attributes = entity.GetAttributesInferred world
            entityTransform.Position <- entityPosition
            entityTransform.Size <- attributes.SizeInferred
            entityTransform.Offset <- attributes.OffsetInferred
            if not Snaps2dSelected && ImGui.IsCtrlUp ()
            then entity.SetTransformPositionSnapped positionSnap entityTransform world
            else entity.SetTransform entityTransform world
        if entity.Surnames.Length > 1 then
            if World.getEntityAllowedToMount entity world then
                entity.SetMountOptWithAdjustment (Some (Address.makeParent ())) world
        match entity.TryGetProperty (nameof entity.ProbeBounds) world with
        | Some property when property.PropertyType = typeof<Box3> ->
            entity.ResetProbeBounds world
        | Some _ | None -> ()

    let private createEntity atMouse inHierarchy world =
        snapshot CreateEntity world
        let dispatcherName = NewEntityDispatcherName
        let overlayNameDescriptor =
            match NewEntityOverlayName with
            | "(Default Overlay)" -> DefaultOverlay
            | "(Routed Overlay)" -> RoutedOverlay
            | "(No Overlay)" -> NoOverlay
            | overlayName -> ExplicitOverlay overlayName
        let name = World.generateEntitySequentialName dispatcherName SelectedGroup world
        let surnames =
            match SelectedEntityOpt with
            | Some entity ->
                if inHierarchy then
                    if entity.GetExists world
                    then Array.add name entity.Surnames
                    else [|name|]
                else
                    match NewEntityParentOpt with
                    | Some newEntityParent when newEntityParent.GetExists world -> Array.add name newEntityParent.Surnames
                    | Some _ | None -> [|name|]
            | None -> [|name|]
        let entity = World.createEntity6 false dispatcherName overlayNameDescriptor (Some surnames) SelectedGroup world
        inductEntity atMouse entity world
        selectEntityOpt (Some entity) world
        ImGui.SetWindowFocus "Viewport"
        ShowSelectedEntity <- true

    let private trySaveSelectedEntity filePath world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            try World.writeEntityToFile false false filePath entity world
                try let deploymentPath = PathF.Combine (TargetDir, PathF.GetRelativePath(TargetDir, filePath).Replace("../", ""))
                    if Directory.Exists (PathF.GetDirectoryName deploymentPath) then
                        if File.Exists deploymentPath then File.SetAttributes (deploymentPath, FileAttributes.None)
                        File.Copy (filePath, deploymentPath, true)
                        File.SetAttributes (deploymentPath, FileAttributes.ReadOnly)
                with exn -> MessageBoxOpt <- Some ("Could not deploy file due to: " + scstring exn)
                EntityFilePaths <- Map.add entity.EntityAddress EntityFileDialogState.FilePath EntityFilePaths
                true
            with exn ->
                MessageBoxOpt <- Some ("Could not save file due to: " + scstring exn)
                false
        | Some _ | None -> false

    let private tryLoadEntity filePath world =

        // attempt to load entity descriptor
        let entityAndDescriptorOpt =
            try let entityDescriptorStr = File.ReadAllText filePath
                let entityDescriptor = scvalue<EntityDescriptor> entityDescriptorStr
                let entityProperties =
                    Map.removeMany
                        [nameof Entity.Position
                         nameof Entity.Rotation
                         nameof Entity.Elevation
                         nameof Entity.Visible]
                        entityDescriptor.EntityProperties
                let entityDescriptor = { entityDescriptor with EntityProperties = entityProperties }
                let entity =
                    match SelectedEntityOpt with
                    | Some entity when entity.GetExists world -> entity
                    | Some _ | None ->
                        let name = Gen.nameForEditor entityDescriptor.EntityDispatcherName
                        let surnames =
                            match NewEntityParentOpt with
                            | Some newEntityParent when newEntityParent.GetExists world -> Array.add name newEntityParent.Surnames
                            | Some _ | None -> [|name|]
                        Nu.Entity (Array.append SelectedGroup.GroupAddress.Names surnames)
                Right (entity, entityDescriptor)
            with exn -> Left exn

        // attempt to load entity
        match entityAndDescriptorOpt with
        | Right (entity, entityDescriptor) ->
            let worldStateOld = world.CurrentState
            try if not (entity.GetExists world) || not (entity.GetProtected world) then
                    if entity.GetExists world then
                        snapshot LoadEntity world
                        let order = entity.GetOrder world
                        let position = entity.GetPosition world
                        let rotation = entity.GetRotation world
                        let elevation = entity.GetElevation world
                        let propagatedDescriptorOpt = entity.GetPropagatedDescriptorOpt world
                        World.destroyEntityImmediate entity world
                        let entity = World.readEntity false true entityDescriptor (Some entity.Name) entity.Parent world
                        entity.SetOrder order world
                        entity.SetPosition position world
                        entity.SetRotation rotation world
                        entity.SetElevation elevation world
                        entity.SetPropagatedDescriptorOpt propagatedDescriptorOpt world
                    else
                        snapshot LoadEntity world
                        let entity = World.readEntity false true entityDescriptor (Some entity.Name) entity.Parent world
                        inductEntity false entity world
                    SelectedEntityOpt <- Some entity
                    EntityFilePaths <- Map.add entity.EntityAddress EntityFileDialogState.FilePath EntityFilePaths
                    EntityFileDialogState.FileName <- ""
                    true
                else
                    MessageBoxOpt <- Some "Cannot load into a protected simulant (such as a group created by the ImSim or MMCC API)."
                    false
            with exn ->
                MessageBoxOpt <- Some ("Could not load entity file due to: " + scstring exn)
                World.switch worldStateOld world
                false

        // error
        | Left exn ->
            MessageBoxOpt <- Some ("Could not load entity file due to: " + scstring exn)
            false

    let private tryDeleteSelectedEntity world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            if not (entity.GetProtected world) then
                if entity.HasPropagationTargets world then
                    ShowDeleteEntityDialog <- true
                    false
                else
                    snapshot DeleteEntity world
                    World.destroyEntity entity world
                    SelectedEntityOpt <- None
                    true
            else
                MessageBoxOpt <- Some "Cannot destroy a protected simulant (such as an entity created by the ImSim or MMCC API)."
                false
        | Some _ | None -> false

    let private tryAutoBoundsSelectedEntity world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            snapshot AutoBoundsEntity world
            entity.AutoBounds world
            true
        | Some _ | None -> false

    let rec private propagateEntityStructure entity world =
        snapshot PropagateEntity world
        World.propagateEntityStructure entity world

    let private tryPropagateSelectedEntityStructure world =
        match SelectedEntityOpt with
        | Some selectedEntity when selectedEntity.GetExists world ->
            propagateEntityStructure selectedEntity world
            true
        | Some _ | None -> false

    let private tryWipeSelectedEntityPropagationTargets world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            snapshot WipePropagationTargets world
            entity.SetPropagatedDescriptorOpt None world
            World.clearPropagationTargets entity world
            true
        | Some _ | None -> false

    let private tryReorderSelectedEntity up world =
        if String.IsNullOrWhiteSpace EntityHierarchySearchStr then
            match SelectedEntityOpt with
            | Some entity when entity.GetExists world ->
                let peerOpt =
                    if up
                    then World.tryGetPreviousEntity entity world
                    else World.tryGetNextEntity entity world
                match peerOpt with
                | Some peer ->
                    snapshot ReorderEntities world
                    World.swapEntityOrders entity peer world
                | None -> ()
            | Some _ | None -> ()

    let private tryCutSelectedEntity world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            if not (entity.GetProtected world) then
                if entity.HasPropagationTargets world then
                    ShowCutEntityDialog <- true
                    false
                else
                    snapshot CutEntity world
                    selectEntityOpt None world
                    World.cutEntityToClipboard entity world
                    true
            else
                MessageBoxOpt <- Some "Cannot cut a protected simulant (such as an entity created by the ImSim or MMCC API)."
                false
        | Some _ | None -> false

    let private tryCopySelectedEntity world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            if not (entity.GetProtected world) then
                World.copyEntityToClipboard entity world
                true
            else
                MessageBoxOpt <- Some "Cannot copy a protected simulant (such as an entity created by the ImSim or MMCC API)."
                false
        | Some _ | None -> false

    let private tryPaste atMouse parentOpt world =
        if World.canPasteEntityFromClipboard world then
            snapshot PasteEntity world
            let positionSnapEir = if Snaps2dSelected then Left (a__ Snaps2d) else Right (a__ Snaps3d)
            let parent = match parentOpt with Some parent -> parent | None -> SelectedGroup :> Simulant
            match World.tryPasteEntityFromClipboard NewEntityDistance RightClickPosition positionSnapEir atMouse parent world with
            | Some entity ->
                selectEntityOpt (Some entity) world
                ImGui.SetWindowFocus "Viewport"
                ShowSelectedEntity <- true
                true
            | None -> false
        else false

    let private trySetSelectedEntityFamilyStatic static_ world =
        let rec setEntityFamilyStatic static_ (entity : Entity) world =
            entity.SetStatic static_ world
            for child in entity.GetChildren world do
                setEntityFamilyStatic static_ child world
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            snapshot (SetEntityFamilyStatic static_) world
            setEntityFamilyStatic static_ entity world
        | Some _ | None -> ()

    let private trySaveSelectedGroup filePath world =
        try World.writeGroupToFile filePath SelectedGroup world
            try let deploymentPath = PathF.Combine (TargetDir, PathF.GetRelativePath(TargetDir, filePath).Replace("../", ""))
                if Directory.Exists (PathF.GetDirectoryName deploymentPath) then
                    if File.Exists deploymentPath then File.SetAttributes (deploymentPath, FileAttributes.None)
                    File.Copy (filePath, deploymentPath, true)
                    File.SetAttributes (deploymentPath, FileAttributes.ReadOnly)
            with exn -> MessageBoxOpt <- Some ("Could not deploy file due to: " + scstring exn)
            GroupFilePaths <- Map.add SelectedGroup.GroupAddress GroupFileDialogState.FilePath GroupFilePaths
            true
        with exn ->
            MessageBoxOpt <- Some ("Could not save file due to: " + scstring exn)
            false

    let private tryLoadGroup filePath (world : World) =

        // attempt to load group descriptor
        let groupAndDescriptorOpt =
            try let groupDescriptorStr = File.ReadAllText filePath
                let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                let groupName =
                    match groupDescriptor.GroupProperties.TryFind Constants.Engine.NamePropertyName with
                    | Some (Atom (name, _) | Text (name, _)) -> name
                    | _ -> failwithumf ()
                Right (SelectedScreen / groupName, groupDescriptor)
            with exn -> Left exn

        // attempt to load group
        match groupAndDescriptorOpt with
        | Right (group, groupDescriptor) ->
            let worldStateOld = world.CurrentState
            try if not (group.GetExists world) || not (group.GetProtected world) then
                    if group.GetExists world then
                        World.destroyGroupImmediate SelectedGroup world
                    let group = World.readGroup groupDescriptor None SelectedScreen world
                    selectGroup true group
                    match SelectedEntityOpt with
                    | Some entity when not (entity.GetExists world) || entity.Group <> SelectedGroup -> selectEntityOpt None world
                    | Some _ | None -> ()
                    GroupFilePaths <- Map.add group.GroupAddress GroupFileDialogState.FilePath GroupFilePaths
                    GroupFileDialogState.FileName <- ""
                    true
                else
                    MessageBoxOpt <- Some "Cannot load into a protected simulant (such as a group created by the ImSim or MMCC API)."
                    false
            with exn ->
                MessageBoxOpt <- Some ("Could not load group file due to: " + scstring exn)
                World.switch worldStateOld world
                false

        // error
        | Left exn ->
            MessageBoxOpt <- Some ("Could not load group file due to: " + scstring exn)
            false

    let private tryReloadAssets world =
        let assetSourceDir = TargetDir + "/../../.."
        match World.tryReloadAssetGraph assetSourceDir TargetDir Constants.Engine.RefinementDir world with
        | Right assetGraph ->
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            AssetGraphStr <- PrettyPrinter.prettyPrint (scstring assetGraph) prettyPrinter
        | Left error ->
            MessageBoxOpt <- Some ("Asset reload error due to: " + error + "'.")

    let private tryReloadCode world =
        if World.getAllowCodeReload world then
            let worldStateOld = world.CurrentState
            snapshot ReloadCode world
            selectEntityOpt None world // NOTE: makes sure old dispatcher doesn't hang around in old cached entity state.
            let workingDirPath = TargetDir + "/../../.."
            Log.info ("Inspecting directory " + workingDirPath + " for F# code...")
            try match Array.ofSeq (Directory.EnumerateFiles (workingDirPath, "*.fsproj")) with
                | [||] -> Log.error ("Unable to find fsproj file in '" + workingDirPath + "'.")
                | fsprojFilePaths ->

                    // generate code reload fsx file string
                    // TODO: P1: consider rewriting this code to use the XML representation to ensure more reliable parsing.
                    let fsprojFilePath = fsprojFilePaths.[0]
                    Log.info ("Inspecting code for F# project '" + fsprojFilePath + "'...")
                    let fsprojFileLines = // TODO: P1: consider loading hard-coded references from Nu.fsproj.
                        [|"""<PackageReference Include="Aether.Physics2D" Version="2.2.0" />"""
                          """<PackageReference Include="DotRecast.Recast.Toolset" Version="2025.2.1" />"""
                          """<PackageReference Include="JoltPhysicsSharp" Version="2.18.4" />"""
                          """<PackageReference Include="Magick.NET-Q8-AnyCPU" Version="14.8.1" />"""
                          """<PackageReference Include="Pfim" Version="0.11.3" />"""
                          """<PackageReference Include="Prime" Version="11.1.2" />"""
                          """<PackageReference Include="System.Configuration.ConfigurationManager" Version="9.0.5" />"""
                          """<PackageReference Include="System.Drawing.Common" Version="9.0.5" />"""
                          """<PackageReference Include="Twizzle.ImGui-Bundle.NET" Version="1.91.5.2" />"""|]
                        |> Array.append (File.ReadAllLines fsprojFilePath)
                    let fsprojNugetPaths =
                        fsprojFileLines
                        |> Array.map (fun line -> line.Trim ())
                        |> Array.filter (fun line -> line.Contains "PackageReference")
                        |> Array.filter (fun line -> not (line.Contains "PackageReference Update="))
                        |> Array.map (fun line -> line.Replace ("<PackageReference Include=", "nuget: "))
                        |> Array.map (fun line -> line.Replace (" Version=", ", "))
                        |> Array.map (fun line -> line.Replace ("/>", ""))
                        |> Array.map (fun line -> line.Replace ("\"", ""))
                        |> Array.map (fun line -> line.Trim ())
                    let fsprojDllFilePaths =
                        fsprojFileLines
                        |> Array.map (fun line -> line.Trim ())
                        |> Array.filter (fun line -> line.Contains "HintPath" && line.Contains ".dll")
                        |> Array.map (fun line -> line.Replace ("<HintPath>", ""))
                        |> Array.map (fun line -> line.Replace ("</HintPath>", ""))
                        |> Array.map (fun line -> line.Replace ("=", ""))
                        |> Array.map (fun line -> line.Replace ("\"", ""))
                        |> Array.map (fun line -> PathF.Normalize line)
                        |> Array.map (fun line -> line.Trim ())
                    let fsprojProjectLines = // TODO: see if we can pull these from the fsproj as well...
                        ["#r \"../../../../../Nu/Nu.Math/bin/" + Constants.Engine.BuildName + "/netstandard2.1/Nu.Math.dll\""
                         "#r \"../../../../../Nu/Nu.Pipe/bin/" + Constants.Engine.BuildName + "/net9.0/Nu.Pipe.dll\""
                         "#r \"../../../../../Nu/Nu.Spine/bin/" + Constants.Engine.BuildName + "/net9.0/Nu.Spine.dll\""
                         "#r \"../../../../../Nu/Nu/bin/" + Constants.Engine.BuildName + "/net9.0/Nu.dll\""]
                    let fsprojFsFilePaths =
                        fsprojFileLines
                        |> Array.map (fun line -> line.Trim ())
                        |> Array.filter (fun line -> line.Contains "Compile Include" && line.Contains ".fs")
                        |> Array.filter (fun line -> line.Contains "Compile Include" && not (line.Contains "Program.fs"))
                        |> Array.map (fun line -> line.Replace ("<Compile Include", ""))
                        |> Array.map (fun line -> line.Replace ("/>", ""))
                        |> Array.map (fun line -> line.Replace ("=", ""))
                        |> Array.map (fun line -> line.Replace ("\"", ""))
                        |> Array.map (fun line -> PathF.Normalize line)
                        |> Array.map (fun line -> line.Trim ())
                    let fsprojDefineConstants =
                        fsprojFileLines
                        |> Array.map (fun line -> line.Trim ())
                        |> Array.filter (fun line -> line.Contains "DefineConstants")
                        |> Array.map (fun line -> line.Replace ("DefineConstants", ""))
                        |> Array.map (fun line -> line.Replace ("/", ""))
                        |> Array.map (fun line -> line.Replace (">", ""))
                        |> Array.map (fun line -> line.Replace ("<", ""))
                        |> String.join ";" // combine all of them since we can't tell which is which
                    let fsxFileString =
                        String.Join ("\n", Array.map (fun (nugetPath : string) -> "#r \"" + nugetPath + "\"") fsprojNugetPaths) + "\n" +
                        String.Join ("\n", Array.map (fun (filePath : string) -> "#r \"../../../" + filePath + "\"") fsprojDllFilePaths) + "\n" +
                        String.Join ("\n", fsprojProjectLines) + "\n" +
                        String.Join ("\n", Array.map (fun (filePath : string) -> "#load \"../../../" + filePath + "\"") fsprojFsFilePaths)

                    // dispose of existing fsi eval session
                    (FsiSession :> IDisposable).Dispose ()

                    // HACK: fix a memory leak caused by FsiEvaluationSession hanging around in a lambda and also roots.
                    let mutable fsiDynamicCompiler = FsiSession.GetType().GetField("fsiDynamicCompiler", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(FsiSession)
                    fsiDynamicCompiler.GetType().GetField("resolveAssemblyRef", BindingFlags.NonPublic ||| BindingFlags.Instance).SetValue(fsiDynamicCompiler, null)
                    fsiDynamicCompiler <- null

                    // HACK: same as above, but for another place.
                    let mutable tcConfigB = FsiSession.GetType().GetField("tcConfigB", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(FsiSession)
                    tcConfigB.GetType().GetField("tryGetMetadataSnapshot@", BindingFlags.NonPublic ||| BindingFlags.Instance).SetValue(tcConfigB, null)
                    tcConfigB <- null

                    // HACK: manually clear fsi eval session since it has such a massive object footprint
                    FsiSession <- Unchecked.defaultof<_>
                    GC.Collect ()

                    // notify user fsi session has been reset
                    InteractiveOutputStr <- "(fsi session reset)"

                    // mark interactive as needing initialization
                    InteractiveNeedsInitialization <- true

                    // create a new session for code reload
                    Log.info ("Compiling code via generated F# script:\n" + fsxFileString)
                    let fsiArgs = if String.notEmpty fsprojDefineConstants then Array.add ("--define:" + fsprojDefineConstants) FsiArgs else FsiArgs
                    FsiSession <- Shell.FsiEvaluationSession.Create (FsiConfig, fsiArgs, FsiInStream, FsiOutStream, FsiErrorStream)
                    match FsiSession.EvalInteractionNonThrowing fsxFileString with
                    | (Choice1Of2 _, _) ->
                        let errorStr = string FsiErrorStream
                        if errorStr.Length > 0
                        then Log.info ("Code compiled with the following warnings (these may disable debugging of reloaded code):\n" + errorStr)
                        else Log.info "Code compiled with no warnings."
                        Log.info "Updating code..."
                        focusPropertyOpt None world // drop any reference to old property type
                        World.updateLateBindings FsiSession.DynamicAssemblies world // replace references to old types
                        Log.info "Code updated."
                    | (Choice2Of2 _, diags) ->
                        let diagsStr = diags |> Array.map _.Message |> String.join Environment.NewLine
                        Log.error ("Failed to compile code due to (see full output in the console):\n" + diagsStr)
                        World.switch worldStateOld world
                    FsiErrorStream.GetStringBuilder().Clear() |> ignore<StringBuilder>
                    FsiOutStream.GetStringBuilder().Clear() |> ignore<StringBuilder>

                    // issue code reload event
                    World.publishPlus () Nu.Game.Handle.CodeReloadEvent (EventTrace.debug "Gaia" "tryReloadCode" "" EventTrace.empty) Nu.Game.Handle false false world

            with exn ->
                Log.error ("Failed to inspect for F# code due to: " + scstring exn)
                World.switch worldStateOld world

        else MessageBoxOpt <- Some "Code reloading not allowed by current plugin. This is likely because you're using the GaiaPlugin which doesn't allow it."

    let private tryReloadAll world =
        tryReloadAssets world
        tryReloadCode world

    let private resetEye () =
        DesiredEye2dCenter <- v2Zero
        DesiredEye3dCenter <- Constants.Engine.Eye3dCenterDefault
        DesiredEye3dRotation <- quatIdentity

    let private toggleAdvancing (world : World) =
        let wasAdvancing = world.Advancing
        snapshot (if wasAdvancing then Halt else Advance) world
        World.setAdvancing (not world.Advancing) world

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
        | Right (Some (filePath, dirPath, types)) ->
            let pluginTypeOpt = Array.tryFind (fun (ty : Type) -> ty.IsSubclassOf typeof<NuPlugin>) types
            match pluginTypeOpt with
            | Some ty ->
                AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler (constant (resolveAssemblyAt dirPath)))
                let plugin = Activator.CreateInstance ty :?> NuPlugin
                Right (Some (filePath, dirPath, plugin))
            | None ->
                Directory.SetCurrentDirectory gaiaDir
                Left ()
        | Right None -> Right None
        | Left () -> Left ()

    let selectNuPlugin gaiaPlugin =
        let gaiaState =
            try if File.Exists Constants.Gaia.StateFilePath
                then scvalue (File.ReadAllText Constants.Gaia.StateFilePath)
                else GaiaState.defaultState
            with _ -> GaiaState.defaultState
        let gaiaDirectory = Directory.GetCurrentDirectory ()
        match trySelectTargetDirAndMakeNuPluginFromFilePathOpt gaiaState.ProjectDllPath with
        | Right (Some (filePath, targetDir, plugin)) ->
            Configure.fromAppConfig filePath
            try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
            with _ -> Log.info "Could not save gaia state."
            (gaiaState, targetDir, plugin)
        | Right None ->
            try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
            with _ -> Log.info "Could not save gaia state."
            (gaiaState, ".", gaiaPlugin)
        | Left () ->
            if not (String.IsNullOrWhiteSpace gaiaState.ProjectDllPath) then
                Log.error ("Invalid Nu Assembly: " + gaiaState.ProjectDllPath)
            (GaiaState.defaultState, ".", gaiaPlugin)

    let private makeWorld sdlDeps worldConfig geometryViewport rasterViewport outerViewport (plugin : NuPlugin) =

        // make the world
        let world = World.make sdlDeps worldConfig geometryViewport rasterViewport outerViewport plugin

        // initialize event filter as not to flood the log
        World.setEventFilter Constants.Gaia.EventFilter world

        // attempt to process ImSim once to make sure initial simulants are created
        World.tryProcessSimulants true world

        // apply any selected mode
        match worldConfig.ModeOpt with
        | Some mode ->
            match plugin.EditModes.TryGetValue mode with
            | (true, modeFn) -> modeFn world
            | (false, _) -> ()
        | None -> ()

        // attempt to process ImSim again to ensure simulants in new mode are created
        World.tryProcessSimulants true world

        // figure out which screen to use
        let screen =
            match Game.GetDesiredScreen world with
            | Desire screen -> screen
            | DesireNone ->
                match Game.GetSelectedScreenOpt world with
                | None ->
                    let screen = Game / "Screen"
                    if not (screen.GetExists world) then
                        let screen = World.createScreen (Some "Screen") world
                        Game.SetDesiredScreen (Desire screen) world
                        screen
                    else screen
                | Some screen -> screen
            | DesireIgnore ->
                match Game.GetSelectedScreenOpt world with
                | None ->
                    let screen = Game / "Screen"
                    if not (screen.GetExists world) then
                        let screen = World.createScreen (Some "Screen") world
                        World.setSelectedScreen screen world
                        let eventTrace = EventTrace.debug "World" "selectScreen" "Select" EventTrace.empty
                        World.publishPlus () screen.SelectEvent eventTrace screen false false world
                        screen
                    else screen
                | Some screen -> screen

        // proceed directly to idle state
        World.selectScreen (IdlingState world.GameTime) screen world
        (screen, world)

    let private tryMakeSdlDeps accompanied windowSize =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Gaia" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        match SdlDeps.tryMake sdlConfig accompanied windowSize with
        | Left msg -> Left msg
        | Right sdlDeps -> Right (sdlConfig, sdlDeps)

    (* Update Functions *)

    let private updateEntityContext world =
        if canEditWithMouse world then
            if ImGui.IsMouseReleased ImGuiMouseButton.Right then
                let mousePosition = World.getMousePosition world
                tryMousePick mousePosition world |> ignore<(single * Entity) option>
                RightClickPosition <- mousePosition
                ShowEntityContextMenu <- true

    let private updateEntityDrag world =

        // attempt to start dragging an entity with the mouse
        if canEditWithMouse world then

            // attempt to start dragging
            if ImGui.IsMouseClicked ImGuiMouseButton.Left then
                let mousePosition = World.getMousePosition world
                match tryMousePick mousePosition world with
                | Some (_, entity) ->
                    if entity.GetIs2d world then
                        if World.isKeyboardAltDown world then
                            let absolute = entity.GetAbsolute world
                            let mousePositionWorld = Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize mousePosition world.RasterViewport
                            let entityDegrees = if entity.MountExists world then entity.GetDegreesLocal world else entity.GetDegrees world
                            DragEntityState <- DragEntityRotation2d (world.DateTime, ref false, mousePositionWorld, entityDegrees.Z + mousePositionWorld.Y, entity)
                        else
                            let entity =
                                if ImGui.IsCtrlDown () && not (entity.GetProtected world) then
                                    snapshot DuplicateEntity world
                                    let entityDescriptor = World.writeEntity false false EntityDescriptor.empty entity world
                                    let entityName = World.generateEntitySequentialName entityDescriptor.EntityDispatcherName entity.Group world
                                    let parent = NewEntityParentOpt |> Option.map cast<Simulant> |> Option.defaultValue entity.Group
                                    let duplicate = World.readEntity false false entityDescriptor (Some entityName) parent world
                                    if ImGui.IsShiftDown () then
                                        duplicate.SetPropagationSourceOpt None world
                                    elif Option.isNone (duplicate.GetPropagationSourceOpt world) then
                                        duplicate.SetPropagationSourceOpt (Some entity) world
                                    let rec getDescendantPairs source entity world =
                                        [for child in World.getEntityChildren entity world do
                                            let childSource = source / child.Name
                                            yield (childSource, child)
                                            yield! getDescendantPairs childSource child world]
                                    for (descendantSource, descendantDuplicate) in getDescendantPairs entity duplicate world do
                                        if descendantDuplicate.GetExists world then
                                            descendantDuplicate.SetPropagatedDescriptorOpt None world
                                            if descendantSource.GetExists world && descendantSource.HasPropagationTargets world then
                                                descendantDuplicate.SetPropagationSourceOpt (Some descendantSource) world
                                    selectEntityOpt (Some duplicate) world
                                    ImGui.SetWindowFocus "Viewport"
                                    ShowSelectedEntity <- true
                                    duplicate
                                else entity
                            let absolute = entity.GetAbsolute world
                            let mousePositionWorld = Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize mousePosition world.RasterViewport
                            let entityPosition = entity.GetPosition world
                            DragEntityState <- DragEntityPosition2d (world.DateTime, ref false, mousePositionWorld, entityPosition.V2 + mousePositionWorld, entity)
                | None -> ()

            // attempt to continue dragging
            match DragEntityState with
            | DragEntityPosition2d (time, snapshottedRef, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = world.DateTime - time
                if entity.GetExists world && localTime.TotalSeconds >= Constants.Gaia.DragMinimumSeconds then
                    if not snapshottedRef.Value then
                        snapshot TranslateEntity world
                        snapshottedRef.Value <- true
                    let mousePositionWorld = World.getMousePosition2dWorld (entity.GetAbsolute world) world
                    let entityPosition = (entityDragOffset - mousePositionWorldOriginal) + (mousePositionWorld - mousePositionWorldOriginal)
                    let entityPositionSnapped =
                        if Snaps2dSelected && ImGui.IsCtrlUp ()
                        then Math.SnapF3d (Triple.fst (getSnaps ()), entityPosition.V3)
                        else entityPosition.V3
                    let entityPosition = entity.GetPosition world
                    let entityPositionDelta = entityPositionSnapped - entityPosition
                    let entityPositionConstrained = entityPosition + entityPositionDelta
                    match Option.bind (flip tryResolve entity) (entity.GetMountOpt world) with
                    | Some parent ->
                        let entityPositionLocal = entityPositionConstrained.Transform (parent.GetAffineMatrix world).Inverted
                        entity.SetPositionLocal entityPositionLocal world
                    | None -> entity.SetPosition entityPositionConstrained world
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                        entity.SetLinearVelocity v3Zero world
                        entity.SetAngularVelocity v3Zero world
            | DragEntityRotation2d (time, snapshottedRef, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = world.DateTime - time
                if entity.GetExists world && localTime.TotalSeconds >= Constants.Gaia.DragMinimumSeconds then
                    if not snapshottedRef.Value then
                        snapshot RotateEntity world
                        snapshottedRef.Value <- true
                    let mousePositionWorld = World.getMousePosition2dWorld (entity.GetAbsolute world) world
                    let entityDegree = (entityDragOffset - mousePositionWorldOriginal.Y) + (mousePositionWorld.Y - mousePositionWorldOriginal.Y)
                    let entityDegreeSnapped =
                        if Snaps2dSelected && ImGui.IsCtrlUp ()
                        then Math.SnapF (Triple.snd (getSnaps ()), entityDegree)
                        else entityDegree
                    let entityDegree = (entity.GetDegreesLocal world).Z
                    if entity.MountExists world then
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegreeLocal = entityDegree + entityDegreeDelta
                        entity.SetDegreesLocal (v3 0.0f 0.0f entityDegreeLocal) world
                    else
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegree = entityDegree + entityDegreeDelta
                        entity.SetDegrees (v3 0.0f 0.0f entityDegree) world
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                        entity.SetLinearVelocity v3Zero world
                        entity.SetAngularVelocity v3Zero world
            | DragEntityInactive -> ()

        // attempt to end dragging
        if ImGui.IsMouseReleased ImGuiMouseButton.Left then
            match DragEntityState with
            | DragEntityPosition2d _ | DragEntityRotation2d _ -> DragEntityState <- DragEntityInactive
            | DragEntityInactive -> ()

    let private updateEyeDrag world =
        if canEditWithMouse world then
            if ImGui.IsMouseClicked ImGuiMouseButton.Middle then
                let mousePositionScreen = World.getMousePosition2dScreen world
                let dragState = DragEye2dCenter (world.Eye2dCenter + mousePositionScreen, mousePositionScreen)
                DragEyeState <- dragState
            match DragEyeState with
            | DragEye2dCenter (entityDragOffset, mousePositionScreenOrig) ->
                let mousePositionScreen = World.getMousePosition2dScreen world
                DesiredEye2dCenter <- (entityDragOffset - mousePositionScreenOrig) + -Constants.Gaia.EyeSpeed * (mousePositionScreen - mousePositionScreenOrig)
                DragEyeState <- DragEye2dCenter (entityDragOffset, mousePositionScreenOrig)
            | DragEyeInactive -> ()
        if ImGui.IsMouseReleased ImGuiMouseButton.Middle then
            match DragEyeState with
            | DragEye2dCenter _ -> DragEyeState <- DragEyeInactive
            | DragEyeInactive -> ()

    let private updateEyeTravel world =
        if canEditWithKeyboard world then
            let delta = world.DateDelta
            let seconds = single delta.TotalSeconds
            let center = world.Eye3dCenter
            let rotation = world.Eye3dRotation
            let moveSpeed =
                if ImGui.IsEnterDown () && ImGui.IsShiftDown () then 512.0f * seconds
                elif ImGui.IsEnterDown () then 64.0f * seconds
                elif ImGui.IsShiftDown () then 1.0f * seconds
                else 8.0f * seconds
            let turnSpeed =
                if ImGui.IsShiftDown () && ImGui.IsEnterUp () then 1.5f * seconds
                else 3.0f * seconds
            if ImGui.IsKeyDown ImGuiKey.W && ImGui.IsCtrlUp () then
                DesiredEye3dCenter <- center + v3Forward.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.S && ImGui.IsCtrlUp () then
                DesiredEye3dCenter <- center + v3Back.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.A && ImGui.IsCtrlUp () then
                DesiredEye3dCenter <- center + v3Left.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.D && ImGui.IsCtrlUp () then
                DesiredEye3dCenter <- center + v3Right.Transform rotation * moveSpeed
            if ImGui.IsKeyDown (if AlternativeEyeTravelInput then ImGuiKey.UpArrow else ImGuiKey.E) && ImGui.IsCtrlUp () then
                let rotation' = rotation * Quaternion.CreateFromAxisAngle (v3Right, turnSpeed)
                DesiredEye3dRotation <-
                    if rotation'.Forward.Dot v3Up < 0.99f then rotation'
                    else
                        Quaternion.CreateFromAxisAngle (v3Down, 2.0f * MathF.Atan2(rotation.Z, rotation.W)) *
                        Quaternion.CreateFromAxisAngle (v3Right, MathF.PI_OVER_2)
            if ImGui.IsKeyDown (if AlternativeEyeTravelInput then ImGuiKey.DownArrow else ImGuiKey.Q) && ImGui.IsCtrlUp () then
                let rotation' = rotation * Quaternion.CreateFromAxisAngle (v3Left, turnSpeed)
                DesiredEye3dRotation <-
                    if rotation'.Forward.Dot v3Down < 0.99f then rotation'
                    else
                        Quaternion.CreateFromAxisAngle (v3Up, 2.0f * MathF.Atan2(rotation.Z, rotation.W)) *
                        Quaternion.CreateFromAxisAngle (v3Right, -MathF.PI_OVER_2)
            if ImGui.IsKeyDown (if AlternativeEyeTravelInput then ImGuiKey.E else ImGuiKey.UpArrow) && ImGui.IsAltUp () then
                DesiredEye3dCenter <- center + v3Up.Transform rotation * moveSpeed
            if ImGui.IsKeyDown (if AlternativeEyeTravelInput then ImGuiKey.Q else ImGuiKey.DownArrow) && ImGui.IsAltUp () then
                DesiredEye3dCenter <- center + v3Down.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.LeftArrow && ImGui.IsAltUp () then
                DesiredEye3dRotation <- Quaternion.CreateFromAxisAngle (v3Up, turnSpeed) * rotation
            if ImGui.IsKeyDown ImGuiKey.RightArrow && ImGui.IsAltUp () then
                DesiredEye3dRotation <- Quaternion.CreateFromAxisAngle (v3Down, turnSpeed) * rotation

    let private updateHotkeys entityHierarchyFocused world =
        if not (modal ()) then
            if ImGui.IsKeyPressed ImGuiKey.F2 && SelectedEntityOpt.IsSome && not (SelectedEntityOpt.Value.GetProtected world) then ShowRenameEntityDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F3 then Snaps2dSelected <- not Snaps2dSelected
            elif ImGui.IsKeyPressed ImGuiKey.F4 && ImGui.IsAltDown () then ShowConfirmExitDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F5 then toggleAdvancing world
            elif ImGui.IsKeyPressed ImGuiKey.F6 then EditWhileAdvancing <- not EditWhileAdvancing
            elif ImGui.IsKeyPressed ImGuiKey.F7 then createRestorePoint world
            elif ImGui.IsKeyPressed ImGuiKey.F8 then ReloadAssetsRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F9 then ReloadCodeRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F10 then setCaptureMode (not CaptureMode) world
            elif ImGui.IsKeyPressed ImGuiKey.F11 then setFullScreen (not FullScreen) world
            elif ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then World.tryToggleWindowFullScreen world
            elif ImGui.IsKeyPressed ImGuiKey.UpArrow && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then tryReorderSelectedEntity true world
            elif ImGui.IsKeyPressed ImGuiKey.DownArrow && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then tryReorderSelectedEntity false world
            elif ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then LogStr <- ""
            elif ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ShowNewGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ShowOpenGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ShowSaveGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.B && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then tryAutoBoundsSelectedEntity world |> ignore<bool>
            elif ImGui.IsKeyPressed ImGuiKey.P && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then NewEntityParentOpt <- SelectedEntityOpt
            elif ImGui.IsKeyPressed ImGuiKey.P && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then NewEntityParentOpt <- None
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltDown () then ShowOpenEntityDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltDown () then (match SelectedEntityOpt with Some entity when entity.GetExists world -> ShowSaveEntityDialog <- true | _ -> ())
            elif ImGui.IsKeyPressed ImGuiKey.R && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ReloadAllRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then searchEntityHierarchy ()
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then ShowOpenProjectDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then freezeEntities world
            elif ImGui.IsKeyPressed ImGuiKey.T && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then thawEntities world
            elif ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then synchronizeNav world
            elif ImGui.IsKeyPressed ImGuiKey.L && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then rerenderLightMaps world
            elif ImGui.IsKeyPressed ImGuiKey.I && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then tryMoveSelectedEntityToOrigin false world |> ignore<bool>
            elif not (ImGui.GetIO ()).WantCaptureKeyboardGlobal || entityHierarchyFocused then
                if ImGui.IsKeyPressed ImGuiKey.Z && ImGui.IsCtrlDown () then tryUndo world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Y && ImGui.IsCtrlDown () then tryRedo world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.X && ImGui.IsCtrlDown () then tryCutSelectedEntity world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlDown () then tryCopySelectedEntity world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.V && ImGui.IsCtrlDown () then tryPaste PasteAtLook (Option.map cast NewEntityParentOpt) world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlDown () then createEntity false false world
                elif ImGui.IsKeyPressed ImGuiKey.Delete then tryDeleteSelectedEntity world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Escape then
                    if not (String.IsNullOrWhiteSpace PropagationSourcesSearchStr) then
                        PropagationSourcesSearchStr <- ""
                    elif not (String.IsNullOrWhiteSpace EntityHierarchySearchStr) then
                        EntityHierarchySearchStr <- ""
                    else
                        focusPropertyOpt None world
                        selectEntityOpt None world

    (* Top-Level Functions *)

    // TODO: split this function up or at least apply intention blocks?
    let private imGuiEntity branch filtering (entity : Entity) (world : World) =
        let selected = match SelectedEntityOpt with Some selectedEntity -> entity = selectedEntity | None -> false
        let treeNodeFlags =
            (if selected then ImGuiTreeNodeFlags.Selected else ImGuiTreeNodeFlags.None) |||
            (if not branch || filtering then ImGuiTreeNodeFlags.Leaf else ImGuiTreeNodeFlags.None) |||
            (if NewEntityParentOpt = Some entity && world.DateTime.Millisecond / 400 % 2 = 0 then ImGuiTreeNodeFlags.Bullet else ImGuiTreeNodeFlags.None) |||
            ImGuiTreeNodeFlags.OpenOnArrow
        if not filtering then
            if ExpandEntityHierarchy then ImGui.SetNextItemOpen true
            if CollapseEntityHierarchy then ImGui.SetNextItemOpen false
        if ShowSelectedEntity then
            match SelectedEntityOpt with
            | Some selectedEntity when selectedEntity.GetExists world ->
                let relation = relate entity selectedEntity
                if  Array.notExists ((=) Constants.Address.ParentName) relation.Names &&
                    relation.Names.Length > 0 then
                    ImGui.SetNextItemOpen true
            | Some _ | None -> ()
        let expanded = ImGui.TreeNodeEx (entity.Name, treeNodeFlags)
        if ShowSelectedEntity && Some entity = SelectedEntityOpt then
            ImGui.SetScrollHereY 0.5f
        // NOTE: dummied out until we can do something about #603.
        //if ImGui.IsKeyPressed ImGuiKey.Space && ImGui.IsItemFocused () && ImGui.IsWindowFocused () then
        //    selectEntityOpt (Some entity) world
        if ImGui.IsMouseReleased ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            selectEntityOpt (Some entity) world
        if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            if not (entity.GetAbsolute world) then
                if entity.GetIs2d world then
                    DesiredEye2dCenter <- (entity.GetPerimeterCenter world).V2
                else
                    let eyeCenterOffset = (v3Back * NewEntityDistance).Transform world.Eye3dRotation
                    DesiredEye3dCenter <- entity.GetPosition world + eyeCenterOffset
        let mutable openPopupContextItemWhenUnselected = false
        let popupContextItemTitle = "##popupContextItem" + scstringMemo entity
        if ImGui.BeginPopupContextItem popupContextItemTitle then
            if ImGui.IsMouseReleased ImGuiMouseButton.Right then openPopupContextItemWhenUnselected <- true
            selectEntityOpt (Some entity) world
            if ImGui.MenuItem "Create Entity" then createEntity false true world
            if SelectedEntityOpt.IsSome && ImGui.MenuItem "Create Entity at Local Origin" then
                createEntity true true world
                tryMoveSelectedEntityToOrigin true world |> ignore<bool>
            if ImGui.MenuItem "Delete Entity" then tryDeleteSelectedEntity world |> ignore<bool>
            ImGui.Separator ()
            if ImGui.MenuItem "Cut Entity" then tryCutSelectedEntity world |> ignore<bool>
            if ImGui.MenuItem "Copy Entity" then tryCopySelectedEntity world |> ignore<bool>
            if ImGui.MenuItem "Paste Entity" then tryPaste PasteAtLook (Some entity) world |> ignore<bool>
            if ImGui.MenuItem "Paste Entity at Local Origin" then
                if tryPaste PasteAtLook (Some entity) world then
                    tryMoveSelectedEntityToOrigin true world |> ignore<bool>
            ImGui.Separator ()
            if ImGui.MenuItem ("Open Entity", "Ctrl+Alt+O") then ShowOpenEntityDialog <- true
            if ImGui.MenuItem ("Save Entity", "Ctrl+Alt+S") then
                match SelectedEntityOpt with
                | Some entity when entity.GetExists world ->
                    match Map.tryFind entity.EntityAddress EntityFilePaths with
                    | Some filePath -> EntityFileDialogState.FilePath <- filePath
                    | None -> EntityFileDialogState.FileName <- ""
                    ShowSaveEntityDialog <- true
                | Some _ | None -> ()
            ImGui.Separator ()
            if NewEntityParentOpt = Some entity then
                if ImGui.MenuItem ("Reset Creation Parent", "Ctrl+Shift+P") then
                    NewEntityParentOpt <- None
                    ShowEntityContextMenu <- false
            else
                if ImGui.MenuItem ("Set as Creation Parent", "Ctrl+P") then
                    NewEntityParentOpt <- SelectedEntityOpt
                    ShowEntityContextMenu <- false
            if ImGui.MenuItem ("Auto Bounds Entity", "Ctrl+B") then tryAutoBoundsSelectedEntity world |> ignore<bool>
            if ImGui.MenuItem ("Move to Origin", "Ctrl+I") then tryMoveSelectedEntityToOrigin false world |> ignore<bool>
            if ImGui.MenuItem ("Propagate Entity") then tryPropagateSelectedEntityStructure world |> ignore<bool>
            if ImGui.MenuItem ("Wipe Propagated Descriptor") then tryWipeSelectedEntityPropagationTargets world |> ignore<bool>
            match SelectedEntityOpt with
            | Some entity when entity.GetExists world ->
                if entity.GetStatic world then
                    if ImGui.MenuItem "Make Entity Family Non-Static" then
                        trySetSelectedEntityFamilyStatic false world
                else
                    if ImGui.MenuItem "Make Entity Family Static" then
                        trySetSelectedEntityFamilyStatic true world
            | Some _ | None -> ()
            let operation = HierarchyContext { EditContext = makeContext None None }
            World.editGame operation Game world
            World.editScreen operation SelectedScreen world
            World.editGroup operation SelectedGroup world
            match SelectedEntityOpt with
            | Some selectedEntity -> World.editEntity operation selectedEntity world
            | None -> ()
            ImGui.EndPopup ()
        if openPopupContextItemWhenUnselected then
            ImGui.OpenPopup popupContextItemTitle
        if ImGui.BeginDragDropTarget () then
            if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr) then
                match DragDropPayloadOpt with
                | Some payload ->
                    let sourceEntityAddressStr = payload
                    let sourceEntity = Nu.Entity sourceEntityAddressStr
                    if not (sourceEntity.GetProtected world) then
                        if ImGui.IsCtrlDown () then
                            let entityDescriptor = World.writeEntity false false EntityDescriptor.empty sourceEntity world
                            let entityName = World.generateEntitySequentialName entityDescriptor.EntityDispatcherName sourceEntity.Group world
                            let parent = Nu.Entity (SelectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                            let duplicate = World.readEntity false false entityDescriptor (Some entityName) parent world
                            if ImGui.IsShiftDown () then
                                duplicate.SetPropagationSourceOpt None world
                            elif Option.isNone (duplicate.GetPropagationSourceOpt world) then
                                duplicate.SetPropagationSourceOpt (Some sourceEntity) world
                            let rec getDescendantPairs source entity world =
                                [for child in World.getEntityChildren entity world do
                                    let childSource = source / child.Name
                                    yield (childSource, child)
                                    yield! getDescendantPairs childSource child world]
                            for (descendantSource, descendantDuplicate) in getDescendantPairs entity duplicate world do
                                if descendantDuplicate.GetExists world then
                                    descendantDuplicate.SetPropagatedDescriptorOpt None world
                                    if descendantSource.GetExists world && descendantSource.HasPropagationTargets world then
                                        descendantDuplicate.SetPropagationSourceOpt (Some descendantSource) world
                            selectEntityOpt (Some duplicate) world
                            ShowSelectedEntity <- true
                        elif ImGui.IsAltDown () then
                            let next = Nu.Entity (SelectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                            let previousOpt = World.tryGetPreviousEntity next world
                            let parentOpt = match next.Parent with :? Entity as parent -> Some parent | _ -> None
                            let canMove =
                                match parentOpt with
                                | Some parent ->
                                    let parentToSource = Address.relate sourceEntity.EntityAddress parent.EntityAddress
                                    Array.contains Constants.Address.ParentName parentToSource.Names
                                | None -> true
                            if canMove then
                                let sourceEntity' =
                                    match parentOpt with
                                    | Some parent -> parent / sourceEntity.Name
                                    | None -> SelectedGroup / sourceEntity.Name
                                if sourceEntity'.GetExists world then
                                    snapshot ReorderEntities world
                                    World.insertEntityOrder sourceEntity previousOpt next world
                                    ShowSelectedEntity <- true
                                else
                                    snapshot ReorderEntities world
                                    World.insertEntityOrder sourceEntity previousOpt next world
                                    World.renameEntityImmediate sourceEntity sourceEntity' world
                                    if NewEntityParentOpt = Some sourceEntity then NewEntityParentOpt <- Some sourceEntity'
                                    selectEntityOpt (Some sourceEntity') world
                                    ShowSelectedEntity <- true
                            else Log.warn "Cannot mount an entity circularly."
                        else
                            let parent = Nu.Entity (SelectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                            let sourceEntity' = parent / sourceEntity.Name
                            let parentToSource = Address.relate sourceEntity.EntityAddress parent.EntityAddress
                            if Array.contains Constants.Address.ParentName parentToSource.Names then
                                if sourceEntity'.GetExists world
                                then MessageBoxOpt <- Some "Cannot reparent an entity where the parent entity contains a child with the same name."
                                else
                                    snapshot RenameEntity world
                                    World.renameEntityImmediate sourceEntity sourceEntity' world
                                    if NewEntityParentOpt = Some sourceEntity then NewEntityParentOpt <- Some sourceEntity'
                                    selectEntityOpt (Some sourceEntity') world
                                    ShowSelectedEntity <- true
                            else Log.warn "Cannot mount an entity circularly."
                    else MessageBoxOpt <- Some "Cannot relocate a protected simulant (such as an entity created by the ImSim or MMCC API)."
                | None -> ()
            ImGui.EndDragDropTarget ()
        if ImGui.BeginDragDropSource () then
            let entityAddressStr = entity.EntityAddress |> scstringMemo |> Symbol.distill
            DragDropPayloadOpt <- Some entityAddressStr
            ImGui.Text (entity.Name + if ImGui.IsCtrlDown () then " (Copy)" else "")
            ImGui.SetDragDropPayload ("Entity", IntPtr.Zero, 0u) |> ignore<bool>
            ImGui.EndDragDropSource ()
        if entity.GetExists world && entity.Has<Freezer3dFacet> world then // check for existence since entity may have been deleted just above
            let frozen = entity.GetFrozen world
            let (text, color) = if frozen then ("Thaw", Color.CornflowerBlue) else ("Freeze", Color.DarkRed)
            ImGui.SameLine ()
            ImGui.PushStyleColor (ImGuiCol.Button, color.Abgr)
            ImGui.PushID ("##frozen" + scstringMemo entity)
            if ImGui.SmallButton text then
                let frozen = not frozen
                snapshot (SetEntityFrozen frozen) world
                entity.SetFrozen frozen world
            ImGui.PopID ()
            ImGui.PopStyleColor ()
        let hasPropagationTargets = entity.HasPropagationTargets world
        let hasPropagationDescriptorOpt = Option.isSome (entity.GetPropagatedDescriptorOpt world)
        if hasPropagationTargets || hasPropagationDescriptorOpt then
            ImGui.SameLine ()
            ImGui.PushID ("##push" + scstringMemo entity)
            if ImGui.SmallButton "Push" then
                propagateEntityStructure entity world
            ImGui.PopID ()
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Propagate entity structure to all targets, preserving propagation data."
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            ImGui.PushID ("##wipe" + scstringMemo entity)
            if ImGui.SmallButton "Wipe" then
                snapshot WipePropagationTargets world
                World.clearPropagationTargets entity world
                entity.SetPropagatedDescriptorOpt None world
            ImGui.PopID ()
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Clear entity structure propagation targets, wiping any propagated descriptor data."
                ImGui.EndTooltip ()
        expanded

    let rec private imGuiEntityHierarchy (entity : Entity) world =
        if entity.GetExists world then // NOTE: entity may have been moved during this process.
            let filtering =
                not (String.IsNullOrWhiteSpace EntityHierarchySearchStr)
            let visible =
                String.IsNullOrWhiteSpace EntityHierarchySearchStr || entity.Name.ToLowerInvariant().Contains (EntityHierarchySearchStr.ToLowerInvariant ())
            let expanded =
                if visible then
                    let branch = entity.HasChildren world
                    imGuiEntity branch filtering entity world
                else false
            if filtering || expanded then
                entity.GetChildren world
                |> Array.ofSeq
                |> Array.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity))
                |> Array.sortBy fst
                |> Array.map snd
                |> Array.iter (fun child -> imGuiEntityHierarchy child world)
                if visible then ImGui.TreePop ()

    let private imGuiEditPropertyRecord
        (getProperty : PropertyDescriptor -> Simulant -> World -> obj)
        (setProperty : bool -> obj -> PropertyDescriptor -> Simulant -> World -> unit)
        (focusProperty : unit -> unit)
        (headered : bool)
        (propertyDescriptor : PropertyDescriptor)
        (simulant : Simulant)
        (world : World) =
        let propertyValue = getProperty propertyDescriptor simulant world
        let context = makeContext (Some focusProperty) None
        let (promoted, edited, propertyValue) = World.imGuiEditPropertyRecord headered propertyDescriptor.PropertyName propertyDescriptor.PropertyType propertyValue context world
        if promoted || edited then setProperty (not edited) propertyValue propertyDescriptor simulant world

    let private imGuiEditProperty
        (getProperty : PropertyDescriptor -> Simulant -> World -> obj)
        (setProperty : bool -> obj -> PropertyDescriptor -> Simulant -> World -> unit)
        (focusProperty : unit -> unit)
        (propertyDescriptor : PropertyDescriptor)
        (simulant : Simulant)
        (world : World) =
        let propertyValue = getProperty propertyDescriptor simulant world
        let context = makeContext (Some focusProperty) None
        let (promoted, edited, propertyValue) = World.imGuiEditProperty propertyDescriptor.PropertyName propertyDescriptor.PropertyType propertyValue context world
        if promoted || edited then setProperty (not edited) propertyValue propertyDescriptor simulant world

    let private imGuiEditEntityAppliedTypes (entity : Entity) world =
        let dispatcherNameCurrent = getTypeName (entity.GetDispatcher world)
        if ImGui.BeginCombo ("Dispatcher Name", dispatcherNameCurrent, ImGuiComboFlags.HeightRegular) then
            let dispatcherNames = (World.getEntityDispatchers world).Keys
            let dispatcherNamePicked = tryPickName dispatcherNames
            for dispatcherName in dispatcherNames do
                if ImGui.Selectable (dispatcherName, strEq dispatcherName dispatcherNameCurrent) then
                    if not (entity.GetProtected world) then
                        snapshot ChangeEntityDispatcher world
                        World.changeEntityDispatcher dispatcherName entity world
                    else MessageBoxOpt <- Some "Cannot change dispatcher of a protected simulant (such as an entity created by the ImSim or MMCC API)."
                if Some dispatcherName = dispatcherNamePicked then ImGui.SetScrollHereY Constants.Gaia.HeightRegularPickOffset
                if dispatcherName = dispatcherNameCurrent then ImGui.SetItemDefaultFocus ()
            ImGui.EndCombo ()
        let facetNameEmpty = "(Empty)"
        let facetNamesValue = entity.GetFacetNames world
        let facetNamesSelectable = (World.getFacets world).Keys |> Seq.toArray |> Array.append [|facetNameEmpty|]
        let facetNamesPropertyDescriptor = { PropertyName = Constants.Engine.FacetNamesPropertyName; PropertyType = typeof<string Set> }
        let mutable facetNamesValue' = Set.empty
        let mutable edited = false
        ImGui.Indent ()
        for i in 0 .. facetNamesValue.Count do
            let last = i = facetNamesValue.Count
            let mutable facetName = if not last then Seq.item i facetNamesValue else facetNameEmpty
            if ImGui.BeginCombo ("Facet Name " + string i, facetName, ImGuiComboFlags.HeightRegular) then
                let facetNameSelectablePicked = tryPickName facetNamesSelectable
                for facetNameSelectable in facetNamesSelectable do
                    if ImGui.Selectable (facetNameSelectable, strEq facetName NewEntityDispatcherName) then
                        facetName <- facetNameSelectable
                        edited <- true
                    if Some facetNameSelectable = facetNameSelectablePicked then ImGui.SetScrollHereY Constants.Gaia.HeightRegularPickOffset
                    if facetNameSelectable = facetName then ImGui.SetItemDefaultFocus ()
                ImGui.EndCombo ()
            if not last && ImGui.IsItemFocused () then focusPropertyOpt (Some (facetNamesPropertyDescriptor, entity :> Simulant)) world
            if facetName <> facetNameEmpty then facetNamesValue' <- Set.add facetName facetNamesValue'
        ImGui.Unindent ()
        if edited then setPropertyValueIgnoreError facetNamesValue' facetNamesPropertyDescriptor entity world

    let private imGuiEditProperties (simulant : Simulant) world =
        let propertyDescriptors = world |> SimulantPropertyDescriptor.getPropertyDescriptors simulant |> Array.ofList
        let propertyDescriptorses = propertyDescriptors |> Array.groupBy EntityPropertyDescriptor.getCategory |> Map.ofSeq
        for (propertyCategory, propertyDescriptors) in propertyDescriptorses.Pairs do
            let (mountActive, modelUsed) =
                match simulant with
                | :? Entity as entity ->
                    let mountActive =
                        match entity.GetMountOpt world with
                        | Some mount ->
                            let parentAddress = Address.resolve mount entity.EntityAddress
                            let parent = World.deriveFromAddress parentAddress
                            parent.Names.Length >= 4 && World.getExists parent world
                        | None -> false
                    (mountActive, (entity.GetProperty Constants.Engine.ModelPropertyName world).PropertyType <> typeof<unit>)
                | :? Group as group -> (false, (group.GetProperty Constants.Engine.ModelPropertyName world).PropertyType <> typeof<unit>)
                | :? Screen as screen -> (false, (screen.GetProperty Constants.Engine.ModelPropertyName world).PropertyType <> typeof<unit>)
                | :? Game as game -> (false, (game.GetProperty Constants.Engine.ModelPropertyName world).PropertyType <> typeof<unit>)
                | _ -> failwithumf ()
            if  (propertyCategory <> "Basic Model Properties" || modelUsed) &&
                ImGui.CollapsingHeader (propertyCategory, ImGuiTreeNodeFlags.DefaultOpen ||| ImGuiTreeNodeFlags.OpenOnArrow) then
                let propertyDescriptors =
                    propertyDescriptors
                    |> Array.filter (fun pd ->
                        SimulantPropertyDescriptor.getEditable pd simulant)
                    |> Array.filter (fun pd ->
                        match pd.PropertyName with
                        | nameof Entity.Position
                        | nameof Entity.Degrees
                        | nameof Entity.Scale
                        | nameof Entity.Elevation
                        | nameof Entity.Enabled
                        | nameof Entity.Visible -> not mountActive
                        | nameof Entity.PositionLocal
                        | nameof Entity.DegreesLocal
                        | nameof Entity.ScaleLocal
                        | nameof Entity.ElevationLocal
                        | nameof Entity.EnabledLocal
                        | nameof Entity.VisibleLocal -> mountActive
                        | _ -> true)
                    |> Array.sortBy (fun pd ->
                        match pd.PropertyName with
                        | Constants.Engine.NamePropertyName -> "!00" // put Name first
                        | Constants.Engine.MountOptPropertyName -> "!01" // and so on...
                        | nameof Entity.PropagationSourceOpt -> "!02"
                        | Constants.Engine.ModelPropertyName -> "!03"
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
                    if containsProperty propertyDescriptor.PropertyName simulant world then // NOTE: this check is necessary because interaction with a property rollout can cause properties to be removed.
                        match propertyDescriptor.PropertyName with
                        | Constants.Engine.NamePropertyName -> // NOTE: name edit properties can't be replaced.
                            match simulant with
                            | :? Screen as screen ->
                                let mutable name = screen.Name
                                ImGui.InputText ("Name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                ImGui.Text ("(" + string (screen.GetId world) + ")")
                            | :? Group as group ->
                                let mutable name = group.Name
                                ImGui.InputText ("##name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                ImGui.Text ("(" + string (group.GetId world) + ")")
                                ImGui.SameLine ()
                                if not (group.GetProtected world) then
                                    if ImGui.Button "Rename" then
                                        ShowRenameGroupDialog <- true
                                else ImGui.Text "Name"
                            | :? Entity as entity ->
                                let mutable name = entity.Name
                                ImGui.InputText ("##name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                if not (entity.GetProtected world) then
                                    if ImGui.Button "Rename" then
                                        ShowRenameEntityDialog <- true
                                else ImGui.Text "Name"
                                ImGui.SameLine ()
                                ImGui.Text ("(" + string (entity.GetId world) + ")")
                            | _ -> ()
                            if ImGui.IsItemFocused () then focusPropertyOpt None world
                        | Constants.Engine.ModelPropertyName ->
                            let getPropertyValue propertyDescriptor simulant world =
                                let propertyValue = getPropertyValue propertyDescriptor simulant world
                                if propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                                    match World.tryTruncateModel propertyValue simulant world with
                                    | Some truncatedValue -> truncatedValue
                                    | None -> propertyValue
                                else propertyValue
                            let setPropertyValue skipSnapshot propertyValue propertyDescriptor simulant world =
                                let propertyValue =
                                    if propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                                        match World.tryUntruncateModel propertyValue simulant world with
                                        | Some untruncatedValue -> untruncatedValue
                                        | None -> propertyValue
                                    else propertyValue
                                setPropertyValue skipSnapshot propertyValue propertyDescriptor simulant world
                            let focusProperty () = focusPropertyOpt (Some (propertyDescriptor, simulant)) world
                            let mutable replaced = false
                            let replaceProperty =
                                ReplaceProperty
                                    { IndicateReplaced = fun () -> replaced <- true
                                      PropertyDescriptor = propertyDescriptor
                                      EditContext = makeContext (Some focusProperty) None }
                            World.edit replaceProperty simulant world
                            if not replaced then
                                if  FSharpType.IsRecord propertyDescriptor.PropertyType ||
                                    FSharpType.isRecordAbstract propertyDescriptor.PropertyType then
                                    imGuiEditPropertyRecord getPropertyValue setPropertyValue focusProperty false propertyDescriptor simulant world
                                else imGuiEditProperty getPropertyValue setPropertyValue focusProperty propertyDescriptor simulant world
                        | _ ->
                            let focusProperty () = focusPropertyOpt (Some (propertyDescriptor, simulant)) world
                            let mutable replaced = false
                            let replaceProperty =
                                ReplaceProperty
                                    { IndicateReplaced = fun () -> replaced <- true
                                      PropertyDescriptor = propertyDescriptor
                                      EditContext = makeContext (Some focusProperty) None }
                            World.edit replaceProperty simulant world
                            if not replaced then imGuiEditProperty getPropertyValue setPropertyValue focusProperty propertyDescriptor simulant world
            if propertyCategory = "Ambient Properties" then // applied types directly after ambient properties
                match simulant with
                | :? Game as game ->
                    let mutable dispatcherNameCurrent = getTypeName (game.GetDispatcher world)
                    ImGui.InputText (Constants.Engine.DispatcherNamePropertyName, &dispatcherNameCurrent, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                | :? Screen as screen ->
                    let mutable dispatcherNameCurrent = getTypeName (screen.GetDispatcher world)
                    ImGui.InputText (Constants.Engine.DispatcherNamePropertyName, &dispatcherNameCurrent, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                | :? Group as group ->
                    let mutable dispatcherNameCurrent = getTypeName (group.GetDispatcher world)
                    ImGui.InputText (Constants.Engine.DispatcherNamePropertyName, &dispatcherNameCurrent, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                | :? Entity as entity ->
                    imGuiEditEntityAppliedTypes entity world
                | _ ->
                    Log.infoOnce "Unexpected simulant type."
        let unfocusProperty () = focusPropertyOpt None world
        let appendProperties : AppendProperties = { EditContext = makeContext None (Some unfocusProperty) }
        World.edit (AppendProperties appendProperties) simulant world

    // NOTE: this function isn't used, but we hope it will be usable once we get unblocked on https://github.com/bryanedds/Nu/issues/549#issuecomment-2551574527
    let private imGuiInnerViewportWindow world =
        let windowName = "Inner Viewport"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            let io = ImGui.GetIO ()
            let displaySize = io.DisplaySize.V2i
            let windowPosition = ImGui.GetWindowPos().V2i
            let windowSize = ImGui.GetWindowSize().V2i
            let rasterViewport = Viewport.makeRaster (box2i (windowPosition.MapY (fun y -> displaySize.Y - y - windowSize.Y)) windowSize)
            World.setRasterViewport rasterViewport world
        ImGui.End ()

    let private imGuiViewportManipulation (world : World) =

        // viewport
        let io = ImGui.GetIO ()
        ImGui.SetNextWindowPos v2Zero
        ImGui.SetNextWindowSize io.DisplaySize
        if ImGui.IsKeyReleased ImGuiKey.Escape && not (modal ()) then ImGui.SetNextWindowFocus ()
        if ImGui.Begin ("Viewport", ImGuiWindowFlags.NoBackground ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoInputs ||| ImGuiWindowFlags.NoNav) then
            if not CaptureMode then

                // physics debug rendering
                if PhysicsDebugRendering2d then
                    World.imGuiRenderPhysics2d world
                if PhysicsDebugRendering3d then
                    let mutable settings3d = DrawSettings (DrawShapeWireframe = true)
                    World.imGuiRenderPhysics3d settings3d world

                // user-defined viewport manipulation
                let rasterViewport = world.RasterViewport
                let projectionMatrix = Viewport.getProjection3d world.Eye3dFieldOfView rasterViewport
                let projection = projectionMatrix.ToArray ()
                let operation =
                    ViewportOverlay
                        { ViewportView = Viewport.getView3d world.Eye3dCenter world.Eye3dRotation
                          ViewportProjection = projectionMatrix
                          ViewportBounds = box2 v2Zero io.DisplaySize
                          EditContext = makeContext None None }
                World.editGame operation Game world
                World.editScreen operation SelectedScreen world
                World.editGroup operation SelectedGroup world
                match SelectedEntityOpt with
                | Some entity when entity.GetExists world && entity.GetIs3d world ->
                    let operation =
                        ViewportOverlay
                            { ViewportView = Viewport.getView3d world.Eye3dCenter world.Eye3dRotation
                              ViewportProjection = projectionMatrix
                              ViewportBounds = box2 v2Zero io.DisplaySize
                              EditContext = makeContext None None }
                    World.editEntity operation entity world
                | Some _ | None -> ()

                // light probe bounds manipulation
                match SelectedEntityOpt with
                | Some entity when entity.GetExists world && entity.Has<LightProbe3dFacet> world ->
                    let mutable lightProbeBounds = entity.GetProbeBounds world
                    let manipulationResult =
                        ImGuizmo.ManipulateBox3
                            (world.Eye3dCenter,
                             world.Eye3dRotation,
                             world.Eye3dFieldOfView,
                             rasterViewport,
                             (if not Snaps2dSelected && ImGui.IsCtrlUp () then Triple.fst Snaps3d else 0.0f),
                             &lightProbeBounds)
                    match manipulationResult with
                    | ImGuiEditActive started ->
                        if started then snapshot (ChangeProperty (None, nameof Entity.ProbeBounds)) world
                        entity.SetProbeBounds lightProbeBounds world
                    | ImGuiEditInactive -> ()
                | Some _ | None -> ()

                // setup guizmo manipulations
                ImGuizmo.SetOrthographic false
                ImGuizmo.SetRect (0.0f, 0.0f, io.DisplaySize.X, io.DisplaySize.Y)
                ImGuizmo.SetDrawlist (ImGui.GetBackgroundDrawList ())

                // transform manipulation
                match SelectedEntityOpt with
                | Some entity when entity.GetExists world && entity.GetIs3d world && not io.WantCaptureMouseLocal && not (ImGuizmo.IsViewManipulateHovered ()) ->
                    let viewMatrix = Viewport.getView3d world.Eye3dCenter world.Eye3dRotation
                    let view = viewMatrix.ToArray ()
                    let affineMatrix = entity.GetAffineMatrix world
                    let affine = affineMatrix.ToArray ()
                    let (p, r, s) =
                        if not Snaps2dSelected && ImGui.IsCtrlUp ()
                        then Snaps3d
                        else (0.0f, 0.0f, 0.0f)
                    let mutable copying = false
                    if not ManipulationActive then
                        if ImGui.IsCtrlDown () && not (entity.GetProtected world) then ManipulationOperation <- OPERATION.TRANSLATE; copying <- true
                        elif ImGui.IsShiftDown () then ManipulationOperation <- OPERATION.SCALE
                        elif ImGui.IsAltDown () then ManipulationOperation <- OPERATION.ROTATE
                        elif ImGui.IsKeyDown ImGuiKey.X then ManipulationOperation <- OPERATION.ROTATE_X
                        elif ImGui.IsKeyDown ImGuiKey.Y then ManipulationOperation <- OPERATION.ROTATE_Y
                        elif ImGui.IsKeyDown ImGuiKey.Z then ManipulationOperation <- OPERATION.ROTATE_Z
                        else ManipulationOperation <- OPERATION.TRANSLATE
                    let mutable snap =
                        match ManipulationOperation with
                        | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> r
                        | _ -> 0.0f // NOTE: doing other snapping ourselves since I don't like guizmo's implementation.
                    let delta = m4Identity.ToArray ()
                    let manipulationResult =
                        if snap = 0.0f
                        then ImGuizmo.Manipulate (&view.[0], &projection.[0], ManipulationOperation, MODE.WORLD, &affine.[0], &delta.[0])
                        else ImGuizmo.Manipulate (&view.[0], &projection.[0], ManipulationOperation, MODE.WORLD, &affine.[0], &delta.[0], &snap)
                    if manipulationResult then
                        let manipulationAwaken =
                            if not ManipulationActive && ImGui.IsMouseDown ImGuiMouseButton.Left then
                                ManipulationActive <- true
                                true
                            else false
                        let affine' = Matrix4x4.CreateFromArray affine
                        let mutable (position, rotation, degrees, scale) = (v3Zero, quatIdentity, v3Zero, v3One)
                        if Matrix4x4.Decompose (affine', &scale, &rotation, &position) then
                            let delta = Matrix4x4.CreateFromArray delta
                            let translation = delta.Translation
                            let epsilon = 0.0001f // NOTE: making this any higher can create false negatives and leave entities positioned at random offsets.
                            if not (Math.ApproximatelyEqual (translation.X, 0.0f, epsilon)) then position.X <- Math.SnapF (p, position.X)
                            if not (Math.ApproximatelyEqual (translation.Y, 0.0f, epsilon)) then position.Y <- Math.SnapF (p, position.Y)
                            if not (Math.ApproximatelyEqual (translation.Z, 0.0f, epsilon)) then position.Z <- Math.SnapF (p, position.Z)
                            rotation <- rotation.Normalized // try to avoid weird angle combinations
                            let rollPitchYaw = rotation.RollPitchYaw
                            degrees.X <- Math.RadiansToDegrees rollPitchYaw.X
                            degrees.Y <- Math.RadiansToDegrees rollPitchYaw.Y
                            degrees.Z <- Math.RadiansToDegrees rollPitchYaw.Z
                            degrees <- if degrees.X = 180.0f && degrees.Z = 180.0f then v3 0.0f (180.0f - degrees.Y) 0.0f else degrees
                            degrees <- v3 degrees.X (if degrees.Y > 180.0f then degrees.Y - 360.0f else degrees.Y) degrees.Z
                            degrees <- v3 degrees.X (if degrees.Y < -180.0f then degrees.Y + 360.0f else degrees.Y) degrees.Z
                            let scaling = delta.Scale
                            if not (Math.ApproximatelyEqual (scaling.X, 0.0f, epsilon)) then scale.X <- Math.SnapF (s, scale.X)
                            if not (Math.ApproximatelyEqual (scaling.Y, 0.0f, epsilon)) then scale.Y <- Math.SnapF (s, scale.Y)
                            if not (Math.ApproximatelyEqual (scaling.Z, 0.0f, epsilon)) then scale.Z <- Math.SnapF (s, scale.Z)
                            if scale.X < 0.01f then scale.X <- 0.01f
                            if scale.Y < 0.01f then scale.Y <- 0.01f
                            if scale.Z < 0.01f then scale.Z <- 0.01f
                        let entity =
                            if copying then
                                if manipulationAwaken then snapshot DuplicateEntity world
                                let entityDescriptor = World.writeEntity false false EntityDescriptor.empty entity world
                                let entityName = World.generateEntitySequentialName entityDescriptor.EntityDispatcherName entity.Group world
                                let parent = NewEntityParentOpt |> Option.map cast<Simulant> |> Option.defaultValue entity.Group
                                let duplicate = World.readEntity false false entityDescriptor (Some entityName) parent world
                                if ImGui.IsShiftDown () then
                                    duplicate.SetPropagationSourceOpt None world
                                elif Option.isNone (duplicate.GetPropagationSourceOpt world) then
                                    duplicate.SetPropagationSourceOpt (Some entity) world
                                let rec getDescendantPairs source entity world =
                                    [for child in World.getEntityChildren entity world do
                                        let childSource = source / child.Name
                                        yield (childSource, child)
                                        yield! getDescendantPairs childSource child world]
                                for (descendantSource, descendantDuplicate) in getDescendantPairs entity duplicate world do
                                    if descendantDuplicate.GetExists world then
                                        descendantDuplicate.SetPropagatedDescriptorOpt None world
                                        if descendantSource.GetExists world && descendantSource.HasPropagationTargets world then
                                            descendantDuplicate.SetPropagationSourceOpt (Some descendantSource) world
                                if duplicate.Has<Freezer3dFacet> world then duplicate.SetFrozen false world
                                selectEntityOpt (Some duplicate) world
                                ImGui.SetWindowFocus "Viewport"
                                ShowSelectedEntity <- true
                                duplicate
                            else
                                if manipulationAwaken then
                                    let snapshotType =
                                        match ManipulationOperation with
                                        | OPERATION.TRANSLATE -> TranslateEntity
                                        | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> RotateEntity
                                        | _ -> ScaleEntity
                                    snapshot snapshotType world
                                    entity
                                else entity
                        match Option.bind (flip tryResolve entity) (entity.GetMountOpt world) with
                        | Some mount ->
                            let mountAffineMatrixInverse = (mount.GetAffineMatrix world).Inverted
                            let positionLocal = position.Transform mountAffineMatrixInverse
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
                            match ManipulationOperation with
                            | OPERATION.TRANSLATE -> entity.SetPositionLocal positionLocal world
                            | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> entity.SetDegreesLocal degreesLocal world
                            | OPERATION.SCALE -> entity.SetScaleLocal scaleLocal world
                            | _ -> () // nothing to do
                        | None ->
                            match ManipulationOperation with
                            | OPERATION.TRANSLATE -> entity.SetPosition position world
                            | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> entity.SetDegrees degrees world
                            | OPERATION.SCALE -> entity.SetScale scale world
                            | _ -> () // nothing to do
                        if world.Advancing then
                            match entity.TryGetProperty (nameof entity.LinearVelocity) world with
                            | Some property when property.PropertyType = typeof<Vector3> -> entity.SetLinearVelocity v3Zero world
                            | Some _ | None -> ()
                            match entity.TryGetProperty (nameof entity.AngularVelocity) world with
                            | Some property when property.PropertyType = typeof<Vector3> -> entity.SetAngularVelocity v3Zero world
                            | Some _ | None -> ()
                    if ImGui.IsMouseReleased ImGuiMouseButton.Left then
                        if ManipulationActive then
                            do (ImGuizmo.Enable false; ImGuizmo.Enable true) // HACK: forces imguizmo to end manipulation when mouse is release over an imgui window.
                            match Option.bind (flip tryResolve entity) (entity.GetMountOpt world) with
                            | Some _ ->
                                match ManipulationOperation with
                                | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z when r <> 0.0f ->
                                    let degreesLocal = Math.SnapDegree3d (r, entity.GetDegreesLocal world)
                                    entity.SetDegreesLocal degreesLocal world
                                | _ -> ()
                            | None ->
                                match ManipulationOperation with
                                | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z when r <> 0.0f ->
                                    let degrees = Math.SnapDegree3d (r, entity.GetDegrees world)
                                    entity.SetDegrees degrees world
                                | _ -> ()
                            ManipulationOperation <- OPERATION.TRANSLATE
                            ManipulationActive <- false
                | Some _ | None -> ()

                // view manipulation
                if not ManipulationActive && DragEntityState = DragEntityInactive then
                    let eyeRotationOld = world.Eye3dRotation
                    let eyeRotationArray = Matrix4x4.CreateFromQuaternion(eyeRotationOld).Transposed.ToArray()
                    ImGuizmo.ViewManipulate (&eyeRotationArray.[0], 1.0f, v2 (single rasterViewport.Bounds.Size.X - 525.0f) 100.0f, v2 128.0f 128.0f, uint 0x00000000)
                    let eyeRotation = Matrix4x4.CreateFromArray(eyeRotationArray).Transposed.Rotation
                    let eyeDiv = eyeRotation.RollPitchYaw.Z / MathF.PI_OVER_2 // NOTE: this and the eyeUpright variable mitigate #932.
                    let eyeUpright = Math.ApproximatelyEqual (eyeDiv, round eyeDiv, 0.01f)
                    if not io.WantCaptureMouseGlobal && eyeRotationOld.Up.Dot eyeRotation.Up >= 0.0f && eyeUpright then DesiredEye3dRotation <- eyeRotation
                    if ImGuizmo.IsUsingViewManipulate () then io.SwallowMouse ()

            // clear editing states that this code otherwise affects
            else
                ManipulationOperation <- OPERATION.TRANSLATE
                ManipulationActive <- false

        ImGui.End ()

    let private imGuiFullScreenWindow world =
        if not CaptureMode then
            if ImGui.Begin ("Full Screen Enabled", ImGuiWindowFlags.NoNav ||| ImGuiWindowFlags.AlwaysAutoResize) then
                ImGui.Text "Capture Mode (F10)"
                ImGui.SameLine ()
                let mutable captureMode = CaptureMode
                if ImGui.Checkbox ("##captureMode", &captureMode) then
                    setCaptureMode captureMode world
                if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                    ImGui.Text "Toggle capture mode (F10 to toggle)."
                    ImGui.EndTooltip ()
                ImGui.Text "Full Screen (F11)"
                ImGui.SameLine ()
                let mutable fullScreen = FullScreen
                ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                setFullScreen fullScreen world
                if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                    ImGui.Text "Toggle full screen view (F11 to toggle)."
                    ImGui.EndTooltip ()
            ImGui.End ()

    let private imGuiMainMenuWindow (world : World) =
        
        // menu window
        if ImGui.Begin ("Gaia", ImGuiWindowFlags.MenuBar) then
            
            // menu bar
            if ImGui.BeginMenuBar () then

                // game menu
                if ImGui.BeginMenu "Game" then
                    if ImGui.MenuItem "New Project" then ShowNewProjectDialog <- true
                    if ImGui.MenuItem ("Open Project", "Ctrl+Shift+O") then ShowOpenProjectDialog <- true
                    if ImGui.MenuItem "Close Project" then ShowCloseProjectDialog <- true
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Undo", "Ctrl+Z") then tryUndo world |> ignore<bool>
                    if ImGui.MenuItem ("Redo", "Ctrl+Y") then tryRedo world |> ignore<bool>
                    ImGui.Separator ()
                    if not world.Advancing then
                        if ImGui.MenuItem ("Advance", "F5") then toggleAdvancing world
                    else
                        if ImGui.MenuItem ("Halt", "F5") then toggleAdvancing world
                    if EditWhileAdvancing
                    then if ImGui.MenuItem ("Disable Edit while Advancing", "F6") then EditWhileAdvancing <- false
                    else if ImGui.MenuItem ("Enable Edit while Advancing", "F6") then EditWhileAdvancing <- true
                    if ImGui.MenuItem ("Create Restore Point", "F7") then createRestorePoint world
                    if ImGui.MenuItem ("Clear Log", "Alt+C") then LogStr <- ""
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Reload Assets", "F8") then ReloadAssetsRequested <- 1
                    if ImGui.MenuItem ("Reload Code", "F9") then ReloadCodeRequested <- 1
                    if ImGui.MenuItem ("Reload All", "Ctrl+R") then ReloadAllRequested <- 1
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Exit", "Alt+F4") then ShowConfirmExitDialog <- true
                    ImGui.EndMenu ()

                // screen menu
                if ImGui.BeginMenu "Screen" then
                    if ImGui.MenuItem ("Thaw Entities", "Ctrl+Shift+T") then freezeEntities world
                    if ImGui.MenuItem ("Freeze Entities", "Ctrl+Shift+F") then freezeEntities world
                    if ImGui.MenuItem ("Rebuild Navigation", "Ctrl+Shift+N") then synchronizeNav world
                    if ImGui.MenuItem ("Rerender Light Maps", "Ctrl+Shift+L") then rerenderLightMaps world
                    ImGui.EndMenu ()

                // group menu
                if ImGui.BeginMenu "Group" then
                    if ImGui.MenuItem ("New Group", "Ctrl+N") then ShowNewGroupDialog <- true
                    if ImGui.MenuItem ("Open Group", "Ctrl+O") then ShowOpenGroupDialog <- true
                    if ImGui.MenuItem ("Save Group", "Ctrl+S") then
                        match Map.tryFind SelectedGroup.GroupAddress GroupFilePaths with
                        | Some filePath -> GroupFileDialogState.FilePath <- filePath
                        | None -> GroupFileDialogState.FileName <- ""
                        ShowSaveGroupDialog <- true
                    if ImGui.MenuItem "Close Group" then
                        let groups = world |> World.getGroups SelectedScreen |> Set.ofSeq
                        if not (SelectedGroup.GetProtected world) && Set.count groups > 1 then
                            snapshot CloseGroup world
                            let groupsRemaining = Set.remove SelectedGroup groups
                            selectEntityOpt None world
                            World.destroyGroupImmediate SelectedGroup world
                            GroupFilePaths <- Map.remove SelectedGroup.GroupAddress GroupFilePaths
                            selectGroup true (Seq.head groupsRemaining)
                        else MessageBoxOpt <- Some "Cannot close protected or only group."
                    ImGui.EndMenu ()

                // entity menu
                if ImGui.BeginMenu "Entity" then
                    if ImGui.MenuItem ("Create Entity", "Ctrl+Enter") then createEntity false false world
                    if ImGui.MenuItem ("Delete Entity", "Delete") then tryDeleteSelectedEntity world |> ignore<bool>
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Cut Entity", "Ctrl+X") then tryCutSelectedEntity world |> ignore<bool>
                    if ImGui.MenuItem ("Copy Entity", "Ctrl+C") then tryCopySelectedEntity world |> ignore<bool>
                    if ImGui.MenuItem ("Paste Entity", "Ctrl+V") then tryPaste PasteAtLook (Option.map cast NewEntityParentOpt) world |> ignore<bool>
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Open Entity", "Ctrl+Alt+O") then ShowOpenEntityDialog <- true
                    if ImGui.MenuItem ("Save Entity", "Ctrl+Alt+S") then
                        match SelectedEntityOpt with
                        | Some entity when entity.GetExists world ->
                            match Map.tryFind entity.EntityAddress EntityFilePaths with
                            | Some filePath -> EntityFileDialogState.FilePath <- filePath
                            | None -> EntityFileDialogState.FileName <- ""
                            ShowSaveEntityDialog <- true
                        | Some _ | None -> ()
                    ImGui.Separator ()
                    if SelectedEntityOpt.IsSome && NewEntityParentOpt = SelectedEntityOpt then
                        if ImGui.MenuItem ("Reset Creation Parent", "Ctrl+Shift+P") then
                            NewEntityParentOpt <- None
                            ShowEntityContextMenu <- false
                    else
                        if ImGui.MenuItem ("Set as Creation Parent", "Ctrl+P") then
                            NewEntityParentOpt <- SelectedEntityOpt
                            ShowEntityContextMenu <- false
                    if ImGui.MenuItem ("Auto Bounds Entity", "Ctrl+B") then tryAutoBoundsSelectedEntity world |> ignore<bool>
                    if ImGui.MenuItem ("Move to Origin", "Ctrl+I") then tryMoveSelectedEntityToOrigin false world |> ignore<bool>
                    if ImGui.MenuItem ("Propagate Entity") then tryPropagateSelectedEntityStructure world |> ignore<bool>
                    if ImGui.MenuItem ("Wipe Propagation Targets") then tryWipeSelectedEntityPropagationTargets world |> ignore<bool>
                    ImGui.EndMenu ()

                // fin
                ImGui.EndMenuBar ()

            // tool bar
            if ImGui.Button "Create" then createEntity false false world
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 200.0f
            if ImGui.BeginCombo ("##newEntityDispatcherName", NewEntityDispatcherName, ImGuiComboFlags.HeightRegular) then
                let dispatcherNames = (World.getEntityDispatchers world).Keys
                let dispatcherNamePicked = tryPickName dispatcherNames
                for dispatcherName in dispatcherNames do
                    if ImGui.Selectable (dispatcherName, strEq dispatcherName NewEntityDispatcherName) then NewEntityDispatcherName <- dispatcherName
                    if Some dispatcherName = dispatcherNamePicked then ImGui.SetScrollHereY Constants.Gaia.HeightRegularPickOffset
                    if dispatcherName = NewEntityDispatcherName then ImGui.SetItemDefaultFocus ()
                ImGui.EndCombo ()
            ImGui.SameLine ()
            ImGui.Text "w/"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 150.0f
            let overlayNames = Seq.append ["(Default Overlay)"; "(Routed Overlay)"; "(No Overlay)"] (World.getOverlayNames world)
            if ImGui.BeginCombo ("##newEntityOverlayName", NewEntityOverlayName, ImGuiComboFlags.HeightRegular) then
                let overlayNamePicked = tryPickName overlayNames
                for overlayName in overlayNames do
                    if ImGui.Selectable (overlayName, strEq overlayName NewEntityOverlayName) then NewEntityOverlayName <- overlayName
                    if Some overlayName = overlayNamePicked then ImGui.SetScrollHereY Constants.Gaia.HeightRegularPickOffset
                    if overlayName = NewEntityOverlayName then ImGui.SetItemDefaultFocus ()
                ImGui.EndCombo ()
            ImGui.SameLine ()
            if ImGui.Button "Auto Bounds" then tryAutoBoundsSelectedEntity world |> ignore<bool>
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Infer an entity's bounds from the asset(s) it uses. (Ctrl+B)"
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            if ImGui.Button "Delete" then tryDeleteSelectedEntity world |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            if world.Halted then
                if ImGui.Button "Advance (F5)" then toggleAdvancing world
            else
                if ImGui.Button "Halt (F5)" then toggleAdvancing world
                ImGui.SameLine ()
                ImGui.Checkbox ("Edit", &EditWhileAdvancing) |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Eye:"
            ImGui.SameLine ()
            if ImGui.Button "Reset" then resetEye ()
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                let mutable eye2dCenter = world.Eye2dCenter
                let mutable eye3dCenter = world.Eye3dCenter
                let mutable eye3dDegrees = Math.RadiansToDegrees3d world.Eye3dRotation.RollPitchYaw
                ImGui.InputFloat2 ("Eye 2d Center", &eye2dCenter, "%3.3f", ImGuiInputTextFlags.ReadOnly) |> ignore
                ImGui.InputFloat3 ("Eye 3d Center", &eye3dCenter, "%3.3f", ImGuiInputTextFlags.ReadOnly) |> ignore
                ImGui.InputFloat3 ("Eye 3d Degrees", &eye3dDegrees, "%3.3f", ImGuiInputTextFlags.ReadOnly) |> ignore
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Reload:"
            ImGui.SameLine ()
            if ImGui.Button "Assets" then ReloadAssetsRequested <- 1
            ImGui.SameLine ()
            if ImGui.Button "Code" then ReloadCodeRequested <- 1
            ImGui.SameLine ()
            if ImGui.Button "All" then ReloadAllRequested <- 1
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Mode:"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 130.0f
            if ImGui.BeginCombo ("##projectEditMode", ProjectEditMode, ImGuiComboFlags.HeightRegular) then
                let editModes = World.getEditModes world
                for (editModeName, editModeFn) in editModes.Pairs do
                    if ImGui.Selectable (editModeName, strEq editModeName ProjectEditMode) then
                        ProjectEditMode <- editModeName
                        snapshot (SetEditMode 0) world // snapshot before mode change
                        selectEntityOpt None world
                        editModeFn world
                        World.tryProcessSimulants true world
                        snapshot (SetEditMode 1) world // snapshot before after change
                    if editModeName = ProjectEditMode then ImGui.SetItemDefaultFocus ()
                ImGui.EndCombo ()
            ImGui.SameLine ()
            if ImGui.Button "Thaw" then thawEntities world
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Thaw all frozen entities. (Ctrl+Shift+T)"
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            if ImGui.Button "Freeze" then freezeEntities world
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Freeze all thawed entities. (Ctrl+Shift+F)"
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            if ImGui.Button "Renavigate" then synchronizeNav world
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Rebuild navigation mesh. (Ctrl+Shift+N)"
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            if ImGui.Button "Relight" then rerenderLightMaps world
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Rerender all light maps. (Ctrl+Shift+L)"
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Capture Mode"
            ImGui.SameLine ()
            let mutable captureMode = CaptureMode
            ImGui.Checkbox ("##captureMode", &captureMode) |> ignore<bool>
            setCaptureMode captureMode world
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Toggle capture mode view (F10 to toggle)."
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            ImGui.Text "Full Screen"
            ImGui.SameLine ()
            let mutable fullScreen = FullScreen
            ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
            setFullScreen fullScreen world
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Toggle full screen view (F11 to toggle)."
                ImGui.EndTooltip ()
            ImGui.SameLine ()
        ImGui.End ()

    let private imGuiHierarchyWindow world =

        // track state for hot key input
        let entityHierarchyFocused =
            let mutable entityHierarchyFocused = false
            let windowName = "Entity Hierarchy"
            if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
                
                // store focus
                if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            
                // allow defocus of entity hierarchy?
                entityHierarchyFocused <- ImGui.IsWindowFocused ()

                // hierarchy operations
                if ImGui.Button "Collapse All" then
                    CollapseEntityHierarchy <- true
                    ImGui.SetWindowFocus "Viewport"
                ImGui.SameLine ()
                if ImGui.Button "Expand All" then
                    ExpandEntityHierarchy <- true
                    ImGui.SetWindowFocus "Viewport"
                ImGui.SameLine ()
                if ImGui.Button "Show Entity" then
                    ShowSelectedEntity <- true
                    ImGui.SetWindowFocus "Viewport"

                // entity search
                if EntityHierarchySearchRequested then
                    ImGui.SetKeyboardFocusHere ()
                    EntityHierarchySearchStr <- ""
                    EntityHierarchySearchRequested <- false
                ImGui.SetNextItemWidth -1.0f
                ImGui.InputTextWithHint ("##entityHierarchySearchStr", "[enter search text]", &EntityHierarchySearchStr, 4096u) |> ignore<bool>
                if ImGui.IsItemFocused () then entityHierarchyFocused <- false

                // creation parent display
                match NewEntityParentOpt with
                | Some newEntityParent when newEntityParent.GetExists world ->
                    let creationParentStr = scstring (Address.skip 2 newEntityParent.EntityAddress)
                    if ImGui.Button creationParentStr then NewEntityParentOpt <- None
                    if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                        ImGui.Text (creationParentStr + " (click to reset)")
                        ImGui.EndTooltip ()
                | Some _ | None ->
                    ImGui.Button (scstring (Address.skip 2 SelectedGroup.GroupAddress)) |> ignore<bool>
                    NewEntityParentOpt <- None
                ImGui.SameLine ()
                ImGui.Text "(creation parent)"

                // group selection
                let groups = world |> World.getGroups SelectedScreen |> Seq.sort
                let mutable selectedGroupName = SelectedGroup.Name
                ImGui.SetNextItemWidth -1.0f
                if ImGui.BeginCombo ("##selectedGroupName", selectedGroupName, ImGuiComboFlags.HeightRegular) then
                    for group in groups do
                        if ImGui.Selectable (group.Name, strEq group.Name selectedGroupName) then
                            selectEntityOpt None world
                            selectGroup true group
                        if group.Name = selectedGroupName then ImGui.SetItemDefaultFocus ()
                    ImGui.EndCombo ()
                if ImGui.BeginDragDropTarget () then
                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr) then
                        match DragDropPayloadOpt with
                        | Some payload ->
                            let sourceEntityAddressStr = payload
                            let sourceEntity = Nu.Entity sourceEntityAddressStr
                            if not (sourceEntity.GetProtected world) then
                                if ImGui.IsCtrlDown () then
                                    let entityDescriptor = World.writeEntity false false EntityDescriptor.empty sourceEntity world
                                    let entityName = World.generateEntitySequentialName entityDescriptor.EntityDispatcherName sourceEntity.Group world
                                    let parent = sourceEntity.Group
                                    let duplicate = World.readEntity false false entityDescriptor (Some entityName) parent world
                                    if ImGui.IsShiftDown () then
                                        duplicate.SetPropagationSourceOpt None world
                                    elif Option.isNone (duplicate.GetPropagationSourceOpt world) then
                                        duplicate.SetPropagationSourceOpt (Some sourceEntity) world
                                    let rec getDescendantPairs source entity world =
                                        [for child in World.getEntityChildren entity world do
                                            let childSource = source / child.Name
                                            yield (childSource, child)
                                            yield! getDescendantPairs childSource child world]
                                    for (descendantSource, descendantDuplicate) in getDescendantPairs sourceEntity duplicate world do
                                        if descendantDuplicate.GetExists world then
                                            descendantDuplicate.SetPropagatedDescriptorOpt None world
                                            if descendantSource.GetExists world && descendantSource.HasPropagationTargets world then
                                                descendantDuplicate.SetPropagationSourceOpt (Some descendantSource) world
                                    selectEntityOpt (Some duplicate) world
                                    ShowSelectedEntity <- true
                                else
                                    let sourceEntity' = Nu.Entity (SelectedGroup.GroupAddress <-- Address.makeFromName sourceEntity.Name)
                                    if not (sourceEntity'.GetExists world) then
                                        if World.getEntityAllowedToMount sourceEntity world then
                                            sourceEntity.SetMountOptWithAdjustment None world
                                        World.renameEntityImmediate sourceEntity sourceEntity' world
                                        if NewEntityParentOpt = Some sourceEntity then NewEntityParentOpt <- Some sourceEntity'
                                        selectEntityOpt (Some sourceEntity') world
                                        ShowSelectedEntity <- true
                                    else MessageBoxOpt <- Some "Cannot unparent an entity when there exists another unparented entity with the same name."
                            else MessageBoxOpt <- Some "Cannot relocate a protected simulant (such as an entity created by the ImSim or MMCC API)."
                        | None -> ()
                    ImGui.EndDragDropTarget ()

                // entity editing
                ImGui.BeginChild "Container" |> ignore<bool>
                let children =
                    World.getSovereignEntities SelectedGroup world
                    |> Array.ofSeq
                    |> Array.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity))
                    |> Array.sortBy fst
                    |> Array.map snd
                for child in children do
                    imGuiEntityHierarchy child world
                ImGui.EndChild ()

                // finish entity showing
                ShowSelectedEntity <- false

                // fin
                entityHierarchyFocused

            // allow defocus of entity hierarchy
            else false

        // fin
        ImGui.End ()
        entityHierarchyFocused

    let private imGuiTimelineWindow () =
        let windowName = "Timeline"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            if ImGui.BeginListBox ("##history", v2 -1.0f -1.0f) then
                for (snapshotType, _) in List.rev Pasts do
                    let snapshotLabel = snapshotType.Label
                    ImGui.Text snapshotLabel
                    if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                        ImGui.Text snapshotLabel
                        ImGui.EndTooltip ()
                ImGui.SeparatorText "<Present>"
                if TimelineChanged then
                    ImGui.SetScrollHereY 0.5f
                    TimelineChanged <- false
                for (snapshotType, _) in Futures do
                    let snapshotLabel = snapshotType.Label
                    ImGui.Text snapshotLabel
                    if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                        ImGui.Text snapshotLabel
                        ImGui.EndTooltip ()
                ImGui.EndListBox ()
        ImGui.End ()

    let private imGuiGamePropertiesWindow world =
        let windowName = "Game Properties"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            imGuiEditProperties Game world
        ImGui.End ()

    let private imGuiScreenPropertiesWindow world =
        let windowName = "Screen Properties"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            imGuiEditProperties SelectedScreen world
        ImGui.End ()

    let private imGuiGroupPropertiesWindow world =
        let windowName = "Group Properties"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            imGuiEditProperties SelectedGroup world
        ImGui.End ()

    let private imGuiEntityPropertiesWindow world =
        let windowName = "Entity Properties"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            match SelectedEntityOpt with
            | Some entity when entity.GetExists world -> imGuiEditProperties entity world
            | Some _ | None -> ()
        ImGui.End ()

    let private imGuiPropagationSourcesWindow world =
        let windowName = "Propagation Sources"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            ImGui.SetNextItemWidth -1.0f
            ImGui.InputTextWithHint ("##propagationSourcesSearchStr", "[enter search text]", &PropagationSourcesSearchStr, 4096u) |> ignore<bool>
            let propagationSources =
                World.getPropagationSources world
                |> Seq.filter (fun entity ->
                    String.IsNullOrWhiteSpace PropagationSourcesSearchStr ||
                    entity.Name.ToLowerInvariant().Contains (PropagationSourcesSearchStr.ToLowerInvariant ()))
                |> Seq.filter (fun entity -> not (entity.GetProtected world))
                |> hashSetPlus HashIdentity.Structural
            ImGui.BeginChild "Container" |> ignore<bool>
            for entity in propagationSources do
                let treeNodeFlags = ImGuiTreeNodeFlags.Leaf ||| if Option.contains entity SelectedEntityOpt then ImGuiTreeNodeFlags.Selected else ImGuiTreeNodeFlags.None
                if ImGui.TreeNodeEx (entity.Name, treeNodeFlags) then
                    if ImGui.IsMouseReleased ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                        selectEntityOpt (Some entity) world
                    if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                        if not (entity.GetAbsolute world) then
                            if entity.GetIs2d world then
                                DesiredEye2dCenter <- (entity.GetPerimeterCenter world).V2
                            else
                                let eyeCenterOffset = (v3Back * NewEntityDistance).Transform world.Eye3dRotation
                                DesiredEye3dCenter <- entity.GetPosition world + eyeCenterOffset
                    let propagationSourceItemTitle = "##propagationSourceItem" + scstring entity
                    if ImGui.BeginPopupContextItem propagationSourceItemTitle then
                        if ImGui.MenuItem "Propagate Entity" then
                            propagateEntityStructure entity world
                        if ImGui.MenuItem "Wipe Propagated Descriptor" then
                            snapshot WipePropagationTargets world
                            World.clearPropagationTargets entity world
                            entity.SetPropagatedDescriptorOpt None world
                        if ImGui.MenuItem "Show in Hierarchy" then
                            ShowSelectedEntity <- true
                            ShowEntityContextMenu <- false
                        ImGui.EndPopup ()
                    ImGui.SameLine ()
                    ImGui.PushID ("##create" + scstringMemo entity)
                    if ImGui.SmallButton "Create" then
                        snapshot DuplicateEntity world
                        let parent = NewEntityParentOpt |> Option.map cast<Simulant> |> Option.defaultValue entity.Group
                        let positionSnapEir = if Snaps2dSelected then Left (a__ Snaps2d) else Right (a__ Snaps3d)
                        let duplicate = World.pasteEntity NewEntityDistance RightClickPosition positionSnapEir PasteAtLook entity parent world
                        selectEntityOpt (Some duplicate) world
                    if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                        ImGui.Text "Create a copy of the specified entity."
                        ImGui.EndTooltip ()
                    ImGui.PopID ()
                    match SelectedEntityOpt with
                    | Some selectedEntity when not (propagationSources.Contains selectedEntity) ->
                        ImGui.SameLine ()
                        ImGui.PushID ("##asChild" + scstringMemo entity)
                        if ImGui.SmallButton "as Child" then
                            snapshot DuplicateEntity world
                            let parent =
                                SelectedEntityOpt
                                |> Option.map cast<Simulant>
                                |> Option.orElse (Option.map cast<Simulant> NewEntityParentOpt)
                                |> Option.defaultValue entity.Group
                            let positionSnapEir = if Snaps2dSelected then Left (a__ Snaps2d) else Right (a__ Snaps3d)
                            let duplicate = World.pasteEntity NewEntityDistance RightClickPosition positionSnapEir PasteAtLook entity parent world
                            selectEntityOpt (Some duplicate) world
                        ImGui.PopID ()
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            ImGui.Text "Create a copy of the specified entity as a child of the selected entity."
                            ImGui.EndTooltip ()
                        ImGui.SameLine ()
                        ImGui.PushID ("##atLocalOrigin" + scstringMemo entity)
                        if ImGui.SmallButton "at Local Origin" then
                            snapshot DuplicateEntity world
                            let parent =
                                SelectedEntityOpt
                                |> Option.map cast<Simulant>
                                |> Option.orElse (Option.map cast<Simulant> NewEntityParentOpt)
                                |> Option.defaultValue entity.Group
                            let positionSnapEir = if Snaps2dSelected then Left (a__ Snaps2d) else Right (a__ Snaps3d)
                            let duplicate = World.pasteEntity NewEntityDistance RightClickPosition positionSnapEir PasteAtLook entity parent world
                            duplicate.SetPositionLocal v3Zero world
                            selectEntityOpt (Some duplicate) world
                        ImGui.PopID ()
                        if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                            ImGui.Text "Create as copy of the specified entity as a child of the selected entity at local origin."
                            ImGui.EndTooltip ()
                    | Some _ | None -> ()
                    ImGui.TreePop ()
            ImGui.EndChild ()
        ImGui.End ()

    let private imGuiOverlayerWindow () =
        let windowName = "Edit Overlayer"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            if ImGui.Button "Save" then
                let overlayerSourceDir = TargetDir + "/../../.."
                let overlayerFilePath = overlayerSourceDir + "/" + Assets.Global.AssetGraphFilePath
                try let overlays = scvalue<Overlay list> OverlayerStr
                    let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                    File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
                with exn -> MessageBoxOpt <- Some ("Could not save asset graph due to: " + scstring exn)
            ImGui.SameLine ()
            if ImGui.Button "Load" then
                let overlayerFilePath = TargetDir + "/" + Assets.Global.OverlayerFilePath
                let overlayer = Overlayer.makeFromFileOpt [] overlayerFilePath
                let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                OverlayerStr <- PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
            ImGui.InputTextMultiline ("##overlayerStr", &OverlayerStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
        ImGui.End ()

    let private imGuiAssetGraphWindow () =
        let windowName = "Edit Asset Graph"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            if ImGui.Button "Save" then
                let assetSourceDir = TargetDir + "/../../.."
                let assetGraphFilePath = assetSourceDir + "/" + Assets.Global.AssetGraphFilePath
                try let packageDescriptorsStr = AssetGraphStr |> scvalue<Map<string, PackageDescriptor>> |> scstring
                    let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                    File.WriteAllText (assetGraphFilePath, PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter)
                with exn -> MessageBoxOpt <- Some ("Could not save asset graph due to: " + scstring exn)
            ImGui.SameLine ()
            if ImGui.Button "Load" then
                let assetGraph = AssetGraph.makeFromFileOpt (TargetDir + "/" + Assets.Global.AssetGraphFilePath)
                let packageDescriptorsStr = scstring assetGraph.PackageDescriptors
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                AssetGraphStr <- PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
            ImGui.InputTextMultiline ("##assetGraphStr", &AssetGraphStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
        ImGui.End ()

    let private imGuiEditPropertyWindow world =
        if PropertyEditorFocusRequested then
            ImGui.SetNextWindowFocus ()
            PropertyEditorFocusRequested <- false
        let windowName = "Edit Property"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            match PropertyFocusedOpt with
            | Some (propertyDescriptor, simulant) when
                World.getExists simulant world &&
                containsProperty propertyDescriptor.PropertyName simulant world &&
                propertyDescriptor.PropertyType <> typeof<ComputedProperty> ->
                ToSymbolMemo.Evict Constants.Gaia.PropertyValueStrMemoEvictionAge
                OfSymbolMemo.Evict Constants.Gaia.PropertyValueStrMemoEvictionAge
                let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType, ToSymbolMemo, OfSymbolMemo)
                let propertyValueUntruncated = getPropertyValue propertyDescriptor simulant world
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
                try let propertyValueSymbol = converter.ConvertTo (propertyValue, typeof<Symbol>) :?> Symbol
                    let mutable propertyValueStr = PrettyPrinter.prettyPrintSymbol propertyValueSymbol PrettyPrinter.defaultPrinter
                    let isPropertyAssetTag = propertyDescriptor.PropertyType.IsGenericType && propertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>
                    if  isPropertyAssetTag then
                        ImGui.SameLine ()
                        if ImGui.Button "Pick" then searchAssetViewer ()
                    if  propertyDescriptor.PropertyName = Constants.Engine.FacetNamesPropertyName &&
                        propertyDescriptor.PropertyType = typeof<string Set> then
                        ImGui.InputTextMultiline ("##propertyValueStr", &propertyValueStr, 4096u, v2 -1.0f -1.0f, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                    elif ImGui.InputTextMultiline ("##propertyValueStr", &propertyValueStr, 131072u, v2 -1.0f -1.0f) then
                        let pasts = Pasts
                        try let propertyValueEscaped = propertyValueStr
                            let propertyValueUnescaped = String.unescape propertyValueEscaped
                            let propertyValueTruncated = converter.ConvertFromString propertyValueUnescaped
                            let propertyValue =
                                if propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                                    match World.tryUntruncateModel propertyValueTruncated simulant world with
                                    | Some truncatedValue -> truncatedValue
                                    | None -> propertyValueTruncated
                                else propertyValueTruncated
                            setPropertyValue false propertyValue propertyDescriptor simulant world
                        with _ ->
                            Pasts <- pasts
                    if isPropertyAssetTag && ImGui.BeginDragDropTarget () then
                        if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                            match DragDropPayloadOpt with
                            | Some payload ->
                                let pasts = Pasts
                                try let propertyValueEscaped = payload
                                    let propertyValueUnescaped = String.unescape propertyValueEscaped
                                    let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                    setPropertyValue false propertyValue propertyDescriptor simulant world
                                with _ ->
                                    Pasts <- pasts
                            | None -> ()
                        ImGui.EndDragDropTarget ()
                with :? TargetException as exn ->
                    PropertyFocusedOpt <- None
                    Log.warn ("Encountered undesired exception due to likely logic problem in Nu: " + scstring exn)
            | Some _ | None -> ()
        ImGui.End ()

    let private imGuiMetricsWindow (world : World) =

        // metrics window
        let windowName = "Metrics"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
                
            // store focus
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName

            // fps
            ImGui.Text "Fps:"
            ImGui.SameLine ()
            let currentDateTime = DateTimeOffset.Now
            let elapsedDateTime = currentDateTime - FpsStartDateTime
            if elapsedDateTime.TotalSeconds >= 5.0 then
                FpsStartUpdateTime <- world.UpdateTime
                FpsStartDateTime <- currentDateTime
            let elapsedDateTime = currentDateTime - FpsStartDateTime
            let time = double (world.UpdateTime - FpsStartUpdateTime)
            let frames = time / elapsedDateTime.TotalSeconds
            ImGui.Text (if not (Double.IsNaN frames) then String.Format ("{0:f2}", frames) else "0.00")

            // draw call count
            ImGui.Text "Draw Call Count:"
            ImGui.SameLine ()
            ImGui.Text (string (OpenGL.Hl.GetDrawCallCount ()))

            // draw instance count
            ImGui.Text "Draw Instance Count:"
            ImGui.SameLine ()
            ImGui.Text (string (OpenGL.Hl.GetDrawInstanceCount ()))

            // frame timing plot
            GcTimings.Enqueue (single world.Timers.GcFrameTime.TotalMilliseconds)
            GcTimings.Dequeue () |> ignore<single>
            MiscTimings.Enqueue
                (single
                    (world.Timers.InputTimer.Elapsed.TotalMilliseconds +
                     world.Timers.AudioTimer.Elapsed.TotalMilliseconds))
            MiscTimings.Dequeue () |> ignore<single>
            PhysicsTimings.Enqueue (single world.Timers.PhysicsTimer.Elapsed.TotalMilliseconds + Seq.last MiscTimings)
            PhysicsTimings.Dequeue () |> ignore<single>
            UpdateTimings.Enqueue
                (single
                    (world.Timers.PreProcessTimer.Elapsed.TotalMilliseconds +
                     world.Timers.PreUpdateTimer.Elapsed.TotalMilliseconds +
                     world.Timers.UpdateTimer.Elapsed.TotalMilliseconds +
                     world.Timers.PostUpdateTimer.Elapsed.TotalMilliseconds +
                     world.Timers.PerProcessTimer.Elapsed.TotalMilliseconds +
                     world.Timers.CoroutinesTimer.Elapsed.TotalMilliseconds +
                     world.Timers.TaskletsTimer.Elapsed.TotalMilliseconds +
                     world.Timers.DestructionTimer.Elapsed.TotalMilliseconds +
                     world.Timers.PostProcessTimer.Elapsed.TotalMilliseconds) + Seq.last PhysicsTimings)
            UpdateTimings.Dequeue () |> ignore<single>
            RenderMessagesTimings.Enqueue (single world.Timers.RenderMessagesTimer.Elapsed.TotalMilliseconds + Seq.last UpdateTimings)
            RenderMessagesTimings.Dequeue () |> ignore<single>
            ImGuiTimings.Enqueue (single world.Timers.ImGuiTimer.Elapsed.TotalMilliseconds + Seq.last RenderMessagesTimings)
            ImGuiTimings.Dequeue () |> ignore<single>
            MainThreadTimings.Enqueue (single world.Timers.MainThreadTime.TotalMilliseconds)
            MainThreadTimings.Dequeue () |> ignore<single>
            FrameTimings.Enqueue (single world.Timers.FrameTimer.Elapsed.TotalMilliseconds)
            FrameTimings.Dequeue () |> ignore<single>
            if ImPlot.BeginPlot ("FrameTimings", v2 -1.0f -1.0f, ImPlotFlags.NoTitle ||| ImPlotFlags.NoInputs) then
                ImPlot.SetupLegend (ImPlotLocation.West, ImPlotLegendFlags.Outside)
                ImPlot.SetupAxesLimits (0.0, double (dec TimingsArray.Length), 0.0, 40.0)
                ImPlot.SetupAxes ("Frame", "Time (ms)", ImPlotAxisFlags.NoLabel ||| ImPlotAxisFlags.NoTickLabels, ImPlotAxisFlags.None)
                FrameTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Frame Time", &TimingsArray.[0], TimingsArray.Length)
                MainThreadTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Main Thread", &TimingsArray.[0], TimingsArray.Length)
                ImGuiTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("ImGui Time", &TimingsArray.[0], TimingsArray.Length)
                RenderMessagesTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Render Msgs", &TimingsArray.[0], TimingsArray.Length)
                UpdateTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Update Time", &TimingsArray.[0], TimingsArray.Length)
                PhysicsTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Physics Time", &TimingsArray.[0], TimingsArray.Length)
                MiscTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Misc Time", &TimingsArray.[0], TimingsArray.Length)
                GcTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotShaded ("GC Time", &TimingsArray.[0], TimingsArray.Length)
                ImPlot.EndPlot ()

        // fin
        ImGui.End ()

    let private imGuiInteractiveWindow world =
        let windowName = "Interactive"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            let mutable toBottom = false
            let eval = ImGui.Button "Eval" || ImGui.IsAnyItemActive () && ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlDown () && ImGui.IsShiftUp ()
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Evaluate current expression (Ctrl+Enter)"
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            let enter = ImGui.Button "Enter" || ImGui.IsAnyItemActive () && ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlDown () && ImGui.IsShiftDown ()
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Evaluate current expression, then clear input (Ctrl+Shift+Enter)"
                ImGui.EndTooltip ()
            if eval || enter then
                snapshot (Evaluate InteractiveInputStr) world
                let initialEval = InteractiveNeedsInitialization
                if InteractiveNeedsInitialization then

                    // run initialization script
                    let projectDllPathValid = File.Exists ProjectDllPath
                    let initial =
                        "#r \"System.Configuration.ConfigurationManager.dll\"\n" +
                        "#r \"System.Drawing.Common.dll\"\n" +
                        "#r \"FSharp.Core.dll\"\n" +
                        "#r \"FSharp.Compiler.Service.dll\"\n" +
                        "#r \"Aether.Physics2D.dll\"\n" +
                        "#r \"JoltPhysicsSharp.dll\"\n" +
                        "#r \"AssimpNet.dll\"\n" +
                        "#r \"BulletSharp.dll\"\n" +
                        "#r \"Csv.dll\"\n" +
                        "#r \"DotRecast.Recast.Toolset.dll\"\n" +
                        "#r \"FParsec.dll\"\n" +
                        "#r \"Magick.NET-Q8-AnyCPU.dll\"\n" +
                        "#r \"OpenGL.Net.dll\"\n" +
                        "#r \"Pfim.dll\"\n" +
                        "#r \"SDL2-CS.dll\"\n" +
                        "#r \"TiledSharp.dll\"\n" +
                        "#r \"Twizzle.ImGui-Bundle.NET.dll\"\n" +
                        "#r \"Prime.dll\"\n" +
                        "#r \"Nu.Math.dll\"\n" +
                        "#r \"Nu.dll\"\n" +
                        "#r \"Nu.Gaia.dll\"\n" +
                        (if projectDllPathValid then "#r \"" + PathF.GetFileName ProjectDllPath + "\"\n" else "") +
                        "open System\n" +
                        "open System.Numerics\n" +
                        "open Prime\n" +
                        "open Nu\n" +
                        "open Nu.Gaia"
                    match FsiSession.EvalInteractionNonThrowing initial with
                    | (Choice1Of2 _, _) -> ()
                    | (Choice2Of2 exn, _) -> Log.error ("Could not initialize fsi eval due to: " + scstring exn)

                    // attempt to open template namespace "MyGame", as well as namespace derived from project name
                    if projectDllPathValid then
                        let errorStr = string FsiErrorStream // preserve any existing error
                        let namespaceName = PathF.GetFileNameWithoutExtension (ProjectDllPath.Replace (" ", ""))
                        FsiSession.EvalInteractionNonThrowing ("open " + "MyGame") |> ignore<Choice<_, _> * _>
                        FsiSession.EvalInteractionNonThrowing ("open " + namespaceName) |> ignore<Choice<_, _> * _>
                        FsiErrorStream.GetStringBuilder().Clear() |> ignore<StringBuilder> // ignore any open directive errors
                        FsiErrorStream.Write errorStr // restore previous error string

                    // eval initialization finished
                    InteractiveNeedsInitialization <- false

                // attempt to run interactive input
                if InteractiveInputStr.Contains (nameof TargetDir) then FsiSession.AddBoundValue (nameof TargetDir, TargetDir)
                if InteractiveInputStr.Contains (nameof ProjectDllPath) then FsiSession.AddBoundValue (nameof ProjectDllPath, ProjectDllPath)
                if InteractiveInputStr.Contains (nameof SelectedScreen) then FsiSession.AddBoundValue (nameof SelectedScreen, SelectedScreen)
                if InteractiveInputStr.Contains (nameof SelectedGroup) then FsiSession.AddBoundValue (nameof SelectedGroup, SelectedGroup)
                if InteractiveInputStr.Contains (nameof SelectedEntityOpt) then
                    if SelectedEntityOpt.IsNone // HACK: 1/2: workaround for binding a null value with AddBoundValue.
                    then FsiSession.EvalInteractionNonThrowing "let selectedEntityOpt = Option<Entity>.None;;" |> ignore<Choice<_, _> * _>
                    else FsiSession.AddBoundValue (nameof SelectedEntityOpt, SelectedEntityOpt)
                if InteractiveInputStr.Contains (nameof world) then FsiSession.AddBoundValue (nameof world, world)
                if File.Exists Constants.Gaia.InteractiveInputFilePath then
                    File.SetAttributes (Constants.Gaia.InteractiveInputFilePath, FileAttributes.None)
                    File.WriteAllText (Constants.Gaia.InteractiveInputFilePath, InteractiveInputStr)
                    File.SetAttributes (Constants.Gaia.InteractiveInputFilePath, FileAttributes.ReadOnly)
                match FsiSession.EvalInteractionNonThrowing (InteractiveInputStr + ";;", Constants.Gaia.InteractiveInputFilePath) with
                | (Choice1Of2 _, _) ->
                    let errorStr = string FsiErrorStream
                    let outStr = string FsiOutStream
                    let outStr =
                        if initialEval then
                            let outStr = outStr.Replace ("\r\n> ", "") // TODO: ensure the use of \r\n also works on linux.
                            let outStrLines = outStr.Split "\r\n"
                            let outStrLines = Array.filter (fun (line : string) -> not (line.Contains "--> Referenced '")) outStrLines
                            String.join "\r\n" outStrLines
                        else outStr
                    let outStr =
                        if SelectedEntityOpt.IsNone // HACK: 2/2: strip eval output relating to above 1/2 hack.
                        then outStr.Replace ("val selectedEntityOpt: Entity option = None\r\n", "")
                        else outStr
                    if errorStr.Length > 0
                    then InteractiveOutputStr <- InteractiveOutputStr + errorStr
                    else InteractiveOutputStr <- InteractiveOutputStr + Environment.NewLine + outStr
                | (Choice2Of2 _, diags) ->
                    let diagsStr = diags |> Array.map _.Message |> String.join Environment.NewLine
                    InteractiveOutputStr <- InteractiveOutputStr + Environment.NewLine + diagsStr
                InteractiveOutputStr <-
                    InteractiveOutputStr.Split Environment.NewLine
                    |> Array.filter (not << String.IsNullOrWhiteSpace)
                    |> String.join Environment.NewLine
                FsiErrorStream.GetStringBuilder().Clear() |> ignore<StringBuilder>
                FsiOutStream.GetStringBuilder().Clear() |> ignore<StringBuilder>
                toBottom <- true

            ImGui.SameLine ()
            if ImGui.Button "Clear" || ImGui.IsKeyReleased ImGuiKey.C && ImGui.IsAltDown () then InteractiveOutputStr <- ""
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Clear evaluation output (Alt+C)"
                ImGui.EndTooltip ()
            if InteractiveInputFocusRequested then ImGui.SetKeyboardFocusHere (); InteractiveInputFocusRequested <- false
            ImGui.InputTextMultiline ("##interactiveInputStr", &InteractiveInputStr, 131072u, v2 -1.0f 130.0f, if eval then ImGuiInputTextFlags.ReadOnly else ImGuiInputTextFlags.None) |> ignore<bool>
            if enter then InteractiveInputStr <- ""
            if eval || enter then InteractiveInputFocusRequested <- true
            ImGui.Separator ()
            ImGui.BeginChild ("##interactiveOutputStr", v2Zero, ImGuiChildFlags.None, ImGuiWindowFlags.AlwaysHorizontalScrollbar ||| ImGuiWindowFlags.AlwaysVerticalScrollbar) |> ignore<bool>
            ImGui.TextUnformatted InteractiveOutputStr
            if toBottom then ImGui.SetScrollHereY 1.0f
            ImGui.EndChild ()
        ImGui.End ()

    let private imGuiEventTracingWindow world =
        let windowName = "Event Tracing"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            let mutable traceEvents = world |> World.getEventTracerOpt |> Option.isSome
            if ImGui.Checkbox ("Trace Events", &traceEvents) then
                World.setEventTracerOpt (if traceEvents then Some (Log.custom "Event") else None) world
            let eventFilter = World.getEventFilter world
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<EventFilter>).PrettyPrinter
            let mutable eventFilterStr = PrettyPrinter.prettyPrint (scstring eventFilter) prettyPrinter
            if ImGui.InputTextMultiline ("##eventFilterStr", &eventFilterStr, 131072u, v2 -1.0f -1.0f) then
                try let eventFilter = scvalue<EventFilter> eventFilterStr
                    World.setEventFilter eventFilter world
                with _ -> ()
        ImGui.End ()

    let private imGuiAudioPlayerWindow world =
        let windowName = "Audio Player"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            ImGui.Text "Master Sound Volume"
            let mutable masterSoundVolume = World.getMasterSoundVolume world
            if ImGui.SliderFloat ("##masterSoundVolume", &masterSoundVolume, 0.0f, 1.0f) then World.setMasterSoundVolume masterSoundVolume world
            ImGui.SameLine ()
            ImGui.Text (string masterSoundVolume)
            ImGui.Text "Master Song Volume"
            let mutable masterSongVolume = World.getMasterSongVolume world
            if ImGui.SliderFloat ("##masterSongVolume", &masterSongVolume, 0.0f, 1.0f) then World.setMasterSongVolume masterSongVolume world
            ImGui.SameLine ()
            ImGui.Text (string masterSongVolume)
        ImGui.End ()

    let private imGuiRendererWindow world =
        let windowName = "Renderer"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            let renderer3dConfig = World.getRenderer3dConfig world
            let mutable renderer3dEdited = false
            let mutable lightMappingEnabled = renderer3dConfig.LightMappingEnabled
            let mutable lightShadowingEnabled = renderer3dConfig.LightShadowingEnabled
            let mutable sssEnabled = renderer3dConfig.SssEnabled
            let mutable ssaoEnabled = renderer3dConfig.SsaoEnabled
            let mutable ssaoSampleCount = renderer3dConfig.SsaoSampleCount
            let mutable ssvfEnabled = renderer3dConfig.SsvfEnabled
            let mutable ssrlEnabled = renderer3dConfig.SsrlEnabled
            let mutable ssrrEnabled = renderer3dConfig.SsrrEnabled
            let mutable bloomEnabled = renderer3dConfig.BloomEnabled
            let mutable fxaaEnabled = renderer3dConfig.FxaaEnabled
            renderer3dEdited <- ImGui.Checkbox ("Light Mapping Enabled", &lightMappingEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Light Shadowing Enabled", &lightShadowingEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Sss Enabled", &sssEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Ssao Enabled", &ssaoEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.SliderInt ("Ssao Sample Count", &ssaoSampleCount, 0, Constants.Render.SsaoSampleCountMax) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Ssvf Enabled", &ssvfEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Ssrl Enabled", &ssrlEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Ssrr Enabled", &ssrrEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Bloom Enabled", &bloomEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Fxaa Enabled", &fxaaEnabled) || renderer3dEdited
            if renderer3dEdited then
                let renderer3dConfig =
                    { LightMappingEnabled = lightMappingEnabled
                      LightShadowingEnabled = lightShadowingEnabled
                      SssEnabled = sssEnabled
                      SsaoEnabled = ssaoEnabled
                      SsaoSampleCount = ssaoSampleCount
                      SsvfEnabled = ssvfEnabled
                      SsrlEnabled = ssrlEnabled
                      SsrrEnabled = ssrrEnabled
                      BloomEnabled = bloomEnabled
                      FxaaEnabled = fxaaEnabled }
                World.enqueueRenderMessage3d (ConfigureRenderer3d renderer3dConfig) world
        ImGui.End ()

    let private imGuiLogWindow (world : World) =
        let lines = LogStr.Split '\n'
        let warnings = lines |> Seq.filter (fun line -> line.Contains "|Warning|") |> Seq.length
        let errors = lines |> Seq.filter (fun line -> line.Contains "|Error|") |> Seq.length
        let flag = warnings > 0 || errors > 0
        let flash = flag && world.DateTime.Millisecond / 400 % 2 = 0
        if flash then
            let flashColor =
                if errors > 0 then let red = Color.Red in red.Abgr
                elif warnings > 0 then let yellow = Color.Yellow in yellow.Abgr
                else failwithumf ()
            ImGui.PushStyleColor (ImGuiCol.TitleBg, flashColor)
            ImGui.PushStyleColor (ImGuiCol.TitleBgActive, flashColor)
            ImGui.PushStyleColor (ImGuiCol.TitleBgCollapsed, flashColor)
            ImGui.PushStyleColor (ImGuiCol.Tab, flashColor)
            ImGui.PushStyleColor (ImGuiCol.TabHovered, flashColor)
            ImGui.PushStyleColor (ImGuiCol.TabSelected, flashColor)
            ImGui.PushStyleColor (ImGuiCol.TabDimmed, flashColor)
            ImGui.PushStyleColor (ImGuiCol.TabDimmedSelected, flashColor)
        let windowName = "Log"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            ImGui.Text "Log:"
            ImGui.SameLine ()
            if ImGui.SmallButton "Clear" then LogStr <- ""
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Clear evaluation output (Alt+C)"
                ImGui.EndTooltip ()
            ImGui.BeginChild ("##outputBufferStr", v2Zero, ImGuiChildFlags.None, ImGuiWindowFlags.AlwaysHorizontalScrollbar ||| ImGuiWindowFlags.AlwaysVerticalScrollbar) |> ignore<bool>
            ImGui.TextUnformatted LogStr
            ImGui.EndChild ()
        if flash then for i in 0 .. dec 8 do ImGui.PopStyleColor ()
        ImGui.End ()

    let private imGuiEditorConfigWindow () =
        let windowName = "Editor"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            ImGui.Text "Transform Snapping"
            ImGui.SetNextItemWidth 50.0f
            let mutable index = if Snaps2dSelected then 0 else 1
            if ImGui.Combo ("##snapsSelection", &index, [|"2d"; "3d"|], 2) then
                match index with
                | 0 -> Snaps2dSelected <- true
                | _ -> Snaps2dSelected <- false
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Use 2d or 3d snapping (F3 to swap mode)."
                ImGui.EndTooltip ()
            ImGui.SameLine ()
            let mutable (p, d, s) = if Snaps2dSelected then Snaps2d else Snaps3d
            ImGui.Text "Pos"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 50.0f
            ImGui.DragFloat ("##p", &p, (if Snaps2dSelected then 0.1f else 0.01f), 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "Deg"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 50.0f
            if Snaps2dSelected
            then ImGui.DragFloat ("##d", &d, 0.1f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
            else ImGui.DragFloat ("##d", &d, 0.0f, 0.0f, 0.0f, "%2.2f") |> ignore<bool> // unchangable 3d rotation
            ImGui.SameLine ()
            ImGui.Text "Scl"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 50.0f
            ImGui.DragFloat ("##s", &s, 0.01f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
            if Snaps2dSelected then Snaps2d <- (p, d, s) else Snaps3d <- (p, d, s)
            ImGui.Text "Creation Elevation (2d)"
            ImGui.DragFloat ("##newEntityElevation", &NewEntityElevation, SnapDrag, Single.MinValue, Single.MaxValue, "%2.2f") |> ignore<bool>
            ImGui.Text "Creation Distance (3d)"
            ImGui.DragFloat ("##newEntityDistance", &NewEntityDistance, SnapDrag, 0.5f, Single.MaxValue, "%2.2f") |> ignore<bool>
            ImGui.Text "Input"
            ImGui.Checkbox ("Alternative Eye Travel Input", &AlternativeEyeTravelInput) |> ignore<bool>
            ImGui.Text "Debug"
            ImGui.Checkbox ("Physics Debug Rendering (2d)", &PhysicsDebugRendering2d) |> ignore<bool>
            ImGui.Checkbox ("Physics Debug Rendering (3d)", &PhysicsDebugRendering3d) |> ignore<bool>
            ImGui.Checkbox ("ImGui Debug Window", &ImGuiDebugWindow) |> ignore<bool>
        ImGui.End ()

    let private imGuiAssetViewerWindow world =
        let windowName = "Asset Viewer"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNav) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            ImGui.SetNextItemWidth -1.0f
            let searchActivePrevious = not (String.IsNullOrWhiteSpace AssetViewerSearchStr)
            if AssetViewerSearchRequested then
                ImGui.SetKeyboardFocusHere ()
                AssetViewerSearchStr <- ""
                AssetViewerSearchRequested <- false
            ImGui.InputTextWithHint ("##assetViewerSearchStr", "[enter search text]", &AssetViewerSearchStr, 4096u) |> ignore<bool>
            let searchActiveCurrent = not (String.IsNullOrWhiteSpace AssetViewerSearchStr)
            let searchDeactivated = searchActivePrevious && not searchActiveCurrent
            ImGui.BeginChild "Container" |> ignore<bool>
            for packageEntry in Metadata.getMetadataPackagesLoaded () |> Array.sortWith (fun a b -> String.Compare (a.Key, b.Key, true)) do
                let flags = ImGuiTreeNodeFlags.SpanAvailWidth ||| ImGuiTreeNodeFlags.OpenOnArrow
                if searchActiveCurrent then ImGui.SetNextItemOpen true
                if searchDeactivated then ImGui.SetNextItemOpen false
                if ImGui.TreeNodeEx (packageEntry.Key, flags) then
                    for assetEntry in packageEntry.Value |> Array.sortWith (fun a b -> String.Compare (a.Key, b.Key, true)) do
                        let assetName = assetEntry.Key
                        if (assetName.ToLowerInvariant ()).Contains (AssetViewerSearchStr.ToLowerInvariant ()) then
                            let assetImageSize = v2Dup (ImGui.GetFontSize () + 3.0f)
                            match World.imGuiTryGetTextureId (asset packageEntry.Key assetName) world with
                            | ValueSome textureId ->
                                ImGui.Image (nativeint textureId, assetImageSize)
                                if ImGui.IsItemHovered ImGuiHoveredFlags.DelayShort then
                                    let zoom = ImGui.IsShiftDown ()
                                    let size = if zoom then v2Dup 256.0f else v2Dup 128.0f
                                    let offset = if zoom then v2 -288.0f -128.0f else v2 -160.0f -64.0f
                                    let tooltipPosition = ImGui.GetMousePos () + offset
                                    ImGui.SetNextWindowPos tooltipPosition
                                    if ImGui.BeginTooltip () then
                                        ImGui.Image (nativeint textureId, size)
                                        ImGui.EndTooltip ()
                            | ValueNone -> ImGui.Dummy assetImageSize
                            ImGui.SameLine ()
                            ImGui.TreeNodeEx (assetName, flags ||| ImGuiTreeNodeFlags.Leaf) |> ignore<bool>
                            if ImGui.BeginDragDropSource () then // NOTE: it appears that drag-dropping only works from nodes in Dear ImGui.
                                let packageNameText = if Symbol.shouldBeExplicit packageEntry.Key then String.surround "\"" packageEntry.Key else packageEntry.Key
                                let assetNameText = if Symbol.shouldBeExplicit assetName then String.surround "\"" assetName else assetName
                                let assetTagStr = "[" + packageNameText + " " + assetNameText + "]"
                                DragDropPayloadOpt <- Some assetTagStr
                                ImGui.Text assetTagStr
                                ImGui.SetDragDropPayload ("Asset", IntPtr.Zero, 0u) |> ignore<bool>
                                ImGui.EndDragDropSource ()
                            ImGui.TreePop ()
                    ImGui.TreePop ()
            ImGui.EndChild ()
        ImGui.End ()

    let private imGuiDebugWindow () =
        if ImGuiDebugWindow then
            ImGui.ShowDebugLogWindow ()

    let private imGuiNewProjectDialog world =

        // prompt user to create new project
        let programDir = PathF.GetDirectoryName (Assembly.GetEntryAssembly().Location)
        let title = "Create Nu Project... *EDITOR RESTART REQUIRED!*"
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        ImGui.SetNextWindowSize (v2 900.0f 0.0f) // HACK: this is needed since auto-resizing windows don't work with ImGui.TextWrapped (https://github.com/ocornut/imgui/issues/778)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, &ShowNewProjectDialog) then
            ImGui.Text "Project Name"
            ImGui.SameLine ()
            ImGui.InputText ("##newProjectName", &NewProjectName, 4096u) |> ignore<bool>
            NewProjectName <- NewProjectName.Replace("\t", "").Replace(".", "")
            ImGui.Text "Project Type"
            ImGui.SameLine ()
            if ImGui.BeginCombo ("##newProjectType", NewProjectType) then
                for projectType in ["ImSim Game"; "ImSim Empty"; "MMCC Game"; "MMCC Empty"] do
                    if ImGui.Selectable projectType then
                        NewProjectType <- projectType
                ImGui.EndCombo ()
            let projectTypeDescription =
                match NewProjectType with
                | "ImSim Game" -> "Create a full ImSim game project. This contains the structures and pieces that embody the best practices of ImSim usage."
                | "ImSim Empty" -> "Create an empty ImSim project. This contains the minimum code needed to initially learn or experiment with the ImSim API."
                | "MMCC Game" -> "Create a full MMCC game project. This contains the structures and pieces that embody the best practices of MMCC usage."
                | "MMCC Empty" -> "Create an empty MMCC project. This contains the minimum code needed to initially learn or experiment with the MMCC API."
                | _ -> failwithumf ()
            ImGui.Separator ()
            ImGui.TextWrapped ("Description: " + projectTypeDescription)
            ImGui.Separator ()
            let projectsDir = PathF.GetFullPath (programDir + "/../../../../../Projects")
            let newProjectDir = PathF.GetFullPath (projectsDir + "/" + NewProjectName)
            let newProjectDllPath = newProjectDir + "/bin/" + Constants.Gaia.BuildName + "/net9.0/" + NewProjectName + ".dll"
            let newFileName = NewProjectName + ".fsproj"
            let newProject = PathF.GetFullPath (newProjectDir + "/" + newFileName)
            let validName = not (String.IsNullOrWhiteSpace NewProjectName) && Array.notExists (fun char -> NewProjectName.Contains (string char)) (PathF.GetInvalidPathChars ())
            let validDirectory = not (Directory.Exists newProjectDir)
            if not validName then
                ImGui.Text "Invalid project name!"
            elif not validDirectory then
                ImGui.Text "Project already exists!"
            elif ImGui.Button "Create" || ImGui.IsKeyReleased ImGuiKey.Enter then

                // choose a template, ensuring it exists
                let slnDir = PathF.GetFullPath (programDir + "/../../../../..")
                let (templateFileName, templateDir, editMode, shortName) =
                    match NewProjectType with
                    | "MMCC Empty" -> ("Nu.Template.Mmcc.Empty.fsproj", PathF.GetFullPath (programDir + "/../../../../Nu.Template.Mmcc.Empty"), "Initial", "nu-template-mmcc-empty")
                    | "MMCC Game" -> ("Nu.Template.Mmcc.Game.fsproj", PathF.GetFullPath (programDir + "/../../../../Nu.Template.Mmcc.Game"), "Title", "nu-template-mmcc-game")
                    | "ImSim Empty" -> ("Nu.Template.ImSim.Empty.fsproj", PathF.GetFullPath (programDir + "/../../../../Nu.Template.ImSim.Empty"), "Initial", "nu-template-imsim-empty")
                    | "ImSim Game" -> ("Nu.Template.ImSim.Game.fsproj", PathF.GetFullPath (programDir + "/../../../../Nu.Template.ImSim.Game"), "Title", "nu-template-imsim-game")
                    | _ -> failwithumf ()
                if Directory.Exists templateDir then

                    // attempt to create project files
                    try Log.info ("Creating project '" + NewProjectName + "' in '" + projectsDir + "'...")

                        // install nu template
                        Directory.SetCurrentDirectory templateDir
                        Process.Start("dotnet", "new install ./ --force").WaitForExit()

                        // instantiate nu template
                        Directory.SetCurrentDirectory projectsDir
                        Directory.CreateDirectory NewProjectName |> ignore<DirectoryInfo>
                        Directory.SetCurrentDirectory newProjectDir
                        Process.Start("dotnet", "new " + shortName + " --force").WaitForExit()

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
                            ["Project(\"{6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705}\") = \"" + NewProjectName + "\", \"Projects\\" + NewProjectName + "\\" + NewProjectName + ".fsproj\", \"{" + projectGuidStr + "}\""
                             "EndProject"] @
                            List.skip insertionIndex slnLines
                        let insertionIndex = List.findIndex ((=) "\tGlobalSection(SolutionProperties) = preSolution") slnLines - 1
                        let slnLines =
                            List.take insertionIndex slnLines @
                            ["\t\t{" + projectGuidStr + "}.Debug|Any CPU.ActiveCfg = Debug|Any CPU"
                             "\t\t{" + projectGuidStr + "}.Debug|Any CPU.Build.0 = Debug|Any CPU"
                             "\t\t{" + projectGuidStr + "}.Mixed|Any CPU.ActiveCfg = Debug|Any CPU"
                             "\t\t{" + projectGuidStr + "}.Mixed|Any CPU.Build.0 = Debug|Any CPU"
                             "\t\t{" + projectGuidStr + "}.Release|Any CPU.ActiveCfg = Release|Any CPU"
                             "\t\t{" + projectGuidStr + "}.Release|Any CPU.Build.0 = Release|Any CPU"] @
                            List.skip insertionIndex slnLines
                        let insertionIndex = List.findIndex ((=) "\tGlobalSection(ExtensibilityGlobals) = postSolution") slnLines - 1
                        let slnLines =
                            List.take insertionIndex slnLines @
                            ["\t\t{" + projectGuidStr + "} = {E3C4D6E1-0572-4D80-84A9-8001C21372D3}"] @
                            List.skip insertionIndex slnLines
                        File.WriteAllLines ("Nu.sln", List.toArray slnLines)
                        Log.info ("Project '" + NewProjectName + "'" + " created.")

                        // configure editor to open new project then exit
                        let gaiaState = makeGaiaState newProjectDllPath (Some editMode) true world
                        let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                        let gaiaDirectory = PathF.GetDirectoryName gaiaFilePath
                        try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                            Directory.SetCurrentDirectory gaiaDirectory
                            ShowRestartDialog <- true
                        with _ -> Log.error "Could not save gaia state and open new project."

                        // close dialog
                        ShowNewProjectDialog <- false
                        NewProjectName <- "My Game"

                    // log failure
                    with exn -> Log.error ("Failed to create new project '" + NewProjectName + "' due to: " + scstring exn)

                // template project missing
                else
                    Log.error "Template project is missing; new project cannot be generated."
                    ShowNewProjectDialog <- false

            // escape to cancel
            if ImGui.IsKeyReleased ImGuiKey.Escape then
                ShowNewProjectDialog <- false
                NewProjectName <- "My Game"

            // fin
            ImGui.EndPopup ()

    let private imGuiOpenProjectDialog world =
        let title = "Choose a project .dll... *EDITOR RESTART REQUIRED!*"
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, &ShowOpenProjectDialog, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Game Assembly Path:"
            ImGui.SameLine ()
            ImGui.InputTextWithHint ("##openProjectFilePath", "[enter game .dll path]", &OpenProjectFilePath, 4096u) |> ignore<bool>
            ImGui.SameLine ()
            if ImGui.Button "..." then ShowOpenProjectFileDialog <- true
            ImGui.Text "Edit Mode:"
            ImGui.SameLine ()
            ImGui.InputText ("##openProjectEditMode", &OpenProjectEditMode, 4096u) |> ignore<bool>
            ImGui.Checkbox ("Use Imperative Execution (faster, but no Undo / Redo)", &OpenProjectImperativeExecution) |> ignore<bool>
            if  (ImGui.Button "Open" || ImGui.IsKeyReleased ImGuiKey.Enter) &&
                String.notEmpty OpenProjectFilePath &&
                File.Exists OpenProjectFilePath then
                ShowOpenProjectDialog <- false
                let gaiaState = makeGaiaState OpenProjectFilePath (Some OpenProjectEditMode) true world
                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                let gaiaDirectory = PathF.GetDirectoryName gaiaFilePath
                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                    Directory.SetCurrentDirectory gaiaDirectory
                    ShowRestartDialog <- true
                with _ ->
                    revertOpenProjectState world
                    Log.info "Could not save editor state and open project."
            if ImGui.IsKeyReleased ImGuiKey.Escape then
                revertOpenProjectState world
                ShowOpenProjectDialog <- false
            ImGui.EndPopup ()

    let private imGuiOpenProjectFileDialog () =
        ProjectFileDialogState.Title <- "Choose a game .dll..."
        ProjectFileDialogState.FilePattern <- "*.dll"
        ProjectFileDialogState.FileDialogType <- ImGuiFileDialogType.Open
        if ImGui.FileDialog (&ShowOpenProjectFileDialog, ProjectFileDialogState) then
            OpenProjectFilePath <- ProjectFileDialogState.FilePath

    let private imGuiCloseProjectDialog (world : World) =
        let title = "Close project... *EDITOR RESTART REQUIRED!*"
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, &ShowCloseProjectDialog, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Close the project and use Gaia in its default state?"
            ImGui.Checkbox ("Proceed w/ Imperative Execution (faster, but no Undo / Redo)", &CloseProjectImperativeExecution) |> ignore<bool>
            if ImGui.Button "Okay" || ImGui.IsKeyReleased ImGuiKey.Enter then
                ShowCloseProjectDialog <- false
                let gaiaState = { GaiaState.defaultState with ProjectImperativeExecution = CloseProjectImperativeExecution }
                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                let gaiaDirectory = PathF.GetDirectoryName gaiaFilePath
                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                    Directory.SetCurrentDirectory gaiaDirectory
                    ShowRestartDialog <- true
                with _ ->
                    CloseProjectImperativeExecution <- world.Imperative
                    Log.info "Could not clear editor state and close project."
            if ImGui.IsKeyReleased ImGuiKey.Escape then
                CloseProjectImperativeExecution <- world.Imperative
                ShowCloseProjectDialog <- false
            ImGui.EndPopup ()

    let private imGuiNewGroupDialog world =
        let title = "Create a group..."
        let opening = not (ImGui.IsPopupOpen title)
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if opening then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, &ShowNewGroupDialog, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Group Name:"
            ImGui.SameLine ()
            if opening then ImGui.SetKeyboardFocusHere ()
            ImGui.InputTextWithHint ("##newGroupName", "[enter group name]", &NewGroupName, 4096u) |> ignore<bool>
            let newGroup = SelectedScreen / NewGroupName
            if ImGui.BeginCombo ("##newGroupDispatcherName", NewGroupDispatcherName, ImGuiComboFlags.HeightRegular) then
                let dispatcherNames = (World.getGroupDispatchers world).Keys
                let dispatcherNamePicked = tryPickName dispatcherNames
                for dispatcherName in dispatcherNames do
                    if ImGui.Selectable (dispatcherName, strEq dispatcherName NewGroupDispatcherName) then NewGroupDispatcherName <- dispatcherName
                    if Some dispatcherName = dispatcherNamePicked then ImGui.SetScrollHereY Constants.Gaia.HeightRegularPickOffset
                    if dispatcherName = NewGroupDispatcherName then ImGui.SetItemDefaultFocus ()
                ImGui.EndCombo ()
            if (ImGui.Button "Create" || ImGui.IsKeyReleased ImGuiKey.Enter) &&
                String.notEmpty NewGroupName &&
                Address.validateIdentifierName NewGroupName &&
                not (NewGroupName.Contains '"') &&
                not (newGroup.GetExists world) then
                let worldStateOld = world.CurrentState
                try snapshot CreateGroup world
                    World.createGroup5 false NewGroupDispatcherName (Some NewGroupName) SelectedScreen world |> ignore<Group>
                    selectEntityOpt None world
                    selectGroup true newGroup
                    ShowNewGroupDialog <- false
                    NewGroupName <- ""
                with exn ->
                    MessageBoxOpt <- Some ("Could not create group due to: " + scstring exn)
                    World.switch worldStateOld world
            if ImGui.IsKeyReleased ImGuiKey.Escape then ShowNewGroupDialog <- false
            ImGui.EndPopup ()

    let private imGuiOpenGroupDialog world =
        GroupFileDialogState.Title <- "Choose a nugroup file..."
        GroupFileDialogState.FilePattern <- "*.nugroup"
        GroupFileDialogState.FileDialogType <- ImGuiFileDialogType.Open
        if ImGui.FileDialog (&ShowOpenGroupDialog, GroupFileDialogState) then
            snapshot OpenGroup world
            let loaded = tryLoadGroup GroupFileDialogState.FilePath world
            ShowOpenGroupDialog <- not loaded

    let private imGuiSaveGroupDialog world =
        GroupFileDialogState.Title <- "Save a nugroup file..."
        GroupFileDialogState.FilePattern <- "*.nugroup"
        GroupFileDialogState.FileDialogType <- ImGuiFileDialogType.Save
        if ImGui.FileDialog (&ShowSaveGroupDialog, GroupFileDialogState) then
            if not (PathF.HasExtension GroupFileDialogState.FilePath) then GroupFileDialogState.FilePath <- GroupFileDialogState.FilePath + ".nugroup"
            let saved = trySaveSelectedGroup GroupFileDialogState.FilePath world
            ShowSaveGroupDialog <- not saved

    let private imGuiRenameGroupDialog world =
        match SelectedGroup with
        | group when group.GetExists world ->
            let title = "Rename group..."
            ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
            let opening = not (ImGui.IsPopupOpen title)
            if opening then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &ShowRenameGroupDialog, ImGuiWindowFlags.AlwaysAutoResize) then
                ImGui.Text "Group Name:"
                ImGui.SameLine ()
                if opening then
                    ImGui.SetKeyboardFocusHere ()
                    GroupRename <- group.Name
                ImGui.InputTextWithHint ("##groupName", "[enter group name]", &GroupRename, 4096u) |> ignore<bool>
                let group' = group.Screen / GroupRename
                if (ImGui.Button "Apply" || ImGui.IsKeyReleased ImGuiKey.Enter) &&
                    String.notEmpty GroupRename &&
                    Address.validateIdentifierName GroupRename &&
                    not (GroupRename.Contains '"') &&
                    not (group'.GetExists world) then
                    snapshot RenameGroup world
                    World.renameGroupImmediate group group' world
                    selectGroup true group'
                    ShowRenameGroupDialog <- false
                if ImGui.IsKeyReleased ImGuiKey.Escape then ShowRenameGroupDialog <- false
                ImGui.EndPopup ()
        | _ -> ShowRenameGroupDialog <- false

    let private imGuiOpenEntityDialog world =
        EntityFileDialogState.Title <- "Choose a nuentity file..."
        EntityFileDialogState.FilePattern <- "*.nuentity"
        EntityFileDialogState.FileDialogType <- ImGuiFileDialogType.Open
        if ImGui.FileDialog (&ShowOpenEntityDialog, EntityFileDialogState) then
            let loaded = tryLoadEntity EntityFileDialogState.FilePath world
            ShowOpenEntityDialog <- not loaded

    let private imGuiSaveEntityDialog world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            EntityFileDialogState.Title <- "Save a nuentity file..."
            EntityFileDialogState.FilePattern <- "*.nuentity"
            EntityFileDialogState.FileDialogType <- ImGuiFileDialogType.Save
            if ImGui.FileDialog (&ShowSaveEntityDialog, EntityFileDialogState) then
                if not (PathF.HasExtension EntityFileDialogState.FilePath) then EntityFileDialogState.FilePath <- EntityFileDialogState.FilePath + ".nuentity"
                let saved = trySaveSelectedEntity EntityFileDialogState.FilePath world
                ShowSaveEntityDialog <- not saved
        | Some _ | None -> ShowSaveEntityDialog <- false

    let private imGuiRenameEntityDialog world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            let title = "Rename entity..."
            ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
            let opening = not (ImGui.IsPopupOpen title)
            if opening then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &ShowRenameEntityDialog, ImGuiWindowFlags.AlwaysAutoResize) then
                ImGui.Text "Entity Name:"
                ImGui.SameLine ()
                if opening then
                    ImGui.SetKeyboardFocusHere ()
                    EntityRename <- entity.Name
                ImGui.InputTextWithHint ("##entityRename", "[enter entity name]", &EntityRename, 4096u) |> ignore<bool>
                let entity' = Nu.Entity (Array.add EntityRename entity.Parent.SimulantAddress.Names)
                if (ImGui.Button "Apply" || ImGui.IsKeyReleased ImGuiKey.Enter) &&
                   String.notEmpty EntityRename &&
                   Address.validateIdentifierName EntityRename &&
                   not (EntityRename.Contains '"') &&
                   not (entity'.GetExists world) then
                    snapshot RenameEntity world
                    World.renameEntityImmediate entity entity' world
                    SelectedEntityOpt <- Some entity'
                    ShowRenameEntityDialog <- false
                if ImGui.IsKeyReleased ImGuiKey.Escape then ShowRenameEntityDialog <- false
                ImGui.EndPopup ()
        | Some _ | None -> ShowRenameEntityDialog <- false

    let private imGuiDeleteEntityDialog world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            let title = "Entity deletion confirmation..."
            ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
            let opening = not (ImGui.IsPopupOpen title)
            if opening then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &ShowDeleteEntityDialog, ImGuiWindowFlags.AlwaysAutoResize) then
                ImGui.Text "Selected entity is an entity propagation source."
                ImGui.Text "Select a deletion option:"
                if ImGui.Button "Wipe propagation targets and delete entity." then
                    snapshot DeleteEntity world
                    World.clearPropagationTargets entity world
                    World.destroyEntity entity world
                    SelectedEntityOpt <- None
                    ShowDeleteEntityDialog <- false
                if ImGui.Button "Ignore propagation targets and delete entity (if you plan on replacing it)." then
                    snapshot DeleteEntity world
                    World.destroyEntity entity world
                    SelectedEntityOpt <- None
                    ShowDeleteEntityDialog <- false
                if ImGui.Button "Cancel deletion." || ImGui.IsKeyReleased ImGuiKey.Escape then
                    ShowDeleteEntityDialog <- false
                ImGui.EndPopup ()
        | Some _ | None -> ShowRenameEntityDialog <- false

    let private imGuiCutEntityDialog world =
        match SelectedEntityOpt with
        | Some entity when entity.GetExists world ->
            let title = "Entity cut confirmation..."
            ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
            let opening = not (ImGui.IsPopupOpen title)
            if opening then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &ShowCutEntityDialog, ImGuiWindowFlags.AlwaysAutoResize) then
                ImGui.Text "Selected entity is an entity propagation source."
                ImGui.Text "Select a cut option:"
                if ImGui.Button "Wipe propagation targets and cut entity." || ImGui.IsKeyReleased ImGuiKey.Enter then
                    snapshot CutEntity world
                    World.clearPropagationTargets entity world
                    World.cutEntityToClipboard entity world
                    SelectedEntityOpt <- None
                    ShowCutEntityDialog <- false
                if ImGui.Button "Ignore propagation targets and cut entity (if you plan on pasting or replacing it)." then
                    snapshot CutEntity world
                    World.cutEntityToClipboard entity world
                    SelectedEntityOpt <- None
                    ShowCutEntityDialog <- false
                if ImGui.Button "Cancel cut operation." || ImGui.IsKeyReleased ImGuiKey.Escape then
                    ShowCutEntityDialog <- false
                ImGui.EndPopup ()
        | Some _ | None -> ShowRenameEntityDialog <- false

    let private imGuiConfirmExitDialog world =
        let title = "Are you okay with exiting Gaia?"
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, &ShowConfirmExitDialog, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Any unsaved changes will be lost."
            if ImGui.Button "Okay" || ImGui.IsKeyReleased ImGuiKey.Enter then
                let gaiaState = makeGaiaState ProjectDllPath (Some ProjectEditMode) false world
                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                let gaiaDirectory = PathF.GetDirectoryName gaiaFilePath
                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                    Directory.SetCurrentDirectory gaiaDirectory
                with _ -> Log.error "Could not save gaia state."
                World.exit world
            ImGui.SameLine ()
            if ImGui.Button "Cancel" || ImGui.IsKeyReleased ImGuiKey.Escape then ShowConfirmExitDialog <- false
            ImGui.EndPopup ()

    let private imGuiRestartDialog world =
        let title = "Editor restart required."
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Gaia will apply your configuration changes and exit. Restart Gaia after exiting."
            if ImGui.Button "Okay" || ImGui.IsKeyPressed ImGuiKey.Enter then // HACK: checking key pressed event so that previous gui's key release won't bypass this.
                World.exit world
            ImGui.EndPopup ()

    let private imGuiMessageBoxDialog (message : string) =
        let title = "Message!"
        let mutable showing = true
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        ImGui.SetNextWindowSize (v2 900.0f 0.0f) // HACK: this is needed since auto-resizing windows don't work with ImGui.TextWrapped (https://github.com/ocornut/imgui/issues/778)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, &showing) then
            ImGui.TextWrapped message
            if ImGui.Button "Okay" || ImGui.IsKeyReleased ImGuiKey.Enter || ImGui.IsKeyReleased ImGuiKey.Escape then showing <- false
            if not showing then MessageBoxOpt <- None
            ImGui.EndPopup ()

    let private imGuiViewportContext world =
        ImGui.SetNextWindowPos RightClickPosition
        ImGui.SetNextWindowSize (v2 290.0f -1.0f)
        if ImGui.Begin ("Context Menu", ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoNav) then
            if ImGui.Button "Create" then
                createEntity true false world
                ShowEntityContextMenu <- false
            ImGui.SameLine ()
            ImGui.SetNextItemWidth -1.0f
            if ImGui.BeginCombo ("##newEntityDispatcherName", NewEntityDispatcherName, ImGuiComboFlags.HeightRegular) then
                let dispatcherNames = (World.getEntityDispatchers world).Keys
                let dispatcherNamePicked = tryPickName dispatcherNames
                for dispatcherName in dispatcherNames do
                    if ImGui.Selectable (dispatcherName, strEq dispatcherName NewEntityDispatcherName) then
                        NewEntityDispatcherName <- dispatcherName
                    if Some dispatcherName = dispatcherNamePicked then ImGui.SetScrollHereY Constants.Gaia.HeightRegularPickOffset
                    if dispatcherName = NewEntityDispatcherName then ImGui.SetItemDefaultFocus ()
                ImGui.EndCombo ()
            if SelectedEntityOpt.IsSome then
                if ImGui.Button "Create as Child" then
                    createEntity true true world
                    ShowEntityContextMenu <- false
                ImGui.SameLine ()
                if ImGui.Button "at Local Origin" then
                    createEntity true true world
                    tryMoveSelectedEntityToOrigin true world |> ignore<bool>
                    ShowEntityContextMenu <- false
            if ImGui.Button "Delete" then
                tryDeleteSelectedEntity world |> ignore<bool>
                ShowEntityContextMenu <- false
            if  ImGui.IsMouseReleased ImGuiMouseButton.Right ||
                ImGui.IsKeyReleased ImGuiKey.Escape then
                ShowEntityContextMenu <- false
            ImGui.Separator ()
            if ImGui.Button "Cut Entity" then
                tryCutSelectedEntity world |> ignore<bool>
                ShowEntityContextMenu <- false
            if ImGui.Button "Copy Entity" then
                tryCopySelectedEntity world |> ignore<bool>
                ShowEntityContextMenu <- false
            if ImGui.Button "Paste Entity" then
                tryPaste PasteAtMouse (Option.map cast NewEntityParentOpt) world |> ignore<bool>
                ShowEntityContextMenu <- false
            if SelectedEntityOpt.IsSome then
                if ImGui.Button "Paste Entity as Child" then
                    tryPaste PasteAtMouse (Option.map cast SelectedEntityOpt) world |> ignore<bool>
                    ShowEntityContextMenu <- false
                ImGui.SameLine ()
                ImGui.PushID "##pasteAsChildLocalOrigin" // NOTE: needed to differentiate from same-named button above.
                if ImGui.Button "at Local Origin" then
                    if tryPaste PasteAtMouse (Option.map cast SelectedEntityOpt) world then
                        tryMoveSelectedEntityToOrigin true world |> ignore<bool>
                    ShowEntityContextMenu <- false
                ImGui.PopID ()
            ImGui.Separator ()
            if ImGui.Button "Open Entity" then
                ShowOpenEntityDialog <- true
                ShowEntityContextMenu <- false
            if ImGui.Button "Save Entity" then
                match SelectedEntityOpt with
                | Some entity when entity.GetExists world ->
                    match Map.tryFind entity.EntityAddress EntityFilePaths with
                    | Some filePath -> EntityFileDialogState.FilePath <- filePath
                    | None -> EntityFileDialogState.FileName <- ""
                    ShowSaveEntityDialog <- true
                    ShowEntityContextMenu <- false
                | Some _ | None -> ()
            ImGui.Separator ()
            if SelectedEntityOpt.IsSome && NewEntityParentOpt = SelectedEntityOpt then
                if ImGui.Button "Reset Creation Parent" then
                    NewEntityParentOpt <- None
                    ShowEntityContextMenu <- false
            elif ImGui.Button "Set as Creation Parent" then
                NewEntityParentOpt <- SelectedEntityOpt
                ShowEntityContextMenu <- false
            if ImGui.Button "Auto Bounds Entity" then
                tryAutoBoundsSelectedEntity world |> ignore<bool>
                ShowEntityContextMenu <- false
            if ImGui.Button "Move to Origin" then
                tryMoveSelectedEntityToOrigin false world |> ignore<bool>
                ShowEntityContextMenu <- false
            if ImGui.Button "Propagate Entity" then
                tryPropagateSelectedEntityStructure world |> ignore<bool>
                ShowEntityContextMenu <- false
            if ImGui.Button "Wipe Propagated Descriptor" then
                tryWipeSelectedEntityPropagationTargets world |> ignore<bool>
                ShowEntityContextMenu <- false
            if ImGui.Button "Show in Hierarchy" then
                ShowSelectedEntity <- true
                ShowEntityContextMenu <- false
            let operation = ViewportContext { RightClickPosition = RightClickPosition; EditContext = makeContext None None }
            World.editGame operation Game world
            World.editScreen operation SelectedScreen world
            World.editGroup operation SelectedGroup world
            match SelectedEntityOpt with
            | Some selectedEntity -> World.editEntity operation selectedEntity world
            | None -> ()
        ImGui.End ()

    let private imGuiReloadingAssetsDialog world =
        let title = "Reloading assets..."
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
            ImGui.EndPopup ()
        ReloadAssetsRequested <- inc ReloadAssetsRequested
        if ReloadAssetsRequested = 4 then // NOTE: takes multiple frames to see dialog.
            tryReloadAssets world
            ReloadAssetsRequested <- 0

    let private imGuiReloadingCodeDialog world =
        let title = "Reloading code..."
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
            ImGui.EndPopup ()
        ReloadCodeRequested <- inc ReloadCodeRequested
        if ReloadCodeRequested = 4 then // NOTE: takes multiple frames to see dialog.
            tryReloadCode world
            ReloadCodeRequested <- 0

    let private imGuiReloadingAllDialog world =
        let title = "Reloading assets and code..."
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
            ImGui.EndPopup ()
        ReloadAllRequested <- inc ReloadAllRequested
        if ReloadAllRequested = 4 then // NOTE: takes multiple frames to see dialog.
            tryReloadAll world
            ReloadAllRequested <- 0

    let private imGuiSelectedWindowRestoration () =
        SelectedWindowRestoreRequested <- inc SelectedWindowRestoreRequested
        if SelectedWindowRestoreRequested = 4 then
            match SelectedWindowOpt with
            | Some window -> ImGui.SetWindowFocus window
            | None -> ()
            SelectedWindowRestoreRequested <- 0

    let private imGuiExceptionDialog exn worldStateOld world =
        let title = "Unhandled Exception!"
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        ImGui.SetNextWindowSize (v2 900.0f 0.0f) // HACK: this is needed since auto-resizing windows don't work with ImGui.TextWrapped (https://github.com/ocornut/imgui/issues/778)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title) then
            ImGui.Text "Exception text:"
            ImGui.TextWrapped (scstring exn)
            ImGui.Text "How would you like to handle this exception?"
            if ImGui.Button "Soft Rewind: undo the current ImGui frame." then
                World.switch worldStateOld world
                RecoverableExceptionOpt <- None
            if ImGui.Button "Hard Rewind: undo the last edit operation." then
                tryUndo world |> ignore<bool>
                RecoverableExceptionOpt <- None
            if ImGui.Button "Ignore: proceed with current world as-is." then
                RecoverableExceptionOpt <- None
            if ImGui.Button "Exit: close the editor." then
                World.exit world
                RecoverableExceptionOpt <- None
            ImGui.EndPopup ()

    let private imGuiProcess (world : World) =

        // store old world state
        let worldStateOld = world.CurrentState

        // detect if eyes were changed somewhere other than in the editor (such as in gameplay code)
        if  world.Eye2dCenter <> DesiredEye2dCenter ||
            world.Eye3dCenter <> DesiredEye3dCenter ||
            world.Eye3dRotation <> DesiredEye3dRotation then
            EyeChangedElsewhere <- true

        // enable global docking
        ImGui.DockSpaceOverViewport (0u, ImGui.GetMainViewport (), ImGuiDockNodeFlags.PassthruCentralNode) |> ignore<uint>

        // attempt to proceed with normal operation
        match RecoverableExceptionOpt with
        | None ->

            // use a generalized exception process
            try imGuiViewportManipulation world

                // windows
                let entityHierarchyFocused =
                    if FullScreen then
                        imGuiFullScreenWindow world
                        false
                    else
                        imGuiMainMenuWindow world
                        let entityHierarchyFocused = imGuiHierarchyWindow world
                        ExpandEntityHierarchy <- false
                        CollapseEntityHierarchy <- false
                        imGuiTimelineWindow ()
                        imGuiGamePropertiesWindow world 
                        imGuiScreenPropertiesWindow world 
                        imGuiGroupPropertiesWindow world 
                        imGuiEntityPropertiesWindow world 
                        imGuiEditPropertyWindow world
                        imGuiAssetGraphWindow ()
                        imGuiOverlayerWindow ()
                        imGuiPropagationSourcesWindow world
                        imGuiEditorConfigWindow ()
                        imGuiAudioPlayerWindow world
                        imGuiRendererWindow world
                        imGuiEventTracingWindow world
                        imGuiInteractiveWindow world
                        imGuiMetricsWindow world
                        imGuiLogWindow world
                        imGuiAssetViewerWindow world // HACK: for some reason, this window must be processed last in order for it to be selected by default.
                        imGuiDebugWindow ()
                        entityHierarchyFocused

                // prompt dialogs
                match MessageBoxOpt with
                | None ->
                    if ShowNewProjectDialog then imGuiNewProjectDialog world
                    if ShowOpenProjectDialog && not ShowOpenProjectFileDialog then imGuiOpenProjectDialog world
                    elif ShowOpenProjectFileDialog then imGuiOpenProjectFileDialog ()
                    if ShowCloseProjectDialog then imGuiCloseProjectDialog world
                    if ShowNewGroupDialog then imGuiNewGroupDialog world
                    if ShowOpenGroupDialog then imGuiOpenGroupDialog world
                    if ShowSaveGroupDialog then imGuiSaveGroupDialog world
                    if ShowRenameGroupDialog then imGuiRenameGroupDialog world
                    if ShowOpenEntityDialog then imGuiOpenEntityDialog world
                    if ShowSaveEntityDialog then imGuiSaveEntityDialog world
                    if ShowRenameEntityDialog then imGuiRenameEntityDialog world
                    if ShowDeleteEntityDialog then imGuiDeleteEntityDialog world
                    if ShowCutEntityDialog then imGuiCutEntityDialog world
                    if ShowConfirmExitDialog then imGuiConfirmExitDialog world
                    if ShowRestartDialog then imGuiRestartDialog world
                | Some message -> imGuiMessageBoxDialog message

                // viewport context menu
                if ShowEntityContextMenu then imGuiViewportContext world

                // non-imgui input
                updateEyeDrag world
                updateEyeTravel world
                updateEntityContext world
                updateEntityDrag world
                updateHotkeys entityHierarchyFocused world

                // reloading dialogs
                if ReloadAssetsRequested > 0 then imGuiReloadingAssetsDialog world
                if ReloadCodeRequested > 0 then imGuiReloadingCodeDialog world
                if ReloadAllRequested > 0 then imGuiReloadingAllDialog world

                // selected window restoration
                if SelectedWindowRestoreRequested > 0 then imGuiSelectedWindowRestoration ()

            // propagate exception to dialog
            with exn -> RecoverableExceptionOpt <- Some exn

        // exception handling dialog
        | Some exn -> imGuiExceptionDialog exn worldStateOld world

    let private imGuiRender world =

        // augmentative rendering while not in capture mode
        if not CaptureMode then

            // HACK: in order to successfully focus entity properties when clicking in the viewport in the current version
            // of Dear ImGui, we seem to have to the the window focus command AFTER normal processing.
            if EntityPropertiesFocusRequested then
                ImGui.SetWindowFocus "Entity Properties"
                EntityPropertiesFocusRequested <- false

            // render light probes of the selected group in light box and view frustum
            let lightBox = World.getLight3dViewBox world
            let eyeFrustum = World.getEye3dFrustum world
            let entities = World.getLightProbes3dInViewBox lightBox (HashSet ()) world
            let lightProbeModels =
                entities
                |> Seq.filter (fun entity -> entity.Group = SelectedGroup && eyeFrustum.Intersects (entity.GetBounds world))
                |> Seq.map (fun light -> (light.GetAffineMatrix world, false, Omnipresent, None, MaterialProperties.defaultProperties))
                |> SList.ofSeq
            if SList.notEmpty lightProbeModels then
                World.enqueueRenderMessage3d
                    (RenderStaticModels
                        { StaticModels = lightProbeModels
                          StaticModel = Assets.Default.LightProbeModel
                          Clipped = false
                          DepthTest = LessThanOrEqualTest
                          RenderType = DeferredRenderType
                          RenderPass = NormalPass })
                    world

            // render lights of the selected group in play
            let entities = World.getLights3dInViewBox lightBox (HashSet ()) world
            let lightModels =
                entities
                |> Seq.filter (fun entity -> entity.Group = SelectedGroup && eyeFrustum.Intersects (entity.GetBounds world))
                |> Seq.map (fun light -> (light.GetAffineMatrix world, false, Omnipresent, None, MaterialProperties.defaultProperties))
                |> SList.ofSeq
            if SList.notEmpty lightModels then
                World.enqueueRenderMessage3d
                    (RenderStaticModels
                        { StaticModels = lightModels
                          StaticModel = Assets.Default.LightbulbModel
                          Clipped = false
                          DepthTest = LessThanOrEqualTest
                          RenderType = DeferredRenderType
                          RenderPass = NormalPass })
                    world

            // render selection highlights
            match SelectedEntityOpt with
            | Some entity when entity.GetExists world ->
                if entity.GetIs2d world then
                    let absolute = entity.GetAbsolute world
                    let bounds = entity.GetBounds world
                    let elevation = Single.MaxValue
                    let transform = Transform.makePerimeter absolute bounds v3Zero elevation
                    let image = Assets.Default.HighlightSprite
                    World.enqueueRenderMessage2d
                        (LayeredOperation2d
                            { Elevation = elevation
                              Horizon = bounds.Bottom.Y
                              AssetTag = image
                              RenderOperation2d =
                                RenderSprite
                                    { Transform = transform
                                      InsetOpt = ValueNone
                                      ClipOpt = ValueNone
                                      Image = image
                                      Color = Color.One
                                      Blend = Transparent
                                      Emission = Color.Zero
                                      Flip = FlipNone }})
                        world
                else
                    let bounds = entity.GetBounds world
                    let bounds = box3 (bounds.Min - v3Dup 0.005f) (bounds.Size + v3Dup 0.01f) // slightly bigger to eye to prevent z-fighting with selected entity
                    for i in 0 .. dec 6 do
                        let (translation, rotation, scale) =
                            match i with
                            | 0 -> (v3Forward * bounds.Depth * 0.5f, quatIdentity, v3 bounds.Width bounds.Height 0.01f) // back face
                            | 1 -> (v3Back * bounds.Depth * 0.5f, Quaternion.CreateFromAxisAngle (v3Up, MathF.PI), v3 bounds.Width bounds.Height 0.01f) // front face
                            | 2 -> (v3Left * bounds.Width * 0.5f, Quaternion.CreateFromAxisAngle (v3Up, MathF.PI_OVER_2), v3 bounds.Depth bounds.Height 0.01f) // left face
                            | 3 -> (v3Right * bounds.Width * 0.5f, Quaternion.CreateFromAxisAngle (v3Up, -MathF.PI_OVER_2), v3 bounds.Depth bounds.Height 0.01f) // right face
                            | 5 -> (v3Down * bounds.Height * 0.5f, Quaternion.CreateFromAxisAngle (v3Right, -MathF.PI_OVER_2), v3 bounds.Width bounds.Depth 0.01f) // bottom face
                            | 4 -> (v3Up * bounds.Height * 0.5f, Quaternion.CreateFromAxisAngle (v3Right, MathF.PI_OVER_2), v3 bounds.Width bounds.Depth 0.01f) // top face
                            | _ -> failwithumf ()
                        let position = bounds.Center + translation
                        let sort =
                            let faceDistance = world.Eye3dCenter.Distance position
                            let centerDistance = world.Eye3dCenter.Distance bounds.Center
                            if faceDistance < centerDistance then Single.MaxValue else Single.MinValue
                        let mutable boundsMatrix = Matrix4x4.CreateAffine (position, rotation, scale)
                        World.enqueueRenderMessage3d
                            (RenderStaticModel
                                { ModelMatrix = boundsMatrix
                                  CastShadow = false
                                  Presence = Omnipresent
                                  InsetOpt = None
                                  MaterialProperties = { MaterialProperties.defaultProperties with SpecularScalarOpt = ValueSome 0.0f }
                                  StaticModel = Assets.Default.HighlightModel
                                  Clipped = false // not needed when forward-rendered
                                  DepthTest = LessThanOrEqualTest
                                  RenderType = ForwardRenderType (0.0f, sort)
                                  RenderPass = NormalPass })
                            world
            | Some _ | None -> ()

    let private imGuiPostProcess (world : World) =

        // override local desired eye changes if eye was changed elsewhere
        if EyeChangedElsewhere then
            DesiredEye2dCenter <- world.Eye2dCenter
            DesiredEye3dCenter <- world.Eye3dCenter
            DesiredEye3dRotation <- world.Eye3dRotation
            EyeChangedElsewhere <- false
        else
            World.setEye2dCenter DesiredEye2dCenter world
            World.setEye3dCenter DesiredEye3dCenter world
            World.setEye3dRotation DesiredEye3dRotation world

    let rec private runWithCleanUpAndErrorProtection firstFrame world =
        try World.runWithoutCleanUp tautology ignore ignore imGuiRender imGuiProcess imGuiPostProcess firstFrame world
            World.cleanUp world
            Constants.Engine.ExitCodeSuccess
        with exn ->
            if tryUndo world then
                Futures <- [] // NOTE: clearing invalid futures.
                let wasAdvancing = world.Advancing
                if wasAdvancing then World.setAdvancing false world
                let errorMsg =
                    "Unexpected exception!\n" +
                    "Rewound to previous world" + (if wasAdvancing then " and halted." else ".") +
                    "\nError due to: " + exn.Message +
                    "\nStack trace:\n" + string exn.StackTrace
                Log.error errorMsg
                MessageBoxOpt <- Some errorMsg
                runWithCleanUpAndErrorProtection false world
            else
                let errorMsg = "Unexpected exception! Could not rewind world. Error due to: " + scstring exn
                Log.error errorMsg
                Constants.Engine.ExitCodeFailure

    let rec private runWithCleanUp gaiaState targetDir_ screen world =
        OpenProjectFilePath <- gaiaState.ProjectDllPath
        OpenProjectImperativeExecution <- gaiaState.ProjectImperativeExecution
        CloseProjectImperativeExecution <- gaiaState.ProjectImperativeExecution
        Snaps2dSelected <- gaiaState.Snaps2dSelected
        Snaps2d <- gaiaState.Snaps2d
        Snaps3d <- gaiaState.Snaps3d
        NewEntityDispatcherName <- World.getEntityDispatchers world |> Seq.head |> fun kvp -> kvp.Key
        NewEntityElevation <- gaiaState.CreationElevation
        NewEntityDistance <- gaiaState.CreationDistance
        { new TraceListener () with
            override this.Write (message : string) = concatLog message
            override this.WriteLine (message : string) = concatLog (message + "\n") }
        |> Trace.Listeners.Add
        |> ignore<int>
        AlternativeEyeTravelInput <- gaiaState.AlternativeEyeTravelInput
        let world =
            if not gaiaState.ProjectFreshlyLoaded then
                EditWhileAdvancing <- gaiaState.EditWhileAdvancing
                DesiredEye2dCenter <- gaiaState.DesiredEye2dCenter
                DesiredEye3dCenter <- gaiaState.DesiredEye3dCenter
                DesiredEye3dRotation <- gaiaState.DesiredEye3dRotation
                World.setEye2dCenter DesiredEye2dCenter world
                World.setEye3dCenter DesiredEye3dCenter world
                World.setEye3dRotation DesiredEye3dRotation world
                World.setMasterSoundVolume gaiaState.MasterSoundVolume world
                World.setMasterSongVolume gaiaState.MasterSongVolume world
                world
            else world
        TargetDir <- targetDir_
        ProjectDllPath <- OpenProjectFilePath
        ProjectFileDialogState <- ImGuiFileDialogState TargetDir
        ProjectEditMode <- Option.defaultValue "" gaiaState.ProjectEditModeOpt
        ProjectImperativeExecution <- OpenProjectImperativeExecution
        GroupFileDialogState <- ImGuiFileDialogState (TargetDir + "/../../..")
        EntityFileDialogState <- ImGuiFileDialogState (TargetDir + "/../../..")
        selectScreen false screen
        selectGroupInitial screen world
        AssetGraphStr <-
            let assetGraph = AssetGraph.makeFromFileOpt (TargetDir + "/" + Assets.Global.AssetGraphFilePath)
            let packageDescriptorsStr = scstring assetGraph.PackageDescriptors
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
        OverlayerStr <-
            let overlayerFilePath = TargetDir + "/" + Assets.Global.OverlayerFilePath
            let overlayer = Overlayer.makeFromFileOpt [] overlayerFilePath
            let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
            PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
        FsiSession <- Shell.FsiEvaluationSession.Create (FsiConfig, FsiArgs, FsiInStream, FsiOutStream, FsiErrorStream)
        let result = runWithCleanUpAndErrorProtection true world
        (FsiSession :> IDisposable).Dispose () // not sure why we have to cast here...
        FsiErrorStream.Dispose ()
        FsiInStream.Dispose ()
        FsiOutStream.Dispose ()
        result

    (* Public Functions *)

    /// Run Gaia.
    let run gaiaState targetDir plugin =

        // ensure imgui ini file exists and was created by Gaia before initialising imgui
        let imguiIniFilePath = targetDir + "/imgui.ini"
        if  not (File.Exists imguiIniFilePath) ||
            (File.ReadAllLines imguiIniFilePath).[0] <> "[Window][Gaia]" then
            File.WriteAllText (imguiIniFilePath, ImGuiIniFileStr)

        // attempt to create SDL dependencies
        let windowSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let outerViewport = Viewport.makeOuter windowSize
        let rasterViewport = Viewport.makeRaster outerViewport.Bounds
        let geometryViewport = Viewport.makeGeometry outerViewport.Bounds.Size
        match tryMakeSdlDeps true windowSize with
        | Right (sdlConfig, sdlDeps) ->

            // create the world
            let worldConfig =
                { Imperative = gaiaState.ProjectImperativeExecution
                  Accompanied = true
                  Advancing = false
                  FramePacing = false
                  ModeOpt = gaiaState.ProjectEditModeOpt
                  SdlConfig = sdlConfig }
            let (screen, world) =
                makeWorld sdlDeps worldConfig geometryViewport rasterViewport outerViewport plugin

            // subscribe to events related to editing
            World.subscribe handleNuMouseButton Game.MouseLeftDownEvent Game world |> ignore
            World.subscribe handleNuMouseButton Game.MouseLeftUpEvent Game world |> ignore
            World.subscribe handleNuMouseButton Game.MouseMiddleDownEvent Game world |> ignore
            World.subscribe handleNuMouseButton Game.MouseMiddleUpEvent Game world |> ignore
            World.subscribe handleNuMouseButton Game.MouseRightDownEvent Game world |> ignore
            World.subscribe handleNuMouseButton Game.MouseRightUpEvent Game world |> ignore
            World.subscribe handleNuLifeCycleGroup (Game.LifeCycleEvent (nameof Group)) Game world |> ignore
            World.subscribe handleNuSelectedScreenOptChange Game.SelectedScreenOpt.ChangeEvent Game world |> ignore
            World.subscribe handleNuExitRequest Game.ExitRequestEvent Game world |> ignore

            // run the world
            runWithCleanUp gaiaState targetDir screen world

        // handle error
        | Left error -> Log.error error; Constants.Engine.ExitCodeFailure