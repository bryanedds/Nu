// Gaia - The Nu Game Engine editor.
// Required Notice:
// Copyright (C) Bryan Edds.
// Gaia - The Nu Game Engine editor is licensed under the Nu Game Engine Noncommercial License.
// See: https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Reflection
open System.Text
open FSharp.Compiler.Interactive
open FSharp.Reflection
open Microsoft.FSharp.Core
open ImGuiNET
open ImGuizmoNET
open ImPlotNET
open JoltPhysicsSharp
open Prime
open Nu
open Nu.Gaia

//////////////////////////////////////////////
// TODO:                                    //
// Custom properties in order of priority:  //
//  Enums                                   //
//  Flag Enums                              //
//  DateTimeOffset                          //
//  SymbolicCompression                     //
//  CollisionMask                           //
//  CollisionCategories                     //
//  CollisionDetection                      //
//////////////////////////////////////////////

[<RequireQualifiedAccess>]
module Gaia =

    (* Active Editing States *)

    let mutable private Pasts = [] : (SnapshotType * WorldState) list
    let mutable private Futures = [] : (SnapshotType * WorldState) list
    let mutable private Stepping = false
    let mutable private SelectedWindowOpt = Some "Viewport" // restored as gaia starts so the viewport has focus
    let mutable private SelectedWindowRestoreRequested = 1 // NOTE: restoring takes some frames, outlasting windows appearing.
    let mutable private EntityPropertiesFocusRequested = false
    let mutable private ViewportFocusRequested = false
    let mutable private ImGuiIniResetRequested = false
    let mutable private TimelineChanged = false
    let mutable private ManipulationActive = false
    let mutable private ManipulationScalePrevious = v3One // the selection's scale as last manipulated, starting at one
    let mutable private ManipulationPositionStart = v3Zero // the entity's position when manipulation was taken up
    let mutable private ManipulationOperation = OPERATION.TRANSLATE
    let mutable private ExpandEntityHierarchy = false
    let mutable private CollapseEntityHierarchy = false
    let mutable private ShowSelectedEntity = false
    let mutable private RightClickPosition = v2Zero
    let mutable private PropertyFocusedOpt = Option<PropertyDescriptor * Simulant>.None
    let mutable private PropertyEditorFocusRequested = false
    let mutable private PropertyValueSearchStr = ""
    let mutable private PropertyValueSearchIndex = -1
    let mutable private PropertyValueSearchRequested = false
    let mutable private PropertyValueSelectionRequestOpt = Option<int * int>.None
    let mutable private EntityHierarchySearchRequested = false
    let mutable private AssetViewerSearchRequested = false
    let mutable private DragDropPayloadOpt = Option<DragDropPayload>.None
    let mutable private DragEntityState = DragEntityInactive
    let mutable private DragEyeState = DragEyeInactive
    let mutable private SelectedScreen = Game / "Screen" // TODO: P1: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable private SelectedGroup = SelectedScreen / "Group" // use the default group
    let mutable private SelectedEntities = Set.empty<Entity>
    let mutable private SelectionCollapseRequestedOpt = Option<Entity>.None // what to select alone on mouse release
    let mutable private OpenProjectDllsOpt = Option<string array>.None
    let mutable private OpenProjectNames = [||]
    let mutable private OpenProjectIndex = -1
    let mutable private OpenProjectEditMode = ""
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
    let mutable private ResetEyeRequested = false
    let mutable private FpsStartDateTime = DateTimeOffset.Now
    let mutable private FpsStartUpdateTime = 0L
    let mutable private InteractiveNeedsInitialization = true
    let mutable private InteractiveInputFocusRequested = false
    let mutable private InteractiveInputStr = ""
    let mutable private InteractiveOutputStr = ""
    let mutable private FsiSessionHasProjectCode = false
    let mutable private LogStr = ""
    let private EntityHierarchyIndexed = List<Entity> () // indexed lookup of hierarchy's displayed entities
    let private PropagationSourcesIndexed = List<Entity> () // indexed lookup of the propagation sources displayed
    let private FsiSessionLateBindingTypes = HashSet<Type> HashIdentity.Reference

    (* Configuration States *)

    let mutable private OverlayMode = false
    let mutable private ViewMode = NormalMode
    let mutable private EditWhileAdvancing = false
    let mutable private ManipulationWorld = true
    let mutable private Snaps2dSelected = true
    let mutable private Snaps2d = Constants.Gaia.Snaps2dDefault
    let mutable private Snaps3d = Constants.Gaia.Snaps3dDefault
    let mutable private SnapDrag = 0.1f
    let mutable private PhysicsDebugRendering2d = false
    let mutable private PhysicsDebugRendering3d = false
    let mutable private PhysicsDebugRenderingDistance3d = Constants.Render.Body3dRenderDistanceMax
    let mutable private PhysicsDebugRenderingHeightMaps3d = false
    let mutable private ImGuiDebugWindow = false
    let mutable private EntityHierarchySearchStr = ""
    let mutable private EntityHierarchyShowModified = false
    let mutable private PropagationSourcesSearchStr = ""
    let mutable private AssetViewerSearchStr = ""
    let mutable private LoadPackageName = ""

    (* Project States *)

    let mutable private TargetDir = "."
    let mutable private ProjectDllPath = ""
    let mutable private ProjectFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private ProjectEditMode = ""
    let mutable private ProjectImperativeExecution = false
    let mutable private GroupFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private GroupFilePaths = Map.empty<Group, string>
    let mutable private GroupWorldStates = Map.empty<Group, WorldState>
    let mutable private EntityWorldStatesReverted = Map.empty<Entity, WorldState list>
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
    let mutable private ShowInputsDialog = false
    let mutable private ShowConfirmExitDialog = false
    let mutable private ShowRestartDialog = false
    let mutable private ReloadAssetsRequested = 0
    let mutable private ReloadCodeRequested = (false, 0)
    let mutable private ReloadAllRequested = (false, 0)
    let modal () =
        MessageBoxOpt.IsSome ||
        RecoverableExceptionOpt.IsSome ||
        ShowEntityContextMenu ||
        ShowNewProjectDialog ||
        ShowOpenProjectDialog ||
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
        ShowInputsDialog ||
        ShowConfirmExitDialog ||
        ShowRestartDialog ||
        ReloadAssetsRequested <> 0 ||
        snd ReloadCodeRequested <> 0 ||
        snd ReloadAllRequested <> 0

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
Size=1920,54
Collapsed=0
DockId=0x0000000F,0

[Window][Propagation Sources]
Pos=290,844
Size=665,236
Collapsed=0
DockId=0x0000000E,3

[Window][Edit Overlayer]
Pos=290,844
Size=665,236
Collapsed=0
DockId=0x0000000E,2

[Window][Edit Asset Graph]
Pos=290,844
Size=665,236
Collapsed=0
DockId=0x0000000E,1

[Window][Edit Property]
Pos=290,844
Size=665,236
Collapsed=0
DockId=0x0000000E,0

[Window][Log]
Pos=957,844
Size=666,236
Collapsed=0
DockId=0x00000002,7

[Window][Metrics]
Pos=957,844
Size=666,236
Collapsed=0
DockId=0x00000002,6

[Window][Interactive]
Pos=957,844
Size=666,236
Collapsed=0
DockId=0x00000002,5

[Window][Event Tracing]
Pos=957,844
Size=666,236
Collapsed=0
DockId=0x00000002,4

[Window][Renderer]
Pos=957,844
Size=666,236
Collapsed=0
DockId=0x00000002,3

[Window][Audio Player]
Pos=957,844
Size=666,236
Collapsed=0
DockId=0x00000002,2

[Window][Editor]
Pos=957,844
Size=666,236
Collapsed=0
DockId=0x00000002,1

[Window][Asset Viewer]
Pos=957,844
Size=666,236
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
Pos=836,504
Size=247,71
Collapsed=0

[Window][ContextMenu]
Pos=853,391
Size=250,135
Collapsed=0

[Window][Viewport]
Pos=0,0
Size=1920,1080
Collapsed=0

[Window][Entity Hierarchy]
Pos=0,56
Size=288,786
Collapsed=0
DockId=0x00000009,0

[Window][Timeline]
Pos=0,844
Size=288,236
Collapsed=0
DockId=0x0000000A,0

[Window][Entity Properties]
Pos=1625,56
Size=295,1024
Collapsed=0
DockId=0x00000001,3

[Window][Group Properties]
Pos=1625,56
Size=295,1024
Collapsed=0
DockId=0x00000001,2

[Window][Screen Properties]
Pos=1625,56
Size=295,1024
Collapsed=0
DockId=0x00000001,1

[Window][Game Properties]
Pos=1625,56
Size=295,1024
Collapsed=0
DockId=0x00000001,0

[Window][Dear ImGui Debug Log]
Pos=293,58
Size=418,212
Collapsed=0

[Window][Create Nu Project... *EDITOR RESTART REQUIRED!*]
Pos=699,495
Size=524,138
Collapsed=0

[Window][Choose a project... *EDITOR RESTART REQUIRED!*]
Pos=434,298
Size=411,123
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

[Window][Gaia Inputs]
Pos=448,180
Size=1024,720
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
Size=1920,1080
Collapsed=0

[Window][Context Menu]
Pos=1377,435
Size=280,323
Collapsed=0

[Window][Block Editor]
Pos=290,844
Size=665,236
Collapsed=0
DockId=0x0000000E,4

[Docking][Data]
DockSpace           ID=0x7C6B3D9B Window=0xA87D555D Pos=0,0 Size=1920,1080 Split=Y
  DockNode          ID=0x0000000F Parent=0x7C6B3D9B SizeRef=1920,54 HiddenTabBar=1 Selected=0x48908BE7
  DockNode          ID=0x00000010 Parent=0x7C6B3D9B SizeRef=1920,1024 Split=X
    DockNode        ID=0x0000000C Parent=0x00000010 SizeRef=983,1080 Split=X
      DockNode      ID=0x00000004 Parent=0x0000000C SizeRef=288,1080 Split=Y
        DockNode    ID=0x00000009 Parent=0x00000004 SizeRef=284,786 Selected=0xAE464409
        DockNode    ID=0x0000000A Parent=0x00000004 SizeRef=284,236 Selected=0x0F18B61B
      DockNode      ID=0x0000000B Parent=0x0000000C SizeRef=693,1080 Split=Y
        DockNode    ID=0x0000000D Parent=0x0000000B SizeRef=1327,426 CentralNode=1 Selected=0xB80FFB58
        DockNode    ID=0x00000003 Parent=0x0000000B SizeRef=1327,236 Split=X Selected=0xB205577F
          DockNode  ID=0x0000000E Parent=0x00000003 SizeRef=345,205 Selected=0x9CF3CB04
          DockNode  ID=0x00000002 Parent=0x00000003 SizeRef=346,205 Selected=0xD92922EC
    DockNode        ID=0x00000001 Parent=0x00000010 SizeRef=295,1080 Selected=0xD5116FF8

"""

    (* ImGui Callbacks *)
    
    let private propertyValueTextCallback =
        ImGuiInputTextCallback (fun data ->
            match PropertyValueSelectionRequestOpt with
            | Some (selectionStart, selectionEnd) ->
                let data = ImGuiInputTextCallbackDataPtr data
                data.SelectionStart <- selectionStart
                data.SelectionEnd <- selectionEnd
                data.CursorPos <- selectionEnd
                PropertyValueSelectionRequestOpt <- None
            | None -> ()
            0)

    (* Prelude Functions *)

    let private truncateLog () =
        if LogStr.Length > Constants.Gaia.LogCharactersMax then
            let cutPoint = LogStr.IndexOf ('\n', LogStr.Length - Constants.Gaia.LogCharactersMax) |> inc
            LogStr <- "...\n" + LogStr[cutPoint ..]

    let private concatLog (str : string) =
        truncateLog ()
        LogStr <- LogStr + str

    let private shouldSwallowMouseButton (world : World) =
        let io = ImGui.GetIO ()
        not io.WantCaptureMouseGlobal &&
        (world.TimeHalted || EditWhileAdvancing)

    let private canEditWithMouse (world : World) =
        let io = ImGui.GetIO ()
        match ViewMode with
        | NormalMode | FreeMode ->
            not io.WantCaptureMouseGlobal &&
            (world.TimeHalted || EditWhileAdvancing)
        | CaptureMode -> false

    let private canEditWithKeyboard (world : World) =
        let io = ImGui.GetIO ()
        not io.WantCaptureKeyboardGlobal &&
        (world.TimeHalted || EditWhileAdvancing)

    let private snapshot snapshotType (world : World) =
        Pasts <- (snapshotType, world.CurrentState) :: Pasts
        Futures <- []
        TimelineChanged <- true

    let private makeGaiaState projectDllPath editModeOpt freshlyLoaded world : GaiaState =
        GaiaState.make
            projectDllPath editModeOpt freshlyLoaded OpenProjectImperativeExecution
            DesiredEye2dCenter DesiredEye3dCenter DesiredEye3dRotation (World.getMasterSoundVolume world) (World.getMasterSongVolume world)            
            Snaps2dSelected Snaps2d Snaps3d NewEntityElevation NewEntityDistance OverlayMode

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
        | Some (_, simulant : Simulant) when not (simulant :? Entity) -> selectEntities true Set.empty world
        | Some _ | None -> ()
        PropertyFocusedOpt <- targetOpt

    and private selectEntities focusEntityProperties (entities : Entity Set) world =

        // adjust editing state as needed when what's inspected changes
        let entityOpt = Seq.tryExactlyOne entities
        if entityOpt <> Seq.tryExactlyOne SelectedEntities then

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

            // adjust focus accordingly
            if entityOpt.IsSome && focusEntityProperties then EntityPropertiesFocusRequested <- true

        // adjust drag state and set the selection
        if entities.IsEmpty then DragEntityState <- DragEntityInactive
        SelectedEntities <- entities

    let private toggleEntitySelection (entity : Entity) world =
        if SelectedEntities.Contains entity
        then selectEntities false (Set.remove entity SelectedEntities) world
        else selectEntities true (Set.add entity SelectedEntities) world

    let private selectEntityUnlessSelected focusEntityProperties (entity : Entity) world =
        if not (SelectedEntities.Contains entity) then selectEntities focusEntityProperties (Set.singleton entity) world

    let private pruneEntitySelection world =
        let entities = Set.filter (fun (entity : Entity) -> entity.GetExists world && entity.Group = SelectedGroup) SelectedEntities
        if entities.Count <> SelectedEntities.Count then selectEntities false entities world

    let private getSelectedEntityRoots viaMounts world =
        let selected = Set.filter (fun (entity : Entity) -> entity.GetExists world) SelectedEntities
        let rec hasSelectedAncestor (entity : Entity) =
            (match entity.Parent with
             | :? Entity as parent -> selected.Contains parent || hasSelectedAncestor parent
             | _ -> false) ||
            (viaMounts &&
             match Option.bind (flip tryResolve entity) (entity.GetMountOpt world) with
             | Some mount -> selected.Contains mount || hasSelectedAncestor mount
             | None -> false)
        selected
        |> Seq.filter (not << hasSelectedAncestor)
        |> Seq.sortBy (fun entity ->
            match EntityHierarchyIndexed.IndexOf entity with
            | -1 -> Int32.MaxValue
            | index -> index)
        |> List.ofSeq

    let private getSelectedEntitiesUnprotected world =
        getSelectedEntityRoots false world |> List.forall (fun entity -> entity.GetProtection world = Unprotected)

    let private getSelectionMixed world =
        let entities = SelectedEntities |> Seq.filter (fun (entity : Entity) -> entity.GetExists world) |> List.ofSeq
        List.exists (fun (entity : Entity) -> entity.GetIs2d world) entities &&
        List.exists (fun (entity : Entity) -> not (entity.GetIs2d world)) entities

    let private duplicateEntities propagate (parentOpt : Simulant option) (entities : Entity list) world =
        [for entity in entities do
            let entityDescriptor = World.writeEntity false false EntityDescriptor.empty entity world
            let dispatcherName = entityDescriptor.EntityDispatcherName
            let entityName = World.generateEntitySequentialName dispatcherName entity.Group world
            let parent =
                match parentOpt with
                | Some parent -> parent
                | None -> NewEntityParentOpt |> Option.map cast<Simulant> |> Option.defaultValue entity.Group
            let duplicate = World.readEntity false false entityDescriptor (Some entityName) parent world
            if not propagate then
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
            (entity, duplicate)]

    let private getReplacement (entity : Entity) (replacements : (Entity * Entity) list) =
        replacements
        |> List.tryFind (fun (entity', _) -> entity' = entity)
        |> Option.orElse (List.tryHead replacements)
        |> Option.map snd
        |> Option.defaultValue entity

    let private duplicateSelectedEntities propagate world =
        let duplicates = duplicateEntities propagate None (getSelectedEntityRoots false world) world
        ImGui.SetWindowFocus "Viewport"
        selectEntities true (duplicates |> List.map snd |> Set.ofList) world
        ShowSelectedEntity <- true
        duplicates

    let private frameEntities (entities : Entity list) world =
        let entities = entities |> List.filter (fun (entity : Entity) -> entity.GetExists world && not (entity.GetAbsolute world))
        let (entities2d, entities3d) = List.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
        let getCenter (boxes : Box3 list) =
            let min = boxes |> List.map _.Min |> List.reduce (fun a b -> Vector3.Min (a, b))
            let max = boxes |> List.map _.Max |> List.reduce (fun a b -> Vector3.Max (a, b))
            ((min + max) * 0.5f, (max - min).Length () * 0.5f)
        match entities2d with
        | [] -> ()
        | [entity] -> World.setEye2dCenter (entity.GetPerimeterCenter world).V2 world
        | entities ->
            let (center, _) = getCenter (List.map (fun (entity : Entity) -> entity.GetPerimeter world) entities)
            World.setEye2dCenter center.V2 world
        match entities3d with
        | [] -> ()
        | [entity] ->
            let eyeCenterOffset = (v3Back * NewEntityDistance).Transform world.Eye3dRotation
            World.setEye3dCenter (entity.GetPosition world + eyeCenterOffset) world
        | entities ->
            let (center, radius) = getCenter (List.map (fun (entity : Entity) -> entity.GetBounds world) entities)
            let distance = max NewEntityDistance (radius / tan (world.Eye3dFieldOfView * 0.5f)) // far enough to see all
            World.setEye3dCenter (center + (v3Back * distance).Transform world.Eye3dRotation) world

    let private getHierarchyDragEntities (entity : Entity) world =
        if SelectedEntities.Contains entity
        then getSelectedEntityRoots false world
        else [entity]

    let private getHierarchyDropDestination (parentOpt : Entity option) (entity : Entity) =
        match parentOpt with
        | Some parent -> parent / entity.Name
        | None -> SelectedGroup / entity.Name

    let private tryGetHierarchyDropProblem (entities : Entity list) (parentOpt : Entity option) world =
        let getDestination = getHierarchyDropDestination parentOpt
        let circular (entity : Entity) =
            match parentOpt with
            | Some parent ->
                let entityToParent = Address.relate entity.EntityAddress parent.EntityAddress
                not (Array.contains Constants.Address.ParentName entityToParent.Names)
            | None -> false
        let clashing (entity : Entity) =
            let destination = getDestination entity
            destination <> entity && destination.GetExists world
        if List.exists (fun (entity : Entity) -> entity.GetProtection world <> Unprotected) entities then
            Some "Cannot relocate a protected simulant (such as an entity created by the ImSim or MMCC API)."
        elif List.exists circular entities then
            Some "Cannot mount an entity circularly."
        elif List.exists clashing entities || (entities |> List.distinctBy _.Name |> List.length) < entities.Length then
            match parentOpt with
            | Some _ -> Some "Cannot reparent an entity where the parent entity contains a child with the same name."
            | None -> Some "Cannot unparent an entity when there exists another unparented entity with the same name."
        else None

    let private tryDropEntitiesInHierarchy entities (parentOpt : Entity option) (nextOpt : Entity option) world =
        match tryGetHierarchyDropProblem entities parentOpt world with
        | Some problem -> MessageBoxOpt <- Some problem
        | None ->
            snapshot (if nextOpt.IsSome then ReorderEntities else RenameEntity) world
            let moves =
                [for entity in entities do
                    match nextOpt with
                    | Some next -> World.insertEntityOrder entity (World.tryGetPreviousEntity next world) next world
                    | None -> ()
                    let entity' = getHierarchyDropDestination parentOpt entity
                    if entity' <> entity then
                        if parentOpt.IsNone && World.getEntityAllowedToMount entity world then
                            entity.SetMountOptWithAdjustment false None world
                        World.renameEntityImmediate entity entity' world
                        if NewEntityParentOpt = Some entity then NewEntityParentOpt <- Some entity'
                    (entity, entity')]
            selectEntities false (moves |> List.map snd |> Set.ofList) world
            ShowSelectedEntity <- true

    let private tryDuplicateEntitiesInHierarchy (entities : Entity list) (parent : Simulant) world =
        if List.forall (fun (entity : Entity) -> entity.GetProtection world = Unprotected) entities then
            snapshot DuplicateEntity world
            let duplicates = duplicateEntities (ImGui.IsShiftUp ()) (Some parent) entities world
            selectEntities false (duplicates |> List.map snd |> Set.ofList) world
            ShowSelectedEntity <- true
        else
            let message = "Cannot relocate a protected simulant (such as an entity created by the ImSim or MMCC API)."
            MessageBoxOpt <- Some message

    let private applyEntityHierarchySelectionRequests (multiSelectIo : ImGuiMultiSelectIOPtr) world =
        let popupOpen = ImGui.IsPopupOpen ("", ImGuiPopupFlags.AnyPopupId)
        if ImGui.IsAltUp () then // alt+up and alt+down reorder entities instead
            let mutable entities = SelectedEntities
            let requests = multiSelectIo.Requests
            for i in 0 .. dec requests.Size do
                let request = requests[i]
                match request.Type with
                | ImGuiSelectionRequestType.SetAll ->
                    if request.Selected then entities <- Set.ofSeq EntityHierarchyIndexed
                    elif not popupOpen then entities <- Set.empty // NOTE: imgui clears as focus leaves for a popup.
                | ImGuiSelectionRequestType.SetRange ->
                    let (first, last) = (int request.RangeFirstItem, int request.RangeLastItem)
                    for index in min first last .. max first last do
                        if index >= 0 && index < EntityHierarchyIndexed.Count then
                            let entity = EntityHierarchyIndexed[index]
                            entities <- if request.Selected then Set.add entity entities else Set.remove entity entities
                | _ -> ()
            if entities <> SelectedEntities then selectEntities false entities world

    let private applyPropagationSourcesSelectionRequests (multiSelectIo : ImGuiMultiSelectIOPtr) world =
        let requests = multiSelectIo.Requests
        for i in 0 .. dec requests.Size do
            let request = requests[i]
            if request.Type = ImGuiSelectionRequestType.SetRange && request.Selected then
                let index = int request.RangeFirstItem // NOTE: a single selection's range is just one item.
                if index >= 0 && index < PropagationSourcesIndexed.Count then
                    selectEntities false (Set.singleton PropagationSourcesIndexed[index]) world

    let private setViewMode viewMode world =
        ignore<World> world // not yet used for anything here
        match (ViewMode, viewMode) with
        | (NormalMode, NormalMode) | (FreeMode, FreeMode) | (CaptureMode, CaptureMode) -> ()
        | (NormalMode, FreeMode) -> ViewMode <- viewMode
        | (FreeMode, NormalMode) -> ViewMode <- viewMode; SelectedWindowRestoreRequested <- 1
        | (NormalMode, CaptureMode) -> ViewMode <- viewMode
        | (CaptureMode, NormalMode) -> ViewMode <- viewMode; SelectedWindowRestoreRequested <- 1
        | (FreeMode, CaptureMode) -> ViewMode <- viewMode
        | (CaptureMode, FreeMode) -> ViewMode <- viewMode

        // focus the viewport so that nothing stays input-focused, including after any window restoration
        SelectedWindowOpt <- Some "Viewport"
        ViewportFocusRequested <- true

    let private toggleViewMode viewMode world =
        if ViewMode = viewMode
        then setViewMode NormalMode world
        else setViewMode viewMode world

    let private toggleOverlayMode () =
        OverlayMode <- not OverlayMode
        ViewportFocusRequested <- true // focus the viewport so that nothing stays input-focused

    let private tryUndo (world : World) =
        if  not (World.getImperative world) &&
            match Pasts with
            | (pastSnapshotType, pastWorldState) :: pasts' ->
                let future = (pastSnapshotType, world.CurrentState)
                World.switch pastWorldState world
                if pastSnapshotType.IsRestorePoint then World.setTimeAdvancing false world
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
            pruneEntitySelection world
            World.setEye2dCenter DesiredEye2dCenter world
            World.setEye3dCenter DesiredEye3dCenter world
            World.setEye3dRotation DesiredEye3dRotation world
            true
        else false

    let private tryRedo world =
        if  not (World.getImperative world) &&
            match Futures with
            | (futureSnapshotType, futureWorldState) :: futures' ->
                let past = (futureSnapshotType, world.CurrentState)
                World.switch futureWorldState world
                if futureSnapshotType.IsRestorePoint then World.setTimeAdvancing false world
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
            pruneEntitySelection world
            World.setEye2dCenter DesiredEye2dCenter world
            World.setEye3dCenter DesiredEye3dCenter world
            World.setEye3dRotation DesiredEye3dRotation world
            true
        else false

    let private searchAssetViewer () =
        ImGui.SetWindowFocus "Asset Viewer"
        AssetViewerSearchRequested <- true

    let private searchEntityHierarchy () =
        ImGui.SetWindowFocus "Entity Hierarchy"
        EntityHierarchySearchRequested <- true

    let private searchPropertyValue () =
        ImGui.SetWindowFocus "Edit Property"
        PropertyValueSearchRequested <- true

    let private makeEditContext focusPropertyOpt unfocusPropertyOpt =
        { Snapshot = snapshot
          FocusProperty = match focusPropertyOpt with Some focus -> focus | None -> fun () -> ()
          UnfocusProperty = match unfocusPropertyOpt with Some unfocus -> unfocus | None -> fun () -> ()
          SearchAssetViewer = fun () -> searchAssetViewer ()
          DragDropPayloadOpt = DragDropPayloadOpt
          SnapDrag = SnapDrag
          SelectedScreen = SelectedScreen
          SelectedGroup = SelectedGroup
          SelectedEntities = SelectedEntities
          ToSymbolMemo = ToSymbolMemo
          OfSymbolMemo = OfSymbolMemo }

    let private detectEyeChangedElsewhere (world : World) =
        if  world.Eye2dCenter <> DesiredEye2dCenter ||
            world.Eye3dCenter <> DesiredEye3dCenter ||
            world.Eye3dRotation <> DesiredEye3dRotation then
            EyeChangedElsewhere <- true

    let private synchronizeLocalDesiredEyeChanges (world : World) =
        if EyeChangedElsewhere then
            DesiredEye2dCenter <- world.Eye2dCenter
            DesiredEye3dCenter <- world.Eye3dCenter
            DesiredEye3dRotation <- world.Eye3dRotation
            EyeChangedElsewhere <- false
        else
            World.setEye2dCenter DesiredEye2dCenter world
            World.setEye3dCenter DesiredEye3dCenter world
            World.setEye3dRotation DesiredEye3dRotation world

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
        |> Seq.filter (fun entity -> entity.Has<LightProbeFacet> world)
        |> Seq.iter (fun lightProbe -> lightProbe.SetProbeStale true world)

    let private tryMoveSelectedEntityToOrigin skipSnapshot world =
        match getSelectedEntityRoots true world with
        | [] -> false
        | entities ->
            if not skipSnapshot then snapshot MoveEntityToOrigin world
            for entity in entities do
                match Option.bind (flip tryResolve entity) (entity.GetMountOpt world) with
                | Some _ -> entity.SetPositionLocal v3Zero world
                | None -> entity.SetPosition v3Zero world
            true

    let private getSnaps () =
        if Snaps2dSelected
        then Snaps2d
        else Snaps3d

    let private getPickCandidates2d world =
        let entities = World.getEntities2dInView (HashSet (QuadelementEqualityComparer ())) world
        let entitiesInGroup =
            entities
            |> Seq.filter (fun entity -> entity.Group = SelectedGroup && entity.Group.GetEditing world)
            |> Seq.toArray
        entitiesInGroup

    let private getPickCandidates3d world =
        let elements = HashSet (OctelementEqualityComparer ())
        World.getEntities3dInView elements world |> ignore<_>
        World.getLightProbesInFrustum world.Eye3dFrustumView elements world |> ignore<_>
        World.getLights3dInFrustum world.Eye3dFrustumView elements world |> ignore<_>
        let entitiesInGroup =
            elements
            |> Seq.map (fun element -> element.Entry)
            |> Seq.filter (fun entity -> entity.Group = SelectedGroup && entity.Group.GetEditing world)
            |> Seq.toArray
        entitiesInGroup

    let private tryMousePick mousePosition world =
        let entities2d = getPickCandidates2d world
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d world
        match pickedOpt with
        | Some entity -> Some (0.0f, entity)
        | None ->
            let entities3d = getPickCandidates3d world
            World.tryPickEntity3d mousePosition entities3d world

    let private mousePickAll mousePosition world =
        let entities2d = World.pickEntities2d mousePosition (getPickCandidates2d world) world
        if Array.notEmpty entities2d then entities2d
        else World.pickEntities3d mousePosition (getPickCandidates3d world) world |> Array.map snd

    let private tryPickName names =
        let io = ImGui.GetIO ()
        io.SwallowKeyboard ()
        [|for key in int ImGuiKey.A .. int ImGuiKey.Z do
            if ImGui.IsKeyReleased (enum key) then
                let chr = char (key - int ImGuiKey.A + 97)
                names
                |> Seq.filter (fun (name : string) -> name.Length > 0 && Char.ToLowerInvariant name[0] = chr)
                |> Seq.tryHead|]
        |> Array.definitize
        |> Array.tryHead

    let private searchPropertyValueStr forward (propertyValueStr : string) =
        if PropertyValueSearchStr.Length > 0 then
            let comparison = StringComparison.OrdinalIgnoreCase
            let index =
                if forward then
                    let startIndex = min (PropertyValueSearchIndex + 1) propertyValueStr.Length
                    match propertyValueStr.IndexOf (PropertyValueSearchStr, startIndex, comparison) with
                    | -1 -> propertyValueStr.IndexOf (PropertyValueSearchStr, comparison)
                    | index -> index
                else
                    let index =
                        if PropertyValueSearchIndex > 0
                        then propertyValueStr.LastIndexOf (PropertyValueSearchStr, PropertyValueSearchIndex - 1, comparison)
                        else -1
                    if index = -1 then propertyValueStr.LastIndexOf (PropertyValueSearchStr, comparison) else index
            PropertyValueSearchIndex <- index
            if index >= 0 then
                let selectionStart = Encoding.UTF8.GetByteCount (propertyValueStr.AsSpan (0, index))
                let selectionLength = Encoding.UTF8.GetByteCount (propertyValueStr.AsSpan (index, PropertyValueSearchStr.Length))
                PropertyValueSelectionRequestOpt <- Some (selectionStart, selectionStart + selectionLength)

    let private countPropertyValueSearchMatches (propertyValueStr : string) =
        let mutable count = 0
        if PropertyValueSearchStr.Length > 0 then
            let mutable index = propertyValueStr.IndexOf (PropertyValueSearchStr, StringComparison.OrdinalIgnoreCase)
            while index >= 0 do
                count <- inc count
                index <- propertyValueStr.IndexOf (PropertyValueSearchStr, index + PropertyValueSearchStr.Length, StringComparison.OrdinalIgnoreCase)
        count

    let private revertOpenProjectState (world : World) =
        OpenProjectDllsOpt <- None
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

    (* Nu Event Handling Functions *)

    let private handleNuMouseButton (_ : Event<MouseButtonData, Game>) world =
        if shouldSwallowMouseButton world then Resolve else Cascade

    let private handleNuLifeCycleGroup (evt : Event<LifeCycleEventData, Game>) world =

        // handle tricky bits of group unregistration that occur due to selection state
        match evt.Data with
        | UnregisteringData simulant ->

            // forget the group's baseline for change detection
            let group = simulant :?> Group
            GroupWorldStates <- Map.remove group GroupWorldStates
            EntityWorldStatesReverted <- Map.filter (fun entity _ -> entity.Group <> group) EntityWorldStatesReverted

            // when currently the selected group, make sure we select another group, creating a default "Scene" group
            // when necessary
            if SelectedGroup :> Simulant = simulant then

                // locate other selectable groups in the same screen
                let selectables =
                    world
                    |> World.getGroups SelectedScreen
                    |> Seq.filter (fun group -> group :> Simulant <> simulant)
                if Seq.isEmpty selectables then

                    // when no group remains and the one being unregistered is not the default "Scene" group that will
                    // be automatically created elsewhere, create the default "Scene" group
                    if simulant.Name <> "Scene" then
                        let group = createSceneGroup SelectedScreen world 
                        SelectedGroup <- group

                    // otherwise leave SelectedGroup as-is since it will be recreated elsewhere
                    else ()

                // select the first selectable group
                else SelectedGroup <- Seq.head selectables

            // otherwise when the selected entity is a child of the unregistering group, deselect it
            elif (match simulant with :? Entity as entity -> SelectedEntities.Contains entity | _ -> false) then
                SelectedEntities <- Set.remove (simulant :?> Entity) SelectedEntities

        // otherwise nothing to do
        | _ -> ()
        Cascade

    let private handleNuSelectedScreenOptChange (evt : Event<Screen option, Game>) world =
        match evt.Data with
        | Some screen ->
            selectScreen true screen
            selectGroupInitial screen world
            selectEntities true Set.empty world
            focusPropertyOpt None world
            Cascade
        | None -> Cascade // just keep current group selection and screen if no screen selected

    let private handleNuExitRequest _ (_ : World) =
        if not (modal ()) then ShowConfirmExitDialog <- true
        Resolve

    (* Editor Command Functions *)

    let private createRestorePoint world =
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Default.Sound world
        snapshot RestorePoint world

    let private inductEntity atMouse (entity : Entity) (world : World)=
        let (positionSnap, _, _) = getSnaps ()
        let mutable entityTransform = entity.GetTransform world
        if entity.GetIs2d world then
            let absolute = entity.GetAbsolute world
            let entityPosition =
                if atMouse then Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize RightClickPosition world.WindowViewport
                elif not absolute then world.Eye2dCenter
                else v2Zero
            entityTransform.Position <- entityPosition.V3
            if entity.Surnames.Length = 1 then
                entityTransform.Elevation <- NewEntityElevation
            let attributes = entity.GetAttributesInferred world
            if not attributes.Unimportant then
                entityTransform.Size <- attributes.SizeInferred
                entityTransform.Offset <- attributes.OffsetInferred
            if Snaps2dSelected && ImGui.IsCtrlUp () then Transform.snapPosition (positionSnap, &entityTransform)
        else
            let eyeCenter = world.Eye3dCenter
            let eyeRotation = world.Eye3dRotation
            let eyeFieldOfView = world.Eye3dFieldOfView
            let entityPosition =
                if atMouse then
                    let ray = Viewport.mouseToWorld3d eyeCenter eyeRotation eyeFieldOfView RightClickPosition world.WindowViewport
                    let forward = eyeRotation.Forward
                    let plane = plane3 (eyeCenter + forward * NewEntityDistance) -forward
                    (ray.Intersection plane).Value
                else eyeCenter + v3Forward.Transform eyeRotation * NewEntityDistance
            entityTransform.Position <- entityPosition
            let attributes = entity.GetAttributesInferred world
            if not attributes.Unimportant then
                entityTransform.Size <- attributes.SizeInferred
                entityTransform.Offset <- attributes.OffsetInferred
            if not Snaps2dSelected && ImGui.IsCtrlUp () then Transform.snapPosition (positionSnap, &entityTransform)
        entity.SetPosition entityTransform.Position world
        entity.SetElevation entityTransform.Elevation world
        entity.SetSize entityTransform.Size world
        entity.SetOffset entityTransform.Offset world
        if entity.Surnames.Length > 1 then
            if World.getEntityAllowedToMount entity world then
                entity.SetMountOptWithAdjustment true (Some Address.parent) world
        match entity.TryGetProperty (nameof entity.ProbeBounds) world with
        | Some property when property.PropertyType = typeof<Box3> ->
            entity.ResetProbeBounds world
        | Some _ | None -> ()

    let private createEntity atMouse inHierarchy dispatcherNameOverride (parentOverride : Simulant option) world =
        snapshot CreateEntity world
        let dispatcherName = Option.defaultValue NewEntityDispatcherName dispatcherNameOverride
        let overlayNameDescriptor =
            match NewEntityOverlayName with
            | "(Default Overlay)" -> DefaultOverlay
            | "(Routed Overlay)" -> RoutedOverlay
            | "(No Overlay)" -> NoOverlay
            | overlayName -> ExplicitOverlay overlayName
        let name = World.generateEntitySequentialName dispatcherName SelectedGroup world
        let parentOpt =
            if inHierarchy then Option.map cast<Simulant> (Seq.tryExactlyOne SelectedEntities)
            else Option.orElse (Option.map cast<Simulant> NewEntityParentOpt) parentOverride
        let surnames =
            match parentOpt with
            | Some (:? Entity as parent) when parent.GetExists world -> Array.add name parent.Surnames
            | Some _ | None -> [|name|] // a group parent, or none, places it at the top
        let entity = World.createEntity7 false dispatcherName (Some Address.parent) overlayNameDescriptor (Some surnames) SelectedGroup world
        inductEntity atMouse entity world
        selectEntities true (Set.singleton entity) world
        ImGui.SetWindowFocus "Viewport"
        ShowSelectedEntity <- true
        entity

    let private tryGetAssetEntityDispatcher (assetTag : AssetTag) =
        match Metadata.tryGetMetadata assetTag with
        | ValueSome metadata ->
            match metadata with
            | TextureMetadata _ -> Some (nameof StaticSpriteDispatcher, fun (entity : Entity) world -> entity.SetStaticImage (AssetTag.specialize assetTag) world)
            | TileMapMetadata _ -> Some (nameof TileMapDispatcher, fun (entity : Entity) world -> entity.SetTileMap (AssetTag.specialize assetTag) world)
            | StaticModelMetadata _ -> Some (nameof StaticModelDispatcher, fun (entity : Entity) world -> entity.SetStaticModel (AssetTag.specialize assetTag) world)
            | AnimatedModelMetadata _ -> Some (nameof AnimatedModelDispatcher, fun (entity : Entity) world -> entity.SetAnimatedModel (AssetTag.specialize assetTag) world)
            | RawMetadata | SoundMetadata | SongMetadata -> None
        | ValueNone -> None

    let private tryCreateEntityOfAsset atMouse parentOverride assetTag world =
        match tryGetAssetEntityDispatcher assetTag with
        | Some (dispatcherName, giveAsset) ->
            let entity = createEntity atMouse false (Some dispatcherName) parentOverride world
            giveAsset entity world
            entity.AutoBounds world
            Some entity
        | None -> None

    let private createEntityOfPropagationSource pasteType (parentOverride : Simulant option) (source : Entity) world =
        snapshot DuplicateEntity world
        let parent =
            match parentOverride with
            | Some parent -> parent
            | None -> NewEntityParentOpt |> Option.map cast<Simulant> |> Option.defaultValue source.Group
        let positionSnapEir = if Snaps2dSelected then Left (a__ Snaps2d) else Right (a__ Snaps3d)
        let entity = World.pasteEntity NewEntityDistance RightClickPosition positionSnapEir pasteType source parent world
        selectEntities true (Set.singleton entity) world
        ShowSelectedEntity <- true
        entity

    let private trySaveSelectedEntity filePath world =
        match Seq.tryExactlyOne SelectedEntities with
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
                    match Seq.tryExactlyOne SelectedEntities with
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
            try if not (entity.GetExists world) || entity.GetProtection world = Unprotected then
                    if entity.GetExists world then
                        snapshot LoadEntity world
                        let order = entity.GetOrder world
                        let mountOpt = entity.GetMountOpt world
                        let position = entity.GetPosition world
                        let rotation = entity.GetRotation world
                        let elevation = entity.GetElevation world
                        let propagatedDescriptorOpt = entity.GetPropagatedDescriptorOpt world
                        let entityProperties = Map.add Constants.Engine.MountOptPropertyName (valueToSymbol mountOpt) entityDescriptor.EntityProperties // NOTE: keeping the existing entity's mount rather than reading it from the file.
                        let entityDescriptor = { entityDescriptor with EntityProperties = entityProperties }
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
                    SelectedEntities <- Set.singleton entity
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

    let private deleteSelectedEntities wipePropagationTargets world =
        snapshot DeleteEntity world
        for entity in getSelectedEntityRoots false world do
            if wipePropagationTargets then World.clearPropagationTargets entity world
            World.destroyEntity entity world
        selectEntities true Set.empty world

    let private tryDeleteSelectedEntity world =
        match getSelectedEntityRoots false world with
        | [] -> false
        | entities ->
            if getSelectedEntitiesUnprotected world then
                if List.exists (fun (entity : Entity) -> entity.HasPropagationTargets world) entities then
                    ShowDeleteEntityDialog <- true
                    false
                else
                    deleteSelectedEntities false world
                    true
            else
                MessageBoxOpt <- Some "Cannot destroy a protected simulant (such as an entity created by the ImSim or MMCC API)."
                false

    let private tryAutoBoundsSelectedEntity world =
        match getSelectedEntityRoots false world with
        | [] -> false
        | entities ->
            snapshot AutoBoundsEntity world
            for entity in entities do entity.AutoBounds world
            true

    let private propagateEntityStructure entity world =
        snapshot PropagateEntity world
        World.propagateEntityStructure entity world

    let private tryPropagateSelectedEntityStructure world =
        match getSelectedEntityRoots false world with
        | [] -> false
        | entities ->
            snapshot PropagateEntity world
            for entity in entities do World.propagateEntityStructure entity world
            true

    let private tryWipeSelectedEntityPropagationTargets world =
        match getSelectedEntityRoots false world with
        | [] -> false
        | entities ->
            snapshot WipePropagationTargets world
            for entity in entities do
                entity.SetPropagatedDescriptorOpt None world
                World.clearPropagationTargets entity world
            true

    let private tryReorderSelectedEntity up world =
        if String.IsNullOrWhiteSpace EntityHierarchySearchStr then
            match Seq.tryExactlyOne SelectedEntities with
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

    let private cutSelectedEntities wipePropagationTargets world =
        snapshot CutEntity world
        let entities = getSelectedEntityRoots false world
        if wipePropagationTargets then
            for entity in entities do World.clearPropagationTargets entity world
        selectEntities true Set.empty world
        World.cutEntitiesToClipboard entities world

    let private tryCutSelectedEntity world =
        match getSelectedEntityRoots false world with
        | [] -> false
        | entities ->
            if getSelectedEntitiesUnprotected world then
                if List.exists (fun (entity : Entity) -> entity.HasPropagationTargets world) entities then
                    ShowCutEntityDialog <- true
                    false
                else
                    cutSelectedEntities false world
                    true
            else
                MessageBoxOpt <- Some "Cannot cut a protected simulant (such as an entity created by the ImSim or MMCC API)."
                false

    let private tryCopySelectedEntity world =
        match getSelectedEntityRoots false world with
        | [] -> false
        | entities ->
            if getSelectedEntitiesUnprotected world then
                World.copyEntitiesToClipboard entities world
                true
            else
                MessageBoxOpt <- Some "Cannot copy a protected simulant (such as an entity created by the ImSim or MMCC API)."
                false

    let private tryPaste atMouse parentOpt world =
        if World.canPasteEntityFromClipboard world then
            snapshot PasteEntity world
            let positionSnapEir = if Snaps2dSelected then Left (a__ Snaps2d) else Right (a__ Snaps3d)
            let parent = match parentOpt with Some parent -> parent | None -> SelectedGroup :> Simulant
            let entities = World.pasteEntitiesFromClipboard NewEntityDistance RightClickPosition positionSnapEir atMouse parent world
            match entities with
            | _ :: _ ->
                selectEntities true (Set.ofList entities) world
                ImGui.SetWindowFocus "Viewport"
                ShowSelectedEntity <- true
                true
            | [] -> false
        else false

    let private trySetSelectedEntityFamilyStatic static_ world =
        let rec setEntityFamilyStatic static_ (entity : Entity) world =
            entity.SetStatic static_ world
            for child in entity.GetChildren world do
                setEntityFamilyStatic static_ child world
        match getSelectedEntityRoots false world with
        | [] -> ()
        | entities ->
            snapshot (SetEntityFamilyStatic static_) world
            for entity in entities do setEntityFamilyStatic static_ entity world

    let private trySaveSelectedGroup filePath world =
        try World.writeGroupToFile filePath SelectedGroup world
            GroupWorldStates <- Map.add SelectedGroup world.CurrentState GroupWorldStates
            EntityWorldStatesReverted <- Map.filter (fun entity _ -> entity.Group <> SelectedGroup) EntityWorldStatesReverted
            try let deploymentPath = PathF.Combine (TargetDir, PathF.GetRelativePath(TargetDir, filePath).Replace("../", ""))
                if Directory.Exists (PathF.GetDirectoryName deploymentPath) then
                    if File.Exists deploymentPath then File.SetAttributes (deploymentPath, FileAttributes.None)
                    File.Copy (filePath, deploymentPath, true)
                    File.SetAttributes (deploymentPath, FileAttributes.ReadOnly)
            with exn -> MessageBoxOpt <- Some ("Could not deploy file due to: " + scstring exn)
            GroupFilePaths <- Map.add SelectedGroup GroupFileDialogState.FilePath GroupFilePaths
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
            try if not (group.GetExists world) || group.GetProtection world = Unprotected then
                    if group.GetExists world then World.destroyGroupImmediate SelectedGroup world
                    let group = World.readGroup groupDescriptor None SelectedScreen world
                    GroupWorldStates <- Map.add group world.CurrentState GroupWorldStates
                    selectGroup true group
                    pruneEntitySelection world
                    GroupFilePaths <- Map.add group GroupFileDialogState.FilePath GroupFilePaths
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

    let private getEntityModified (entity : Entity) world =
        if entity.GetProtection world <> DeclarativeProtection then // NOTE: a declared entity isn't saved, so isn't tracked.
            match Map.tryFind entity.Group GroupWorldStates with
            | Some worldState ->
                World.getEntityModified entity worldState world &&
                (match Map.tryFind entity EntityWorldStatesReverted with // NOTE: as just after a revert is unmodified.
                 | Some worldStates -> List.forall (fun worldState -> World.getEntityModified entity worldState world) worldStates
                 | None -> true)
            | None -> false
        else false

    let private getEntityRevertible (entity : Entity) world =
        entity.GetExists world &&
        entity.GetProtection world = Unprotected &&
        (match Map.tryFind entity.Group GroupWorldStates with
         | Some worldState -> WorldState.getEntityExists entity worldState
         | None -> false) &&
        getEntityModified entity world

    let private tryRevertSelectedEntity world =
        match Seq.tryExactlyOne SelectedEntities with
        | Some entity when getEntityRevertible entity world ->
            let worldStateOld = world.CurrentState
            try snapshot RevertEntity world
                World.revertEntity entity GroupWorldStates[entity.Group] world
                for entity in Seq.append [entity] (entity.GetDescendants world) do
                    let worldStates = Option.defaultValue [] (Map.tryFind entity EntityWorldStatesReverted)
                    EntityWorldStatesReverted <- Map.add entity (world.CurrentState :: worldStates) EntityWorldStatesReverted
                selectEntities true (Set.singleton entity) world
                true
            with exn ->
                MessageBoxOpt <- Some ("Could not revert entity due to: " + scstring exn)
                World.switch worldStateOld world
                false
        | Some _ | None -> false

    let private tryReloadAssets world =
        let assetSourceDir = TargetDir + "/../../.."
        let refinementDir = Constants.Engine.RefinementDir
        let blockCompression = Constants.Render.TextureBlockCompression
        match World.tryReloadAssetGraph assetSourceDir TargetDir refinementDir blockCompression world with
        | Right assetGraph ->
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            AssetGraphStr <- PrettyPrinter.prettyPrint (scstring assetGraph) prettyPrinter
        | Left error ->
            MessageBoxOpt <- Some ("Asset reload error due to: " + error + "'.")
        match World.tryReloadOverlayer assetSourceDir TargetDir world with
        | Right overlayer ->
            let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
            OverlayerStr <- PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
        | Left error ->
            MessageBoxOpt <- Some ("Overlayer reload error due to: " + error + "'.")

    let private noteFsiSessionLateBindingTypes () =
        let mutable noted = false
        for assembly in FsiSession.DynamicAssemblies do
            for ty in assembly.GetTypes () do
                if  (ty.IsSubclassOf typeof<Facet> ||
                     ty.IsSubclassOf typeof<EntityDispatcher> ||
                     ty.IsSubclassOf typeof<GroupDispatcher> ||
                     ty.IsSubclassOf typeof<ScreenDispatcher> ||
                     ty.IsSubclassOf typeof<GameDispatcher>) &&
                    not ty.IsAbstract &&
                    FsiSessionLateBindingTypes.Add ty then
                    noted <- true
        noted

    let private tryReloadCode initializing world =
        if World.getAllowCodeReload world then
            let worldStateOld = world.CurrentState
            snapshot ReloadCode world
            selectEntities true Set.empty world // NOTE: makes sure old dispatcher doesn't hang around in old cached entity state.
            let workingDirPath = TargetDir + "/../../.."
            Log.info ("Inspecting directory " + workingDirPath + " for F# code...")
            try match Array.ofSeq (Directory.EnumerateFiles (workingDirPath, "*.fsproj")) with
                | [||] -> Log.error ("Unable to find fsproj file in '" + workingDirPath + "'.")
                | fsprojFilePaths ->

                    // generate code reload fsx file string
                    // TODO: P1: consider rewriting this code to use the XML representation to ensure more reliable parsing.
                    let fsprojFilePath = fsprojFilePaths[0]
                    Log.info ("Inspecting code for F# project '" + fsprojFilePath + "'...")
                    let fsprojFileLines = // TODO: P1: consider loading these references from Nu.fsproj.
                        [|"""<PackageReference Include="Aether.Physics2D" Version="2.2.0" />"""
                          """<PackageReference Include="AstcEncoderCSharp" Version="5.5.0" />"""
                          """<PackageReference Include="Box2D.NET" Version="3.1.654" />"""
                          """<PackageReference Include="BCnEncoder.Net" Version="2.2.1" />"""
                          """<PackageReference Include="DotRecast.Recast.Toolset" Version="2026.1.1" />"""
                          """<PackageReference Include="JoltPhysicsSharp" Version="2.24.0" />"""
                          """<PackageReference Include="Magick.NET-Q8-AnyCPU" Version="15.1.0" />"""
                          """<PackageReference Include="Pfim" Version="0.11.4" />"""
                          """<PackageReference Include="Prime" Version="11.5.5" />"""
                          """<PackageReference Include="System.Configuration.ConfigurationManager" Version="11.0.0" />"""
                          """<PackageReference Include="System.Drawing.Common" Version="11.0.0" />"""
                          """<PackageReference Include="Twizzle.ImGui-Bundle.NET" Version="1.91.5.2" />"""
                          """<PackageReference Include="ppy.SDL3-CS" Version="2026.722.0" />"""
                          """<PackageReference Include="ppy.SDL3_ttf-CS" Version="2026.722.0" />"""
                          """<PackageReference Include="ppy.SDL3_image-CS" Version="2026.722.0" />"""
                          """<PackageReference Include="ppy.SDL3_mixer-CS" Version="2026.722.0" />"""
                          """<PackageReference Include="Vortice.ShaderCompiler" Version="1.9.0" />"""
                          """<PackageReference Include="Vortice.Vulkan" Version="3.2.3" />"""
                          """<PackageReference Include="Vortice.VulkanMemoryAllocator" Version="1.7.0" />"""|]
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
                    let fsprojDllFilePaths = // TODO: see if we can pull these from the fsproj as well...
                        [|"../Nu.Dependencies/AssimpNet/netstandard2.1/AssimpNet.dll"
                          "../Nu.Dependencies/BulletSharpPInvoke/netstandard2.1/BulletSharp.dll"
                          "../Nu.Dependencies/TiledSharp/lib/netstandard2.0/TiledSharp.dll"|]
                    let fsprojProjectLines = // TODO: see if we can pull these from the fsproj as well...
                        [|"#r \"../../../../../Nu/Nu.Math/bin/" + Constants.Engine.BuildName + "/" + Constants.Engine.TargetFramework + "/Nu.Math.dll\""
                          "#r \"../../../../../Nu/Nu.Pipe/bin/" + Constants.Engine.BuildName + "/" + Constants.Engine.TargetFramework + "/Nu.Pipe.dll\""
                          "#r \"../../../../../Nu/Nu/bin/" + Constants.Engine.BuildName + "/" + Constants.Engine.TargetFramework + "/Nu.dll\""|]
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

                    // the new session won't contain project code until the reload script below succeeds
                    FsiSessionHasProjectCode <- false

                    // clear the late binding types so that we can note them again after the reload script below succeeds
                    FsiSessionLateBindingTypes.Clear ()

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
                        World.updateLateBindings initializing FsiSession.DynamicAssemblies world // replace references to old types
                        noteFsiSessionLateBindingTypes () |> ignore<bool>
                        FsiSessionHasProjectCode <- true
                        Log.info "Code updated."
                    | (Choice2Of2 exn, diags) ->
                        if diags.Length > 0 then
                            let diagsStr =
                                diags
                                |> Array.map (fun diag ->
                                    let range = diag.Range
                                    let start = range.Start
                                    let end_ = range.End
                                    let debugCodeProperty = typeof<FSharp.Compiler.Text.Range>.GetProperty ("DebugCode", BindingFlags.NonPublic ||| BindingFlags.Instance)
                                    let debugCodeOpt = if notNull debugCodeProperty then debugCodeProperty.GetValue range :?> string else null
                                    let debugCode = if String.IsNullOrEmpty debugCodeOpt then "<unavailable>" else debugCodeOpt
                                    let diagStr =
                                        string diag.Severity + " " + diag.ErrorNumberText + " in " +
                                        diag.FileName + " " + string start + "-" + string end_ + "\n" +
                                        debugCode + " -> " + diag.Message
                                    diagStr)
                                |> String.join Environment.NewLine
                            Log.error ("Failed to compile code due to:\n" + diagsStr)
                        else Log.error ("Failed to compile code due to:\n" + scstring exn)
                        World.switch worldStateOld world
                    FsiErrorStream.GetStringBuilder().Clear() |> ignore<StringBuilder>
                    FsiOutStream.GetStringBuilder().Clear() |> ignore<StringBuilder>

                    // issue code reload event
                    World.publishPlus () Nu.Game.Handle.CodeReloadEvent (EventTrace.debug "Gaia" "tryReloadCode" "" EventTrace.empty) Nu.Game.Handle false false world

            with exn ->
                Log.error ("Failed to inspect for F# code due to: " + scstring exn)
                World.switch worldStateOld world

        else MessageBoxOpt <- Some "Code reloading not allowed by current plugin. This is likely because you're using the GaiaPlugin which doesn't allow it."

    let private tryReloadAll initializing world =
        tryReloadAssets world
        tryReloadCode initializing world

    let private toggleTimeAdvancing (world : World) =
        let wasTimeAdvancing = world.TimeAdvancing
        snapshot (if wasTimeAdvancing then Halt else Advance) world
        World.setTimeAdvancing (not world.TimeAdvancing) world

    let private step (world : World) =
        if world.TimeHalted then
            snapshot Step world
            World.setTimeAdvancing true world
            Stepping <- true

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

    let private makeWorld sdlDeps worldConfig windowSize geometryViewport windowViewport (plugin : NuPlugin) =

        // make the edit context maker
        let tryMakeEditContext = fun () -> Some (makeEditContext None None)

        // make the world
        let world = World.make tryMakeEditContext sdlDeps worldConfig windowSize geometryViewport windowViewport plugin

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
            match Game.GetSelectedScreenOpt world with
            | None ->
                let screen = Game / "Screen"
                if not (screen.GetExists world) then
                    let screen = World.createScreen (Some "Screen") world
                    World.setSelectedScreen screen world
                    let eventTrace = EventTrace.debug "World" "selectScreen" "Select" EventTrace.empty
                    World.publishPlus () screen.SelectEvent eventTrace screen false false world
                    let eventTrace = EventTrace.debug "World" "selectScreen" "PostSelect" EventTrace.empty
                    World.publishPlus (Some screen) Game.PostSelectEvent eventTrace screen false false world
                    screen
                else screen
            | Some screen -> screen

        // proceed directly to idle state
        World.selectScreen (IdlingState world.UnscaledTime) screen world
        (screen, world)

    let private tryMakeSdlDeps accompanied windowSize =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Gaia" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        match SdlDeps.tryMake sdlConfig accompanied windowSize with
        | Left msg -> Left msg
        | Right sdlDeps -> Right (sdlConfig, sdlDeps)

    (* Update Functions *)

    let private getEyeDragPivot world =
        match getSelectedEntityRoots false world |> List.filter (fun entity -> entity.GetIs3d world) with
        | [] -> world.Eye3dCenter + world.Eye3dRotation.Forward * NewEntityDistance
        | entities ->
            let sum = List.fold (fun sum (entity : Entity) -> sum + entity.GetPosition world) v3Zero entities
            sum / single entities.Length

    let private updateEyeDrag world =
        if canEditWithMouse world then
            let middleStarting = // a middle drag restarts as 2d or 3d as alt changes mid-drag
                ImGui.IsMouseClicked ImGuiMouseButton.Middle ||
                match DragEyeState with
                | DragEye2dPan _ -> ImGui.IsAltUp ()
                | DragEye3dPan _ -> ImGui.IsAltDown ()
                | _ -> false
            if middleStarting then
                if ImGui.IsAltDown () then
                    let mousePositionInset = World.getMousePosition2dInset world
                    DragEyeState <- DragEye2dPan mousePositionInset
                else
                    let mousePosition = World.getMousePosition world
                    let pivot = getEyeDragPivot world
                    let distance = max (Vector3.Distance (pivot, world.Eye3dCenter)) Constants.Gaia.Eye3dDragDistanceMinimum
                    DragEyeState <- DragEye3dPan (world.DateTime, distance, mousePosition)
            let rightStartTimeOpt = // a right drag restarts as orbit or free look as alt changes mid-drag, keeping its time
                match DragEyeState with
                | DragEye3dOrbit (time, _, _, _, _, _) when ImGui.IsAltUp () -> Some time
                | DragEye3dFreeLook (time, _, _, _) when ImGui.IsAltDown () -> Some time
                | _ -> if ImGui.IsMouseClicked ImGuiMouseButton.Right then Some world.DateTime else None
            match rightStartTimeOpt with
            | Some time ->
                let mousePosition = World.getMousePosition world
                let forward = world.Eye3dRotation.Forward
                let yaw = MathF.Atan2 (-forward.X, -forward.Z)
                let pitch = Math.Clamp (MathF.Asin (Math.Clamp (forward.Y, -1.0f, 1.0f)), -Constants.Gaia.Eye3dPitchMaximum, Constants.Gaia.Eye3dPitchMaximum)
                DragEyeState <-
                    if ImGui.IsAltDown ()
                    then DragEye3dOrbit (time, world.Eye3dCenter, getEyeDragPivot world, yaw, pitch, mousePosition)
                    else DragEye3dFreeLook (time, yaw, pitch, mousePosition)
            | None -> ()
            match DragEyeState with
            | DragEye2dPan mousePositionInsetPrevious ->
                let mousePositionInset = World.getMousePosition2dInset world
                let speed =
                    if ImGui.IsEnterDown () || ImGui.IsCtrlDown () then Constants.Gaia.Eye2dPanSpeedFast
                    elif ImGui.IsShiftDown () then Constants.Gaia.Eye2dPanSpeedSlow
                    else Constants.Gaia.Eye2dPanSpeed
                DesiredEye2dCenter <- DesiredEye2dCenter - (mousePositionInset - mousePositionInsetPrevious) * speed // by frame so the speed can change mid-drag
                DragEyeState <- DragEye2dPan mousePositionInset
            | DragEye3dPan (time, distance, mousePositionPrevious) ->
                let mousePosition = World.getMousePosition world
                let mouseDelta = mousePosition - mousePositionPrevious // by frame so the speed can change mid-drag
                let speed =
                    if ImGui.IsEnterDown () || ImGui.IsCtrlDown () then Constants.Gaia.Eye3dPanSpeedFast
                    elif ImGui.IsShiftDown () then Constants.Gaia.Eye3dPanSpeedSlow
                    else Constants.Gaia.Eye3dPanSpeed
                let unitsPerPixel = 2.0f * distance * tan (world.Eye3dFieldOfView * 0.5f) / single world.WindowViewport.Bounds.Size.Y // the world under the mouse at the pivot's distance
                let rotation = world.Eye3dRotation
                DesiredEye3dCenter <- DesiredEye3dCenter + (rotation.Right * -mouseDelta.X + rotation.Up * mouseDelta.Y) * unitsPerPixel * speed
                DragEyeState <- DragEye3dPan (time, distance, mousePosition)
            | DragEye3dOrbit (time, center, pivot, yaw, pitch, mousePositionOrig) ->
                if (world.DateTime - time).TotalSeconds >= Constants.Gaia.DragMinimumSeconds then // a right click held only briefly opens the context menu instead
                    let mouseDelta = World.getMousePosition world - mousePositionOrig
                    let yaw' = yaw - mouseDelta.X * Constants.Gaia.Eye3dOrbitSpeed
                    let pitch' = Math.Clamp (pitch - mouseDelta.Y * Constants.Gaia.Eye3dOrbitSpeed, -Constants.Gaia.Eye3dPitchMaximum, Constants.Gaia.Eye3dPitchMaximum) // never upside-down
                    let rotation = Quaternion.CreateFromAxisAngle (v3Up, yaw) * Quaternion.CreateFromAxisAngle (v3Right, pitch)
                    let rotation' = Quaternion.CreateFromAxisAngle (v3Up, yaw') * Quaternion.CreateFromAxisAngle (v3Right, pitch')
                    DesiredEye3dRotation <- rotation'
                    DesiredEye3dCenter <- pivot + (center - pivot).Transform (rotation' * Quaternion.Inverse rotation)
            | DragEye3dFreeLook (time, yaw, pitch, mousePositionOrig) ->
                if (world.DateTime - time).TotalSeconds >= Constants.Gaia.DragMinimumSeconds then // a right click held only briefly opens the context menu instead
                    let mouseDelta = World.getMousePosition world - mousePositionOrig
                    let yaw' = yaw - mouseDelta.X * Constants.Gaia.Eye3dOrbitSpeed
                    let pitch' = Math.Clamp (pitch - mouseDelta.Y * Constants.Gaia.Eye3dOrbitSpeed, -Constants.Gaia.Eye3dPitchMaximum, Constants.Gaia.Eye3dPitchMaximum) // never upside-down
                    DesiredEye3dRotation <- Quaternion.CreateFromAxisAngle (v3Up, yaw') * Quaternion.CreateFromAxisAngle (v3Right, pitch')
            | DragEyeInactive ->
                ()
            let scroll = ImGui.GetIO().MouseWheel
            match DragEyeState with
            | DragEyeInactive when scroll <> 0.0f ->
                let travel =
                    if ImGui.IsEnterDown () || ImGui.IsCtrlDown () then Constants.Gaia.Eye3dScrollTravelFast
                    elif ImGui.IsShiftDown () then Constants.Gaia.Eye3dScrollTravelSlow
                    else Constants.Gaia.Eye3dScrollTravel
                DesiredEye3dCenter <- world.Eye3dCenter + world.Eye3dRotation.Forward * scroll * travel
            | _ -> ()
        match DragEyeState with
        | DragEye2dPan _ | DragEye3dPan _ when ImGui.IsMouseReleased ImGuiMouseButton.Middle ->
            DragEyeState <- DragEyeInactive
        | DragEye3dOrbit _ | DragEye3dFreeLook _ when ImGui.IsMouseReleased ImGuiMouseButton.Right ->
            DragEyeState <- DragEyeInactive
        | _ -> ()

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
            if ImGui.IsKeyDown ImGuiKey.W && ImGui.IsCtrlUp () && ImGui.IsAltUp () then
                DesiredEye3dCenter <- center + v3Forward.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.S && ImGui.IsCtrlUp () && ImGui.IsAltUp () then
                DesiredEye3dCenter <- center + v3Back.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.A && ImGui.IsCtrlUp () && ImGui.IsAltUp () then
                DesiredEye3dCenter <- center + v3Left.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.D && ImGui.IsCtrlUp () && ImGui.IsAltUp () then
                DesiredEye3dCenter <- center + v3Right.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.UpArrow && ImGui.IsCtrlUp () then
                let rotation' = rotation * Quaternion.CreateFromAxisAngle (v3Right, turnSpeed)
                DesiredEye3dRotation <-
                    if rotation'.Forward.Dot v3Up < 0.99f then rotation'
                    else
                        Quaternion.CreateFromAxisAngle (v3Down, 2.0f * MathF.Atan2(rotation.Z, rotation.W)) *
                        Quaternion.CreateFromAxisAngle (v3Right, MathF.PI_OVER_2)
            if ImGui.IsKeyDown ImGuiKey.DownArrow && ImGui.IsCtrlUp () then
                let rotation' = rotation * Quaternion.CreateFromAxisAngle (v3Left, turnSpeed)
                DesiredEye3dRotation <-
                    if rotation'.Forward.Dot v3Down < 0.99f then rotation'
                    else
                        Quaternion.CreateFromAxisAngle (v3Up, 2.0f * MathF.Atan2(rotation.Z, rotation.W)) *
                        Quaternion.CreateFromAxisAngle (v3Right, -MathF.PI_OVER_2)
            if ImGui.IsKeyDown ImGuiKey.E && ImGui.IsAltUp () then
                DesiredEye3dCenter <- center + v3Up.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.Q && ImGui.IsAltUp () then
                DesiredEye3dCenter <- center + v3Down.Transform rotation * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.LeftArrow && ImGui.IsAltUp () then
                DesiredEye3dRotation <- Quaternion.CreateFromAxisAngle (v3Up, turnSpeed) * rotation
            if ImGui.IsKeyDown ImGuiKey.RightArrow && ImGui.IsAltUp () then
                DesiredEye3dRotation <- Quaternion.CreateFromAxisAngle (v3Down, turnSpeed) * rotation

    let private updateEntityContext world =
        if canEditWithMouse world then
            let eyeDragged = // a right drag that's panned the 2d eye opens no context menu as it ends
                match DragEyeState with
                | DragEye3dPan (time, _, _)
                | DragEye3dOrbit (time, _, _, _, _, _)
                | DragEye3dFreeLook (time, _, _, _) -> (world.DateTime - time).TotalSeconds >= Constants.Gaia.DragMinimumSeconds
                | _ -> false
            if ImGui.IsMouseReleased ImGuiMouseButton.Right && not eyeDragged then
                let mousePosition = World.getMousePosition world
                match tryMousePick mousePosition world with
                | Some (_, entity) -> selectEntityUnlessSelected true entity world
                | None -> ()
                RightClickPosition <- mousePosition
                ShowEntityContextMenu <- true

    let private updateEntityDrag world =

        // attempt to start dragging an entity with the mouse
        if canEditWithMouse world then

            // attempt to start dragging, unless the eye's dragged, or to restart a drag as moving or rotating as alt
            // changes mid-drag, keeping its time and snapshot so it's still a single undo
            let dragStartOpt =
                if ImGui.IsMouseClicked ImGuiMouseButton.Left then
                    let mousePosition = World.getMousePosition world
                    match tryMousePick mousePosition world with
                    | Some (_, entity) when ImGui.IsShiftDown () && ImGui.IsCtrlUp () && ImGui.IsAltUp () ->
                        toggleEntitySelection entity world // shift+click adds to or takes from the selection
                        None
                    | Some (_, entity) when ImGui.IsCtrlDown () && ImGui.IsAltUp () ->
                        if entity.GetIs2d world then // ctrl+click adds to or takes from the selection, ctrl+drag duplicates it
                            let absolute = entity.GetAbsolute world
                            let mousePositionWorld = Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize mousePosition world.WindowViewport
                            DragEntityState <- DragEntityDuplicate2d (world.DateTime, mousePositionWorld, entity)
                        else toggleEntitySelection entity world
                        None
                    | Some (_, entity) ->

                        // when the lone selected entity is under the mouse, even behind others, clicking it acts on it
                        // and selects the entity behind it on release unless dragged, cycling through overlapping entities
                        let picks = mousePickAll mousePosition world
                        let selectedIndexOpt =
                            match Seq.tryExactlyOne SelectedEntities with
                            | Some selected -> Array.tryFindIndex ((=) selected) picks
                            | None -> None
                        let entity =
                            match selectedIndexOpt with
                            | Some selectedIndex ->
                                if picks.Length > 1 then SelectionCollapseRequestedOpt <- Some picks[(inc selectedIndex) % picks.Length]
                                picks[selectedIndex]
                            | None ->
                                // keep the selection to drag, unless just clicked
                                if SelectedEntities.Contains entity && SelectedEntities.Count > 1
                                then SelectionCollapseRequestedOpt <- Some entity
                                else selectEntities true (Set.singleton entity) world
                                entity
                        if entity.GetIs2d world && not (getSelectionMixed world) // not 2d and 3d together
                        then Some (world.DateTime, ref false, entity)
                        else None

                    | None -> None
                else
                    match DragEntityState with
                    | DragEntityPosition2d (time, snapshottedRef, _, _, entity) when World.isKeyboardAltDown world && entity.GetExists world ->
                        Some (time, snapshottedRef, entity)
                    | DragEntityRotation2d (time, snapshottedRef, _, _, entity) when not (World.isKeyboardAltDown world) && entity.GetExists world ->
                        Some (time, snapshottedRef, entity)
                    | _ -> None
            match dragStartOpt with
            | Some (time, snapshottedRef, entity) ->
                let absolute = entity.GetAbsolute world
                let mousePositionWorld = Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize (World.getMousePosition world) world.WindowViewport
                if World.isKeyboardAltDown world then
                    let entityDegrees = if entity.MountExists world then entity.GetDegreesLocal world else entity.GetDegrees world
                    DragEntityState <- DragEntityRotation2d (time, snapshottedRef, mousePositionWorld, entityDegrees.Z + mousePositionWorld.Y, entity)
                else
                    let entityPosition = entity.GetPosition world
                    DragEntityState <- DragEntityPosition2d (time, snapshottedRef, mousePositionWorld, entityPosition.V2 + mousePositionWorld, entity)
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
                        then Math.Snap3d (Triple.fst (getSnaps ()), entityPosition.V3)
                        else entityPosition.V3
                    let entityPosition = entity.GetPosition world
                    let entityPositionDelta = entityPositionSnapped - entityPosition
                    let entityPositionConstrained = entityPosition + entityPositionDelta
                    entity.SetPosition entityPositionConstrained world
                    for other in getSelectedEntityRoots true world do
                        if other <> entity then other.SetPosition (other.GetPosition world + entityPositionDelta) world
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
                    let entityDegreeDelta = entityDegreeSnapped - entityDegree
                    let roots = getSelectedEntityRoots true world
                    let center =
                        List.fold (fun sum (root : Entity) -> sum + root.GetPosition world) v3Zero roots /
                        single (max 1 roots.Length)
                    let rotationOld = entity.GetRotation world
                    if entity.MountExists world then
                        let entityDegreeLocal = entityDegree + entityDegreeDelta
                        entity.SetDegreesLocal (v3 0.0f 0.0f entityDegreeLocal) world
                    else
                        let entityDegree = entityDegree + entityDegreeDelta
                        entity.SetDegrees (v3 0.0f 0.0f entityDegree) world
                    if roots.Length > 1 then // the selection orbits its center as it turns, as the 3d gizmo turns it
                        let rotationDelta = entity.GetRotation world * rotationOld.Inverted
                        for root in roots do
                            let offset = (root.GetPosition world - center).Transform rotationDelta
                            root.SetPosition (center + offset) world
                            if root <> entity then
                                root.SetDegreesLocal (root.GetDegreesLocal world + v3 0.0f 0.0f entityDegreeDelta) world
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                        entity.SetLinearVelocity v3Zero world
                        entity.SetAngularVelocity v3Zero world
            | DragEntityDuplicate2d (time, mousePositionWorldOriginal, entity) ->
                let localTime = world.DateTime - time
                if entity.GetExists world && localTime.TotalSeconds >= Constants.Gaia.DragMinimumSeconds then
                    selectEntityUnlessSelected true entity world // ctrl+drag duplicates the selection
                    if getSelectedEntitiesUnprotected world && not (getSelectionMixed world) then
                        snapshot DuplicateEntity world
                        let duplicate = duplicateSelectedEntities (ImGui.IsShiftUp ()) world |> getReplacement entity
                        let entityDragOffset = (duplicate.GetPosition world).V2 + mousePositionWorldOriginal
                        DragEntityState <- DragEntityPosition2d (time, ref true, mousePositionWorldOriginal, entityDragOffset, duplicate)
                    else DragEntityState <- DragEntityInactive
            | DragEntityInactive -> ()

        // attempt to end dragging, selecting just the entity clicked within a selection (or the entity behind the one
        // clicked when cycling) when it wasn't dragged
        if ImGui.IsMouseReleased ImGuiMouseButton.Left then
            match SelectionCollapseRequestedOpt with
            | Some entity ->
                let dragged =
                    match DragEntityState with
                    | DragEntityPosition2d (_, snapshottedRef, _, _, _) -> snapshottedRef.Value
                    | DragEntityRotation2d (_, snapshottedRef, _, _, _) -> snapshottedRef.Value
                    | DragEntityDuplicate2d _ | DragEntityInactive -> false
                if not dragged then selectEntities true (Set.singleton entity) world
                SelectionCollapseRequestedOpt <- None
            | None -> ()
            match DragEntityState with
            | DragEntityPosition2d _ | DragEntityRotation2d _ -> DragEntityState <- DragEntityInactive
            | DragEntityDuplicate2d (_, _, entity) -> // released before it was a drag, so a ctrl+click
                if entity.GetExists world then toggleEntitySelection entity world
                DragEntityState <- DragEntityInactive
            | DragEntityInactive -> ()

    let private updateAssetDrag world =
        let io = ImGui.GetIO ()
        if (*ImGui.BeginDragDropViewport ()*)true then
            match DragDropPayloadOpt with
            | Some (DragDropAsset (_, assetTag)) when
                ImGui.IsMouseReleased ImGuiMouseButton.Left &&
                not io.WantCaptureMouse &&
                (*NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr*)true ->
                RightClickPosition <- World.getMousePosition world
                tryCreateEntityOfAsset true None assetTag world |> ignore<Entity option>
            | Some (DragDropPropagationSource source) when
                ImGui.IsMouseReleased ImGuiMouseButton.Left &&
                not io.WantCaptureMouse &&
                (*NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "PropagationSource").NativePtr*)true ->
                if source.GetExists world then
                    RightClickPosition <- World.getMousePosition world
                    createEntityOfPropagationSource PasteAtMouse None source world |> ignore<Entity>
            | Some _ | None -> ()
            (*ImGui.EndDragDropTarget ()*)

    let private updateHotkeys entityHierarchyFocused world =
        if not (modal ()) then
            let io = ImGui.GetIO ()
            if ImGui.IsKeyPressed ImGuiKey.F2 && (match Seq.tryExactlyOne SelectedEntities with Some entity -> entity.GetProtection world = Unprotected | None -> false) then ShowRenameEntityDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F3 then Snaps2dSelected <- not Snaps2dSelected
            elif ImGui.IsKeyPressed ImGuiKey.F4 && ImGui.IsAltDown () then ShowConfirmExitDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F5 then toggleTimeAdvancing world
            elif ImGui.IsKeyPressed ImGuiKey.F6 then EditWhileAdvancing <- not EditWhileAdvancing
            elif ImGui.IsKeyPressed ImGuiKey.F7 then createRestorePoint world
            elif ImGui.IsKeyPressed ImGuiKey.F8 then ReloadAssetsRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F9 && ImGui.IsShiftUp () then ReloadCodeRequested <- (false, 1)
            elif ImGui.IsKeyPressed ImGuiKey.F9 && ImGui.IsShiftDown () then ReloadCodeRequested <- (true, 1)
            elif ImGui.IsKeyPressed ImGuiKey.F10 then toggleOverlayMode ()
            elif ImGui.IsKeyPressed ImGuiKey.F11 then toggleViewMode FreeMode world
            elif ImGui.IsKeyPressed ImGuiKey.F12 then toggleViewMode CaptureMode world
            elif ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then World.tryToggleWindowFullScreen world
            elif ImGui.IsKeyPressed ImGuiKey.UpArrow && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then tryReorderSelectedEntity true world
            elif ImGui.IsKeyPressed ImGuiKey.DownArrow && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then tryReorderSelectedEntity false world
            elif ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then LogStr <- ""
            elif ImGui.IsKeyPressed ImGuiKey.L && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then ImGuiIniResetRequested <- true
            elif ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltDown () then step world
            elif ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ShowNewGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ShowOpenGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ShowSaveGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.B && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then tryAutoBoundsSelectedEntity world |> ignore<bool>
            elif ImGui.IsKeyPressed ImGuiKey.P && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then NewEntityParentOpt <- Seq.tryExactlyOne SelectedEntities
            elif ImGui.IsKeyPressed ImGuiKey.P && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then NewEntityParentOpt <- None
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltDown () then ShowOpenEntityDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltDown () then (match Seq.tryExactlyOne SelectedEntities with Some entity when entity.GetExists world -> ShowSaveEntityDialog <- true | _ -> ())
            elif ImGui.IsKeyPressed ImGuiKey.R && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ReloadAllRequested <- (false, 1)
            elif ImGui.IsKeyPressed ImGuiKey.R && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then ReloadAllRequested <- (true, 1)
            elif ImGui.IsKeyPressed ImGuiKey.F && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then
                if SelectedWindowOpt = Some "Edit Property" && PropertyFocusedOpt.IsSome
                then searchPropertyValue () // the property editor searches its value instead when it's focused on one
                else searchEntityHierarchy ()
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then ShowOpenProjectDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then freezeEntities world
            elif ImGui.IsKeyPressed ImGuiKey.T && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then thawEntities world
            elif ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then synchronizeNav world
            elif ImGui.IsKeyPressed ImGuiKey.L && ImGui.IsCtrlDown () && ImGui.IsShiftDown () && ImGui.IsAltUp () then rerenderLightMaps world
            elif ImGui.IsKeyPressed ImGuiKey.I && ImGui.IsCtrlDown () && ImGui.IsShiftUp () && ImGui.IsAltUp () then tryMoveSelectedEntityToOrigin false world |> ignore<bool>
            elif ImGui.IsKeyPressed ImGuiKey.Escape && SelectedWindowOpt = Some "Edit Property" && not (String.IsNullOrWhiteSpace PropertyValueSearchStr) then
                PropertyValueSearchStr <- ""
                PropertyValueSearchIndex <- -1
            elif not io.WantCaptureKeyboardGlobal || entityHierarchyFocused then
                if ImGui.IsKeyPressed ImGuiKey.Z && ImGui.IsCtrlDown () then tryUndo world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Y && ImGui.IsCtrlDown () then tryRedo world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.X && ImGui.IsCtrlDown () then tryCutSelectedEntity world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlDown () then tryCopySelectedEntity world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.V && ImGui.IsCtrlDown () then tryPaste PasteAtLook (Option.map cast NewEntityParentOpt) world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlDown () then createEntity false false None None world |> ignore<Entity>
                elif ImGui.IsKeyPressed ImGuiKey.Delete then tryDeleteSelectedEntity world |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.M && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ManipulationWorld <- not ManipulationWorld
                elif ImGui.IsKeyPressed ImGuiKey.I && ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltUp () then ShowInputsDialog <- true
                elif ImGui.IsKeyPressed ImGuiKey.Escape then
                    if not (String.IsNullOrWhiteSpace PropagationSourcesSearchStr) then
                        PropagationSourcesSearchStr <- ""
                    elif not (String.IsNullOrWhiteSpace EntityHierarchySearchStr) then
                        EntityHierarchySearchStr <- ""
                    elif entityHierarchyFocused then
                        ViewportFocusRequested <- true
                    else
                        ImGuiInternal.tryCancelDragDrop ()
                        DragDropPayloadOpt <- None // TODO: P1: remove this line when AcceptDragDropPayload is exposed.
                        focusPropertyOpt None world
                        selectEntities true Set.empty world

    let private updateResetEye () =
        if ResetEyeRequested then
            DesiredEye2dCenter <- v2Zero
            DesiredEye3dCenter <- Constants.Engine.Eye3dCenterDefault
            DesiredEye3dRotation <- quatIdentity
            ResetEyeRequested <- false

    (* Top-Level Functions *)

    // TODO: split this function up or at least apply intention blocks?
    let private imGuiEntity branch filtering (entity : Entity) (world : World) =
        let selected = SelectedEntities.Contains entity
        let treeNodeFlags =
            (if selected then ImGuiTreeNodeFlags.Selected else ImGuiTreeNodeFlags.None) |||
            (if not branch || filtering then ImGuiTreeNodeFlags.Leaf else ImGuiTreeNodeFlags.None) |||
            (if NewEntityParentOpt = Some entity && world.DateTime.Millisecond / 400 % 2 = 0 then ImGuiTreeNodeFlags.Bullet else ImGuiTreeNodeFlags.None) |||
            ImGuiTreeNodeFlags.OpenOnArrow
        if not filtering then
            if ExpandEntityHierarchy then ImGui.SetNextItemOpen true
            if CollapseEntityHierarchy then ImGui.SetNextItemOpen false
        if ShowSelectedEntity then // open the way to every selected entity
            let containsSelected =
                SelectedEntities |> Seq.exists (fun (selectedEntity : Entity) ->
                    selectedEntity.GetExists world &&
                    let relation = relate entity selectedEntity
                    Array.notExists ((=) Constants.Address.ParentName) relation.Names && relation.Names.Length > 0)
            if containsSelected then ImGui.SetNextItemOpen true
        let treeNodeLabel = if filtering then entity.Name + "##" + scstringMemo entity else entity.Name
        ImGui.SetNextItemSelectionUserData (int64 EntityHierarchyIndexed.Count)
        EntityHierarchyIndexed.Add entity
        let modified = getEntityModified entity world
        if modified then ImGui.PushStyleColor (ImGuiCol.Text, Color.Gold.Abgr)
        let expanded = ImGui.TreeNodeEx (treeNodeLabel, treeNodeFlags)
        if modified then ImGui.PopStyleColor ()
        if ShowSelectedEntity && SelectedEntities.Contains entity then
            ImGui.SetScrollHereY 0.5f
        if  ImGui.IsItemFocused () &&
            (ImGui.IsKeyPressed ImGuiKey.Enter || ImGui.IsKeyPressed ImGuiKey.KeypadEnter) &&
            ImGui.IsCtrlUp () && ImGui.IsShiftUp () && ImGui.IsAltUp () then
            ViewportFocusRequested <- true // hand the keyboard back to the viewport after picking an entity
        if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            let entities = if SelectedEntities.Contains entity then getSelectedEntityRoots false world else [entity]
            frameEntities entities world // the selection when it's among it
            detectEyeChangedElsewhere world
        let mutable openPopupContextItemWhenUnselected = false
        let popupContextItemTitle = "##popupContextItem" + scstringMemo entity
        if ImGui.BeginPopupContextItem popupContextItemTitle then
            if ImGui.IsMouseReleased ImGuiMouseButton.Right then openPopupContextItemWhenUnselected <- true
            selectEntityUnlessSelected false entity world // leaving focus be, since moving it would close this popup
            let singular = SelectedEntities.Count <= 1 // what's done to one entity, or under one, isn't shown for several
            if singular then
                if ImGui.MenuItem "Create Entity" then createEntity false true None None world |> ignore<Entity>
                if ImGui.MenuItem "Create Entity at Local Origin" then
                    createEntity true true None None world |> ignore<Entity>
                    tryMoveSelectedEntityToOrigin true world |> ignore<bool>
            if ImGui.MenuItem "Delete Entity" then tryDeleteSelectedEntity world |> ignore<bool>
            if singular && entity.GetProtection world = Unprotected then
                if ImGui.MenuItem ("Rename Entity", "F2") then ShowRenameEntityDialog <- true
            ImGui.Separator ()
            if ImGui.MenuItem "Cut Entity" then tryCutSelectedEntity world |> ignore<bool>
            if ImGui.MenuItem "Copy Entity" then tryCopySelectedEntity world |> ignore<bool>
            if singular then
                if ImGui.MenuItem "Paste Entity" then tryPaste PasteAtLook (Some entity) world |> ignore<bool>
                if ImGui.MenuItem "Paste Entity at Local Origin" then
                    if tryPaste PasteAtLook (Some entity) world then
                        tryMoveSelectedEntityToOrigin true world |> ignore<bool>
                ImGui.Separator ()
                if ImGui.MenuItem ("Open Entity", "Ctrl+Alt+O") then ShowOpenEntityDialog <- true
                if ImGui.MenuItem ("Save Entity", "Ctrl+Alt+S") then
                    match Seq.tryExactlyOne SelectedEntities with
                    | Some entity when entity.GetExists world ->
                        match Map.tryFind entity.EntityAddress EntityFilePaths with
                        | Some filePath -> EntityFileDialogState.FilePath <- filePath
                        | None -> EntityFileDialogState.FileName <- ""
                        ShowSaveEntityDialog <- true
                    | Some _ | None -> ()
                if getEntityRevertible entity world then
                    if ImGui.MenuItem "Revert Entity" then
                        tryRevertSelectedEntity world |> ignore<bool>
            ImGui.Separator ()
            if singular then
                if NewEntityParentOpt = Some entity then
                    if ImGui.MenuItem ("Reset Creation Parent", "Ctrl+Shift+P") then
                        NewEntityParentOpt <- None
                        ShowEntityContextMenu <- false
                else
                    if ImGui.MenuItem ("Set as Creation Parent", "Ctrl+P") then
                        NewEntityParentOpt <- Some entity
                        ShowEntityContextMenu <- false
            if ImGui.MenuItem ("Auto Bounds Entity", "Ctrl+B") then tryAutoBoundsSelectedEntity world |> ignore<bool>
            if ImGui.MenuItem ("Move to Origin", "Ctrl+I") then tryMoveSelectedEntityToOrigin false world |> ignore<bool>
            if ImGui.MenuItem ("Propagate Entity") then tryPropagateSelectedEntityStructure world |> ignore<bool>
            if ImGui.MenuItem ("Wipe Propagated Descriptor") then tryWipeSelectedEntityPropagationTargets world |> ignore<bool>
            let entities = getSelectedEntityRoots false world // offering what changes any of them
            if List.exists (fun (entity : Entity) -> entity.GetStatic world) entities then
                if ImGui.MenuItem "Make Entity Family Non-Static" then
                    trySetSelectedEntityFamilyStatic false world
            if List.exists (fun (entity : Entity) -> not (entity.GetStatic world)) entities then
                if ImGui.MenuItem "Make Entity Family Static" then
                    trySetSelectedEntityFamilyStatic true world
            let operation = HierarchyContext { EditContext = makeEditContext None None }
            World.editGame tautology operation Game world
            World.editScreen tautology operation SelectedScreen world
            World.editGroup tautology operation SelectedGroup world
            match Seq.tryExactlyOne SelectedEntities with
            | Some selectedEntity when singular -> World.editEntity tautology operation selectedEntity world
            | Some _ | None -> ()
            detectEyeChangedElsewhere world
            ImGui.EndPopup ()
        if openPopupContextItemWhenUnselected then
            ImGui.OpenPopup popupContextItemTitle
        if ImGui.BeginDragDropTarget () then
            if NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr then
                match DragDropPayloadOpt with
                | Some (DragDropAsset (_, assetTag)) ->
                    let parent = Nu.Entity (SelectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                    tryCreateEntityOfAsset false (Some (parent :> Simulant)) assetTag world |> ignore<Entity option>
                | Some _ | None -> ()
            if NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "PropagationSource").NativePtr then
                match DragDropPayloadOpt with
                | Some (DragDropPropagationSource source) when source.GetExists world ->
                    let parent = Nu.Entity (SelectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                    createEntityOfPropagationSource PasteAtLook (Some (parent :> Simulant)) source world |> ignore<Entity>
                | Some _ | None -> ()
            if NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr then
                match DragDropPayloadOpt with
                | Some (DragDropEntity sourceEntity) ->
                    let entities = getHierarchyDragEntities sourceEntity world
                    let target = Nu.Entity (SelectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                    if ImGui.IsCtrlDown () then // ctrl+drop duplicates, ctrl+shift without propagation
                        tryDuplicateEntitiesInHierarchy entities target world
                    elif ImGui.IsAltDown () then // alt+drop places before the target, under its parent
                        let parentOpt = match target.Parent with :? Entity as parent -> Some parent | _ -> None
                        tryDropEntitiesInHierarchy entities parentOpt (Some target) world
                    else tryDropEntitiesInHierarchy entities (Some target) None world
                | Some _ | None -> ()
            ImGui.EndDragDropTarget ()
        if ImGui.BeginDragDropSource () then
            DragDropPayloadOpt <- Some (DragDropEntity entity)
            let count = (getHierarchyDragEntities entity world).Length
            let label = if count > 1 then string count + " entities" else entity.Name
            ImGui.Text (label + if ImGui.IsCtrlDown () then " (Copy)" else "")
            ImGui.SetDragDropPayload ("Entity", IntPtr.Zero, 0u) |> ignore<bool>
            ImGui.EndDragDropSource ()
        if entity.GetExists world then // check for existence since entity may have been deleted just above
            if entity.Has<Freezer3dFacet> world then
                let frozen = entity.GetFrozen world
                let (text, color) = if frozen then ("Thaw", Color.CornflowerBlue) else ("Freeze", Color.DarkRed)
                ImGui.SameLine ()
                ImGui.PushStyleColor (ImGuiCol.Button, color.Abgr)
                ImGui.PushID ("##frozen" + scstringMemo entity)
                if ImGui.SmallButton text then // on a selected row, for every selected freezer
                    let frozen = not frozen
                    snapshot (SetEntityFrozen frozen) world
                    let entities = if SelectedEntities.Contains entity then List.ofSeq SelectedEntities else [entity]
                    for entity in entities do
                        if entity.GetExists world && entity.Has<Freezer3dFacet> world then entity.SetFrozen frozen world
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

    let rec private imGuiEntityHierarchy (shownOpt : Entity HashSet option) (entity : Entity) world =
        if entity.GetExists world then // NOTE: entity may have been moved during this process.
            let filtering =
                not (String.IsNullOrWhiteSpace EntityHierarchySearchStr)
            let shown =
                match shownOpt with
                | Some shown -> shown.Contains entity
                | None -> true
            let visible =
                shown &&
                (String.IsNullOrWhiteSpace EntityHierarchySearchStr || entity.Name.ToLowerInvariant().Contains (EntityHierarchySearchStr.ToLowerInvariant ()))
            let expanded =
                if visible then
                    let branch =
                        match shownOpt with
                        | Some shown -> entity.GetChildren world |> Seq.exists shown.Contains
                        | None -> entity.HasChildren world
                    if shownOpt.IsSome && not filtering then ImGui.SetNextItemOpen true // the way to each shown entity is open
                    imGuiEntity branch filtering entity world
                else false
            if filtering && shown || expanded then
                entity.GetChildren world
                |> Array.ofSeq
                |> Array.map (fun entity -> (struct (entity.Surnames.Length, entity.GetOrder world), entity))
                |> Array.sortBy fst
                |> Array.map snd
                |> Array.iter (fun child -> imGuiEntityHierarchy shownOpt child world)
                if visible then ImGui.TreePop ()

    let private imGuiAppendPropertiesDispatcherExplicit simulant world =
        let ty = getType (World.getDispatcher simulant world)
        if ImGui.CollapsingHeader (ty.Name.Spaced, ImGuiTreeNodeFlags.DefaultOpen ||| ImGuiTreeNodeFlags.OpenOnArrow ||| ImGuiTreeNodeFlags.AllowOverlap) then
            let y = ImGui.GetCursorPosY ()
            let unfocusProperty () = focusPropertyOpt None world
            let appendProperties : AppendProperties = { EditContext = makeEditContext None (Some unfocusProperty) }
            World.edit (fun o -> o.GetType () = ty) (AppendProperties appendProperties) simulant world
            y
        else ImGui.GetCursorPosY ()

    let private imGuiEditPropertyRecord
        (getProperty : PropertyDescriptor -> Simulant -> World -> obj)
        (setProperty : bool -> obj -> PropertyDescriptor -> Simulant -> World -> unit)
        (focusProperty : unit -> unit)
        (headered : bool)
        (propertyDescriptor : PropertyDescriptor)
        (simulant : Simulant)
        (world : World) =
        let propertyValue = getProperty propertyDescriptor simulant world
        let context = makeEditContext (Some focusProperty) None
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
        let context = makeEditContext (Some focusProperty) None
        let (promoted, edited, propertyValue) = World.imGuiEditProperty propertyDescriptor.PropertyName propertyDescriptor.PropertyType propertyValue context world
        if promoted || edited then setProperty (not edited) propertyValue propertyDescriptor simulant world

    let private imGuiEditEntityAppliedTypes (entity : Entity) world =
        let dispatcherNameCurrent = getTypeName (entity.GetDispatcher world)
        if ImGui.BeginCombo ("Dispatcher Name", dispatcherNameCurrent, ImGuiComboFlags.HeightRegular) then
            let dispatcherNames = (World.getEntityDispatchers world).Keys
            let dispatcherNamePicked = tryPickName dispatcherNames
            for dispatcherName in dispatcherNames do
                if ImGui.Selectable (dispatcherName, (dispatcherName = dispatcherNameCurrent)) then
                    if entity.GetProtection world = Unprotected then
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
                    if ImGui.Selectable (facetNameSelectable, (facetName = NewEntityDispatcherName)) then
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
        let mutable appendedToDispatcher = false
        let propertyDescriptors = SimulantPropertyDescriptor.getCategorizedPropertyDescriptors simulant world
        for (propertyCategory, propertyDescriptors) in propertyDescriptors do
            let propertyCategoryName = match propertyCategory with Left name -> name | Right ty -> ty.Name.Spaced
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
                    (mountActive, (entity.TryGetProperty Constants.Engine.ModelPropertyName world).Value.PropertyType <> typeof<unit>)
                | :? Group as group -> (false, (group.TryGetProperty Constants.Engine.ModelPropertyName world).Value.PropertyType <> typeof<unit>)
                | :? Screen as screen -> (false, (screen.TryGetProperty Constants.Engine.ModelPropertyName world).Value.PropertyType <> typeof<unit>)
                | :? Game as game -> (false, (game.TryGetProperty Constants.Engine.ModelPropertyName world).Value.PropertyType <> typeof<unit>)
                | _ -> failwithumf ()
            match propertyCategory with // preempt with dispatcher category if it is not represented by any of the properties
            | Right ty when not appendedToDispatcher && not (ty.IsAssignableTo typeof<Dispatcher>) ->
                let yBeforeHeader = ImGui.GetCursorPosY ()
                let yAfterHeader = imGuiAppendPropertiesDispatcherExplicit simulant world
                appendedToDispatcher <- true
                if yAfterHeader = ImGui.GetCursorPosY () then // overwrite dispatcher header when no dispatcher properties
                    let drawList = ImGui.GetWindowDrawList ()
                    let itemMin = ImGui.GetItemRectMin ()
                    let itemMax = ImGui.GetItemRectMax ()
                    drawList.AddRectFilled (itemMin, itemMax, ImGui.ColorConvertFloat4ToU32 (Vector4 (0.4f, 0.4f, 0.4f, 1.0f)), 5.0f) // draw colored square to overwrite unutilized header
                    ImGui.SetCursorPosY yBeforeHeader // back up cursor Y if it didn't advance due to no dispatcher properties
            | Right _ | Left _ -> ()
            if  (propertyCategoryName <> "Model" || modelUsed) &&
                (propertyCategoryName = "Ambient" || ImGui.CollapsingHeader (propertyCategoryName + "##category", ImGuiTreeNodeFlags.DefaultOpen ||| ImGuiTreeNodeFlags.OpenOnArrow)) then
                let propertyDescriptors =
                    propertyDescriptors
                    |> Seq.filter (fun pd ->
                        SimulantPropertyDescriptor.getEditable pd simulant &&
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
                    |> Seq.sortBy (fun pd ->
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
                            | :? Game as game ->
                                let mutable name = game.Name
                                ImGui.InputText ("Name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                ImGui.Text ("(" + string (game.GetId world) + ")")
                            | :? Screen as screen ->
                                let mutable name = screen.Name
                                ImGui.InputText ("Name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                ImGui.Text ("(" + string (screen.GetId world) + ")")
                            | :? Group as group ->
                                let mutable name = group.Name
                                ImGui.InputText ("##name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                if group.GetProtection world = Unprotected then
                                    if ImGui.Button "Rename" then
                                        ShowRenameGroupDialog <- true
                                else ImGui.Text "Name"
                                ImGui.SameLine ()
                                ImGui.Text ("(" + string (group.GetId world) + ")")
                            | :? Entity as entity ->
                                let mutable name = entity.Name
                                ImGui.InputText ("##name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                if entity.GetProtection world = Unprotected then
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
                                      EditContext = makeEditContext (Some focusProperty) None }
                            World.edit tautology replaceProperty simulant world
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
                                      EditContext = makeEditContext (Some focusProperty) None }
                            World.edit tautology replaceProperty simulant world
                            if not replaced then imGuiEditProperty getPropertyValue setPropertyValue focusProperty propertyDescriptor simulant world
                match propertyCategory with
                | Right ty ->
                    let unfocusProperty () = focusPropertyOpt None world
                    let appendProperties : AppendProperties = { EditContext = makeEditContext None (Some unfocusProperty) }
                    World.edit (fun o -> o.GetType () = ty) (AppendProperties appendProperties) simulant world
                    if ty.IsAssignableTo typeof<Dispatcher> then appendedToDispatcher <- true
                | Left _ -> ()
            if propertyCategoryName = "Ambient" then // applied types directly after ambient properties
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
        if not appendedToDispatcher then imGuiAppendPropertiesDispatcherExplicit simulant world |> ignore<single>
        detectEyeChangedElsewhere world

    let private imGuiViewportManipulation (world : World) =

        // viewport
        let io = ImGui.GetIO ()
        ImGui.SetNextWindowPos v2Zero
        ImGui.SetNextWindowSize io.DisplaySize
        if ImGui.IsKeyReleased ImGuiKey.Escape && not (modal ()) then ImGui.SetNextWindowFocus ()
        if ImGui.Begin ("Viewport", ImGuiWindowFlags.NoBackground ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoInputs) then
            if not ViewMode.IsCaptureMode then

                // physics debug rendering
                if PhysicsDebugRendering3d then
                    let mutable settings3d = DrawSettings (DrawShapeWireframe = true)
                    World.imGuiRenderPhysics3dPlus settings3d PhysicsDebugRenderingDistance3d PhysicsDebugRenderingHeightMaps3d world
                if PhysicsDebugRendering2d then
                    World.imGuiRenderPhysics2d world

                // user-defined viewport manipulation
                let windowViewport = world.WindowViewport
                let projectionMatrix = Viewport.getProjection3d world.Eye3dFieldOfView windowViewport
                let projection = projectionMatrix.ToArray ()
                let operation =
                    ViewportOverlay
                        { ViewportView = Viewport.getView3d world.Eye3dCenter world.Eye3dRotation
                          ViewportProjection = projectionMatrix
                          ViewportBounds = box2 v2Zero io.DisplaySize
                          EditContext = makeEditContext None None }
                World.editGame tautology operation Game world
                World.editScreen tautology operation SelectedScreen world
                World.editGroup tautology operation SelectedGroup world
                match Seq.tryExactlyOne SelectedEntities with
                | Some entity when entity.GetExists world && entity.GetIs3d world ->
                    let operation =
                        ViewportOverlay
                            { ViewportView = Viewport.getView3d world.Eye3dCenter world.Eye3dRotation
                              ViewportProjection = projectionMatrix
                              ViewportBounds = box2 v2Zero io.DisplaySize
                              EditContext = makeEditContext None None }
                    World.editEntity tautology operation entity world
                | Some _ | None -> ()
                detectEyeChangedElsewhere world

                // light probe bounds manipulation
                match Seq.tryExactlyOne SelectedEntities with
                | Some entity when entity.GetExists world && entity.Has<LightProbeFacet> world ->
                    let mutable lightProbeBounds = entity.GetProbeBounds world
                    let manipulationResult =
                        ImGuizmo.ManipulateBox3
                            (world.Eye3dCenter,
                             world.Eye3dRotation,
                             world.Eye3dFieldOfView,
                             windowViewport,
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
                let inner = windowViewport.Inner
                let bounds = windowViewport.Bounds
                let outer = windowViewport.Outer
                let offsetInner = v2i 0 ((bounds.Min.Y - inner.Min.Y) + (bounds.Max.Y - inner.Max.Y))
                let offsetBounds = (outer.Size - bounds.Size) / -2
                let innerImGuiMin = inner.Min + offsetInner + offsetBounds
                let innerImGui = box2 innerImGuiMin.V2 inner.Size.V2
                ImGuizmo.SetRect (innerImGui.Min.X, innerImGui.Min.Y, innerImGui.Size.X, innerImGui.Size.Y)
                ImGuizmo.SetDrawlist (ImGui.GetBackgroundDrawList ())

                // attempt transform manipulation of the selection
                match getSelectedEntityRoots true world with
                | _ when ImGui.IsMouseDown ImGuiMouseButton.Middle || ImGui.IsMouseDown ImGuiMouseButton.Right ->
                    () // neither manipulation shows nor takes input while the eye's dragged
                | entity :: _ when not io.WantCaptureMouseLocal ->
                    if entity.GetIs3d world && not (getSelectionMixed world) then // not 2d and 3d together
                        let viewMatrix = Viewport.getView3d world.Eye3dCenter world.Eye3dRotation
                        let view = viewMatrix.ToArray ()
                        let roots = getSelectedEntityRoots true world
                        let grouped = roots.Length > 1 // several move together about their center
                        let center = List.fold (fun sum (root : Entity) -> sum + root.GetPosition world) v3Zero roots / single roots.Length
                        let rotationPivot = if grouped then quatIdentity else entity.GetRotation world
                        let affineMatrix =
                            if grouped
                            then Matrix4x4.CreateAffine (center, rotationPivot, v3One)
                            else entity.GetAffineMatrix world
                        let affine = affineMatrix.ToArray ()
                        let (p, r, s) =
                            if not Snaps2dSelected && ImGui.IsCtrlUp ()
                            then Snaps3d
                            else (0.0f, 0.0f, 0.0f)
                        let mutable copying = false
                        if not ManipulationActive then
                            if ImGui.IsCtrlDown () && getSelectedEntitiesUnprotected world then
                                ManipulationOperation <- OPERATION.TRANSLATE
                                copying <- true
                            elif ImGui.IsShiftDown () then ManipulationOperation <- OPERATION.SCALE
                            elif ImGui.IsAltDown () then ManipulationOperation <- OPERATION.ROTATE
                            elif ImGui.IsKeyDown ImGuiKey.X then ManipulationOperation <- OPERATION.ROTATE_X
                            elif ImGui.IsKeyDown ImGuiKey.Y then ManipulationOperation <- OPERATION.ROTATE_Y
                            elif ImGui.IsKeyDown ImGuiKey.Z then ManipulationOperation <- OPERATION.ROTATE_Z
                            else ManipulationOperation <- OPERATION.TRANSLATE
                        let manipulationSpace =
                            if ManipulationWorld then MODE.WORLD else MODE.LOCAL
                        let mutable snap =
                            match ManipulationOperation with
                            | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> r
                            | _ -> 0.0f // NOTE: doing other snapping ourselves since I don't like guizmo's implementation.
                        let delta = m4Identity.ToArray ()
                        let manipulationResult =
                            if snap = 0.0f
                            then ImGuizmo.Manipulate (&view[0], &projection[0], ManipulationOperation, manipulationSpace, &affine[0], &delta[0])
                            else ImGuizmo.Manipulate (&view[0], &projection[0], ManipulationOperation, manipulationSpace, &affine[0], &delta[0], &snap)
                        if manipulationResult && grouped then
                            let manipulationAwaken =
                                if not ManipulationActive && ImGui.IsMouseDown ImGuiMouseButton.Left then
                                    ManipulationActive <- true
                                    true
                                else false
                            if manipulationAwaken then
                                ManipulationScalePrevious <- v3One
                                if copying then // ctrl+drag duplicates the selection, shift without propagation
                                    snapshot DuplicateEntity world
                                    duplicateSelectedEntities (ImGui.IsShiftUp ()) world |> ignore<(Entity * Entity) list>
                                else
                                    let snapshotType =
                                        match ManipulationOperation with
                                        | OPERATION.TRANSLATE -> TranslateEntity
                                        | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z -> RotateEntity
                                        | _ -> ScaleEntity
                                    snapshot snapshotType world
                            let mutable (positionPivot, rotationPivot', scalePivot) = (v3Zero, quatIdentity, v3One)
                            if Matrix4x4.Decompose (Matrix4x4.CreateFromArray affine, &scalePivot, &rotationPivot', &positionPivot) then
                                let roots = getSelectedEntityRoots true world // the duplicates, when just duplicated
                                match ManipulationOperation with
                                | OPERATION.TRANSLATE ->
                                    let mutable center' = positionPivot
                                    let translation = (Matrix4x4.CreateFromArray delta).Translation
                                    let epsilon = 0.0001f
                                    if not (Math.ApproximatelyEqual (translation.X, 0.0f, epsilon)) then center'.X <- Math.SnapF (p, center'.X)
                                    if not (Math.ApproximatelyEqual (translation.Y, 0.0f, epsilon)) then center'.Y <- Math.SnapF (p, center'.Y)
                                    if not (Math.ApproximatelyEqual (translation.Z, 0.0f, epsilon)) then center'.Z <- Math.SnapF (p, center'.Z)
                                    let offset = center' - center // the center snaps, the selection following
                                    for root in roots do root.SetPosition (root.GetPosition world + offset) world
                                | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z ->
                                    let rotationDelta = rotationPivot'.Normalized * rotationPivot.Inverted
                                    for root in roots do
                                        let offset = (root.GetPosition world - center).Transform rotationDelta
                                        root.SetPosition (center + offset) world
                                        root.SetRotation (rotationDelta * root.GetRotation world) world
                                | OPERATION.SCALE ->
                                    let scaleRatio = scalePivot / ManipulationScalePrevious // the gizmo gives scale since it was taken up
                                    ManipulationScalePrevious <- scalePivot
                                    for root in roots do
                                        let offset = (root.GetPosition world - center).Transform rotationPivot.Inverted * scaleRatio
                                        root.SetPosition (center + offset.Transform rotationPivot) world
                                        root.SetScale (Vector3.Max (root.GetScale world * scaleRatio, v3Dup 0.01f)) world
                                | _ -> ()
                                if world.TimeAdvancing then
                                    for root in roots do
                                        match root.TryGetProperty (nameof root.LinearVelocity) world with
                                        | Some property when property.PropertyType = typeof<Vector3> -> root.SetLinearVelocity v3Zero world
                                        | Some _ | None -> ()
                                        match root.TryGetProperty (nameof root.AngularVelocity) world with
                                        | Some property when property.PropertyType = typeof<Vector3> -> root.SetAngularVelocity v3Zero world
                                        | Some _ | None -> ()
                        if manipulationResult && not grouped then
                            let manipulationAwaken =
                                if not ManipulationActive && ImGui.IsMouseDown ImGuiMouseButton.Left then
                                    ManipulationActive <- true
                                    true
                                else false
                            if manipulationAwaken then ManipulationPositionStart <- entity.GetPosition world
                            let affine' = Matrix4x4.CreateFromArray affine
                            let mutable (position, rotation, degrees, scale) = (v3Zero, quatIdentity, v3Zero, v3One)
                            if Matrix4x4.Decompose (affine', &scale, &rotation, &position) then
                                let delta = Matrix4x4.CreateFromArray delta
                                let translation = delta.Translation
                                let epsilon = 0.0001f // NOTE: making this any higher can create false negatives and leave entities positioned at random offsets.
                                if ManipulationWorld then

                                    // snap in world space, aligning each moved coordinate to the world grid
                                    if not (Math.ApproximatelyEqual (translation.X, 0.0f, epsilon)) then position.X <- Math.SnapF (p, position.X)
                                    if not (Math.ApproximatelyEqual (translation.Y, 0.0f, epsilon)) then position.Y <- Math.SnapF (p, position.Y)
                                    if not (Math.ApproximatelyEqual (translation.Z, 0.0f, epsilon)) then position.Z <- Math.SnapF (p, position.Z)

                                else

                                    // snap in object space, moving the entity from where it was taken up by
                                    // whole snaps along each of its own axes that it was moved along
                                    let rotationObject = entity.GetRotation world
                                    let rotationObjectInverse = rotationObject.Inverted
                                    let translationObject = translation.Transform rotationObjectInverse
                                    let mutable offsetObject = (position - ManipulationPositionStart).Transform rotationObjectInverse
                                    if not (Math.ApproximatelyEqual (translationObject.X, 0.0f, epsilon)) then offsetObject.X <- Math.SnapF (p, offsetObject.X)
                                    if not (Math.ApproximatelyEqual (translationObject.Y, 0.0f, epsilon)) then offsetObject.Y <- Math.SnapF (p, offsetObject.Y)
                                    if not (Math.ApproximatelyEqual (translationObject.Z, 0.0f, epsilon)) then offsetObject.Z <- Math.SnapF (p, offsetObject.Z)
                                    position <- ManipulationPositionStart + offsetObject.Transform rotationObject

                                rotation <- rotation.Normalized // try to avoid weird angle combinations
                                let rollPitchYaw = rotation.RollPitchYaw
                                degrees.X <- radToDegF rollPitchYaw.X
                                degrees.Y <- radToDegF rollPitchYaw.Y
                                degrees.Z <- radToDegF rollPitchYaw.Z
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
                                if copying then // ctrl+drag duplicates the selection, ctrl+shift without propagation
                                    if manipulationAwaken then snapshot DuplicateEntity world
                                    duplicateSelectedEntities (ImGui.IsShiftUp ()) world |> getReplacement entity
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
                            match ManipulationOperation with
                            | OPERATION.TRANSLATE -> entity.SetPosition position world
                            | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z ->
                                match Option.bind (flip tryResolve entity) (entity.GetMountOpt world) with
                                | Some mount ->
                                    // NOTE: setting local degrees directly so they're normalized the same way as world degrees.
                                    let mountRotationInverse = (mount.GetRotation world).Inverted
                                    let rotationLocal = mountRotationInverse * rotation
                                    let rollPitchYawLocal = rotationLocal.RollPitchYaw
                                    let mutable degreesLocal = v3Zero
                                    degreesLocal.X <- radToDegF rollPitchYawLocal.X
                                    degreesLocal.Y <- radToDegF rollPitchYawLocal.Y
                                    degreesLocal.Z <- radToDegF rollPitchYawLocal.Z
                                    degreesLocal <- if degreesLocal.X = 180.0f && degreesLocal.Z = 180.0f then v3 0.0f (180.0f - degreesLocal.Y) 0.0f else degreesLocal
                                    degreesLocal <- v3 degreesLocal.X (if degreesLocal.Y > 180.0f then degreesLocal.Y - 360.0f else degreesLocal.Y) degreesLocal.Z
                                    degreesLocal <- v3 degreesLocal.X (if degreesLocal.Y < -180.0f then degreesLocal.Y + 360.0f else degreesLocal.Y) degreesLocal.Z
                                    entity.SetDegreesLocal degreesLocal world
                                | None -> entity.SetDegrees degrees world
                            | OPERATION.SCALE -> entity.SetScale scale world
                            | _ -> () // nothing to do
                            if world.TimeAdvancing then
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
                                    | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z when r <> 0.0f && not grouped ->
                                        let degreesLocal = Math.SnapDegree3d (r, entity.GetDegreesLocal world)
                                        entity.SetDegreesLocal degreesLocal world
                                    | _ -> ()
                                | None ->
                                    match ManipulationOperation with
                                    | OPERATION.ROTATE | OPERATION.ROTATE_X | OPERATION.ROTATE_Y | OPERATION.ROTATE_Z when r <> 0.0f && not grouped ->
                                        let degrees = Math.SnapDegree3d (r, entity.GetDegrees world)
                                        entity.SetDegrees degrees world
                                    | _ -> ()
                                ManipulationOperation <- OPERATION.TRANSLATE
                                ManipulationActive <- false

                // otherwise do view manipulation
                // NOTE: ViewManipulate isn't used at the same time as Manipulate because for some reason, they don't
                // seem to be working properly together.
                | _ ->

                    if not ManipulationActive && DragEntityState = DragEntityInactive then
                        let eyeRotationOld = world.Eye3dRotation
                        let eyeRotationArray = Matrix4x4.CreateFromQuaternion(eyeRotationOld).Transposed.ToArray()
                        let size = v2 128.0f 128.0f
                        let position =
                            if OverlayMode && not ViewMode.IsFreeMode
                            then v2 (single windowViewport.Bounds.Size.X - 475.0f) 100.0f
                            else v2 (innerImGui.Max.X - 178.0f) (innerImGui.Min.Y + 44.0f)
                        ImGuizmo.ViewManipulate (&eyeRotationArray[0], 1.0f, position, size, uint 0x00000000)
                        let eyeRotation = Matrix4x4.CreateFromArray(eyeRotationArray).Transposed.Rotation
                        let eyeDiv = eyeRotation.RollPitchYaw.Z / MathF.PI_OVER_2 // NOTE: this and the eyeUpright variable mitigate #932.
                        let eyeUpright = Math.ApproximatelyEqual (eyeDiv, round eyeDiv, 0.01f)
                        if ImGuizmo.IsUsingViewManipulate () then
                            if  (not io.WantCaptureMouseGlobal || ImGuizmo.IsViewManipulateHovered ()) &&
                                eyeRotationOld.Up.Dot eyeRotation.Up >= 0.0f &&
                                eyeUpright then
                                DesiredEye3dRotation <- eyeRotation
                                World.setEye3dRotation DesiredEye3dRotation world // NOTE: we also set directly to fix #1361.
                            io.SwallowMouse ()

            // clear editing states that this code otherwise affects
            else
                ManipulationOperation <- OPERATION.TRANSLATE
                ManipulationActive <- false

        ImGui.End ()

    let private imGuiFullScreenWindow world =
        if ViewMode.IsFreeMode then
            if ImGui.Begin ("Full Screen Enabled", ImGuiWindowFlags.NoNav ||| ImGuiWindowFlags.AlwaysAutoResize) then
                ImGui.Text "Free Mode (F11)"
                ImGui.SameLine ()
                let mutable freeMode = ViewMode.IsFreeMode
                if ImGui.Checkbox ("##freeMode", &freeMode) then
                    setViewMode (if freeMode then FreeMode else NormalMode) world
                if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                    ImGui.Text "Toggle free mode (F11 to toggle)."
                    ImGui.EndTooltip ()
                ImGui.Text "Capture Mode (F12)"
                ImGui.SameLine ()
                let mutable captureMode = ViewMode.IsCaptureMode
                if ImGui.Checkbox ("##captureMode", &captureMode) then
                    setViewMode (if captureMode then CaptureMode else NormalMode) world
                if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                    ImGui.Text "Toggle capture mode (F12 to toggle)."
                    ImGui.EndTooltip ()
            ImGui.End ()

    let private imGuiMainMenuWindow (world : World) =
        
        // menu window
        if ImGui.Begin ("Gaia", ImGuiWindowFlags.MenuBar ||| ImGuiWindowFlags.NoNavInputs) then
            
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
                    if not world.TimeAdvancing then
                        if ImGui.MenuItem ("Advance", "F5") then toggleTimeAdvancing world
                        if ImGui.MenuItem ("Step", "Alt+S") then step world
                    else
                        if ImGui.MenuItem ("Halt", "F5") then toggleTimeAdvancing world
                    if EditWhileAdvancing
                    then if ImGui.MenuItem ("Disable Edit while Advancing", "F6") then EditWhileAdvancing <- false
                    else if ImGui.MenuItem ("Enable Edit while Advancing", "F6") then EditWhileAdvancing <- true
                    if ImGui.MenuItem ("Create Restore Point", "F7") then createRestorePoint world
                    if ImGui.MenuItem ("Clear Log", "Alt+C") then LogStr <- ""
                    if ImGui.MenuItem ("Reset Layout", "Alt+L") then ImGuiIniResetRequested <- true
                    if ImGui.MenuItem ("Reload Assets", "F8") then ReloadAssetsRequested <- 1
                    if ImGui.MenuItem ("Reload Code", "F9") then ReloadCodeRequested <- (false, 1)
                    if ImGui.MenuItem ("Reload Code and Initialize", "Shift+F9") then ReloadCodeRequested <- (true, 1)
                    if ImGui.MenuItem ("Reload All", "Ctrl+R") then ReloadAllRequested <- (false, 1)
                    if ImGui.MenuItem ("Reload All and Initialize", "Ctrl+Shift+R") then ReloadAllRequested <- (true, 1)
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Show Inputs", "I") then ShowInputsDialog <- true
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Exit", "Alt+F4") then ShowConfirmExitDialog <- true
                    ImGui.EndMenu ()

                // screen menu
                if ImGui.BeginMenu "Screen" then
                    if ImGui.MenuItem ("Thaw Entities", "Ctrl+Shift+T") then thawEntities world
                    if ImGui.MenuItem ("Freeze Entities", "Ctrl+Shift+F") then freezeEntities world
                    if ImGui.MenuItem ("Rebuild Navigation", "Ctrl+Shift+N") then synchronizeNav world
                    if ImGui.MenuItem ("Rerender Light Maps", "Ctrl+Shift+L") then rerenderLightMaps world
                    ImGui.EndMenu ()

                // group menu
                if ImGui.BeginMenu "Group" then
                    if ImGui.MenuItem ("New Group", "Ctrl+N") then ShowNewGroupDialog <- true
                    if ImGui.MenuItem ("Open Group", "Ctrl+O") then ShowOpenGroupDialog <- true
                    if ImGui.MenuItem ("Save Group", "Ctrl+S") then
                        match Map.tryFind SelectedGroup GroupFilePaths with
                        | Some filePath -> GroupFileDialogState.FilePath <- filePath
                        | None -> GroupFileDialogState.FileName <- ""
                        ShowSaveGroupDialog <- true
                    if ImGui.MenuItem "Close Group" then
                        let groups = world |> World.getGroups SelectedScreen |> Set.ofSeq
                        if SelectedGroup.GetProtection world = Unprotected && Set.count groups > 1 then
                            snapshot CloseGroup world
                            let groupsRemaining = Set.remove SelectedGroup groups
                            selectEntities true Set.empty world
                            World.destroyGroupImmediate SelectedGroup world
                            GroupFilePaths <- Map.remove SelectedGroup GroupFilePaths
                            selectGroup true (Seq.head groupsRemaining)
                        else MessageBoxOpt <- Some "Cannot close protected or only group."
                    ImGui.EndMenu ()

                // entity menu
                if ImGui.BeginMenu "Entity" then
                    if ImGui.MenuItem ("Create Entity", "Ctrl+Enter") then createEntity false false None None world |> ignore<Entity>
                    if ImGui.MenuItem ("Delete Entity", "Delete") then tryDeleteSelectedEntity world |> ignore<bool>
                    let renamable = (match Seq.tryExactlyOne SelectedEntities with Some entity -> entity.GetProtection world = Unprotected | None -> false)
                    if ImGui.MenuItem ("Rename Entity", "F2", false, renamable) then ShowRenameEntityDialog <- true
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Cut Entity", "Ctrl+X") then tryCutSelectedEntity world |> ignore<bool>
                    if ImGui.MenuItem ("Copy Entity", "Ctrl+C") then tryCopySelectedEntity world |> ignore<bool>
                    if ImGui.MenuItem ("Paste Entity", "Ctrl+V") then tryPaste PasteAtLook (Option.map cast NewEntityParentOpt) world |> ignore<bool>
                    ImGui.Separator ()
                    if ImGui.MenuItem ("Open Entity", "Ctrl+Alt+O") then ShowOpenEntityDialog <- true
                    if ImGui.MenuItem ("Save Entity", "Ctrl+Alt+S", false, SelectedEntities.Count <= 1) then
                        match Seq.tryExactlyOne SelectedEntities with
                        | Some entity when entity.GetExists world ->
                            match Map.tryFind entity.EntityAddress EntityFilePaths with
                            | Some filePath -> EntityFileDialogState.FilePath <- filePath
                            | None -> EntityFileDialogState.FileName <- ""
                            ShowSaveEntityDialog <- true
                        | Some _ | None -> ()
                    let revertible = (match Seq.tryExactlyOne SelectedEntities with Some entity -> getEntityRevertible entity world | None -> false)
                    if ImGui.MenuItem ("Revert Entity", null, false, revertible) then tryRevertSelectedEntity world |> ignore<bool>
                    ImGui.Separator ()
                    if (Seq.tryExactlyOne SelectedEntities).IsSome && NewEntityParentOpt = Seq.tryExactlyOne SelectedEntities then
                        if ImGui.MenuItem ("Reset Creation Parent", "Ctrl+Shift+P") then
                            NewEntityParentOpt <- None
                            ShowEntityContextMenu <- false
                    else
                        if ImGui.MenuItem ("Set as Creation Parent", "Ctrl+P") then
                            NewEntityParentOpt <- Seq.tryExactlyOne SelectedEntities
                            ShowEntityContextMenu <- false
                    if ImGui.MenuItem ("Auto Bounds Entity", "Ctrl+B") then tryAutoBoundsSelectedEntity world |> ignore<bool>
                    if ImGui.MenuItem ("Move to Origin", "Ctrl+I") then tryMoveSelectedEntityToOrigin false world |> ignore<bool>
                    if ImGui.MenuItem ("Propagate Entity") then tryPropagateSelectedEntityStructure world |> ignore<bool>
                    if ImGui.MenuItem ("Wipe Propagation Targets") then tryWipeSelectedEntityPropagationTargets world |> ignore<bool>
                    ImGui.EndMenu ()

                // fin
                ImGui.EndMenuBar ()

            // tool bar
            if ImGui.Button "Create" then createEntity false false None None world |> ignore<Entity>
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 200.0f
            if ImGui.BeginCombo ("##newEntityDispatcherName", NewEntityDispatcherName, ImGuiComboFlags.HeightRegular) then
                let dispatcherNames = (World.getEntityDispatchers world).Keys
                let dispatcherNamePicked = tryPickName dispatcherNames
                for dispatcherName in dispatcherNames do
                    if ImGui.Selectable (dispatcherName, (dispatcherName = NewEntityDispatcherName)) then NewEntityDispatcherName <- dispatcherName
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
                    if ImGui.Selectable (overlayName, (overlayName = NewEntityOverlayName)) then NewEntityOverlayName <- overlayName
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
            if world.TimeHalted then
                if ImGui.Button "Advance (F5)" then toggleTimeAdvancing world
                ImGui.SameLine ()
                if ImGui.Button "Step" then step world
            else
                if ImGui.Button "Halt (F5)" then toggleTimeAdvancing world
                ImGui.SameLine ()
                ImGui.Checkbox ("Edit", &EditWhileAdvancing) |> ignore<bool>
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Manip:"
            ImGui.SameLine ()
            if ImGui.Button (if ManipulationWorld then "Wld" else "Obj") then
                ManipulationWorld <- not ManipulationWorld
            ImGui.SameLine ()
            ImGui.Text "Eye:"
            ImGui.SameLine ()
            if ImGui.Button "Reset" then ResetEyeRequested <- true
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
            if ImGui.Button "Code" then ReloadCodeRequested <- withSnd 1 ReloadCodeRequested
            ImGui.SameLine ()
            if ImGui.Button "All" then ReloadAllRequested <- withSnd 1 ReloadAllRequested
            ImGui.SameLine ()
            ImGui.Text "|"
            ImGui.SameLine ()
            ImGui.Text "Mode:"
            ImGui.SameLine ()
            ImGui.SetNextItemWidth 130.0f
            if ImGui.BeginCombo ("##projectEditMode", ProjectEditMode, ImGuiComboFlags.HeightRegular) then
                let editModes = World.getEditModes world
                for (editModeName, editModeFn) in editModes.Pairs do
                    if ImGui.Selectable (editModeName, (editModeName = ProjectEditMode)) then
                        ProjectEditMode <- editModeName
                        snapshot (SetEditMode 0) world // snapshot before mode change
                        selectEntities true Set.empty world
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
            if ImGui.Button "Renav" then synchronizeNav world
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

            // overlay mode
            ImGui.SameLine ()
            ImGui.Text "Overlay Mode"
            ImGui.SameLine ()
            let mutable overlayMode = OverlayMode
            if ImGui.Checkbox ("##overlayMode", &overlayMode) then toggleOverlayMode ()
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Toggle overlay mode (F10 to toggle)."
                ImGui.EndTooltip ()

            // free mode
            ImGui.SameLine ()
            ImGui.Text "Free Mode"
            ImGui.SameLine ()
            let mutable freeMode = ViewMode.IsFreeMode
            if ImGui.Checkbox ("##freeMode", &freeMode) then
                setViewMode (if freeMode then FreeMode else NormalMode) world
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Toggle free mode (F11 to toggle)."
                ImGui.EndTooltip ()

            // capture mode
            ImGui.SameLine ()
            ImGui.Text "Capture Mode"
            ImGui.SameLine ()
            let mutable captureMode = ViewMode.IsCaptureMode
            if ImGui.Checkbox ("##captureMode", &captureMode) then
                setViewMode (if captureMode then CaptureMode else NormalMode) world
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Toggle capture mode view (F12 to toggle)."
                ImGui.EndTooltip ()

        ImGui.End ()

    let private imGuiHierarchyWindow (world : World) =

        // track state for hot key input
        let entityHierarchyFocused =
            let mutable entityHierarchyFocused = false
            let windowName = "Entity Hierarchy"
            if ImGui.Begin windowName then

                // store focus, including its rows' child window
                entityHierarchyFocused <- ImGui.IsWindowFocused ImGuiFocusedFlags.RootAndChildWindows
                if entityHierarchyFocused && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName

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
                let functional = world.Functional
                if functional then
                    ImGui.SetNextItemWidth (-ImGui.GetFrameHeight () - ImGui.GetStyle().ItemSpacing.X) // leaving room for the checkbox
                ImGui.InputTextWithHint ("##entityHierarchySearchStr", "[enter search text]", &EntityHierarchySearchStr, 4096u) |> ignore<bool>
                if ImGui.IsItemFocused () then entityHierarchyFocused <- false
                if functional then
                    ImGui.SameLine ()
                    ImGui.Checkbox ("##entityHierarchyShowModified", &EntityHierarchyShowModified) |> ignore<bool>
                    if ImGui.IsItemHovered () && ImGui.BeginTooltip () then
                        ImGui.Text "Show Modified"
                        ImGui.EndTooltip ()

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
                        if ImGui.Selectable (group.Name, group.Name = selectedGroupName) then
                            selectEntities true Set.empty world
                            selectGroup true group
                        if group.Name = selectedGroupName then ImGui.SetItemDefaultFocus ()
                    ImGui.EndCombo ()
                if ImGui.BeginDragDropTarget () then
                    if NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr then
                        match DragDropPayloadOpt with
                        | Some (DragDropAsset (_, assetTag)) -> tryCreateEntityOfAsset false (Some (SelectedGroup :> Simulant)) assetTag world |> ignore<Entity option>
                        | Some _ | None -> ()
                    if NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "PropagationSource").NativePtr then
                        match DragDropPayloadOpt with
                        | Some (DragDropPropagationSource source) when source.GetExists world ->
                            createEntityOfPropagationSource PasteAtLook (Some (SelectedGroup :> Simulant)) source world |> ignore<Entity>
                        | Some _ | None -> ()
                    if NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr then
                        match DragDropPayloadOpt with
                        | Some (DragDropEntity sourceEntity) ->
                            let entities = getHierarchyDragEntities sourceEntity world
                            if ImGui.IsCtrlDown () // ctrl+drop duplicates, ctrl+shift without propagation
                            then tryDuplicateEntitiesInHierarchy entities SelectedGroup world
                            else tryDropEntitiesInHierarchy entities None None world
                        | Some _ | None -> ()
                    ImGui.EndDragDropTarget ()

                // entity editing, first 
                ImGui.BeginChild "Container" |> ignore<bool>

                // when showing modified entities, find them and their ancestors
                let shownOpt =
                    if EntityHierarchyShowModified then
                        let shown = HashSet<Entity> HashIdentity.Structural
                        for entity in World.getEntities SelectedGroup world do
                            if getEntityModified entity world then
                                let mutable ancestorOpt = Some entity
                                while (match ancestorOpt with Some ancestor -> shown.Add ancestor | None -> false) do
                                    ancestorOpt <- match ancestorOpt.Value.Parent with :? Entity as parent -> Some parent | _ -> None
                        Some shown
                    else None

                // draw the hierarchy
                let children =
                    World.getEntitiesSovereign SelectedGroup world
                    |> Array.ofSeq
                    |> Array.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity))
                    |> Array.sortBy fst
                    |> Array.map snd
                let multiSelectIo = ImGui.BeginMultiSelect (ImGuiMultiSelectFlags.NoSelectAll, SelectedEntities.Count, -1)
                applyEntityHierarchySelectionRequests multiSelectIo world // NOTE: resolves against last frame's items.
                EntityHierarchyIndexed.Clear ()
                for child in children do
                    imGuiEntityHierarchy shownOpt child world
                let multiSelectIo = ImGui.EndMultiSelect ()
                applyEntityHierarchySelectionRequests multiSelectIo world
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
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            if ImGui.BeginChild ("##history", v2 -1.0f -1.0f, ImGuiChildFlags.FrameStyle, ImGuiWindowFlags.NoNavInputs) then
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
            ImGui.EndChild ()
        ImGui.End ()

    let private imGuiGamePropertiesWindow world =
        let windowName = "Game Properties"
        let focused =
            if ImGui.Begin windowName then
                if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
                imGuiEditProperties Game world
                ImGui.IsWindowFocused ImGuiFocusedFlags.RootAndChildWindows
            else false
        ImGui.End ()
        focused

    let private imGuiScreenPropertiesWindow world =
        let windowName = "Screen Properties"
        let focused =
            if ImGui.Begin windowName then
                if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
                imGuiEditProperties SelectedScreen world
                ImGui.IsWindowFocused ImGuiFocusedFlags.RootAndChildWindows
            else false
        ImGui.End ()
        focused

    let private imGuiGroupPropertiesWindow world =
        let windowName = "Group Properties"
        let focused =
            if ImGui.Begin windowName then
                if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
                imGuiEditProperties SelectedGroup world
                ImGui.IsWindowFocused ImGuiFocusedFlags.RootAndChildWindows
            else false
        ImGui.End ()
        focused

    let private imGuiEntityPropertiesWindow world =
        let windowName = "Entity Properties"
        let focused =
            if ImGui.Begin windowName then
                if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
                match Seq.tryExactlyOne SelectedEntities with
                | Some entity when entity.GetExists world && SelectedEntities.Count = 1 -> imGuiEditProperties entity world
                | Some _ | None -> () // nothing for none, nor for several since they're not edited together
                ImGui.IsWindowFocused ImGuiFocusedFlags.RootAndChildWindows
            else false
        ImGui.End ()
        focused

    let private imGuiPropagationSourcesWindow world =
        let windowName = "Propagation Sources"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
            ImGui.SetNextItemWidth -1.0f
            ImGui.InputTextWithHint ("##propagationSourcesSearchStr", "[enter search text]", &PropagationSourcesSearchStr, 4096u) |> ignore<bool>
            let propagationSources =
                World.getPropagationSources world
                |> Seq.filter (fun entity ->
                    String.IsNullOrWhiteSpace PropagationSourcesSearchStr ||
                    entity.Name.ToLowerInvariant().Contains (PropagationSourcesSearchStr.ToLowerInvariant ()))
                |> Seq.filter (fun entity -> entity.GetProtection world = Unprotected)
                |> hashSetPlus HashIdentity.Structural
            ImGui.BeginChild ("Container", v2Zero, ImGuiChildFlags.None, ImGuiWindowFlags.NoNavInputs) |> ignore<bool>
            let selectionSize = if Seq.exists SelectedEntities.Contains propagationSources then 1 else 0
            let multiSelectIo = ImGui.BeginMultiSelect (ImGuiMultiSelectFlags.SingleSelect, selectionSize, -1)
            applyPropagationSourcesSelectionRequests multiSelectIo world // NOTE: resolves against last frame's items.
            PropagationSourcesIndexed.Clear ()
            for entity in propagationSources do
                let treeNodeFlags = ImGuiTreeNodeFlags.Leaf ||| if SelectedEntities.Contains entity then ImGuiTreeNodeFlags.Selected else ImGuiTreeNodeFlags.None
                ImGui.SetNextItemSelectionUserData (int64 PropagationSourcesIndexed.Count)
                PropagationSourcesIndexed.Add entity
                if ImGui.TreeNodeEx (entity.Name + "##" + scstringMemo entity, treeNodeFlags) then
                    if ImGui.BeginDragDropSource () then
                        DragDropPayloadOpt <- Some (DragDropPropagationSource entity)
                        ImGui.Text entity.Name
                        ImGui.SetDragDropPayload ("PropagationSource", IntPtr.Zero, 0u) |> ignore<bool>
                        ImGui.EndDragDropSource ()
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
                        createEntityOfPropagationSource PasteAtLook None entity world |> ignore<Entity>
                    if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                        ImGui.Text "Create a copy of the specified entity."
                        ImGui.EndTooltip ()
                    ImGui.PopID ()
                    match Seq.tryExactlyOne SelectedEntities with
                    | Some selectedEntity ->
                        let propagationSources =
                            [entity; yield! World.getEntityDescendants entity world]
                            |> List.map (fun entity -> entity.GetPropagationSourceOpt world)
                            |> List.definitize
                        if entity <> selectedEntity && not (List.contains selectedEntity propagationSources) then
                            ImGui.SameLine ()
                            ImGui.PushID ("##asChild" + scstringMemo entity)
                            if ImGui.SmallButton "as Child" then
                                snapshot DuplicateEntity world
                                let positionSnapEir = if Snaps2dSelected then Left (a__ Snaps2d) else Right (a__ Snaps3d)
                                let duplicate = World.pasteEntity NewEntityDistance RightClickPosition positionSnapEir PasteAtLook entity selectedEntity world
                                selectEntities true (Set.singleton duplicate) world
                            ImGui.PopID ()
                            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                                ImGui.Text "Create a copy of the specified entity as a child of the selected entity."
                                ImGui.EndTooltip ()
                            ImGui.SameLine ()
                            ImGui.PushID ("##atLocalOrigin" + scstringMemo entity)
                            if ImGui.SmallButton "at Local Origin" then
                                snapshot DuplicateEntity world
                                let positionSnapEir = if Snaps2dSelected then Left (a__ Snaps2d) else Right (a__ Snaps3d)
                                let duplicate = World.pasteEntity NewEntityDistance RightClickPosition positionSnapEir PasteAtLook entity selectedEntity world
                                duplicate.SetPositionLocal v3Zero world
                                selectEntities true (Set.singleton duplicate) world
                            ImGui.PopID ()
                            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                                ImGui.Text "Create as copy of the specified entity as a child of the selected entity at local origin."
                                ImGui.EndTooltip ()
                    | None -> ()
                    ImGui.TreePop ()
            let multiSelectIo = ImGui.EndMultiSelect ()
            applyPropagationSourcesSelectionRequests multiSelectIo world
            ImGui.EndChild ()
        ImGui.End ()

    let private imGuiOverlayerWindow () =
        let windowName = "Edit Overlayer"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            if ImGui.Button "Save" then
                let overlayerSourceDir = TargetDir + "/../../.."
                let overlayerFilePath = overlayerSourceDir + "/" + Assets.Global.OverlayerFilePath
                try let overlays = scvalue<Overlay list> OverlayerStr
                    let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                    File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
                with exn -> MessageBoxOpt <- Some ("Could not save overlayer due to: " + scstring exn)
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
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
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
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
            if ImGui.IsWindowFocused ImGuiFocusedFlags.RootAndChildWindows && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
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

                    // search the property value text
                    ImGui.SetNextItemWidth (ImGui.GetContentRegionAvail().X * 0.5f)
                    if PropertyValueSearchRequested then
                        ImGui.SetKeyboardFocusHere ()
                        PropertyValueSearchRequested <- false
                    let searchStrPrevious = PropertyValueSearchStr
                    if ImGui.InputTextWithHint ("##propertyValueSearchStr", "[search text]", &PropertyValueSearchStr, 4096u, ImGuiInputTextFlags.EnterReturnsTrue) then
                        searchPropertyValueStr (not (ImGui.IsShiftDown ())) propertyValueStr
                        PropertyValueSelectionRequestOpt <- None // enter finds the next match without leaving the search box
                        ImGui.SetKeyboardFocusHere (-1) // keep the search box active as enter would otherwise deactivate it
                    let searchTabbed = ImGui.IsItemActive () && ImGui.IsKeyPressed ImGuiKey.Tab && ImGui.IsShiftUp ()
                    if PropertyValueSearchStr <> searchStrPrevious then PropertyValueSearchIndex <- -1
                    if searchTabbed && PropertyValueSearchIndex >= 0 then // reselect the current match as the value text takes focus
                        PropertyValueSearchIndex <- dec PropertyValueSearchIndex
                        searchPropertyValueStr true propertyValueStr
                    ImGui.SameLine ()
                    if ImGui.Button "<" then searchPropertyValueStr false propertyValueStr
                    ImGui.SameLine ()
                    if ImGui.Button ">" then searchPropertyValueStr true propertyValueStr
                    if PropertyValueSearchStr.Length > 0 then
                        ImGui.SameLine ()
                        let count = countPropertyValueSearchMatches propertyValueStr
                        if PropertyValueSearchIndex >= 0 && PropertyValueSearchIndex + PropertyValueSearchStr.Length <= propertyValueStr.Length then
                            let ordinal = countPropertyValueSearchMatches (propertyValueStr.Substring (0, PropertyValueSearchIndex + PropertyValueSearchStr.Length))
                            ImGui.Text (string ordinal + " of " + string count + " found")
                        else ImGui.Text (string count + " found")
                    if searchTabbed then
                        ImGui.SetKeyboardFocusHere ()

                    // edit the property value text
                    let propertyValueFlags = ImGuiInputTextFlags.CallbackAlways
                    if  propertyDescriptor.PropertyName = Constants.Engine.FacetNamesPropertyName &&
                        propertyDescriptor.PropertyType = typeof<string Set> then
                        ImGui.InputTextMultiline ("##propertyValueStr", &propertyValueStr, 4096u, v2 -1.0f -1.0f, ImGuiInputTextFlags.ReadOnly ||| propertyValueFlags, propertyValueTextCallback) |> ignore<bool>
                    elif ImGui.InputTextMultiline ("##propertyValueStr", &propertyValueStr, 131072u, v2 -1.0f -1.0f, propertyValueFlags, propertyValueTextCallback) then
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
                    if ImGui.IsItemActive () && ImGui.IsKeyPressed ImGuiKey.Tab && ImGui.IsShiftDown () then
                        PropertyValueSearchRequested <- true

                    // accept an asset dropped onto the property value text
                    if isPropertyAssetTag && ImGui.BeginDragDropTarget () then
                        if NativePtr.notNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr then
                            match DragDropPayloadOpt with
                            | Some (DragDropAsset (assetTagStr, _)) ->
                                let pasts = Pasts
                                try let propertyValueEscaped = assetTagStr
                                    let propertyValueUnescaped = String.unescape propertyValueEscaped
                                    let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                    setPropertyValue false propertyValue propertyDescriptor simulant world
                                with _ ->
                                    Pasts <- pasts
                            | Some _ | None -> ()
                        ImGui.EndDragDropTarget ()
                with :? TargetException as exn ->
                    PropertyFocusedOpt <- None
                    Log.warn ("Encountered undesired exception due to likely logic problem in Nu: " + scstring exn)
            | Some _ | None -> ()
        ImGui.End ()

    let private imGuiMetricsWindow (world : World) =

        // metrics window
        let windowName = "Metrics"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
                
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
            ImGui.Text (string (Vulkan.Hl.getDrawCallCount ()))

            // draw instance count
            ImGui.Text "Draw Instance Count:"
            ImGui.SameLine ()
            ImGui.Text (string (Vulkan.Hl.getDrawInstanceCount ()))

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
                ImPlot.SetupAxesLimits (0.0, double (dec TimingsArray.Length), 0.0, 50.0)
                ImPlot.SetupAxes ("Frame", "Time (ms)", ImPlotAxisFlags.NoLabel ||| ImPlotAxisFlags.NoTickLabels, ImPlotAxisFlags.None)
                FrameTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Frame Time", &TimingsArray[0], TimingsArray.Length)
                MainThreadTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Main Thread", &TimingsArray[0], TimingsArray.Length)
                ImGuiTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("ImGui Time", &TimingsArray[0], TimingsArray.Length)
                RenderMessagesTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Render Msgs", &TimingsArray[0], TimingsArray.Length)
                UpdateTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Update Time", &TimingsArray[0], TimingsArray.Length)
                PhysicsTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Physics Time", &TimingsArray[0], TimingsArray.Length)
                MiscTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotLine ("Misc Time", &TimingsArray[0], TimingsArray.Length)
                GcTimings.CopyTo (TimingsArray, 0)
                ImPlot.PlotShaded ("GC Time", &TimingsArray[0], TimingsArray.Length)
                ImPlot.EndPlot ()

        // fin
        ImGui.End ()

    let private imGuiInteractiveWindow world =
        let windowName = "Interactive"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
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
                        "#r \"Astc-Encoder-CSharp.dll\"\n" +
                        "#r \"Box2D.NET.dll\"\n" +
                        "#r \"BCnEncoder.dll\"\n" +
                        "#r \"JoltPhysicsSharp.dll\"\n" +
                        "#r \"AssimpNet.dll\"\n" +
                        "#r \"BulletSharp.dll\"\n" +
                        "#r \"Csv.dll\"\n" +
                        "#r \"DotRecast.Recast.Toolset.dll\"\n" +
                        "#r \"FParsec.dll\"\n" +
                        "#r \"Magick.NET-Q8-AnyCPU.dll\"\n" +
                        "#r \"Pfim.dll\"\n" +
                        "#r \"SDL3-CS.dll\"\n" +
                        "#r \"SDL3_image-CS.dll\"\n" +
                        "#r \"SDL3_mixer-CS.dll\"\n" +
                        "#r \"SDL3_ttf-CS.dll\"\n" +
                        "#r \"TiledSharp.dll\"\n" +
                        "#r \"Twizzle.ImGui-Bundle.NET.dll\"\n" +
                        "#r \"Vortice.ShaderCompiler.dll\"\n" +
                        "#r \"Vortice.Vulkan.dll\"\n" +
                        "#r \"Vortice.VulkanMemoryAllocator.dll\"\n" +
                        "#r \"Prime.dll\"\n" +
                        "#r \"Nu.Math.dll\"\n" +
                        "#r \"Nu.dll\"\n" +
                        "#r \"Nu.Gaia.dll\"\n" +
                        (if projectDllPathValid && not FsiSessionHasProjectCode then "#r \"" + PathF.GetFileName ProjectDllPath + "\"\n" else "") +
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
                if InteractiveInputStr.Contains (nameof SelectedEntities) then FsiSession.AddBoundValue (nameof SelectedEntities, SelectedEntities)
                if InteractiveInputStr.Contains (nameof world) then FsiSession.AddBoundValue (nameof world, world)
                if File.Exists Constants.Gaia.InteractiveInputFilePath then
                    File.SetAttributes (Constants.Gaia.InteractiveInputFilePath, FileAttributes.None)
                    File.WriteAllText (Constants.Gaia.InteractiveInputFilePath, InteractiveInputStr)
                    File.SetAttributes (Constants.Gaia.InteractiveInputFilePath, FileAttributes.ReadOnly)
                let interactiveInputStr = InteractiveInputStr + ";;"
                match FsiSession.EvalInteractionNonThrowing (interactiveInputStr, Constants.Gaia.InteractiveInputFilePath) with
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
                    if errorStr.Length > 0
                    then InteractiveOutputStr <- InteractiveOutputStr + errorStr
                    else InteractiveOutputStr <- InteractiveOutputStr + Environment.NewLine + outStr
                    if noteFsiSessionLateBindingTypes () then // make any facets or dispatchers defined by the evaluation available, as with code reloading
                        selectEntities true Set.empty world // NOTE: makes sure old dispatcher doesn't hang around in old cached entity state.
                        focusPropertyOpt None world // drop any reference to old property type
                        World.updateLateBindings false FsiSession.DynamicAssemblies world
                | (Choice2Of2 exn, diags) ->
                    let exnStr = "Unhandled exception: " + scstring exn
                    InteractiveOutputStr <- InteractiveOutputStr + Environment.NewLine + exnStr
                    let diagsStr = diags |> Array.map _.Message |> String.join Environment.NewLine
                    if diagsStr.Length > 0 then
                        InteractiveOutputStr <- InteractiveOutputStr + Environment.NewLine + diagsStr
                InteractiveOutputStr <-
                    InteractiveOutputStr.Split Environment.NewLine
                    |> Array.filter (not << String.IsNullOrWhiteSpace)
                    |> String.join Environment.NewLine
                FsiErrorStream.GetStringBuilder().Clear() |> ignore<StringBuilder>
                FsiOutStream.GetStringBuilder().Clear() |> ignore<StringBuilder>
                toBottom <- true

            ImGui.SameLine ()
            if ImGui.Button "Clear" then InteractiveOutputStr <- ""
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Clear evaluation output"
                ImGui.EndTooltip ()
            if InteractiveInputFocusRequested then ImGui.SetKeyboardFocusHere (); InteractiveInputFocusRequested <- false
            let inputTextHeight = ImGui.GetContentRegionAvail().Y * 0.65f
            ImGui.InputTextMultiline ("##interactiveInputStr", &InteractiveInputStr, 131072u, v2 -1.0f inputTextHeight, if eval then ImGuiInputTextFlags.ReadOnly else ImGuiInputTextFlags.None) |> ignore<bool>
            if enter then InteractiveInputStr <- ""
            if eval || enter then InteractiveInputFocusRequested <- true
            ImGui.Separator ()
            ImGui.BeginChild ("##interactiveOutputStr", v2Zero, ImGuiChildFlags.None, ImGuiWindowFlags.AlwaysHorizontalScrollbar ||| ImGuiWindowFlags.AlwaysVerticalScrollbar ||| ImGuiWindowFlags.NoNavInputs) |> ignore<bool>
            ImGui.TextUnformatted InteractiveOutputStr
            if toBottom then ImGui.SetScrollHereY 1.0f
            ImGui.EndChild ()
        ImGui.End ()

    let private imGuiEventTracingWindow world =
        let windowName = "Event Tracing"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
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
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
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
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            let renderer3dConfig = World.getRenderer3dConfig world
            let mutable renderer3dEdited = false
            let mutable skyBoxEnabled = renderer3dConfig.SkyBoxEnabled
            let mutable lightMappingEnabled = renderer3dConfig.LightMappingEnabled
            let mutable lightShadowingEnabled = renderer3dConfig.LightShadowingEnabled
            let mutable sssEnabled = renderer3dConfig.SssEnabled
            let mutable ssaoEnabled = renderer3dConfig.SsaoEnabled
            let mutable ssaoSampleCount = renderer3dConfig.SsaoSampleCount
            let mutable ssvfEnabled = renderer3dConfig.SsvfEnabled
            let mutable ssrlEnabled = renderer3dConfig.SsrlEnabled
            let mutable ssrrEnabled = renderer3dConfig.SsrrEnabled
            let mutable bloomEnabled = renderer3dConfig.BloomEnabled
            let mutable depthOfFieldEnabled = renderer3dConfig.DepthOfFieldEnabled
            let mutable chromaticAberrationEnabled = renderer3dConfig.ChromaticAberrationEnabled
            let mutable fxaaEnabled = renderer3dConfig.FxaaEnabled
            let mutable fxaaSpanMax = renderer3dConfig.FxaaSpanMax
            let mutable fxaaReduceMinDivisor = renderer3dConfig.FxaaReduceMinDivisor
            let mutable fxaaReduceMulDivisor = renderer3dConfig.FxaaReduceMulDivisor
            renderer3dEdited <- ImGui.Checkbox ("Sky Box Enabled", &skyBoxEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Light Mapping Enabled", &lightMappingEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Light Shadowing Enabled", &lightShadowingEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Sss Enabled", &sssEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Ssao Enabled", &ssaoEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.SliderInt ("Ssao Sample Count", &ssaoSampleCount, 0, Constants.Render.SsaoSampleCountMax) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Ssvf Enabled", &ssvfEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Ssrl Enabled", &ssrlEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Ssrr Enabled", &ssrrEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Bloom Enabled", &bloomEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Depth Of Field Enabled", &depthOfFieldEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Chromatic Aberration Enabled", &chromaticAberrationEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.Checkbox ("Fxaa Enabled", &fxaaEnabled) || renderer3dEdited
            renderer3dEdited <- ImGui.InputFloat ("Fxaa Span Max", &fxaaSpanMax) || renderer3dEdited
            renderer3dEdited <- ImGui.InputFloat ("Fxaa Reduce Min Divisor", &fxaaReduceMinDivisor) || renderer3dEdited
            renderer3dEdited <- ImGui.InputFloat ("Fxaa Reduce Mul Divisor", &fxaaReduceMulDivisor) || renderer3dEdited
            if renderer3dEdited then
                let renderer3dConfig =
                    { SkyBoxEnabled = skyBoxEnabled
                      LightMappingEnabled = lightMappingEnabled
                      LightShadowingEnabled = lightShadowingEnabled
                      SssEnabled = sssEnabled
                      SsaoEnabled = ssaoEnabled
                      SsaoSampleCount = ssaoSampleCount
                      SsvfEnabled = ssvfEnabled
                      SsrlEnabled = ssrlEnabled
                      SsrrEnabled = ssrrEnabled
                      BloomEnabled = bloomEnabled
                      DepthOfFieldEnabled = depthOfFieldEnabled
                      ChromaticAberrationEnabled = chromaticAberrationEnabled
                      FxaaEnabled = fxaaEnabled
                      FxaaSpanMax = fxaaSpanMax
                      FxaaReduceMinDivisor = fxaaReduceMinDivisor
                      FxaaReduceMulDivisor = fxaaReduceMulDivisor }
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
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            ImGui.Text "Log:"
            ImGui.SameLine ()
            if ImGui.SmallButton "Clear" then LogStr <- ""
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Clear evaluation output (Alt+C)"
                ImGui.EndTooltip ()
            ImGui.BeginChild ("##outputBufferStr", v2Zero, ImGuiChildFlags.None, ImGuiWindowFlags.AlwaysHorizontalScrollbar ||| ImGuiWindowFlags.AlwaysVerticalScrollbar ||| ImGuiWindowFlags.NoNavInputs) |> ignore<bool>
            ImGui.TextUnformatted LogStr
            ImGui.EndChild ()
        if flash then for i in 0 .. dec 8 do ImGui.PopStyleColor ()
        ImGui.End ()

    let private imGuiEditorConfigWindow () =
        let windowName = "Editor"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            ImGui.Text "Transform Snapping"
            ImGui.SetNextItemWidth 50.0f
            let mutable index = if Snaps2dSelected then 0 else 1
            if ImGui.Combo ("##snapsSelection", &index, [|"2d"; "3d"|], 2) then
                match index with
                | 0 -> Snaps2dSelected <- true
                | _ -> Snaps2dSelected <- false
            if ImGui.IsItemHovered ImGuiHoveredFlags.DelayNormal && ImGui.BeginTooltip () then
                ImGui.Text "Use 2d or 3d snapping (F3 to switch)."
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
            ImGui.Text "Debug"
            ImGui.Checkbox ("Physics Debug Rendering (2d)", &PhysicsDebugRendering2d) |> ignore<bool>
            ImGui.Checkbox ("Physics Debug Rendering (3d)", &PhysicsDebugRendering3d) |> ignore<bool>
            if PhysicsDebugRendering3d then
                ImGui.Indent ()
                ImGui.Text "Distance"
                ImGui.SameLine ()
                ImGui.DragFloat ("##physicsDebugRenderingDistance3d", &PhysicsDebugRenderingDistance3d, SnapDrag, 1.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
                ImGui.Checkbox ("Height Maps", &PhysicsDebugRenderingHeightMaps3d) |> ignore<bool>
                ImGui.Unindent ()
            ImGui.Checkbox ("ImGui Debug Window", &ImGuiDebugWindow) |> ignore<bool>
        ImGui.End ()

    let private imGuiAssetViewerWindow world =
        let windowName = "Asset Viewer"
        if ImGui.Begin (windowName, ImGuiWindowFlags.NoNavInputs) then
            if ImGui.IsWindowFocused () && SelectedWindowRestoreRequested = 0 then SelectedWindowOpt <- Some windowName
            ImGui.SetNextItemWidth (ImGui.GetContentRegionAvail().X * 0.5f)
            let searchActivePrevious = not (String.IsNullOrWhiteSpace AssetViewerSearchStr)
            if AssetViewerSearchRequested then
                ImGui.SetKeyboardFocusHere ()
                AssetViewerSearchStr <- ""
                AssetViewerSearchRequested <- false
            ImGui.InputTextWithHint ("##assetViewerSearchStr", "[search text]", &AssetViewerSearchStr, 4096u) |> ignore<bool>
            let searchActiveCurrent = not (String.IsNullOrWhiteSpace AssetViewerSearchStr)
            let searchDeactivated = searchActivePrevious && not searchActiveCurrent
            ImGui.SameLine ()
            ImGui.SetNextItemWidth -1.0f
            if ImGui.InputTextWithHint ("##loadPackageName", "[package to load]", &LoadPackageName, 4096u, ImGuiInputTextFlags.EnterReturnsTrue) then
                Metadata.loadMetadataPackage LoadPackageName
                LoadPackageName <- ""
            ImGui.BeginChild ("Container", v2Zero, ImGuiChildFlags.None, ImGuiWindowFlags.NoNavInputs) |> ignore<bool>
            for packageEntry in Metadata.getMetadataPackagesLoaded () |> Array.sortWith (fun a b -> String.Compare (a.Key, b.Key, true)) do
                let flags = ImGuiTreeNodeFlags.OpenOnArrow
                if searchActiveCurrent then ImGui.SetNextItemOpen true
                if searchDeactivated then ImGui.SetNextItemOpen false
                if ImGui.TreeNodeEx (packageEntry.Key, flags) then
                    for assetEntry in packageEntry.Value |> Array.sortWith (fun a b -> String.Compare (a.Key, b.Key, true)) do
                        let packageName = packageEntry.Key
                        let assetName = assetEntry.Key
                        let retained = assetName.ToLowerInvariant().Contains(AssetViewerSearchStr.ToLowerInvariant ())
                        let nonIcon = not (Assets.Default.Icons.Contains (asset packageName assetName))
                        if retained && nonIcon then
                            let assetImageSize = v2Dup (ImGui.GetFontSize () + 3.0f)
                            let image =
                                match __c assetEntry.Value with
                                | RawMetadata -> Assets.Default.RawIconIcon
                                | TextureMetadata _ -> asset<Image> packageName assetName
                                | TileMapMetadata _ -> Assets.Default.TileMapIcon
                                | StaticModelMetadata _ -> Assets.Default.StaticModelIcon
                                | AnimatedModelMetadata _ -> Assets.Default.AnimatedModelIcon
                                | SoundMetadata -> Assets.Default.SoundIcon
                                | SongMetadata -> Assets.Default.SongIcon
                            match World.imGuiTryGetTextureId image world with
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
                                let packageNameText = if Symbol.shouldBeExplicit packageName then String.surround "\"" packageName else packageName
                                let assetNameText = if Symbol.shouldBeExplicit assetName then String.surround "\"" assetName else assetName
                                let assetTagStr = "[" + packageNameText + " " + assetNameText + "]"
                                DragDropPayloadOpt <- Some (DragDropAsset (assetTagStr, asset<obj> packageName assetName))
                                ImGui.Text assetTagStr
                                ImGui.SetDragDropPayload ("Asset", IntPtr.Zero, 0u) |> ignore<bool>
                                ImGui.EndDragDropSource ()
                            match __c assetEntry.Value with
                            | SoundMetadata ->
                                ImGui.SameLine ()
                                if ImGui.SmallButton "Play" then
                                    World.playSound 0.0f 0.0f 1.0f (asset packageName assetName) world
                            | _ -> ()
                            let assetTag = asset<obj> packageName assetName
                            if (tryGetAssetEntityDispatcher assetTag).IsSome then
                                ImGui.SameLine ()
                                if ImGui.SmallButton "Create" then
                                    tryCreateEntityOfAsset false None assetTag world |> ignore<Entity option>
                                match Seq.tryExactlyOne SelectedEntities with
                                | Some selectedEntity ->
                                    ImGui.SameLine ()
                                    if ImGui.SmallButton "as Child" then
                                        tryCreateEntityOfAsset false (Some (selectedEntity :> Simulant)) assetTag world |> ignore<Entity option>
                                    ImGui.SameLine ()
                                    if ImGui.SmallButton "at Local Origin" then
                                        match tryCreateEntityOfAsset false (Some (selectedEntity :> Simulant)) assetTag world with
                                        | Some entity -> entity.SetPositionLocal v3Zero world
                                        | None -> ()
                                | None -> ()
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
            let newProjectDllPath = newProjectDir + "/bin/" + Constants.Gaia.BuildName + "/" + Constants.Engine.TargetFramework + "/" + NewProjectName + ".dll"
            let validName = not (String.IsNullOrWhiteSpace NewProjectName) && (PathF.GetInvalidFileNameChars () |> Array.notExists NewProjectName.Contains)
            let validDirectory = not (Directory.Exists newProjectDir)
            if not validName then
                ImGui.Text "Invalid project name! It must not be empty or contain characters unusable as file names."
            elif not validDirectory then
                ImGui.Text "Project already exists!"
            elif ImGui.Button "Create" || ImGui.IsKeyReleased ImGuiKey.Enter then

                // choose a template, ensuring it exists
                let slnDir = PathF.GetFullPath (programDir + "/../../../../..")
                let (templateDir, editMode, shortName) =
                    match NewProjectType with
                    | "MMCC Empty" -> (PathF.GetFullPath (programDir + "/../../../../Nu.Template.Mmcc.Empty"), "Initial", "nu-template-mmcc-empty")
                    | "MMCC Game" -> (PathF.GetFullPath (programDir + "/../../../../Nu.Template.Mmcc.Game"), "Title", "nu-template-mmcc-game")
                    | "ImSim Empty" -> (PathF.GetFullPath (programDir + "/../../../../Nu.Template.ImSim.Empty"), "Initial", "nu-template-imsim-empty")
                    | "ImSim Game" -> (PathF.GetFullPath (programDir + "/../../../../Nu.Template.ImSim.Game"), "Title", "nu-template-imsim-game")
                    | _ -> failwithumf ()
                // work around https://github.com/dotnet/sdk/pull/55105 by manual normalization of drive letter
                let templateDir = if templateDir.Length > 1 && templateDir[1] = ':' then string (Char.ToUpperInvariant templateDir[0]) + templateDir.Substring 1 else templateDir
                if Directory.Exists templateDir then

                    // attempt to create project files
                    try Log.info ("Creating project '" + NewProjectName + "' in '" + projectsDir + "'...")

                        // install nu template (--force does in-place update)
                        Process.Start("dotnet", "new install \"" + templateDir + "\" --force").WaitForExit()

                        // instantiate nu template
                        Directory.CreateDirectory newProjectDir |> ignore<DirectoryInfo>
                        Process.Start("dotnet", "new " + shortName + " --name \"" + NewProjectName + "\" --output \"" + newProjectDir + "\"").WaitForExit() // no --force because we already checked directory existence

                        // substitute project guid in project file
                        Process.Start("dotnet", "sln \"" + slnDir + "\" add \"" + newProjectDir + "/" + NewProjectName + ".fsproj\"").WaitForExit()
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
        let title = "Choose a project... *EDITOR RESTART REQUIRED!*"
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, &ShowOpenProjectDialog, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Game Project:"
            ImGui.SameLine ()
            let gaiaDir = PathF.Normalize AppContext.BaseDirectory
            let openProjectDlls =
                match OpenProjectDllsOpt with
                | None ->
                    let openProjectDlls =
                        gaiaDir + "../../../../../Projects" // scan project directories for DLLs
                        |> PathF.GetFullPath
                        |> Directory.GetDirectories 
                        |> Array.collect (fun projectDir ->
                            let projectDir = PathF.Normalize projectDir
                            let projectName = PathF.GetFileName projectDir
                            let dllDir = projectDir + "/bin/" + Constants.Gaia.BuildName + "/" + Constants.Engine.TargetFramework
                            if Directory.Exists dllDir
                            then Directory.GetFiles (dllDir, projectName + ".dll") |> Array.map PathF.Normalize
                            else [||])
                    let openProjectNames =
                        Array.map PathF.GetFileNameWithoutExtension openProjectDlls
                    let (openProjectDlls, openProjectNames, openProjectIndex) =
                        if Array.contains ProjectDllPath openProjectDlls then
                            (openProjectDlls, openProjectNames, Array.IndexOf (openProjectDlls, ProjectDllPath))
                        elif String.isEmpty ProjectDllPath then // no selected item
                            (openProjectDlls, openProjectNames, -1)
                        else // ensure currently opened DLL is in the list even if from a different TargetFramework
                            let projectDllDirectories = ProjectDllPath.Split "/"
                            (Array.append openProjectDlls [|ProjectDllPath|],
                             Array.append openProjectNames [|PathF.GetFileNameWithoutExtension ProjectDllPath + " (" + projectDllDirectories[projectDllDirectories.Length - 2] + ")"|],
                             Array.length openProjectDlls)
                    OpenProjectDllsOpt <- Some openProjectDlls
                    OpenProjectNames <- openProjectNames
                    OpenProjectIndex <- openProjectIndex
                    openProjectDlls
                | Some openProjectDlls -> openProjectDlls
            ImGui.Combo ("##openProjectFileSelector", &OpenProjectIndex, OpenProjectNames, OpenProjectNames.Length) |> ignore<bool>
            ImGui.Text "Edit Mode:"
            ImGui.SameLine ()
            ImGui.InputText ("##openProjectEditMode", &OpenProjectEditMode, 4096u) |> ignore<bool>
            ImGui.Checkbox ("Use Imperative Execution (faster, but no Undo / Redo)", &OpenProjectImperativeExecution) |> ignore<bool>
            if ImGui.Button "Open" || ImGui.IsKeyReleased ImGuiKey.Enter then
                ShowOpenProjectDialog <- false
                let gaiaState = makeGaiaState openProjectDlls[OpenProjectIndex] (Some OpenProjectEditMode) true world
                try File.WriteAllText (gaiaDir + Constants.Gaia.StateFilePath, printGaiaState gaiaState)
                    Directory.SetCurrentDirectory gaiaDir
                    ShowRestartDialog <- true
                with _ ->
                    revertOpenProjectState world
                    Log.info "Could not save editor state and open project."
            if ImGui.IsKeyReleased ImGuiKey.Escape then
                revertOpenProjectState world
                ShowOpenProjectDialog <- false
            ImGui.EndPopup ()
            if not ShowOpenProjectDialog then OpenProjectDllsOpt <- None

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
                    if ImGui.Selectable (dispatcherName, (dispatcherName = NewGroupDispatcherName)) then NewGroupDispatcherName <- dispatcherName
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
                    selectEntities true Set.empty world
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
        GroupFileDialogState.FileDialogType <- OpenFileDialog
        if ImGui.FileDialog (&ShowOpenGroupDialog, GroupFileDialogState) then
            snapshot OpenGroup world
            let loaded = tryLoadGroup GroupFileDialogState.FilePath world
            ShowOpenGroupDialog <- not loaded

    let private imGuiSaveGroupDialog world =
        GroupFileDialogState.Title <- "Save a nugroup file..."
        GroupFileDialogState.FilePattern <- "*.nugroup"
        GroupFileDialogState.FileDialogType <- SaveFileDialog
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
        EntityFileDialogState.FileDialogType <- OpenFileDialog
        if ImGui.FileDialog (&ShowOpenEntityDialog, EntityFileDialogState) then
            let loaded = tryLoadEntity EntityFileDialogState.FilePath world
            ShowOpenEntityDialog <- not loaded

    let private imGuiSaveEntityDialog world =
        match Seq.tryExactlyOne SelectedEntities with
        | Some entity when entity.GetExists world ->
            EntityFileDialogState.Title <- "Save a nuentity file..."
            EntityFileDialogState.FilePattern <- "*.nuentity"
            EntityFileDialogState.FileDialogType <- SaveFileDialog
            if ImGui.FileDialog (&ShowSaveEntityDialog, EntityFileDialogState) then
                if not (PathF.HasExtension EntityFileDialogState.FilePath) then EntityFileDialogState.FilePath <- EntityFileDialogState.FilePath + ".nuentity"
                let saved = trySaveSelectedEntity EntityFileDialogState.FilePath world
                ShowSaveEntityDialog <- not saved
        | Some _ | None -> ShowSaveEntityDialog <- false

    let private imGuiRenameEntityDialog world =
        match Seq.tryExactlyOne SelectedEntities with
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
                    SelectedEntities <- SelectedEntities |> Set.remove entity |> Set.add entity'
                    ShowRenameEntityDialog <- false
                if ImGui.IsKeyReleased ImGuiKey.Escape then ShowRenameEntityDialog <- false
                ImGui.EndPopup ()
        | Some _ | None -> ShowRenameEntityDialog <- false

    let private imGuiDeleteEntityDialog world =
        match getSelectedEntityRoots false world with
        | _ :: _ ->
            let title = "Entity deletion confirmation..."
            ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
            let opening = not (ImGui.IsPopupOpen title)
            if opening then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &ShowDeleteEntityDialog, ImGuiWindowFlags.AlwaysAutoResize) then
                ImGui.Text "Selected entity is an entity propagation source."
                ImGui.Text "Select a deletion option:"
                if ImGui.Button "Wipe propagation targets and delete entity." then
                    deleteSelectedEntities true world
                    ShowDeleteEntityDialog <- false
                if ImGui.Button "Ignore propagation targets and delete entity (if you plan on replacing it)." then
                    deleteSelectedEntities false world
                    ShowDeleteEntityDialog <- false
                if ImGui.Button "Cancel deletion." || ImGui.IsKeyReleased ImGuiKey.Escape then
                    ShowDeleteEntityDialog <- false
                ImGui.EndPopup ()
        | [] -> ShowDeleteEntityDialog <- false

    let private imGuiCutEntityDialog world =
        match getSelectedEntityRoots false world with
        | _ :: _ ->
            let title = "Entity cut confirmation..."
            ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
            let opening = not (ImGui.IsPopupOpen title)
            if opening then ImGui.OpenPopup title
            if ImGui.BeginPopupModal (title, &ShowCutEntityDialog, ImGuiWindowFlags.AlwaysAutoResize) then
                ImGui.Text "Selected entity is an entity propagation source."
                ImGui.Text "Select a cut option:"
                if ImGui.Button "Wipe propagation targets and cut entity." || ImGui.IsKeyReleased ImGuiKey.Enter then
                    cutSelectedEntities true world
                    ShowCutEntityDialog <- false
                if ImGui.Button "Ignore propagation targets and cut entity (if you plan on pasting or replacing it)." then
                    cutSelectedEntities false world
                    ShowCutEntityDialog <- false
                if ImGui.Button "Cancel cut operation." || ImGui.IsKeyReleased ImGuiKey.Escape then
                    ShowCutEntityDialog <- false
                ImGui.EndPopup ()
        | [] -> ShowCutEntityDialog <- false

    let private imGuiInputsDialog () =
        let title = "Gaia Inputs"
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        ImGui.SetNextWindowSize (v2 1024.0f 720.0f, ImGuiCond.Appearing)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, &ShowInputsDialog) then
            let sections =
                [|("Eye travel, while the viewport has the keyboard",
                   [|("W / S", "Move forward / back.")
                     ("A / D", "Move left / right.")
                     ("E / Q", "Move up / down.")
                     ("Up / Down", "Look up / down.")
                     ("Left / Right", "Turn left / right.")
                     ("Hold Shift", "Move or turn slower.")
                     ("Hold Enter / Enter+Shift", "Move faster / fastest.")|])
                  ("Viewport mouse",
                   [|("Left click", "Select the entity under the mouse. Clicking the selected entity again cycles to those behind it.")
                     ("Shift+click / Ctrl+click", "Add to or remove from the selection.")
                     ("Left drag (2d)", "Move the selection.")
                     ("Alt+left drag (2d)", "Rotate the selection. Alt switches between rotating and moving mid-drag.")
                     ("Ctrl+left drag (2d)", "Duplicate and move the selection (Ctrl+Shift duplicates without propagation).")
                     ("Gizmo drag (3d)", "Translate the selection. Gizmo modifiers only take effect as the drag starts.")
                     ("Shift+gizmo drag", "Scale the selection.")
                     ("Alt+gizmo drag", "Rotate the selection.")
                     ("Hold X / Y / Z + gizmo drag", "Rotate the selection about that axis.")
                     ("Ctrl+gizmo drag", "Duplicate and translate the selection (Ctrl+Shift duplicates without propagation).")
                     ("Hold Ctrl mid entity drag", "Ignore snaps.")
                     ("Right click", "Open the context menu, selecting the entity under the mouse.")
                     ("Right drag", "Free look.")
                     ("Alt+right drag", "Orbit about the selection, or a point ahead when nothing's selected. Alt switches between orbiting and free look mid-drag.")
                     ("Middle drag", "Pan the 3d eye.")
                     ("Alt+middle drag", "Pan the 2d eye. Alt switches between 2d and 3d panning mid-drag.")
                     ("Wheel", "Move the eye forward / back.")
                     ("Hold Shift / Ctrl or Enter", "Pan or wheel slower / faster.")
                     ("Drop an asset or propagation source", "Create an entity from it.")|])
                  ("Entity hierarchy",
                   [|("Click / Ctrl+click / Shift+click", "Select / toggle / select a range.")
                     ("Arrow keys", "Move between entities.")
                     ("Enter", "Hand the keyboard back to the viewport.")
                     ("Double click", "Frame the entity, or the selection it's in.")
                     ("Right click", "Open the entity's context menu.")
                     ("Drag onto an entity", "Parent the dragged entities under it.")
                     ("Alt+drag onto an entity", "Place the dragged entities before it.")
                     ("Ctrl+drag onto an entity", "Duplicate the dragged entities under it (Ctrl+Shift without propagation).")
                     ("Drag onto the group selector", "Move the dragged entities to the top level (Ctrl duplicates them).")
                     ("Drop an asset or propagation source", "Create an entity from it.")|])
                  ("Property editor",
                   [|("Ctrl+F", "Search property values.")
                     ("Enter, in the search box", "Find the next match.")
                     ("Tab / Shift+Tab", "Go from the search box to the value text / back.")
                     ("Escape", "Clear the search.")
                     ("Drop an asset onto an asset property", "Set the property to it.")|])
                  ("Other windows",
                   [|("Double click a propagation source", "Move the eye to it.")
                     ("Ctrl+Enter / Ctrl+Shift+Enter", "Eval / enter, in the interactive window.")|])
                  ("Keyboard Shortcuts",
                   [|("I", "Show or hide this list.")
                     ("M", "Toggle manipulation between world and object space.")
                     ("Delete", "Delete the selected entity.")
                     ("Escape", "Clear search / deselect.")
                     ("Alt+Enter", "Toggle full screen.")
                     ("Alt+Up / Alt+Down", "Move the selected entity up or down among its siblings.")
                     ("Alt+S", "Step time by one frame.")
                     ("Alt+C", "Clear the log.")
                     ("Alt+L", "Reset the window layout.")
                     ("Ctrl+N / Ctrl+O / Ctrl+S", "New, open or save a group.")
                     ("Ctrl+Shift+O", "Open a project.")
                     ("Ctrl+Alt+O / Ctrl+Alt+S", "Open or save an entity.")
                     ("Ctrl+R / Ctrl+Shift+R", "Reload all / reload all and initialize.")
                     ("Ctrl+B", "Auto bounds the selected entity.")
                     ("Ctrl+I", "Move the selected entity to the origin.")
                     ("Ctrl+P / Ctrl+Shift+P", "Set / reset the creation parent.")
                     ("Ctrl+F", "Search the entity hierarchy, or property values when the property editor is focused.")
                     ("Ctrl+Shift+F / Ctrl+Shift+T", "Freeze / thaw all entities.")
                     ("Ctrl+Shift+N", "Rebuild navigation.")
                     ("Ctrl+Shift+L", "Rerender light maps.")
                     ("Ctrl+Z / Ctrl+Y", "Undo / redo.")
                     ("Ctrl+X / Ctrl+C", "Cut / copy the selected entity.")
                     ("Ctrl+V", "Paste an entity where the eye looks.")
                     ("Ctrl+Enter", "Create an entity.")
                     ("F2", "Rename the selected entity.")
                     ("F3", "Switch between 2d and 3d snaps.")
                     ("F5", "Advance or halt time.")
                     ("F6", "Toggle editing while advancing.")
                     ("F7", "Create a restore point.")
                     ("F8", "Reload assets.")
                     ("F9 / Shift+F9", "Reload code / reload code and initialize.")
                     ("F10", "Toggle overlay mode.")
                     ("F11", "Toggle free mode.")
                     ("F12", "Toggle capture mode.")
                     ("Alt+F4", "Exit Gaia.")|])|]
            for (section, inputs) in sections do
                ImGui.SeparatorText section
                if ImGui.BeginTable (section, 2, ImGuiTableFlags.RowBg) then
                    ImGui.TableSetupColumn ("Input", ImGuiTableColumnFlags.WidthFixed, 300.0f)
                    ImGui.TableSetupColumn ("Action", ImGuiTableColumnFlags.WidthStretch)
                    for (input, action) in inputs do
                        ImGui.TableNextRow ()
                        ImGui.TableNextColumn () |> ignore<bool>
                        ImGui.Text input
                        ImGui.TableNextColumn () |> ignore<bool>
                        ImGui.TextWrapped action
                    ImGui.EndTable ()
            if ImGui.IsKeyPressed (ImGuiKey.I, false) || ImGui.IsKeyReleased ImGuiKey.Escape then ShowInputsDialog <- false
            ImGui.EndPopup ()

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
                createEntity true false None None world |> ignore<Entity>
                ShowEntityContextMenu <- false
            ImGui.SameLine ()
            ImGui.SetNextItemWidth -1.0f
            if ImGui.BeginCombo ("##newEntityDispatcherName", NewEntityDispatcherName, ImGuiComboFlags.HeightRegular) then
                let dispatcherNames = (World.getEntityDispatchers world).Keys
                let dispatcherNamePicked = tryPickName dispatcherNames
                for dispatcherName in dispatcherNames do
                    if ImGui.Selectable (dispatcherName, (dispatcherName = NewEntityDispatcherName)) then
                        NewEntityDispatcherName <- dispatcherName
                    if Some dispatcherName = dispatcherNamePicked then ImGui.SetScrollHereY Constants.Gaia.HeightRegularPickOffset
                    if dispatcherName = NewEntityDispatcherName then ImGui.SetItemDefaultFocus ()
                ImGui.EndCombo ()
            let singular = SelectedEntities.Count <= 1 // what's done to one entity, or under one, isn't shown for several
            if (Seq.tryExactlyOne SelectedEntities).IsSome && singular then
                if ImGui.Button "Create as Child" then
                    createEntity true true None None world |> ignore<Entity>
                    ShowEntityContextMenu <- false
                ImGui.SameLine ()
                if ImGui.Button "at Local Origin" then
                    createEntity true true None None world |> ignore<Entity>
                    tryMoveSelectedEntityToOrigin true world |> ignore<bool>
                    ShowEntityContextMenu <- false
            if ImGui.Button "Delete" then
                tryDeleteSelectedEntity world |> ignore<bool>
                ShowEntityContextMenu <- false
            if (match Seq.tryExactlyOne SelectedEntities with Some entity -> entity.GetProtection world = Unprotected | None -> false) then
                ImGui.SameLine ()
                if ImGui.Button "Rename" then
                    ShowRenameEntityDialog <- true
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
            if (Seq.tryExactlyOne SelectedEntities).IsSome && singular then
                if ImGui.Button "Paste Entity as Child" then
                    tryPaste PasteAtMouse (Option.map cast (Seq.tryExactlyOne SelectedEntities)) world |> ignore<bool>
                    ShowEntityContextMenu <- false
                ImGui.SameLine ()
                ImGui.PushID "##pasteAsChildLocalOrigin" // NOTE: needed to differentiate from same-named button above.
                if ImGui.Button "at Local Origin" then
                    if tryPaste PasteAtMouse (Option.map cast (Seq.tryExactlyOne SelectedEntities)) world then
                        tryMoveSelectedEntityToOrigin true world |> ignore<bool>
                    ShowEntityContextMenu <- false
                ImGui.PopID ()
            if singular then
                ImGui.Separator ()
                if ImGui.Button "Open Entity" then
                    ShowOpenEntityDialog <- true
                    ShowEntityContextMenu <- false
                if ImGui.Button "Save Entity" then
                    match Seq.tryExactlyOne SelectedEntities with
                    | Some entity when entity.GetExists world ->
                        match Map.tryFind entity.EntityAddress EntityFilePaths with
                        | Some filePath -> EntityFileDialogState.FilePath <- filePath
                        | None -> EntityFileDialogState.FileName <- ""
                        ShowSaveEntityDialog <- true
                        ShowEntityContextMenu <- false
                    | Some _ | None -> ()
            ImGui.Separator ()
            if singular then
                if (Seq.tryExactlyOne SelectedEntities).IsSome && NewEntityParentOpt = Seq.tryExactlyOne SelectedEntities then
                    if ImGui.Button "Reset Creation Parent" then
                        NewEntityParentOpt <- None
                        ShowEntityContextMenu <- false
                elif ImGui.Button "Set as Creation Parent" then
                    NewEntityParentOpt <- Seq.tryExactlyOne SelectedEntities
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
            let operation = ViewportContext { RightClickPosition = RightClickPosition; EditContext = makeEditContext None None }
            World.editGame tautology operation Game world
            World.editScreen tautology operation SelectedScreen world
            World.editGroup tautology operation SelectedGroup world
            match Seq.tryExactlyOne SelectedEntities with
            | Some selectedEntity when singular -> World.editEntity tautology operation selectedEntity world
            | Some _ | None -> ()
            detectEyeChangedElsewhere world
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

    let private imGuiReloadingCodeDialog initializing world =
        let title = "Reloading code..."
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
            ImGui.EndPopup ()
        ReloadCodeRequested <- mapSnd inc ReloadCodeRequested
        if snd ReloadCodeRequested = 4 then // NOTE: takes multiple frames to see dialog.
            tryReloadCode initializing world
            ReloadCodeRequested <- (false, 0)

    let private imGuiReloadingAllDialog initializing world =
        let title = "Reloading assets and code..."
        ImGui.SetNextWindowPos (ImGui.MainViewportCenter, ImGuiCond.Appearing, v2Dup 0.5f)
        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
        if ImGui.BeginPopupModal (title, ImGuiWindowFlags.AlwaysAutoResize) then
            ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
            ImGui.EndPopup ()
        ReloadAllRequested <- mapSnd inc ReloadAllRequested
        if snd ReloadAllRequested = 4 then // NOTE: takes multiple frames to see dialog.
            tryReloadAll initializing world
            ReloadAllRequested <- (false, 0)

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
        if ImGui.BeginPopupModal title then
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

        // reference old world state
        let worldStateOld = world.CurrentState

        // store the world state snapshot for change detection for each group
        for group in World.getGroups1 world do
            if not (GroupWorldStates.ContainsKey group) then
                GroupWorldStates <- Map.add group worldStateOld GroupWorldStates

        // detect if eyes were changed somewhere other than in the editor (such as in gameplay code)
        detectEyeChangedElsewhere world

        // update styling
        ImGui.StyleAdobeInspired OverlayMode

        // enable global docking
        let dockNodeFlags = ImGuiDockNodeFlags.NoDockingOverCentralNode ||| ImGuiDockNodeFlags.PassthruCentralNode
        let dockSpaceId = ImGui.DockSpaceOverViewport (0u, ImGui.GetMainViewport (), dockNodeFlags)

        // enable keyboard nav in the windows that allow it (the entity hierarchy and the property windows)
        let io = ImGui.GetIO ()
        io.ConfigFlags <- io.ConfigFlags ||| ImGuiConfigFlags.NavEnableKeyboard
        io.ConfigNavCaptureKeyboard <- false
        let keyOwnerId = ImGui.GetID "Gaia"
        ImGuiInternal.setKeyOwner ImGuiKey.LeftAlt keyOwnerId
        ImGuiInternal.setKeyOwner ImGuiKey.RightAlt keyOwnerId
        if ImGui.IsAltDown () then
            for key in [ImGuiKey.UpArrow; ImGuiKey.DownArrow; ImGuiKey.LeftArrow; ImGuiKey.RightArrow] do
                ImGuiInternal.setKeyOwner key keyOwnerId

        // attempt to proceed with normal operation
        match RecoverableExceptionOpt with
        | None ->

            // update stepping state
            if Stepping then
                World.setTimeAdvancing false world
                Stepping <- false

            // use a generalized exception process
            try imGuiViewportManipulation world

                // windows
                let (entityHierarchyFocused, simulantPropertiesFocused) =
                    if not ViewMode.IsNormalMode then
                        imGuiFullScreenWindow world
                        (false, false)
                    else
                        imGuiMainMenuWindow world
                        let entityHierarchyFocused = imGuiHierarchyWindow world
                        ExpandEntityHierarchy <- false
                        CollapseEntityHierarchy <- false
                        imGuiTimelineWindow ()
                        let gamePropertiesFocused = imGuiGamePropertiesWindow world
                        let screenPropertiesFocused = imGuiScreenPropertiesWindow world
                        let groupPropertiesFocused = imGuiGroupPropertiesWindow world
                        let entityPropertiesFocused = imGuiEntityPropertiesWindow world
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
                        let simulantPropertiesFocused = gamePropertiesFocused || screenPropertiesFocused || groupPropertiesFocused || entityPropertiesFocused
                        (entityHierarchyFocused, simulantPropertiesFocused)

                // HACK: keep the arrow keys from navigating a focused property window, leaving them to eye travel
                // while tab still navigates, or to the text field being edited there
                if simulantPropertiesFocused then
                    let arrowOwnerIdOpt =
                        if canEditWithKeyboard world then Some keyOwnerId
                        elif io.WantTextInput then
                            let activeId = ImGuiInternal.getActiveId ()
                            if activeId <> 0u then Some activeId else None // 0 is any owner, which imgui disallows
                        else None
                    match arrowOwnerIdOpt with
                    | Some arrowOwnerId ->
                        for key in [ImGuiKey.UpArrow; ImGuiKey.DownArrow; ImGuiKey.LeftArrow; ImGuiKey.RightArrow] do
                            ImGuiInternal.setKeyOwner key arrowOwnerId
                    | None -> ()

                // prompt dialogs
                match MessageBoxOpt with
                | None ->
                    if ShowNewProjectDialog then imGuiNewProjectDialog world
                    if ShowOpenProjectDialog then imGuiOpenProjectDialog world
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
                    if ShowInputsDialog then imGuiInputsDialog ()
                    if ShowConfirmExitDialog then imGuiConfirmExitDialog world
                    if ShowRestartDialog then imGuiRestartDialog world
                | Some message -> imGuiMessageBoxDialog message

                // viewport context menu
                if ShowEntityContextMenu then imGuiViewportContext world

                // non-imgui input
                updateEntityContext world // before the eye drag ends, so it sees whether a right drag's panned the eye
                updateEyeDrag world
                if not entityHierarchyFocused then updateEyeTravel world
                updateEntityDrag world
                updateAssetDrag world
                updateHotkeys entityHierarchyFocused world

                // deferred imgui input
                updateResetEye ()

                // HACK: because ImGui.BeginDragDropViewport isn't available yet -
                // https://github.com/ocornut/imgui/issues/5204
                // we rely on the DragDropPayloadOpt value being Nonified with the following mouse input check.
                // TODO: P1: get rid of this ASAP so we can rely on ImGui.AcceptDragDropPayload exclusively.
                if not (ImGui.IsMouseDown ImGuiMouseButton.Left) then DragDropPayloadOpt <- None

                // reloading dialogs
                if ReloadAssetsRequested > 0 then imGuiReloadingAssetsDialog world
                if snd ReloadCodeRequested > 0 then imGuiReloadingCodeDialog (fst ReloadCodeRequested) world
                if snd ReloadAllRequested > 0 then imGuiReloadingAllDialog (fst ReloadAllRequested) world

                // attempt to update window viewport
                if OverlayMode then
                    let windowViewport = World.getWindowViewport world
                    let windowViewport = Viewport.makeWindow windowViewport.Bounds windowViewport.Bounds windowViewport.Outer.Size
                    World.setWindowViewport windowViewport world
                else
                    match ImGuiInternal.tryGetCentralDockNodeBounds dockSpaceId with
                    | Some insetImGui ->
                        let windowViewport = World.getWindowViewport world
                        let offset = (windowViewport.Outer.Size - windowViewport.Bounds.Size) / 2
                        let inset = box2i (insetImGui.Min + offset) insetImGui.Size
                        let windowViewport = Viewport.makeWindow inset windowViewport.Bounds windowViewport.Outer.Size
                        World.setWindowViewport windowViewport world
                    | None -> () // TODO: log here or something?

                // selected window restoration
                if SelectedWindowRestoreRequested > 0 then imGuiSelectedWindowRestoration ()

            // propagate exception to dialog
            with exn -> RecoverableExceptionOpt <- Some exn

        // exception handling dialog
        | Some exn -> imGuiExceptionDialog exn worldStateOld world

    let private imGuiRender (world : World) =

        // augmentative rendering while not in capture mode
        if not ViewMode.IsCaptureMode then

            // HACK: in order to successfully focus entity properties when clicking in the viewport in the current version
            // of Dear ImGui, we seem to have to the the window focus command AFTER normal processing.
            // NOTE: the keyboard's handed back to the viewport the frame after, so selecting brings entity properties
            // forward without its keyboard nav taking the keys gaia uses (#603).
            if ViewportFocusRequested then
                ImGui.SetWindowFocus "Viewport"
                ViewportFocusRequested <- false
            if EntityPropertiesFocusRequested then
                ImGui.SetWindowFocus "Entity Properties"
                EntityPropertiesFocusRequested <- false
                ViewportFocusRequested <- true

            // render light probes of the selected group in view
            let eyeFrustum = world.Eye3dFrustumView
            let entities = World.getLightProbesInFrustum eyeFrustum (HashSet ()) world
            let lightProbeModels =
                entities
                |> Seq.filter (fun entity -> entity.Group = SelectedGroup && entity.Group.GetEditing world && eyeFrustum.Intersects (entity.GetBounds world))
                |> Seq.map (fun light -> (light.GetAffineMatrix world, false, Omnipresent, None, Material.empty, MaterialProperties.defaultProperties))
                |> SList.ofSeq
            if SList.notEmpty lightProbeModels then
                World.enqueueRenderMessage3d
                    (RenderStaticModels
                        { StaticModels = lightProbeModels
                          StaticModel = Assets.Default.LightProbeModel
                          RenderType = DeferredRenderType
                          RenderPass = NormalPass })
                    world

            // render lights of the selected group in play
            let entities = World.getLights3dInFrustum eyeFrustum (HashSet ()) world
            let lightModels =
                entities
                |> Seq.filter (fun entity -> entity.Group = SelectedGroup && entity.Group.GetEditing world && eyeFrustum.Intersects (entity.GetBounds world))
                |> Seq.map (fun light -> (light.GetAffineMatrix world, false, Omnipresent, None, Material.empty, MaterialProperties.defaultProperties))
                |> SList.ofSeq
            if SList.notEmpty lightModels then
                World.enqueueRenderMessage3d
                    (RenderStaticModels
                        { StaticModels = lightModels
                          StaticModel = Assets.Default.LightbulbModel
                          RenderType = DeferredRenderType
                          RenderPass = NormalPass })
                    world

            // render selection highlights
            for entity in Seq.filter (fun (entity : Entity) -> entity.GetExists world) SelectedEntities do
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
                                      UserDefinedImages = UserDefinedImages.defaultImages
                                      PipelineName = Constants.Render.PipelineNameDefault
                                      Color = Color.One
                                      Blend = Transparent
                                      Emission = Color.Zero
                                      Flip = Unflipped
                                      UserDefinedSettings = UserDefinedSettings.defaultSettings }})
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
                        let mutable boundsMatrix = Matrix4x4.CreateAffine (position, rotation, scale)
                        World.enqueueRenderMessage3d
                            (RenderStaticModel
                                { ModelMatrix = boundsMatrix
                                  CastShadow = false
                                  Presence = Omnipresent
                                  InsetOpt = None
                                  Material = { Material.empty with ClippedOpt = ValueSome true }
                                  MaterialProperties = { MaterialProperties.defaultProperties with SpecularScalarOpt = ValueSome 0.0f }
                                  StaticModel = Assets.Default.HighlightModel
                                  RenderType = DeferredRenderType
                                  RenderPass = NormalPass })
                            world

    let private imGuiPostProcess (world : World) =

        // override local desired eye changes if eye was changed elsewhere
        synchronizeLocalDesiredEyeChanges world

        // handle any imgui ini reset request
        if ImGuiIniResetRequested then
            ImGui.LoadIniSettingsFromMemory (ImGuiIniFileStr.AsSpan ())
            ImGuiIniResetRequested <- false

    let rec private runWithCleanUpAndErrorProtection firstFrameCallbackOpt world =
        try World.runWithoutCleanUp tautology ignore ignore imGuiRender imGuiProcess imGuiPostProcess firstFrameCallbackOpt world
            World.cleanUp world
            Constants.Engine.ExitCodeSuccess
        with exn ->
            if tryUndo world then
                Futures <- [] // NOTE: clearing invalid futures.
                let wasTimeAdvancing = world.TimeAdvancing
                if wasTimeAdvancing then World.setTimeAdvancing false world
                let errorMsg =
                    "Unexpected exception!\n" +
                    "Rewound to previous world" + (if wasTimeAdvancing then " and halted." else ".") +
                    "\nError due to: " + exn.Message +
                    "\nStack trace:\n" + string exn.StackTrace
                Log.error errorMsg
                MessageBoxOpt <- Some errorMsg
                runWithCleanUpAndErrorProtection None world
            else
                let errorMsg = "Unexpected exception! Could not rewind world. Error due to: " + scstring exn
                Log.error errorMsg
                Constants.Engine.ExitCodeFailure

    let private runWithCleanUp gaiaState targetDir_ screen world =
        OpenProjectImperativeExecution <- gaiaState.ProjectImperativeExecution
        CloseProjectImperativeExecution <- gaiaState.ProjectImperativeExecution
        OverlayMode <- gaiaState.OverlayMode
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
        let world =
            if not gaiaState.ProjectFreshlyLoaded then
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
        ProjectDllPath <- gaiaState.ProjectDllPath
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
        let result = runWithCleanUpAndErrorProtection (Some ignore) world
        (FsiSession :> IDisposable).Dispose () // not sure why we have to cast here...
        FsiErrorStream.Dispose ()
        FsiInStream.Dispose ()
        FsiOutStream.Dispose ()
        result

    (* Public Functions *)

    /// Run Gaia.
    let run gaiaState targetDir plugin =

        // ensure imgui ini file exists and was created by Gaia before initialising imgui
        let imguiIniFilePath = targetDir + "/" + Constants.Gaia.ImGuiIniFilePath
        if  not (File.Exists imguiIniFilePath) ||
            (File.ReadAllLines imguiIniFilePath)[0] <> "[Window][Gaia]" then
            File.WriteAllText (imguiIniFilePath, ImGuiIniFileStr)

        // attempt to create SDL dependencies
        let windowSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let windowViewport = Viewport.makeWindow1 windowSize
        let geometryViewport = Viewport.makeGeometry windowViewport.Bounds.Size
        match tryMakeSdlDeps true windowSize with
        | Right (sdlConfig, sdlDeps) ->

            // make world config appropriate for Gaia
            let worldConfig =
                { Imperative = gaiaState.ProjectImperativeExecution
                  Accompanied = true
                  TimeAdvancing = false
                  FramePacing = false
                  TickDeltaOverride = None
                  ModeOpt = gaiaState.ProjectEditModeOpt
                  SdlConfig = sdlConfig }

            // attempt to create the world
            let screenAndWorldOpt =
                try let screenAndworld = makeWorld sdlDeps worldConfig windowSize geometryViewport windowViewport plugin
                    Right screenAndworld
                with exn ->
                    let gaiaDirPath = PathF.GetDirectoryName (Assembly.GetExecutingAssembly ()).Location
                    let stateFilePath = gaiaDirPath + "/" + Constants.Gaia.StateFilePath
                    try File.Delete stateFilePath with _ -> ()
                    Left ("Failed to create world due to:\n" + scstring exn + "\nDeleted " + stateFilePath + ". Restart Gaia to proceed with empty editor.")

            // attempt to initialize events for world and run
            match screenAndWorldOpt with
            | Right (screen, world) ->

                // subscribe to events related to editing
                World.subscribeLambda handleNuMouseButton Game.MouseLeftDownEvent Game world |> ignore
                World.subscribeLambda handleNuMouseButton Game.MouseLeftUpEvent Game world |> ignore
                World.subscribeLambda handleNuMouseButton Game.MouseMiddleDownEvent Game world |> ignore
                World.subscribeLambda handleNuMouseButton Game.MouseMiddleUpEvent Game world |> ignore
                World.subscribeLambda handleNuMouseButton Game.MouseRightDownEvent Game world |> ignore
                World.subscribeLambda handleNuMouseButton Game.MouseRightUpEvent Game world |> ignore
                World.subscribeLambda handleNuLifeCycleGroup (Game.LifeCycleEvent (nameof Group)) Game world |> ignore
                World.subscribeLambda handleNuSelectedScreenOptChange Game.PostSelectEvent Game world |> ignore
                World.subscribeLambda handleNuExitRequest Game.ExitRequestEvent Game world |> ignore

                // run the world
                runWithCleanUp gaiaState targetDir screen world

            // handle error
            | Left error -> Log.error error; Constants.Engine.ExitCodeFailure

        // handle error
        | Left error -> Log.error error; Constants.Engine.ExitCodeFailure