// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open DotRecast.Core.Numerics
open DotRecast.Detour
open DotRecast.Recast
open DotRecast.Recast.Geom
open DotRecast.Recast.Toolset.Builder
open DotRecast.Recast.Toolset.Tools
open Prime

[<AutoOpen>]
module WorldScreenModule =

    type Screen with

        member this.GetDispatcher world = World.getScreenDispatcher this world
        member this.Dispatcher = lensReadOnly (nameof this.Dispatcher) this this.GetDispatcher
        member this.GetModelGeneric<'a> world = World.getScreenModel<'a> this world
        member this.SetModelGeneric<'a> value world = World.setScreenModel<'a> false value this world |> snd'
        member this.ModelGeneric<'a> () = lens Constants.Engine.ModelPropertyName this this.GetModelGeneric<'a> this.SetModelGeneric<'a>
        member this.GetTransitionState world = World.getScreenTransitionState this world
        member this.SetTransitionState value world = World.setScreenTransitionState value this world |> snd'
        member this.TransitionState = lens (nameof this.TransitionState) this this.GetTransitionState this.SetTransitionState
        member this.GetIncoming world = World.getScreenIncoming this world
        member this.SetIncoming value world = World.setScreenIncoming value this world |> snd'
        member this.Incoming = lens (nameof this.Incoming) this this.GetIncoming this.SetIncoming
        member this.GetOutgoing world = World.getScreenOutgoing this world
        member this.SetOutgoing value world = World.setScreenOutgoing value this world |> snd'
        member this.Outgoing = lens (nameof this.Outgoing) this this.GetOutgoing this.SetOutgoing
        member this.GetSlideOpt world = World.getScreenSlideOpt this world
        member this.SetSlideOpt value world = World.setScreenSlideOpt value this world |> snd'
        member this.SlideOpt = lens (nameof this.SlideOpt) this this.GetSlideOpt this.SetSlideOpt
        member this.GetNav3d world = World.getScreenNav3d this world
        member this.Nav3d = lensReadOnly (nameof this.Nav3d) this this.GetNav3d
        member this.GetProtected world = World.getScreenProtected this world
        member this.Protected = lensReadOnly (nameof this.Protected) this this.GetProtected
        member this.GetPersistent world = World.getScreenPersistent this world
        member this.SetPersistent value world = World.setScreenPersistent value this world |> snd'
        member this.Persistent = lens (nameof this.Persistent) this this.GetPersistent this.SetPersistent
        member this.GetDestroying world = World.getScreenDestroying this world
        member this.Destroying = lensReadOnly (nameof this.Destroying) this this.GetDestroying
        member this.GetOrder world = World.getScreenOrder this world
        member this.Order = lensReadOnly (nameof this.Order) this this.GetOrder
        member this.GetId world = World.getScreenId this world
        member this.Id = lensReadOnly (nameof this.Id) this this.GetId

        member this.RegisterEvent = Events.RegisterEvent --> this
        member this.UnregisteringEvent = Events.UnregisteringEvent --> this
        member this.ChangeEvent propertyName = Events.ChangeEvent propertyName --> this
        member this.PreUpdateEvent = Events.PreUpdateEvent --> this
        member this.UpdateEvent = Events.UpdateEvent --> this
        member this.PostUpdateEvent = Events.PostUpdateEvent --> this
        member this.TimeUpdateEvent = Events.TimeUpdateEvent --> this
        member this.SelectEvent = Events.SelectEvent --> this
        member this.DeselectingEvent = Events.DeselectingEvent --> this
        member this.IncomingStartEvent = Events.IncomingStartEvent --> this
        member this.IncomingFinishEvent = Events.IncomingFinishEvent --> this
        member this.OutgoingStartEvent = Events.OutgoingStartEvent --> this
        member this.OutgoingFinishEvent = Events.OutgoingFinishEvent --> this

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world =
            let mutable property = Unchecked.defaultof<_>
            let found = World.tryGetScreenProperty (propertyName, this, world, &property)
            if found then Some property else None

        /// Get a property value and type.
        member this.GetProperty propertyName world =
            World.getScreenProperty propertyName this world

        /// Get an xtension property value.
        member this.TryGet<'a> propertyName world : 'a =
            World.tryGetScreenXtensionValue<'a> propertyName this world

        /// Get an xtension property value.
        member this.Get<'a> propertyName world : 'a =
            World.getScreenXtensionValue<'a> propertyName this world

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world =
            World.trySetScreenProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world =
            World.setScreenProperty propertyName property this world |> snd'

        /// To try set an xtension property value.
        member this.TrySet<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.trySetScreenXtensionProperty propertyName property this world

        /// Set an xtension property value.
        member this.Set<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.setScreenXtensionProperty propertyName property this world

        /// Check that a screen is in an idling state (not transitioning in nor out).
        member this.Idling world =
            match this.GetTransitionState world with
            | IdlingState _ -> true
            | _ -> false

        /// Check that a screen is selected.
        member this.Selected world =
            let gameState = World.getGameState Game.Handle world
            match gameState.OmniScreenOpt with
            | Some omniScreen when this.Name = omniScreen.Name -> true
            | _ ->
                match gameState.SelectedScreenOpt with
                | Some screen when this.Name = screen.Name -> true
                | _ -> false

        /// Check that a screen exists in the world.
        member this.Exists world = World.getScreenExists this world

        /// Check that a screen dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a screen dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Get a screen's change event address.
        member this.GetChangeEvent propertyName = this.ChangeEvent propertyName

        /// Send a signal to a screen.
        member this.Signal signal world = (this.GetDispatcher world).Signal (signal, this, world)

    type World with

        static member internal preUpdateScreen (screen : Screen) world =

            // pre-update via dispatcher
            let dispatcher = World.getScreenDispatcher screen world
            let world = dispatcher.PreUpdate (screen, world)

            // publish pre-update event
            let eventTrace = EventTrace.debug "World" "preUpdateScreen" "" EventTrace.empty
            World.publishPlus () screen.PreUpdateEvent eventTrace screen false false world

        static member internal updateScreen (screen : Screen) world =

            // update via dispatcher
            let dispatcher = World.getScreenDispatcher screen world
            let world = dispatcher.Update (screen, world)

            // publish update event
            let eventTrace = EventTrace.debug "World" "updateScreen" "" EventTrace.empty
            World.publishPlus () screen.UpdateEvent eventTrace screen false false world

        static member internal postUpdateScreen (screen : Screen) world =

            // post-update via dispatcher
            let dispatcher = World.getScreenDispatcher screen world
            let world = dispatcher.PostUpdate (screen, world)

            // publish post-update event
            let eventTrace = EventTrace.debug "World" "postUpdateScreen" "" EventTrace.empty
            World.publishPlus () screen.PostUpdateEvent eventTrace screen false false world

        static member internal renderScreen renderPass (screen : Screen) world =

            // render via dispatcher
            let dispatcher = screen.GetDispatcher world
            dispatcher.Render (renderPass, screen, world)

        /// Edit a screen with the given operation using the ImGui APIs.
        /// Intended only to be called by editors like Gaia.
        static member editScreen operation (screen : Screen) world =
            let dispatcher = screen.GetDispatcher world
            dispatcher.Edit (operation, screen, world)

        /// Attempt to truncate a model.
        static member tryTruncateScreenModel<'model> (model : 'model) (screen : Screen) world =
            let dispatcher = screen.GetDispatcher world
            dispatcher.TryTruncateModel<'model> model

        /// Attempt to untruncate a model.
        static member tryUntruncateScreenModel<'model> (model : 'model) (screen : Screen) world =
            let dispatcher = screen.GetDispatcher world
            dispatcher.TryUntruncateModel<'model> (model, screen, world)

        /// Get all the screens in the world.
        static member getScreens world =
            let simulants = World.getSimulants world
            match simulants.TryGetValue (Game.Handle :> Simulant) with
            | (true, screensOpt) ->
                match screensOpt with
                | Some screens -> screens |> Seq.map cast<Screen>
                | None -> Seq.empty
            | (false, _) -> Seq.empty

        /// Set the dissolve aspects of a screen.
        static member setScreenDissolve dissolveDescriptor songOpt (screen : Screen) world =
            let dissolveImageOpt = Some dissolveDescriptor.DissolveImage
            let world = screen.SetIncoming { Transition.make Incoming with TransitionLifeTime = dissolveDescriptor.IncomingTime; DissolveImageOpt = dissolveImageOpt; SongOpt = songOpt } world
            let world = screen.SetOutgoing { Transition.make Outgoing with TransitionLifeTime = dissolveDescriptor.OutgoingTime; DissolveImageOpt = dissolveImageOpt; SongOpt = songOpt } world
            world

        /// Destroy a screen in the world immediately. Can be dangerous if existing in-flight publishing depends on the
        /// screen's existence. Consider using World.destroyScreen instead.
        static member destroyScreenImmediate (screen : Screen) world =
            let world = World.tryRemoveSimulantFromDestruction screen world
            EventGraph.cleanEventAddressCache screen.ScreenAddress
            if World.getScreenExists screen world then
                let groups = World.getGroups screen world
                let world = World.unregisterScreen screen world
                let world = World.removeTasklets screen world
                let world = World.destroyGroupsImmediate groups world
                World.removeScreenState screen world
            else world

        /// Destroy a screen in the world at the end of the current update.
        static member destroyScreen (screen : Screen) world =
            World.addSimulantToDestruction screen world

        /// Create a screen and add it to the world.
        static member createScreen3 dispatcherName nameOpt world =
            let dispatchers = World.getScreenDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find ScreenDispatcher named '" + dispatcherName + "'.")
            let screenState = ScreenState.make world.GameTime nameOpt dispatcher
            let screenState = Reflection.attachProperties ScreenState.copy screenState.Dispatcher screenState world
            let screen = Game.Handle / screenState.Name
            let world =
                if World.getScreenExists screen world then
                    if screen.GetDestroying world
                    then World.destroyScreenImmediate screen world
                    else failwith ("Screen '" + scstring screen + "' already exists and cannot be created."); world
                else world
            let world = World.addScreen false screenState screen world
            (screen, world)

        /// Create a screen from a simulant descriptor.
        static member createScreen2 descriptor world =
            let (screen, world) =
                let screenNameOpt =
                    match descriptor.SimulantSurnamesOpt with
                    | None -> None
                    | Some [|name|] -> Some name
                    | Some _ -> failwith "Screen cannot have multiple names."
                World.createScreen3 descriptor.SimulantDispatcherName screenNameOpt world
            let world =
                List.fold (fun world (propertyName, property) ->
                    World.setScreenProperty propertyName property screen world |> snd')
                    world descriptor.SimulantProperties
            let world =
                List.fold (fun world childDescriptor ->
                    World.createGroup3 childDescriptor screen world |> snd)
                    world descriptor.SimulantChildren
            (screen, world)

        /// Create a screen and add it to the world.
        static member createScreen<'d when 'd :> ScreenDispatcher> nameOpt world =
            World.createScreen3 typeof<'d>.Name nameOpt world

        /// Create a screen with a dissolving transition, and add it to the world.
        static member createDissolveScreen5 dispatcherName nameOpt dissolveDescriptor songOpt world =
            let (screen, world) = World.createScreen3 dispatcherName nameOpt world
            let world = World.setScreenDissolve dissolveDescriptor songOpt screen world
            (screen, world)
        
        /// Create a screen with a dissolving transition, and add it to the world.
        static member createDissolveScreen<'d when 'd :> ScreenDispatcher> nameOpt dissolveDescriptor songOpt world =
            World.createDissolveScreen5 typeof<'d>.Name nameOpt dissolveDescriptor songOpt world

        /// Write a screen to a screen descriptor.
        static member writeScreen writePropagationHistory (screenDescriptor : ScreenDescriptor) screen world =
            let screenState = World.getScreenState screen world
            let screenDispatcherName = getTypeName screenState.Dispatcher
            let screenDescriptor = { screenDescriptor with ScreenDispatcherName = screenDispatcherName }
            let getScreenProperties = Reflection.writePropertiesFromTarget tautology3 screenDescriptor.ScreenProperties screenState
            let screenDescriptor = { screenDescriptor with ScreenProperties = getScreenProperties }
            let groups = World.getGroups screen world
            { screenDescriptor with GroupDescriptors = World.writeGroups writePropagationHistory groups world }

        /// Write multiple screens to a game descriptor.
        static member writeScreens writePropagationHistory screens world =
            screens |>
            Seq.sortBy (fun (screen : Screen) -> screen.GetOrder world) |>
            Seq.filter (fun (screen : Screen) -> screen.GetPersistent world) |>
            Seq.fold (fun screenDescriptors screen -> World.writeScreen writePropagationHistory ScreenDescriptor.empty screen world :: screenDescriptors) [] |>
            Seq.rev |>
            Seq.toList

        /// Write a screen to a file.
        static member writeScreenToFile writePropagationHistory (filePath : string) screen world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<GameDescriptor>).PrettyPrinter
            let screenDescriptor = World.writeScreen writePropagationHistory ScreenDescriptor.empty screen world
            let screenDescriptorStr = scstring screenDescriptor
            let screenDescriptorPretty = PrettyPrinter.prettyPrint screenDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, screenDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a screen from a screen descriptor.
        static member readScreen screenDescriptor nameOpt world =

            // make the dispatcher
            let dispatcherName = screenDescriptor.ScreenDispatcherName
            let dispatchers = World.getScreenDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find a ScreenDispatcher named '" + dispatcherName + "'.")

            // make the screen state and populate its properties
            let screenState = ScreenState.make world.GameTime None dispatcher
            let screenState = Reflection.attachProperties ScreenState.copy screenState.Dispatcher screenState world
            let screenState = Reflection.readPropertiesToTarget ScreenState.copy screenDescriptor.ScreenProperties screenState

            // apply the name if one is provided
            let screenState =
                match nameOpt with
                | Some name -> { screenState with Name = name }
                | None -> screenState

            // add the screen's state to the world
            let screen = Screen (ntoa screenState.Name)
            let world = World.addScreen true screenState screen world
            
            // read the screen's groups
            let world = World.readGroups screenDescriptor.GroupDescriptors screen world |> snd
            (screen, world)

        /// Read multiple screens from a game descriptor.
        static member readScreens screenDescriptors world =
            let (screensRev, world) =
                List.fold
                    (fun (screens, world) screenDescriptor ->
                        let screenNameOpt = ScreenDescriptor.getNameOpt screenDescriptor
                        let (screen, world) = World.readScreen screenDescriptor screenNameOpt world
                        (screen :: screens, world))
                    ([], world)
                    screenDescriptors
            (List.rev screensRev, world)

        /// Read a screen from a file.
        static member readScreenFromFile (filePath : string) nameOpt world =
            let screenDescriptorStr = File.ReadAllText filePath
            let screenDescriptor = scvalue<ScreenDescriptor> screenDescriptorStr
            World.readScreen screenDescriptor nameOpt world

        /// Apply a screen behavior to a screen.
        static member applyScreenBehavior setScreenSlide behavior (screen : Screen) world =
            match behavior with
            | Vanilla ->
                world
            | Dissolve (dissolveDescriptor, songOpt) ->
                World.setScreenDissolve dissolveDescriptor songOpt screen world
            | Slide (dissolveDescriptor, slideDescriptor, songOpt, destination) ->
                let world = World.setScreenDissolve dissolveDescriptor songOpt screen world
                setScreenSlide slideDescriptor destination screen world
            | OmniScreen ->
                World.setOmniScreen screen world

        static member internal getNav3dDescriptors contents =
            [for (bounds, affineMatrix, staticModel, surfaceIndex, content) in contents do
                match content with
                | NavShape.EmptyNavShape -> ()
                | NavShape.BoundsNavShape -> Left bounds
                | NavShape.StaticModelSurfaceNavShape ->
                    match Metadata.tryGetStaticModelMetadata staticModel with
                    | Some physicallyBasedModel ->
                        if surfaceIndex >= 0 && surfaceIndex < physicallyBasedModel.Surfaces.Length then
                            Right (bounds, affineMatrix, physicallyBasedModel.Surfaces.[surfaceIndex])
                    | None -> ()
                | NavShape.StaticModelNavShape ->
                    match Metadata.tryGetStaticModelMetadata staticModel with
                    | Some physicallyBasedModel ->
                        for surface in physicallyBasedModel.Surfaces do
                            Right (bounds, affineMatrix, surface)
                    | None -> ()]

        static member internal tryBuildNav3dMesh contents config =

            // attempt to create a 3d input geometry provider
            let geomProviderOpt =
                match World.getNav3dDescriptors contents with
                | [] -> None
                | descriptors ->

                    // attempt to compute bounds and vertices
                    let mutable boundsOpt = None
                    let vertices =
                        [|for descriptor in descriptors do
                            match descriptor with
                            | Left bounds ->
                                match boundsOpt with
                                | None -> boundsOpt <- Some bounds
                                | Some (bounds' : Box3) -> boundsOpt <- Some (bounds'.Combine bounds)
                                let corners = bounds.Corners
                                for corner in corners do
                                    corner.X; corner.Y; corner.Z
                            | Right (bounds, affineMatrix : Matrix4x4, surface) ->
                                let geometry = surface.PhysicallyBasedGeometry
                                match boundsOpt with
                                | None -> boundsOpt <- Some bounds
                                | Some (bounds' : Box3) -> boundsOpt <- Some (bounds'.Combine bounds)
                                if geometry.PrimitiveType = OpenGL.PrimitiveType.Triangles then
                                    for v in geometry.Vertices do
                                        let v' = Vector3.Transform (v, affineMatrix)
                                        v'.X; v'.Y; v'.Z|]

                    // compute indices
                    let mutable offset = 0
                    let indices =
                        [|for descriptor in descriptors do
                            match descriptor with
                            | Left _ ->
                                // as the corners are ordered in Box3.Corners...
                                //
                                //     6--------7
                                //    /|       /|
                                //   / |      / |
                                //  5--------4  |
                                //  |  0-----|--3
                                //  | /      | /
                                //  |/       |/
                                //  1--------2
                                //
                                offset + 2; offset + 3; offset + 7 // right
                                offset + 2; offset + 7; offset + 4
                                offset + 0; offset + 1; offset + 5 // left
                                offset + 0; offset + 5; offset + 6
                                offset + 4; offset + 5; offset + 6 // top
                                offset + 4; offset + 6; offset + 7
                                offset + 0; offset + 1; offset + 2 // bottom
                                offset + 0; offset + 2; offset + 3
                                offset + 0; offset + 3; offset + 7 // back
                                offset + 0; offset + 7; offset + 6
                                offset + 1; offset + 2; offset + 4 // front
                                offset + 1; offset + 4; offset + 5
                                offset <- offset + 8
                            | Right (_, _, surface) ->
                                let geometry = surface.PhysicallyBasedGeometry
                                if geometry.PrimitiveType = OpenGL.PrimitiveType.Triangles then
                                    for i in geometry.Indices do
                                        i + offset
                                offset <- offset + geometry.Vertices.Length|]

                    // attempt to create geometry provider
                    match boundsOpt with
                    | Some bounds when vertices.Length >= 3 && indices.Length >= 3 ->
                        let provider = Nav3dInputGeomProvider (vertices, indices, bounds)
                        Some (provider :> IInputGeomProvider)
                    | Some _ | None -> None

            // attempt to execute 3d navigation mesh construction steps
            match geomProviderOpt with
            | Some geomProvider ->
                let rcConfig =
                    RcConfig
                        (config.PartitionType,
                         config.CellSize, config.CellHeight,
                         config.AgentSlopeMax, config.AgentHeight, config.AgentRadius, config.AgentClimbMax,
                         config.RegionSizeMin, config.RegionSizeMerge,
                         config.EdgeLengthMax, config.EdgeErrorMax,
                         config.VertsPerPolygon, config.DetailSampleDistance, config.DetailSampleErrorMax,
                         config.FilterLowHangingObstacles, config.FilterLedgeSpans, config.FilterWalkableLowHeightSpans,
                         SampleAreaModifications.SAMPLE_AREAMOD_WALKABLE, true)
                let rcBuilderConfig = RcBuilderConfig (rcConfig, geomProvider.GetMeshBoundsMin (), geomProvider.GetMeshBoundsMax ())
                let rcBuilder = RcBuilder ()
                let rcBuilderResult = rcBuilder.Build (geomProvider, rcBuilderConfig)
                let dtCreateParams = DemoNavMeshBuilder.GetNavMeshCreateParams (geomProvider, config.CellSize, config.CellHeight, config.AgentHeight, config.AgentRadius, config.AgentClimbMax, rcBuilderResult)
                match DtNavMeshBuilder.CreateNavMeshData dtCreateParams with
                | null -> None // some sort of argument issue
                | dtMeshData ->
                    DemoNavMeshBuilder.UpdateAreaAndFlags dtMeshData |> ignore<DtMeshData> // ignoring flow-syntax
                    let dtNavMesh = DtNavMesh (dtMeshData, 6, 0) // TODO: introduce constant?
                    let dtQuery = DtNavMeshQuery dtNavMesh
                    Some (rcBuilderResult, dtNavMesh, dtQuery)

            // geometry not found
            | None -> None

        static member internal setNav3dBodyOpt contentOpt (source : Entity) world =
            let screen = source.Screen
            let nav3d = World.getScreenNav3d screen world
            match (nav3d.Nav3dBodies.TryFind source, contentOpt) with
            | (Some body, Some body') ->
                if body' <> body then // OPTIMIZATION: preserve map reference if no content changes detected.
                    let nav3d = { nav3d with Nav3dBodies = Map.add source body' nav3d.Nav3dBodies }
                    World.setScreenNav3d nav3d screen world |> snd'
                else world
            | (None, Some body) ->
                let nav3d = { nav3d with Nav3dBodies = Map.add source body nav3d.Nav3dBodies }
                World.setScreenNav3d nav3d screen world |> snd'
            | (Some _, None) ->
                let nav3d = { nav3d with Nav3dBodies = Map.remove source nav3d.Nav3dBodies }
                World.setScreenNav3d nav3d screen world |> snd'
            | (None, None) -> world

        /// Set the given screen's 3d navigation configuration.
        static member setNav3dConfig config screen world =
            let nav3d = World.getScreenNav3d screen world
            if config <> nav3d.Nav3dConfig then // OPTIMIZATION: preserve map reference if no content changes detected.
                let nav3d = { nav3d with Nav3dConfig = config }
                World.setScreenNav3d nav3d screen world |> snd'
            else world

        /// Attempt to synchronize the given screen's 3d navigation information.
        static member synchronizeNav3d screen world =
            let nav3d = World.getScreenNav3d screen world
            let rebuild =
                match (nav3d.Nav3dBodiesOldOpt, nav3d.Nav3dConfigOldOpt) with
                | (Some bodiesOld, Some configOld) -> nav3d.Nav3dBodies =/= bodiesOld || nav3d.Nav3dConfig =/= configOld
                | (None, Some _) | (Some _, None) -> Log.infoOnce "Unexpected 3d navigation state; navigation rebuild declined."; false
                | (None, None) -> nav3d.Nav3dBodies.Count <> 0
            if rebuild then
                let bodies = nav3d.Nav3dBodies.Values
                match World.tryBuildNav3dMesh bodies nav3d.Nav3dConfig with
                | Some navMesh ->
                    let nav3d =
                        { nav3d with
                            Nav3dBodiesOldOpt = Some nav3d.Nav3dBodies
                            Nav3dConfigOldOpt = Some nav3d.Nav3dConfig
                            Nav3dMeshOpt = Some navMesh }
                    World.setScreenNav3d nav3d screen world |> snd'
                | None -> Log.info "Unable to build 3d navigation mesh."; world
            else world

        /// Query the given screen's 3d navigation information if it exists.
        static member tryQueryNav3d query screen world =
            let nav3d = World.getScreenNav3d screen world
            match nav3d.Nav3dMeshOpt with
            | Some (_, dtNavMesh, dtQuery) -> Some (query nav3d.Nav3dConfig dtNavMesh dtQuery)
            | None -> None

        static member internal tryNav3dFollowQuery moveSpeed (startPosition : Vector3) (endPosition : Vector3) navConfig (navMesh : DtNavMesh) (query : DtNavMeshQuery) =

            // attempt to compute start position information
            let mutable startRef = 0L
            let mutable startPosition = RcVec3f (startPosition.X, startPosition.Y, startPosition.Z)
            let mutable startIsOverPoly = false
            let mutable startStatus = DtStatus.DT_IN_PROGRESS
            let filter = DtQueryDefaultFilter (int SampleAreaModifications.SAMPLE_POLYFLAGS_ALL, int SampleAreaModifications.SAMPLE_POLYFLAGS_DISABLED, [||])
            while startStatus = DtStatus.DT_IN_PROGRESS do
                startStatus <- query.FindNearestPoly (startPosition, RcVec3f (2f, 4f, 2f), filter, &startRef, &startPosition, &startIsOverPoly)

            // attempt to compute end position information
            let mutable endRef = 0L
            let mutable endPosition = RcVec3f (endPosition.X, endPosition.Y, endPosition.Z)
            let mutable endIsOverPoly = false
            let mutable endStatus = DtStatus.DT_IN_PROGRESS
            while endStatus = DtStatus.DT_IN_PROGRESS do
                endStatus <- query.FindNearestPoly (endPosition, RcVec3f (2f, 4f, 2f), filter, &endRef, &endPosition, &endIsOverPoly)

            // attempt to compute path
            if startStatus = DtStatus.DT_SUCCESS && endStatus = DtStatus.DT_SUCCESS then
                let navMeshTool = RcTestNavMeshTool ()
                let mutable polys = List ()
                let mutable path = List ()
                let mutable pathStatus = DtStatus.DT_IN_PROGRESS
                while pathStatus = DtStatus.DT_IN_PROGRESS do
                    pathStatus <- navMeshTool.FindFollowPath (navMesh, query, startRef, endRef, startPosition, endPosition, filter, true, &polys, &path)
                if pathStatus = DtStatus.DT_SUCCESS && path.Count > 0 then
                    let mutable pathIndex = 0
                    let mutable travel = 0.0f
                    let mutable step = RcVec3f.Zero
                    while pathIndex < path.Count && travel < moveSpeed do
                        let substep = path.[pathIndex] - startPosition
                        let substepTrunc =
                            if travel + substep.Length () > moveSpeed then
                                let travelOver = travel + substep.Length () - moveSpeed
                                let travelDelta = substep.Length () - travelOver + 0.0001f
                                RcVec3f.Normalize substep * travelDelta
                            else substep
                        travel <- travel + substepTrunc.Length ()
                        step <- step + substepTrunc
                        pathIndex <- inc pathIndex
                    let stepPosition = startPosition + step
                    Some (v3 stepPosition.X (stepPosition.Y - navConfig.CellHeight) stepPosition.Z)
                else None
            else None

        /// Compute (navRotation, navAngularVelocity) for the given turn speed and navDirection.
        static member nav3dFace turnSpeed (rotation : Quaternion) (navDirection : Vector3) =
            let navRotation = Quaternion.CreateFromAxisAngle (v3Up, atan2 navDirection.X navDirection.Z + MathF.PI)
            let navAngularVelocityYOpt = rotation.Forward.AngleBetween navRotation.Forward
            let navAngularVelocityY = if Single.IsNaN navAngularVelocityYOpt then 0.0f else navAngularVelocityYOpt
            let navRotation =
                if navAngularVelocityY > turnSpeed then
                    let sign = (Vector3.Cross (rotation.Forward, navRotation.Forward)).Y
                    rotation * Quaternion.CreateFromAxisAngle (v3Up, MathF.CopySign (turnSpeed, sign))
                else navRotation
            let navAngularVelocityYOpt = rotation.Forward.AngleBetween navRotation.Forward
            let navAngularVelocityY = if Single.IsNaN navAngularVelocityYOpt then 0.0f else navAngularVelocityYOpt
            let navAngularVelocity = v3Up * navAngularVelocityY
            (navRotation, navAngularVelocity)

        /// Compute navigation information that results in following the given destination.
        static member nav3dFollow distanceMinOpt distanceMaxOpt moveSpeed turnSpeed (position : Vector3) (rotation : Quaternion) (destination : Vector3) screen world =
            let distance = (destination - position).Magnitude
            if  (Option.isNone distanceMinOpt || distance > distanceMinOpt.Value) &&
                (Option.isNone distanceMaxOpt || distance <= distanceMaxOpt.Value) then
                match World.tryQueryNav3d (World.tryNav3dFollowQuery moveSpeed position destination) screen world with
                | Some (Some navPosition) ->
                    // TODO: consider doing an offset physics ray cast to align navPosition with near
                    // ground. Additionally, consider removing the CellHeight offset in the above query so
                    // that we don't need to do an offset here at all.
                    let navLinearVelocity = navPosition - position
                    let (navRotation, navAngularVelocity) = World.nav3dFace turnSpeed rotation navLinearVelocity
                    { NavPosition = navPosition; NavRotation = navRotation; NavLinearVelocity = navLinearVelocity; NavAngularVelocity = navAngularVelocity }
                | _ ->
                    let navDirection = destination - position
                    let (navRotation, navAngularVelocity) = World.nav3dFace turnSpeed rotation navDirection
                    { NavPosition = position; NavRotation = navRotation; NavLinearVelocity = v3Zero; NavAngularVelocity = navAngularVelocity }
            elif Option.isNone distanceMaxOpt || distance <= distanceMaxOpt.Value then
                let navDirection = destination - position
                let (navRotation, navAngularVelocity) = World.nav3dFace turnSpeed rotation navDirection
                { NavPosition = position; NavRotation = navRotation; NavLinearVelocity = v3Zero; NavAngularVelocity = navAngularVelocity }
            else { NavPosition = position; NavRotation = rotation; NavLinearVelocity = v3Zero; NavAngularVelocity = v3Zero }