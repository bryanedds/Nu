// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.IO
open Prime
open DotRecast.Core
open DotRecast.Recast
open DotRecast.Recast.Geom
open DotRecast.Recast.Toolset.Builder

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
        member this.GetNavigationMap world = World.getScreenNavigationMap this world
        member this.NavigationMap = lensReadOnly (nameof this.NavigationMap) this this.GetNavigationMap
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

        static member internal tryCreateNavigationMesh mesh (_ : World) =

            // attempt to create an input geometry provider
            let meshConfig = mesh.NavigationMeshConfig
            let meshContent = mesh.NavigationMeshContent
            let geomProviderOpt =
                match meshContent with
                | NavigationMeshModel _ -> failwithnie ()
                | NavigationMeshModelSurface (staticModel, surfaceIndex) ->
                    match Metadata.tryGetStaticModelMetadata staticModel with
                    | Some physicallyBasedModel ->
                        if surfaceIndex >= 0 && surfaceIndex < physicallyBasedModel.Surfaces.Length then
                            let surface = physicallyBasedModel.Surfaces.[surfaceIndex]
                            let geometry = surface.PhysicallyBasedGeometry
                            if geometry.PrimitiveType = OpenGL.PrimitiveType.Triangles then
                                let vertices = [|for v in geometry.Vertices do yield v.X; yield v.Y; yield v.Z|]
                                let provider = NavigationInputGeomProvider (vertices, geometry.Indices, geometry.Bounds)
                                Some (provider :> IInputGeomProvider)
                            else None
                        else None
                    | None -> None

            // attempt execute navigation mesh construction steps
            match geomProviderOpt with
            | Some geomProvider ->

                (* Step 1: Initialize builder config. *)
                let bmin = geomProvider.GetMeshBoundsMin ()
                let bmax = geomProvider.GetMeshBoundsMax ()
                let ctx = RcContext ()
                let rcConfig =
                    RcConfig
                        (meshConfig.PartitionType,
                         meshConfig.CellSize, meshConfig.CellHeight,
                         meshConfig.AgentMaxSlope, meshConfig.AgentHeight, meshConfig.AgentRadius, meshConfig.AgentMaxClimb,
                         meshConfig.RegionMinSize, meshConfig.RegionMergeSize,
                         meshConfig.EdgeMaxLength, meshConfig.EdgeMaxError,
                         meshConfig.VertsPerPolygon,
                         meshConfig.DetailSampleDistance, meshConfig.DetailSampleMaxError,
                         true, true, true,
                         SampleAreaModifications.SAMPLE_AREAMOD_GROUND, true)
                let rcBuilderConfig = RcBuilderConfig (rcConfig, bmin, bmax)

                (* Step 2: Rasterize input polygon soup. *)
                let solid = RcHeightfield (rcBuilderConfig.width, rcBuilderConfig.height, rcBuilderConfig.bmin, rcBuilderConfig.bmax, rcConfig.Cs, rcConfig.Ch, rcConfig.BorderSize)
                for geom in geomProvider.Meshes () do

                    // allocate array that can hold triangle area types.
                    // if you have multiple meshes you need to process, allocate an array which can hold the max number
                    // of triangles you need to process.
                    let verts = geom.GetVerts ()
                    let tris = geom.GetTris ()
                    let ntris = tris.Length / 3

                    // find triangles which are walkable based on their slope and rasterize them.
                    // if your input data is multiple meshes, you can transform them here, calculate the are type for
                    // each of the meshes and rasterize them.
                    let triareas = RcCommons.MarkWalkableTriangles (ctx, rcConfig.WalkableSlopeAngle, verts, tris, ntris, rcConfig.WalkableAreaMod)
                    RcRasterizations.RasterizeTriangles (ctx, verts, tris, triareas, ntris, solid, rcConfig.WalkableClimb)

                (* Step 3: Filter walkable surfaces. *)
                
                // once all geometry is rasterized, we do initial pass of filtering to remove unwanted overhangs caused
                // by the conservative rasterization as well as filter spans where the character cannot possibly stand.
                RcFilters.FilterLowHangingWalkableObstacles (ctx, rcConfig.WalkableClimb, solid)
                RcFilters.FilterLedgeSpans (ctx, rcConfig.WalkableHeight, rcConfig.WalkableClimb, solid)
                RcFilters.FilterWalkableLowHeightSpans (ctx, rcConfig.WalkableHeight, solid)

                (* Step 4: Partition walkable surface to simple regions. *)
                
                // compact the heightfield so that it is faster to handle from now on.
                // this will result more cache coherent data as well as the neighbours between walkable cells will be
                // calculated.
                let chf = RcCompacts.BuildCompactHeightfield (ctx, rcConfig.WalkableHeight, rcConfig.WalkableClimb, solid)

                // erode the walkable area by agent radius.
                RcAreas.ErodeWalkableArea (ctx, rcConfig.WalkableRadius, chf)

                // partition the heightfield so that we can use simple algorithm later to triangulate the walkable areas.
                // there are 3 partitioning methods, each with some pros and cons:
                // 1) watershed partitioning
                // - the classic Recast partitioning
                // - creates the nicest tessellation
                // - usually slowest
                // - partitions the heightfield into nice regions without holes or overlaps
                // - the are some corner cases where this method creates produces holes and overlaps
                // - holes may appear when a small obstacles is close to large open area (triangulation can handle this)
                // - overlaps may occur if you have narrow spiral corridors (i.e stairs), this make triangulation to fail
                // * generally the best choice if you precompute the navmesh, use this if you have large open areas
                // 2) monotone partioning
                // - fastest
                // - partitions the heightfield into regions without holes and overlaps (guaranteed)
                // - creates long thin polygons, which sometimes causes paths with detours
                // * use this if you want fast navmesh generation
                // 3) layer partitoining
                // - quite fast
                // - partitions the heighfield into non-overlapping regions
                // - relies on the triangulation code to cope with holes (thus slower than monotone partitioning)
                // - produces better triangles than monotone partitioning
                // - does not have the corner cases of watershed partitioning
                // - can be slow and create a bit ugly tessellation (still better than monotone) if you have large open
                // areas with small obstacles (not a problem if you use tiles)
                // * good choice to use for tiled navmesh with medium and small sized tiles
                match meshConfig.PartitionType with
                | RcPartition.WATERSHED ->
                    // prepare for region partitioning, by calculating distance field along the walkable surface and
                    // partition the walkable surface into simple regions without holes.
                    RcRegions.BuildDistanceField (ctx, chf)
                    RcRegions.BuildRegions (ctx, chf, rcConfig.MinRegionArea, rcConfig.MergeRegionArea)
                | RcPartition.MONOTONE ->
                    // partition the walkable surface into simple regions without holes. Monotone partitioning does not
                    // need distancefield.
                    RcRegions.BuildRegionsMonotone (ctx, chf, rcConfig.MinRegionArea, rcConfig.MergeRegionArea)
                | RcPartition.LAYERS ->
                    // partition the walkable surface into simple regions without holes.
                    RcRegions.BuildLayerRegions (ctx, chf, rcConfig.MinRegionArea) |> ignore<bool>
                | _ -> failwithumf ()

                (* Step 5: Trace and simplify region contours. *)
                let cset = RcContours.BuildContours (ctx, chf, rcConfig.MaxSimplificationError, rcConfig.MaxEdgeLen, RcBuildContoursFlags.RC_CONTOUR_TESS_WALL_EDGES)

                (* Step 6: Build polygons mesh from contours. *)
                let pmesh = RcMeshs.BuildPolyMesh (ctx, cset, rcConfig.MaxVertsPerPoly)

                (* Step 7: Create detail mesh which allows to access approximate height on each polygon. *)
                let dmesh = RcMeshDetails.BuildPolyMeshDetail (ctx, pmesh, chf, rcConfig.DetailSampleDist, rcConfig.DetailSampleMaxError)
                Some (chf, cset, pmesh, dmesh, ctx)

            | None -> None

        static member internal setScreenNavigationMeshOpt meshName meshOpt screen world =
            let map = World.getScreenNavigationMap screen world
            match (map.NavigationMeshes.TryFind meshName, meshOpt) with
            | (Some (mesh, _, _, _, _, _), Some mesh') ->
                if mesh' <> mesh then
                    match World.tryCreateNavigationMesh mesh' world with
                    | Some (chf', cset', pmesh', dmesh', ctx') ->
                        let map = { map with NavigationMeshes = Map.add meshName (mesh', chf', cset', pmesh', dmesh', ctx') map.NavigationMeshes }
                        World.setScreenNavigationMap map screen world |> snd'
                    | None ->
                        let map = { map with NavigationMeshes = Map.remove meshName map.NavigationMeshes }
                        World.setScreenNavigationMap map screen world |> snd'
                else world
            | (None, Some mesh) ->
                match World.tryCreateNavigationMesh mesh world with
                | Some (chf, cset, pmesh, dmesh, ctx) ->
                    let map = { map with NavigationMeshes = Map.add meshName (mesh, chf, cset, pmesh, dmesh, ctx) map.NavigationMeshes }
                    World.setScreenNavigationMap map screen world |> snd'
                | None ->
                    let map = { map with NavigationMeshes = Map.remove meshName map.NavigationMeshes }
                    World.setScreenNavigationMap map screen world |> snd'
            | (Some (_, _, _, _, _, _), None) ->
                let map = { map with NavigationMeshes = Map.remove meshName map.NavigationMeshes }
                World.setScreenNavigationMap map screen world |> snd'
            | (None, None) -> world