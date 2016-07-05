// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Prime
open Nu
    
[<AutoOpen>]
module EventsModule =

    /// The data for a mouse move event.
    type [<StructuralEquality; NoComparison>] MouseMoveData =
        { Position : Vector2 }

    /// The data for a mouse button event.
    type [<StructuralEquality; NoComparison>] MouseButtonData =
        { Position : Vector2
          Button : MouseButton
          Down : bool }

    /// The data for a keyboard key event.
    type [<StructuralEquality; NoComparison>] KeyboardKeyData =
        { ScanCode : int
          Repeated : bool
          Down : bool }

    /// The data for a collision event.
    type [<StructuralEquality; NoComparison>] CollisionData =
        { Normal : Vector2
          Speed : single
          Collidee : Entity }

[<RequireQualifiedAccess>]
module Events =

    let Any = Prime.Events.Any
    let Subscribe = ntoa<obj Address> !!"Subscribe"
    let Unsubscribe = ntoa<obj Address> !!"Unsubscribe"
    let Update = ntoa<unit> !!"Update"
    let Select = ntoa<unit> !!"Select"
    let Deselect = ntoa<unit> !!"Deselect"
    let Down = ntoa<unit> !!"Down"
    let Up = ntoa<unit> !!"Up"
    let Click = ntoa<unit> !!"Click"
    let On = ntoa<unit> !!"On"
    let Off = ntoa<unit> !!"Off"
    let Touch = ntoa<Vector2> !!"Touch"
    let Untouch = ntoa<Vector2> !!"Untouch"
    let Mouse = ntoa<obj> !!"Mouse"
    let MouseMove = Mouse -<- ntoa<MouseMoveData> !!"Move"
    let MouseDrag = Mouse -<- ntoa<MouseMoveData> !!"Drag"
    let MouseLeft = Mouse -<- ntoa<MouseButtonData> !!"Left"
    let MouseCenter = Mouse -<- ntoa<MouseButtonData> !!"Center"
    let MouseRight = Mouse -<- ntoa<MouseButtonData> !!"Right"
    let MouseX1 = Mouse -<- ntoa<MouseButtonData> !!"X1"
    let MouseX2 = Mouse -<- ntoa<MouseButtonData> !!"X2"
    let MouseLeftDown = MouseLeft -|- ntoa !!"Down"
    let MouseLeftUp = MouseLeft -|- ntoa !!"Up"
    let MouseLeftChange = MouseLeft -|- ntoa !!"Change"
    let MouseCenterDown = MouseCenter -|- ntoa !!"Down"
    let MouseCenterUp = MouseCenter -|- ntoa !!"Up"
    let MouseCenterChange = MouseCenter -|- ntoa !!"Change"
    let MouseRightDown = MouseRight -|- ntoa !!"Down"
    let MouseRightUp = MouseRight -|- ntoa !!"Up"
    let MouseRightChange = MouseRight -|- ntoa !!"Change"
    let MouseX1Down = MouseX1 -|- ntoa !!"Down"
    let MouseX1Up = MouseX1 -|- ntoa !!"Up"
    let MouseX1Change = MouseX1 -|- ntoa !!"Change"
    let MouseX2Down = MouseX2 -|- ntoa !!"Down"
    let MouseX2Up = MouseX2 -|- ntoa !!"Up"
    let MouseX2Change = MouseX2 -|- ntoa !!"Change"
    let KeyboardKey = ntoa<obj> !!"KeyboardKey"
    let KeyboardKeyDown = Mouse -<- ntoa<KeyboardKeyData> !!"Down"
    let KeyboardKeyUp = Mouse -<- ntoa<KeyboardKeyData> !!"Up"
    let KeyboardKeyChange = Mouse -<- ntoa<KeyboardKeyData> !!"Change"
    let Collision = ntoa<CollisionData> !!"Collision"
    let Incoming = ntoa<unit> !!"Incoming"
    let IncomingStart = Incoming -|- ntoa !!"Start"
    let IncomingFinish = Incoming -|- ntoa !!"Finish"
    let Outgoing = ntoa<unit> !!"Outgoing"
    let OutgoingStart = Outgoing -|- ntoa !!"Start"
    let OutgoingFinish = Outgoing -|- ntoa !!"Finish"
    let Assets = ntoa<obj> !!"Assets"
    let AssetsReload = Assets -<- ntoa<unit> !!"Reload"
    let Ambient = ntoa<obj> !!"Ambient"
    let AmbientChange = Ambient -<- ntoa<AmbientChangeData> !!"Change"
    let AmbientChangeProperty = fun propertyName -> Ambient -<- ltoa<AmbientChangeData> [!!"Change"; !!propertyName]
    let Game = ntoa<obj> !!"Game"
    let GameChange = Game -<- ntoa<ParticipantChangeData<Game, World>> !!"Change"
    let Screen = ntoa<obj> !!"Screen"
    let ScreenAdd = Screen -<- ntoa<unit> !!"Add"
    let ScreenRemoving = Screen -<- ntoa<unit> !!"Removing"
    let ScreenChange = Screen -<- ntoa<ParticipantChangeData<Screen, World>> !!"Change"
    let Group = ntoa<obj> !!"Group"
    let GroupAdd = Group -<- ntoa<unit> !!"Add"
    let GroupRemoving = Group -<- ntoa<unit> !!"Removing"
    let GroupChange = Group -<- ntoa<ParticipantChangeData<Group, World>> !!"Change"
    let Entity = ntoa<obj> !!"Entity"
    let EntityAdd = Entity -<- ntoa<unit> !!"Add"
    let EntityRemoving = Entity -<- ntoa<unit> !!"Removing"
    let EntityChange = Entity -<- ntoa<ParticipantChangeData<Entity, World>> !!"Change"
    let EntityChangeProperty = fun propertyName -> Entity -<- ltoa<ParticipantChangeData<Entity, World>> [!!"Change"; !!propertyName]

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimulantOperators =

    /// Convert a name string to a screen's proxy.
    let (!>) screenNameStr = Screen.proxy ^ ntoa ^ Name.make screenNameStr

    /// Convert a name to a screen's proxy.
    let ntos screenName = Screen.proxy ^ ntoa screenName

    /// Convert a group's proxy to an entity's by appending the entity's name at the end.
    let gtoe (group : Group) entityName = Entity.proxy ^ atoa<Group, Entity> group.GroupAddress ->- ntoa entityName

    /// Convert a screen's proxy to a group's by appending the group's name at the end.
    let stog (screen : Screen) groupName = Group.proxy ^ atoa<Screen, Group> screen.ScreenAddress ->- ntoa groupName

    /// Convert an entity's proxy to a group's by removing the entity's name from the end.
    let etog (entity : Entity) = !< entity

    /// Convert a group's proxy to a screen's by removing the group's name from the end.
    let gtos group = Screen.proxy ^ Address.take<Group, Screen> 1 group.GroupAddress

module Simulants =

    /// The game. Always exists.
    let Game = { GameAddress = Address.empty }

    /// The default screen - may or may not exist.
    let DefaultScreen = !> Constants.Engine.DefaultScreenName
    
    /// The default group - may or may not exist.
    let DefaultGroup = DefaultScreen => Constants.Engine.DefaultGroupName
    
    /// The default entity - may or may not exist.
    let DefaultEntity = DefaultGroup => Constants.Engine.DefaultEntityName

[<AutoOpen>]
module DescriptorsModule =

    /// Describes an entity value independent of the engine.
    type [<NoComparison>] EntityDescriptor =
        { EntityDispatcher : string
          EntityProperties : Map<string, Symbol> }
    
        /// The empty entity descriptor.
        static member empty =
            { EntityDispatcher = String.Empty
              EntityProperties = Map.empty }
    
    /// Describes a group value independent of the engine.
    type [<NoComparison>] GroupDescriptor =
        { GroupDispatcher : string
          GroupProperties : Map<string, Symbol>
          Entities : EntityDescriptor list }
    
        /// The empty group descriptor.
        static member empty =
            { GroupDispatcher = String.Empty
              GroupProperties = Map.empty
              Entities = [] }
    
    /// Describes a screen value independent of the engine.
    type [<NoComparison>] ScreenDescriptor =
        { ScreenDispatcher : string
          ScreenProperties : Map<string, Symbol>
          Groups : GroupDescriptor list }
    
        /// The empty screen descriptor.
        static member empty =
            { ScreenDispatcher = String.Empty
              ScreenProperties = Map.empty
              Groups = [] }
    
    /// Describes a game value independent of the engine.
    type [<NoComparison>] GameDescriptor =
        { GameDispatcher : string
          GameProperties : Map<string, Symbol>
          Screens : ScreenDescriptor list }
    
        /// The empty game descriptor.
        static member empty =
            { GameDispatcher = String.Empty
              GameProperties = Map.empty
              Screens = [] }

module Descriptors =

    /// Describe a game with the given properties values and contained screens.
    let Game<'d when 'd :> GameDispatcher> properties screens =
        { GameDispatcher = typeof<'d>.Name
          GameProperties = Map.ofSeq properties
          Screens = List.ofSeq screens }

    /// Describe a screen with the given properties values and contained groups.
    let Screen<'d when 'd :> ScreenDispatcher> properties groups =
        { ScreenDispatcher = typeof<'d>.Name
          ScreenProperties = Map.ofSeq properties
          Groups = List.ofSeq groups }

    /// Describe a group with the given properties values and contained entities.
    let Group<'d when 'd :> GroupDispatcher> properties entities =
        { GroupDispatcher = typeof<'d>.Name
          GroupProperties = Map.ofSeq properties
          Entities = List.ofSeq entities }

    /// Describe an entity with the given properties values.
    let Entity<'d when 'd :> EntityDispatcher> properties =
        { EntityDispatcher = typeof<'d>.Name
          EntityProperties = Map.ofSeq properties }

[<AutoOpen>]
module CommandModule =

    /// A command that transforms the world in some manner.
    type [<NoEquality; NoComparison>] Command =
        { Execute : World -> World }

[<AutoOpen>]
module WorldAmbientStateModule =

    type World with
    
        /// Get the tick rate.
        static member getTickRate world =
            World.getAmbientStateBy AmbientState.getTickRate world

        /// Get the tick rate as a floating-point value.
        static member getTickRateF world =
            World.getAmbientStateBy AmbientState.getTickRateF world

        /// Set the tick rate without waiting for the end of the current update. Only use
        /// this if you need it and understand the engine internals well enough to know the
        /// consequences.
        static member setTickRateImmediately tickRate world =
            World.updateAmbientState (AmbientState.setTickRateImmediately tickRate) world

        /// Set the tick rate.
        static member setTickRate tickRate world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world; Operation = fun world -> World.setTickRateImmediately tickRate world }) world

        /// Reset the tick time to 0.
        static member resetTickTime world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world; Operation = fun world -> World.updateAmbientState AmbientState.resetTickTime world }) world

        /// Get the world's tick time.
        static member getTickTime world =
            World.getAmbientStateBy AmbientState.getTickTime world

        /// Query that the world is ticking.
        static member isTicking world =
            World.getAmbientStateBy AmbientState.isTicking world

        static member internal updateTickTime world =
            World.updateAmbientStateWithoutEvent AmbientState.updateTickTime world

        /// Get the world's update count.
        static member getUpdateCount world =
            World.getAmbientStateBy AmbientState.getUpdateCount world

        static member internal incrementUpdateCount world =
            World.updateAmbientStateWithoutEvent AmbientState.incrementUpdateCount world

        /// Get the the liveness state of the engine.
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        /// Place the engine into a state such that the app will exit at the end of the current update.
        static member exit world =
            World.updateAmbientState AmbientState.exit world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal clearTasklets world =
            World.updateAmbientStateWithoutEvent AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.updateAmbientStateWithoutEvent (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet tasklet world =
            World.updateAmbientStateWithoutEvent (AmbientState.addTasklet tasklet) world

        /// Add multiple tasklets to be executed by the engine at the scheduled times.
        static member addTasklets tasklets world =
            World.updateAmbientStateWithoutEvent (AmbientState.addTasklets tasklets) world

        /// Get a value from the camera used to view the world.
        static member getCameraBy by world =
            World.getAmbientStateBy (AmbientState.getCameraBy by) world

        /// Get the camera used to view the world.
        static member getCamera world =
            World.getAmbientStateBy AmbientState.getCamera world

        /// Update the camera used to view the world.
        static member updateCamera updater world =
            World.updateAmbientState (AmbientState.updateCamera updater) world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            AmbientState.getAssetMetadataMap ^ World.getAmbientState world

        static member internal setAssetMetadataMap assetMetadataMap world =
            World.updateAmbientState (AmbientState.setAssetMetadataMap assetMetadataMap) world

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            World.updateAmbientState (AmbientState.setOverlayer overlayer) world

        /// Get intrinsic overlays.
        static member getIntrinsicOverlays world =
            World.getOverlayerBy Overlayer.getIntrinsicOverlays world

        /// Get extrinsic overlays.
        static member getExtrinsicOverlays world =
            World.getOverlayerBy Overlayer.getExtrinsicOverlays world

        static member internal getOverlayRouter world =
            World.getAmbientStateBy AmbientState.getOverlayRouter world

        static member internal getSymbolStoreBy by world =
            World.getAmbientStateBy (AmbientState.getSymbolStoreBy by) world

        static member internal getSymbolStore world =
            World.getAmbientStateBy AmbientState.getSymbolStore world

        static member internal setSymbolStore symbolStore world =
            World.updateAmbientState (AmbientState.setSymbolStore symbolStore) world

        static member internal updateSymbolStore updater world =
            World.updateAmbientState (AmbientState.updateSymbolStore updater) world

        /// Try to load a symbol store package with the given name.
        static member tryLoadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.tryLoadSymbolStorePackage packageName) world

        /// Unload a symbol store package with the given name.
        static member unloadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.unloadSymbolStorePackage packageName) world

        /// Try to find a symbol with the given asset tag.
        static member tryFindSymbol assetTag world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbol assetTag symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Try to find symbols with the given asset tags.
        static member tryFindSymbols assetTags world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbols assetTags symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Reload all the symbols in the symbol store.
        static member reloadSymbols world =
            World.updateSymbolStore SymbolStore.reloadSymbols world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            World.getAmbientStateBy AmbientState.getUserState world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            World.updateAmbientState (AmbientState.updateUserState updater) world

[<AutoOpen>]
module WorldEventHandlerModule =

    type World with

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)
            
        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)