// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimulantOperators =

    /// Derive a screen from a name string.
    let (!>) screenNameStr = Screen (ntoa !!screenNameStr)

    /// Derivce a screen from a name.
    let ntos screenName = Screen (ntoa screenName)

    /// Derive an entity from its layer.
    let ltoe (layer : Layer) entityName = Entity (atoa<Layer, Entity> layer.LayerAddress ->- ntoa entityName)

    /// Derive layer from its screen.
    let stol (screen : Screen) layerName = Layer (atoa<Screen, Layer> screen.ScreenAddress ->- ntoa layerName)

    /// Derive an entity from its layer.
    let etol (entity : Entity) = !< entity

    /// Derive a screen from one of its layers.
    let ltos (layer : Layer) = Screen (Address.take<Layer, Screen> 1 layer.LayerAddress)

module Simulants =

    /// The game. Always exists.
    let Game = Game Address.empty

    /// The default screen - may or may not exist.
    let DefaultScreen = !> Constants.Engine.DefaultScreenName
    
    /// The default layer - may or may not exist.
    let DefaultLayer = DefaultScreen => Constants.Engine.DefaultLayerName
    
    /// The default entity - may or may not exist.
    let DefaultEntity = DefaultLayer => Constants.Engine.DefaultEntityName

module Descriptors =

    /// Describe a game with the given properties values and contained screens.
    let Game<'d when 'd :> GameDispatcher> properties screens =
        { GameDispatcher = typeof<'d>.Name
          GameProperties = Map.ofSeq properties
          Screens = List.ofSeq screens }

    /// Describe a screen with the given properties values and contained layers.
    let Screen<'d when 'd :> ScreenDispatcher> properties layers =
        { ScreenDispatcher = typeof<'d>.Name
          ScreenProperties = Map.ofSeq properties
          Layers = List.ofSeq layers }

    /// Describe a layer with the given properties values and contained entities.
    let Layer<'d when 'd :> LayerDispatcher> properties entities =
        { LayerDispatcher = typeof<'d>.Name
          LayerProperties = Map.ofSeq properties
          Entities = List.ofSeq entities }

    /// Describe an entity with the given properties values.
    let Entity<'d when 'd :> EntityDispatcher> properties =
        { EntityDispatcher = typeof<'d>.Name
          EntityProperties = Map.ofSeq properties }

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