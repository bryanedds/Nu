// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimulantOperators =

    /// Convert a name string to a screen's proxy.
    let (!>) screenNameStr = Screen.proxy ^ ntoa !!screenNameStr

    /// Convert a name to a screen's proxy.
    let ntos screenName = Screen.proxy ^ ntoa screenName

    /// Convert a layer's proxy to an entity's by appending the entity's name at the end.
    let ltoe (layer : Layer) entityName = Entity.proxy ^ atoa<Layer, Entity> layer.LayerAddress ->- ntoa entityName

    /// Convert a screen's proxy to a layer's by appending the layer's name at the end.
    let stol (screen : Screen) layerName = Layer.proxy ^ atoa<Screen, Layer> screen.ScreenAddress ->- ntoa layerName

    /// Convert an entity's proxy to a layer's by removing the entity's name from the end.
    let etol (entity : Entity) = !< entity

    /// Convert a layer's proxy to a screen's by removing the layer's name from the end.
    let ltos layer = Screen.proxy ^ Address.take<Layer, Screen> 1 layer.LayerAddress

module Simulants =

    /// The game. Always exists.
    let Game = { GameAddress = Address.empty }

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