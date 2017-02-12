// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimulantOperators =

    /// Derive a screen from a name string.
    let (!>) screenName = Screen (ntoa screenName)

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