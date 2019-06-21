// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimulantOperators =

    /// Derive a screen from a name.
    let ntos screenName = Screen (ntoa screenName)

    /// Derive an entity from its layer.
    let ltoe (layer : Layer) entityName = Entity (atoa<Layer, Entity> layer.LayerAddress ->- ntoa entityName)

    /// Derive layer from its screen.
    let stol (screen : Screen) layerName = Layer (atoa<Screen, Layer> screen.ScreenAddress ->- ntoa layerName)

    /// Derive entity from its screen.
    let stoe (screen : Screen) layerName entityName = ltoe (stol screen layerName) entityName

    /// Derive a layer from its entity.
    let etol (entity : Entity) = Layer (Address.allButLast entity.EntityAddress)

    /// Derive a screen from one of its layers.
    let ltos (layer : Layer) = Screen (Address.allButLast layer.LayerAddress)

    /// Derive a screen from one of its entities.
    let etos (entity : Entity) = ltos (etol entity)

    /// Derive a screen from its name.
    let (!>) (screenName : string) = Screen screenName

[<RequireQualifiedAccess>]
module Default =

    /// The default game. Always exists.
    let Game = Game ()

    /// The default screen - may or may not exist.
    let Screen = Screen Constants.Engine.DefaultScreenName
    
    /// The default layer - may or may not exist.
    let Layer = Screen => Constants.Engine.DefaultLayerName
    
    /// The default entity - may or may not exist.
    let Entity = Layer => Constants.Engine.DefaultEntityName