// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

/// A marker interface for simulant descriptors.
type SimulantDescriptor =
    interface
        abstract Children : SimulantDescriptor list
        end

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

    interface SimulantDescriptor with
        member this.Children =
            this.Screens |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a screen value independent of the engine.
and [<NoComparison>] ScreenDescriptor =
    { ScreenDispatcher : string
      ScreenProperties : Map<string, Symbol>
      Layers : LayerDescriptor list }

    /// The empty screen descriptor.
    static member empty =
        { ScreenDispatcher = String.Empty
          ScreenProperties = Map.empty
          Layers = [] }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.ScreenProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>
          
    interface SimulantDescriptor with
        member this.Children =
            this.Layers |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a layer value independent of the engine.
and [<NoComparison>] LayerDescriptor =
    { LayerDispatcher : string
      LayerProperties : Map<string, Symbol>
      Entities : EntityDescriptor list }

    /// The empty layer descriptor.
    static member empty =
        { LayerDispatcher = String.Empty
          LayerProperties = Map.empty
          Entities = [] }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.LayerProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>

    interface SimulantDescriptor with
        member this.Children =
            this.Entities |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes an entity value independent of the engine.
and [<NoComparison>] EntityDescriptor =
    { EntityDispatcher : string
      EntityProperties : Map<string, Symbol> }

    /// The empty entity descriptor.
    static member empty =
        { EntityDispatcher = String.Empty
          EntityProperties = Map.empty }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.EntityProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>

    interface SimulantDescriptor with
        member this.Children = []

/// Describes the behavior of a screen.
type [<NoComparison>] ScreenBehavior =
    | Vanilla
    | Omniscreen
    | Dissolve of DissolveData
    | Splash of DissolveData * SplashData * Screen

/// Describes the view for a simulant
type SimulantView = interface end

/// Describes the view for a game.
type [<NoComparison>] GameView =
    | GameFromDescriptor of GameDescriptor * Game
    | GameFromFile of string * Game
    interface SimulantView

/// Describes the view for a screen.
type [<NoComparison>] ScreenView =
    | ScreenFromDescriptor of ScreenDescriptor * ScreenBehavior * Screen
    | ScreenFromLayerFile of Type * string * ScreenBehavior * Screen
    | ScreenFromFile of string * ScreenBehavior * Screen
    interface SimulantView

/// Describes the view for a layer.
type [<NoComparison>] LayerView =
    | LayerFromDescriptor of LayerDescriptor * Layer
    | LayerFromFile of string * Layer
    interface SimulantView

/// Describes the view for an entity.
type [<NoComparison>] EntityView =
    | EntityFromDescriptor of EntityDescriptor * Entity
    | EntityFromFile of string * Entity
    interface SimulantView

/// Contains primitives for describing simulants.
module Describe =

    /// Describe a game with the given properties values and contained screens.
    let game<'d when 'd :> GameDispatcher> (properties : (World PropertyTag * obj) seq) (screens : ScreenDescriptor seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { GameDispatcher = typeof<'d>.Name
          GameProperties = properties
          Screens = List.ofSeq screens }

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> (properties : (World PropertyTag * obj) seq) (layers : LayerDescriptor seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { ScreenDispatcher = typeof<'d>.Name
          ScreenProperties = properties
          Layers = List.ofSeq layers }

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> (properties : (World PropertyTag * obj) seq) (entities : EntityDescriptor seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { LayerDispatcher = typeof<'d>.Name
          LayerProperties = properties
          Entities = List.ofSeq entities }

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> (properties : (World PropertyTag * obj) seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { EntityDispatcher = typeof<'d>.Name
          EntityProperties = properties }

/// Contains primitives for describing simulant views.    
module View =

    /// Describe a game with the given properties values and contained screens.
    let game<'d when 'd :> GameDispatcher> properties children game =
        GameFromDescriptor (Describe.game<'d> properties children, game)

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> properties children behavior screen =
        ScreenFromDescriptor (Describe.screen<'d> properties children, behavior, screen)

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> properties children layer =
        LayerFromDescriptor (Describe.layer<'d> properties children, layer)

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> properties entity =
        EntityFromDescriptor (Describe.entity<'d> properties, entity)

    /// Describe a game to be loaded from a file.
    let gameFromFile<'d when 'd :> GameDispatcher> fileName game =
        GameFromFile (fileName, game)

    /// Describe a screen to be loaded from a file.
    let screenFromFile<'d when 'd :> ScreenDispatcher> fileName behavior screen =
        ScreenFromFile (fileName, behavior, screen)

    /// Describe a screen to be loaded from a file.
    let screenFromLayerFile<'d when 'd :> ScreenDispatcher> fileName behavior screen =
        ScreenFromLayerFile (typeof<'d>, fileName, behavior, screen)

    /// Describe a layer to be loaded from a file.
    let layerFromFile<'d when 'd :> LayerDispatcher> fileName layer =
        LayerFromFile (fileName, layer)

    /// Describe an entity to be loaded from a file.
    let entityFromFile<'d when 'd :> EntityDispatcher> fileName entity =
        EntityFromFile (fileName, entity)

[<AutoOpen>]
module WorldModelDeclarative =

    /// Create a property declaration.
    let prop (p : PropertyTag<'a, World>) (v : 'a) =
        (p :> PropertyTag<World>, v :> obj)

    /// Pair an empty list of commands with a model.
    let inline just model = (model, [])