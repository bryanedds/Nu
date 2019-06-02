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
    let game<'d when 'd :> GameDispatcher> (props : (World PropertyTag * obj) seq) (children : ScreenDescriptor seq) =
        let properties = Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) props
        { GameDispatcher = typeof<'d>.Name
          GameProperties = Map.ofSeq properties
          Screens = Seq.toList children }

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> (props : (World PropertyTag * obj) seq) (children : LayerDescriptor seq) =
        let properties = Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) props
        { ScreenDispatcher = typeof<'d>.Name
          ScreenProperties = Map.ofSeq properties
          Layers = Seq.toList children }

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> (props : (World PropertyTag * obj) seq) (children : EntityDescriptor seq) =
        let properties = Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) props
        { LayerDispatcher = typeof<'d>.Name
          LayerProperties = Map.ofSeq properties
          Entities = Seq.toList children }

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> (props : (World PropertyTag * obj) seq) =
        let properties = Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) props
        { EntityDispatcher = typeof<'d>.Name
          EntityProperties = Map.ofSeq properties }

    let prop (p : PropertyTag<'a, World>) (v : 'a) =
        (p :> PropertyTag<World>, v :> obj)

/// Contains primitives for describing simulant views.    
module View =

    /// Describe a game with the given properties values and contained screens.
    let game<'d when 'd :> GameDispatcher> props children game =
        GameFromDescriptor (Describe.game<'d> props children, game)

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> props children behavior screen =
        ScreenFromDescriptor (Describe.screen<'d> props children, behavior, screen)

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> props children layer =
        LayerFromDescriptor (Describe.layer<'d> props children, layer)

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> props entity =
        EntityFromDescriptor (Describe.entity<'d> props, entity)

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

    /// Pair an empty list of commands with a model.
    let inline just model = (model, [])