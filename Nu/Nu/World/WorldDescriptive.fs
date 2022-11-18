// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime
open Nu

/// Describes a generalized simulant value independent of the engine.
/// Not used for serialization.
type [<NoComparison>] SimulantDescriptor =
    { SimulantSurnamesOpt : string array option
      SimulantDispatcherName : string
      SimulantProperties : (string * Property) list
      SimulantChildren : SimulantDescriptor list }

/// Describes an entity value independent of the engine.
/// Used to directly serialize an entity.
type [<NoComparison>] EntityDescriptor =
    { EntityDispatcherName : string
      EntityProperties : Map<string, Symbol>
      EntityDescriptors : EntityDescriptor list }

[<RequireQualifiedAccess>]
module EntityDescriptor =

    /// Derive a name from the descriptor.
    let getNameOpt descriptor =
        descriptor.EntityProperties |>
        Map.tryFind Constants.Engine.NamePropertyName |>
        Option.map symbolToValue<string>

    /// Set a name for the descriptor.
    let setNameOpt nameOpt descriptor =
        match nameOpt with
        | Some name -> { descriptor with EntityProperties = Map.add Constants.Engine.NamePropertyName (valueToSymbol name) descriptor.EntityProperties }
        | None -> { descriptor with EntityProperties = Map.remove Constants.Engine.NamePropertyName descriptor.EntityProperties }

    /// The empty entity descriptor.
    let empty =
        { EntityDispatcherName = String.Empty
          EntityProperties = Map.empty
          EntityDescriptors  = [] }

/// Describes a group value independent of the engine.
/// Used to directly serialize a group.
type [<NoComparison>] GroupDescriptor =
    { GroupDispatcherName : string
      GroupProperties : Map<string, Symbol>
      EntityDescriptors : EntityDescriptor list }

[<RequireQualifiedAccess>]
module GroupDescriptor =

    /// Derive a name from the dispatcher.
    let getNameOpt dispatcher =
        dispatcher.GroupProperties |>
        Map.tryFind Constants.Engine.NamePropertyName |>
        Option.map symbolToValue<string>

    /// The empty group descriptor.
    let empty =
        { GroupDispatcherName = String.Empty
          GroupProperties = Map.empty
          EntityDescriptors = [] }

/// Describes a screen value independent of the engine.
/// Used to directly serialize a screen.
type [<NoComparison>] ScreenDescriptor =
    { ScreenDispatcherName : string
      ScreenProperties : Map<string, Symbol>
      GroupDescriptors : GroupDescriptor list }

[<RequireQualifiedAccess>]
module ScreenDescriptor =

    /// Derive a name from the dispatcher.
    let getNameOpt dispatcher =
        dispatcher.ScreenProperties |>
        Map.tryFind Constants.Engine.NamePropertyName |>
        Option.map symbolToValue<string>

    /// The empty screen descriptor.
    let empty =
        { ScreenDispatcherName = String.Empty
          ScreenProperties = Map.empty
          GroupDescriptors = [] }

/// Describes a game value independent of the engine.
/// Used to directly serialize a game.
type [<NoComparison>] GameDescriptor =
    { GameDispatcherName : string
      GameProperties : Map<string, Symbol>
      ScreenDescriptors : ScreenDescriptor list }

[<RequireQualifiedAccess>]
module GameDescriptor =

    /// The empty game descriptor.
    let empty =
        { GameDispatcherName = String.Empty
          GameProperties = Map.empty
          ScreenDescriptors = [] }