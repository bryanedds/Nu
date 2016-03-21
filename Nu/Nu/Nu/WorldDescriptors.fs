// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

/// Describes an entity value independent of the engine.
type [<NoComparison>] EntityDescriptor =
    { EntityDispatcher : string
      EntityFields : Map<string, Symbol> }

    /// The empty entity descriptor.
    static member empty =
        { EntityDispatcher = String.Empty
          EntityFields = Map.empty }

/// Describes a group value independent of the engine.
type [<NoComparison>] GroupDescriptor =
    { GroupDispatcher : string
      GroupFields : Map<string, Symbol>
      Entities : EntityDescriptor list }

    /// The empty group descriptor.
    static member empty =
        { GroupDispatcher = String.Empty
          GroupFields = Map.empty
          Entities = [] }

/// Describes a screen value independent of the engine.
type [<NoComparison>] ScreenDescriptor =
    { ScreenDispatcher : string
      ScreenFields : Map<string, Symbol>
      Groups : GroupDescriptor list }

    /// The empty screen descriptor.
    static member empty =
        { ScreenDispatcher = String.Empty
          ScreenFields = Map.empty
          Groups = [] }

/// Describes a game value independent of the engine.
type [<NoComparison>] GameDescriptor =
    { GameDispatcher : string
      GameFields : Map<string, Symbol>
      Screens : ScreenDescriptor list }

    /// The empty game descriptor.
    static member empty =
        { GameDispatcher = String.Empty
          GameFields = Map.empty
          Screens = [] }