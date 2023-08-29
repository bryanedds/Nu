// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade

type PartyMenuState =
    | PartyMenuOpened
    | PartyMenuClosed

type [<SymbolicExpansion>] PartyMenu =
    { PartyMenuState : PartyMenuState
      PartyMenuSelections : int list }