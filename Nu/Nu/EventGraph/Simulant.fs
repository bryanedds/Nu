// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open Nu

/// A participant in the event graph.
type Simulant =
    interface
        abstract SimulantAddress : Address
        end

/// Simulant operators.
[<AutoOpen>]
module SimulantOperators =

    /// Operators for the Simulant type.
    type Simulant with

        /// The names of the simulant.
        member this.Names = this.SimulantAddress.Names

        /// The name of the simulant.
        member this.Name = Array.last this.SimulantAddress.Names

/// A simulant in the event system that is globalized and compatible with generalized events.
type GlobalSimulantGeneralized =
    { GsgAddress : GlobalSimulantGeneralized Address }
    interface Simulant with
        member this.SimulantAddress = atoa<GlobalSimulantGeneralized, Simulant> this.GsgAddress
        end