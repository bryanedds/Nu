namespace Nu
open Prime

/// Handles simulation property changes.
type PropertyChangeHandler<'key> = Simulation<'key> -> Simulation<'key> -> Simulation<'key>

/// A simulation in terms of an observable property bag.
and Simulation<'key> =
    abstract member GetPropertyOpt<'a> : 'key -> 'a option
    abstract member SetPropertyOpt<'a> : 'key -> 'a option -> Simulation<'key>
    abstract member HandlePropertyChange : 'key -> PropertyChangeHandler<'key> -> (Simulation<'key> -> Simulation<'key>) * Simulation<'key>

[<RequireQualifiedAccess>]
module Simulation =
    let getPropertyOpt (key : 'key) (simulation : Simulation<'key>) = simulation.GetPropertyOpt key
    let setPropertyOpt (key : 'key) (valueOpt : 'a option) (simulation : Simulation<'key>) = simulation.SetPropertyOpt key valueOpt
    let handlePropertyChange (key : 'key) handler (simulation : Simulation<'key>) = simulation.HandlePropertyChange key handler