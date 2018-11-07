namespace Nu
open Prime

/// Handles simulation property changes.
type PropertyChangeHandler<'key> = Simulation<'key> -> Simulation<'key> -> Simulation<'key>

/// A simulation in terms of an observable property bag.
and Simulation<'key> =
    abstract member GetProperty<'a> : 'key -> 'a option
    abstract member SetProperty<'a> : 'key -> 'a option -> Simulation<'key>
    abstract member HandlePropertyChange : 'key -> PropertyChangeHandler<'key> -> (Simulation<'key> -> Simulation<'key>) * Simulation<'key>

[<RequireQualifiedAccess>]
module Simulation =
    let getProperty (key : 'key) (simulation : Simulation<'key>) = simulation.GetProperty key
    let setProperty (key : 'key) (valueOpt : 'a option) (simulation : Simulation<'key>) = simulation.SetProperty key valueOpt
    let handlePropertyChange (key : 'key) handler (simulation : Simulation<'key>) = simulation.HandlePropertyChange key handler