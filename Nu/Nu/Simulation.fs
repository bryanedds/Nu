namespace Nu

/// A generalization of simulations in terms of an observable property bag.
type Simulation<'key, 'uuid> =
    abstract member GetProperty : 'key -> obj option
    abstract member SetProperty : 'key -> obj option -> Simulation<'key, 'uuid>
    abstract member HandlePropertyChange : 'key -> PropertyChangeHandler<'key, 'uuid> -> ('uuid * Simulation<'key, 'uuid>)
    abstract member ForgetPropertyChange : 'uuid -> Simulation<'key, 'uuid>

/// Handles property changes.
and PropertyChangeHandler<'key, 'uuid> = Simulation<'key, 'uuid> -> Simulation<'key, 'uuid> -> Simulation<'key, 'uuid>

[<RequireQualifiedAccess>]
module Simulation =
    let getProperty (key : 'key) (simulation : Simulation<'key, 'uuid>) = simulation.GetProperty key
    let setProperty (key : 'key) (valueOpt : obj option) (simulation : Simulation<'key, 'uuid>) = simulation.SetProperty key valueOpt
    let handlePropertyChange (key : 'key) handler (simulation : Simulation<'key, 'uuid>) = simulation.HandlePropertyChange key handler
    let forgetPropertyChange (uuid : 'uuid) (simulation : Simulation<'key, 'uuid>) = simulation.ForgetPropertyChange uuid
