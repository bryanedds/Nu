// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open Prime
open Nu

/// A model-message-command-content (MMCC) signal tag type.
type Signal = interface end

/// A model-message-command-content (MMCC) message tag type.
type Message = inherit Signal

/// A model-message-command-content (MMCC) command tag type.
type Command = inherit Signal

[<AutoOpen>]
module SignalOperators =

    /// Signal constructor.
    /// Wonky name because F# reserve `sig` as a keyword.
    let inline signal<'s when 's :> Signal> (signal : 's) = signal :> Signal

    /// Singleton signal-value pair constructor.
    let inline withSignal (signal : Signal) value = ([signal], value)

    /// Signals-value pair constructor.
    let inline withSignals (signals : Signal list) value = (signals, value)

    /// Signaless signals-value pair constructor.
    let inline just value = (([] : Signal list), value)

[<RequireQualifiedAccess>]
module Signal =

    let rec
        processSignal<'model, 'message, 'command, 's, 'w when 'message :> Message and 'command :> Command and 's :> Simulant>
        (processMessage : 'model * 'message * 's * 'w -> Signal list * 'model)
        (processCommand : 'model * 'command * 's * 'w -> Signal list * 'w)
        (modelLens : Lens<'model, 's, 'w>)
        (signal : Signal)
        (simulant : 's)
        (world : 'w) :
        'w =
        match signal :> obj with
        | :? 'message as message ->
            let model = Lens.get modelLens world
            let (signals, model) = processMessage (model, message, simulant, world)
            let world = Lens.set model modelLens world
            match signals with
            | _ :: _ -> processSignals processMessage processCommand modelLens signals simulant world
            | [] -> world
        | :? 'command as command ->
            let model = Lens.get modelLens world
            let (signals, world) = processCommand (model, command, simulant, world)
            match signals with
            | _ :: _ -> processSignals processMessage processCommand modelLens signals simulant world
            | [] -> world
        | _ -> failwithumf ()

    and processSignals processMessage processCommand modelLens signals simulant world =
        List.fold
            (fun world signal -> processSignal processMessage processCommand modelLens signal simulant world)
            world signals