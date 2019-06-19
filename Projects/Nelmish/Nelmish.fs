namespace Nelmish
open System
open Prime
open Nu
open Nu.Declarative
module Nelmish =

    let Game = new Game ()
    let Screen = new Screen "Screen"
    let Layer = Screen => "Layer"
    let IncButton = Layer => "Inc"
    let DecButton = Layer => "Dec"
    let CountText = Layer => "Count"

    type Model =
        int

    type Message =
        | Increment
        | Decrement

    type Game with
        member this.GetModel world : Model = this.Get Property? Model world
        member this.SetModel (value : Model) world = this.Set Property? Model value world
        member this.Model = PropertyTag.make this Property? Model this.GetModel this.SetModel

    type NelmishDispatcher () =
        inherit GameDispatcher<Model, Message, unit> (fun game -> game.Model)

        override this.Bindings (_, _, _) =
            [IncButton.ClickEvent ==> Increment
             DecButton.ClickEvent ==> Decrement]

        override this.Update (message, model, _, _) =
            match message with
            | Increment -> just (inc model)
            | Decrement -> just (dec model)

        override this.Command (_, _, _, world) =
            world

        override this.Layout (_, _, _) =
            [Layout.screen Screen Vanilla []
                [Layout.layer Layer []
                    [Layout.entity<ButtonDispatcher> IncButton [Entity.Text == "Increment"]
                     Layout.entity<ButtonDispatcher> DecButton [Entity.Text == "Decrement"]
                     Layout.entity<TextDispatcher> CountText [Entity.Text != Game.Model.MapOut scstring]]]]