namespace Nelmish
open Prime
open OpenTK
open Nu
open Nu.Declarative
module Nelmish =

    // here we create references for the entities that we are going to define for our game
    let DecrementButton = Default.Layer => "Decrement"
    let IncrementButton = Default.Layer => "Increment"
    let CounterButton = Default.Layer => "Counter"

    // this is our Elm-style model type
    type Model =
        int * unit list

    // this is our Elm-style message type
    type Message =
        | Decrement
        | Increment
        | Reset

    // this is our model property declaration
    type Game with
        member this.GetModel = this.Get Property? Model
        member this.SetModel = this.Set Property? Model
        member this.Model = Lens.make<Model, World> Property? Model this.GetModel this.SetModel this

    // this is our Elm-style game dispatcher
    type NelmishDispatcher () =
        inherit GameDispatcher<Model, Message, unit> (fun game -> game.Model)

        // this is our model property definition
        static member Properties =
            [define Game.Model (0, [])]

        // here we define the Bindings used to connect events to their desired messages
        override this.Bindings (_, _, _) =
            [DecrementButton.ClickEvent ==> Decrement
             IncrementButton.ClickEvent ==> Increment
             CounterButton.ClickEvent ==> Reset]

        // here we handle the above messages
        override this.Update (message, model, _, _) =
            match message with
            | Decrement -> just (dec (fst model), snd model)
            | Increment -> just (inc (fst model), () :: snd model)
            | Reset -> just (0, [])

        // here we describe the layout of the game including its one screen, one layer, and three button entities
        override this.Layout (_, game, _) =
            [Layout.screen Default.Screen Vanilla []
                [Layout.layer Default.Layer []
                    [Layout.entities
                        (game.Model --> snd) $ fun () -> Layout.button Default.Entity [Entity.Text == "Alpha"]
                     Layout.button DecrementButton
                        [Entity.Text == "-"
                         Entity.Position == Vector2 (-256.0f, 64.0f)]
                     Layout.button IncrementButton
                        [Entity.Text == "+"
                         Entity.Position == Vector2 (0.0f, 64.0f)]
                     Layout.button CounterButton
                        [Entity.Text =|= game.Model --> string
                         Entity.Position == Vector2 (-128.0f, -32.0f)]]]]