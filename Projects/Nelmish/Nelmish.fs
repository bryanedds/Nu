namespace Nelmish
open Prime
open OpenTK
open Nu
open Nu.Declarative
module Nelmish =

    // here we create references for the entities that we are going to define for our game
    let DecrementButton = Default.Layer / "Decrement"
    let IncrementButton = Default.Layer / "Increment"
    let CounterText = Default.Layer / "Counter"
    let ResetButton = Default.Layer / "Reset"

    // this is our Elm-style model type
    type Model =
        int

    // this is our Elm-style message type
    type Message =
        | Decrement
        | Increment
        | Reset

    // this is our Elm-style game dispatcher
    type NelmishDispatcher () =
        inherit GameDispatcher<Model, Message, unit> (0) // initial model value

        // here we define the Bindings used to connect events to their desired messages
        override this.Bindings (_, _, _) =
            [DecrementButton.ClickEvent => Decrement
             IncrementButton.ClickEvent => Increment
             ResetButton.ClickEvent => Reset]

        // here we handle the above messages
        override this.Message (message, model, _, _) =
            match message with
            | Decrement -> just (dec model)
            | Increment -> just (inc model)
            | Reset -> just 0

        // here we describe the content of the game including its one screen, one layer, three
        // button entities, and one text control.
        override this.Content (_, game, _) =
            [Content.screen Default.Screen Vanilla []
                [Content.layer Default.Layer []
                    [Content.button DecrementButton
                        [Entity.Text == "-"
                         Entity.Position == Vector2 (-256.0f, 64.0f)]
                     Content.button IncrementButton
                        [Entity.Text == "+"
                         Entity.Position == Vector2 (0.0f, 64.0f)]
                     Content.text CounterText
                        [Entity.Text ==> game.Model this --> scstring
                         Entity.Position == Vector2 (-128.0f, -32.0f)]
                     Content.entityIf (game.Model this --> isNonZero) $ fun () ->
                        Content.button ResetButton
                            [Entity.Text == "Reset"
                             Entity.Position == Vector2 (-128.0f, -128.0f)]]]]