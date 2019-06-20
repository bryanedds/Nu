namespace Nelmish
open Prime
open OpenTK
open Nu
open Nu.Declarative
module Nelmish =

    // here we create references for the simulants that we are going to define for our game
    let Screen = !> "Screen"
    let Layer = Screen => "Layer"
    let IncButton = Layer => "Inc"
    let DecButton = Layer => "Dec"
    let CountText = Layer => "Count"

    // this is our Elm-style model type
    type Model =
        int

    // this is our Elm-style message type
    type Message =
        | Decrement
        | Increment

    // this is our model property declaration
    type Game with
        member this.GetModel = this.Get Property? Model
        member this.SetModel = this.Set Property? Model
        member this.Model = Lens.make<Model, World> Property? Model this.GetModel this.SetModel this

    // this is our Elm-style game simulant dispatcher
    type NelmishDispatcher () =
        inherit GameDispatcher<Model, Message, unit> (fun game -> game.Model)

        // here we define the value of our model
        static member Properties =
            [define Game.Model 0]

        // here we define the Bindings used to connect events to their desired messages
        override this.Bindings (_, _, _) =
            [DecButton.ClickEvent ==> Decrement
             IncButton.ClickEvent ==> Increment]

        // here we handle the above messages
        override this.Update (message, model, _, _) =
            match message with
            | Decrement -> just (dec model)
            | Increment -> just (inc model)

        // here we describe the layout of the game including its one screen, one layer, and three
        // entities, two of which are button controls and one of which is a text control
        override this.Layout (_, game, _) =
            [Layout.screen Screen Vanilla []
                [Layout.layer Layer []
                    [Layout.entity<ButtonDispatcher> DecButton
                        [Entity.Text == "-"
                         Entity.Position == Vector2 (-256.0f, 64.0f)]
                     Layout.entity<ButtonDispatcher> IncButton
                        [Entity.Text == "+"
                         Entity.Position == Vector2 (0.0f, 64.0f)]
                     Layout.entity<TextDispatcher> CountText
                        [Entity.Text =|= game.Model --> string
                         Entity.Position == Vector2 (-128.0f, -32.0f)]]]]