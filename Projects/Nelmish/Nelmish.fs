namespace Nelmish
open Prime
open Nu

// this is our Elm-style model type
type Model =
    Model of int

// this is our Elm-style message type
type Message =
    | Decrement
    | Increment
    | Reset

// this is our Elm-style game dispatcher
type NelmishDispatcher () =
    inherit GameForger<Model, Message, unit> (Model 0) // initial model value

    // here we handle the Elm-style messages
    override this.Message (Model model, message, _, _) =
        match message with
        | Decrement -> just (Model (model - 1))
        | Increment -> just (Model (model + 1))
        | Reset -> just (Model 0)

    // here we describe the forge of the game including its one screen, one group, three
    // button entities, and one text control.
    override this.Forge (Model model, _) =
        Forge.game []
            [Forge.screen Simulants.Default.Screen.Name Vanilla []
                [Forge.group Simulants.Default.Screen.Name []
                    [yield Forge.button "Decrement"
                        [Entity.Text == "-"
                         Entity.Position == v3 -256.0f 64.0f 0.0f
                         Entity.ClickEvent ==> msg Decrement]
                     yield Forge.button "Increment"
                        [Entity.Text == "+"
                         Entity.Position == v3 0.0f 64.0f 0.0f
                         Entity.ClickEvent ==> msg Increment]
                     yield Forge.text "Counter"
                        [Entity.Text == string model
                         Entity.Position == v3 -128.0f -32.0f 0.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)]
                     if model <> 0 then
                        yield Forge.button "Reset"
                           [Entity.Text == "Reset"
                            Entity.Position == v3 -128.0f -128.0f 0.0f
                            Entity.ClickEvent ==> msg Reset]]]]