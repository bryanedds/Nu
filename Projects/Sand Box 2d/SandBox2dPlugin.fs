namespace SandBox2d
open System
open Nu
open SandBox2d

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type SandBox2dPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        [Simulants.SandBox; Simulants.RaceCourse]
        |> List.map (fun s -> s.Name, Game.SetDesiredScreen (Desire s))
        |> Map.ofList