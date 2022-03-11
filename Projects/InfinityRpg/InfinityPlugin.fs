namespace InfinityRpg
open Nu
open InfinityRpg

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type InfinityPlugin () =
    inherit NuPlugin ()

    // specify the game dispatcher to use at run-time
    override this.StandAloneConfig =
        typeof<InfinityDispatcher>
        
    // specify the screen dispatcher to use in the editor
    override this.EditorConfig =
        (Simulants.Gameplay.Screen, typeof<GameplayDispatcher>)