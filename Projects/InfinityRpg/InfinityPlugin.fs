namespace InfinityRpg
open Nu
open InfinityRpg

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type InfinityPlugin () =
    inherit NuPlugin ()

    // specify the game dispatcher to use at run-time
    override this.GetGameDispatcher () =
        typeof<InfinityDispatcher>
        
    // specify the screen dispatcher to use in the editor
    override this.GetEditorScreenDispatcher () =
        (Simulants.Gameplay.Screen, typeof<GameplayDispatcher>)

    // route overlays to specific dispatchers
    override this.MakeOverlayRoutes () =
        [(typeof<ButtonDispatcher>.Name, Some "ButtonDispatcherRoute")
         (typeof<TextDispatcher>.Name, Some "TextDispatcherRoute")]