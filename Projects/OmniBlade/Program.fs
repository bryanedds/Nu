namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade
module Program =

    type OmniPlugin () =
        inherit NuPlugin ()

        override this.MakeGameDispatchers () =
            [StandAloneDispatcher () :> GameDispatcher
             EditorDispatcher () :> GameDispatcher]

        override this.MakeScreenDispatchers () =
            [BattleDispatcher () :> ScreenDispatcher]

        override this.MakeEntityDispatchers () =
            [CharacterDispatcher () :> EntityDispatcher
             ReticlesDispatcher () :> EntityDispatcher
             RingMenuDispatcher () :> EntityDispatcher]

        override this.GetStandAloneGameDispatcherName () =
            typeof<StandAloneDispatcher>.Name

        override this.GetEditorGameDispatcherName () =
            typeof<EditorDispatcher>.Name

        override this.GetEditorGameplayScreenDispatcherNameOpt () =
            Some typeof<BattleDispatcher>.Name

        override this.MakeOverlayRoutes () =
            [typeof<ButtonDispatcher>.Name, Some "ButtonDispatcherRouted"]

    let [<EntryPoint; STAThread>] main _ =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "OmniBlade" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        Nu.init worldConfig.NuConfig
        let tryMakeWorld sdlDeps worldConfig =
            let plugin = OmniPlugin ()
            World.tryMake plugin sdlDeps worldConfig
        World.run tryMakeWorld worldConfig