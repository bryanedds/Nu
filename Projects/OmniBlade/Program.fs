open System
open Prime
open Nu
open OmniBlade

type OmniPlugin () =
    inherit NuPlugin ()
    override this.GetStandAloneGameDispatcher () = typeof<StandAloneDispatcher>
    override this.GetEditorGameDispatcher () = typeof<EditorDispatcher>
    override this.GetEditorScreenDispatcherOpt () = Some typeof<BattleDispatcher>
    override this.MakeOverlayRoutes () = [typeof<ButtonDispatcher>.Name, Some "ButtonDispatcherRouted"]

let [<EntryPoint; STAThread>] main _ =
    let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "OmniBlade" }
    let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
    let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
    Nu.init worldConfig.NuConfig
    let tryMakeWorld sdlDeps worldConfig = World.tryMake (OmniPlugin ()) sdlDeps worldConfig
    World.run tryMakeWorld worldConfig