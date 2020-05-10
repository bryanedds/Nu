namespace OmniBlade
open Nu
open OmniBlade

type OmniPlugin () =
    inherit NuPlugin ()
    override this.GetStandAloneGameDispatcher () = typeof<StandAloneDispatcher>
    override this.GetEditorGameDispatcher () = typeof<EditorDispatcher>
    override this.GetEditorScreenDispatcherOpt () = Some typeof<FieldDispatcher>
    override this.MakeOverlayRoutes () = [typeof<ButtonDispatcher>.Name, Some "ButtonDispatcherRouted"]