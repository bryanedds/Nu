namespace OmniBlade
open Nu
open OmniBlade

type OmniPlugin () =
    inherit NuPlugin ()
    override this.GetGameDispatcher () = typeof<OmniDispatcher>
    override this.GetEditorScreenDispatcher () = typeof<FieldDispatcher>
    override this.MakeOverlayRoutes () =
        [(typeof<ButtonDispatcher>.Name, Some "ButtonDispatcherRouted")
         (typeof<TextDispatcher>.Name, Some "TextDispatcherRouted")]