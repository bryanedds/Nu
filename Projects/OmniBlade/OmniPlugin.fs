namespace OmniBlade
open Nu
open OmniBlade

type OmniPlugin () =
    inherit NuPlugin ()
    override this.GetGameDispatcher () = typeof<OmniDispatcher>
    override this.GetEditorScreenDispatcher () = typeof<FieldDispatcher>
    override this.MakeOverlayRoutes () =
        [(typeof<ButtonDispatcher>.Name, Some "ButtonDispatcherRoute")
         (typeof<TextDispatcher>.Name, Some "TextDispatcherRoute")]