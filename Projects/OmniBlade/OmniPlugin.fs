// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open Nu
open OmniBlade

type OmniPlugin () =
    inherit NuPlugin ()
    override this.GetGameDispatcher () = typeof<OmniDispatcher>
    override this.GetEditorScreenDispatcher () = (Simulants.Battle.Screen, typeof<BattleDispatcher>)
    override this.MakeOverlayRoutes () =
        [(typeof<ButtonDispatcher>.Name, Some "ButtonDispatcherRoute")
         (typeof<TextDispatcher>.Name, Some "TextDispatcherRoute")]