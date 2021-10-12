// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open Nu
open OmniBlade

type OmniPlugin () =
    inherit NuPlugin ()
    override this.StandAloneConfig = typeof<OmniBladeDispatcher>
    override this.EditorConfig = (Simulants.Field.Screen, typeof<DebugFieldDispatcher>)