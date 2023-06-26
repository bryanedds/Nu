// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open Nu

/// The default plugin used by Gaia when a plugin from a game is not utilized.
type GaiaPlugin () =
    inherit NuPlugin ()
    override this.ImGuiProcess world =
        ImGuiNET.ImGui.Begin "asd" |> ignore<bool>
        ImGuiNET.ImGui.Text "asdASDASDASD"
        ImGuiNET.ImGui.End ()
        world