// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds.

namespace Nu.Gaia
open System
open System.IO
open System.Runtime.InteropServices
open Nu
open Nu.Gaia
module Program =
    [<DllImport("user32.dll")>]
    extern bool SetProcessDPIAware()
    let [<EntryPoint; STAThread>] main _ =
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        if OperatingSystem.IsWindows() then SetProcessDPIAware() |> ignore // Disable DPI zoom on Windows, otherwise the scaled UI will be cut off
        let (gaiaState, targetDir, plugin) = Nu.initPlus (fun () -> Gaia.selectNuPlugin (GaiaPlugin ()))
        Gaia.run gaiaState targetDir plugin
