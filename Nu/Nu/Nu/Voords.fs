namespace Nu
open System
open System.Configuration
module Voords =

    (* WISDOM: Virtual coordinates can be necessary, but they should be avoided if possible.
    Virtual resolution is based on PSVita as it's pretty much the lowest common denominator for
    modern mobile resolutions. Devices with other ratios will have to have their displays padded
    with extra background imagery. *)

    // TODO: implement virtual scaling in renderer / editor view / et al.
    let ActualResolutionX = Int32.Parse ConfigurationManager.AppSettings.["ActualResolutionX"]
    let ActualResolutionY = Int32.Parse ConfigurationManager.AppSettings.["ActualResolutionY"]
    let VirtualResolutionX = 960
    let VirtualResolutionY = 544
    let VirtualMultiplierX = single VirtualResolutionX / single ActualResolutionX
    let VirtualMultiplierY = single VirtualResolutionY / single ActualResolutionY