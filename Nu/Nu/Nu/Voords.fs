namespace Nu
open System
open System.Configuration
module Voords =

    (* WISDOM: Virtual coordinates can be necessary, but they should be avoided if possible.
    Virtual resolution is based on PSVita as it's pretty much the lowest common denominator for
    modern mobile resolutions. Devices with other resolutions will have to have their displays
    padded with extra background imagery. *)

    let getActualResolutionOrDefault isX defaultResolution =
        let actualResolution = ref 0
        let appSetting = ConfigurationManager.AppSettings.["ActualResolution" + if isX then "X" else "Y"]
        if not <| Int32.TryParse (appSetting, actualResolution) then actualResolution := defaultResolution
        !actualResolution

    // TODO: implement virtual scaling in renderer / editor view / et al.
    let ActualResolutionX = getActualResolutionOrDefault true 960
    let ActualResolutionY = getActualResolutionOrDefault false 544
    let VirtualResolutionX = 960
    let VirtualResolutionY = 544
    let VirtualMultiplierX = single VirtualResolutionX / single ActualResolutionX
    let VirtualMultiplierY = single VirtualResolutionY / single ActualResolutionY