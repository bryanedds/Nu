// Gaia - The Nu Game Engine editor.
// Required Notice:
// Copyright (C) Bryan Edds.
// Gaia - The Nu Game Engine editor is licensed under the Nu Game Engine Noncommercial License.
// See: https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Gaia
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gaia =

        let [<Uniform>] LogCharactersMax = Constants.Runtime.LohSize / sizeof<char> - 5000 // NOTE: small enough to not allocate on the LOH.
        let [<Literal>] PositionSnap2dDefault = 8.0f
        let [<Literal>] DegreesSnap2dDefault = 5.0f
        let [<Literal>] ScaleSnap2dDefault = 0.1f
        let [<Uniform>] Snaps2dDefault = (PositionSnap2dDefault, DegreesSnap2dDefault, ScaleSnap2dDefault)
        let [<Literal>] PositionSnap3dDefault = 0.1f
        let [<Literal>] DegreesSnap3dDefault = 5.0f
        let [<Literal>] ScaleSnap3dDefault = 0.1f
        let [<Uniform>] Snaps3dDefault = (PositionSnap3dDefault, DegreesSnap3dDefault, ScaleSnap3dDefault)
        let [<Literal>] CreationElevationDefault = 0.0f
        let [<Literal>] DragMinimumSeconds = 0.25
        let [<Literal>] PropertyValueStrMemoEvictionAge = 5.0 // NOTE: this is a somewhat arbitrary number that works well for Omni Blade, but it's unknown how well it will hold up for more complex games.
        let [<Literal>] HeightRegularPickOffset = 0.0f // NOTE: this used to be a value of -0.05f in order to make quick-picked names align with the top of drop downs better, but it had to be zero'd out to satisfy an assertion that it be in range from 0.0f - 1.0f.
        let [<Literal>] Eye2dPanSpeed = 2.0f // NOTE: might be nice to be able to configure this just like entity creation elevation in the editor.
        let [<Literal>] Eye2dPanSpeedFast = 6.0f // how many times faster than the mouse the 2d eye pans with enter or ctrl held.
        let [<Literal>] Eye2dPanSpeedSlow = 0.5f // how many times faster than the mouse the 2d eye pans with shift held.
        let [<Literal>] Eye3dPanSpeed = 4.0f // how many times faster than the world under the mouse the 3d eye pans.
        let [<Literal>] Eye3dPanSpeedFast = 12.0f // how many times faster it pans with enter or ctrl held.
        let [<Literal>] Eye3dPanSpeedSlow = 1.0f // how many times faster it pans with shift held.
        let [<Literal>] Eye3dOrbitSpeed = 0.005f // radians the 3d eye orbits per pixel dragged.
        let [<Literal>] Eye3dScrollTravel = 1.0f // how far the 3d eye travels forward or back per notch scrolled.
        let [<Literal>] Eye3dScrollTravelFast = 8.0f // how far it travels per notch with enter or ctrl held.
        let [<Literal>] Eye3dScrollTravelSlow = 0.125f // how far it travels per notch with shift held.
        let [<Literal>] Eye3dPitchMaximum = 1.55f // the most the 3d eye pitches up or down as it orbits, in radians, short of upside-down.
        let [<Literal>] Eye3dDragDistanceMinimum = 0.1f // the least distance from the 3d eye to its pivot that 3d panning goes by.
        let [<Literal>] ImGuiIniFilePath = "imgui.ini"
        let [<Literal>] StateFilePath = "Nu.Gaia.State.txt"
        let [<Literal>] InteractiveInputFilePath = "input.fsx"
        let [<Literal>] NonePick = "\"None\""
        let [<Uniform>] EventFilter =
            NotAny
                [Pattern (Rexpr "PreUpdate", [])
                 Pattern (Rexpr "Update", [])
                 Pattern (Rexpr "PostUpdate", [])
                 Pattern (Rexpr "Render", [])
                 Pattern (Rexpr "Change", [])
                 Pattern (Rexpr "Integration", [])
                 Pattern (Rexpr "BodyTransform", [])
                 Pattern (Rexpr "Mouse/Move", [])]
        let [<Literal>] BuildName =
#if DEBUG
            "Debug"
#else
            "Release"
#endif