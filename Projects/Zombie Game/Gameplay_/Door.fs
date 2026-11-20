// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// A door of an area, acted on at the door before where one stands to open it, which is where it's declared - the
/// gameplay screen opening it, if it opens, or saying why it won't.
type DoorDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<InteractableFacet>]

    static member Properties =
        [define Entity.Size (v3Dup 0.3f)]
