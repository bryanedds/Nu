// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// Something to examine or use - a typewriter, a note, the item box, a panel - its own position the spot to stand at,
/// its looks the scenery's, the gameplay screen running its cue when it's acted on.
type FeatureDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<InteractableFacet>]

    static member Properties =
        [define Entity.Size (v3Dup 0.3f)]
