// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// A box of an area - a wall, a floor, a desk's top, a cabinet - a static model sized by its scale, flat colored by
/// its material properties. What's solid to walkers is the plan's say, not the block's.
type BlockDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<StaticModelFacet>]

    static member Properties =
        [define Entity.StaticModel Assets.Gameplay.BoxModel
         define Entity.Material SurfaceType.BasicMaterial
         define Entity.Static true]
