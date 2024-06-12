// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open Nu.Effects
module Effects =

    /// Produces an aspect that simulates a hopping behavior.
    let Hop (start, stop, height, hopLength) =
        Aspects
            [|Positions
                (Sum, Linear, Once,
                 [|{ TweenValue = start; TweenLength = hopLength }
                   { TweenValue = stop; TweenLength = 0L }|])
              Positions
                (Sum, SinScaled 0.5f, Once,
                 [|{ TweenValue = v3Zero; TweenLength = hopLength }
                   { TweenValue = v3 0.0f height 0.0f; TweenLength = 0L }|])|]

    /// Produces an aspect that simulates a circling behavior.
    let Circle (radius, repetitions, length) =
        Aspects
            [|Positions
               (Sum, SinScaled repetitions, Once,
                [|{ TweenValue = v3Zero; TweenLength = length }
                  { TweenValue = v3 -radius 0.0f 0.0f; TweenLength = 0L }|])
              Positions
               (Sum, CosScaled repetitions, Once,
                [|{ TweenValue = v3Zero; TweenLength = length }
                  { TweenValue = v3 0.0f -radius 0.0f; TweenLength = 0L }|])
              Positions
               (Sum, Constant, Once,
                [|{ TweenValue = v3 0.0f radius 0.0f; TweenLength = length }
                  { TweenValue = v3 0.0f radius 0.0f; TweenLength = 0L }|])|]