namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<StructuralEquality; NoComparison>] PropType =
    | LongGrass

type [<StructuralEquality; NoComparison>] PropAnimationType =
    | PropAnimationStanding
    | PropAnimationDestroyed

type [<ReferenceEquality; NoComparison>] Prop =
    { PropType : PropType
      PropImage : Image AssetTag
      PropAnimationType : PropAnimationType
      Position : Vector2 }

    static member initial =
        { PropType = LongGrass
          PropImage = Assets.Gameplay.LongGrassImage
          PropAnimationType = PropAnimationStanding
          Position = v2Zero}
    
    static member makeLongGrass coordinates propAnimationType =
        { PropType = LongGrass
          PropImage = Assets.Gameplay.LongGrassImage
          PropAnimationType = propAnimationType
          Position = vctovf coordinates }