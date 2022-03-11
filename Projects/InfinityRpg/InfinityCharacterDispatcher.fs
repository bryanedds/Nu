namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module CharacterDispatcherModule =
    
    type Entity with
        member this.GetCharacterAnimationState = this.Get Property? CharacterAnimationState
        member this.SetCharacterAnimationState = this.Set Property? CharacterAnimationState
        member this.CharacterAnimationState = lens<CharacterAnimationState> Property? CharacterAnimationState this.GetCharacterAnimationState this.SetCharacterAnimationState this
        member this.GetCharacterAnimationSheet = this.Get Property? CharacterAnimationSheet
        member this.SetCharacterAnimationSheet = this.Set Property? CharacterAnimationSheet
        member this.CharacterAnimationSheet = lens<Image AssetTag> Property? CharacterAnimationSheet this.GetCharacterAnimationSheet this.SetCharacterAnimationSheet this
        member this.GetCharacterAnimationTime = this.Get Property? CharacterAnimationTime
        member this.SetCharacterAnimationTime = this.Set Property? CharacterAnimationTime
        member this.CharacterAnimationTime = lens<int64> Property? CharacterAnimationTime this.GetCharacterAnimationTime this.SetCharacterAnimationTime this
    
    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static let getSpriteInsetOpt animationState time =
            let referenceTime =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> 0L
                | _ -> animationState.StartTime
            let animationFrames =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> 2
                | CharacterAnimationActing -> 2
                | CharacterAnimationDefending -> 1
                | CharacterAnimationSpecial -> 1
                | CharacterAnimationSlain -> 1
            let animationOffsetC =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> v2i 0 0
                | CharacterAnimationActing -> v2i 0 2
                | CharacterAnimationDefending -> v2i 4 0
                | CharacterAnimationSpecial -> v2i 6 0
                | CharacterAnimationSlain -> v2i 4 2
            let animationDelay =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> Constants.Gameplay.CharacterAnimationFacingDelay
                | CharacterAnimationActing -> Constants.Gameplay.CharacterAnimationActingDelay
                | CharacterAnimationDefending -> 1L // doesn't matter - no animation frames
                | CharacterAnimationSpecial -> 1L // doesn't matter - no animation frames
                | CharacterAnimationSlain -> 1L // doesn't matter - no animation frames
            let directionOffsetC =
                match animationState.Direction with
                | Upward -> v2i 0 0
                | Rightward -> v2i animationFrames 0
                | Downward -> v2i 0 1
                | Leftward -> v2i animationFrames 1
            let animatedXOffsetC =
                abs (time - referenceTime) /
                animationDelay % int64 animationFrames |>
                int
            let animatedOffsetC = v2i animatedXOffsetC 0
            let spriteCoordinates = animationOffsetC + directionOffsetC + animatedOffsetC
            let spriteOffset =
                v2
                    (Constants.Gameplay.TileSize.X * single spriteCoordinates.X)
                    (Constants.Gameplay.TileSize.Y * single spriteCoordinates.Y)
            let spriteInset = v4Bounds spriteOffset Constants.Gameplay.TileSize
            Some spriteInset

        static member Properties =
            [define Entity.Size Constants.Gameplay.TileSize
             define Entity.Elevation Constants.Gameplay.CharacterElevation
             define Entity.Omnipresent true
             define Entity.CharacterAnimationState CharacterAnimationState.initial
             define Entity.CharacterAnimationSheet Assets.Gameplay.PlayerImage
             define Entity.CharacterAnimationTime 0L]

        override this.Actualize (entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let time = entity.GetCharacterAnimationTime world
                let transform = entity.GetTransform world
                let animationState = entity.GetCharacterAnimationState world
                let animationSheet = entity.GetCharacterAnimationSheet world
                let message =
                    { Elevation = transform.Elevation
                      PositionY = transform.Position.Y
                      AssetTag = AssetTag.generalize animationSheet
                      RenderDescriptor =
                        SpriteDescriptor
                          { Transform = transform
                            Absolute = entity.GetAbsolute world
                            Offset = v2Zero
                            InsetOpt = getSpriteInsetOpt animationState time
                            Image = animationSheet
                            Color = Color.White
                            Blend = Transparent
                            Glow = Color.Zero
                            Flip = FlipNone }}
                World.enqueueRenderLayeredMessage message world
            else world