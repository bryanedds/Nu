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
    
    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static let getSpriteInsetOpt animationState world =
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
                | CharacterAnimationFacing -> Constants.InfinityRpg.CharacterAnimationFacingDelay
                | CharacterAnimationActing -> Constants.InfinityRpg.CharacterAnimationActingDelay
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
                abs (World.getTickTime world - animationState.StartTime) /
                animationDelay % int64 animationFrames |>
                int
            let animatedOffsetC = v2i animatedXOffsetC 0
            let spriteCoordinates = animationOffsetC + directionOffsetC + animatedOffsetC
            let spriteOffset =
                v2
                    (Constants.Layout.TileSize.X * single spriteCoordinates.X)
                    (Constants.Layout.TileSize.Y * single spriteCoordinates.Y)
            let spriteInset = v4Bounds spriteOffset Constants.Layout.TileSize
            Some spriteInset

        static member Properties =
            [define Entity.Size Constants.Layout.TileSize
             define Entity.Elevation Constants.Layout.CharacterElevation
             define Entity.PublishChanges true
             define Entity.Omnipresent true
             define Entity.CharacterAnimationState CharacterAnimationState.initial
             define Entity.CharacterAnimationSheet Assets.Gameplay.PlayerImage]
        
        override this.Actualize (entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                let animationState = entity.GetCharacterAnimationState world
                let animationSheet = entity.GetCharacterAnimationSheet world
                let renderMessage =
                    LayeredDescriptorMessage
                        { Elevation = transform.Elevation
                          PositionY = transform.Position.Y
                          AssetTag = AssetTag.generalize animationSheet
                          RenderDescriptor =
                            SpriteDescriptor
                              { Transform = transform
                                Offset = v2Zero
                                InsetOpt = getSpriteInsetOpt animationState world
                                Image = animationSheet
                                Color = Color.White
                                Glow = Color.Zero
                                Flip = FlipNone }}
                World.enqueueRenderMessage renderMessage world
            else world