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
        member this.GetCharacter = this.GetModel<Character>
        member this.SetCharacter = this.SetModel<Character>
        member this.Character = this.Model<Character> ()
    
    type CharacterDispatcher () =
        inherit EntityDispatcher<Character, unit, unit> (Character.initial)

        static let getSpriteInsetOpt character world =
            let animationState = character.CharacterAnimationState
            let animationFrames =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> 2
                | CharacterAnimationActing -> 2
                | CharacterAnimationDefending -> 1
                | CharacterAnimationSpecial -> 1
                | CharacterAnimationSlain -> 1
            let animationOffsetM =
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
            let directionCoordsOffset =
                match animationState.Direction with
                | Upward -> v2i 0 0
                | Rightward -> v2i animationFrames 0
                | Downward -> v2i 0 1
                | Leftward -> v2i animationFrames 1
            let animatedXOffsetM =
                abs (World.getTickTime world - animationState.StartTime) /
                animationDelay % int64 animationFrames |>
                int
            let animatedOffsetM = v2i animatedXOffsetM 0
            let spriteCoordsinates = animationOffsetM + directionCoordsOffset + animatedOffsetM
            let spriteOffset =
                v2
                    (Constants.Layout.TileSize.X * single spriteCoordsinates.X)
                    (Constants.Layout.TileSize.Y * single spriteCoordsinates.Y)
            let spriteInset = v4Bounds spriteOffset Constants.Layout.TileSize
            Some spriteInset

        static member Properties =
            [define Entity.Depth Constants.Layout.CharacterDepth
             define Entity.PublishChanges true
             define Entity.Omnipresent true]

        override this.Initializers (character, _) =
            [Entity.Position <== character --> fun character -> character.Position]
        
        override this.View (character, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                [Render (transform.Depth, transform.Position.Y, AssetTag.generalize character.CharacterAnimationSheet,
                     SpriteDescriptor
                       { Transform = transform
                         Offset = v2Zero
                         InsetOpt = getSpriteInsetOpt character world
                         Image = character.CharacterAnimationSheet
                         Color = Color.White
                         Glow = Color.Zero
                         Flip = FlipNone })]
            else []