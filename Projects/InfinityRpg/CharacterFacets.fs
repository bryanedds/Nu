namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module CharacterStateFacetModule =

    type Entity with

        member this.GetCharacterState world : CharacterState = this.Get Property? CharacterState world
        member this.SetCharacterState (value : CharacterState) world = this.Set Property? CharacterState value world
        member this.CharacterState = PropertyTag.make this Property? CharacterState this.GetCharacterState this.SetCharacterState
        member this.GetActivityState world : ActivityState = this.Get Property? ActivityState world
        member this.SetActivityState (value : ActivityState) world = this.Set Property? ActivityState value world
        member this.ActivityState = PropertyTag.make this Property? ActivityState this.GetActivityState this.SetActivityState

    type CharacterStateFacet () =
        inherit Facet ()

        static member PropertyDefinitions =
            [define Entity.CharacterState CharacterState.empty
             define Entity.ActivityState NoActivity]

[<AutoOpen>]
module CharacterAnimationFacetModule =

    type Entity with
    
        member this.GetCharacterAnimationState world : CharacterAnimationState = this.Get Property? CharacterAnimationState world
        member this.SetCharacterAnimationState (value : CharacterAnimationState) world = this.Set Property? CharacterAnimationState value world
        member this.CharacterAnimationState = PropertyTag.make this Property? CharacterAnimationState this.GetCharacterAnimationState this.SetCharacterAnimationState
        member this.GetCharacterAnimationSheet world : Image AssetTag = this.Get Property? CharacterAnimationSheet world
        member this.SetCharacterAnimationSheet (value : Image AssetTag) world = this.Set Property? CharacterAnimationSheet value world
        member this.CharacterAnimationSheet = PropertyTag.make this Property? CharacterAnimationSheet this.GetCharacterAnimationSheet this.SetCharacterAnimationSheet

    type CharacterAnimationFacet () =
        inherit Facet ()
        
        static let getSpriteInsetOpt (entity : Entity) world =
            let animationState = entity.GetCharacterAnimationState world
            let animationFrames =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> 2
                | CharacterAnimationActing -> 2
                | CharacterAnimationDefending -> 1
                | CharacterAnimationSpecial -> 1
                | CharacterAnimationSlain -> 1
            let animationOffsetM =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> Vector2i (0, 0)
                | CharacterAnimationActing -> Vector2i (0, 2)
                | CharacterAnimationDefending -> Vector2i (4, 0)
                | CharacterAnimationSpecial -> Vector2i (6, 0)
                | CharacterAnimationSlain -> Vector2i (4, 2)
            let animationStutter =
                match animationState.AnimationType with
                | CharacterAnimationFacing -> Constants.InfinityRpg.CharacterAnimationFacingStutter
                | CharacterAnimationActing -> Constants.InfinityRpg.CharacterAnimationActingStutter
                | CharacterAnimationDefending -> 1L // doesn't matter - no animation frames
                | CharacterAnimationSpecial -> 1L // doesn't matter - no animation frames
                | CharacterAnimationSlain -> 1L // doesn't matter - no animation frames
            let directionCoordsOffset =
                match animationState.Direction with
                | Upward -> Vector2i (0, 0)
                | Rightward -> Vector2i (animationFrames, 0)
                | Downward -> Vector2i (0, 1)
                | Leftward -> Vector2i (animationFrames, 1)
            let animatedXOffsetM =
                Math.Abs (World.getTickTime world - animationState.StartTime) /
                animationStutter % int64 animationFrames |>
                int
            let animatedOffsetM = Vector2i (animatedXOffsetM, 0)
            let spriteCoordsinates = animationOffsetM + directionCoordsOffset + animatedOffsetM
            let spriteOffset =
                Vector2
                    (Constants.Layout.TileSize.X * single spriteCoordsinates.X,
                     Constants.Layout.TileSize.Y * single spriteCoordsinates.Y)
            let spriteInset =
                Vector4
                    (spriteOffset.X,
                     spriteOffset.Y,
                     spriteOffset.X + Constants.Layout.TileSize.X,
                     spriteOffset.Y + Constants.Layout.TileSize.Y)
            Some spriteInset

        static member PropertyDefinitions =
            [define Entity.CharacterAnimationState
                    { StartTime = 0L
                      AnimationType = CharacterAnimationFacing
                      Direction = Upward }
             define Entity.CharacterAnimationSheet Assets.PlayerImage]

        override facet.Actualize (entity, world) =
            if entity.GetInView world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = entity.GetDepth world
                              PositionY = (entity.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = Vector2.Zero
                                      ViewType = entity.GetViewType world
                                      InsetOpt = getSpriteInsetOpt entity world
                                      Image = entity.GetCharacterAnimationSheet world
                                      Color = Vector4.One }}|])
                    world
            else world

[<AutoOpen>]
module CharacterCameraFacetModule =

    type CharacterCameraFacet () =
        inherit Facet ()

        static let handlePostUpdate evt world =
            let character = evt.Subscriber : Entity
            let eyeCenter = character.GetPosition world + character.GetSize world * 0.5f
            let eyeCenter =
                if Simulants.Field.GetExists world then
                    let eyeSize = World.getEyeSize world
                    let eyeCornerNegative = eyeCenter - eyeSize * 0.5f
                    let eyeCornerPositive = eyeCenter + eyeSize * 0.5f
                    let fieldCornerNegative = Simulants.Field.GetPosition world
                    let fieldCornerPositive = Simulants.Field.GetPosition world + Simulants.Field.GetSize world
                    let fieldBoundsNegative = fieldCornerNegative + eyeSize * 0.5f
                    let fieldBoundsPositive = fieldCornerPositive - eyeSize * 0.5f
                    let eyeCenterX =
                        if eyeCornerNegative.X < fieldCornerNegative.X then fieldBoundsNegative.X
                        elif eyeCornerPositive.X > fieldCornerPositive.X then fieldBoundsPositive.X
                        else eyeCenter.X
                    let eyeCenterY =
                        if eyeCornerNegative.Y < fieldCornerNegative.Y then fieldBoundsNegative.Y
                        elif eyeCornerPositive.Y > fieldCornerPositive.Y then fieldBoundsPositive.Y
                        else eyeCenter.Y
                    Vector2 (eyeCenterX, eyeCenterY)
                else eyeCenter
            World.setEyeCenter eyeCenter world

        override facet.Register (entity, world) =
            Stream.monitor handlePostUpdate entity (Stream.make entity.PostUpdateEvent) world