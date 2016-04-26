namespace InfinityRpg
open System
open SDL2
open OpenTK
open Prime
open Prime.Observation
open Nu
open InfinityRpg

[<AutoOpen>]
module CharacterStateFacetModule =

    type Entity with

        member this.GetCharacterType world : CharacterType = (this.GetXtension world)?CharacterType
        member this.SetCharacterType (value : CharacterType) world = this.UpdateXtension (fun xtension -> xtension?ActivityState <- value) world
        member this.GetActivityState world : ActivityState = (this.GetXtension world)?ActivityState
        member this.SetActivityState (value : ActivityState) world = this.UpdateXtension (fun xtension -> xtension?ActivityState <- value) world
        member this.GetHitPoints world : int = (this.GetXtension world)?HitPoints
        member this.SetHitPoints (value : int) world = this.UpdateXtension (fun xtension -> xtension?HitPoints <- value) world
        member this.GetSpecialPoints world : int = (this.GetXtension world)?SpecialPoints
        member this.SetSpecialPoints (value : int) world = this.UpdateXtension (fun xtension -> xtension?SpecialPoints <- value) world
        member this.GetPowerBuff world : single = (this.GetXtension world)?PowerBuff
        member this.SetPowerBuff (value : single) world = this.UpdateXtension (fun xtension -> xtension?PowerBuff <- value) world
        member this.GetShieldBuff world : single = (this.GetXtension world)?ShieldBuff
        member this.SetShieldBuff (value : single) world = this.UpdateXtension (fun xtension -> xtension?ShieldBuff <- value) world
        member this.GetMindBuff world : single = (this.GetXtension world)?MindBuff
        member this.SetMindBuff (value : single) world = this.UpdateXtension (fun xtension -> xtension?MindBuff <- value) world
        member this.GetCounterBuff world : single = (this.GetXtension world)?CounterBuff
        member this.SetCounterBuff (value : single) world = this.UpdateXtension (fun xtension -> xtension?CounterBuff <- value) world
        member this.GetStatuses world : StatusType Set = (this.GetXtension world)?Statuses
        member this.SetStatuses (value : StatusType Set) world = this.UpdateXtension (fun xtension -> xtension?Statuses <- value) world
        member this.GetEquippedWeapon world : WeaponType option = (this.GetXtension world)?EquippedWeapon
        member this.SetEquippedWeapon (value : WeaponType option) world = this.UpdateXtension (fun xtension -> xtension?EquippedWeapon <- value) world
        member this.GetEquippedArmor world : ArmorType option = (this.GetXtension world)?EquippedArmor
        member this.SetEquippedArmor (value : ArmorType option) world = this.UpdateXtension (fun xtension -> xtension?EquippedArmor <- value) world
        member this.GetEquippedRelics world : RelicType list = (this.GetXtension world)?EquippedRelics
        member this.SetEquippedRelics (value : RelicType list) world = this.UpdateXtension (fun xtension -> xtension?EquippedRelics <- value) world
        member this.GetControlType world : ControlType = (this.GetXtension world)?ControlType
        member this.SetControlType (value : ControlType) world = this.UpdateXtension (fun xtension -> xtension?ControlType <- value) world

    type CharacterStateFacet () =
        inherit Facet ()

        static member PropertyDefinitions =
            [Define? CharacterType Player
             Define? ActivityState NoActivity
             Define? HitPoints 10 // note this is an arbitrary number as hp max is calculated
             Define? SpecialPoints 1 // sp max is calculated
             Define? PowerBuff 1.0f // rate at which power is buffed / debuffed
             Define? ShieldBuff 1.0f // rate at which shield is buffed / debuffed
             Define? MindBuff 1.0f // rate at which mind is buffed / debuffed
             Define? CounterBuff 1.0f // rate at which counter is buffed / debuffed
             Define? Statuses Set.empty<StatusType>
             Define? EquippedWeapon Option<WeaponType>.None
             Define? EquippedArmor Option<ArmorType>.None
             Define? EquippedRelics list<RelicType>.Empty
             Define? ControlType Uncontrolled] // level is calculated from base experience + added experience

[<AutoOpen>]
module CharacterAnimationFacetModule =

    type CharacterAnimationType =
        | CharacterAnimationFacing
        | CharacterAnimationActing
        | CharacterAnimationDefending
        | CharacterAnimationSpecial // works for jump, cast magic, being healed, and perhaps others!
        | CharacterAnimationSlain

    type CharacterAnimationState =
        { AnimationType : CharacterAnimationType
          Direction : Direction
          StartTime : int64 }

    type Entity with
    
        member this.GetCharacterAnimationState world : CharacterAnimationState = (this.GetXtension world)?CharacterAnimationState
        member this.SetCharacterAnimationState (value : CharacterAnimationState) world = this.UpdateXtension (fun xtension -> xtension?CharacterAnimationState <- value) world
        member this.GetCharacterAnimationSheet world : AssetTag = (this.GetXtension world)?CharacterAnimationSheet
        member this.SetCharacterAnimationSheet (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?CharacterAnimationSheet <- value) world

    type CharacterAnimationFacet () =
        inherit Facet ()
        
        static let getOptSpriteInset (entity : Entity) world =
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
            [Define?
                CharacterAnimationState
                    { AnimationType = CharacterAnimationFacing
                      Direction = Upward
                      StartTime = 0L }
             Define? CharacterAnimationSheet Constants.Assets.PlayerImage]

        override facet.Actualize (entity, world) =
            if entity.InView world then
                World.addRenderMessage
                    (RenderDescriptorsMessage
                        [LayerableDescriptor
                            { Depth = entity.GetDepth world
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = Vector2.Zero
                                      ViewType = entity.GetViewType world
                                      OptInset = getOptSpriteInset entity world
                                      Image = entity.GetCharacterAnimationSheet world
                                      Color = Vector4.One }}])
                    world
            else world

[<AutoOpen>]
module CharacterCameraFacetModule =

    type CharacterCameraFacet () =
        inherit Facet ()

        static let handleUpdate evt world =
            let character = evt.Subscriber : Entity
            let world =
                World.updateCamera (fun camera ->
                    let eyeCenter = character.GetPosition world + character.GetSize world * 0.5f
                    let eyeCenter =
                        if World.containsEntity Simulants.Field world then
                            let eyeSize = camera.EyeSize
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
                    { camera with EyeCenter = eyeCenter })
                    world
            (Cascade, world)

        override facet.Register (entity, world) =
            monitor handleUpdate (observe (Events.Update ->- entity) entity) world