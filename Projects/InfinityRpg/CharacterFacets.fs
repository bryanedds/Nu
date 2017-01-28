namespace InfinityRpg
open System
open OpenTK
open Prime
open Prime.Stream
open Nu
open InfinityRpg

[<AutoOpen>]
module CharacterStateFacetModule =

    type Entity with

        member this.GetCharacterType world : CharacterType = this.Get Property? CharacterType world
        member this.SetCharacterType (value : CharacterType) world = this.Set Property? ActivityState value world
        member this.CharacterType = PropertyTag.make this Property? CharacterType this.GetCharacterType this.SetCharacterType
        member this.GetActivityState world : ActivityState = this.Get Property? ActivityState world
        member this.SetActivityState (value : ActivityState) world = this.Set Property? ActivityState value world
        member this.ActivityState = PropertyTag.make this Property? ActivityState this.GetActivityState this.SetActivityState
        member this.GetHitPoints world : int = this.Get Property? HitPoints world
        member this.SetHitPoints (value : int) world = this.Set Property? HitPoints value world
        member this.HitPoints = PropertyTag.make this Property? HitPoints this.GetHitPoints this.SetHitPoints
        member this.GetSpecialPoints world : int = this.Get Property? SpecialPoints world
        member this.SetSpecialPoints (value : int) world = this.Set Property? SpecialPoints value world
        member this.SpecialPoints = PropertyTag.make this Property? SpecialPoints this.GetSpecialPoints this.SetSpecialPoints
        member this.GetPowerBuff world : single = this.Get Property? PowerBuff world
        member this.SetPowerBuff (value : single) world = this.Set Property? PowerBuff value world
        member this.PowerBuff = PropertyTag.make this Property? PowerBuff this.GetPowerBuff this.SetPowerBuff
        member this.GetShieldBuff world : single = this.Get Property? ShieldBuff world
        member this.SetShieldBuff (value : single) world = this.Set Property? ShieldBuff value world
        member this.ShieldBuff = PropertyTag.make this Property? ShieldBuff this.GetShieldBuff this.SetShieldBuff
        member this.GetMindBuff world : single = this.Get Property? MindBuff world
        member this.SetMindBuff (value : single) world = this.Set Property? MindBuff value world
        member this.MindBuff = PropertyTag.make this Property? MindBuff this.GetMindBuff this.SetMindBuff
        member this.GetCounterBuff world : single = this.Get Property? CounterBuff world
        member this.SetCounterBuff (value : single) world = this.Set Property? CounterBuff value world
        member this.CounterBuff = PropertyTag.make this Property? CounterBuff this.GetCounterBuff this.SetCounterBuff
        member this.GetStatuses world : StatusType Set = this.Get Property? Statuses world
        member this.SetStatuses (value : StatusType Set) world = this.Set Property? Statuses value world
        member this.Statuses = PropertyTag.make this Property? Statuses this.GetStatuses this.SetStatuses
        member this.GetEquippedWeapon world : WeaponType option = this.Get Property? EquippedWeapon world
        member this.SetEquippedWeapon (value : WeaponType option) world = this.Set Property? EquippedWeapon value world
        member this.EquippedWeapon = PropertyTag.make this Property? EquippedWeapon this.GetEquippedWeapon this.SetEquippedWeapon
        member this.GetEquippedArmor world : ArmorType option = this.Get Property? EquippedArmor world
        member this.SetEquippedArmor (value : ArmorType option) world = this.Set Property? EquippedArmor value world
        member this.EquippedArmor = PropertyTag.make this Property? EquippedArmor this.GetEquippedArmor this.SetEquippedArmor
        member this.GetEquippedRelics world : RelicType list = this.Get Property? EquippedRelics world
        member this.SetEquippedRelics (value : RelicType list) world = this.Set Property? EquippedRelics value world
        member this.EquippedRelics = PropertyTag.make this Property? EquippedRelics this.GetEquippedRelics this.SetEquippedRelics
        member this.GetControlType world : ControlType = this.Get Property? ControlType world
        member this.SetControlType (value : ControlType) world = this.Set Property? ControlType value world
        member this.ControlType = PropertyTag.make this Property? ControlType this.GetControlType this.SetControlType

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
    
        member this.GetCharacterAnimationState world : CharacterAnimationState = this.Get Property? CharacterAnimationState world
        member this.SetCharacterAnimationState (value : CharacterAnimationState) world = this.Set Property? CharacterAnimationState value world
        member this.CharacterAnimationState = PropertyTag.make this Property? CharacterAnimationState this.GetCharacterAnimationState this.SetCharacterAnimationState
        member this.GetCharacterAnimationSheet world : AssetTag = this.Get Property? CharacterAnimationSheet world
        member this.SetCharacterAnimationSheet (value : AssetTag) world = this.Set Property? CharacterAnimationSheet value world
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
            [Define?
                CharacterAnimationState
                    { AnimationType = CharacterAnimationFacing
                      Direction = Upward
                      StartTime = 0L }
             Define? CharacterAnimationSheet Assets.PlayerImage]

        override facet.Actualize (entity, world) =
            if entity.InView world then
                World.addRenderMessage
                    (RenderDescriptorsMessage
                        [LayerableDescriptor
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
                                      Color = Vector4.One }}])
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
                if World.entityExists Simulants.Field world then
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
            monitor handlePostUpdate entity (stream (Events.PostUpdate ->- entity)) world