namespace InfinityRpg
open System
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observation
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module CharacterStateFacetModule =

    type Entity with
    
        member entity.CharacterType = entity?CharacterType : CharacterType
        static member getCharacterType (entity : Entity) = entity.CharacterType
        static member setCharacterType (value : CharacterType) (entity : Entity) = entity?CharacterType <- value
        
        member entity.ActivityState = entity?ActivityState : ActivityState
        static member getActivityState (entity : Entity) = entity.ActivityState
        static member setActivityState (value : ActivityState) (entity : Entity) = entity?ActivityState <- value
        
        member entity.HitPoints = entity?HitPoints : int
        static member getHitPoints (entity : Entity) = entity.HitPoints
        static member setHitPoints (value : int) (entity : Entity) = entity?HitPoints <- value
        
        member entity.SpecialPoints = entity?SpecialPoints : int
        static member getSpecialPoints (entity : Entity) = entity.SpecialPoints
        static member setSpecialPoints (value : int) (entity : Entity) = entity?SpecialPoints <- value
        
        member entity.PowerBuff = entity?PowerBuff : single
        static member getPowerBuff (entity : Entity) = entity.PowerBuff
        static member setPowerBuff (value : single) (entity : Entity) = entity?PowerBuff <- value
        
        member entity.ShieldBuff = entity?ShieldBuff : single
        static member getShieldBuff (entity : Entity) = entity.ShieldBuff
        static member setShieldBuff (value : single) (entity : Entity) = entity?ShieldBuff <- value
        
        member entity.MindBuff = entity?MindBuff : single
        static member getMindBuff (entity : Entity) = entity.MindBuff
        static member setMindBuff (value : single) (entity : Entity) = entity?MindBuff <- value
        
        member entity.CounterBuff = entity?CounterBuff : single
        static member getCounterBuff (entity : Entity) = entity.CounterBuff
        static member setCounterBuff (value : single) (entity : Entity) = entity?CounterBuff <- value
        
        member entity.Statuses = entity?Statuses : StatusType Set
        static member getStatuses (entity : Entity) = entity.Statuses
        static member setStatuses (value : StatusType Set) (entity : Entity) = entity?Statuses <- value
        
        member entity.EquippedWeapon = entity?EquippedWeapon : WeaponType option
        static member getEquippedWeapon (entity : Entity) = entity.EquippedWeapon
        static member setEquippedWeapon (value : WeaponType option) (entity : Entity) = entity?EquippedWeapon <- value
        
        member entity.EquippedArmor = entity?EquippedArmor : ArmorType option
        static member getEquippedArmor (entity : Entity) = entity.EquippedArmor
        static member setEquippedArmor (value : ArmorType option) (entity : Entity) = entity?EquippedArmor <- value
        
        member entity.EquippedRelics = entity?EquippedRelics : RelicType list
        static member getEquippedRelics (entity : Entity) = entity.EquippedRelics
        static member setEquippedRelics (value : RelicType list) (entity : Entity) = entity?EquippedRelics <- value
        
        member entity.ControlType = entity?ControlType : ControlType
        static member getControlType (entity : Entity) = entity.ControlType
        static member setControlType (value : ControlType) (entity : Entity) = entity?ControlType <- value

    type CharacterStateFacet () =
        inherit Facet ()

        static member FieldDefinitions =
            [define? CharacterType Player
             define? ActivityState NoActivity
             define? HitPoints 10 // note this is an arbitrary number as hp max is calculated
             define? SpecialPoints 1 // sp max is calculated
             define? PowerBuff 1.0f // rate at which power is buffed / debuffed
             define? ShieldBuff 1.0f // rate at which shield is buffed / debuffed
             define? MindBuff 1.0f // rate at which mind is buffed / debuffed
             define? CounterBuff 1.0f // rate at which counter is buffed / debuffed
             define? Statuses Set.empty<StatusType>
             define? EquippedWeapon Option<WeaponType>.None
             define? EquippedArmor Option<ArmorType>.None
             define? EquippedRelics list<RelicType>.Empty
             define? ControlType Uncontrolled] // level is calculated from base experience + added experience

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
    
        member entity.CharacterAnimationState = entity?CharacterAnimationState : CharacterAnimationState
        static member getCharacterAnimationState (entity : Entity) = entity.CharacterAnimationState
        static member setCharacterAnimationState (value : CharacterAnimationState) (entity : Entity) = entity?CharacterAnimationState <- value
    
        member entity.CharacterAnimationSheet = entity?CharacterAnimationSheet : AssetTag
        static member getCharacterAnimationSheet (entity : Entity) = entity.CharacterAnimationSheet
        static member setCharacterAnimationSheet (value : AssetTag) (entity : Entity) = entity?CharacterAnimationSheet <- value

    type CharacterAnimationFacet () =
        inherit Facet ()
        
        static let getOptSpriteInset (entity : Entity) world =
            let animationState = entity.CharacterAnimationState
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
                | CharacterAnimationFacing -> CharacterAnimationFacingStutter
                | CharacterAnimationActing -> CharacterAnimationActingStutter
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
                Math.Abs (world.State.TickTime - animationState.StartTime) /
                animationStutter % int64 animationFrames |>
                int
            let animatedOffsetM = Vector2i (animatedXOffsetM, 0)
            let spriteCoordsinates = animationOffsetM + directionCoordsOffset + animatedOffsetM
            let spriteOffset =
                Vector2 (
                    TileSize.X * single spriteCoordsinates.X,
                    TileSize.Y * single spriteCoordsinates.Y)
            let spriteInset =
                Vector4 (
                    spriteOffset.X,
                    spriteOffset.Y,
                    spriteOffset.X + TileSize.X,
                    spriteOffset.Y + TileSize.Y)
            Some spriteInset

        static member FieldDefinitions =
            [define?
                CharacterAnimationState
                    { AnimationType = CharacterAnimationFacing
                      Direction = Upward
                      StartTime = 0L }
             define? CharacterAnimationSheet PlayerImage]

        override facet.GetRenderDescriptors (entity, world) =
            if entity.Visible && Camera.inView3 entity.ViewType entity.Position entity.Size world.State.Camera then
                [LayerableDescriptor
                    { Depth = entity.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = entity.Position
                              Size = entity.Size
                              Rotation = entity.Rotation
                              ViewType = entity.ViewType
                              OptInset = getOptSpriteInset entity world
                              Image = entity.CharacterAnimationSheet
                              Color = Vector4.One }}]
            else []

[<AutoOpen>]
module CharacterCameraFacetModule =

    type CharacterCameraFacet () =
        inherit Facet ()

        static let handleTick event world =
            let character = World.getEntity event.SubscriberAddress world
            let address = event.SubscriberAddress
            let world =
                World.updateCamera (fun camera ->
                    let eyeCenter = character.Position + character.Size * 0.5f
                    let eyeCenter =
                        match World.getOptEntity (gatoea (Address.allButLast address) FieldName) world with
                        | Some field ->
                            let eyeSize = camera.EyeSize
                            let eyeCornerNegative = eyeCenter - eyeSize * 0.5f
                            let eyeCornerPositive = eyeCenter + eyeSize * 0.5f
                            let fieldCornerNegative = field.Position
                            let fieldCornerPositive = field.Position + field.Size
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
                        | None -> eyeCenter
                    { camera with EyeCenter = eyeCenter })
                    world
            (Cascade, world)

        override facet.Register (entity, address, world) =
            let world = observe TickEventAddress address |> monitor handleTick <| world
            (entity, world)