namespace InfinityRpg
open System
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module CharacterStateFacetModule =

    type Entity with
    
        member entity.CharacterType = entity?CharacterType : CharacterType
        static member setCharacterType (value : CharacterType) (entity : Entity) = entity?CharacterType <- value
        member entity.ActivityState = entity?ActivityState : ActivityState
        static member setActivityState (value : ActivityState) (entity : Entity) = entity?ActivityState <- value
        member entity.HitPoints = entity?HitPoints : int
        static member setHitPoints (value : int) (entity : Entity) = entity?HitPoints <- value
        member entity.SpecialPoints = entity?SpecialPoints : int
        static member setSpecialPoints (value : int) (entity : Entity) = entity?SpecialPoints <- value
        member entity.PowerBuff = entity?PowerBuff : single
        static member setPowerBuff (value : single) (entity : Entity) = entity?PowerBuff <- value
        member entity.ShieldBuff = entity?ShieldBuff : single
        static member setShieldBuff (value : single) (entity : Entity) = entity?ShieldBuff <- value
        member entity.MindBuff = entity?MindBuff : single
        static member setMindBuff (value : single) (entity : Entity) = entity?MindBuff <- value
        member entity.CounterBuff = entity?CounterBuff : single
        static member setCounterBuff (value : single) (entity : Entity) = entity?CounterBuff <- value
        member entity.Statuses = entity?Statuses : StatusType Set
        static member setStatuses (value : StatusType Set) (entity : Entity) = entity?Statuses <- value
        member entity.EquippedWeapon = entity?EquippedWeapon : WeaponType option
        static member setEquippedWeapon (value : WeaponType option) (entity : Entity) = entity?EquippedWeapon <- value
        member entity.EquippedArmor = entity?EquippedArmor : ArmorType option
        static member setEquippedArmor (value : ArmorType option) (entity : Entity) = entity?EquippedArmor <- value
        member entity.EquippedRelics = entity?EquippedRelics : RelicType list
        static member setEquippedRelics (value : RelicType list) (entity : Entity) = entity?EquippedRelics <- value
        member entity.AddedExperience = entity?AddedExperience : int
        static member setAddedExperience (value : int) (entity : Entity) = entity?AddedExperience <- value
        member entity.ControlType = entity?ControlType : ControlType
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
             define? AddedExperience 0
             define? ControlType Uncontrolled] // level is calculated from base experience + added experience

[<AutoOpen>]
module CharacterAnimationFacetModule =

    type CharacterAnimationType =
        | CharacterAnimationFacing
        | CharacterAnimationActing

    type CharacterAnimationState =
        { CharacterAnimationType : CharacterAnimationType
          CharacterAnimationDirection : Direction
          CharacterAnimationStutter : int
          ChangeTime : int64 }

    type Entity with
    
        member entity.CharacterAnimationState = entity?CharacterAnimationState : CharacterAnimationState
        static member setCharacterAnimationState (value : CharacterAnimationState) (entity : Entity) = entity?CharacterAnimationState <- value
        member entity.CharacterAnimationSheet = entity?CharacterAnimationSheet : Image
        static member setCharacterAnimationSheet (value : Image) (entity : Entity) = entity?CharacterAnimationSheet <- value

    type CharacterAnimationFacet () =
        inherit Facet ()
        
        static let getOptSpriteInset (entity : Entity) world =
            let imageAssetTag = Image.toAssetTag entity.CharacterAnimationSheet
            let imageSize = Metadata.getTextureSizeAsVector2 imageAssetTag world.State.AssetMetadataMap
            let spriteSize = imageSize * 0.25f
            let animationState = entity.CharacterAnimationState
            let animationTypeCoordsOffset =
                match animationState.CharacterAnimationType with
                | CharacterAnimationFacing -> Vector2i (0, 0)
                | CharacterAnimationActing -> Vector2i (0, 2)
            let directionCoordsOffset =
                match animationState.CharacterAnimationDirection with
                | Upward -> Vector2i (0, 0)
                | Rightward -> Vector2i (2, 0)
                | Downward -> Vector2i (0, 1)
                | Leftward -> Vector2i (2, 1)
            let frameXOffset =
                (int world.State.TickTime - int animationState.ChangeTime) /
                animationState.CharacterAnimationStutter % 2
            let frameCoordsOffset = Vector2i (frameXOffset, 0)
            let spriteCoordsinates = animationTypeCoordsOffset + directionCoordsOffset + frameCoordsOffset
            let spriteOffset =
                Vector2 (
                    spriteSize.X * single spriteCoordsinates.X,
                    spriteSize.Y * single spriteCoordsinates.Y)
            let spriteInset =
                Vector4 (
                    spriteOffset.X,
                    spriteOffset.Y,
                    spriteOffset.X + spriteSize.X,
                    spriteOffset.Y + spriteSize.Y)
            Some spriteInset

        static member FieldDefinitions =
            [define?
                CharacterAnimationState
                { CharacterAnimationType = CharacterAnimationFacing
                  CharacterAnimationDirection = Upward
                  CharacterAnimationStutter = 16
                  ChangeTime = 0L }
             define? CharacterAnimationSheet PlayerImage]

        override facet.GetRenderDescriptors (entity, world) =
            if entity.Visible && Camera.inView3 entity.ViewType entity.Position entity.Size world.Camera then
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
            let (address, character : Entity) = World.unwrapAS event world
            let eyeCenter = character.Position + character.Size * 0.5f
            let eyeCenter =
                match World.getOptEntity (gatoea (Address.allButLast address) FieldName) world with
                | Some field ->
                    let eyeSize = world.Camera.EyeSize
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
            let camera = { world.Camera with EyeCenter = eyeCenter }
            (Cascade, World.setCamera camera world)

        override facet.Register (address, entity, world) =
            let world = observe TickEventAddress address |> monitor handleTick world |> snd
            (entity, world)