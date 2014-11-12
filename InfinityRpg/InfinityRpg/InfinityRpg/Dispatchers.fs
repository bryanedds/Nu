namespace InfinityRpg
open System
open System.ComponentModel
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module PlayerCharacterDispatcherModule =

    type PlayerCharacterDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? GravityScale 0.0f
             define? LinearDamping 0.0f
             define? FixedRotation true]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             "CharacterAnimationFacet"
             "CharacterControlFacet"]

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
            let imageSize =
                Metadata.getTextureSizeAsVector2
                    entity.CharacterAnimationSheet.ImageAssetName
                    entity.CharacterAnimationSheet.PackageName
                    world.State.AssetMetadataMap
            let spriteSize = imageSize * 0.25f
            let animationState = entity.CharacterAnimationState
            let animationTypeCoordsOffset =
                match animationState.CharacterAnimationType with
                | CharacterAnimationFacing -> Vector2I (0, 0)
                | CharacterAnimationActing -> Vector2I (0, 2)
            let directionCoordsOffset =
                match animationState.CharacterAnimationDirection with
                | North -> Vector2I (0, 0)
                | East -> Vector2I (2, 0)
                | South -> Vector2I (0, 1)
                | West -> Vector2I (2, 1)
            let frameXOffset =
                (int world.State.TickTime - int animationState.ChangeTime) /
                animationState.CharacterAnimationStutter % 2
            let frameCoordsOffset = Vector2I (frameXOffset, 0)
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
                  CharacterAnimationDirection = North
                  CharacterAnimationStutter = 16
                  ChangeTime = 0L }
             define? CharacterAnimationSheet PlayerImage]

        override this.GetRenderDescriptors (entity, world) =
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
module CharacterControlFacetModule =

    type CharacterControlFacet () =
        inherit Facet ()

        let [<Literal>] WalkForce = 200.0f

        let handleKeyboardKeyChange event world =
            let (address, character, keyData) = Event.unwrapASD<Entity, KeyboardKeyData> event
            if not keyData.IsRepeat then
                let xForce =
                    if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) world then WalkForce
                    elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) world then -WalkForce
                    else 0.0f
                let yForce =
                    if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_UP) world then WalkForce
                    elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_DOWN) world then -WalkForce
                    else 0.0f
                let force = Vector2 (xForce, yForce)
                let world = World.setBodyLinearVelocity force character.PhysicsId world
                let facingDirection =
                    if force.X > 0.0f then East
                    elif force.X < 0.0f then West
                    elif force.Y > 0.0f then North
                    elif force.Y < 0.0f then South
                    else character.CharacterAnimationState.CharacterAnimationDirection
                let characterState = { character.CharacterAnimationState with CharacterAnimationDirection = facingDirection }
                let character = Entity.setCharacterAnimationState characterState character
                let world = World.setEntity address character world
                (Cascade, world)
            else (Cascade, world)

        override this.Register (address, entity, world) =
            // TODO: make this a specifiable check in-engine.
            if Reflection.dispatchesAs typeof<PlayerCharacterDispatcher> entity.DispatcherNp then
                let world = React.from ChangeKeyboardKeyEventAddress address |> React.subscribe handleKeyboardKeyChange world |> snd
                (entity, world)
            else failwith "PlayerCharacterDispatcher required"

[<AutoOpen>]
module InfinityRpgModule =

    type InfinityRpgDispatcher () =
        inherit GameDispatcher ()