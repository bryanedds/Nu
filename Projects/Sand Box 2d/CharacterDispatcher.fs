// https://github.com/bryanedds/Nu/pull/1305#issuecomment-3903203460
// "Copy-Pasting and modifying the entire dispatcher is the appropriate implementation pattern for things like character dispatchers.
// It might not seem to make sense from first principles, but you're just going to have to trust my domain knowledge on this one."

namespace SandBox2d
open System.Numerics
open Nu

[<AutoOpen>]
module Character2dDispatcherExtensions =
    type Entity with
        member this.GetCharacter2dRightDirection world : Vector3 = this.Get (nameof this.Character2dRightDirection) world
        member this.SetCharacter2dRightDirection (value : Vector3) world = this.Set (nameof this.Character2dRightDirection) value world
        member this.Character2dRightDirection = lens (nameof this.Character2dRightDirection) this this.GetCharacter2dRightDirection this.SetCharacter2dRightDirection

/// Gives an entity the base behavior of 2d physics-driven character in a platformer.
type Character2dDispatcher () =
    inherit Nu.Character2dDispatcher ()

    static let computeWalkCelInset time delay (celSize : Vector2) (celRun : int) =
        let compressedTime =
            match (time, delay) with
            | (UpdateTime time, UpdateTime delay) -> time / delay
            | (TickTime time, TickTime delay) -> time / delay
            | (_, _) -> failwith "Cannot operate on incompatible GameTime values."
        let frame = compressedTime % int64 celRun
        let i = single (frame % 3L)
        let j = single (frame / 3L)
        let offset = v2 (i * celSize.X) (j * celSize.Y) 
        box2 offset celSize

    static member Properties =
        [define Entity.Character2dRightDirection v3Right]

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            // we have to use a bit of hackery to remember whether the character is facing left or
            // right when there is no velocity
            let facingLeft = entity.GetCharacter2dFacingLeft world
            let velocity = World.getBodyLinearVelocity (entity.GetBodyId world) world
            let right = entity.GetCharacter2dRightDirection world
            let rightVelocity = velocity.Dot right // NOTE: the default implementation assumes right is always v3Right.
            if facingLeft && rightVelocity > 1.0f then entity.SetCharacter2dFacingLeft false world
            elif not facingLeft && rightVelocity < -1.0f then entity.SetCharacter2dFacingLeft true world

    override this.Render (_, entity, world) =
        let bodyId = entity.GetBodyId world
        let facingLeft = entity.GetCharacter2dFacingLeft world
        let velocity = entity.GetLinearVelocity world
        let celSize = entity.GetCelSize world
        let celRun = entity.GetCelRun world
        let animationDelay = entity.GetAnimationDelay world
        let mutable transform = entity.GetTransform world
        let struct (insetOpt, image) =
            let right = entity.GetCharacter2dRightDirection world
            let rightVelocity = velocity.Dot right // NOTE: the default implementation assumes right is always v3Right.
            if not (World.getBodyGrounded bodyId world) then
                let image = entity.GetCharacter2dJumpImage world
                struct (ValueNone, image)
            elif rightVelocity < 5.0f && rightVelocity > -5.0f then
                let image = entity.GetCharacter2dIdleImage world
                struct (ValueNone, image)
            else
                let image = entity.GetCharacter2dWalkSheet world
                struct (ValueSome (computeWalkCelInset world.GameTime animationDelay celSize celRun), image)
        World.enqueueLayeredOperation2d
            { Elevation = transform.Elevation
              Horizon = transform.Horizon
              AssetTag = image
              RenderOperation2d =
                RenderSprite
                    { Transform = transform
                      InsetOpt = insetOpt
                      ClipOpt = ValueNone
                      Image = image
                      Color = Color.One
                      Blend = Transparent
                      Emission = Color.Zero
                      Flip = if facingLeft then FlipH else FlipNone }}
            world