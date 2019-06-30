namespace Elmario
open Prime
open OpenTK
open Nu
open Nu.Declarative

[<AutoOpen>]
module ElmarioController =

    type Model =
        bool

    type Entity with
        
        member this.GetIdleLeftImage = this.Get Property? IdleLeftImage
        member this.SetIdleLeftImage = this.Set Property? IdleLeftImage
        member this.IdleLeftImage = Lens.make<Image AssetTag, World> Property? IdleLeftImage this.GetIdleLeftImage this.SetIdleLeftImage this
        member this.GetIdleRightImage = this.Get Property? IdleRightImage
        member this.SetIdleRightImage = this.Set Property? IdleRightImage
        member this.IdleRightImage = Lens.make<Image AssetTag, World> Property? IdleRightImage this.GetIdleRightImage this.SetIdleRightImage this
        member this.GetJumpLeftImage = this.Get Property? JumpLeftImage
        member this.SetJumpLeftImage = this.Set Property? JumpLeftImage
        member this.JumpLeftImage = Lens.make<Image AssetTag, World> Property? JumpLeftImage this.GetJumpLeftImage this.SetJumpLeftImage this
        member this.GetJumpRightImage = this.Get Property? JumpRightImage
        member this.SetJumpRightImage = this.Set Property? JumpRightImage
        member this.JumpRightImage = Lens.make<Image AssetTag, World> Property? JumpRightImage this.GetJumpRightImage this.SetJumpRightImage this
        member this.GetWalkLeftSheet = this.Get Property? WalkLeftSheet
        member this.SetWalkLeftSheet = this.Set Property? WalkLeftSheet
        member this.WalkLeftSheet = Lens.make<Image AssetTag, World> Property? WalkLeftSheet this.GetWalkLeftSheet this.SetWalkLeftSheet this
        member this.GetWalkRightSheet = this.Get Property? WalkRightSheet
        member this.SetWalkRightSheet = this.Set Property? WalkRightSheet
        member this.WalkRightSheet = Lens.make<Image AssetTag, World> Property? WalkRightSheet this.GetWalkRightSheet this.SetWalkRightSheet this
        member this.GetFacingLeft = this.Get Property? FacingLeft
        member this.SetFacingLeft = this.Set Property? FacingLeft
        member this.FacingLeft = Lens.make<bool, World> Property? FacingLeft this.GetFacingLeft this.SetFacingLeft this

    type ElmarioController () =
        inherit EntityDispatcher ()

        static let computeWalkCelInset (celSize : Vector2) (celRun : int) delay time =
            let timeCompressed = time / delay
            let frame = timeCompressed % int64 celRun
            let i = single (frame % 3L)
            let j = single (frame / 3L)
            let offset = v2 (i * celSize.X) (j * celSize.Y) 
            v4 offset.X offset.Y (offset.X + celSize.X) (offset.Y + celSize.Y)

        static member FacetNames =
            [typeof<RigidBodyFacet>.Name]

        static member Properties =
            [define Entity.AnimationDelay 6L
             define Entity.CelSize (v2 28.0f 28.0f)
             define Entity.CelRun 8
             define Entity.FixedRotation true
             define Entity.GravityScale 1.0f
             define Entity.CollisionBody (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v2Zero })
             define Entity.IdleLeftImage (AssetTag.make "Gameplay" "IdleLeft")
             define Entity.IdleRightImage (AssetTag.make "Gameplay" "IdleRight")
             define Entity.JumpLeftImage (AssetTag.make "Gameplay" "JumpLeft")
             define Entity.JumpRightImage (AssetTag.make "Gameplay" "JumpRight")
             define Entity.WalkLeftSheet (AssetTag.make "Gameplay" "WalkLeft")
             define Entity.WalkRightSheet (AssetTag.make "Gameplay" "WalkRight")
             define Entity.FacingLeft false]

        override this.Update (entity, world) =

            //// default gravity gives too floaty of a feel, so we use a constant fall force
            //let physicsId = entity.GetPhysicsId world
            //let gravity =
            //    if World.isBodyOnGround physicsId world
            //    then Vector2 (0.0f, -1000.0f)
            //    else Vector2 (0.0f, -30000.0f)
            //let world = World.applyBodyForce gravity physicsId world

            // we have to a bit of hackery to remember whether the character is facing left or right
            // when there is no velocity
            let facingLeft = entity.GetFacingLeft world
            let velocity = World.getBodyLinearVelocity (entity.GetPhysicsId world) world
            if facingLeft && velocity.X > 1.0f then entity.SetFacingLeft false world
            elif not facingLeft && velocity.X < -1.0f then entity.SetFacingLeft true world
            else world

        override this.Actualize (entity, world) =
            let time = World.getTickTime world
            let physicsId = entity.GetPhysicsId world
            let facingLeft = entity.GetFacingLeft world
            let velocity = World.getBodyLinearVelocity physicsId world
            let celSize = entity.GetCelSize world
            let celRun = entity.GetCelRun world
            let animationDelay = entity.GetAnimationDelay world
            let (insetOpt, image) =
                if not (World.isBodyOnGround physicsId world) then
                    let image = if facingLeft then entity.GetJumpLeftImage world else entity.GetJumpRightImage world
                    (None, image)
                elif velocity.X < 5.0f && velocity.X > -5.0f then
                    let image = if facingLeft then entity.GetIdleLeftImage world else entity.GetIdleRightImage world
                    (None, image)
                elif velocity.X < 0.0f then
                    let image = entity.GetWalkLeftSheet world
                    (Some (computeWalkCelInset celSize celRun animationDelay time), image)
                else
                    let image = entity.GetWalkRightSheet world
                    (Some (computeWalkCelInset celSize celRun animationDelay time), image)
            let renderMessage =
                RenderDescriptorsMessage
                    [|LayerableDescriptor
                        { Depth = entity.GetDepthLayered world
                          PositionY = (entity.GetPosition world).Y
                          LayeredDescriptor =
                            SpriteDescriptor
                                { Position = entity.GetPosition world
                                  Size = entity.GetSize world
                                  Rotation = entity.GetRotation world
                                  Offset = v2Zero
                                  ViewType = entity.GetViewType world
                                  InsetOpt = insetOpt
                                  Image = image
                                  Color = v4One }}|]
            World.enqueueRenderMessage renderMessage world