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
        
        member this.GetIdleLeftCycle = this.Get Property? IdleLeftCycle
        member this.SetIdleLeftCycle = this.Set Property? IdleLeftCycle
        member this.IdleLeftCycle = Lens.make<Image AssetTag, World> Property? IdleLeftCycle this.GetIdleLeftCycle this.SetIdleLeftCycle this
        member this.GetIdleRightCycle = this.Get Property? IdleRightCycle
        member this.SetIdleRightCycle = this.Set Property? IdleRightCycle
        member this.IdleRightCycle = Lens.make<Image AssetTag, World> Property? IdleRightCycle this.GetIdleRightCycle this.SetIdleRightCycle this
        member this.GetJumpLeftCycle = this.Get Property? JumpLeftCycle
        member this.SetJumpLeftCycle = this.Set Property? JumpLeftCycle
        member this.JumpLeftCycle = Lens.make<Image AssetTag, World> Property? JumpLeftCycle this.GetJumpLeftCycle this.SetJumpLeftCycle this
        member this.GetJumpRightCycle = this.Get Property? JumpRightCycle
        member this.SetJumpRightCycle = this.Set Property? JumpRightCycle
        member this.JumpRightCycle = Lens.make<Image AssetTag, World> Property? JumpRightCycle this.GetJumpRightCycle this.SetJumpRightCycle this
        member this.GetWalkLeftCycle = this.Get Property? WalkLeftCycle
        member this.SetWalkLeftCycle = this.Set Property? WalkLeftCycle
        member this.WalkLeftCycle = Lens.make<Image AssetTag, World> Property? WalkLeftCycle this.GetWalkLeftCycle this.SetWalkLeftCycle this
        member this.GetWalkRightCycle = this.Get Property? WalkRightCycle
        member this.SetWalkRightCycle = this.Set Property? WalkRightCycle
        member this.WalkRightCycle = Lens.make<Image AssetTag, World> Property? WalkRightCycle this.GetWalkRightCycle this.SetWalkRightCycle this
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
             define Entity.CollisionBody (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v2Zero })
             define Entity.IdleLeftCycle (AssetTag.make Assets.DefaultPackage "IdleLeftCycle")
             define Entity.IdleRightCycle (AssetTag.make Assets.DefaultPackage "IdleRightCycle")
             define Entity.JumpLeftCycle (AssetTag.make Assets.DefaultPackage "JumpLeftCycle")
             define Entity.JumpRightCycle (AssetTag.make Assets.DefaultPackage "JumpRightCycle")
             define Entity.WalkLeftCycle (AssetTag.make Assets.DefaultPackage "WalkLeftCycle")
             define Entity.WalkRightCycle (AssetTag.make Assets.DefaultPackage "WalkRightCycle")
             define Entity.FacingLeft false]

        override this.Update (entity, world) =
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
                    let image = if facingLeft then entity.GetJumpLeftCycle world else entity.GetJumpRightCycle world
                    (None, image)
                elif velocity.X < 5.0f && velocity.X > -5.0f then
                    let image = if facingLeft then entity.GetIdleLeftCycle world else entity.GetIdleRightCycle world
                    (None, image)
                elif velocity.X < 0.0f then
                    let image = entity.GetWalkLeftCycle world
                    (Some (computeWalkCelInset celSize celRun animationDelay time), image)
                else
                    let image = entity.GetWalkRightCycle world
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