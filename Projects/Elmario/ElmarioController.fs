namespace Elmario
open Prime
open OpenTK
open Nu
open Nu.Declarative

[<AutoOpen>]
module ElmarioController =

    let IdleRightCycle = AssetTag.make "Gameplay" "IdleRightCycle"
    let JumpRightCycle = AssetTag.make "Gameplay" "JumpRightCycle"
    let WalkRightCycle = AssetTag.make "Gameplay" "WalkRightCycle"

    type Model =
        bool

    type ElmarioController () =
        inherit EntityDispatcher<Model, unit, unit> (true)

        static let computeWalkCelInset (celSize : Vector2) time =
            let timeCompressed = time / 5L
            let frame = timeCompressed % 8L
            let i = single (frame % 3L)
            let j = single (frame / 3L)
            let offset = v2 (i * celSize.X) (j * celSize.Y) 
            v4 offset.X offset.Y (offset.X + celSize.X) (offset.Y + celSize.Y)

        static member FacetNames =
            [typeof<RigidBodyFacet>.Name]

        static member Properties =
            [define Entity.FixedRotation true
             define Entity.Friction 0.5f
             define Entity.LinearDamping 0.0f
             define Entity.CollisionBody (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v2Zero })]

        override this.View (_, entity, world) =
            let time = World.getTickTime world
            let physicsId = entity.GetPhysicsId world
            let velocity = World.getBodyLinearVelocity physicsId world
            let (insetOpt, image) =
                if not (World.isBodyOnGround physicsId world) then (None, JumpRightCycle)
                elif velocity.X < 5.0f && velocity.X > -5.0f then (None, IdleRightCycle)
                else
                    let celSize = World.getTextureSizeF WalkRightCycle world / 3.0f
                    (Some (computeWalkCelInset celSize time), WalkRightCycle)
            [View.Render
                (LayerableDescriptor
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
                              Color = v4One }})]