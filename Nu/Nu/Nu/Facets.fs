namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open TiledSharp
open Nu
open Nu.NuConstants
module World = WorldPrims

[<AutoOpen>]
module Entity2dFacetModule =

    type Entity with

        [<XField>] member this.Position with get () = this?Position () : Vector2
        member this.SetPosition (value : Vector2) : Entity = this?Position <- value
        [<XField>] member this.Depth with get () = this?Depth () : single
        member this.SetDepth (value : single) : Entity = this?Depth <- value
        [<XField>] member this.Rotation with get () = this?Rotation () : single
        member this.SetRotation (value : single) : Entity = this?Rotation <- value
        [<XField>] member this.Size with get () = this?Size () : Vector2
        member this.SetSize (value : Vector2) : Entity = this?Size <- value

        member this.PropagatePhysics (address : Address, world : World) : World = this?PropagatePhysics (address, world)
        member this.HandleBodyTransformMessage (address : Address, message : BodyTransformMessage, world : World) : World = this?HandleBodyTransformMessage (address, message, world)
        member this.GetRenderDescriptors (viewAbsolute : Matrix3, viewRelative : Matrix3, world : World) : RenderDescriptor list = this?GetRenderDescriptors (this, viewAbsolute, viewRelative, world)
        member this.GetQuickSize (world : World) : Vector2 = this?GetQuickSize (this, world)
        member this.IsTransformRelative (world : World) : bool = this?IsTransformRelative (this, world)

        static member private sortFstDesc (priority, _) (priority2, _) =
            if priority = priority2 then 0
            elif priority > priority2 then -1
            else 1

        static member mouseToEntity position world (entity : Entity) =
            let positionScreen = Camera.mouseToScreen position world.Camera
            let view = (if entity.IsTransformRelative world then Camera.getViewRelativeF else Camera.getViewAbsoluteF) world.Camera
            let positionEntity = positionScreen * view
            positionEntity

        static member setPositionSnapped snap position (entity : Entity) =
            let snapped = NuMath.snap2F snap position
            entity.SetPosition snapped

        static member getTransform (entity : Entity) =
            { Transform.Position = entity.Position
              Depth = entity.Depth
              Size = entity.Size
              Rotation = entity.Rotation }

        static member setTransform positionSnap rotationSnap transform (entity : Entity) =
            let transform = NuMath.snapTransform positionSnap rotationSnap transform
            entity
                .SetPosition(transform.Position)
                .SetDepth(transform.Depth)
                .SetSize(transform.Size)
                .SetRotation(transform.Rotation)

        static member pickingSort entities world =
            let priorities = List.map (fun (entity : Entity) -> entity.GetPickingPriority world) entities
            let prioritiesAndEntities = List.zip priorities entities
            let prioritiesAndEntitiesSorted = List.sortWith Entity.sortFstDesc prioritiesAndEntities
            List.map snd prioritiesAndEntitiesSorted

        static member tryPick position entities world =
            let entitiesSorted = Entity.pickingSort entities world
            List.tryFind
                (fun entity ->
                    let positionEntity = Entity.mouseToEntity position world entity
                    let transform = Entity.getTransform entity
                    let picked = NuMath.isInBox3 positionEntity transform.Position transform.Size
                    picked)
                entitiesSorted

[<RequireQualifiedAccess>]
module Entity2dFacet =

    let init (entity2d : Entity) (_ : IXDispatcherContainer) =
        entity2d
            .SetPosition(Vector2.Zero)
            .SetDepth(0.0f)
            .SetSize(DefaultEntitySize)
            .SetRotation(0.0f)

    let getPickingPriority (entity2d : Entity) =
        entity2d.Depth

[<AutoOpen>]
module GuiFacetModule =

    type Entity with

        [<XField>] member this.Enabled with get () = this?Enabled () : bool
        member this.SetEnabled (value : bool) : Entity = this?Enabled <- value

[<RequireQualifiedAccess>]
module GuiFacet =

    let init (entity : Entity) (_ : IXDispatcherContainer) =
        entity.SetEnabled true

[<AutoOpen>]
module SimpleBodyFacetModule =

    type Entity with

        [<XField>] member this.MinorId with get () = this?MinorId () : Guid
        member this.SetMinorId (value : Guid) : Entity = this?MinorId <- value
        [<XField>] member this.BodyType with get () = this?BodyType () : BodyType
        member this.SetBodyType (value : BodyType) : Entity = this?BodyType <- value
        [<XField>] member this.Density with get () = this?Density () : single
        member this.SetDensity (value : single) : Entity = this?Density <- value
        [<XField>] member this.Friction with get () = this?Friction () : single
        member this.SetFriction (value : single) : Entity = this?Friction <- value
        [<XField>] member this.Restitution with get () = this?Restitution () : single
        member this.SetRestitution (value : single) : Entity = this?Restitution <- value
        [<XField>] member this.FixedRotation with get () = this?FixedRotation () : bool
        member this.SetFixedRotation (value : bool) : Entity = this?FixedRotation <- value
        [<XField>] member this.LinearDamping with get () = this?LinearDamping () : single
        member this.SetLinearDamping (value : single) : Entity = this?LinearDamping <- value
        [<XField>] member this.AngularDamping with get () = this?AngularDamping () : single
        member this.SetAngularDamping (value : single) : Entity = this?AngularDamping <- value
        [<XField>] member this.GravityScale with get () = this?GravityScale () : single
        member this.SetGravityScale (value : single) : Entity = this?GravityScale <- value
        [<XField>] member this.CollisionCategories with get () = this?CollisionCategories () : string
        member this.SetCollisionCategories (value : string) : Entity = this?CollisionCategories <- value
        [<XField>] member this.CollisionMask with get () = this?CollisionMask () : string
        member this.SetCollisionMask (value : string) : Entity = this?CollisionMask <- value
        [<XField>] member this.IsBullet with get () = this?IsBullet () : bool
        member this.SetIsBullet (value : bool) : Entity = this?IsBullet <- value
        [<XField>] member this.IsSensor with get () = this?IsSensor () : bool
        member this.SetIsSensor (value : bool) : Entity = this?IsSensor <- value
        
        static member getPhysicsId (this : Entity) = PhysicsId (this.Id, this.MinorId)

[<RequireQualifiedAccess>]
module SimpleBodyFacet =

    let private makeCreateBodyMessage makeBodyShape (entity : Entity) (address : Address) =
        CreateBodyMessage
            { EntityAddress = address
              PhysicsId = Entity.getPhysicsId entity
              Position = entity.Position + entity.Size * 0.5f
              Rotation = entity.Rotation
              BodyProperties =
                { Shape = makeBodyShape entity
                  BodyType = entity.BodyType
                  Density = entity.Density
                  Friction = entity.Friction
                  Restitution = entity.Restitution
                  FixedRotation = entity.FixedRotation
                  LinearDamping = entity.LinearDamping
                  AngularDamping = entity.AngularDamping
                  GravityScale = entity.GravityScale
                  CollisionCategories = Physics.toCollisionCategories entity.CollisionCategories
                  CollisionMask = Physics.toCollisionCategories entity.CollisionMask
                  IsBullet = entity.IsBullet
                  IsSensor = entity.IsSensor }}

    let init (entity : Entity) (_ : IXDispatcherContainer) =
        entity
            .SetMinorId(NuCore.getId ())
            .SetBodyType(BodyType.Dynamic)
            .SetDensity(NormalDensity)
            .SetFriction(0.0f)
            .SetRestitution(0.0f)
            .SetFixedRotation(false)
            .SetLinearDamping(1.0f)
            .SetAngularDamping(1.0f)
            .SetGravityScale(1.0f)
            .SetCollisionCategories("1")
            .SetCollisionMask("*")
            .SetIsBullet(false)
            .SetIsSensor(false)

    let registerPhysics makeBodyShape address world =
        let entity = World.getEntity address world
        let createBodyMessage = makeCreateBodyMessage makeBodyShape entity address
        { world with PhysicsMessages = createBodyMessage :: world.PhysicsMessages }

    let unregisterPhysics address world =
        let entity = World.getEntity address world
        let destroyBodyMessage = DestroyBodyMessage { PhysicsId = Entity.getPhysicsId entity }
        { world with PhysicsMessages = destroyBodyMessage :: world.PhysicsMessages }

    let propagatePhysics makeBodyShape address world =
        let world = unregisterPhysics address world
        registerPhysics makeBodyShape address world

    let handleBodyTransformMessage address (message : BodyTransformMessage) world =
        let entity = World.getEntity address world
        let entity =
            entity
                .SetPosition(message.Position - entity.Size * 0.5f) // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                .SetRotation(message.Rotation)
        World.setEntity message.EntityAddress entity world

[<AutoOpen>]
module SimpleSpriteFacetModule =

    type Entity with

        [<XField>] member this.ImageSprite with get () = this?ImageSprite () : Sprite
        member this.SetImageSprite (value : Sprite) : Entity = this?ImageSprite <- value

[<RequireQualifiedAccess>]
module SimpleSpriteFacet =

    let init (entity : Entity) (_ : IXDispatcherContainer) =
        entity.SetImageSprite { SpriteAssetName = "Image3"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

    let getRenderDescriptors entity (viewAbsolute : Matrix3) (viewRelative : Matrix3) =
        if not entity.Visible then []
        else
            [LayerableDescriptor <|
                LayeredSpriteDescriptor
                    { Descriptor =
                        { Position = entity.Position * viewRelative
                          Size = entity.Size * Matrix3.getScaleMatrix viewAbsolute
                          Rotation = entity.Rotation
                          OptInset = None
                          Sprite = entity.ImageSprite
                          Color = Vector4.One }
                      Depth = entity.Depth }]

    let getQuickSize (entity : Entity) world =
        let sprite = entity.ImageSprite
        match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
        | None -> DefaultEntitySize
        | Some size -> size

[<AutoOpen>]
module SimpleAnimatedSpriteFacetModule =

    type Entity with

        [<XField>] member this.Stutter with get () = this?Stutter () : int
        member this.SetStutter (value : int) : Entity = this?Stutter <- value
        [<XField>] member this.TileCount with get () = this?TileCount () : int
        member this.SetTileCount (value : int) : Entity = this?TileCount <- value
        [<XField>] member this.TileRun with get () = this?TileRun () : int
        member this.SetTileRun (value : int) : Entity = this?TileRun <- value
        [<XField>] member this.TileSize with get () = this?TileSize () : Vector2
        member this.SetTileSize (value : Vector2) : Entity = this?TileSize <- value
        [<XField>] member this.ImageSprite with get () = this?ImageSprite () : Sprite
        member this.SetImageSprite (value : Sprite) : Entity = this?ImageSprite <- value

[<RequireQualifiedAccess>]
module SimpleAnimatedSpriteFacet =

    let private getImageOptInset (entity : Entity) world =
        let tile = (int world.Ticks / entity.Stutter) % entity.TileCount
        let tileI = tile % entity.TileRun
        let tileJ = tile / entity.TileRun
        let tileX = single tileI * entity.TileSize.X
        let tileY = single tileJ * entity.TileSize.Y
        let inset = Vector4 (tileX, tileY, tileX + entity.TileSize.X, tileY + entity.TileSize.Y)
        Some inset

    let init (entity : Entity) (_ : IXDispatcherContainer) =
        entity
            .SetStutter(4)
            .SetTileCount(16)
            .SetTileRun(4)
            .SetTileSize(Vector2 (16.0f, 16.0f))
            .SetImageSprite { SpriteAssetName = "Image7"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

    let getRenderDescriptors (entity : Entity) (viewAbsolute : Matrix3) (viewRelative : Matrix3) (world : World) =
        if not entity.Visible then []
        else
            [LayerableDescriptor <|
                LayeredSpriteDescriptor
                    { Descriptor =
                        { Position = entity.Position * viewRelative
                          Size = entity.Size * Matrix3.getScaleMatrix viewAbsolute
                          Rotation = entity.Rotation
                          OptInset = getImageOptInset entity world
                          Sprite = entity.ImageSprite
                          Color = Vector4.One }
                      Depth = entity.Depth }]

    let getQuickSize (entity : Entity) =
        entity.TileSize