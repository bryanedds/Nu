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
module SimpleBodyFacetModule =

    type Entity with

        [<XField>] member this.PhysicsId with get () = this?PhysicsId () : PhysicsId
        member this.SetPhysicsId (value : PhysicsId) : Entity = this?PhysicsId <- value
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

module SimpleBodyFacet =

    let private makeCreateBodyMessage makeBodyShape (entity : Entity) (address : Address) =
        CreateBodyMessage 
            { EntityAddress = address
              PhysicsId = entity.PhysicsId
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

    let init (entity : Entity) =
        entity
            .SetPhysicsId(Physics.getId entity.Id)
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

    let registerPhysics makeBodyShape (entity : Entity) address world =
        let createBodyMessage = makeCreateBodyMessage makeBodyShape entity address
        { world with PhysicsMessages = createBodyMessage :: world.PhysicsMessages }

    let unregisterPhysics (entity : Entity) world =
        let destroyBodyMessage = DestroyBodyMessage { PhysicsId = entity.PhysicsId }
        { world with PhysicsMessages = destroyBodyMessage :: world.PhysicsMessages }

    let propagatePhysics makeBodyShape entity address world =
        let world = unregisterPhysics entity world
        registerPhysics makeBodyShape entity address world

    let handleBodyTransformMessage (entity : Entity) (message : BodyTransformMessage) world =
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

module SimpleSpriteFacet =

    let init (entity : Entity) =
        entity.SetImageSprite { SpriteAssetName = "Image3"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

    let getRenderDescriptors (entity : Entity) (viewAbsolute : Matrix3) (viewRelative : Matrix3) =
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

    let getQuickSize (entity : Entity) (world : World) =
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

module SimpleAnimatedSpriteFacet =

    let private getImageOptInset (entity : Entity) world =
        let tile = (int world.Ticks / entity.Stutter) % entity.TileCount
        let tileI = tile % entity.TileRun
        let tileJ = tile / entity.TileRun
        let tileX = single tileI * entity.TileSize.X
        let tileY = single tileJ * entity.TileSize.Y
        let inset = Vector4 (tileX, tileY, tileX + entity.TileSize.X, tileY + entity.TileSize.Y)
        Some inset

    let init (entity : Entity) =
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