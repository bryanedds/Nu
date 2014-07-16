namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open TiledSharp
open Nu
open Nu.NuConstants

[<AutoOpen>]
module Entity2dFacetModule =

    type Entity with

        member entity.Position with get () = entity?Position () : Vector2
        static member setPosition (value : Vector2) (entity : Entity) : Entity = entity?Position <- value
        member entity.Depth with get () = entity?Depth () : single
        static member setDepth (value : single) (entity : Entity) : Entity = entity?Depth <- value
        member entity.Rotation with get () = entity?Rotation () : single
        static member setRotation (value : single) (entity : Entity) : Entity = entity?Rotation <- value
        member entity.Size with get () = entity?Size () : Vector2
        static member setSize (value : Vector2) (entity : Entity) : Entity = entity?Size <- value

        static member propagatePhysics (address : Address) (entity : Entity) (world : World) : World = entity?PropagatePhysics (address, world)
        static member handleBodyTransformMessage (address : Address) (message : BodyTransformMessage) (entity : Entity) (world : World) : World = entity?HandleBodyTransformMessage (address, message, world)
        static member getRenderDescriptors (entity : Entity) (world : World) : RenderDescriptor list = entity?GetRenderDescriptors (entity, world)
        static member getQuickSize  (entity : Entity) (world : World) : Vector2 = entity?GetQuickSize (entity, world)
        static member isTransformRelative  (entity : Entity) (world : World) : bool = entity?IsTransformRelative (entity, world)

        static member private sortFstDesc (priority, _) (priority2, _) =
            if priority = priority2 then 0
            elif priority > priority2 then -1
            else 1

        static member mouseToEntity position world (entity : Entity) =
            let positionScreen = Camera.mouseToScreen position world.Camera
            let view = (if Entity.isTransformRelative entity world then Camera.getViewRelativeF else Camera.getViewAbsoluteF) world.Camera
            let positionEntity = positionScreen * view
            positionEntity

        static member setPositionSnapped snap position (entity : Entity) =
            let snapped = NuMath.snap2F snap position
            Entity.setPosition snapped entity

        static member getTransform (entity : Entity) =
            { Transform.Position = entity.Position
              Depth = entity.Depth
              Size = entity.Size
              Rotation = entity.Rotation }

        static member setTransform positionSnap rotationSnap transform (entity : Entity) =
            let transform = NuMath.snapTransform positionSnap rotationSnap transform
            entity |>
                Entity.setPosition transform.Position |>
                Entity.setDepth transform.Depth |>
                Entity.setSize transform.Size |>
                Entity.setRotation transform.Rotation

        static member pickingSort entities world =
            let priorities = List.map (fun (entity : Entity) -> Entity.getPickingPriority entity world) entities
            let prioritiesAndEntities = List.zip priorities entities
            let prioritiesAndEntitiesSorted = List.sortWith Entity.sortFstDesc prioritiesAndEntities
            List.map snd prioritiesAndEntitiesSorted

        static member tryPick position entities world =
            let entitiesSorted = Entity.pickingSort entities world
            List.tryFind
                (fun entity ->
                    let positionEntity = Entity.mouseToEntity position world entity
                    let transform = Entity.getTransform entity
                    let picked = NuMath.isPointInBounds3 positionEntity transform.Position transform.Size
                    picked)
                entitiesSorted

[<RequireQualifiedAccess>]
module Entity2dFacet =

    let init (entity2d : Entity) (_ : IXDispatcherContainer) =
        entity2d |>
            Entity.setPosition Vector2.Zero |>
            Entity.setDepth 0.0f |>
            Entity.setSize DefaultEntitySize |>
            Entity.setRotation 0.0f

    let getPickingPriority (entity2d : Entity) =
        entity2d.Depth

[<AutoOpen>]
module GuiFacetModule =

    type Entity with

        member entity.Enabled with get () = entity?Enabled () : bool
        static member setEnabled (value : bool) (entity : Entity) : Entity = entity?Enabled <- value

[<RequireQualifiedAccess>]
module GuiFacet =

    let init (entity : Entity) (_ : IXDispatcherContainer) =
        Entity.setEnabled true entity

[<AutoOpen>]
module SimpleBodyFacetModule =

    type Entity with

        member entity.MinorId with get () = entity?MinorId () : Guid
        static member setMinorId (value : Guid) (entity : Entity) : Entity = entity?MinorId <- value
        member entity.BodyType with get () = entity?BodyType () : BodyType
        static member setBodyType (value : BodyType) (entity : Entity) : Entity = entity?BodyType <- value
        member entity.Density with get () = entity?Density () : single
        static member setDensity (value : single) (entity : Entity) : Entity = entity?Density <- value
        member entity.Friction with get () = entity?Friction () : single
        static member setFriction (value : single) (entity : Entity) : Entity = entity?Friction <- value
        member entity.Restitution with get () = entity?Restitution () : single
        static member setRestitution (value : single) (entity : Entity) : Entity = entity?Restitution <- value
        member entity.FixedRotation with get () = entity?FixedRotation () : bool
        static member setFixedRotation (value : bool) (entity : Entity) : Entity = entity?FixedRotation <- value
        member entity.LinearDamping with get () = entity?LinearDamping () : single
        static member setLinearDamping (value : single) (entity : Entity) : Entity = entity?LinearDamping <- value
        member entity.AngularDamping with get () = entity?AngularDamping () : single
        static member setAngularDamping (value : single) (entity : Entity) : Entity = entity?AngularDamping <- value
        member entity.GravityScale with get () = entity?GravityScale () : single
        static member setGravityScale (value : single) (entity : Entity) : Entity = entity?GravityScale <- value
        member entity.CollisionCategories with get () = entity?CollisionCategories () : string
        static member setCollisionCategories (value : string) (entity : Entity) : Entity = entity?CollisionCategories <- value
        member entity.CollisionMask with get () = entity?CollisionMask () : string
        static member setCollisionMask (value : string) (entity : Entity) : Entity = entity?CollisionMask <- value
        member entity.IsBullet with get () = entity?IsBullet () : bool
        static member setIsBullet (value : bool) (entity : Entity) : Entity = entity?IsBullet <- value
        member entity.IsSensor with get () = entity?IsSensor () : bool
        static member setIsSensor (value : bool) (entity : Entity) : Entity = entity?IsSensor <- value

        static member getPhysicsId (entity : Entity) = PhysicsId (entity.Id, entity.MinorId)

[<RequireQualifiedAccess>]
module SimpleBodyFacet =

    let init (entity : Entity) (_ : IXDispatcherContainer) =
        entity |>
            Entity.setMinorId -<| NuCore.makeId () |>
            Entity.setBodyType BodyType.Dynamic |>
            Entity.setDensity NormalDensity |>
            Entity.setFriction 0.0f |>
            Entity.setRestitution 0.0f |>
            Entity.setFixedRotation false |>
            Entity.setLinearDamping 1.0f |>
            Entity.setAngularDamping 1.0f |>
            Entity.setGravityScale 1.0f |>
            Entity.setCollisionCategories "1" |>
            Entity.setCollisionMask "*" |>
            Entity.setIsBullet false |>
            Entity.setIsSensor false

    let registerPhysics makeBodyShape address world =
        let entity = World.getEntity address world
        let bodyProperties = 
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
              IsSensor = entity.IsSensor }
        let physicsId = Entity.getPhysicsId entity
        let position = entity.Position + entity.Size * 0.5f
        let rotation = entity.Rotation
        World.createBody address physicsId position rotation bodyProperties world

    let unregisterPhysics address world =
        let entity = World.getEntity address world
        World.destroyBody (Entity.getPhysicsId entity) world

    let propagatePhysics makeBodyShape address world =
        let world = unregisterPhysics address world
        registerPhysics makeBodyShape address world

    let handleBodyTransformMessage address (message : BodyTransformMessage) world =
        let entity = World.getEntity address world
        let entity =
            entity |>
                Entity.setPosition (message.Position - entity.Size * 0.5f) |> // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                Entity.setRotation message.Rotation
        World.setEntity message.EntityAddress entity world

[<AutoOpen>]
module SimpleSpriteFacetModule =

    type Entity with

        member entity.ImageSprite with get () = entity?ImageSprite () : Sprite
        static member setImageSprite (value : Sprite) (entity : Entity) : Entity = entity?ImageSprite <- value

[<RequireQualifiedAccess>]
module SimpleSpriteFacet =

    let init (entity : Entity) (_ : IXDispatcherContainer) =
        Entity.setImageSprite { SpriteAssetName = "Image3"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } entity

    let getRenderDescriptors entity viewType world =
        if not entity.Visible || not <| Camera.inView3 entity.Position entity.Size world.Camera then []
        else
            [LayerableDescriptor
                { Depth = entity.Depth
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = entity.Position
                          Size = entity.Size
                          Rotation = entity.Rotation
                          ViewType = viewType
                          OptInset = None
                          Sprite = entity.ImageSprite
                          Color = Vector4.One }}]

    let getQuickSize (entity : Entity) world =
        let sprite = entity.ImageSprite
        match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
        | None -> DefaultEntitySize
        | Some size -> size

[<AutoOpen>]
module SimpleAnimatedSpriteFacetModule =

    type Entity with

        member entity.Stutter with get () = entity?Stutter () : int
        static member setStutter (value : int) (entity : Entity) : Entity = entity?Stutter <- value
        member entity.TileCount with get () = entity?TileCount () : int
        static member setTileCount (value : int) (entity : Entity) : Entity = entity?TileCount <- value
        member entity.TileRun with get () = entity?TileRun () : int
        static member setTileRun (value : int) (entity : Entity) : Entity = entity?TileRun <- value
        member entity.TileSize with get () = entity?TileSize () : Vector2
        static member setTileSize (value : Vector2) (entity : Entity) : Entity = entity?TileSize <- value

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
        entity |>
            Entity.setStutter 4 |>
            Entity.setTileCount 16 |>
            Entity.setTileRun 4 |>
            Entity.setTileSize -<| Vector2 (16.0f, 16.0f) |>
            Entity.setImageSprite { SpriteAssetName = "Image7"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

    let getRenderDescriptors (entity : Entity) viewType (world : World) =
        if not entity.Visible || not <| Camera.inView3 entity.Position entity.Size world.Camera then []
        else
            [LayerableDescriptor
                { Depth = entity.Depth
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = entity.Position
                          Size = entity.Size
                          Rotation = entity.Rotation
                          ViewType = viewType
                          OptInset = getImageOptInset entity world
                          Sprite = entity.ImageSprite
                          Color = Vector4.One }}]

    let getQuickSize (entity : Entity) =
        entity.TileSize