namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open FSharpx
open FSharpx.Lens.Operators
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module EntityModule =

    type Entity with

        [<XField>] member this.Position with get () = this?Position () : Vector2
        member this.SetPosition (value : Vector2) : Entity = this?Position <- value
        [<XField>] member this.Depth with get () = this?Depth () : single
        member this.SetDepth (value : single) : Entity = this?Depth <- value
        [<XField>] member this.Rotation with get () = this?Rotation () : single
        member this.SetRotation (value : single) : Entity = this?Rotation <- value
        [<XField>] member this.Size with get () = this?Size () : Vector2
        member this.SetSize (value : Vector2) : Entity = this?Size <- value
        [<XField>] member this.Enabled with get () = this?Enabled () : bool
        member this.SetEnabled (value : bool) : Entity = this?Enabled <- value
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
        [<XField>] member this.ImageSprite with get () = this?ImageSprite () : Sprite
        member this.SetImageSprite (value : Sprite) : Entity = this?ImageSprite <- value

        member this.Init (dispatcherContainer : IXDispatcherContainer) : Entity = this?Init dispatcherContainer
        member this.Register (address : Address, world : World) : World = this?Register (address, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, world)
        member this.PropagatePhysics (address : Address, world : World) : World = this?PropagatePhysics (address, world)
        member this.HandleBodyTransformMessage (address : Address, message : BodyTransformMessage, world : World) : World = this?HandleBodyTransformMessage (address, message, world)
        member this.GetRenderDescriptors (viewAbsolute : Matrix3, viewRelative : Matrix3, world : World) : RenderDescriptor list = this?GetRenderDescriptors (viewAbsolute, viewRelative, world)
        member this.GetQuickSize (world : World) : Vector2 = this?GetQuickSize world
        member this.IsTransformRelative (world : World) : bool = this?IsTransformRelative world

[<RequireQualifiedAccess>]
module Entity =

    let mouseToEntity position world (entity : Entity) =
        let positionScreen = Camera.mouseToScreen position world.Camera
        let view = (if entity.IsTransformRelative world then Camera.getViewRelativeF else Camera.getViewAbsoluteF) world.Camera
        let positionEntity = positionScreen * view
        positionEntity

    let setPositionSnapped snap position (entity : Entity) =
        let snapped = NuMath.snap2F snap position
        entity.SetPosition snapped

    let getTransform (entity : Entity) =
        { Transform.Position = entity.Position
          Depth = entity.Depth
          Size = entity.Size
          Rotation = entity.Rotation }

    let setTransform positionSnap rotationSnap transform (entity : Entity) =
        let transform = NuMath.snapTransform positionSnap rotationSnap transform
        entity
            .SetPosition(transform.Position)
            .SetDepth(transform.Depth)
            .SetSize(transform.Size)
            .SetRotation(transform.Rotation)

    let getPickingPriority (entity : Entity) =
        entity.Depth

    let makeDefaultUninitialized dispatcherName optName =
        let id = NuCore.getId ()
        { Id = id
          Name = match optName with None -> string id | Some name -> name
          Visible = true
          FacetNamesNs = []
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

    let makeDefault dispatcherName optName dispatcherContainer =
        let entity = makeDefaultUninitialized dispatcherName optName
        entity.Init dispatcherContainer

    let writeToXml (writer : XmlWriter) entity =
        writer.WriteStartElement typeof<Entity>.Name
        Xtension.writeTargetProperties writer entity
        writer.WriteEndElement ()

    let writeManyToXml (writer : XmlWriter) (entities : Map<_, _>) =
        for entityKvp in entities do
            writeToXml writer entityKvp.Value

    let readFromXml (entityNode : XmlNode) defaultDispatcherName dispatcherContainer =
        let entity = makeDefaultUninitialized defaultDispatcherName None
        Xtension.readTargetXDispatcher entityNode entity
        let entity = entity.Init dispatcherContainer
        Xtension.readTargetProperties entityNode entity
        entity

    let readManyFromXml (parentNode : XmlNode) defaultDispatcherName dispatcherContainer =
        let entityNodes = parentNode.SelectNodes "Entity"
        let entities =
            Seq.map
                (fun entityNode -> readFromXml entityNode defaultDispatcherName dispatcherContainer)
                (enumerable entityNodes)
        Seq.toList entities