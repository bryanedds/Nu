// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Buffers.Binary
open System.IO
open System.Numerics
open Prime

/// The endianness which indicates byte order in a raw asset.
type [<Struct>] Endianness =
    | LittleEndian
    | BigEndian

/// The format of a raw asset.
type RawFormat =
    | RawUInt8
    | RawUInt16 of Endianness
    | RawUInt32 of Endianness
    | RawSingle of Endianness

/// A height map for 3d terrain constructed from a raw asset.
type [<Struct>] RawHeightMap =
    { Resolution : Vector2i
      RawFormat : RawFormat
      RawAsset : Raw AssetTag }

type HeightMapMetadata =
    { Resolution : Vector2i
      HeightsNormalized : single array
      PositionsAndTexCoordses : struct (Vector3 * Vector2) array }

/// A height map for terrain.
type HeightMap =
    | ImageHeightMap of Image AssetTag // only supports 8-bit depth on Red channel
    | RawHeightMap of RawHeightMap

    static member private tryGetTextureData tryGetAssetFilePath (assetTag : Image AssetTag) =
        match tryGetAssetFilePath assetTag with
        | Some filePath ->
            match OpenGL.Texture.TryCreateTextureData filePath with
            | Some textureData ->
                let metadata = textureData.Metadata
                let (blockCompressed, bytes) = textureData.Bytes
                textureData.Dispose ()
                Some (metadata, blockCompressed, bytes)
            | None -> None
        | None -> None

    static member private tryGetRawAssetData tryGetAssetFilePath (assetTag : Raw AssetTag) =
        match tryGetAssetFilePath assetTag with
        | Some filePath ->
            try let bytes = File.ReadAllBytes filePath
                Some bytes
            with exn ->
                Log.info ("Could not load texture '" + filePath + "' due to: " + scstring exn)
                None
        | None -> None

    static member private tryGetImageHeightMapMetadata tryGetAssetFilePath (bounds : Box3) tiles image =

        // attempt to load texture data
        match HeightMap.tryGetTextureData tryGetAssetFilePath image with
        | Some (metadata, blockCompressed, bytes) ->

            // currently only supporting height data from block-compressed files
            if not blockCompressed then

                // compute normalize heights
                let resolutionX = metadata.TextureWidth
                let resolutionY = metadata.TextureHeight
                let scalar = 1.0f / single Byte.MaxValue
                let heightsNormalized =
                    [|for y in 0 .. dec resolutionY do
                        for x in 0 .. dec resolutionX do
                            let index = (resolutionX * y + x) * 4 + 2 // extract r channel of pixel
                            single bytes[index] * scalar|]

                // compute positions and tex coordses
                let quadSizeX = bounds.Size.X / single (dec resolutionX)
                let quadSizeY = bounds.Size.Z / single (dec resolutionY)
                let terrainHeight = bounds.Size.Y
                let terrainPositionX = bounds.Min.X
                let terrainPositionY = bounds.Min.Y
                let terrainPositionZ = bounds.Min.Z
                let texelWidth = 1.0f / single resolutionX
                let texelHeight = 1.0f / single resolutionY
                let positionsAndTexCoordses =
                    [|for y in 0 .. dec resolutionY do
                        for x in 0 .. dec resolutionX do
                            let normalized = heightsNormalized.[y * resolutionX + x]
                            let position = v3 (single x * quadSizeX + terrainPositionX) (normalized * terrainHeight + terrainPositionY) (single y * quadSizeY + terrainPositionZ)
                            let texCoords = v2 (single x * texelWidth) (single y * texelHeight) * tiles
                            struct (position, texCoords)|]

                // fin
                Some { Resolution = v2i resolutionX resolutionY; HeightsNormalized = heightsNormalized; PositionsAndTexCoordses = positionsAndTexCoordses }
            else Log.info "Block-compressed image files are unsupported for use as height maps."; None
        | None -> None

    static member private tryGetRawHeightMapMetadata tryGetAssetFilePath (bounds : Box3) tiles map =

        // ensure raw asset exists
        match HeightMap.tryGetRawAssetData tryGetAssetFilePath map.RawAsset with
        | Some rawAsset ->

            try // read normalized heights
                let resolutionX = map.Resolution.X
                let resolutionY = map.Resolution.Y
                let quadSizeX = bounds.Size.X / single (dec resolutionX)
                let quadSizeY = bounds.Size.Z / single (dec resolutionY)
                let terrainHeight = bounds.Size.Y
                let terrainPositionX = bounds.Min.X
                let terrainPositionY = bounds.Min.Y
                let terrainPositionZ = bounds.Min.Z
                let texelWidth = 1.0f / single resolutionX
                let texelHeight = 1.0f / single resolutionY
                use rawMemory = new MemoryStream (rawAsset)
                use rawReader = new BinaryReader (rawMemory)
                let heightsNormalized =
                    [|match map.RawFormat with
                      | RawUInt8 ->
                        let scalar = 1.0f / single Byte.MaxValue
                        for _ in 0 .. dec (resolutionY * resolutionX) do
                            single (rawReader.ReadByte ()) * scalar
                      | RawUInt16 endianness ->
                        let scalar = 1.0f / single UInt16.MaxValue
                        for _ in 0 .. dec (resolutionY * resolutionX) do
                            let value =
                                match endianness with
                                | LittleEndian -> BinaryPrimitives.ReadUInt16LittleEndian (rawReader.ReadBytes 2)
                                | BigEndian -> BinaryPrimitives.ReadUInt16BigEndian (rawReader.ReadBytes 2)
                            single value * scalar
                      | RawUInt32 endianness ->
                        let scalar = 1.0f / single UInt32.MaxValue
                        for _ in 0 .. dec (resolutionY * resolutionX) do
                            let value =
                                match endianness with
                                | LittleEndian -> BinaryPrimitives.ReadUInt32LittleEndian (rawReader.ReadBytes 4)
                                | BigEndian -> BinaryPrimitives.ReadUInt32BigEndian (rawReader.ReadBytes 4)
                            single value * scalar
                      | RawSingle endianness ->
                        for _ in 0 .. dec (resolutionY * resolutionX) do
                            let value =
                                match endianness with
                                | LittleEndian -> BinaryPrimitives.ReadSingleLittleEndian (rawReader.ReadBytes 4)
                                | BigEndian -> BinaryPrimitives.ReadSingleBigEndian (rawReader.ReadBytes 4)
                            value|]

                // compute positions and tex coordses
                let positionsAndTexCoordses =
                    [|for y in 0 .. dec resolutionY do
                        for x in 0 .. dec resolutionX do
                            let normalized = heightsNormalized.[y * resolutionX + x]
                            let position = v3 (single x * quadSizeX + terrainPositionX) (normalized * terrainHeight + terrainPositionY) (single y * quadSizeY + terrainPositionZ)
                            let texCoords = v2 (single x * texelWidth) (single y * texelHeight) * tiles
                            struct (position, texCoords)|]

                // fin
                Some { Resolution = v2i resolutionX resolutionY; HeightsNormalized = heightsNormalized; PositionsAndTexCoordses = positionsAndTexCoordses }
            with exn -> Log.infoOnce ("Attempt to read raw height map failed with the following exception: " + exn.Message); None
        | None -> None

    /// Attempt to compute height map metadata, loading assets as required.
    /// NOTE: if the heightmap pixel represents a quad in the terrain geometry in the exporting program, the geometry
    /// produced here is slightly different, with the border slightly clipped, and the terrain and quad size, slightly
    /// larger. i.e if the original map is 32m^2 and the original quad 1m^2 and the heightmap is 32x32, the quad axes
    /// below will be > 1.0.
    static member tryGetMetadata (tryGetAssetFilePath : AssetTag -> string option) bounds tiles heightMap =
        match heightMap with
        | ImageHeightMap image -> HeightMap.tryGetImageHeightMapMetadata tryGetAssetFilePath bounds tiles image
        | RawHeightMap map -> HeightMap.tryGetRawHeightMapMetadata tryGetAssetFilePath bounds tiles map

/// Identifies a body that can be found in a physics engine.
type [<CustomEquality; NoComparison>] BodyId =
    { BodySource : Simulant
      BodyIndex : int }

    /// Hash a BodyId.
    static member hash pid =
        pid.BodySource.SimulantAddress.GetHashCode () ^^^
        pid.BodyIndex.GetHashCode ()

    /// Equate BodyIds.
    static member equals pid pid2 =
        Address.equals pid.BodySource.SimulantAddress pid2.BodySource.SimulantAddress &&
        pid.BodyIndex = pid2.BodyIndex

    interface BodyId IEquatable with
        member this.Equals that =
            BodyId.equals this that

    override this.Equals that =
        match that with
        | :? BodyId as that -> BodyId.equals this that
        | _ -> false

    override this.GetHashCode () =
        BodyId.hash this

/// Identifies a body shape in a physics engine.
and ShapeIndex =
    { BodyId : BodyId
      ShapeIndex : int }

/// Identifies a joint in a physics engine.
and JointId =
    { JointSource : Simulant
      JointIndex : int }

/// Describes body shape-specific properties.
type BodyShapeProperties =
    { ShapeIndex : int
      FrictionOpt : single option
      RestitutionOpt : single option
      CollisionCategoriesOpt : int option
      CollisionMaskOpt : int option
      SensorOpt : bool option }

    /// The empty body shape properties value.
    static member empty =
        { ShapeIndex = 0
          FrictionOpt = None
          RestitutionOpt = None
          CollisionCategoriesOpt = None
          CollisionMaskOpt = None
          SensorOpt = None }

/// Internal object that carries interstitial information between Nu and a physics engine.
type [<NoEquality; NoComparison>] BodyUserObject =
    { BodyId : BodyId
      Dispose : unit -> unit }

/// Describes the substantial nature of a body in terms of mass or density.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax)>]
type [<Struct>] Substance =
    | Mass of Mass : single
    | Density of Density : single

/// Describe the form of collision detection to use.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax)>]
type [<Struct>] CollisionDetection =
    | Discontinuous
    | Continuous of MotionThreshold : single * SweptSphereRadius : single

/// The shape of a physics body box.
type BodyBox =
    { Size : Vector3
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }
    static member ofBox3 (box : Box3) =
        { Size = box.Size; TransformOpt = Some (Affine.makeTranslation box.Center); PropertiesOpt = None }

/// The shape of a physics body sphere.
type BodySphere =
    { Radius : single
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type BodyCapsule =
    { Height : single
      Radius : single
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type BodyBoxRounded =
    { Size : Vector3
      Radius : single
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a triangulated physics body convex hull.
type BodyConvexHull =
    { Vertices : Vector3 array
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a triangulated physics body static model.
type BodyStaticModel =
    { StaticModel : StaticModel AssetTag
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a triangulated physics body static model surface.
type BodyStaticModelSurface =
    { SurfaceIndex : int
      StaticModel : StaticModel AssetTag
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a triangulated physics body geometry.
type BodyGeometry =
    { Vertices : Vector3 array
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a triangulated physics body terrain.
type BodyTerrain =
    { Resolution : Vector2i
      Bounds : Box3
      HeightMap : HeightMap
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body.
/// TODO: consider renaming this family of types from Body___ to ___Shape.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type BodyShape =
    | BodyEmpty
    | BodyBox of BodyBox
    | BodySphere of BodySphere
    | BodyCapsule of BodyCapsule
    | BodyBoxRounded of BodyBoxRounded
    | BodyConvexHull of BodyConvexHull
    | BodyStaticModel of BodyStaticModel
    | BodyStaticModelSurface of BodyStaticModelSurface
    | BodyGeometry of BodyGeometry
    | BodyTerrain of BodyTerrain
    | BodyShapes of BodyShape list

/// The type of a physics body; Static, Kinematic, or Dynamic.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax)>]
type BodyType =
    | Static
    | Kinematic
    | Dynamic

/// The properties needed to describe the physical part of a body.
type BodyProperties =
    { BodyIndex : int
      Center : Vector3
      Rotation : Quaternion
      Scale : Vector3
      BodyType : BodyType
      BodyShape : BodyShape
      SleepingAllowed : bool
      Enabled : bool
      Friction : single
      Restitution : single
      LinearVelocity : Vector3
      LinearDamping : single
      AngularVelocity : Vector3
      AngularDamping : single
      AngularFactor : Vector3
      Substance : Substance
      GravityOverride : Vector3 option
      CollisionDetection : CollisionDetection
      CollisionCategories : int
      CollisionMask : int
      Sensor : bool }

type JointAngle =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3
      Axis : Vector3
      Axis2 : Vector3
      AngleMin : single
      AngleMax : single
      Softness : single
      BiasFactor : single
      RelaxationFactor : single
      BreakImpulseThreshold : single }

type JointDistance =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3
      Length : single
      Frequency : single }

type JointFriction =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointGear =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointMotor =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointPrismatic =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointPulley =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointRevolute =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointRope =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointWheel =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

/// A joint on physics bodies.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type JointDevice =
    | JointEmpty
    | JointAngle of JointAngle
    | JointDistance of JointDistance
    | JointFriction of JointFriction
    | JointGear of JointGear
    | JointMotor of JointMotor
    | JointPrismatic of JointPrismatic
    | JointPulley of JointPulley
    | JointRevolute of JointRevolute
    | JointRope of JointRope
    | JointWheel of JointWheel

type JointProperties =
    { JointIndex : int
      JointDevice : JointDevice }

    static member empty =
        { JointIndex = 0
          JointDevice = JointEmpty }

/// A message to the physics system to create a body.
type CreateBodyMessage =
    { BodyId : BodyId
      BodyProperties : BodyProperties }

/// A message to the physics system to create multiple bodies.
type CreateBodiesMessage =
    { BodySource : Simulant
      BodiesProperties : BodyProperties list }

/// A message to the physics system to destroy a body.
type DestroyBodyMessage =
    { BodyId : BodyId }

/// A message to the physics system to destroy multiple bodies.
type DestroyBodiesMessage =
    { BodyIds : BodyId list }

/// A message to the physics system to create a joint.
type CreateJointMessage =
    { JointSource : Simulant
      JointProperties : JointProperties }

/// A message to the physics system to create multiple joints.
type CreateJointsMessage =
    { JointsSource : Simulant
      JointsProperties : JointProperties list }

/// A message to the physics system to destroy a joint.
type DestroyJointMessage =
    { JointId : JointId }

/// A message to the physics system to destroy multiple joints.
type DestroyJointsMessage =
    { JointIds : JointId list }

/// A message to the physics system to destroy a body.
type SetBodyEnabledMessage =
    { BodyId : BodyId
      Enabled : bool }

/// A message to the physics system to destroy a body.
type SetBodyCenterMessage =
    { BodyId : BodyId
      Center : Vector3 }

/// A message to the physics system to set the rotation of a body.
type SetBodyRotationMessage =
    { BodyId : BodyId
      Rotation : Quaternion }

/// A message to the physics system to set the linear velocity of a body.
type SetBodyLinearVelocityMessage =
    { BodyId : BodyId
      LinearVelocity : Vector3 }

/// A message to the physics system to set the angular velocity of a body.
type SetBodyAngularVelocityMessage =
    { BodyId : BodyId
      AngularVelocity : Vector3 }

/// A message to the physics system to apply a linear impulse to a body.
type ApplyBodyLinearImpulseMessage =
    { BodyId : BodyId
      LinearImpulse : Vector3
      Offset : Vector3 }

/// A message to the physics system to apply an angular impulse to a body.
type ApplyBodyAngularImpulseMessage =
    { BodyId : BodyId
      AngularImpulse : Vector3 }

/// A message to the physics system to apply a force to a body.
type ApplyBodyForceMessage =
    { BodyId : BodyId
      Force : Vector3
      Offset : Vector3 }

/// A message to the physics system to apply torque to a body.
type ApplyBodyTorqueMessage =
    { BodyId : BodyId
      Torque : Vector3 }

/// An internally used message to the physics system to set the observed state of a body.
type SetBodyObservableMessage =
    { BodyId : BodyId
      Observable : bool }

/// A message from the physics system describing a body collision that took place.
type BodyCollisionMessage =
    { BodyShapeSource : ShapeIndex
      BodyShapeSource2 : ShapeIndex
      Normal : Vector3 }

/// A message from the physics system describing a body separation that took place.
type BodySeparationMessage =
    { BodyShapeSource : ShapeIndex
      BodyShapeSource2 : ShapeIndex }

/// A message from the physics system describing the updated transform of a body.
type BodyTransformMessage =
    { BodyId : BodyId
      Center : Vector3
      Rotation : Quaternion
      LinearVelocity : Vector3
      AngularVelocity : Vector3 }

/// A message from the physics system.
type IntegrationMessage =
    | BodyCollisionMessage of BodyCollisionMessage
    | BodySeparationMessage of BodySeparationMessage
    | BodyTransformMessage of BodyTransformMessage

/// A message to the physics system.
type PhysicsMessage =
    | CreateBodyMessage of CreateBodyMessage
    | CreateBodiesMessage of CreateBodiesMessage
    | DestroyBodyMessage of DestroyBodyMessage
    | DestroyBodiesMessage of DestroyBodiesMessage
    | CreateJointMessage of CreateJointMessage
    | CreateJointsMessage of CreateJointsMessage
    | DestroyJointMessage of DestroyJointMessage
    | DestroyJointsMessage of DestroyJointsMessage
    | SetBodyEnabledMessage of SetBodyEnabledMessage
    | SetBodyCenterMessage of SetBodyCenterMessage
    | SetBodyRotationMessage of SetBodyRotationMessage
    | SetBodyLinearVelocityMessage of SetBodyLinearVelocityMessage
    | SetBodyAngularVelocityMessage of SetBodyAngularVelocityMessage
    | ApplyBodyLinearImpulseMessage of ApplyBodyLinearImpulseMessage
    | ApplyBodyAngularImpulseMessage of ApplyBodyAngularImpulseMessage
    | ApplyBodyForceMessage of ApplyBodyForceMessage
    | ApplyBodyTorqueMessage of ApplyBodyTorqueMessage
    | SetBodyObservableMessage of SetBodyObservableMessage
    | SetGravityMessage of Vector3
    | ClearPhysicsMessageInternal

/// Represents a physics engine in Nu.
/// TODO: investigate if we'll ever have to handle enough physics or integration messages to necessitate the use of
/// SList instead of List.
type PhysicsEngine =
    /// Check that the physics engine contain the body with the given physics id.
    abstract GetBodyExists : BodyId -> bool
    /// Get the contact normals of the body with the given physics id.
    abstract GetBodyContactNormals : BodyId -> Vector3 list
    /// Get the linear velocity of the body with the given physics id.
    abstract GetBodyLinearVelocity : BodyId -> Vector3
    /// Get the angular velocity of the body with the given physics id.
    abstract GetBodyAngularVelocity : BodyId -> Vector3
    /// Get the contact normals where the body with the given physics id is touching the ground.
    abstract GetBodyToGroundContactNormals : BodyId -> Vector3 list
    /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
    abstract GetBodyToGroundContactNormalOpt : BodyId -> Vector3 option
    /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
    abstract GetBodyToGroundContactTangentOpt : BodyId -> Vector3 option
    /// Check that the body with the given physics id is on the ground.
    abstract IsBodyOnGround : BodyId -> bool
    /// Handle a physics message from an external source.
    abstract HandleMessage : PhysicsMessage -> unit
    /// Integrate the physics system one step.
    abstract Integrate : GameTime -> IntegrationMessage SArray
    /// Handle physics clean up by freeing all created resources.
    abstract CleanUp : unit -> unit

/// The stub implementation of PhysicsEngine.
type [<ReferenceEquality>] StubPhysicsEngine =
    private { StubPhysicsEngine : unit }
    static member make () = { StubPhysicsEngine = () }
    interface PhysicsEngine with
        member physicsEngine.GetBodyExists _ = false
        member physicsEngine.GetBodyContactNormals _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyLinearVelocity _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyAngularVelocity _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormals _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormalOpt _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactTangentOpt _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.IsBodyOnGround _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.HandleMessage _ = ()
        member physicsEngine.Integrate _ = SArray.empty
        member physicsEngine.CleanUp () = ()

[<RequireQualifiedAccess>]
module Physics =

    /// Convert a category mask to a value that represents collision categories.
    /// Examples -
    ///     * = -1
    ///     0 = 0
    ///     1 = 1
    ///     10 = 2
    ///     2 = ERROR - input must be either * or a binary number!
    let categorizeCollisionMask categoryMask =
        match categoryMask with
        | Constants.Physics.CollisionWildcard -> -1
        | _ -> Convert.ToInt32 (categoryMask, 2)

    /// Localize a body shape to a specific size.
    let rec localizeBodyShape (size : Vector3) bodyShape =
        let scaleTranslation (scalar : Vector3) (transformOpt : Affine option) =
            match transformOpt with
            | Some transform -> Some { transform with Translation = transform.Translation * scalar }
            | None -> None
        match bodyShape with
        | BodyEmpty -> BodyEmpty
        | BodyBox bodyBox -> BodyBox { bodyBox with Size = Vector3.Multiply (size, bodyBox.Size); TransformOpt = scaleTranslation size bodyBox.TransformOpt }
        | BodySphere bodySphere -> BodySphere { bodySphere with Radius = size.X * bodySphere.Radius; TransformOpt = scaleTranslation size bodySphere.TransformOpt }
        | BodyCapsule bodyCapsule -> BodyCapsule { bodyCapsule with Height = size.Y * bodyCapsule.Height; Radius = size.Y * bodyCapsule.Radius; TransformOpt = scaleTranslation size bodyCapsule.TransformOpt }
        | BodyBoxRounded bodyBoxRounded -> BodyBoxRounded { bodyBoxRounded with Size = Vector3.Multiply (size, bodyBoxRounded.Size); Radius = size.X * bodyBoxRounded.Radius; TransformOpt = scaleTranslation size bodyBoxRounded.TransformOpt }
        | BodyConvexHull bodyConvexHull -> BodyConvexHull { bodyConvexHull with Vertices = Array.map (fun vertex -> size * vertex) bodyConvexHull.Vertices; TransformOpt = scaleTranslation size bodyConvexHull.TransformOpt }
        | BodyStaticModel _ as bodyStaticModel -> bodyStaticModel
        | BodyStaticModelSurface _ as bodyStaticModelSurface -> bodyStaticModelSurface
        | BodyGeometry _ as bodyGeometry -> bodyGeometry
        | BodyTerrain _ as bodyTerrain -> bodyTerrain
        | BodyShapes bodyShapes -> BodyShapes (List.map (localizeBodyShape size) bodyShapes)