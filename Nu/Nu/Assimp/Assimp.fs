// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open Prime
open System.Reflection

/// Determines how an animated behavior is executed.
type [<Struct>] Playback =
    | Once
    | Loop
    | Bounce

/// Represents different repetition modes for an animated behavior.
type [<DefaultValue "[Cycle 1]">] Repetition =
    | Cycle of Cycles : int
    | Iterate of Iterations : int

/// Describes an animation.
type [<SymbolicExpansion; DefaultValue "[[StartTime 0] [LifeTimeOpt None] [Name \"\"] [Playback Loop] [Rate 1] [Weight 1] [BoneFilterOpt None]]">] Animation =
    { StartTime : GameTime
      LifeTimeOpt : GameTime option
      Name : string
      Playback : Playback
      Rate : single
      Weight : single
      BoneFilterOpt : string Set option }

    /// Make an animation value.
    static member make startTime lifeTimeOpt name playback rate weight boneFilterOpt =
        { StartTime = startTime; LifeTimeOpt = lifeTimeOpt; Name = name; Playback = playback; Rate = rate; Weight = weight; BoneFilterOpt = boneFilterOpt }

    /// Make an play-once animation value.
    static member once startTime lifeTimeOpt name =
        Animation.make startTime lifeTimeOpt name Once 1.0f 1.0f None

    /// Make an looping animation value.
    static member loop startTime lifeTimeOpt name =
        Animation.make startTime lifeTimeOpt name Loop 1.0f 1.0f None

    /// Make an bouncing animation value.
    static member bounce startTime lifeTimeOpt name =
        Animation.make startTime lifeTimeOpt name Bounce 1.0f 1.0f None

/// Specifies the type of desired fragment depth testing.
type [<Struct>] DepthTest =
    | LessThanTest
    | LessThanOrEqualTest
    | EqualTest
    | GreaterThanOrEqualTest
    | GreaterThanTest
    | NeverPassTest
    | AlwaysPassTest

/// The type of rendering used on a surface (for use by the higher-level engine API).
type RenderStyle =
    | Deferred
    | Forward of Subsort : single * Sort : single

/// The shape of a navigation body (includes both 2d and 3d representations, with some cases unsupported depending on
/// the dimensionality of the system utilizing it).
type NavShape =
    | EmptyNavShape
    | BoundsNavShape
    | StaticModelNavShape
    | StaticModelSurfaceNavShape

/// The batch phasing such involved in persisting OpenGL state.
type [<Struct>] BatchPhase =
    | StartingPhase
    | ResumingPhase
    | StoppingPhase
    | SingletonPhase
    member this.Starting = match this with StartingPhase | SingletonPhase -> true | ResumingPhase | StoppingPhase -> false
    member this.Stopping = match this with StoppingPhase | SingletonPhase -> true | ResumingPhase | StartingPhase -> false

/// Additional assimp functionality.
/// Intentionally prevents the original Assimp namespace from being opened.
[<RequireQualifiedAccess>]
module Assimp =

    /// Convert a matrix from an Assimp representation to Nu's.
    let ExportMatrix (m : Assimp.Matrix4x4) =
        Matrix4x4
            (m.A1, m.B1, m.C1, m.D1,
             m.A2, m.B2, m.C2, m.D2,
             m.A3, m.B3, m.C3, m.D3,
             m.A4, m.B4, m.C4, m.D4)

    let internal ComputePositionKeyFrameIndex (animationTime : single, keys : Assimp.VectorKey array) =
        let last = dec keys.Length
        let mutable low = 0
        let mutable high = last
        let mutable found = false
        let mutable i = 0
        while low <= high && not found do
            let mid = (low + high) / 2
            if mid < last then
                let midTime = single keys.[inc mid].Time
                if animationTime < midTime then high <- mid - 1
                elif animationTime > midTime then low <- mid + 1
                else found <- true; i <- mid
            else found <- true; i <- last
        if not found then
            i <- if animationTime < single keys.[inc low].Time then low else dec low
        i

    let internal ComputeRotationKeyFrameIndex (animationTime : single, keys : Assimp.QuaternionKey array) =
        let last = dec keys.Length
        let mutable low = 0
        let mutable high = last
        let mutable found = false
        let mutable i = 0
        while low <= high && not found do
            let mid = (low + high) / 2
            if mid < last then
                let midTime = single keys.[inc mid].Time
                if animationTime < midTime then high <- mid - 1
                elif animationTime > midTime then low <- mid + 1
                else found <- true; i <- mid
            else found <- true; i <- last
        if not found then
            i <- if animationTime < single keys.[inc low].Time then low else dec low
        i

    let internal ComputeScalingKeyFrameIndex (animationTime : single, keys : Assimp.VectorKey array) =
        let last = dec keys.Length
        let mutable low = 0
        let mutable high = last
        let mutable found = false
        let mutable i = 0
        while low <= high && not found do
            let mid = (low + high) / 2
            if mid < last then
                let midTime = single keys.[inc mid].Time
                if animationTime < midTime then high <- mid - 1
                elif animationTime > midTime then low <- mid + 1
                else found <- true; i <- mid
            else found <- true; i <- last
        if not found then
            i <- if animationTime < single keys.[inc low].Time then low else dec low
        i

    let internal InterpolatePosition (animationTime : single, positionKeys : Assimp.VectorKey array) =
        if positionKeys.Length <> 1 then
            let positionIndex = ComputePositionKeyFrameIndex (animationTime, positionKeys)
            let positionIndexNext = inc positionIndex % positionKeys.Length
            let positionKey = positionKeys.[positionIndex]
            let positionKeyNext = positionKeys.[positionIndexNext]
            let deltaTime = single (positionKeyNext.Time - positionKey.Time)
            let factor = (animationTime - single positionKey.Time) / deltaTime
            let start = positionKey.Value
            let stop = positionKeyNext.Value
            let delta = stop - start
            start + factor * delta
        else positionKeys.[0].Value

    let internal InterpolateRotation (animationTime : single, rotationKeys : Assimp.QuaternionKey array) =
        if rotationKeys.Length <> 1 then
            let rotationIndex = ComputeRotationKeyFrameIndex (animationTime, rotationKeys)
            let rotationIndexNext = inc rotationIndex % rotationKeys.Length
            let rotationKey = rotationKeys.[rotationIndex]
            let rotationKeyNext = rotationKeys.[rotationIndexNext]
            let deltaTime = single (rotationKeyNext.Time - rotationKey.Time)
            let factor = (animationTime - single rotationKey.Time) / deltaTime
            let startRotation = rotationKey.Value
            let stopRotation = rotationKeyNext.Value
            let result = Assimp.Quaternion.Slerp (startRotation, stopRotation, factor)
            result.Normalize ()
            result
        else rotationKeys.[0].Value

    let internal InterpolateScaling (animationTime : single, scalingKeys : Assimp.VectorKey array) =
        if scalingKeys.Length <> 1 then
            let scalingIndex = ComputeScalingKeyFrameIndex (animationTime, scalingKeys)
            let scalingIndexNext = inc scalingIndex % scalingKeys.Length
            let scalingKey = scalingKeys.[scalingIndex]
            let scalingKeyNext = scalingKeys.[scalingIndexNext]
            let deltaTime = single (scalingKeyNext.Time - scalingKey.Time)
            let factor = (animationTime - single scalingKey.Time) / deltaTime
            let start = scalingKey.Value
            let stop = scalingKeyNext.Value
            let delta = stop - start
            start + factor * delta
        else scalingKeys.[0].Value

[<AutoOpen>]
module AssimpExtensions =

    let private NodeEmpty =
        Assimp.Node ()

    type MaterialPropertyComparer<'e when 'e : equality> () =
        interface struct (Assimp.Material * string) IEqualityComparer with
            member this.Equals (struct (leftMaterial, leftPropertyName), struct (rightMaterial, rightPropertyName)) =
                leftMaterial = rightMaterial &&
                leftPropertyName = rightPropertyName
            member this.GetHashCode struct (material, propertyName) =
                material.GetHashCode () ^^^
                propertyName.GetHashCode ()

    let private MaterialPropertyCached =
        ConcurrentDictionary<_, _> (MaterialPropertyComparer ())

    type [<Struct>] private BoneInfo =
        { BoneOffset : Assimp.Matrix4x4
          mutable BoneTransform : Assimp.Matrix4x4 }

        static member make offset =
            { BoneOffset = offset
              BoneTransform = Unchecked.defaultof<_> }

    type [<NoEquality; NoComparison; Struct>] private AnimationChannel =
        { TranslationKeys : Assimp.VectorKey array
          RotationKeys : Assimp.QuaternionKey array
          ScalingKeys : Assimp.VectorKey array }

        static member make translationKeys rotationKeys scalingKeys =
            { TranslationKeys = translationKeys
              RotationKeys = rotationKeys
              ScalingKeys = scalingKeys }

    type [<Struct; CustomEquality; NoComparison>] private AnimationChannelKey =
        { AnimationName : string
          NodeName : string
          HashCode : int }

        static member make animationName nodeName =
            let hashCode = hash animationName ^^^ hash nodeName
            { AnimationName = animationName
              NodeName = nodeName
              HashCode = hashCode }

        static member equals left right =
            left.AnimationName = right.AnimationName &&
            left.NodeName = right.NodeName

        static member hash key =
            key.HashCode

        override this.Equals thatObj =
            match thatObj with
            | :? AnimationChannelKey as that -> AnimationChannelKey.equals this that
            | _ -> false

        override this.GetHashCode () =
            AnimationChannelKey.hash this

    type private AnimationChannelKeyComparer<'e when 'e : equality> () =
        interface AnimationChannelKey IEqualityComparer with
            member _.Equals (leftKey, rightKey) = AnimationChannelKey.equals leftKey rightKey
            member _.GetHashCode key = AnimationChannelKey.hash key

    type [<Struct; NoEquality; NoComparison>] AnimationDecomposition =
        { Translation : Assimp.Vector3D
          Rotation : Assimp.Quaternion
          Scaling : Assimp.Vector3D
          Weight : single }

        static member make translation rotation scaling weight =
            { Translation = translation
              Rotation = rotation
              Scaling = scaling
              Weight = weight }

    let private AnimationChannelsCached =
        ConcurrentDictionary<_, _> HashIdentity.Reference

    type Assimp.Quaternion with
        
        member this.Scale (scalar : single) =
            Assimp.Quaternion (this.W * scalar, this.X * scalar, this.Y * scalar, this.Z * scalar)

        member this.Add (that : Assimp.Quaternion) =
            Assimp.Quaternion (this.W + that.W, this.X + that.X, this.Y + that.Y, this.Z + that.Z)

        member this.Abs =
            if this.W < 0.0f then this.Scale -1.0f else this

        member this.Dot (q2 : Assimp.Quaternion) =
            this.W * q2.W + this.X * q2.X + this.Y * q2.Y + this.Z * q2.Z

        member this.Normalized =
            let mutable copy = this
            this.Normalize ()
            copy

    /// Material extensions.
    type Assimp.Material with

        member this.TryGetMaterialProperty propertyName =
            let key = struct (this, propertyName)
            let mutable property = Unchecked.defaultof<_>
            if not (MaterialPropertyCached.TryGetValue (key, &property)) then
                let propertyOpt = ValueOption.ofObj (this.GetNonTextureProperty propertyName)
                MaterialPropertyCached.[key] <- propertyOpt
                propertyOpt
            else property

        member this.RenderStyleOpt =
            match this.TryGetMaterialProperty Constants.Assimp.RenderStylePropertyName with
            | ValueSome property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<RenderStyle> |> ValueSome
                    with _ -> ValueNone
                else ValueNone
            | ValueNone -> ValueNone

        member this.PresenceOpt =
            match this.TryGetMaterialProperty Constants.Assimp.PresencePropertyName with
            | ValueSome property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<Presence> |> ValueSome
                    with _ -> ValueNone
                else ValueNone
            | ValueNone -> ValueNone

        member this.IgnoreLightMapsOpt =
            match this.TryGetMaterialProperty Constants.Assimp.IgnoreLightMapsPropertyName with
            | ValueSome property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<bool> |> ValueSome
                    with _ -> ValueNone
                else ValueNone
            | ValueNone -> ValueNone

        member this.OpaqueDistanceOpt =
            match this.TryGetMaterialProperty Constants.Assimp.OpaqueDistancePropertyName with
            | ValueSome property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<single> |> ValueSome
                    with _ -> ValueNone
                else ValueNone
            | ValueNone -> ValueNone

        member this.FinenessOffsetOpt =
            match this.TryGetMaterialProperty Constants.Assimp.FinenessOffsetPropertyName with
            | ValueSome property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<single> |> ValueSome
                    with _ -> ValueNone
                else ValueNone
            | ValueNone -> ValueNone

        member this.ScatterTypeOpt =
            match this.TryGetMaterialProperty Constants.Assimp.ScatterTypePropertyName with
            | ValueSome property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<ScatterType> |> ValueSome
                    with _ -> ValueNone
                else ValueNone
            | ValueNone -> ValueNone

        member this.TwoSidedOpt =
            match this.TryGetMaterialProperty Constants.Assimp.TwoSidedPropertyName with
            | ValueSome property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<bool> |> ValueSome
                    with _ -> ValueNone
                else ValueSome true
            | ValueNone -> ValueNone

        member this.NavShapeOpt =
            match this.TryGetMaterialProperty Constants.Assimp.NavShapePropertyName with
            | ValueSome property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<NavShape> |> ValueSome
                    with _ -> ValueNone
                else ValueSome EmptyNavShape
            | ValueNone -> ValueNone

    /// Node extensions.
    type Assimp.Node with

        /// The empty assimp node.
        /// NOTE: Do NOT modify this as it is a globally shared stand-in for unavailble assimp nodes!
        static member Empty = NodeEmpty

        /// Get the world transform of the node.
        member this.TransformWorld =
            let mutable parentOpt = this.Parent
            let mutable transform = this.Transform
            while notNull parentOpt do
                transform <- transform * parentOpt.Transform
                parentOpt <- parentOpt.Parent
            transform

        /// Collect all the child nodes of a node, including the node itself.
        member this.CollectNodes () =
            [|this
              for child in this.Children do
                yield! child.CollectNodes ()|]

        /// Collect all the child nodes and transforms of a node, including the node itself.
        member this.CollectNodesAndTransforms (unitType, parentTransform : Matrix4x4) =
            [|let localTransform = Assimp.ExportMatrix this.Transform
              let worldTransform = localTransform * parentTransform
              yield (this, worldTransform)
              for child in this.Children do
                yield! child.CollectNodesAndTransforms (unitType, worldTransform)|]

        /// Map to a TreeNode.
        member this.Map<'a> (parentNames : string array, parentTransform : Matrix4x4, mapper : Assimp.Node -> string array -> Matrix4x4 -> 'a array TreeNode) : 'a array TreeNode =
            let localName = this.Name
            let localTransform = Assimp.ExportMatrix this.Transform
            let worldNames = Array.append parentNames [|localName|]
            let worldTransform = localTransform * parentTransform
            let node = mapper this worldNames worldTransform
            for child in this.Children do
                let child = child.Map<'a> (worldNames, worldTransform, mapper)
                node.Add child
            node

        member this.RenderStyleOpt =
            let mutable entry = Unchecked.defaultof<_>
            if this.Metadata.TryGetValue (nameof RenderStyle, &entry) then
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<RenderStyle> |> ValueSome
                    with _ -> ValueNone
                | _ -> ValueNone
            else ValueNone

        member this.PresenceOpt =
            let mutable entry = Unchecked.defaultof<_>
            if this.Metadata.TryGetValue (nameof Presence, &entry) then
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<Presence> |> ValueSome
                    with _ -> ValueNone
                | _ -> ValueNone
            else ValueNone

        member this.IgnoreLightMapsOpt =
            let mutable entry = Unchecked.defaultof<_>
            if this.Metadata.TryGetValue (Constants.Render.IgnoreLightMapsName, &entry) then
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<bool> |> ValueSome
                    with _ -> ValueNone
                | _ -> ValueNone
            else ValueNone

        member this.OpaqueDistanceOpt =
            let mutable entry = Unchecked.defaultof<_>
            if this.Metadata.TryGetValue (Constants.Render.OpaqueDistanceName, &entry) then
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<single> |> ValueSome
                    with _ -> ValueNone
                | _ -> ValueNone
            else ValueNone

        member this.FinenessOffsetOpt =
            let mutable entry = Unchecked.defaultof<_>
            if this.Metadata.TryGetValue (Constants.Render.FinenessOffsetName, &entry) then
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<single> |> ValueSome
                    with _ -> ValueNone
                | _ -> ValueNone
            else ValueNone

        member this.ScatterTypeOpt =
            let mutable entry = Unchecked.defaultof<_>
            if this.Metadata.TryGetValue (Constants.Render.ScatterTypeName, &entry) then
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<ScatterType> |> ValueSome
                    with _ -> ValueNone
                | _ -> ValueNone
            else ValueNone

        member this.NavShapeOpt =
            let mutable entry = Unchecked.defaultof<_>
            if this.Metadata.TryGetValue (Constants.Render.NavShapeName, &entry) then
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<NavShape> |> ValueSome
                    with _ -> ValueNone
                | _ -> ValueNone
            else ValueNone

    /// Scene extensions.
    type Assimp.Scene with

        member this.IndexDatasToMetadata () =
            for i in 0 .. dec this.Meshes.Count do
                let mesh = this.Meshes.[i]
                let indices = mesh.GetIndices ()
                this.Metadata.Add ("IndexData" + string i, Assimp.Metadata.Entry (Assimp.MetaDataType.Int32, indices))
                mesh.Faces.Clear ()
                mesh.Faces.Capacity <- 0

        member this.ClearColorData () =
            for i in 0 .. dec this.Meshes.Count do
                let mesh = this.Meshes.[i]
                let m_colorsField = (getType mesh).GetField ("m_colors", BindingFlags.Instance ||| BindingFlags.NonPublic)
                m_colorsField.SetValue (mesh, Array.empty<Assimp.Color4D List>)
                for attachment in mesh.MeshAnimationAttachments do
                    let m_colorsField = (getType attachment).GetField ("m_colors", BindingFlags.Instance ||| BindingFlags.NonPublic)
                    m_colorsField.SetValue (attachment, Array.empty<Assimp.Color4D List>)

        member this.TryFindNode (meshIndex, node : Assimp.Node) =
            let nodes =
                [for i in 0 .. dec node.MeshCount do
                    if node.MeshIndices.[i] = meshIndex then
                        node]
            match nodes with
            | node :: _ -> Some node
            | _ ->
                let nodes =
                    [for child in node.Children do
                        match this.TryFindNode (meshIndex, child) with
                        | Some node -> node
                        | None -> ()]
                List.tryHead nodes

        static member private UpdateBoneTransforms
            (time : single,
             boneIds : Dictionary<string, int>,
             boneInfos : BoneInfo array,
             boneWrites : int ref, // OPTIMIZATION: bones writes counter prevents us from traversing nodes in the hierarchy that would be redundant (once per duplicated armature).
             animationChannels : Dictionary<AnimationChannelKey, AnimationChannel>,
             animations : Animation array,
             node : Assimp.Node,
             parentTransform : Assimp.Matrix4x4,
             scene : Assimp.Scene) =

            // compute animation decompositions of the current node.
            let mutable nodeTransform = node.Transform // NOTE: if the node is animated, its transform is replaced by that animation entirely.
            let decompositions = List ()
            for animation in animations do
                let animationStartTime = animation.StartTime.Seconds
                let animationLifeTimeOpt = Option.map (fun (lifeTime : GameTime) -> lifeTime.Seconds) animation.LifeTimeOpt
                let mutable animationChannel = Unchecked.defaultof<_>
                if animationChannels.TryGetValue (AnimationChannelKey.make animation.Name node.Name, &animationChannel) then
                    let localTime = max 0.0f (time - animationStartTime)
                    if  (match animationLifeTimeOpt with Some lifeTime -> localTime < animationStartTime + lifeTime | None -> true) &&
                        (match animation.BoneFilterOpt with Some boneFilter -> boneFilter.Contains node.Name | None -> true) then
                        let localTimeScaled =
                            match animation.Playback with
                            | Once ->
                                localTime * animation.Rate * Constants.Render.AnimatedModelRateScalar
                            | Loop ->
                                let length = single animationChannel.RotationKeys.[dec animationChannel.RotationKeys.Length].Time
                                localTime * animation.Rate * Constants.Render.AnimatedModelRateScalar % length
                            | Bounce ->
                                let length = single animationChannel.RotationKeys.[dec animationChannel.RotationKeys.Length].Time
                                let localTimeScaled = localTime * animation.Rate * Constants.Render.AnimatedModelRateScalar
                                let remainingTime = localTimeScaled % length
                                if int (localTimeScaled / length) % 2 = 1
                                then length - remainingTime
                                else remainingTime
                        let translation = Assimp.InterpolatePosition (localTimeScaled, animationChannel.TranslationKeys)
                        let rotation = Assimp.InterpolateRotation (localTimeScaled, animationChannel.RotationKeys)
                        let scaling = Assimp.InterpolateScaling (localTimeScaled, animationChannel.ScalingKeys)
                        decompositions.Add (AnimationDecomposition.make translation rotation scaling animation.Weight)

            // compute local transform from said decompositions
            // TODO: consider using the more accurate approach discussed here -
            // https://theorangeduck.com/page/quaternion-weighted-average
            if decompositions.Count > 0 then
                let mutable translationAccumulated = Assimp.Vector3D 0.0f
                let mutable rotationAccumulated = Assimp.Quaternion (0.0f, 0.0f, 0.0f, 0.0f)
                let mutable scalingAccumulated = Assimp.Vector3D 0.0f
                let mutable weightAccumulated = 0.0f
                for decomposition in decompositions do
                    translationAccumulated <- translationAccumulated + decomposition.Translation * decomposition.Weight
                    rotationAccumulated <-
                        if rotationAccumulated.Dot decomposition.Rotation < 0.0f
                        then rotationAccumulated.Add (decomposition.Rotation.Scale -decomposition.Weight)
                        else rotationAccumulated.Add (decomposition.Rotation.Scale +decomposition.Weight)
                    scalingAccumulated <- scalingAccumulated + decomposition.Scaling * decomposition.Weight
                    weightAccumulated <- weightAccumulated + decomposition.Weight
                scalingAccumulated <- scalingAccumulated / weightAccumulated
                rotationAccumulated <- rotationAccumulated.Scale (1.0f / weightAccumulated)
                rotationAccumulated <- rotationAccumulated.Abs.Normalized
                translationAccumulated <- translationAccumulated / weightAccumulated
                nodeTransform <- // TODO: see if there's a faster way to construct a TRS matrix here.
                    Assimp.Matrix4x4.FromScaling scalingAccumulated *
                    Assimp.Matrix4x4 (rotationAccumulated.GetMatrix ()) *
                    Assimp.Matrix4x4.FromTranslation translationAccumulated

            // compute current transform and assign the final bone transform where applicable
            let accumulatedTransform = nodeTransform * parentTransform
            let mutable boneId = Unchecked.defaultof<_>
            if boneIds.TryGetValue (node.Name, &boneId) then
                let boneOffset = boneInfos.[boneId].BoneOffset
                boneInfos.[boneId].BoneTransform <- boneOffset * accumulatedTransform
                boneWrites.Value <- inc boneWrites.Value

            // recur if there are still bones left to write
            if boneWrites.Value < boneInfos.Length then
                for i in 0 .. dec node.ChildCount do
                    let child = node.Children.[i]
                    Assimp.Scene.UpdateBoneTransforms (time, boneIds, boneInfos, boneWrites, animationChannels, animations, child, accumulatedTransform, scene)

        /// Compute the bone ids, offsets, and animated transforms of the mesh's bones in the given scene.
        /// Thread-safe.
        member this.ComputeBoneTransforms (time : GameTime, animations : Animation array, mesh : Assimp.Mesh) =

            // pre-compute animation channels
            let mutable animationChannels = Unchecked.defaultof<_>
            if not (AnimationChannelsCached.TryGetValue (this, &animationChannels)) then
                animationChannels <- dictPlus (AnimationChannelKeyComparer ()) []
                for animation in this.Animations do
                    for channel in animation.NodeAnimationChannels do
                        let animationChannel = AnimationChannel.make (Array.ofSeq channel.PositionKeys) (Array.ofSeq channel.RotationKeys) (Array.ofSeq channel.ScalingKeys)
                        animationChannels.[AnimationChannelKey.make animation.Name channel.NodeName] <- animationChannel
                AnimationChannelsCached.[this] <- animationChannels

            // log if there are more bones than we currently support
            if mesh.Bones.Count >= Constants.Render.BonesMax then
                Log.warn ("Assimp mesh bone count exceeded currently supported number of bones in scene '" + this.Name + "'.")

            // pre-compute bone id dict and bone info storage (these should probably persist outside of this function and be reused)
            let boneIds = dictPlus StringComparer.Ordinal []
            let boneInfos = Array.zeroCreate<_> mesh.Bones.Count
            for boneId in 0 .. dec mesh.Bones.Count do
                let bone = mesh.Bones.[boneId]
                let boneName = bone.Name
                boneIds.[boneName] <- boneId
                boneInfos.[boneId] <- BoneInfo.make bone.OffsetMatrix

            // write bone transforms to bone infos array
            Assimp.Scene.UpdateBoneTransforms (time.Seconds, boneIds, boneInfos, ref 0, animationChannels, animations, this.RootNode, Assimp.Matrix4x4.Identity, this)

            // convert bone info transforms to Nu's m4 representation
            let boneOffsets = Array.zeroCreate boneInfos.Length
            let boneTransforms = Array.zeroCreate boneInfos.Length
            for i in 0 .. dec boneInfos.Length do
                let boneInfo = &boneInfos.[i]
                boneOffsets.[i] <- Assimp.ExportMatrix boneInfo.BoneOffset
                boneTransforms.[i] <- Assimp.ExportMatrix boneInfo.BoneTransform
            (boneIds, boneOffsets, boneTransforms)