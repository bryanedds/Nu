// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open Prime

/// Determines how an animation is played.
type [<Struct>] Playback =
    | Once
    | Loop
    | Bounce

/// Describes an animation.
type Animation =
    { StartTime : GameTime
      LifeTimeOpt : GameTime option
      Name : string
      Playback : Playback
      Rate : single
      Weight : single
      BoneFilterOpt : string Set option }

/// The type of rendering used on a surface (for use by the higher-level engine API).
type RenderStyle =
    | Deferred
    | Forward of Subsort : single * Sort : single

/// 3d navigation content.
type Navigation3dContent =
    | NavigationNil
    | NavigationBounds
    | NavigationGeometry
    | NavigationStaticModel
    | NavigationStaticModelSurface of int
    | NavigationStaticModelSurfaces of int array

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
        let mutable low = 0
        let mutable high = keys.Length - 1
        let mutable found = false
        let mutable i = 0
        while low <= high && not found do
            let mid = (low + high) / 2
            let midTime = single keys.[inc mid].Time
            if animationTime < midTime then high <- mid - 1
            elif animationTime > midTime then low <- mid + 1
            else found <- true; i <- mid
        if not found then
            i <- if animationTime < single keys.[inc low].Time then low else dec low
        i

    let internal ComputeRotationKeyFrameIndex (animationTime : single, keys : Assimp.QuaternionKey array) =
        let mutable low = 0
        let mutable high = keys.Length - 1
        let mutable found = false
        let mutable i = 0
        while low <= high && not found do
            let mid = (low + high) / 2
            let midTime = single keys.[inc mid].Time
            if animationTime < midTime then high <- mid - 1
            elif animationTime > midTime then low <- mid + 1
            else found <- true; i <- mid
        if not found then
            i <- if animationTime < single keys.[inc low].Time then low else dec low
        i

    let internal ComputeScalingKeyFrameIndex (animationTime : single, keys : Assimp.VectorKey array) =
        let mutable low = 0
        let mutable high = keys.Length - 1
        let mutable found = false
        let mutable i = 0
        while low <= high && not found do
            let mid = (low + high) / 2
            let midTime = single keys.[inc mid].Time
            if animationTime < midTime then high <- mid - 1
            elif animationTime > midTime then low <- mid + 1
            else found <- true; i <- mid
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

    let private AnimationChannelsDict =
        ConcurrentDictionary<_, _> HashIdentity.Reference

    let private NodeEmpty =
        Assimp.Node ()

    type [<Struct>] private BoneInfo =
        { BoneTransformOffset : Assimp.Matrix4x4
          mutable BoneTransformFinal : Assimp.Matrix4x4 }

        static member make offset =
            { BoneTransformOffset = offset
              BoneTransformFinal = Unchecked.defaultof<_> }

    type [<NoEquality; NoComparison; Struct>] private AnimationChannel =
        { TranslationKeys : Assimp.VectorKey array
          RotationKeys : Assimp.QuaternionKey array
          ScalingKeys : Assimp.VectorKey array }

        static member make translationKeys rotationKeys scalingKeys =
            { TranslationKeys = translationKeys
              RotationKeys = rotationKeys
              ScalingKeys = scalingKeys }

    type [<CustomEquality; NoComparison>] private AnimationChannelKey =
        { AnimationName : string
          NodeName : string
          HashCode : int }

        static member make animationName nodeName =
            let hashCode = hash animationName ^^^ hash nodeName
            { AnimationName = animationName
              NodeName = nodeName
              HashCode = hashCode }

        override this.Equals thatObj =
            match thatObj with
            | :? AnimationChannelKey as that -> this.AnimationName = that.AnimationName && this.NodeName = that.NodeName
            | _ -> false

        override this.GetHashCode () =
            this.HashCode

    type [<NoEquality; NoComparison>] AnimationDecomposition =
        { Translation : Assimp.Vector3D
          Rotation : Assimp.Quaternion
          Scaling : Assimp.Vector3D
          Weight : single }

        static member make translation rotation scaling weight =
            { Translation = translation
              Rotation = rotation
              Scaling = scaling
              Weight = weight }

    type Assimp.Material with

        member this.RenderStyleOpt =
            match this.GetNonTextureProperty (Constants.Assimp.RawPropertyPrefix + nameof RenderStyle) with
            | null -> None
            | property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<RenderStyle> |> Some
                    with _ -> None
                else None

        member this.PresenceOpt =
            match this.GetNonTextureProperty (Constants.Assimp.RawPropertyPrefix + nameof Presence) with
            | null -> None
            | property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<Presence> |> Some
                    with _ -> None
                else None

        member this.IgnoreLightMapsOpt =
            match this.GetNonTextureProperty (Constants.Assimp.RawPropertyPrefix + nameof Constants.Render.IgnoreLightMapsName) with
            | null -> None
            | property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<bool> |> Some
                    with _ -> None
                else None

        member this.OpaqueDistanceOpt =
            match this.GetNonTextureProperty (Constants.Assimp.RawPropertyPrefix + nameof Constants.Render.OpaqueDistanceName) with
            | null -> None
            | property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<single> |> Some
                    with _ -> None
                else None

        member this.TwoSidedOpt =
            match this.GetNonTextureProperty (Constants.Assimp.RawPropertyPrefix + Constants.Render.TwoSidedName) with
            | null -> None
            | property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<bool> |> Some
                    with _ -> None
                else Some true

        member this.Navigation3dContentOpt =
            match this.GetNonTextureProperty (Constants.Assimp.RawPropertyPrefix + Constants.Render.Navigation3dContentName) with
            | null -> None
            | property ->
                if property.PropertyType = Assimp.PropertyType.String then
                    try property.GetStringValue () |> scvalueMemo<Navigation3dContent> |> Some
                    with _ -> None
                else Some NavigationNil

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
            match this.Metadata.TryGetValue (nameof RenderStyle) with
            | (true, entry) ->
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<RenderStyle> |> Some
                    with _ -> None
                | _ -> None
            | (false, _) -> None

        member this.PresenceOpt =
            match this.Metadata.TryGetValue (nameof Presence) with
            | (true, entry) ->
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<Presence> |> Some
                    with _ -> None
                | _ -> None
            | (false, _) -> None

        member this.IgnoreLightMapsOpt =
            match this.Metadata.TryGetValue Constants.Render.IgnoreLightMapsName with
            | (true, entry) ->
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<bool> |> Some
                    with _ -> None
                | _ -> None
            | (false, _) -> None

        member this.OpaqueDistanceOpt =
            match this.Metadata.TryGetValue Constants.Render.OpaqueDistanceName with
            | (true, entry) ->
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<single> |> Some
                    with _ -> None
                | _ -> None
            | (false, _) -> None

        member this.Navigation3dContentOpt =
            match this.Metadata.TryGetValue Constants.Render.Navigation3dContentName with
            | (true, entry) ->
                match entry.DataType with
                | Assimp.MetaDataType.String ->
                    try entry.Data :?> string |> scvalueMemo<Navigation3dContent> |> Some
                    with _ -> None
                | _ -> None
            | (false, _) -> None

    /// Mesh extensions.
    type Assimp.Scene with

        member this.IndexDatasToMetadata () =
            for i in 0 .. dec this.Meshes.Count do
                let mesh = this.Meshes.[i]
                let indices = mesh.GetIndices ()
                this.Metadata.Add ("IndexData" + string i, Assimp.Metadata.Entry (Assimp.MetaDataType.Int32, indices))
                mesh.Faces.Clear ()
                mesh.Faces.Capacity <- 0

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

            // compute local transform of the current node.
            // TODO: see if there's a clean way to get rid of allocation here.
            let mutable nodeTransform = node.Transform // NOTE: if the node is animated, its transform is replaced by that animation entirely.
            let decompositions = List animations.Length
            for animation in animations do
                let animationStartTime = animation.StartTime.Seconds
                let animationLifeTimeOpt = Option.map (fun (lifeTime : GameTime) -> lifeTime.Seconds) animation.LifeTimeOpt
                match animationChannels.TryGetValue (AnimationChannelKey.make animation.Name node.Name) with
                | (true, channel) ->
                    let localTime = time - animationStartTime
                    if  localTime >= 0.0f &&
                        (match animationLifeTimeOpt with Some lifeTime -> localTime < animationStartTime + lifeTime | None -> true) &&
                        (match animation.BoneFilterOpt with Some boneFilter -> boneFilter.Contains node.Name | None -> true) then
                        let localTimeScaled =
                            match animation.Playback with
                            | Once ->
                                localTime * animation.Rate * Constants.Render.AnimatedModelRateScalar
                            | Loop ->
                                let length = single channel.RotationKeys.[dec channel.RotationKeys.Length].Time
                                localTime * animation.Rate * Constants.Render.AnimatedModelRateScalar % length
                            | Bounce ->
                                let length = single channel.RotationKeys.[dec channel.RotationKeys.Length].Time
                                let localTimeScaled = localTime * animation.Rate * Constants.Render.AnimatedModelRateScalar
                                let remainingTime = localTimeScaled % length
                                if int (localTimeScaled / length) % 2 = 1
                                then length - remainingTime
                                else remainingTime
                        let translation = Assimp.InterpolatePosition (localTimeScaled, channel.TranslationKeys)
                        let rotation = Assimp.InterpolateRotation (localTimeScaled, channel.RotationKeys)
                        let scaling = Assimp.InterpolateScaling (localTimeScaled, channel.ScalingKeys)
                        decompositions.Add (AnimationDecomposition.make translation rotation scaling animation.Weight)
                | (false, _) -> ()
            if decompositions.Count > 0 then
                let mutable translationAccumulated = Assimp.Vector3D 0.0f
                let mutable rotationAccumulated = Assimp.Quaternion (1.0f, 0.0f, 0.0f, 0.0f)
                let mutable scalingAccumulated = Assimp.Vector3D 1.0f
                let mutable weightAccumulated = 0.0f
                for i in 0 .. dec decompositions.Count do
                    let factor = weightAccumulated / (weightAccumulated + decompositions.[i].Weight)
                    let factor2 = 1.0f - factor
                    translationAccumulated <- translationAccumulated * factor + decompositions.[i].Translation * factor2
                    rotationAccumulated <- Assimp.Quaternion.Slerp (rotationAccumulated, decompositions.[i].Rotation, factor2)
                    scalingAccumulated <- scalingAccumulated * factor + decompositions.[i].Scaling * factor2
                    weightAccumulated <- weightAccumulated + decompositions.[i].Weight
                nodeTransform <-
                    Assimp.Matrix4x4.FromScaling scalingAccumulated *
                    Assimp.Matrix4x4 (rotationAccumulated.GetMatrix ()) *
                    Assimp.Matrix4x4.FromTranslation translationAccumulated // TODO: see if there's a faster way to construct a TRS matrix here.

            // compute current transform and assign the final bone transform where applicable
            let accumulatedTransform = nodeTransform * parentTransform
            match boneIds.TryGetValue node.Name with
            | (true, boneId) ->
                let boneTransformOffset = boneInfos.[boneId].BoneTransformOffset
                boneInfos.[boneId].BoneTransformFinal <- boneTransformOffset * accumulatedTransform
                boneWrites.Value <- inc boneWrites.Value
            | (false, _) -> ()

            // recur if there are still bones left to write
            if boneWrites.Value < boneInfos.Length then
                for i in 0 .. dec node.Children.Count do
                    let child = node.Children.[i]
                    Assimp.Scene.UpdateBoneTransforms (time, boneIds, boneInfos, boneWrites, animationChannels, animations, child, accumulatedTransform, scene)

        /// Compute the animated transforms of the meshes bones in the given scene.
        /// Thread-safe.
        member this.ComputeBoneTransforms (time : GameTime, animations : Animation array, mesh : Assimp.Mesh) =

            // pre-compute animation channels
            let animationChannels =
                match AnimationChannelsDict.TryGetValue this with
                | (false, _) ->
                    let animationChannels = dictPlus HashIdentity.Structural []
                    for animationId in 0 .. dec this.Animations.Count do
                        let animation = this.Animations.[animationId]
                        for channelId in 0 .. dec animation.NodeAnimationChannels.Count do
                            let channel = animation.NodeAnimationChannels.[channelId]
                            let animationChannel = AnimationChannel.make (Array.ofSeq channel.PositionKeys) (Array.ofSeq channel.RotationKeys) (Array.ofSeq channel.ScalingKeys)
                            animationChannels.[AnimationChannelKey.make animation.Name channel.NodeName] <- animationChannel
                    AnimationChannelsDict.[this] <- animationChannels
                    animationChannels
                | (true, animationChannels) -> animationChannels

            // log if there are more bones than we currently support
            if mesh.Bones.Count >= Constants.Render.BonesMax then
                Log.info ("Assimp mesh bone count exceeded currently supported number of bones in scene '" + this.Name + "'.")

            // pre-compute bone id dict and bone info storage (these should probably persist outside of this function and be reused)
            let boneIds = dictPlus StringComparer.Ordinal []
            let boneInfos = Array.zeroCreate<BoneInfo> mesh.Bones.Count
            for boneId in 0 .. dec mesh.Bones.Count do
                let bone = mesh.Bones.[boneId]
                let boneName = bone.Name
                boneIds.[boneName] <- boneId
                boneInfos.[boneId] <- BoneInfo.make bone.OffsetMatrix

            // write bone transforms to bone infos array
            Assimp.Scene.UpdateBoneTransforms (time.Seconds, boneIds, boneInfos, ref 0, animationChannels, animations, this.RootNode, Assimp.Matrix4x4.Identity, this)

            // convert bone info transforms to Nu's m4 representation
            Array.map (fun (boneInfo : BoneInfo) -> Assimp.ExportMatrix boneInfo.BoneTransformFinal) boneInfos