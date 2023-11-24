// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime

/// Determines how an animation is played.
type [<StructuralEquality; NoComparison>] Playback =
    | Once
    | Loop
    | Bounce

/// Describes an animation.
type [<StructuralEquality; NoComparison>] Animation =
    { StartTime : GameTime
      LifeTimeOpt : GameTime option
      Name : string
      Playback : Playback
      Rate : single
      Weight : single
      BoneFilterOpt : string Set option }

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

    let private AnimationChannelsDict = dictPlus HashIdentity.Reference []

    type [<NoEquality; NoComparison; Struct>] private BoneInfo =
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

    /// Mesh extensions.
    type Assimp.Mesh with

        static member private UpdateBoneTransforms
            (time : GameTime,
             animationChannels : Dictionary<AnimationChannelKey, AnimationChannel>,
             boneIds : Dictionary<string, int>,
             boneInfos : BoneInfo array,
             animations : Animation array,
             node : Assimp.Node,
             parentTransform : Assimp.Matrix4x4,
             scene : Assimp.Scene) =

            // compute local transform of the current node.
            // note that if the node is animated, its transform is replaced by that animation entirely.
            let name = node.Name
            let mutable nodeTransform = node.Transform
            let decompositionOpts =
                [|for animation in animations do
                    match animationChannels.TryGetValue (AnimationChannelKey.make animation.Name name) with
                    | (true, channel) ->
                        let localTime = time - animation.StartTime
                        if  localTime >= GameTime.zero &&
                            (match animation.LifeTimeOpt with Some lifeTime -> localTime < animation.StartTime + lifeTime | None -> true) &&
                            (match animation.BoneFilterOpt with Some boneFilter -> boneFilter.Contains node.Name | None -> true) then
                            let localTimeScaled =
                                match animation.Playback with
                                | Once ->
                                    localTime.Seconds * animation.Rate * Constants.Render.AnimatedModelRateScalar
                                | Loop ->
                                    let length = single channel.RotationKeys.[dec channel.RotationKeys.Length].Time
                                    localTime.Seconds * animation.Rate * Constants.Render.AnimatedModelRateScalar % length
                                | Bounce ->
                                    let length = single channel.RotationKeys.[dec channel.RotationKeys.Length].Time
                                    let localTimeScaled = localTime.Seconds * animation.Rate * Constants.Render.AnimatedModelRateScalar
                                    let remainingTime = localTimeScaled % length
                                    if int (localTimeScaled / length) % 2 = 1
                                    then length - remainingTime
                                    else remainingTime
                            let translation = Assimp.InterpolatePosition (localTimeScaled, channel.TranslationKeys)
                            let rotation = Assimp.InterpolateRotation (localTimeScaled, channel.RotationKeys)
                            let scaling = Assimp.InterpolateScaling (localTimeScaled, channel.ScalingKeys)
                            Some (translation, rotation, scaling, animation.Weight)
                        else None
                    | (false, _) -> None|]
            let decompositions = Array.definitize decompositionOpts
            if Array.notEmpty decompositions then
                let mutable translationAccumulated = Assimp.Vector3D 0.0f
                let mutable rotationAccumulated = Assimp.Quaternion (1.0f, 0.0f, 0.0f, 0.0f)
                let mutable scalingAccumulated = Assimp.Vector3D 1.0f
                let mutable weightAccumulated = 0.0f
                for (translation, rotation, scaling, weight) in decompositions do
                    let factor = weightAccumulated / (weightAccumulated + weight)
                    let factor2 = 1.0f - factor
                    translationAccumulated <- translationAccumulated * factor + translation * factor2
                    rotationAccumulated <- Assimp.Quaternion.Slerp (rotationAccumulated, rotation, factor2)
                    scalingAccumulated <- scalingAccumulated * factor + scaling * factor2
                    weightAccumulated <- weightAccumulated + weight
                nodeTransform <-
                    // TODO: see if there's a faster way to construct a TRS matrix here.
                    Assimp.Matrix4x4.FromScaling scalingAccumulated *
                    Assimp.Matrix4x4 (rotationAccumulated.GetMatrix ()) *
                    Assimp.Matrix4x4.FromTranslation translationAccumulated

            // compute current transform and assign the final bone transform where applicable
            let accumulatedTransform = nodeTransform * parentTransform
            match boneIds.TryGetValue name with
            | (true, boneId) ->
                let boneTransformOffset = boneInfos.[boneId].BoneTransformOffset
                boneInfos.[boneId].BoneTransformFinal <- boneTransformOffset * accumulatedTransform
            | (false, _) -> ()

            // recur
            for i in 0 .. dec node.Children.Count do
                let child = node.Children.[i]
                Assimp.Mesh.UpdateBoneTransforms (time, animationChannels, boneIds, boneInfos, animations, child, accumulatedTransform, scene)

        member this.ComputeBoneTransforms (time, animations, scene : Assimp.Scene) =

            // pre-compute animation channels
            let animationChannels =
                match AnimationChannelsDict.TryGetValue scene with
                | (false, _) ->
                    let animationChannels = dictPlus HashIdentity.Structural []
                    for animationId in 0 .. dec scene.Animations.Count do
                        let animation = scene.Animations.[animationId]
                        for channelId in 0 .. dec animation.NodeAnimationChannels.Count do
                            let channel = animation.NodeAnimationChannels.[channelId]
                            animationChannels.[AnimationChannelKey.make animation.Name channel.NodeName] <-
                                AnimationChannel.make (Array.ofSeq channel.PositionKeys) (Array.ofSeq channel.RotationKeys) (Array.ofSeq channel.ScalingKeys)
                    AnimationChannelsDict.[scene] <- animationChannels
                    animationChannels
                | (true, animationChannels) -> animationChannels

            // log if there are more bones than we currently support
            if this.Bones.Count >= Constants.Render.BonesMax then
                Log.info ("Assimp mesh bone count exceeded currently supported number of bones for mesh '" + this.Name + "' in scene '" + scene.Name + "'.")

            // pre-compute bone id dict and bone info storage (these should probably persist outside of this function and be reused)
            let boneIds = dictPlus StringComparer.Ordinal []
            let boneInfos = Array.zeroCreate<BoneInfo> this.Bones.Count
            for boneId in 0 .. dec this.Bones.Count do
                let bone = this.Bones.[boneId]
                let boneName = bone.Name
                boneIds.[boneName] <- boneId
                boneInfos.[boneId] <- BoneInfo.make bone.OffsetMatrix

            // write bone transforms to bone infos array
            Assimp.Mesh.UpdateBoneTransforms (time, animationChannels, boneIds, boneInfos, animations, scene.RootNode, Assimp.Matrix4x4.Identity, scene)

            // convert bone info transforms to Nu's m4 representation
            Array.map (fun (boneInfo : BoneInfo) -> Assimp.ExportMatrix boneInfo.BoneTransformFinal) boneInfos

    /// Node extensions.
    type Assimp.Node with

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
            seq {
                yield this
                for child in this.Children do
                    yield! child.CollectNodes () }

        /// Collect all the child nodes and transforms of a node, including the node itself.
        member this.CollectNodesAndTransforms (unitType, parentTransform : Matrix4x4) =
            seq {
                let localTransform = Assimp.ExportMatrix this.Transform
                let worldTransform = localTransform * parentTransform
                yield (this, worldTransform)
                for child in this.Children do
                    yield! child.CollectNodesAndTransforms (unitType, worldTransform) }

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