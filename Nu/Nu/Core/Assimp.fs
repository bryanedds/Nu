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
      BonesOpt : string Set option }

/// Additional assimp functionality.
/// Intentionally prevents the original Assimp namespace from being opened.
[<RequireQualifiedAccess>]
module Assimp =

    type [<NoEquality; NoComparison; Struct>] BoneInfo =
        { BoneTransformOffset : Assimp.Matrix4x4
          mutable BoneTransformFinal : Assimp.Matrix4x4 }

        static member make offset =
            { BoneTransformOffset = offset
              BoneTransformFinal = Unchecked.defaultof<_> }

    /// Convert a matrix from an Assimp representation to Nu's.
    let ExportMatrix (m : Assimp.Matrix4x4) =
        Matrix4x4
            (m.A1, m.B1, m.C1, m.D1,
             m.A2, m.B2, m.C2, m.D2,
             m.A3, m.B3, m.C3, m.D3,
             m.A4, m.B4, m.C4, m.D4)

    let TransformQuaternion (m : Assimp.Matrix4x4, q : Assimp.Quaternion) =
        Assimp.Quaternion
            (m.D1 * q.X + m.D2 * q.Y + m.D3 * q.Z + m.D4 * q.W,
             m.A1 * q.X + m.A2 * q.Y + m.A3 * q.Z + m.A4 * q.W,
             m.B1 * q.X + m.B2 * q.Y + m.B3 * q.Z + m.B4 * q.W,
             m.C1 * q.X + m.C2 * q.Y + m.C3 * q.Z + m.C4 * q.W)

    let internal TryGetAnimationChannel (animation : Assimp.Animation, nodeName : string) =
        let mutable resultOpt = None
        let mutable i = 0
        while resultOpt.IsNone && i < animation.NodeAnimationChannels.Count do
            let channel = animation.NodeAnimationChannels.[i]
            if (channel.NodeName = nodeName) then resultOpt <- Some channel
            else i <- inc i
        resultOpt

    let internal ComputePositionKeyFrameIndex (animationTime : single, channel : Assimp.NodeAnimationChannel) =
        let mutable found = false
        let mutable i = 0
        while not found && i < dec channel.PositionKeyCount do
            if animationTime < single channel.PositionKeys.[inc i].Time then found <- true
            else i <- inc i
        i

    let internal ComputeRotationKeyFrameIndex (animationTime : single, channel : Assimp.NodeAnimationChannel) =
        let mutable found = false
        let mutable i = 0
        while not found && i < dec channel.RotationKeyCount do
            if animationTime < single channel.RotationKeys.[inc i].Time then found <- true
            else i <- inc i
        i

    let internal ComputeScalingKeyFrameIndex (animationTime : single, channel : Assimp.NodeAnimationChannel) =
        let mutable found = false
        let mutable i = 0
        while not found && i < dec channel.ScalingKeyCount do
            if animationTime < single channel.ScalingKeys.[inc i].Time then found <- true
            else i <- inc i
        i

    let internal InterpolatePosition (animationTime : single, channel : Assimp.NodeAnimationChannel) =
        if channel.PositionKeys.Count = 1 then
            channel.PositionKeys.[0].Value
        else
            let PositionIndex = ComputePositionKeyFrameIndex (animationTime, channel)
            let NextPositionIndex = inc PositionIndex % channel.PositionKeys.Count
            let DeltaTime = single (channel.PositionKeys.[NextPositionIndex].Time - channel.PositionKeys.[PositionIndex].Time)
            let Factor = (animationTime - single channel.PositionKeys.[PositionIndex].Time) / DeltaTime
            let Start = channel.PositionKeys.[PositionIndex].Value
            let End = channel.PositionKeys.[NextPositionIndex].Value
            let Delta = End - Start
            let Result = Start + Factor * Delta
            Result

    let internal InterpolateRotation (animationTime : single, channel : Assimp.NodeAnimationChannel) =
        if channel.RotationKeys.Count = 1 then
            channel.RotationKeys.[0].Value
        else
            let RotationIndex = ComputeRotationKeyFrameIndex (animationTime, channel)
            let NextRotationIndex = inc RotationIndex % channel.RotationKeys.Count
            let DeltaTime = single (channel.RotationKeys.[NextRotationIndex].Time - channel.RotationKeys.[RotationIndex].Time)
            let Factor = (animationTime - single channel.RotationKeys.[RotationIndex].Time) / DeltaTime
            let StartRotationQ = channel.RotationKeys.[RotationIndex].Value
            let EndRotationQ = channel.RotationKeys.[NextRotationIndex].Value
            let Result = Assimp.Quaternion.Slerp (StartRotationQ, EndRotationQ, Factor)
            Result.Normalize () // TODO: consider not normalizing here since we should already have two unit quaternions.
            Result

    let internal InterpolateScaling (animationTime : single, channel : Assimp.NodeAnimationChannel) =
        if channel.ScalingKeys.Count = 1 then
            channel.ScalingKeys.[0].Value
        else
            let ScalingIndex = ComputeScalingKeyFrameIndex (animationTime, channel)
            let NextScalingIndex = inc ScalingIndex % channel.ScalingKeys.Count
            let DeltaTime = single (channel.ScalingKeys.[NextScalingIndex].Time - channel.ScalingKeys.[ScalingIndex].Time)
            let Factor = (animationTime - single channel.ScalingKeys.[ScalingIndex].Time) / DeltaTime
            let Start = channel.ScalingKeys.[ScalingIndex].Value
            let End = channel.ScalingKeys.[NextScalingIndex].Value
            let Delta = End - Start
            let Result = Start + Factor * Delta
            Result

[<AutoOpen>]
module AssimpExtensions =

    /// Mesh extensions.
    type Assimp.Mesh with

        static member private UpdateBoneTransforms
            (gameTime : GameTime,
             animationIds : Dictionary<string, int>,
             boneIds : Dictionary<string, int>,
             boneInfos : Assimp.BoneInfo array,
             animations : Animation array,
             node : Assimp.Node,
             parentTransform : Assimp.Matrix4x4,
             scene : Assimp.Scene) =

            // compute local transform of the current node.
            // note that if the node is animated, its transform is replaced by that animation entirely.
            let name = node.Name
            let mutable nodeTransform = node.Transform
            let animationOpts =
                [|for animation in animations do
                    match animationIds.TryGetValue animation.Name with
                    | (true, animationId) ->
                        let localTime = gameTime - animation.StartTime
                        let animationAssimp = scene.Animations.[animationId]
                        match Assimp.TryGetAnimationChannel (animationAssimp, name) with
                        | Some channel ->
                            let localMilliseconds = localTime.Milliseconds
                            let translation = Assimp.InterpolatePosition (localMilliseconds, channel)
                            let rotation = Assimp.InterpolateRotation (localMilliseconds, channel)
                            let scale = Assimp.InterpolateScaling (localMilliseconds, channel)
                            Some (translation, rotation, scale, animation.Weight)
                        | None -> None
                    | (false, _) -> None|]
            match Array.definitizePlus animationOpts with
            | (true, animations) ->
                if Array.notEmpty animationOpts then
                    let mutable translationAccumulated = Assimp.Vector3D 0.0f
                    let mutable rotationAccumulated = Assimp.Quaternion (1.0f, 0.0f, 0.0f, 0.0f)
                    let mutable scaleAccumulated = Assimp.Vector3D 1.0f
                    let mutable weightAccumulated = 0.0f
                    for (translation, rotation, scale, weight) in animations do
                        let factor = weightAccumulated / (weightAccumulated + weight)
                        let factor2 = 1.0f - factor
                        translationAccumulated <- translationAccumulated * factor + translation * factor2
                        rotationAccumulated <- Assimp.Quaternion.Slerp (rotationAccumulated, rotation, factor2)
                        scaleAccumulated <- scaleAccumulated * factor + scale * factor2
                        weightAccumulated <- weightAccumulated + weight
                    nodeTransform <-
                        // TODO: see if there's a faster way to construct a TRS matrix here.
                        Assimp.Matrix4x4.FromScaling scaleAccumulated *
                        Assimp.Matrix4x4 (rotationAccumulated.GetMatrix ()) *
                        Assimp.Matrix4x4.FromTranslation translationAccumulated
            | (false, _) ->
                // ignore partial animation inputs
                // TODO: consider logging here?
                ()

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
                Assimp.Mesh.UpdateBoneTransforms (gameTime, animationIds, boneIds, boneInfos, animations, child, accumulatedTransform, scene)

        member this.AnimateBones (gameTime, animations, scene : Assimp.Scene) =

            // pre-compute animation id dict
            let animationIds = dictPlus StringComparer.Ordinal []
            for animationId in 0 .. dec scene.Animations.Count do
                let animation = scene.Animations.[animationId]
                let animationName = animation.Name
                animationIds.[animationName] <- animationId

            // pre-compute bone id dict and bone info storage (these should probably persist outside of this function and be reused)
            let boneIds = dictPlus StringComparer.Ordinal []
            let boneInfos = Array.zeroCreate<Assimp.BoneInfo> this.Bones.Count
            for boneId in 0 .. dec this.Bones.Count do
                let bone = this.Bones.[boneId]
                let boneName = bone.Name
                boneIds.[boneName] <- boneId
                boneInfos.[boneId] <- Assimp.BoneInfo.make bone.OffsetMatrix

            // write bone transforms to bone infos array
            Assimp.Mesh.UpdateBoneTransforms (gameTime, animationIds, boneIds, boneInfos, animations, scene.RootNode, Assimp.Matrix4x4.Identity, scene)

            // convert bone info transforms to Nu's m4 representation
            Array.map (fun (boneInfo : Assimp.BoneInfo) -> Assimp.ExportMatrix boneInfo.BoneTransformFinal) boneInfos

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