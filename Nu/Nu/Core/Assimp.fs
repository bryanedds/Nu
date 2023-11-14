// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime

[<AutoOpen>]
module AssimpExtensions =

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

        /// Convert a matrix from an Assimp representation to Nu's.
        member this.ImportMatrix (m : Assimp.Matrix4x4) =
            Matrix4x4
                (m.A1, m.B1, m.C1, m.D1,
                 m.A2, m.B2, m.C2, m.D2,
                 m.A3, m.B3, m.C3, m.D3,
                 m.A4, m.B4, m.C4, m.D4)

        /// Collect all the child nodes of a node, including the node itself.
        member this.CollectNodes () =
            seq {
                yield this
                for child in this.Children do
                    yield! child.CollectNodes () }

        /// Collect all the child nodes and transforms of a node, including the node itself.
        member this.CollectNodesAndTransforms (unitType, parentTransform : Matrix4x4) =
            seq {
                let localTransform = this.ImportMatrix this.Transform
                let worldTransform = localTransform * parentTransform
                yield (this, worldTransform)
                for child in this.Children do
                    yield! child.CollectNodesAndTransforms (unitType, worldTransform) }

        /// Map to a TreeNode.
        member this.Map<'a> (parentNames : string array, parentTransform : Matrix4x4, mapper : Assimp.Node -> string array -> Matrix4x4 -> 'a array TreeNode) : 'a array TreeNode =
            let localName = this.Name
            let localTransform = this.ImportMatrix this.Transform
            let worldNames = Array.append parentNames [|localName|]
            let worldTransform = localTransform * parentTransform
            let node = mapper this worldNames worldTransform
            for child in this.Children do
                let child = child.Map<'a> (worldNames, worldTransform, mapper)
                node.Add child
            node

[<RequireQualifiedAccess>]
module AssimpAnimation =

    type [<NoEquality; NoComparison; Struct>] BoneInfo =
        { BoneOffset : Assimp.Matrix4x4
          mutable FinalTransform : Assimp.Matrix4x4 }

        static member make offset =
            { BoneOffset = offset
              FinalTransform = Assimp.Matrix4x4.Identity }

    let FindNodeAnim (pAnimation : Assimp.Animation, nodeName : string) =
        let mutable resultOpt = None
        let mutable i = 0
        while resultOpt.IsNone && i < pAnimation.NodeAnimationChannels.Count do
            let pNodeAnim = pAnimation.NodeAnimationChannels.[i]
            if (pNodeAnim.NodeName = nodeName) then resultOpt <- Some pNodeAnim
            i <- inc i
        resultOpt

    let FindPosition (animationTime : single, pNodeAnim : Assimp.NodeAnimationChannel) =
        let mutable found = false
        let mutable i = 0
        while not found && i < dec pNodeAnim.PositionKeyCount do
            if animationTime < single pNodeAnim.PositionKeys.[i + 1].Time then found <- true
            else i <- inc i
        i

    let FindRotation (animationTime : single, pNodeAnim : Assimp.NodeAnimationChannel) =
        let mutable found = false
        let mutable i = 0
        while not found && i < dec pNodeAnim.RotationKeyCount do
            if animationTime < single pNodeAnim.RotationKeys.[i + 1].Time then found <- true
            else i <- inc i
        i

    let FindScaling (animationTime : single, pNodeAnim : Assimp.NodeAnimationChannel) =
        let mutable found = false
        let mutable i = 0
        while not found && i < dec pNodeAnim.ScalingKeyCount do
            if animationTime < single pNodeAnim.ScalingKeys.[i + 1].Time then found <- true
            else i <- inc i
        i

    let CalcInterpolatedPosition (animationTime : single, pNodeAnim : Assimp.NodeAnimationChannel) =
        if pNodeAnim.PositionKeys.Count = 1 then
            pNodeAnim.PositionKeys.[0].Value
        else
            let PositionIndex = FindPosition (animationTime, pNodeAnim)
            let NextPositionIndex = inc PositionIndex
            assert (NextPositionIndex < pNodeAnim.PositionKeys.Count)
            let DeltaTime = single (pNodeAnim.PositionKeys.[NextPositionIndex].Time - pNodeAnim.PositionKeys.[PositionIndex].Time)
            let Factor = (animationTime - single pNodeAnim.PositionKeys.[PositionIndex].Time) / DeltaTime
            assert (Factor >= 0.0f && Factor <= 1.0f)
            let Start = pNodeAnim.PositionKeys.[PositionIndex].Value
            let End = pNodeAnim.PositionKeys.[NextPositionIndex].Value
            let Delta = End - Start
            let Result = Start + Factor * Delta
            Result

    let CalcInterpolatedRotation (animationTime : single, pNodeAnim : Assimp.NodeAnimationChannel) =
        if pNodeAnim.RotationKeys.Count = 1 then
            pNodeAnim.RotationKeys.[0].Value
        else
            let RotationIndex = FindRotation (animationTime, pNodeAnim)
            let NextRotationIndex = inc RotationIndex
            assert (NextRotationIndex < pNodeAnim.RotationKeys.Count)
            let DeltaTime = single (pNodeAnim.RotationKeys.[NextRotationIndex].Time - pNodeAnim.RotationKeys.[RotationIndex].Time)
            let Factor = (animationTime - single pNodeAnim.RotationKeys.[RotationIndex].Time) / DeltaTime
            assert (Factor >= 0.0f && Factor <= 1.0f)
            let StartRotationQ = pNodeAnim.RotationKeys.[RotationIndex].Value
            let EndRotationQ = pNodeAnim.RotationKeys.[NextRotationIndex].Value
            let Result = Assimp.Quaternion.Slerp (StartRotationQ, EndRotationQ, Factor)
            Result.Normalize ()
            Result

    let CalcInterpolatedScaling (animationTime : single, pNodeAnim : Assimp.NodeAnimationChannel) =
        if pNodeAnim.ScalingKeys.Count = 1 then
            pNodeAnim.ScalingKeys.[0].Value
        else
            let ScalingIndex = FindScaling (animationTime, pNodeAnim)
            let NextScalingIndex = inc ScalingIndex
            assert (NextScalingIndex < pNodeAnim.ScalingKeys.Count)
            let DeltaTime = single (pNodeAnim.ScalingKeys.[NextScalingIndex].Time - pNodeAnim.ScalingKeys.[ScalingIndex].Time)
            let Factor = (animationTime - single pNodeAnim.ScalingKeys.[ScalingIndex].Time) / DeltaTime
            assert (Factor >= 0.0f && Factor <= 1.0f)
            let Start = pNodeAnim.ScalingKeys.[ScalingIndex].Value
            let End = pNodeAnim.ScalingKeys.[NextScalingIndex].Value
            let Delta = End - Start
            let Result = Start + Factor * Delta
            Result

    let rec ReadNodeHierarchy (boneMapping : Dictionary<string, int>, boneInfos : BoneInfo array, animationTime : single, animationIndex : int, node : Assimp.Node, ParentTransform : Assimp.Matrix4x4, GlobalInverseTransform : Assimp.Matrix4x4, scene : Assimp.Scene) =
        let nodeName = node.Name
        let animation = scene.Animations.[animationIndex]
        let NodeTransform =
            match FindNodeAnim (animation, nodeName) with
            | Some pNodeAnim ->
                let Scaling = Assimp.Matrix4x4.FromScaling (CalcInterpolatedScaling (animationTime, pNodeAnim))
                let Rotation = (CalcInterpolatedRotation (animationTime, pNodeAnim)).GetMatrix () |> Assimp.Matrix4x4
                let Translation = Assimp.Matrix4x4.FromTranslation (CalcInterpolatedPosition (animationTime, pNodeAnim))
                Translation * Rotation * Scaling
            | None -> node.Transform

        let GlobalTransformation = ParentTransform * NodeTransform

        if boneMapping.ContainsKey nodeName then
            let BoneIndex = boneMapping.[nodeName]
            boneInfos.[BoneIndex].FinalTransform <- GlobalInverseTransform * GlobalTransformation * boneInfos.[BoneIndex].BoneOffset

        for i in 0 .. dec node.Children.Count do
            ReadNodeHierarchy (boneMapping, boneInfos, animationTime, animationIndex, node.Children.[i], GlobalTransformation, GlobalInverseTransform, scene)