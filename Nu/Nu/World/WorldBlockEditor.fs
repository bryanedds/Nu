// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module BlockMapDispatcherExtensions =
    type Entity with
        member this.GetBlockEditor world : BlockEditor = this.Get (nameof BlockEditor) world
        member this.SetBlockEditor (value : BlockEditor) world = this.Set (nameof BlockEditor) value world
        member this.BlockEditor = lens (nameof BlockEditor) this this.GetBlockEditor this.SetBlockEditor

type BlockMapDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Properties =
        [define Entity.BlockEditor BlockEditor.initial]

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay _ ->

            // draw grid lines using World.imGuiSegment
            let blockEditor = entity.GetBlockEditor world
            let blockMap = blockEditor.BlockMap
            let blockBounds = blockMap.BlockChunk.BlockBounds
            let blockScale = blockMap.BlockScale
            let blockMapSize = blockMap.Size
            let bounds = entity.GetBounds world
            let blockMapBounds = Box3 (bounds.Center - blockMapSize * 0.5f, blockMapSize)
            let gridColor = Color (64uy, 64uy, 64uy, 255uy) // TODO: make constant.

            // compute segments
            let segments =
                match blockEditor.BlockPlane with
                | XPos | XNeg ->

                    [|// compute segments down z axis
                      for i in blockBounds.Min.Y .. blockBounds.Max.Y do
                        let y = single i * blockScale.Y - blockMapSize.Y * 0.5f
                        let x = single blockEditor.BlockCursor.BlockPosition.X * blockScale.X + blockMapBounds.Min.X
                        let a = Vector3 (x, y, blockMapBounds.Min.Z)
                        let b = Vector3 (x, y, blockMapBounds.Max.Z)
                        Segment3 (a, b)

                      // compute segments down y axis
                      for i in blockBounds.Min.Z .. blockBounds.Max.Z do
                        let z = single i * blockScale.Z - blockMapSize.Z * 0.5f
                        let x = single blockEditor.BlockCursor.BlockPosition.X * blockScale.X + blockMapBounds.Min.X
                        let a = Vector3 (x, blockMapBounds.Min.Y, z)
                        let b = Vector3 (x, blockMapBounds.Max.Y, z)
                        Segment3 (a, b)|]

                | YPos | YNeg ->

                    [|// compute segments down z axis
                      for i in blockBounds.Min.X .. blockBounds.Max.X do
                        let x = single i * blockScale.X - blockMapSize.X * 0.5f
                        let y = single blockEditor.BlockCursor.BlockPosition.Y * blockScale.Y + blockMapBounds.Min.Y
                        let a = Vector3 (x, y, blockMapBounds.Min.Z)
                        let b = Vector3 (x, y, blockMapBounds.Max.Z)
                        Segment3 (a, b)

                      // compute segments down x axis
                      for i in blockBounds.Min.Z .. blockBounds.Max.Z do
                        let z = single i * blockScale.Z - blockMapSize.Z * 0.5f
                        let y = single blockEditor.BlockCursor.BlockPosition.Y * blockScale.Y + blockMapBounds.Min.Y
                        let a = Vector3 (blockMapBounds.Min.X, y, z)
                        let b = Vector3 (blockMapBounds.Max.X, y, z)
                        World.imGuiSegment3d (Segment3 (a, b)) 1.0f gridColor world|]

                | ZPos | ZNeg ->

                    [|// compute segments down y axis
                      for i in blockBounds.Min.X .. blockBounds.Max.X do
                        let x = single i * blockScale.X - blockMapSize.X * 0.5f
                        let z = single blockEditor.BlockCursor.BlockPosition.Z * blockScale.Z + blockMapBounds.Min.Z
                        let a = Vector3 (x, blockMapBounds.Min.Y, z)
                        let b = Vector3 (x, blockMapBounds.Max.Y, z)
                        Segment3 (a, b)

                      // compute segments down x axis
                      for i in blockBounds.Min.Y .. blockBounds.Max.Y do
                        let y = single i * blockScale.Y - blockMapSize.Y * 0.5f
                        let z = single blockEditor.BlockCursor.BlockPosition.Z * blockScale.Z + blockMapBounds.Min.Z
                        let a = Vector3 (blockMapBounds.Min.X, y, z)
                        let b = Vector3 (blockMapBounds.Max.X, y, z)
                        Segment3 (a, b)|]

            // draw segments
            World.imGuiSegments3d segments 1.0f gridColor world

        | _ -> ()