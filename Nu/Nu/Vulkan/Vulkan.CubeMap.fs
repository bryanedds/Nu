namespace Vulkan
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module CubeMap =

    /// Describes some cube map geometry that's loaded into VRAM.
    type CubeMapGeometry =
        { Bounds : Box3
          PrimitiveType : uint
          ElementCount : int
          Vertices : Vector3 array
          VertexBuffer : uint
          IndexBuffer : uint
          CubeMapVao : uint }