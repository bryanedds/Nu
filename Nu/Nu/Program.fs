(*
This example shows how to pack geometries into monolothic buffers and render them instanced using full flexiblity of
DrawCallInfo. In OpenGL for example, the resulting programm will use glMultiDrawElementsIndirect.
seealso: https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMultiDrawElementsIndirect.xhtml

This way we can objects packed into large vertex buffers at once while providing per object data (e.g. material infos)
and efficient instancing.
*)

open FSharp.Data.Adaptive
open SDL2
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Rendering.GL
open Aardvark.SceneGraph
open Aardvark.Application

module Shader =
    open FShade
    open Aardvark.Rendering.Effects

    type InstanceVertex = { 
        [<Position>]            pos   : V4d 
        [<Normal>]              n     : V3d 
        [<BiNormal>]            b     : V3d 
        [<Tangent>]             t     : V3d 
        [<InstanceTrafo>]       trafo : M44d
    }

    // same as DefaultSurfaces.trafo but assumes ortho model trafos and uses model trafo instead of proper normal matrix.
    let orthoInstanceTrafo (v : InstanceVertex) =
        vertex {
            return 
                { v with 
                    pos = v.trafo * v.pos 
                    n = v.trafo.TransformDir(v.n)
                    b = v.trafo.TransformDir(v.b)
                    t = v.trafo.TransformDir(v.t)
                }
        }

    type Vertex = {
        [<Position>]        pos : V4d
        [<Color>]           c   : V4d
        [<Normal>]          n   : V3d
    }

    // since we need special extension feature not provided by fshade we simply import the functionality (standard approach)
    [<GLSLIntrinsic("gl_DrawIDARB",requiredExtensions=[|"GL_ARB_shader_draw_parameters"|])>]
    let drawId () : int = raise <| FShade.Imperative.FShadeOnlyInShaderCodeException "drawId"

    // define some typed accessors for uniforms/storage buffers.
    type UniformScope with
        member x.ObjectColors : V4d[] = x?StorageBuffer?ObjectColors
        member x.MeshTrafo : M44d[] = x?StorageBuffer?MeshTrafo
    
    // fetches data from perMesh info
    let objectColor (v : Vertex) =
        vertex {
            let id = drawId()
            return 
                { v with 
                    c = uniform.ObjectColors.[id]; 
                    pos = uniform.MeshTrafo.[id] * v.pos  
                    n = uniform.MeshTrafo.[id].TransformDir v.n
                }
        }

// simple naive packing for IndexedGeometries 
// (not complete, nor production quality, e.g. it ignores all attributes other than vertices and normals)
module Packing = 

    open System.Collections.Generic

    type GeometryRange = { firstIndex : int; baseVertex : int; faceVertexCount : int }
    type PackedGeometry = 
        {
            vertices : array<V3f>
            normals  : array<V3f>
            indices  : array<int>
            ranges   : array<GeometryRange>
        }

    // pack together data into larger arrays
    let pack (geometries : seq<IndexedGeometry>) =
        let packedVertices = List<V3f>()
        let packedNormals = List<V3f>()
        let packedIndices = List<int>()
        let ranges = 
            [|
                for g in geometries do
                    assert (g.Mode = IndexedGeometryMode.TriangleList)
                    let firstIndex = packedIndices.Count
                    let baseVertex = packedVertices.Count
                    let indexed = g.ToIndexed()
                    let faceVertexCount = indexed.FaceVertexCount
                    packedVertices.AddRange(indexed.IndexedAttributes.[DefaultSemantic.Positions] |> unbox<V3f[]>)
                    packedNormals.AddRange(indexed.IndexedAttributes.[DefaultSemantic.Normals] |> unbox<V3f[]>)
                    packedIndices.AddRange(indexed.IndexArray |> unbox<int[]>)
                    yield { firstIndex = firstIndex; baseVertex = baseVertex; faceVertexCount = faceVertexCount }
            |]
        { vertices = packedVertices.ToArray(); normals = packedNormals.ToArray(); 
          indices = packedIndices.ToArray(); ranges = ranges }
            
[<EntryPoint>]
let main argv =

    Aardvark.Init ()
    let result = SDL.SDL_Init SDL.SDL_INIT_EVERYTHING
    let win = SDL.SDL_CreateWindow ("Window", SDL.SDL_WINDOWPOS_UNDEFINED, SDL.SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL.SDL_WindowFlags.SDL_WINDOW_OPENGL ||| SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN)
    let gl = SDL.SDL_GL_CreateContext win
    let windowInfo = new Nu.SdlWindowInfo (win)
    let graphicsContext = new Nu.SdlGraphicsContext (gl, win)
    let runtime = new Runtime ()
    let context = new Context (runtime, fun () -> ContextHandle (graphicsContext, windowInfo))
    runtime.Initialize context

    let win =
        window {
            backend Backend.GL
            display Display.Mono
            device DeviceKind.Dedicated
            debug false
            samples 8
        }

    // create simple geometries
    let torus  = IndexedGeometryPrimitives.solidTorus (Torus3d(V3d.Zero,V3d.OOI,1.0,0.2)) C4b.Green 20 20
    let sphere = IndexedGeometryPrimitives.solidPhiThetaSphere (Sphere3d.FromRadius(0.1)) 20 C4b.White

    // and pack them together
    let packedGeometry = 
        [ torus; sphere ] |> Packing.pack


    // manually create buffers for packed vertex data
    let vertices = BufferView(ArrayBuffer(packedGeometry.vertices) :> IBuffer |> AVal.constant,typeof<V3f>)
    let normals = BufferView(ArrayBuffer(packedGeometry.normals) :> IBuffer |> AVal.constant,typeof<V3f>)

    // adjust this size for testing different problem sizes.
    let size = 11
    let trafos =
        [|
            for x in -size .. size do
                for y in -size .. size do
                    for z in -size .. size do
                        yield Trafo3d.Scale(0.3) * Trafo3d.Translation(float x, float y, float z)
        |]

    // for each geometry range, we create an instanced draw call
    let drawCallInfos = 
        [| 
            for r in packedGeometry.ranges do 
                yield DrawCallInfo(FaceVertexCount = r.faceVertexCount,
                                   BaseVertex = r.baseVertex, FirstIndex = r.firstIndex,
                                   FirstInstance = 0, InstanceCount = trafos.Length)
        |]

    // put draw call infos into buffer
    let indirectBuffer = IndirectBuffer.ofArray false drawCallInfos

    // we use per mesh data here. each geometry range gets specific mesh info here.
    let objectColors = AVal.init [| C4f.Red; C4f.DarkGreen |]

    // additionally to per mesh colors, we use special model trafo for each mesh type.
    // this way we automatcially adjust trafo for all instances of the range.
    let perKindAnimation = 
        let sw = System.Diagnostics.Stopwatch.StartNew()
        win.Time |> AVal.map (fun t -> 
            let t = sw.Elapsed.TotalMilliseconds
            [| M44f.RotationX(float32 (t * 0.001)); M44f.Identity |]
        )

    let sg = 
        Sg.indirectDraw IndexedGeometryMode.TriangleList (AVal.constant indirectBuffer)
        |> Sg.vertexBuffer DefaultSemantic.Positions vertices
        |> Sg.vertexBuffer DefaultSemantic.Normals   normals
        |> Sg.uniform "ObjectColors" objectColors
        |> Sg.uniform "MeshTrafo"    perKindAnimation
        |> Sg.index' packedGeometry.indices
        |> Sg.instanceArray DefaultSemantic.InstanceTrafo (trafos |> Array.map (fun t -> t.Forward |> M44f.op_Explicit))
        |> Sg.shader {
            do! Shader.objectColor // adjusts per mesh data
            do! Shader.orthoInstanceTrafo // apply instance trafo, take care of normals (since we did not provide normal matrices for the instances [which would be totally possible of course])
            do! DefaultSurfaces.trafo     // proceed with standard trafo shaders (transform into projective space)
            do! DefaultSurfaces.simpleLighting // apply simple lighting
        }

    Sg.compile

    win.Scene <- sg
    win.Run()

    0