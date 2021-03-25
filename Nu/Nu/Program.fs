#if false
// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Diagnostics
open System.IO
open Prime
module Program =

    (* DISCUSSION - On Nu's authoring story...

    Instead of using a single, general purpose scripting language for authoring tasks in Nu, we use
    a small set of domain-specific languages. For example, the simulant system uses s-expression-
    based DSLs, as does the overlay, asset graph, and effect system. The simulation interactions
    are defined with chains directly in F#.

    What follows is a matrix of engine systems and the authoring language they provide to the user -

    system          | language                  | editor
    -----------------------------------------------------------
    simulants defs  | s-expr DSL                | Gaia
    event filtering | s-expr DSL                | Gaia
    collision bodies| s-expr DSL                | Gaia
    overlay         | s-expr DSL                | Visual Studio & Gaia
    asset graph     | s-expr DSL                | Visual Studio & Gaia
    script language | s-expr DSL                | Visual Studio & Gaia
    effect system   | s-expr DSL                | Gaia & Aether (TBA)
    mind (TBA)      | s-expr DSL                | Gaia & Pheobe (TBA) - http://www.cs.uu.nl/research/techreps/repo/CS-2013/2013-003.pdf
    particles       | F#                        | Visual Studio
    subsystems      | F#                        | Visual Studio
    ECS             | F#                        | Visual Studio
    components      | F# (facets / dispatchers) | Visual Studio
    elmish / MVU    | F# (facets / dispatchers) | Visual Studio
    interactions    | F# (chains)               | Visual Studio (deprecated, but still usable)

    The advantages and limitations that fall out of this are as such -

    The systems that provide an s-expr DSL have their DSLs interpreted at run-time and, unlike code
    in F#, allow for hot-reloading for optimal authoring experiences. For these systems, however,
    no static checking is in place, allowing for trivial errors.

    For the system that isn't interpreted, a strong type system is in place to make sure complex
    data-flow dependencies are made explicit and checked with good error messages. For this system,
    however, no hot-reloading is possible, negatively affecting the authoring experience.

    The trade-offs for each given domain does seem to be approxiately appropriate. *)

    (* WISDOM - Dealing with different device resolutions - Instead of rendering each component
    scaled to a back-buffer of a varying size, render each component unscaled to an off-screen
    buffer of a static size and then blit that with scaling to the back-buffer. NOTE: this only
    applies to 2D ~ will not apply to 3D once implemented in Nu (for obvious reasons). *)

    (* WISDOM: Keep all animation frame numbers even. That way, you can simply halve them if you
    need to move the app from 60fps to 30fps. *)

    (* TODO: investigate Gaia extensibility mechanism. *)

    /// Program entry point.
    let [<EntryPoint; STAThread>] main _ =

        // query user to create new project
        Console.Write "Create a new game with the Nu Game Engine? [y/n]: "
        let result = Console.ReadLine ()
        match result.ToUpper () with
        | "Y" ->

            // execute name entry
            Console.Write "Please enter your project's name (no spaces, tabs or dots, PascalCase is preferred): "
            let name = Console.ReadLine ()
            let name = name.Replace(" ", "").Replace("\t", "").Replace(".", "")
            if Array.notExists (fun char -> name.Contains (string char)) (Path.GetInvalidPathChars ()) then
                
                // compute directories
                let programDir = Reflection.Assembly.GetEntryAssembly().Location |> Path.GetDirectoryName 
                let slnDir = Path.Combine (programDir, "../../../..") |> Path.Simplify
                let templateDir = Path.Combine (programDir, "../../../Nu.Template") |> Path.Simplify
                let templateIdentifier = templateDir.Replace("/", "\\") // this is what dotnet knows the template as for uninstall...
                let templateFileName = "Nu.Template.fsproj"
                let projectsDir = Path.Combine (programDir, "../../../../Projects") |> Path.Simplify
                let newProjDir = Path.Combine (projectsDir, name) |> Path.Simplify
                let newFileName = name + ".fsproj"
                let newProj = Path.Combine (newProjDir, newFileName) |> Path.Simplify
                Console.WriteLine ("Creating project '" + name + "' in '" + projectsDir + "'...")

                // install nu template
                Directory.SetCurrentDirectory templateDir
                Process.Start("dotnet", "new -u \"" + templateIdentifier + "\"").WaitForExit()
                Process.Start("dotnet", "new -i ./").WaitForExit()

                // instantiate nu template
                Directory.SetCurrentDirectory projectsDir
                Directory.CreateDirectory name |> ignore<DirectoryInfo>
                Directory.SetCurrentDirectory newProjDir
                Process.Start("dotnet", "new nu-game --force").WaitForExit()

                // rename project file
                File.Copy (templateFileName, newFileName, true)
                File.Delete templateFileName

                // substitute $safeprojectname$ in project file
                let newProjStr = File.ReadAllText newProj
                let newProjStr = newProjStr.Replace("$safeprojectname$", name)
                File.WriteAllText (newProj, newProjStr)

                // add project to sln file
                // NOTE: not currently working due to project in old file format - user is instructed to do this manually
                //Directory.SetCurrentDirectory slnDir
                //Process.Start("dotnet", "sln add Nu.sln \"" + newProj + "\"").WaitForExit()
                ignore (slnDir, newProj)
                
                // success
                Constants.Engine.SuccessExitCode

            // invalid name; failure
            else
                Console.WriteLine ("Project name '" + name + "' contains invalid path characters.")
                Constants.Engine.FailureExitCode

        // rejected
        | _ -> Constants.Engine.SuccessExitCode
#else
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
#endif