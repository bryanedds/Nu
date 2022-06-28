// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu

/// Force qualification of OpenGL namespace in Nu unless opened explicitly.
[<RequireQualifiedAccess>]
module OpenGL = let _ = ()

namespace OpenGL
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Text
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    /// A texture's metadata.
    type TextureMetadata =
        { TextureWidth : int
          TextureHeight : int
          TextureTexelWidth : single
          TextureTexelHeight : single
          TextureInternalFormat : OpenGL.InternalFormat }

        /// Unpopulated texture data.
        static member empty =
            { TextureWidth = 0
              TextureHeight = 0
              TextureTexelWidth = 0.0f
              TextureTexelHeight = 0.0f
              TextureInternalFormat = Unchecked.defaultof<_> }

    /// Describes some physically-based geometry that's loaded into VRAM.
    type [<StructuralEquality; NoComparison>] PhysicallyBasedGeometry =
        { Bounds : Box3
          PrimitiveType : OpenGL.PrimitiveType
          ElementCount : int
          Vertices : Vector3 array
          VertexBuffer : uint
          ModelBuffer : uint
          IndexBuffer : uint
          PhysicallyBasedVao : uint }

    type [<CustomEquality; NoComparison; Struct>] PhysicallyBasedSurface =
        { mutable HashCode : int
          Transparent : bool
          AlbedoTexture : uint
          MetalnessTexture : uint
          RoughnessTexture : uint
          NormalTexture : uint
          AmbientOcclusionTexture : uint
          PhysicallyBasedGeometry : PhysicallyBasedGeometry }

        static member inline hash surface =
            hash surface.Transparent ^^^
            hash surface.AlbedoTexture * hash surface.MetalnessTexture * hash surface.RoughnessTexture * hash surface.NormalTexture * hash surface.AmbientOcclusionTexture ^^^
            hash surface.PhysicallyBasedGeometry

        static member inline make transparent albedoTexture metalnessTexture roughnessTexture normalTexture ambientOcclusionTexture geometry =
            let mutable result =
                { HashCode = 0
                  Transparent = transparent
                  AlbedoTexture = albedoTexture
                  MetalnessTexture = metalnessTexture
                  RoughnessTexture = roughnessTexture
                  NormalTexture = normalTexture
                  AmbientOcclusionTexture = ambientOcclusionTexture
                  PhysicallyBasedGeometry = geometry }
            result.HashCode <- PhysicallyBasedSurface.hash result
            result

        static member inline equals left right =
            left.HashCode = right.HashCode &&
            left.Transparent = right.Transparent &&
            left.AlbedoTexture = right.AlbedoTexture &&
            left.MetalnessTexture = right.MetalnessTexture &&
            left.RoughnessTexture = right.RoughnessTexture &&
            left.NormalTexture = right.NormalTexture &&
            left.AmbientOcclusionTexture = right.AmbientOcclusionTexture &&
            left.PhysicallyBasedGeometry.IndexBuffer = right.PhysicallyBasedGeometry.IndexBuffer &&
            left.PhysicallyBasedGeometry.VertexBuffer = right.PhysicallyBasedGeometry.VertexBuffer &&
            left.PhysicallyBasedGeometry.PhysicallyBasedVao = right.PhysicallyBasedGeometry.PhysicallyBasedVao

        member this.Equals that =
            PhysicallyBasedSurface.equals this that

        override this.Equals (thatObj : obj) =
            match thatObj with
            | :? PhysicallyBasedSurface as that -> PhysicallyBasedSurface.equals this that
            | _ -> false

        override this.GetHashCode () =
            PhysicallyBasedSurface.hash this

    /// A physically-based static model.
    type [<ReferenceEquality; NoComparison>] PhysicallyBasedStaticModel =
        { Bounds : Box3
          Surfaces : PhysicallyBasedSurface array }

    /// Describes a physically-based shader that's loaded into GPU.
    type [<StructuralEquality; NoComparison>] PhysicallyBasedShader =
        { ViewUniform : int
          ProjectionUniform : int
          EyePositionUniform : int
          AlbedoTextureUniform : int
          MetalnessTextureUniform : int
          RoughnessTextureUniform : int
          NormalTextureUniform : int
          AmbientOcclusionTextureUniform : int
          LightAmbientUniform : int
          LightPositionsUniform : int
          LightColorsUniform : int
          PhysicallyBasedShader : uint }

    /// Describes a second pass of a deferred physically-based shader that's loaded into GPU.
    type [<StructuralEquality; NoComparison>] PhysicallyBasedDeferred2Shader =
        { EyePositionUniform : int
          PositionTextureUniform : int
          NormalTextureUniform : int
          AlbedoTextureUniform : int
          MaterialTextureUniform : int
          LightAmbientUniform : int
          LightPositionsUniform : int
          LightColorsUniform : int
          PhysicallyBasedDeferred2Shader : uint }

    /// Assert a lack of Gl error. Has an generic parameter to enable value pass-through.
    let Assert (a : 'a) =
#if DEBUG_RENDERING_ASSERT
        let error = OpenGL.Gl.GetError ()
        if error <> OpenGL.ErrorCode.NoError then
            Log.debug ("OpenGL assertion failed due to: " + string error)
#endif
        a

#if DEBUG_RENDERING_MESSAGE
    let private DebugMessageListener (_ : OpenGL.DebugSource) (_ : OpenGL.DebugType) (_ : uint) (severity : OpenGL.DebugSeverity) (length : int) (message : nativeint) (_ : nativeint) =
        let messageBytes = Array.zeroCreate<byte> length
        Marshal.Copy (message, messageBytes, 0, length)
        let messageStr = Encoding.ASCII.GetString (messageBytes, 0, length)
        match severity with
        | OpenGL.DebugSeverity.DebugSeverityLow -> Log.info messageStr
        | OpenGL.DebugSeverity.DebugSeverityMedium
        | OpenGL.DebugSeverity.DebugSeverityHigh -> Log.debug messageStr
        | OpenGL.DebugSeverity.DebugSeverityNotification
        | OpenGL.DebugSeverity.DontCare
        | _ -> ()

    let DebugMessageProc =
        OpenGL.Gl.DebugProc DebugMessageListener

    /// Listen to the OpenGL error stream.
    let AttachDebugMessageCallback () =
        OpenGL.Gl.DebugMessageCallback (DebugMessageProc, nativeint 0)
#else
    /// Listen to the OpenGL error stream.
    let AttachDebugMessageCallback () =
        ()
#endif

    /// Create an SDL OpenGL context.
    let CreateSglContext window =
        OpenGL.Gl.Initialize ()
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_ACCELERATED_VISUAL, 1) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MAJOR_VERSION, Constants.Render.OpenGlVersionMajor) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MINOR_VERSION, Constants.Render.OpenGlVersionMinor) |> ignore<int>
        if Constants.Render.OpenGlCore then
            SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_PROFILE_MASK, SDL.SDL_GLprofile.SDL_GL_CONTEXT_PROFILE_CORE) |> ignore<int>
#if DEBUG_RENDERING_MESSAGE
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_FLAGS, int SDL.SDL_GLcontext.SDL_GL_CONTEXT_DEBUG_FLAG) |> ignore<int>
#endif
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DOUBLEBUFFER, 1) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DEPTH_SIZE, 24) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_STENCIL_SIZE, 8) |> ignore<int>
        let glContext = SDL.SDL_GL_CreateContext window
        SDL.SDL_GL_SetSwapInterval 1 |> ignore<int>
        OpenGL.Gl.BindAPI ()
        let version = OpenGL.Gl.GetString OpenGL.StringName.Version
        Log.info ("Initialized OpenGL " + version + ".")
        Assert ()
        glContext

    /// Begin an OpenGL frame.
    let BeginFrame (viewportOffset : Box2i) =
    
        // set viewport
        OpenGL.Gl.Viewport (viewportOffset.Position.X, viewportOffset.Position.Y, viewportOffset.Size.X, viewportOffset.Size.Y)
        Assert ()

        // bind buffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)
        Assert ()

        // clear inner viewport
        OpenGL.Gl.DepthMask true
        OpenGL.Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        OpenGL.Gl.DepthMask false
        Assert ()

        // clear drawing target
        OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
        OpenGL.Gl.Scissor (viewportOffset.Position.X, viewportOffset.Position.Y, viewportOffset.Size.X, viewportOffset.Size.Y)
        OpenGL.Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit)
        OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest

    /// End an OpenGL frame.
    let EndFrame () =
        () // nothing to do

    /// Vertically flip an SDL surface.
    let FlipSurface (surface : SDL.SDL_Surface inref) =
        let rowTop = Array.zeroCreate<byte> surface.pitch
        let rowBottom = Array.zeroCreate<byte> surface.pitch
        for i in 0 .. dec (surface.h / 2) do
            let offsetTop = i * surface.pitch
            let pixelsTop = surface.pixels + nativeint offsetTop
            let offsetBottom = (dec surface.h - i) * surface.pitch
            let pixelsBottom = surface.pixels + nativeint offsetBottom
            Marshal.Copy (pixelsTop, rowTop, 0, surface.pitch)
            Marshal.Copy (pixelsBottom, rowBottom, 0, surface.pitch)
            Marshal.Copy (rowTop, 0, pixelsBottom, surface.pitch)
            Marshal.Copy (rowBottom, 0, pixelsTop, surface.pitch)

    /// Attempt to create a 2d texture from a file.
    let TryCreateTexture2d (minFilter, magFilter, filePath : string) =

        // attempt to load the texture into an SDL surface, converting its format if needed
        let format = SDL.SDL_PIXELFORMAT_ABGR8888 // this is RGBA8888 on little-endian architectures
        let surfaceOpt =
            let unconvertedPtr = SDL_image.IMG_Load filePath
            if unconvertedPtr <> nativeint 0 then
                let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
                let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
                if unconvertedFormat.format <> format then
                    let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                    SDL.SDL_FreeSurface unconvertedPtr
                    let converted = Marshal.PtrToStructure<SDL.SDL_Surface> convertedPtr
                    Some (convertedPtr, converted)
                else Some (unconvertedPtr, unconverted)
            else None

        // ensure surface was loaded into
        match surfaceOpt with
        | Some (surfacePtr, surface) ->

            // upload the texture to gl
            let texture = OpenGL.Gl.GenTexture ()
            OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int minFilter)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int magFilter)
            let internalFormat = OpenGL.InternalFormat.Rgba8
            OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, internalFormat, surface.w, surface.h, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.UnsignedByte, surface.pixels)

            // teardown surface
            SDL.SDL_FreeSurface surfacePtr

            // check for errors
            match OpenGL.Gl.GetError () with
            | OpenGL.ErrorCode.NoError ->
                let metadata =
                    { TextureWidth = surface.w
                      TextureHeight = surface.h
                      TextureTexelWidth = 1.0f / single surface.w
                      TextureTexelHeight = 1.0f / single surface.h
                      TextureInternalFormat = internalFormat }
                Right (metadata, texture)
            | error -> Left (string error)

        // load error
        | None -> Left ("Missing file or unloadable image '" + filePath + "'.")

    /// Attempt to create a sprite texture.
    let TryCreateSpriteTexture filePath =
        TryCreateTexture2d (OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, filePath)

    /// Delete a texture.
    let DeleteTexture (texture : uint) =
        OpenGL.Gl.DeleteTextures texture

    /// Attempt to create physically-based from an Assimp mesh.
    let TryCreatePhysicallyBasedMesh (mesh : Assimp.Mesh) =

        // ensure required data is available
        if  mesh.HasVertices &&
            mesh.HasNormals &&
            mesh.HasTextureCoords 0 then
        
            // attempt to populate geometry data
            if mesh.Vertices.Count = mesh.Normals.Count && mesh.Vertices.Count = mesh.TextureCoordinateChannels.[0].Count then

                // populate vertex data and bounds
                let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 8)
                let mutable positionMin = v3Zero
                let mutable positionMax = v3Zero
                for i in 0 .. dec mesh.Vertices.Count do
                    let v = i * 8
                    let position = mesh.Vertices.[i]
                    let normal = mesh.Normals.[i]
                    let texCoords = mesh.TextureCoordinateChannels.[0].[i]
                    vertexData.[v] <- position.X
                    vertexData.[v+1] <- position.Y
                    vertexData.[v+2] <- position.Z
                    vertexData.[v+3] <- normal.X
                    vertexData.[v+4] <- normal.Y
                    vertexData.[v+5] <- normal.Z
                    vertexData.[v+6] <- texCoords.X
                    vertexData.[v+7] <- 1.0f - texCoords.Y
                    positionMin.X <- min positionMin.X position.X
                    positionMin.Y <- min positionMin.Y position.Y
                    positionMin.Z <- min positionMin.Z position.Z
                    positionMax.X <- max positionMax.X position.X
                    positionMax.Y <- max positionMax.Y position.Y
                    positionMax.Z <- max positionMax.Z position.Z
                let bounds = box3 positionMin (positionMax - positionMin)

                // populate triangle index data
                let indexList = SegmentedList.make ()
                for face in mesh.Faces do
                    let indices = face.Indices
                    if indices.Count = 3 then
                        // NOTE: .obj files exported from blender seem to have reverse winding order, so we add indices in reverse
                        SegmentedList.add indices.[2] indexList
                        SegmentedList.add indices.[1] indexList
                        SegmentedList.add indices.[0] indexList
                let indexData = Seq.toArray indexList

                // fin
                Right (vertexData, indexData, bounds)
                        
            // error
            else Left ("Vertex / normal / tex coords count mismatch.")

        // error
        else Left "Mesh is missing vertices, normals, or texCoords."

    /// Create a mesh for a physically-based quad.
    let CreatePhysicallyBasedQuadMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)       (*    normals    *)         (* tex coords *)
                -1.0f; -1.0f; +0.0f;       0.0f;  0.0f; +1.0f;        0.0f; 0.0f; // bottom-left
                +1.0f; -1.0f; +0.0f;       0.0f;  0.0f; +1.0f;        1.0f; 0.0f; // bottom-right
                +1.0f; +1.0f; +0.0f;       0.0f;  0.0f; +1.0f;        1.0f; 1.0f; // top-right
                +1.0f; +1.0f; +0.0f;       0.0f;  0.0f; +1.0f;        1.0f; 1.0f; // top-right
                -1.0f; +1.0f; +0.0f;       0.0f;  0.0f; +1.0f;        0.0f; 1.0f; // top-left
                -1.0f; -1.0f; +0.0f;       0.0f;  0.0f; +1.0f;        0.0f; 0.0f; // bottom-left
            |]

        // make index data trivially
        let indexData = Array.init 6 id

        // make bounds trivially
        let bounds = box3 (v3 -1.0f -1.0f 0.0f) (v3 2.0f 2.0f 0.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create a mesh for a physically-based cube.
    let CreatePhysicallyBasedCubeMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)       (*    normals    *)         (* tex coords *)

                // back face
                -0.5f; -0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        0.0f; 0.0f; // bottom-left
                +0.5f; +0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        1.0f; 1.0f; // top-right
                +0.5f; -0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        1.0f; 0.0f; // bottom-right         
                +0.5f; +0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        1.0f; 1.0f; // top-right
                -0.5f; -0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        0.0f; 0.0f; // bottom-left
                -0.5f; +0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        0.0f; 1.0f; // top-left

                // front face
                -0.5f; -0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        0.0f; 0.0f; // bottom-left
                +0.5f; -0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        1.0f; 0.0f; // bottom-right
                +0.5f; +0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        1.0f; 1.0f; // top-right
                +0.5f; +0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        1.0f; 1.0f; // top-right
                -0.5f; +0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        0.0f; 1.0f; // top-left
                -0.5f; -0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        0.0f; 0.0f; // bottom-left

                // left face
                -0.5f; +0.5f; +0.5f;      -1.0f;  0.0f;  0.0f;        1.0f; 0.0f; // top-right
                -0.5f; +0.5f; -0.5f;      -1.0f;  0.0f;  0.0f;        1.0f; 1.0f; // top-left
                -0.5f; -0.5f; -0.5f;      -1.0f;  0.0f;  0.0f;        0.0f; 1.0f; // bottom-left
                -0.5f; -0.5f; -0.5f;      -1.0f;  0.0f;  0.0f;        0.0f; 1.0f; // bottom-left
                -0.5f; -0.5f; +0.5f;      -1.0f;  0.0f;  0.0f;        0.0f; 0.0f; // bottom-right
                -0.5f; +0.5f; +0.5f;      -1.0f;  0.0f;  0.0f;        1.0f; 0.0f; // top-right

                // right face
                +0.5f; +0.5f; +0.5f;      +1.0f;  0.0f;  0.0f;        1.0f; 0.0f; // top-left
                +0.5f; -0.5f; -0.5f;      +1.0f;  0.0f;  0.0f;        0.0f; 1.0f; // bottom-right
                +0.5f; +0.5f; -0.5f;      +1.0f;  0.0f;  0.0f;        1.0f; 1.0f; // top-right         
                +0.5f; -0.5f; -0.5f;      +1.0f;  0.0f;  0.0f;        0.0f; 1.0f; // bottom-right
                +0.5f; +0.5f; +0.5f;      +1.0f;  0.0f;  0.0f;        1.0f; 0.0f; // top-left
                +0.5f; -0.5f; +0.5f;      +1.0f;  0.0f;  0.0f;        0.0f; 0.0f; // bottom-left     

                // bottom face
                -0.5f; -0.5f; -0.5f;       0.0f; -1.0f;  0.0f;        0.0f; 1.0f; // top-right
                +0.5f; -0.5f; -0.5f;       0.0f; -1.0f;  0.0f;        1.0f; 1.0f; // top-left
                +0.5f; -0.5f; +0.5f;       0.0f; -1.0f;  0.0f;        1.0f; 0.0f; // bottom-left
                +0.5f; -0.5f; +0.5f;       0.0f; -1.0f;  0.0f;        1.0f; 0.0f; // bottom-left
                -0.5f; -0.5f; +0.5f;       0.0f; -1.0f;  0.0f;        0.0f; 0.0f; // bottom-right
                -0.5f; -0.5f; -0.5f;       0.0f; -1.0f;  0.0f;        0.0f; 1.0f; // top-right

                // top face
                -0.5f; +0.5f; -0.5f;       0.0f; +1.0f;  0.0f;        0.0f; 1.0f; // top-left
                +0.5f; +0.5f ;+0.5f;       0.0f; +1.0f;  0.0f;        1.0f; 0.0f; // bottom-right
                +0.5f; +0.5f; -0.5f;       0.0f; +1.0f;  0.0f;        1.0f; 1.0f; // top-right     
                +0.5f; +0.5f; +0.5f;       0.0f; +1.0f;  0.0f;        1.0f; 0.0f; // bottom-right
                -0.5f; +0.5f; -0.5f;       0.0f; +1.0f;  0.0f;        0.0f; 1.0f; // top-left
                -0.5f; +0.5f; +0.5f;       0.0f; +1.0f;  0.0f;        0.0f; 0.0f  // bottom-left     
            |]

        // make index data trivially
        let indexData = Array.init 36 id

        // make bounds trivially
        let bounds = box3 (v3Dup -0.5f) v3One

        // fin
        (vertexData, indexData, bounds)

    /// Create physically-based geometry from a mesh.
    let CreatePhysicallyBasedGeometry (renderable, vertexData : single array, indexData : int array, bounds) =

        // make buffers
        let (vertices, vertexBuffer, modelBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = OpenGL.Gl.GenVertexArray ()
                OpenGL.Gl.BindVertexArray vao
                Assert ()

                // create vertex buffer
                let vertexBuffer = OpenGL.Gl.GenBuffer ()
                let normalOffset =      (3 (*position*)) * sizeof<single>
                let texCoordsOffset =   (3 (*position*) + 3 (*normal*)) * sizeof<single>
                let vertexSize =        (3 (*position*) + 3 (*normal*) + 2 (*texCoords*)) * sizeof<single>
                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
                let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
                try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
                finally vertexDataPtr.Free ()
                OpenGL.Gl.EnableVertexAttribArray 0u
                OpenGL.Gl.VertexAttribPointer (0u, 3, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint 0)
                OpenGL.Gl.EnableVertexAttribArray 1u
                OpenGL.Gl.VertexAttribPointer (1u, 3, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint normalOffset)
                OpenGL.Gl.EnableVertexAttribArray 2u
                OpenGL.Gl.VertexAttribPointer (2u, 2, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint texCoordsOffset)
                Assert ()

                // create model buffer
                let modelBuffer = OpenGL.Gl.GenBuffer ()
                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, modelBuffer)
                let modelDataPtr = GCHandle.Alloc (m4Identity.ToArray (), GCHandleType.Pinned)
                try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (16 * sizeof<single>), modelDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StreamDraw)
                finally modelDataPtr.Free ()
                OpenGL.Gl.EnableVertexAttribArray 3u
                OpenGL.Gl.VertexAttribPointer (3u, 4, OpenGL.VertexAttribType.Float, false, 16 * sizeof<single>, nativeint 0)
                OpenGL.Gl.VertexAttribDivisor (3u, 1u)
                OpenGL.Gl.EnableVertexAttribArray 4u
                OpenGL.Gl.VertexAttribPointer (4u, 4, OpenGL.VertexAttribType.Float, false, 16 * sizeof<single>, nativeint (4 * sizeof<single>))
                OpenGL.Gl.VertexAttribDivisor (4u, 1u)
                OpenGL.Gl.EnableVertexAttribArray 5u
                OpenGL.Gl.VertexAttribPointer (5u, 4, OpenGL.VertexAttribType.Float, false, 16 * sizeof<single>, nativeint (8 * sizeof<single>))
                OpenGL.Gl.VertexAttribDivisor (5u, 1u)
                OpenGL.Gl.EnableVertexAttribArray 6u
                OpenGL.Gl.VertexAttribPointer (6u, 4, OpenGL.VertexAttribType.Float, false, 16 * sizeof<single>, nativeint (12 * sizeof<single>))
                OpenGL.Gl.VertexAttribDivisor (6u, 1u)
                Assert ()

                // create index buffer
                let indexBuffer = OpenGL.Gl.GenBuffer ()
                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
                try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
                finally indexDataPtr.Free ()
                Assert ()

                // finalize vao
                OpenGL.Gl.BindVertexArray 0u
                Assert ()

                // fin
                ([||], vertexBuffer, modelBuffer, indexBuffer, vao)

            // fake buffers
            else
                    
                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 3)
                for i in 0 .. dec vertices.Length do
                    let j = i * 3
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex
                    
                // fin
                (vertices, 0u, 0u, 0u, 0u)

        // make physically based geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = OpenGL.PrimitiveType.Triangles
              ElementCount = indexData.Length
              Vertices = vertices
              VertexBuffer = vertexBuffer
              ModelBuffer = modelBuffer
              IndexBuffer = indexBuffer
              PhysicallyBasedVao = vao }

        // fin
        geometry

    /// Attempt to create physically-based geometry from an assimp mesh.
    let TryCreatePhysicallyBasedGeometry (renderable, mesh : Assimp.Mesh) =
        let meshOpt =
#if DEBUG_RENDERING_CUBE
            Right CreatePhysicallyBasedCubeMesh ()
#else
            TryCreatePhysicallyBasedMesh mesh
#endif
        match meshOpt with
        | Right (vertexData, indexData, bounds) -> Right (CreatePhysicallyBasedGeometry (renderable, vertexData, indexData, bounds))
        | Left error -> Left error

    /// Create physically-based quad.
    let CreatePhysicallyBasedQuad renderable =
        let (vertexData, indexData, bounds) = CreatePhysicallyBasedQuadMesh ()
        CreatePhysicallyBasedGeometry (renderable, vertexData, indexData, bounds)

    /// Create physically-based cube.
    let CreatePhysicallyBasedCube renderable =
        let (vertexData, indexData, bounds) = CreatePhysicallyBasedCubeMesh ()
        CreatePhysicallyBasedGeometry (renderable, vertexData, indexData, bounds)

    /// Attempt to create physically-based material from an assimp mesh.
    let TryCreatePhysicallyBasedMaterial (dirPath : string, material : Assimp.Material) =
        if  material.HasTextureDiffuse && // mapped to albedo
            material.HasTextureSpecular && // mapped to metalness
            material.HasTextureHeight && // mapped to roughness
            material.HasTextureNormal && // mapped to normal
            material.HasTextureAmbient then // mapped to ambient occlusion
            let (_, albedo) = material.GetMaterialTexture (Assimp.TextureType.Diffuse, 0)
            let (_, metalness) = material.GetMaterialTexture (Assimp.TextureType.Specular, 0)
            let (_, roughness) = material.GetMaterialTexture (Assimp.TextureType.Height, 0)
            let (_, normal) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
            let (_, ambientOcclusion) = material.GetMaterialTexture (Assimp.TextureType.Ambient, 0)
            let albedoFilePath = Path.Combine (dirPath, albedo.FilePath)
            let metalnessFilePath = Path.Combine (dirPath, metalness.FilePath)
            let roughnessFilePath = Path.Combine (dirPath, roughness.FilePath)
            let normalFilePath = Path.Combine (dirPath, normal.FilePath)
            let ambientOcclusionFilePath = Path.Combine (dirPath, ambientOcclusion.FilePath)
            match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, albedoFilePath) with
            | Right albedoTexture ->
                match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, metalnessFilePath) with
                | Right metalnessTexture ->
                    match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, roughnessFilePath) with
                    | Right roughnessTexture ->
                        match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, normalFilePath) with
                        | Right normalTexture ->
                            match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, ambientOcclusionFilePath) with
                            | Right ambientOcclusionTexture -> Right (albedoTexture, metalnessTexture, roughnessTexture, normalTexture, ambientOcclusionTexture)
                            | Left error -> Left ("Could not load texture '" + ambientOcclusionFilePath + "' due to '" + error + "'.")
                        | Left error -> Left ("Could not load texture '" + normalFilePath + "' due to '" + error + "'.")
                    | Left error -> Left ("Could not load texture '" + roughnessFilePath + "' due to '" + error + "'.")
                | Left error -> Left ("Could not load texture '" + metalnessFilePath + "' due to '" + error + "'.")
            | Left error -> Left ("Could not load texture '" + albedoFilePath + "' due to '" + error + "'.")
        else Left ("Could not create physically-based material due to missing diffuse/albedo, metalness, roughness, normal, or ambientOcclusion texture.")

    /// Attempt to create physically-based material from an assimp scene.
    let TryCreatePhysicallyBasedMaterials (dirPath : string, scene : Assimp.Scene) =
        let mutable errorOpt = None
        let materials = dictPlus<int, _> HashIdentity.Structural []
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                match TryCreatePhysicallyBasedMaterial (dirPath, scene.Materials.[i]) with
                | Right material -> materials.Add (i, material)
                | Left error -> errorOpt <- Some error
        match errorOpt with
        | Some error -> Left error
        | None -> Right materials

    let TryCreatePhysicallyBasedStaticModel (renderable, filePath, assimp : Assimp.AssimpContext) =
        try let scene = assimp.ImportFile (filePath, Assimp.PostProcessSteps.CalculateTangentSpace ||| Assimp.PostProcessSteps.JoinIdenticalVertices ||| Assimp.PostProcessSteps.Triangulate ||| Assimp.PostProcessSteps.GenerateSmoothNormals ||| Assimp.PostProcessSteps.SplitLargeMeshes ||| Assimp.PostProcessSteps.LimitBoneWeights ||| Assimp.PostProcessSteps.RemoveRedundantMaterials ||| Assimp.PostProcessSteps.SortByPrimitiveType ||| Assimp.PostProcessSteps.FindDegenerates ||| Assimp.PostProcessSteps.FindInvalidData ||| Assimp.PostProcessSteps.GenerateUVCoords ||| Assimp.PostProcessSteps.FlipWindingOrder)
            let dirPath = Path.GetDirectoryName filePath
            let materialsOpt =
                if renderable
                then TryCreatePhysicallyBasedMaterials (dirPath, scene)
                else Right (dictPlus HashIdentity.Structural [])
            match materialsOpt with
            | Right materials ->
                let mutable errorOpt = None
                let model = SegmentedList.make ()
                let mutable bounds = box3Zero
                for mesh in scene.Meshes do
                    if Option.isNone errorOpt then
                        match TryCreatePhysicallyBasedGeometry (renderable, mesh) with
                        | Right geometry ->
                            let materialOpt =
                                if renderable
                                then materials.TryGetValue mesh.MaterialIndex
                                else (true, ((TextureMetadata.empty, 0u), (TextureMetadata.empty, 0u), (TextureMetadata.empty, 0u), (TextureMetadata.empty, 0u), (TextureMetadata.empty, 0u)))
                            match materialOpt with
                            | (true, (albedoTexture, metalnessTexture, roughnessTexture, normalTexture, ambientOcclusionTexture)) ->
                                let surface =
                                    PhysicallyBasedSurface.make
                                        false // TODO: 3D: deal with transparencies.
                                        (snd albedoTexture)
                                        (snd metalnessTexture)
                                        (snd roughnessTexture)
                                        (snd normalTexture)
                                        (snd ambientOcclusionTexture)
                                        geometry
                                SegmentedList.add surface model
                                bounds <- bounds.Combine geometry.Bounds
                            | (false, _) -> errorOpt <- Some ("Could not locate associated materials for mesh in file name '" + filePath + "'.")
                        | Left error -> errorOpt <- Some ("Could not load geometry for mesh in file name '" + filePath + "' due to: " + error)
                match errorOpt with
                | None -> Right { Bounds = bounds; Surfaces = Array.ofSeq model }
                | Some error -> Left error
            | Left error -> Left ("Could not load materials for static model in file name '" + filePath + "' due to: " + error)
        with exn -> Left ("Could not load static model '" + filePath + "' due to: " + scstring exn)

    /// Create a texture frame buffer.
    let CreateTextureFramebuffer () =

        // create frame buffer object
        let framebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        Assert ()

        // create texture buffer
        let texture = OpenGL.Gl.GenTexture ()
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.InternalFormat.Rgba32f, Constants.Render.ResolutionX, Constants.Render.ResolutionY, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.Float, nativeint 0)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
        OpenGL.Gl.FramebufferTexture2D (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, texture, 0)
        Assert ()

        // create depth and stencil buffers
        let depthStencilBuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.Depth32fStencil8, Constants.Render.ResolutionX, Constants.Render.ResolutionY)
        OpenGL.Gl.FramebufferRenderbuffer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.DepthStencilAttachment, OpenGL.RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        Assert ()

        // fin
        (texture, framebuffer)

    /// Create a geometry frame buffer.
    let TryCreateGeometryFramebuffer () =

        // create frame buffer object
        let framebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        Assert ()

        // create position buffer
        let position = OpenGL.Gl.GenTexture ()
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, position)
        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.InternalFormat.Rgba32f, Constants.Render.ResolutionX, Constants.Render.ResolutionY, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.Float, nativeint 0)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
        OpenGL.Gl.FramebufferTexture2D (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, TextureTarget.Texture2d, position, 0)
        Assert ()

        // create normal buffer
        let normal = OpenGL.Gl.GenTexture ()
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, normal)
        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.InternalFormat.Rgba32f, Constants.Render.ResolutionX, Constants.Render.ResolutionY, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.Float, nativeint 0)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
        OpenGL.Gl.FramebufferTexture2D (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment1, TextureTarget.Texture2d, normal, 0)
        Assert ()

        // create albedo buffer
        let albedo = OpenGL.Gl.GenTexture ()
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, albedo)
        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.InternalFormat.Rgba32f, Constants.Render.ResolutionX, Constants.Render.ResolutionY, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.Float, nativeint 0)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
        OpenGL.Gl.FramebufferTexture2D (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment2, TextureTarget.Texture2d, albedo, 0)
        Assert ()

        // create material buffer
        let material = OpenGL.Gl.GenTexture ()
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, material)
        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.InternalFormat.Rgba32f, Constants.Render.ResolutionX, Constants.Render.ResolutionY, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.Float, nativeint 0)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
        OpenGL.Gl.FramebufferTexture2D (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment3, TextureTarget.Texture2d, material, 0)
        Assert ()

        // associate draw buffers
        OpenGL.Gl.DrawBuffers
            [|int OpenGL.FramebufferAttachment.ColorAttachment0
              int OpenGL.FramebufferAttachment.ColorAttachment1
              int OpenGL.FramebufferAttachment.ColorAttachment2
              int OpenGL.FramebufferAttachment.ColorAttachment3|]

        // create depth and stencil buffers
        let depthStencilBuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.Depth32fStencil8, Constants.Render.ResolutionX, Constants.Render.ResolutionY)
        OpenGL.Gl.FramebufferRenderbuffer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.DepthStencilAttachment, OpenGL.RenderbufferTarget.Renderbuffer, depthStencilBuffer)
        Assert ()
        
        // ensure framebuffer is complete
        if OpenGL.Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer = FramebufferStatus.FramebufferComplete
        then Right (position, normal, albedo, material, framebuffer)
        else Left ("Could not create complete geometry framebuffer.")

    /// Create a shader from vertex and fragment code strings.
    let CreateShaderFromStrs (vertexShaderStr, fragmentShaderStr) =

        // construct gl shader
        let shader = OpenGL.Gl.CreateProgram ()
        Assert ()

        // add vertex shader
        let vertexShader = OpenGL.Gl.CreateShader OpenGL.ShaderType.VertexShader
        OpenGL.Gl.ShaderSource (vertexShader, [|vertexShaderStr|], null)
        OpenGL.Gl.CompileShader vertexShader
        OpenGL.Gl.AttachShader (shader, vertexShader)
        Assert ()

        // add fragement shader to program
        let fragmentShader = OpenGL.Gl.CreateShader OpenGL.ShaderType.FragmentShader
        OpenGL.Gl.ShaderSource (fragmentShader, [|fragmentShaderStr|], null)
        OpenGL.Gl.CompileShader fragmentShader
        OpenGL.Gl.AttachShader (shader, fragmentShader)
        Assert ()

        // link shader
        OpenGL.Gl.LinkProgram shader
        shader

    /// Create a shader from a vertex stream and a fragment stream.
    let CreateShaderFromStreams (vertexStream : StreamReader, fragmentStream : StreamReader) =
        let vertexStr = vertexStream.ReadToEnd ()
        let fragmentStr = fragmentStream.ReadToEnd ()
        CreateShaderFromStrs (vertexStr, fragmentStr)

    /// Create a shader from a vertex file and a fragment file.
    let CreateShaderFromFilePaths (vertexFilePath : string, fragmentFilePath : string) =
        use vertexStream = new StreamReader (File.OpenRead vertexFilePath)
        use fragmentStream = new StreamReader (File.OpenRead fragmentFilePath)
        CreateShaderFromStreams (vertexStream, fragmentStream)

    /// Create a shader from a single file with both a '#shader vertex' section and a '#shader fragment' section.
    let CreateShaderFromFilePath (shaderFilePath : string) =
        use shaderStream = new StreamReader (File.OpenRead shaderFilePath)
        let shaderStr = shaderStream.ReadToEnd ()
        let vertexStrIndex = shaderStr.IndexOf "#shader vertex"
        let fragmentStrIndex = shaderStr.IndexOf "#shader fragment"
        if vertexStrIndex > -1 && fragmentStrIndex > -1 then
            let (vertexStr, fragmentStr) =
                if vertexStrIndex < fragmentStrIndex then
                    (shaderStr.Substring (vertexStrIndex, fragmentStrIndex - vertexStrIndex),
                     shaderStr.Substring (fragmentStrIndex, shaderStr.Length - fragmentStrIndex))
                else
                    (shaderStr.Substring (fragmentStrIndex, vertexStrIndex - fragmentStrIndex),
                     shaderStr.Substring (vertexStrIndex, shaderStr.Length - vertexStrIndex))
            CreateShaderFromStrs (vertexStr.Replace ("#shader vertex", ""), fragmentStr.Replace ("#shader fragment", ""))
        else failwith ("Invalid shader file '" + shaderFilePath + "'. Both vertex and fragment shader sections required.")

    /// Create a sprite shader with attributes:
    ///     0: vec2 position
    /// and uniforms:
    ///     a: mat4 modelViewProjection
    ///     b: vec2 texCoords4
    ///     c: vec4 color
    ///     d: sampler2D tex
    let CreateSpriteShader () =

        // vertex shader code
        let vertexShaderStr =
            [Constants.Render.GlslVersionPragma
             "#define VERTS 4"
             ""
             "const vec4 filters[VERTS] ="
             "  vec4[4]("
             "      vec4(1,1,0,0),"
             "      vec4(1,1,1,0),"
             "      vec4(1,1,1,1),"
             "      vec4(1,1,0,1));"
             ""
             "in vec2 position;"
             "uniform mat4 modelViewProjection;"
             "uniform vec4 texCoords4;"
             "out vec2 texCoords;"
             "void main()"
             "{"
             "  int vertexId = gl_VertexID % VERTS;"
             "  vec4 filt = filters[vertexId];"
             "  gl_Position = modelViewProjection * vec4(position.x, position.y, 0, 1);"
             "  texCoords = vec2(texCoords4.x * filt.x + texCoords4.z * filt.z, texCoords4.y * filt.y + texCoords4.w * filt.w);"
             "}"] |> String.join "\n"

        // fragment shader code
        let fragmentShaderStr =
            [Constants.Render.GlslVersionPragma
             "uniform sampler2D tex;"
             "uniform vec4 color;"
             "in vec2 texCoords;"
             "out vec4 frag;"
             "void main()"
             "{"
             "  frag = color * texture(tex, texCoords);"
             "}"] |> String.join "\n"

        // create shader
        let shader = CreateShaderFromStrs (vertexShaderStr, fragmentShaderStr)
        let modelViewProjectionUniform = OpenGL.Gl.GetUniformLocation (shader, "modelViewProjection")
        let texCoords4Uniform = OpenGL.Gl.GetUniformLocation (shader, "texCoords4")
        let colorUniform = OpenGL.Gl.GetUniformLocation (shader, "color")
        let texUniform = OpenGL.Gl.GetUniformLocation (shader, "tex")
        (modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader)

    let CreatePhysicallyBasedShader (shaderFilePath : string) =

        // create shader
        let shader = CreateShaderFromFilePath shaderFilePath

        // retrieve uniforms
        let viewUniform = OpenGL.Gl.GetUniformLocation (shader, "view")
        let projectionUniform = OpenGL.Gl.GetUniformLocation (shader, "projection")
        let eyePositionUniform = OpenGL.Gl.GetUniformLocation (shader, "eyePosition")
        let albedoTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "albedoTexture")
        let metalnessTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "metalnessTexture")
        let roughnessTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "roughnessTexture")
        let normalTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "normalTexture")
        let ambientOcclusionTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "ambientOcclusionTexture")
        let lightAmbientUniform = OpenGL.Gl.GetUniformLocation (shader, "lightAmbient")
        let lightPositionsUniform = OpenGL.Gl.GetUniformLocation (shader, "lightPositions")
        let lightColorsUniform = OpenGL.Gl.GetUniformLocation (shader, "lightColors")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          EyePositionUniform = eyePositionUniform
          AlbedoTextureUniform = albedoTextureUniform
          MetalnessTextureUniform = metalnessTextureUniform
          RoughnessTextureUniform = roughnessTextureUniform
          NormalTextureUniform = normalTextureUniform
          AmbientOcclusionTextureUniform = ambientOcclusionTextureUniform
          LightAmbientUniform = lightAmbientUniform
          LightPositionsUniform = lightPositionsUniform
          LightColorsUniform = lightColorsUniform
          PhysicallyBasedShader = shader }

    let CreatePhysicallyBasedDeferred2Shader (shaderFilePath : string) =

        // create shader
        let shader = CreateShaderFromFilePath shaderFilePath

        // retrieve uniforms
        let eyePositionUniform = OpenGL.Gl.GetUniformLocation (shader, "eyePosition")
        let positionTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "positionTexture")
        let normalTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "normalTexture")
        let albedoTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "albedoTexture")
        let materialTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "materialTexture")
        let lightAmbientUniform = OpenGL.Gl.GetUniformLocation (shader, "lightAmbient")
        let lightPositionsUniform = OpenGL.Gl.GetUniformLocation (shader, "lightPositions")
        let lightColorsUniform = OpenGL.Gl.GetUniformLocation (shader, "lightColors")

        // make shader record
        { EyePositionUniform = eyePositionUniform
          PositionTextureUniform = positionTextureUniform
          NormalTextureUniform = normalTextureUniform
          AlbedoTextureUniform = albedoTextureUniform
          MaterialTextureUniform = materialTextureUniform
          LightAmbientUniform = lightAmbientUniform
          LightPositionsUniform = lightPositionsUniform
          LightColorsUniform = lightColorsUniform
          PhysicallyBasedDeferred2Shader = shader }

    let CreatePhysicallyBasedDeferredShaders (shaderFilePath, shader2FilePath) =
        let shader = CreatePhysicallyBasedShader shaderFilePath // deferred shader 1 uses the same API as physically based shader
        let shader2 = CreatePhysicallyBasedDeferred2Shader shader2FilePath
        (shader, shader2)

    /// Create a sprite quad for rendering to a shader matching the one created with OpenGL.Hl.CreateSpriteShader.
    let CreateSpriteQuad onlyUpperRightQuadrant =

        // build vertex data
        let vertexData =
            if onlyUpperRightQuadrant then
                [|+0.0f; +0.0f
                  +1.0f; +0.0f
                  +1.0f; +1.0f
                  +0.0f; +1.0f|]
            else
                [|-1.0f; -1.0f
                  +1.0f; -1.0f
                  +1.0f; +1.0f
                  -1.0f; +1.0f|]

        // initialize vao
        let vao = OpenGL.Gl.GenVertexArray ()
        OpenGL.Gl.BindVertexArray vao
        Assert ()

        // create vertex buffer
        let vertexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
        let vertexSize = sizeof<single> * 2
        let vertexDataSize = vertexSize * 4
        let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
        try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint vertexDataSize, vertexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
        finally vertexDataPtr.Free ()
        Assert ()

        // create index buffer
        let indexData = [|0u; 1u; 2u; 2u; 3u; 0u|]
        let indexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
        let indexDataSize = uint (indexData.Length * sizeof<uint>)
        let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
        try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
        finally indexDataPtr.Free ()
        Assert ()

        // finalize vao
        OpenGL.Gl.EnableVertexAttribArray 0u
        OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint 0)
        OpenGL.Gl.BindVertexArray 0u
        Assert ()

        // fin
        (vertexBuffer, indexBuffer, vao)

    /// Draw a sprite whose indices and vertices were created by OpenGL.Gl.CreateSpriteQuad and whose uniforms and shader match those of OpenGL.CreateSpriteShader.
    let DrawSprite (vertices, indices, vao, modelViewProjection : single array, insetOpt : Box2 ValueOption, color : Color, flip, textureWidth, textureHeight, texture, modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader) =

        // compute unflipped tex coords
        let texCoordsUnflipped =
            match insetOpt with
            | ValueSome inset ->
                let texelWidth = 1.0f / single textureWidth
                let texelHeight = 1.0f / single textureHeight
                let borderWidth = texelWidth * Constants.Render.SpriteBorderTexelScalar
                let borderHeight = texelHeight * Constants.Render.SpriteBorderTexelScalar
                let px = inset.Position.X * texelWidth + borderWidth
                let py = (inset.Position.Y + inset.Size.Y) * texelHeight - borderHeight
                let sx = inset.Size.X * texelWidth - borderWidth * 2.0f
                let sy = -inset.Size.Y * texelHeight + borderHeight * 2.0f
                Box2 (px, py, sx, sy)
            | ValueNone -> Box2 (0.0f, 1.0f, 1.0f, -1.0f)
            
        // compute a flipping flags
        let struct (flipH, flipV) =
            match flip with
            | FlipNone -> struct (false, false)
            | FlipH -> struct (true, false)
            | FlipV -> struct (false, true)
            | FlipHV -> struct (true, true)

        // compute tex coords
        let texCoords =
            box2
                (v2
                    (if flipH then texCoordsUnflipped.Position.X + texCoordsUnflipped.Size.X else texCoordsUnflipped.Position.X)
                    (if flipV then texCoordsUnflipped.Position.Y + texCoordsUnflipped.Size.Y else texCoordsUnflipped.Position.Y))
                (v2
                    (if flipH then -texCoordsUnflipped.Size.X else texCoordsUnflipped.Size.X)
                    (if flipV then -texCoordsUnflipped.Size.Y else texCoordsUnflipped.Size.Y))

        // setup state
        OpenGL.Gl.Enable OpenGL.EnableCap.Blend
        OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
        Assert ()

        // setup shader
        OpenGL.Gl.UseProgram shader
        OpenGL.Gl.UniformMatrix4 (modelViewProjectionUniform, false, modelViewProjection)
        OpenGL.Gl.Uniform4 (texCoords4Uniform, texCoords.Position.X, texCoords.Position.Y, texCoords.Size.X, texCoords.Size.Y)
        OpenGL.Gl.Uniform4 (colorUniform, color.R, color.G, color.B, color.A)
        OpenGL.Gl.Uniform1 (texUniform, 0)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
        OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
        OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha)
        Assert ()

        // setup geometry
        OpenGL.Gl.BindVertexArray vao
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertices)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indices)
        Assert ()

        // draw geometry
        OpenGL.Gl.DrawElements (OpenGL.PrimitiveType.Triangles, 6, OpenGL.DrawElementsType.UnsignedInt, nativeint 0)
        Assert ()

        // teardown geometry
        OpenGL.Gl.BindVertexArray 0u
        Assert ()

        // teardown shader
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.UseProgram 0u
        Assert ()

        // teardown state
        OpenGL.Gl.Disable OpenGL.EnableCap.CullFace
        OpenGL.Gl.Disable OpenGL.EnableCap.Blend

    /// Draw a batch of physically-based surfaces.
    let DrawPhysicallyBasedSurfaces
        (eyePosition : Vector3,
         modelsFields : single array,
         modelsCount : int,
         view : single array,
         projection : single array,
         albedoTexture : uint,
         metalnessTexture : uint,
         roughnessTexture : uint,
         normalTexture : uint,
         ambientOcclusionTexture : uint,
         lightAmbient : single,
         lightPositions : single array,
         lightColors : single array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedShader) =

        // setup state
        OpenGL.Gl.DepthMask true
        OpenGL.Gl.DepthFunc DepthFunction.Lequal
        OpenGL.Gl.Enable OpenGL.EnableCap.DepthTest
        OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
        Assert ()

        // setup shader
        OpenGL.Gl.UseProgram shader.PhysicallyBasedShader
        OpenGL.Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        OpenGL.Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        OpenGL.Gl.Uniform3 (shader.EyePositionUniform, eyePosition.X, eyePosition.Y, eyePosition.Z)
        OpenGL.Gl.Uniform1 (shader.AlbedoTextureUniform, 0)
        OpenGL.Gl.Uniform1 (shader.MetalnessTextureUniform, 1)
        OpenGL.Gl.Uniform1 (shader.RoughnessTextureUniform, 2)
        OpenGL.Gl.Uniform1 (shader.NormalTextureUniform, 3)
        OpenGL.Gl.Uniform1 (shader.AmbientOcclusionTextureUniform, 4)
        OpenGL.Gl.Uniform1 (shader.LightAmbientUniform, lightAmbient)
        OpenGL.Gl.Uniform3 (shader.LightPositionsUniform, lightPositions)
        OpenGL.Gl.Uniform3 (shader.LightColorsUniform, lightColors)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, albedoTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture1
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, metalnessTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture2
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, roughnessTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture3
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, normalTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture4
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, ambientOcclusionTexture)
        Assert ()

        // update models buffer
        let modelsFieldsPtr = GCHandle.Alloc (modelsFields, GCHandleType.Pinned)
        try OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, geometry.ModelBuffer)
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (modelsCount * 16 * sizeof<single>), modelsFieldsPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.DynamicDraw)
        finally modelsFieldsPtr.Free ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
        Assert ()

        // setup geometry
        OpenGL.Gl.BindVertexArray geometry.PhysicallyBasedVao
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Assert ()

        // draw geometry
        OpenGL.Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, OpenGL.DrawElementsType.UnsignedInt, nativeint 0, modelsCount)
        Assert ()

        // teardown geometry
        OpenGL.Gl.BindVertexArray 0u
        Assert ()

        // teardown shader
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture1
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture2
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture3
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture4
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.UseProgram 0u
        Assert ()

        // teardown state
        OpenGL.Gl.Disable OpenGL.EnableCap.CullFace
        OpenGL.Gl.Disable OpenGL.EnableCap.DepthTest
        OpenGL.Gl.DepthFunc DepthFunction.Less
        OpenGL.Gl.DepthMask false

    /// Draw a the second pass of a deferred physically-based surface.
    let DrawPhysicallyBasedDeferred2Surface
        (positionTexture : uint,
         normalTexture : uint,
         albedoTexture : uint,
         materialTexture : uint,
         lightAmbient : single,
         lightPositions : single array,
         lightColors : single array,
         geometry : PhysicallyBasedGeometry,
         shader : PhysicallyBasedDeferred2Shader) =

        // setup shader
        OpenGL.Gl.UseProgram shader.PhysicallyBasedDeferred2Shader
        OpenGL.Gl.Uniform1 (shader.PositionTextureUniform, 0)
        OpenGL.Gl.Uniform1 (shader.NormalTextureUniform, 1)
        OpenGL.Gl.Uniform1 (shader.AlbedoTextureUniform, 2)
        OpenGL.Gl.Uniform1 (shader.MaterialTextureUniform, 3)
        OpenGL.Gl.Uniform1 (shader.LightAmbientUniform, lightAmbient)
        OpenGL.Gl.Uniform3 (shader.LightPositionsUniform, lightPositions)
        OpenGL.Gl.Uniform3 (shader.LightColorsUniform, lightColors)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, positionTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture1
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, normalTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture2
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, albedoTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture3
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, materialTexture)
        Assert ()

        // setup geometry
        OpenGL.Gl.BindVertexArray geometry.PhysicallyBasedVao
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Assert ()

        // draw geometry
        OpenGL.Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, OpenGL.DrawElementsType.UnsignedInt, nativeint 0)
        Assert ()

        // teardown geometry
        OpenGL.Gl.BindVertexArray 0u
        Assert ()

        // teardown shader
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture1
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture2
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture3
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.UseProgram 0u