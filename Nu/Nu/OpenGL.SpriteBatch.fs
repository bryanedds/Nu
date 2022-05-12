namespace OpenGL
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module SpriteBatch =

    type [<StructuralEquality; NoComparison; StructLayout (LayoutKind.Sequential)>] private Vertex =
        struct
            val mutable Position : Vector2
            val mutable Coords : Vector2
            val mutable Color : Color
            end

    type [<NoEquality; NoComparison>] private Context =
        { mutable CpuBufferOpt : nativeint ValueOption
          GpuBuffer : uint
          GpuVao : uint }

        static member create spriteMax =

            // setup gpu vao
            let gpuVao = OpenGL.Gl.GenVertexArray ()
            OpenGL.Gl.BindVertexArray gpuVao
            OpenGL.Hl.Assert ()

            // setup gpu buffer
            let gpuBuffer = OpenGL.Gl.GenBuffer ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, gpuBuffer)
            OpenGL.Gl.EnableVertexAttribArray 0u
            OpenGL.Gl.EnableVertexAttribArray 1u
            OpenGL.Gl.EnableVertexAttribArray 2u
            OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "Position"))
            OpenGL.Gl.VertexAttribPointer (1u, 2, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "Coords"))
            OpenGL.Gl.VertexAttribPointer (2u, 4, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "Color"))
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint sizeof<Vertex> * 6u * uint spriteMax, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Hl.Assert ()

            // teardown gpu buffer
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
            OpenGL.Hl.Assert ()

            // tardown gpu vao
            OpenGL.Gl.BindVertexArray 0u
            OpenGL.Hl.Assert ()

            // make context value
            { CpuBufferOpt = ValueNone; GpuBuffer = gpuBuffer; GpuVao = gpuVao }

        static member destroy context =
            OpenGL.Gl.DeleteBuffers context.GpuBuffer
            OpenGL.Gl.DeleteVertexArrays context.GpuVao

        static member bind context =
            OpenGL.Gl.BindVertexArray context.GpuVao
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, context.GpuBuffer)

        static member unbind (_ : Context) =
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
            OpenGL.Gl.BindVertexArray 0u

        static member map context =
            Context.bind context
            let cpuBuffer = OpenGL.Gl.MapBuffer (OpenGL.BufferTarget.ArrayBuffer, OpenGL.BufferAccess.WriteOnly)
            context.CpuBufferOpt <- ValueSome cpuBuffer
            Context.unbind context
            cpuBuffer

        static member unmap context =
            let result = OpenGL.Gl.UnmapBuffer OpenGL.BufferTarget.ArrayBuffer
            context.CpuBufferOpt <- ValueNone
            result

    type [<NoEquality; NoComparison>] private Pool =
        { mutable ContextIndex : int
          Contexts : Context List
          SpriteMax : int }

        static member create spriteMax prealloc =
            let contexts = Seq.init (max 1 prealloc) (fun _ -> OpenGL.Hl.Assert (Context.create spriteMax)) |> List
            { ContextIndex = 0
              Contexts = contexts
              SpriteMax = spriteMax }

        static member destroy pool =
            for context in pool.Contexts do
                Context.destroy context
            pool.Contexts.Clear ()
            pool.ContextIndex <- 0

        static member next pool =
            pool.ContextIndex <- inc pool.ContextIndex

        static member reset pool =
            pool.ContextIndex <- 0

        member this.Current =
            while this.ContextIndex >= this.Contexts.Count do this.Contexts.Add (Context.create this.SpriteMax)
            this.Contexts.[this.ContextIndex]

    type [<StructuralEquality; NoComparison>] private State =
        { Absolute : bool
          BlendingFactorSrc : OpenGL.BlendingFactor
          BlendingFactorDst : OpenGL.BlendingFactor
          BlendingEquation : OpenGL.BlendEquationMode
          Texture : uint }

        static member create absolute bfs bfd beq texture =
            { Absolute = absolute; BlendingFactorSrc = bfs; BlendingFactorDst = bfd; BlendingEquation = beq; Texture = texture }

    type [<NoEquality; NoComparison>] Env =
        private
            { mutable SpriteIndex : int
              mutable ViewProjectionAbsolute : Matrix4x4
              mutable ViewProjectionRelative : Matrix4x4
              ViewProjectionUniform : int
              TexUniform : int
              Shader : uint
              Pool : Pool
              mutable State : State }

    /// Create a batched sprite shader with uniforms:
    ///     0: sampler2D tex
    ///     1: mat4 viewProjection
    /// and attributes:
    ///     0: vec2 position
    ///     1: vec2 coordsIn
    ///     2: vec4 colorIn
    let private CreateShader () =

        // vertex shader code
        // TODO: 3D: figure out how to apply layout(location ...) to uniform.
        let samplerVertexShaderStr =
            [Constants.Render.GlslVersionPragma
             "layout(location = 0) in vec2 position;"
             "layout(location = 1) in vec2 coordsIn;"
             "layout(location = 2) in vec4 colorIn;"
             "uniform mat4 viewProjection;"
             "out vec2 coords;"
             "out vec4 color;"
             "void main()"
             "{"
             "  gl_Position = viewProjection * vec4(position.x, position.y, 0, 1);"
             "  coords = coordsIn;"
             "  color = colorIn;"
             "}"] |> String.join "\n"

        // fragment shader code
        // TODO: 3D: figure out how to apply layout(location ...) to uniform.
        let samplerFragmentShaderStr =
            [Constants.Render.GlslVersionPragma
             "uniform sampler2D tex;"
             "in vec2 coords;"
             "in vec4 color;"
             "out vec4 frag;"
             "void main()"
             "{"
             "  frag = color * texture(tex, coords);"
             "}"] |> String.join "\n"

        // create shader
        let shader = OpenGL.Hl.CreateShaderFromStrs (samplerVertexShaderStr, samplerFragmentShaderStr)
        let viewProjectionUniform = OpenGL.Gl.GetUniformLocation (shader, "viewProjection")
        let texUniform = OpenGL.Gl.GetUniformLocation (shader, "tex")
        OpenGL.Hl.Assert ()

        // fin
        (viewProjectionUniform, texUniform, shader)

    let private Flush env =

        // setup state
        OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
        OpenGL.Gl.Enable OpenGL.EnableCap.Blend
        OpenGL.Hl.Assert ()

        // setup context
        let context = env.Pool.Current
        Context.bind context
        OpenGL.Hl.Assert ()

        // setup shader
        OpenGL.Gl.UseProgram env.Shader
        OpenGL.Gl.UniformMatrix4f (env.ViewProjectionUniform, 1, false, if env.State.Absolute then env.ViewProjectionAbsolute else env.ViewProjectionRelative)
        OpenGL.Gl.Uniform1i (env.TexUniform, 1, 0)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, env.State.Texture)
        OpenGL.Gl.BlendEquation env.State.BlendingEquation
        OpenGL.Gl.BlendFunc (env.State.BlendingFactorSrc, env.State.BlendingFactorDst)
        OpenGL.Hl.Assert ()

        // attempt to draw geometry
        if Context.unmap context then
            OpenGL.Gl.DrawArrays (OpenGL.PrimitiveType.Triangles, 0, 6 * env.SpriteIndex)
            OpenGL.Hl.Assert ()
        else Log.debug "Failed to draw sprite batch arrays due to inability to unmap cpu buffer."

        // teardown shader
        OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.One, OpenGL.BlendingFactor.Zero)
        OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.UseProgram 0u
        OpenGL.Hl.Assert ()

        // teardown context
        Context.unbind context
        OpenGL.Hl.Assert ()

        // teardown state
        OpenGL.Gl.Disable OpenGL.EnableCap.Blend
        OpenGL.Gl.Disable OpenGL.EnableCap.CullFace
        OpenGL.Hl.Assert ()

        // next pool
        env.SpriteIndex <- 0
        Pool.next env.Pool

    let CreateEnv () =
        let (viewProjectionUniform, texUniform, shader) = CreateShader ()
        let pool = Pool.create Constants.Render.SpriteBatchSize Constants.Render.SpriteBatchPoolSize
        let state = State.create false OpenGL.BlendingFactor.SrcAlpha OpenGL.BlendingFactor.OneMinusSrcAlpha OpenGL.BlendEquationMode.FuncAdd 0u
        { SpriteIndex = 0; ViewProjectionAbsolute = m4Identity; ViewProjectionRelative = m4Identity; ViewProjectionUniform = viewProjectionUniform; TexUniform = texUniform; Shader = shader; Pool = pool; State = state }

    let BeginFrame (viewport : Box2i, viewAbsolute, viewRelative, env) =
        let projection =
            Matrix4x4.CreateOrthographicOffCenter
                (single (viewport.Position.X),
                 single (viewport.Position.X + viewport.Size.X),
                 single (viewport.Position.Y),
                 single (viewport.Position.Y + viewport.Size.Y),
                 -1.0f, 1.0f)
        env.ViewProjectionAbsolute <- viewAbsolute * projection
        env.ViewProjectionRelative <- viewRelative * projection

    let NextSprite (absolute, position : Vector2, size : Vector2, pivot : Vector2, rotation, coords : Box2, color, flip, bfs, bfd, beq, texture, env) =

        // adjust to potential sprite batch state changes
        let state = State.create absolute bfs bfd beq texture
        if not (state.Equals env.State) || env.SpriteIndex = Constants.Render.SpriteBatchSize then
            if env.SpriteIndex > 0 then Flush env
            env.State <- state
        OpenGL.Hl.Assert ()

        // access the current context's cpu buffer
        let cpuBuffer =
            let context = env.Pool.Current
            match context.CpuBufferOpt with
            | ValueSome cpuBuffer -> cpuBuffer
            | ValueNone -> OpenGL.Hl.Assert (Context.map context)

        // compute a coord flipping value
        let flipper =
            match flip with
            | FlipNone -> v2 1.0f -1.0f
            | FlipH -> v2 -1.0f -1.0f
            | FlipV -> v2 1.0f 1.0f
            | FlipHV -> v2 -1.0f 1.0f

        // compute vertex positions
        let center = position + pivot
        let position0 = (position - center).Rotate rotation + center
        let position1Unrotated = v2 (position.X + size.X) position.Y
        let position1 = (position1Unrotated - center).Rotate rotation + center
        let position2Unrotated = v2 (position.X + size.X) (position.Y + size.Y)
        let position2 = (position2Unrotated - center).Rotate rotation + center
        let position3Unrotated = v2 position.X (position.Y + size.Y)
        let position3 = (position3Unrotated - center).Rotate rotation + center

        // compute vertices
        let mutable vertex0 = Unchecked.defaultof<Vertex>
        vertex0.Position <- position0
        vertex0.Coords <- coords.BottomLeft * flipper
        vertex0.Color <- color
        let mutable vertex1 = Unchecked.defaultof<Vertex>
        vertex1.Position <- position1
        vertex1.Coords <- coords.BottomRight * flipper
        vertex1.Color <- color
        let mutable vertex2 = Unchecked.defaultof<Vertex>
        vertex2.Position <- position2
        vertex2.Coords <- coords.TopRight * flipper
        vertex2.Color <- color
        let mutable vertex3 = Unchecked.defaultof<Vertex>
        vertex3.Position <- position3
        vertex3.Coords <- coords.TopLeft * flipper
        vertex3.Color <- color

        // upload vertices
        // TODO: 3D: consider using an EBO to reduce bus utilization.
        // TODO: 3D: consider using a single pre-allocated SpriteVertex[6] to reduce marshaling calls.
        let vertexSize = nativeint sizeof<Vertex>
        let cpuOffset = cpuBuffer + nativeint env.SpriteIndex * vertexSize * nativeint 6
        Marshal.StructureToPtr<Vertex> (vertex0, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<Vertex> (vertex1, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<Vertex> (vertex2, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<Vertex> (vertex3, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<Vertex> (vertex0, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<Vertex> (vertex2, cpuOffset, false)

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    let EndFrame env =
        if env.SpriteIndex > 0 then Flush env
        Pool.reset env.Pool

    let DestroyEnv env =
        env.SpriteIndex <- 0
        Pool.destroy env.Pool