namespace OpenGL
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module SpriteBatch =

    let private VertexBuffer = Array.zeroCreate<single> 8

    type [<StructuralEquality; NoComparison; StructLayout (LayoutKind.Sequential)>] private Vertex =
        struct
            val mutable Position : Vector2
            val mutable TexCoords : Vector2
            val mutable Color : Color
            end

        static member inline copy (vertex : Vertex) (ptr : nativeint) =
            VertexBuffer.[0] <- vertex.Position.X
            VertexBuffer.[1] <- vertex.Position.Y
            VertexBuffer.[2] <- vertex.TexCoords.X
            VertexBuffer.[3] <- vertex.TexCoords.Y
            VertexBuffer.[4] <- vertex.Color.R
            VertexBuffer.[5] <- vertex.Color.G
            VertexBuffer.[6] <- vertex.Color.B
            VertexBuffer.[7] <- vertex.Color.A
            Marshal.Copy (VertexBuffer, 0, ptr, 8)

    type [<NoEquality; NoComparison>] private Context =
        { mutable CpuBufferOpt : nativeint ValueOption
          GpuBuffer : uint
          Vao : uint }

        static member create spriteMax =

            // setup vao
            let vao = OpenGL.Gl.GenVertexArray ()
            OpenGL.Gl.BindVertexArray vao
            OpenGL.Hl.Assert ()

            // create gpu buffer
            let gpuBuffer = OpenGL.Gl.GenBuffer ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, gpuBuffer)
            OpenGL.Gl.EnableVertexAttribArray 0u
            OpenGL.Gl.EnableVertexAttribArray 1u
            OpenGL.Gl.EnableVertexAttribArray 2u
            OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "Position"))
            OpenGL.Gl.VertexAttribPointer (1u, 2, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "TexCoords"))
            OpenGL.Gl.VertexAttribPointer (2u, 4, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "Color"))
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint sizeof<Vertex> * 6u * uint spriteMax, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
            OpenGL.Hl.Assert ()

            // teardown vao
            OpenGL.Gl.BindVertexArray 0u
            OpenGL.Hl.Assert ()

            // make context value
            { CpuBufferOpt = ValueNone; GpuBuffer = gpuBuffer; Vao = vao }

        static member destroy context =
            OpenGL.Gl.DeleteBuffers context.GpuBuffer
            OpenGL.Gl.DeleteVertexArrays context.Vao

        static member bind context =
            OpenGL.Gl.BindVertexArray context.Vao
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

    type [<StructuralEquality; NoComparison; Struct>] private State =
        { Absolute : bool
          BlendingFactorSrc : OpenGL.BlendingFactor
          BlendingFactorDst : OpenGL.BlendingFactor
          BlendingEquation : OpenGL.BlendEquationMode
          Texture : uint }

        static member inline changed state state2 =
            state.Absolute <> state2.Absolute ||
            state.BlendingFactorSrc <> state2.BlendingFactorSrc ||
            state.BlendingFactorDst <> state2.BlendingFactorDst ||
            state.BlendingEquation <> state2.BlendingEquation ||
            state.Texture <> state2.Texture

        static member create absolute bfs bfd beq texture =
            { Absolute = absolute; BlendingFactorSrc = bfs; BlendingFactorDst = bfd; BlendingEquation = beq; Texture = texture }

        static member defaultState =
            State.create false OpenGL.BlendingFactor.SrcAlpha OpenGL.BlendingFactor.OneMinusSrcAlpha OpenGL.BlendEquationMode.FuncAdd 0u

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
    ///     1: vec2 texCoordsIn
    ///     2: vec4 colorIn
    let private CreateShader () =

        // vertex shader code
        // TODO: 3D: figure out how to apply layout(location ...) to uniform.
        let samplerVertexShaderStr =
            [Constants.Render.GlslVersionPragma
             "layout(location = 0) in vec2 position;"
             "layout(location = 1) in vec2 texCoordsIn;"
             "layout(location = 2) in vec4 colorIn;"
             "uniform mat4 viewProjection;"
             "out vec2 texCoords;"
             "out vec4 color;"
             "void main()"
             "{"
             "  gl_Position = viewProjection * vec4(position.x, position.y, 0, 1);"
             "  texCoords = texCoordsIn;"
             "  color = colorIn;"
             "}"] |> String.join "\n"

        // fragment shader code
        // TODO: 3D: figure out how to apply layout(location ...) to uniform.
        let samplerFragmentShaderStr =
            [Constants.Render.GlslVersionPragma
             "uniform sampler2D tex;"
             "in vec2 texCoords;"
             "in vec4 color;"
             "out vec4 frag;"
             "void main()"
             "{"
             "  frag = color * texture(tex, texCoords);"
             "}"] |> String.join "\n"

        // create shader
        let shader = OpenGL.Hl.CreateShaderFromStrs (samplerVertexShaderStr, samplerFragmentShaderStr)
        let viewProjectionUniform = OpenGL.Gl.GetUniformLocation (shader, "viewProjection")
        let texUniform = OpenGL.Gl.GetUniformLocation (shader, "tex")
        (viewProjectionUniform, texUniform, shader)

    let private BeginBatch state env =
        env.State <- state
        let context = env.Pool.Current
        let cpuBuffer = OpenGL.Hl.Assert (Context.map context)
        context.CpuBufferOpt <- ValueSome cpuBuffer

    let private EndBatch env =

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
        OpenGL.Gl.Uniform1i (env.TexUniform, 1, 0)
        OpenGL.Gl.UniformMatrix4f (env.ViewProjectionUniform, 1, false, if env.State.Absolute then env.ViewProjectionAbsolute else env.ViewProjectionRelative)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, env.State.Texture)
        OpenGL.Gl.BlendEquation env.State.BlendingEquation
        OpenGL.Gl.BlendFunc (env.State.BlendingFactorSrc, env.State.BlendingFactorDst)
        OpenGL.Hl.Assert ()

        // attempt to draw geometry
        if Context.unmap context then
            OpenGL.Gl.DrawArrays (OpenGL.PrimitiveType.Triangles, 0, 6 * env.SpriteIndex)
            OpenGL.Hl.Assert ()
        else raise (InvalidOperationException "Failed to unmap gpu buffer.")

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

    let private RestartBatch state env =
        EndBatch env
        BeginBatch state env

    let private InBatch env =
        ValueOption.isSome env.Pool.Current.CpuBufferOpt

    let BeginFrame (viewProjectionAbsolute : Matrix4x4 inref, viewProjectionRelative : Matrix4x4 inref, env) =
        if InBatch env then raise (InvalidOperationException "Cannot begin a SpriteBatch frame that is already begun.")
        env.ViewProjectionAbsolute <- viewProjectionAbsolute
        env.ViewProjectionRelative <- viewProjectionRelative
        BeginBatch State.defaultState env

    let EndFrame env =
        if not (InBatch env) then raise (InvalidOperationException "Cannot end a SpriteBatch frame that is not begun.")
        EndBatch env
        Pool.reset env.Pool

    let InterruptFrame fn env =
        if not (InBatch env) then raise (InvalidOperationException "Cannot interrupt a SpriteBatch frame that is not begun.")
        let state = env.State
        OpenGL.Hl.Assert (EndBatch env)
        OpenGL.Hl.Assert (fn ())
        OpenGL.Hl.Assert (BeginBatch state env)

    let SubmitSprite (absolute, position : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2, color, flip, bfs, bfd, beq, texture, env) =

        // ensure frame has begun
        if not (InBatch env) then
            raise (InvalidOperationException "Cannot submit a sprite in a SpriteBatch frame that is not begun.")

        // adjust to potential sprite batch state changes
        let state = State.create absolute bfs bfd beq texture
        if State.changed state env.State || env.SpriteIndex = Constants.Render.SpriteBatchSize then
            RestartBatch state env
            OpenGL.Hl.Assert ()

        // access the current context's cpu buffer
        let cpuBuffer = env.Pool.Current.CpuBufferOpt.Value

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
        vertex0.TexCoords <- texCoords.BottomLeft * flipper
        vertex0.Color <- color
        let mutable vertex1 = Unchecked.defaultof<Vertex>
        vertex1.Position <- position1
        vertex1.TexCoords <- texCoords.BottomRight * flipper
        vertex1.Color <- color
        let mutable vertex2 = Unchecked.defaultof<Vertex>
        vertex2.Position <- position2
        vertex2.TexCoords <- texCoords.TopRight * flipper
        vertex2.Color <- color
        let mutable vertex3 = Unchecked.defaultof<Vertex>
        vertex3.Position <- position3
        vertex3.TexCoords <- texCoords.TopLeft * flipper
        vertex3.Color <- color

        // upload vertices
        // TODO: 3D: consider using a single static EBO for indices to reduce bus utilization.
        let vertexSize = nativeint sizeof<Vertex>
        let cpuOffset = cpuBuffer + nativeint env.SpriteIndex * vertexSize * nativeint 6
        Vertex.copy vertex0 cpuOffset
        let cpuOffset = cpuOffset + vertexSize
        Vertex.copy vertex1 cpuOffset
        let cpuOffset = cpuOffset + vertexSize
        Vertex.copy vertex2 cpuOffset
        let cpuOffset = cpuOffset + vertexSize
        Vertex.copy vertex3 cpuOffset
        let cpuOffset = cpuOffset + vertexSize
        Vertex.copy vertex0 cpuOffset
        let cpuOffset = cpuOffset + vertexSize
        Vertex.copy vertex2 cpuOffset

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    let CreateEnv () =
        let (viewProjectionUniform, texUniform, shader) = CreateShader ()
        let pool = Pool.create Constants.Render.SpriteBatchSize Constants.Render.SpriteBatchPoolPrealloc
        { SpriteIndex = 0; ViewProjectionAbsolute = m4Identity; ViewProjectionRelative = m4Identity; ViewProjectionUniform = viewProjectionUniform; TexUniform = texUniform; Shader = shader; Pool = pool; State = State.defaultState }

    let DestroyEnv env =
        if InBatch env then raise (InvalidOperationException "Cannot destroy a SpriteBatch environemt that has not been ended.")
        env.SpriteIndex <- 0
        Pool.destroy env.Pool