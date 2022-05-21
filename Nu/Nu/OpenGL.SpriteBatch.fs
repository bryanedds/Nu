namespace OpenGL
open System
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module SpriteBatch =

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
              PositionsUniform : int
              TexCoordsesUniform : int
              ColorsUniform : int
              TexUniform : int
              Shader : uint
              Positions : single array
              TexCoordses : single array
              Colors : single array
              Vao : uint
              mutable State : State }

    /// Create a batched sprite shader.
    let private CreateShader () =

        // vertex shader code
        let samplerVertexShaderStr =
            [Constants.Render.GlslVersionPragma
             "#define VERTS 6"
             "uniform mat4 viewProjection;"
             "uniform vec2 positions[" + string Constants.Render.SpriteBatchSize + " * VERTS];"
             "uniform vec2 texCoordses[" + string Constants.Render.SpriteBatchSize + " * VERTS];"
             "uniform vec4 colors[" + string Constants.Render.SpriteBatchSize + "];"
             "out vec2 texCoords;"
             "out vec4 color;"
             "void main()"
             "{"
             "  int spriteId = gl_VertexID / VERTS;"
             "  vec2 position = positions[gl_VertexID];"
             "  gl_Position = viewProjection * vec4(position.x, position.y, 0, 1);"
             "  texCoords = texCoordses[gl_VertexID];"
             "  color = colors[spriteId];"
             "}"] |> String.join "\n"

        // fragment shader code
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
        OpenGL.Hl.Assert ()

        // grab uniform locations
        let viewProjectionUniform = OpenGL.Gl.GetUniformLocation (shader, "viewProjection")
        let positionsUniform = OpenGL.Gl.GetUniformLocation (shader, "positions")
        let texCoordsesUniform = OpenGL.Gl.GetUniformLocation (shader, "texCoordses")
        let colorsUniform = OpenGL.Gl.GetUniformLocation (shader, "colors")
        let texUniform = OpenGL.Gl.GetUniformLocation (shader, "tex")
        OpenGL.Hl.Assert ()

        // fin
        (viewProjectionUniform, positionsUniform, texCoordsesUniform, colorsUniform, texUniform, shader)

    let private BeginBatch state env =
        env.State <- state

    let private EndBatch env =

        // ensure something to draw
        if env.SpriteIndex > 0 then

            // setup state
            OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
            OpenGL.Gl.Enable OpenGL.EnableCap.Blend
            OpenGL.Hl.Assert ()
        
            // setup vao
            OpenGL.Gl.BindVertexArray env.Vao
            OpenGL.Hl.Assert ()

            // setup shader
            OpenGL.Gl.UseProgram env.Shader
            OpenGL.Gl.UniformMatrix4f (env.ViewProjectionUniform, 1, false, if env.State.Absolute then env.ViewProjectionAbsolute else env.ViewProjectionRelative)
            OpenGL.Gl.Uniform2 (env.PositionsUniform, env.Positions)
            OpenGL.Gl.Uniform2 (env.TexCoordsesUniform, env.TexCoordses)
            OpenGL.Gl.Uniform4 (env.ColorsUniform, env.Colors)
            OpenGL.Gl.Uniform1i (env.TexUniform, 1, 0)
            OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, env.State.Texture)
            OpenGL.Gl.BlendEquation env.State.BlendingEquation
            OpenGL.Gl.BlendFunc (env.State.BlendingFactorSrc, env.State.BlendingFactorDst)
            OpenGL.Hl.Assert ()

            // draw geometry
            OpenGL.Gl.DrawArrays (OpenGL.PrimitiveType.Triangles, 0, 6 * env.SpriteIndex)
            OpenGL.Hl.Assert ()

            // teardown shader
            OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.One, OpenGL.BlendingFactor.Zero)
            OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
            OpenGL.Gl.UseProgram 0u
            OpenGL.Hl.Assert ()
        
            // teardown vao
            OpenGL.Gl.BindVertexArray 0u
            OpenGL.Hl.Assert ()

            // teardown state
            OpenGL.Gl.Disable OpenGL.EnableCap.Blend
            OpenGL.Gl.Disable OpenGL.EnableCap.CullFace

            // next batch
            env.SpriteIndex <- 0

    let private RestartBatch state env =
        OpenGL.Hl.Assert (EndBatch env)
        BeginBatch state env

    let BeginFrame (viewProjectionAbsolute : Matrix4x4 inref, viewProjectionRelative : Matrix4x4 inref, env) =
        env.ViewProjectionAbsolute <- viewProjectionAbsolute
        env.ViewProjectionRelative <- viewProjectionRelative
        BeginBatch State.defaultState env

    let EndFrame env =
        EndBatch env

    let InterruptFrame fn env =
        let state = env.State
        OpenGL.Hl.Assert (EndBatch env)
        OpenGL.Hl.Assert (fn ())
        BeginBatch state env

    let inline private PopulateVertex vertexId (position : Vector2) (texCoords : Vector2) (color : Color) env =
        let positionOffset = env.SpriteIndex * 6 * 2 + vertexId * 2
        env.Positions.[positionOffset] <- position.X
        env.Positions.[positionOffset + 1] <- position.Y
        let texCoordsOffset = positionOffset
        env.TexCoordses.[texCoordsOffset] <- texCoords.X
        env.TexCoordses.[texCoordsOffset + 1] <- texCoords.Y
        if positionOffset % (6 * 2) = 0 then
            let colorOffset = env.SpriteIndex * 4
            env.Colors.[colorOffset] <- color.R
            env.Colors.[colorOffset + 1] <- color.G
            env.Colors.[colorOffset + 2] <- color.B
            env.Colors.[colorOffset + 3] <- color.A

    let SubmitSprite (absolute, position : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2, color : Color, flip, bfs, bfd, beq, texture, env) =

        // adjust to potential sprite batch state changes
        let state = State.create absolute bfs bfd beq texture
        if State.changed state env.State || env.SpriteIndex = Constants.Render.SpriteBatchSize then
            RestartBatch state env
            OpenGL.Hl.Assert ()

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
        let texCoords0 = texCoords.BottomLeft * flipper
        let position1Unrotated = v2 (position.X + size.X) position.Y
        let position1 = (position1Unrotated - center).Rotate rotation + center
        let texCoords1 = texCoords.BottomRight * flipper
        let position2Unrotated = v2 (position.X + size.X) (position.Y + size.Y)
        let position2 = (position2Unrotated - center).Rotate rotation + center
        let texCoords2 = texCoords.TopRight * flipper
        let position3Unrotated = v2 position.X (position.Y + size.Y)
        let position3 = (position3Unrotated - center).Rotate rotation + center
        let texCoords3 = texCoords.TopLeft * flipper

        // populate vertices
        PopulateVertex 0 position0 texCoords0 color env
        PopulateVertex 1 position1 texCoords1 color env
        PopulateVertex 2 position2 texCoords2 color env
        PopulateVertex 3 position3 texCoords3 color env
        PopulateVertex 4 position0 texCoords0 color env
        PopulateVertex 5 position2 texCoords2 color env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    let CreateEnv () =

        // create vao
        let vao = OpenGL.Gl.GenVertexArray ()
        OpenGL.Hl.Assert ()

        // create shader
        let (viewProjectionUniform, positionsUniform, texCoordsesUniform, colorsUniform, texUniform, shader) = CreateShader ()
        OpenGL.Hl.Assert ()

        // create env
        { SpriteIndex = 0; ViewProjectionAbsolute = m4Identity; ViewProjectionRelative = m4Identity
          ViewProjectionUniform = viewProjectionUniform; PositionsUniform = positionsUniform; TexCoordsesUniform = texCoordsesUniform
          ColorsUniform = colorsUniform; TexUniform = texUniform; Shader = shader
          Positions = Array.zeroCreate (Constants.Render.SpriteBatchSize * 2 * 6)
          TexCoordses = Array.zeroCreate (Constants.Render.SpriteBatchSize * 2 * 6)
          Colors = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Vao = vao
          State = State.defaultState }

    let DestroyEnv env =
        env.SpriteIndex <- 0