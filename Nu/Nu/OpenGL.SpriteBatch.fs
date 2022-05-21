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
              PerimetersUniform : int
              TexCoordsesUniform : int
              CentersUniform : int
              RotationsUniform : int
              ColorsUniform : int
              TexUniform : int
              Shader : uint
              Perimeters : single array
              Centers : single array
              Rotations : single array
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
             "uniform vec4 perimeters[" + string Constants.Render.SpriteBatchSize + "];"
             "uniform vec2 centers[" + string Constants.Render.SpriteBatchSize + "];"
             "uniform float rotations[" + string Constants.Render.SpriteBatchSize + "];"
             "uniform vec4 texCoordses[" + string Constants.Render.SpriteBatchSize + "];"
             "uniform vec4 colors[" + string Constants.Render.SpriteBatchSize + "];"
             "out vec2 texCoords;"
             "out vec4 color;"
             ""
             "vec2 rotate(vec2 v, float a)"
             "{"
             "  float s = sin(a);"
             "  float c = cos(a);"
             "  mat2 m = mat2(c, -s, s, c);"
             "  return m * v;"
             "}"
             ""
             "void main()"
             "{"
             "  int spriteId = gl_VertexID / VERTS;"
             "  int vertexId = gl_VertexID % VERTS;"
             "  vec2 position;"
             "  vec4 perimeter = perimeters[spriteId];"
             "  vec2 center = centers[spriteId];"
             "  float rotation = rotations[spriteId];"
             "  vec4 texCoords4 = texCoordses[spriteId];"
             "  switch (vertexId)"
             "  {"
             "      case 0: case 4:"
             "          position = vec2(perimeter.x, perimeter.y);"
             "          texCoords = vec2(texCoords4.x, texCoords4.y);"
             "          break;"
             "      case 1:"
             "          position = vec2(perimeter.x + perimeter.z, perimeter.y);"
             "          texCoords = vec2(texCoords4.x + texCoords4.z, texCoords4.y);"
             "          break;"
             "      case 2: case 5:"
             "          position = vec2(perimeter.x + perimeter.z, perimeter.y + perimeter.w);"
             "          texCoords = vec2(texCoords4.x + texCoords4.z, texCoords4.y + texCoords4.w);"
             "          break;"
             "      case 3:"
             "          position = vec2(perimeter.x, perimeter.y + perimeter.w);"
             "          texCoords = vec2(texCoords4.x, texCoords4.y + texCoords4.w);"
             "          break;"
             "  }"
             "  vec2 positionRotated = center + rotate(position - center, rotation);"
             "  gl_Position = viewProjection * vec4(positionRotated.x, positionRotated.y, 0, 1);"
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
        let perimetersUniform = OpenGL.Gl.GetUniformLocation (shader, "perimeters")
        let centersUniform = OpenGL.Gl.GetUniformLocation (shader, "centers")
        let rotationsUniform = OpenGL.Gl.GetUniformLocation (shader, "rotations")
        let texCoordsesUniform = OpenGL.Gl.GetUniformLocation (shader, "texCoordses")
        let colorsUniform = OpenGL.Gl.GetUniformLocation (shader, "colors")
        let texUniform = OpenGL.Gl.GetUniformLocation (shader, "tex")
        OpenGL.Hl.Assert ()

        // fin
        (viewProjectionUniform, perimetersUniform, centersUniform, rotationsUniform, texCoordsesUniform, colorsUniform, texUniform, shader)

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
            OpenGL.Gl.Uniform4 (env.PerimetersUniform, env.Perimeters)
            OpenGL.Gl.Uniform4 (env.TexCoordsesUniform, env.TexCoordses)
            OpenGL.Gl.Uniform2 (env.CentersUniform, env.Centers)
            OpenGL.Gl.Uniform1 (env.RotationsUniform, env.Rotations)
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

    let private PopulateVertex (position : Vector2) (size : Vector2) (center : Vector2) (rotation : single) (texCoords : Box2) (color : Color) env =
        let perimeterOffset = env.SpriteIndex * 4
        env.Perimeters.[perimeterOffset] <- position.X
        env.Perimeters.[perimeterOffset + 1] <- position.Y
        env.Perimeters.[perimeterOffset + 2] <- size.X
        env.Perimeters.[perimeterOffset + 3] <- size.Y
        let centerOffset = env.SpriteIndex * 2
        env.Centers.[centerOffset] <- center.X
        env.Centers.[centerOffset + 1] <- center.Y
        let rotationOffset = env.SpriteIndex
        env.Rotations.[rotationOffset] <- rotation
        let texCoordsOffset = env.SpriteIndex * 4
        env.TexCoordses.[texCoordsOffset] <- texCoords.Position.X
        env.TexCoordses.[texCoordsOffset + 1] <- texCoords.Position.Y
        env.TexCoordses.[texCoordsOffset + 2] <- texCoords.Size.X
        env.TexCoordses.[texCoordsOffset + 3] <- texCoords.Size.Y
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

        // compute center
        let center = position + pivot

        // compute a coord flipping value
        let flipper =
            match flip with
            | FlipNone -> v2 1.0f -1.0f
            | FlipH -> v2 -1.0f -1.0f
            | FlipV -> v2 1.0f 1.0f
            | FlipHV -> v2 -1.0f 1.0f

        // populate vertices
        PopulateVertex position size center rotation texCoords color env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    let CreateEnv () =

        // create vao
        let vao = OpenGL.Gl.GenVertexArray ()
        OpenGL.Hl.Assert ()

        // create shader
        let (viewProjectionUniform, perimetersUniform, centersUniform, rotationsUniform, texCoordsesUniform, colorsUniform, texUniform, shader) = CreateShader ()
        OpenGL.Hl.Assert ()

        // create env
        { SpriteIndex = 0; ViewProjectionAbsolute = m4Identity; ViewProjectionRelative = m4Identity
          ViewProjectionUniform = viewProjectionUniform
          PerimetersUniform = perimetersUniform; CentersUniform = centersUniform; RotationsUniform = rotationsUniform
          TexCoordsesUniform = texCoordsesUniform; ColorsUniform = colorsUniform; TexUniform = texUniform; Shader = shader
          Perimeters = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Centers = Array.zeroCreate (Constants.Render.SpriteBatchSize * 2)
          Rotations = Array.zeroCreate (Constants.Render.SpriteBatchSize)
          TexCoordses = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Colors = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Vao = vao
          State = State.defaultState }

    let DestroyEnv env =
        env.SpriteIndex <- 0