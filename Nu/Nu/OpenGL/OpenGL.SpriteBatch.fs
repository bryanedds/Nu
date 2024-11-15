// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SpriteBatch =

    type [<Struct>] private SpriteBatchState =
        { Absolute : bool
          ClipOpt : Box2 voption
          BlendingFactorSrc : BlendingFactor
          BlendingFactorDst : BlendingFactor
          BlendingEquation : BlendEquationMode
          TextureOpt : Texture.Texture voption }

        static member inline changed state state2 =
            state.Absolute <> state2.Absolute ||
            (match struct (state.ClipOpt, state2.ClipOpt) with
             | struct (ValueSome _, ValueNone) -> true
             | struct (ValueNone, ValueSome _) -> true
             | struct (ValueNone, ValueNone) -> true
             | struct (ValueSome c, ValueSome c2) -> box2Neq c c2) ||
            state.BlendingFactorSrc <> state2.BlendingFactorSrc ||
            state.BlendingFactorDst <> state2.BlendingFactorDst ||
            state.BlendingEquation <> state2.BlendingEquation ||
            (match struct (state.TextureOpt, state2.TextureOpt) with
             | struct (ValueSome _, ValueNone) -> true
             | struct (ValueNone, ValueSome _) -> true
             | struct (ValueNone, ValueNone) -> true
             | struct (ValueSome t, ValueSome t2) -> t <> t2)

        static member inline make absolute clipOpt bfs bfd beq texture =
            { Absolute = absolute; ClipOpt = clipOpt; BlendingFactorSrc = bfs; BlendingFactorDst = bfd; BlendingEquation = beq; TextureOpt = ValueSome texture }

        static member defaultState =
            { Absolute = false; ClipOpt = ValueNone; BlendingFactorSrc = BlendingFactor.SrcAlpha; BlendingFactorDst = BlendingFactor.OneMinusSrcAlpha; BlendingEquation = BlendEquationMode.FuncAdd; TextureOpt = ValueNone }

    /// The environment that contains the internal state required for batching sprites.
    type [<ReferenceEquality>] SpriteBatchEnv =
        private
            { mutable SpriteIndex : int
              mutable ViewProjectionAbsolute : Matrix4x4
              mutable ViewProjectionRelative : Matrix4x4
              PerimetersUniform : int
              TexCoordsesUniform : int
              PivotsUniform : int
              RotationsUniform : int
              ColorsUniform : int
              ViewProjectionUniform : int
              TexUniform : int
              Shader : uint
              Perimeters : single array
              Pivots : single array
              Rotations : single array
              TexCoordses : single array
              Colors : single array
              Vao : uint
              mutable State : SpriteBatchState }
              
    /// Create a sprite batch shader.
    let CreateSpriteBatchShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let perimetersUniform = Gl.GetUniformLocation (shader, "perimeters")
        let pivotsUniform = Gl.GetUniformLocation (shader, "pivots")
        let rotationsUniform = Gl.GetUniformLocation (shader, "rotations")
        let texCoordsesUniform = Gl.GetUniformLocation (shader, "texCoordses")
        let colorsUniform = Gl.GetUniformLocation (shader, "colors")
        let viewProjectionUniform = Gl.GetUniformLocation (shader, "viewProjection")
        let texUniform = Gl.GetUniformLocation (shader, "tex")
        Hl.Assert ()

        // make sprite batch shader tuple
        (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, texUniform, shader)

    let private BeginSpriteBatch state env =
        env.State <- state

    let private EndSpriteBatch windowSize env =

        // ensure something to draw
        match env.State.TextureOpt with
        | ValueSome texture when env.SpriteIndex > 0 ->

            // setup state
            Gl.BlendEquation env.State.BlendingEquation
            Gl.BlendFunc (env.State.BlendingFactorSrc, env.State.BlendingFactorDst)
            Gl.Enable EnableCap.Blend
            Gl.Enable EnableCap.CullFace
            match env.State.ClipOpt with
            | ValueSome clip ->
                let offsetViewport = Constants.Render.OffsetViewport windowSize
                let viewProjection = if env.State.Absolute then env.ViewProjectionAbsolute else env.ViewProjectionRelative
                let minClip = Vector4.Transform (Vector4 (clip.Min, 0.0f, 1.0f), viewProjection)
                let minNdc = minClip / minClip.W * single Constants.Render.VirtualScalar
                let minScissor = (minNdc.V2 + v2One) * 0.5f * Constants.Render.Resolution.V2
                let sizeScissor = clip.Size * v2Dup (single Constants.Render.VirtualScalar)
                Gl.Enable EnableCap.ScissorTest
                Gl.Scissor
                    ((minScissor.X |> round |> int) + offsetViewport.Bounds.Min.X,
                     (minScissor.Y |> round |> int) + offsetViewport.Bounds.Min.Y,
                     int sizeScissor.X,
                     int sizeScissor.Y)
            | ValueNone -> ()
            Hl.Assert ()

            // setup vao
            Gl.BindVertexArray env.Vao
            Hl.Assert ()

            // setup shader
            Gl.UseProgram env.Shader
            Gl.Uniform4 (env.PerimetersUniform, env.Perimeters)
            Gl.Uniform4 (env.TexCoordsesUniform, env.TexCoordses)
            Gl.Uniform2 (env.PivotsUniform, env.Pivots)
            Gl.Uniform1 (env.RotationsUniform, env.Rotations)
            Gl.Uniform4 (env.ColorsUniform, env.Colors)
            Gl.UniformMatrix4 (env.ViewProjectionUniform, false, if env.State.Absolute then env.ViewProjectionAbsolute.ToArray () else env.ViewProjectionRelative.ToArray ())
            Gl.Uniform1 (env.TexUniform, 0)
            Gl.ActiveTexture TextureUnit.Texture0
            Gl.BindTexture (TextureTarget.Texture2d, texture.TextureId)
            Hl.Assert ()

            // draw geometry
            Gl.DrawArrays (PrimitiveType.Triangles, 0, 6 * env.SpriteIndex)
            Hl.ReportDrawCall env.SpriteIndex
            Hl.Assert ()

            // teardown shader
            Gl.BindTexture (TextureTarget.Texture2d, 0u)
            Gl.UseProgram 0u
            Hl.Assert ()
        
            // teardown vao
            Gl.BindVertexArray 0u
            Hl.Assert ()

            // teardown state
            Gl.BlendEquation BlendEquationMode.FuncAdd
            Gl.BlendFunc (BlendingFactor.One, BlendingFactor.Zero)
            Gl.Disable EnableCap.Blend
            Gl.Disable EnableCap.CullFace
            Gl.Disable EnableCap.ScissorTest

            // next batch
            env.SpriteIndex <- 0

        // not ready
        | ValueSome _ | ValueNone -> ()

    let private RestartSpriteBatch state windowSize env =
        Hl.Assert (EndSpriteBatch windowSize env)
        BeginSpriteBatch state env

    /// Beging a new sprite batch frame3.
    let BeginSpriteBatchFrame (viewProjectionAbsolute : Matrix4x4 inref, viewProjectionRelative : Matrix4x4 inref, env) =
        env.ViewProjectionAbsolute <- viewProjectionAbsolute
        env.ViewProjectionRelative <- viewProjectionRelative
        BeginSpriteBatch SpriteBatchState.defaultState env

    /// End the current sprite batch frame, if any.
    let EndSpriteBatchFrame env =
        EndSpriteBatch env

    /// Forcibly end the current sprite batch frame, if any, run the given fn, then restart the sprite batch frame.
    let InterruptSpriteBatchFrame fn windowSize env =
        let state = env.State
        Hl.Assert (EndSpriteBatch windowSize env)
        Hl.Assert (fn ())
        BeginSpriteBatch state env

    let
#if !DEBUG
        inline
#endif
        private PopulateSpriteBatchVertex (perimeter : Box2) (pivot : Vector2) (rotation : single) (texCoords : Box2) (color : Color) env =
        let perimeterOffset = env.SpriteIndex * 4
        env.Perimeters.[perimeterOffset] <- perimeter.Min.X
        env.Perimeters.[perimeterOffset + 1] <- perimeter.Min.Y
        env.Perimeters.[perimeterOffset + 2] <- perimeter.Size.X
        env.Perimeters.[perimeterOffset + 3] <- perimeter.Size.Y
        let pivotOffset = env.SpriteIndex * 2
        env.Pivots.[pivotOffset] <- pivot.X
        env.Pivots.[pivotOffset + 1] <- pivot.Y
        let rotationOffset = env.SpriteIndex
        env.Rotations.[rotationOffset] <- rotation
        let texCoordsOffset = env.SpriteIndex * 4
        env.TexCoordses.[texCoordsOffset] <- texCoords.Min.X
        env.TexCoordses.[texCoordsOffset + 1] <- texCoords.Min.Y
        env.TexCoordses.[texCoordsOffset + 2] <- texCoords.Size.X
        env.TexCoordses.[texCoordsOffset + 3] <- texCoords.Size.Y
        let colorOffset = env.SpriteIndex * 4
        env.Colors.[colorOffset] <- color.R
        env.Colors.[colorOffset + 1] <- color.G
        env.Colors.[colorOffset + 2] <- color.B
        env.Colors.[colorOffset + 3] <- color.A

    /// Submit a sprite to the appropriate sprite batch.
    let SubmitSpriteBatchSprite (absolute, min : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2 inref, clipOpt : Box2 voption inref, color : Color inref, bfs, bfd, beq, texture : Texture.Texture, windowSize, env) =

        // adjust to potential sprite batch state changes
        let state = SpriteBatchState.make absolute clipOpt bfs bfd beq texture
        if SpriteBatchState.changed state env.State || env.SpriteIndex = Constants.Render.SpriteBatchSize then
            RestartSpriteBatch state windowSize env
            Hl.Assert ()

        // populate vertices
        let perimeter = box2 min size
        PopulateSpriteBatchVertex perimeter pivot rotation texCoords color env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    /// Destroy the given sprite batch environment.
    let CreateSpriteBatchEnv shaderFilePath =

        // create vao
        let vao = Gl.GenVertexArray ()
        Hl.Assert ()

        // create shader
        let (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, texUniform, shader) = CreateSpriteBatchShader shaderFilePath
        Hl.Assert ()

        // create env
        { SpriteIndex = 0; ViewProjectionAbsolute = m4Identity; ViewProjectionRelative = m4Identity
          PerimetersUniform = perimetersUniform; PivotsUniform = pivotsUniform; RotationsUniform = rotationsUniform
          TexCoordsesUniform = texCoordsesUniform; ColorsUniform = colorsUniform; ViewProjectionUniform = viewProjectionUniform
          TexUniform = texUniform; Shader = shader
          Perimeters = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Pivots = Array.zeroCreate (Constants.Render.SpriteBatchSize * 2)
          Rotations = Array.zeroCreate (Constants.Render.SpriteBatchSize)
          TexCoordses = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Colors = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Vao = vao
          State = SpriteBatchState.defaultState }

    /// Destroy the given sprite batch environment.
    let DestroySpriteBatchEnv env =
        env.SpriteIndex <- 0