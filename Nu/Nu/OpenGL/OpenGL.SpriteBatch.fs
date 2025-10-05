// Nu Game Engine.
// Copyright (C) Bryan Edds.

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
             | struct (ValueNone, ValueNone) -> false
             | struct (ValueSome c, ValueSome c2) -> box2Neq c c2) ||
            state.BlendingFactorSrc <> state2.BlendingFactorSrc ||
            state.BlendingFactorDst <> state2.BlendingFactorDst ||
            state.BlendingEquation <> state2.BlendingEquation ||
            (match struct (state.TextureOpt, state2.TextureOpt) with
             | struct (ValueSome _, ValueNone) -> true
             | struct (ValueNone, ValueSome _) -> true
             | struct (ValueNone, ValueNone) -> false
             | struct (ValueSome t, ValueSome t2) -> t <> t2)

        static member inline make absolute clipOpt bfs bfd beq texture =
            { Absolute = absolute; ClipOpt = clipOpt; BlendingFactorSrc = bfs; BlendingFactorDst = bfd; BlendingEquation = beq; TextureOpt = ValueSome texture }

        static member defaultState =
            { Absolute = false; ClipOpt = ValueNone; BlendingFactorSrc = BlendingFactor.SrcAlpha; BlendingFactorDst = BlendingFactor.OneMinusSrcAlpha; BlendingEquation = BlendEquationMode.FuncAdd; TextureOpt = ValueNone }

    /// The environment that contains the internal state required for batching sprites.
    type [<ReferenceEquality>] SpriteBatchEnv =
        private
            { mutable SpriteIndex : int
              mutable ViewProjection2dAbsolute : Matrix4x4
              mutable ViewProjection2dRelative : Matrix4x4
              mutable ViewProjectionClipAbsolute : Matrix4x4
              mutable ViewProjectionClipRelative : Matrix4x4
              mutable PerimetersUniform : int
              mutable TexCoordsesUniform : int
              mutable PivotsUniform : int
              mutable RotationsUniform : int
              mutable ColorsUniform : int
              mutable ViewProjectionUniform : int
              mutable TexUniform : int
              mutable Shader : uint
              Perimeters : single array
              Pivots : single array
              Rotations : single array
              TexCoordses : single array
              Colors : single array
              Vao : uint
              mutable State : SpriteBatchState }
              
    let private CreateSpriteBatchShader (shaderFilePath : string) =

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

    /// Reload the shaders used by the environment.
    let ReloadShaders env =
        let (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, texUniform, shader) =
            CreateSpriteBatchShader Constants.Paths.SpriteBatchShaderFilePath
        env.PerimetersUniform <- perimetersUniform
        env.PivotsUniform <- pivotsUniform
        env.RotationsUniform <- rotationsUniform
        env.TexCoordsesUniform <- texCoordsesUniform
        env.ColorsUniform <- colorsUniform
        env.ViewProjectionUniform <- viewProjectionUniform
        env.TexUniform <- texUniform
        env.Shader <- shader

    let private BeginSpriteBatch state env =
        env.State <- state

    let private EndSpriteBatch (viewport : Viewport) env =

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
                let viewProjection = if env.State.Absolute then env.ViewProjectionClipAbsolute else env.ViewProjectionClipRelative
                let minClip = Vector4.Transform (Vector4 (clip.Min, 0.0f, 1.0f), viewProjection)
                let minNdc = minClip / minClip.W * single viewport.DisplayScalar
                let minScissor = (minNdc.V2 + v2One) * 0.5f * viewport.Inset.Size.V2
                let sizeScissor = clip.Size * v2Dup (single viewport.DisplayScalar)
                let offset = viewport.Inset.Min
                Gl.Enable EnableCap.ScissorTest
                Gl.Scissor
                    ((minScissor.X |> round |> int) + offset.X,
                     (minScissor.Y |> round |> int) + offset.Y,
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
            Gl.UniformMatrix4 (env.ViewProjectionUniform, false, if env.State.Absolute then env.ViewProjection2dAbsolute.ToArray () else env.ViewProjection2dRelative.ToArray ())
            Gl.Uniform1 (env.TexUniform, 0)
            Gl.ActiveTexture TextureUnit.Texture0
            Gl.BindTexture (TextureTarget.Texture2d, texture.TextureId)
            Hl.Assert ()

            // draw geometry
            Gl.DrawArrays (PrimitiveType.Triangles, 0, 6 * env.SpriteIndex)
            Hl.ReportDrawCall env.SpriteIndex
            Hl.Assert ()

            // teardown shader
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

    let private RestartSpriteBatch state viewport env =
        Hl.Assert (EndSpriteBatch viewport env)
        BeginSpriteBatch state env

    /// Beging a new sprite batch frame3.
    let BeginSpriteBatchFrame
        (viewProjection2dAbsolute : Matrix4x4 inref,
         viewProjection2dRelative : Matrix4x4 inref,
         viewProjectionClipAbsolute : Matrix4x4 inref,
         viewProjectionClipRelative : Matrix4x4 inref,
         env) =
        env.ViewProjection2dAbsolute <- viewProjection2dAbsolute
        env.ViewProjection2dRelative <- viewProjection2dRelative
        env.ViewProjectionClipAbsolute <- viewProjectionClipAbsolute
        env.ViewProjectionClipRelative <- viewProjectionClipRelative
        BeginSpriteBatch SpriteBatchState.defaultState env

    /// End the current sprite batch frame, if any.
    let EndSpriteBatchFrame env =
        EndSpriteBatch env

    /// Forcibly end the current sprite batch frame, if any, run the given fn, then restart the sprite batch frame.
    let InterruptSpriteBatchFrame fn viewport env =
        let state = env.State
        Hl.Assert (EndSpriteBatch viewport env)
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
    let SubmitSpriteBatchSprite (absolute, min : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2 inref, clipOpt : Box2 voption inref, color : Color inref, bfs, bfd, beq, texture : Texture.Texture, viewport, env) =

        // adjust to potential sprite batch state changes
        let state = SpriteBatchState.make absolute clipOpt bfs bfd beq texture
        if SpriteBatchState.changed state env.State || env.SpriteIndex = Constants.Render.SpriteBatchSize then
            RestartSpriteBatch state viewport env
            Hl.Assert ()

        // populate vertices
        let perimeter = box2 min size
        PopulateSpriteBatchVertex perimeter pivot rotation texCoords color env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    /// Destroy the given sprite batch environment.
    let CreateSpriteBatchEnv () =

        // create shader
        let (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, texUniform, shader) =
            CreateSpriteBatchShader Constants.Paths.SpriteBatchShaderFilePath
        Hl.Assert ()

        // create vao
        let vao =  [|0u|]
        Gl.CreateVertexArrays vao
        let vao = vao.[0]
        Hl.Assert ()

        // create env
        { SpriteIndex = 0
          ViewProjection2dAbsolute = m4Identity
          ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity
          ViewProjectionClipRelative = m4Identity
          PerimetersUniform = perimetersUniform
          PivotsUniform = pivotsUniform
          RotationsUniform = rotationsUniform
          TexCoordsesUniform = texCoordsesUniform
          ColorsUniform = colorsUniform
          ViewProjectionUniform = viewProjectionUniform
          TexUniform = texUniform
          Shader = shader
          Perimeters = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Pivots = Array.zeroCreate (Constants.Render.SpriteBatchSize * 2)
          Rotations = Array.zeroCreate (Constants.Render.SpriteBatchSize)
          TexCoordses = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Colors = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Vao = vao
          State = SpriteBatchState.defaultState }

    /// Destroy the given sprite batch environment.
    let DestroySpriteBatchEnv env =
        Gl.DeleteProgram env.Shader
        Gl.DeleteVertexArrays [|env.Vao|]