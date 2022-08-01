namespace Tactics
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module FieldDispatcher =

    type FieldMessage =
        | UpdateMessage

    type FieldCommand =
        | UpdateCommand

    type Screen with
        member this.GetField world = this.GetModelGeneric<Field> world
        member this.SetField value world = this.SetModelGeneric<Field> value world
        member this.Field = this.ModelGeneric<Field> ()

    type FieldDispatcher () =
        inherit ScreenDispatcher<Field, FieldMessage, FieldCommand> (Field.make 0L FieldToBattle (asset "Field" "Field"))

        static let createFieldHighlightSurfaceDescriptor (vertices : Vector3 array) =
            let bounds = Box3.Enclose vertices
            let indices = [|0; 1; 2; 0; 2; 3|]
            let texCoordses = [|v2 0.0f 0.0f; v2 1.0f 0.0f; v2 1.0f 1.0f; v2 0.0f 1.0f|]
            let normals = Array.init 4 (fun _ -> v3Up)
            let descriptor =
                { Positions = vertices
                  TexCoordses = texCoordses
                  Normals = normals
                  Indices = indices
                  AffineMatrix = m4Identity
                  Bounds = bounds
                  Albedo = Color.White
                  AlbedoImage = asset "Default" "HighlightModelAlbedo"
                  Metalness = 0.0f
                  MetalnessImage = Assets.Default.MaterialMetalness
                  Roughness = 1.2f
                  RoughnessImage = Assets.Default.MaterialRoughness
                  AmbientOcclusion = 1.0f
                  AmbientOcclusionImage = Assets.Default.MaterialAmbientOcclusion
                  NormalImage = Assets.Default.MaterialNormal
                  TextureMinFilterOpt = ValueSome OpenGL.TextureMinFilter.NearestMipmapNearest
                  TextureMagFilterOpt = ValueSome OpenGL.TextureMagFilter.Nearest
                  TwoSided = false }
            descriptor

        override this.Channel (_, screen) =
            [screen.UpdateEvent => msg UpdateMessage
             screen.UpdateEvent => cmd UpdateCommand]

        override this.Message (field, message, _, world) =
            match message with
            | UpdateMessage -> just (Field.advance field world)

        override this.Command (_, command, _, world) =
            match command with
            | UpdateCommand ->
                let moveSpeed = if KeyboardState.isKeyDown KeyboardKey.Return then 0.5f elif KeyboardState.isShiftDown () then 0.02f else 0.12f
                let turnSpeed = if KeyboardState.isShiftDown () then 0.025f else 0.05f
                let position = World.getEyePosition3d world
                let rotation = World.getEyeRotation3d world
                let world =
                    if KeyboardState.isKeyDown KeyboardKey.W
                    then World.setEyePosition3d (position + Vector3.Transform (v3Forward, rotation) * moveSpeed) world
                    else world
                let world =
                    if KeyboardState.isKeyDown KeyboardKey.S
                    then World.setEyePosition3d (position + Vector3.Transform (v3Back, rotation) * moveSpeed) world
                    else world
                let world =
                    if KeyboardState.isKeyDown KeyboardKey.A
                    then World.setEyePosition3d (position + Vector3.Transform (v3Left, rotation) * moveSpeed) world
                    else world
                let world =
                    if KeyboardState.isKeyDown KeyboardKey.D
                    then World.setEyePosition3d (position + Vector3.Transform (v3Right, rotation) * moveSpeed) world
                    else world
                let world =
                    if KeyboardState.isKeyDown KeyboardKey.Up
                    then World.setEyePosition3d (position + Vector3.Transform (v3Up, rotation) * moveSpeed) world
                    else world
                let world =
                    if KeyboardState.isKeyDown KeyboardKey.Down
                    then World.setEyePosition3d (position + Vector3.Transform (v3Down, rotation) * moveSpeed) world
                    else world
                let world =
                    if KeyboardState.isKeyDown KeyboardKey.Left
                    then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Up, turnSpeed)) world
                    else world
                let world =
                    if KeyboardState.isKeyDown KeyboardKey.Right
                    then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Down, turnSpeed)) world
                    else world
                just world

        override this.Content (_, _) =
            [Content.group Simulants.Field.Scene.Group.Name []
                [Content.skyBox Gen.name
                    [Entity.CubeMap == Assets.Default.SkyBoxMap]
                 Content.entity<CharacterDispatcher> Gen.name
                    [Entity.Position == v3 0.0f 2.5f 0.0f]]]

        override this.View (field, _, world) =
            let fieldMetadata = Field.getFieldMetadata field world
            let fieldTexCoordsOffset = box2 (v2 (16.0f * (single (world.UpdateTime / 20L % 3L))) 0.0f) v2Zero
            let fieldUntraversableView = View.Render3d (RenderUserDefinedStaticModel (false, m4Identity, ValueSome fieldTexCoordsOffset, Unchecked.defaultof<_>, ForwardRenderType (0.0f, 0.0f), [|fieldMetadata.FieldUntraversableSurfaceDescriptor|], fieldMetadata.FieldBounds))
            let fieldTraversableView = View.Render3d (RenderUserDefinedStaticModel (false, m4Identity, ValueSome fieldTexCoordsOffset, Unchecked.defaultof<_>, ForwardRenderType (0.0f, -1.0f), [|fieldMetadata.FieldTraversableSurfaceDescriptor|], fieldMetadata.FieldBounds))
            let fieldCursorView =
                match Field.tryGetFieldTileDataAtMouse field world with
                | Some (_, _, vertices) ->
                    let highlightDescriptor = createFieldHighlightSurfaceDescriptor vertices
                    View.Render3d (RenderUserDefinedStaticModel (false, m4Identity, ValueNone, Unchecked.defaultof<_>, ForwardRenderType (-1.0f, 0.0f), [|highlightDescriptor|], highlightDescriptor.Bounds))
                | None -> View.empty
            View.Views [|fieldUntraversableView; fieldTraversableView; fieldCursorView|]