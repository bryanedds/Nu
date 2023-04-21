namespace Tactics
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open Tactics

[<AutoOpen>]
module FieldDispatcher =

    type FieldMessage =
        | UpdateMessage
        interface Message

    type FieldCommand =
        | UpdateCommand
        interface Command

    type Screen with
        member this.GetField world = this.GetModelGeneric<Field> world
        member this.SetField value world = this.SetModelGeneric<Field> value world
        member this.Field = this.ModelGeneric<Field> ()

    type FieldDispatcher () =
        inherit ScreenDispatcher<Field, FieldMessage, FieldCommand> (Field.debug)

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
                  Metallic = 0.0f
                  MetallicImage = Assets.Default.MaterialMetallic
                  Roughness = 0.5f
                  RoughnessImage = Assets.Default.MaterialRoughness
                  AmbientOcclusion = 1.0f
                  AmbientOcclusionImage = Assets.Default.MaterialAmbientOcclusion
                  Emission = 1.0f
                  EmissionImage = Assets.Default.MaterialEmission
                  NormalImage = Assets.Default.MaterialNormal
                  Height = 1.0f
                  HeightImage = Assets.Default.MaterialHeight
                  TextureMinFilterOpt = Some OpenGL.TextureMinFilter.NearestMipmapNearest
                  TextureMagFilterOpt = Some OpenGL.TextureMagFilter.Nearest
                  InvertRoughness = false
                  TwoSided = false }
            descriptor

        override this.Initialize (_, _) =
            [Screen.UpdateEvent => UpdateMessage
             Screen.UpdateEvent => UpdateCommand]

        override this.Message (field, message, _, world) =
            match message with
            | UpdateMessage -> just (Field.advance field world)

        override this.Command (_, command, _, world) =
            match command with
            | UpdateCommand ->
                let position = World.getEyeCenter3d world
                let rotation = World.getEyeRotation3d world
                let moveSpeed =
                    if World.isKeyboardShiftDown world then 0.02f
                    elif World.isKeyboardKeyDown KeyboardKey.Return world then 0.5f
                    else 0.12f
                let turnSpeed =
                    if World.isKeyboardShiftDown world then 0.025f
                    else 0.05f
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.W world
                    then World.setEyeCenter3d (position + Vector3.Transform (v3Forward, rotation) * moveSpeed) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.S world
                    then World.setEyeCenter3d (position + Vector3.Transform (v3Back, rotation) * moveSpeed) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.A world
                    then World.setEyeCenter3d (position + Vector3.Transform (v3Left, rotation) * moveSpeed) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.D world
                    then World.setEyeCenter3d (position + Vector3.Transform (v3Right, rotation) * moveSpeed) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.Up world
                    then World.setEyeCenter3d (position + Vector3.Transform (v3Up, rotation) * moveSpeed) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.Down world
                    then World.setEyeCenter3d (position + Vector3.Transform (v3Down, rotation) * moveSpeed) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.Left world
                    then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Up, turnSpeed)) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.Right world
                    then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Down, turnSpeed)) world
                    else world
                just world

        override this.Content (field, _) =
            [Content.group Simulants.FieldScene.Name []
                [Content.fps "Fps" [Entity.Position == v3 200.0f -250.0f 0.0f]
                 Content.skyBox "SkyBox" []
                 for (index, (vertices, _)) in (Field.getOccupants field).Pairs do
                    Content.entity<CharacterDispatcher> ("Occupant+" + string index)
                        [Entity.Bottom := vertices.Center]]]

        override this.View (field, _, world) =
            let fieldMetadata = Field.getFieldMetadata field
            let fieldTexCoordsOffset = box2 (v2 (16.0f * (single (world.UpdateTime / 20L % 3L))) 0.0f) v2Zero
            let fieldUntraversableView =
                Render3d
                    (RenderUserDefinedStaticModel
                        { Absolute = false
                          ModelMatrix = m4Identity
                          InsetOpt = Some fieldTexCoordsOffset
                          SurfaceProperties = Unchecked.defaultof<_>
                          RenderType = ForwardRenderType (0.0f, 0.0f)
                          SurfaceDescriptors = [|fieldMetadata.FieldUntraversableSurfaceDescriptor|]
                          Bounds = fieldMetadata.FieldBounds })
            let fieldTraversableView =
                Render3d
                    (RenderUserDefinedStaticModel
                        { Absolute = false
                          ModelMatrix = m4Identity
                          InsetOpt = Some fieldTexCoordsOffset
                          SurfaceProperties = Unchecked.defaultof<_>
                          RenderType = ForwardRenderType (-1.0f, 0.0f)
                          SurfaceDescriptors = [|fieldMetadata.FieldTraversableSurfaceDescriptor|]
                          Bounds = fieldMetadata.FieldBounds })
            let fieldCursorView =
                match Field.tryGetFieldTileDataAtMouse field world with
                | Some (_, _, vertices) ->
                    let highlightDescriptor = createFieldHighlightSurfaceDescriptor vertices.FieldTileVertices
                    Render3d
                        (RenderUserDefinedStaticModel
                            { Absolute = false
                              ModelMatrix = m4Identity
                              InsetOpt = None
                              SurfaceProperties = Unchecked.defaultof<_>
                              RenderType = ForwardRenderType (0.0f, -1.0f)
                              SurfaceDescriptors = [|highlightDescriptor|]
                              Bounds = highlightDescriptor.Bounds })
                | None -> View.empty
            Views [|fieldUntraversableView; fieldTraversableView; fieldCursorView|]