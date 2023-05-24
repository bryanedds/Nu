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
            let properties : OpenGL.PhysicallyBased.PhysicallyBasedMaterialProperties =
                { Albedo = Color.White
                  Metallic = 0.0f
                  Roughness = 0.8f
                  AmbientOcclusion = 1.0f
                  Emission = 1.0f
                  Height = 1.0f
                  InvertRoughness = false }
            let descriptor =
                { Positions = vertices
                  TexCoordses = texCoordses
                  Normals = normals
                  Indices = indices
                  AffineMatrix = m4Identity
                  Bounds = bounds
                  MaterialProperties = properties
                  AlbedoImage = asset "Default" "HighlightModelAlbedo"
                  MetallicImage = Assets.Default.MaterialMetallic
                  RoughnessImage = Assets.Default.MaterialRoughness
                  AmbientOcclusionImage = Assets.Default.MaterialAmbientOcclusion
                  EmissionImage = Assets.Default.MaterialEmission
                  NormalImage = Assets.Default.MaterialNormal
                  HeightImage = Assets.Default.MaterialHeight
                  TextureMinFilterOpt = Some OpenGL.TextureMinFilter.NearestMipmapNearest
                  TextureMagFilterOpt = Some OpenGL.TextureMagFilter.Nearest
                  TwoSided = false }
            descriptor

#if AD_HOC_METRICS
        override this.Register (game, world) =
            let world = base.Register (game, world)
            let population = 100
            let spread = 10.0f
            let offset = v3Dup spread * single population * 0.5f
            let positions =
                [|for i in 0 .. dec population do
                    for j in 0 .. dec population do
                        for k in 0 .. dec population do
                            let random = v3 (Gen.randomf1 spread) (Gen.randomf1 spread) (Gen.randomf1 spread) - v3Dup (spread * 0.5f)
                            let position = v3 (single i) (single j) (single k) * spread + random - offset
                            position|]
            let world =
                Seq.fold (fun world position ->
                    let (staticModel, world) = World.createEntity<StaticModelDispatcher> NoOverlay None Simulants.FieldScene world
                    staticModel.SetPosition position world)
                    world positions
            world
#endif

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
                    if World.isKeyboardKeyDown KeyboardKey.Q world
                    then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Right, turnSpeed)) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.E world
                    then World.setEyeRotation3d (rotation * Quaternion.CreateFromAxisAngle (v3Left, turnSpeed)) world
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
                    then World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Up, turnSpeed) * rotation) world
                    else world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.Right world
                    then World.setEyeRotation3d (Quaternion.CreateFromAxisAngle (v3Down, turnSpeed) * rotation) world
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
                          Presence = Omnipresent
                          InsetOpt = Some fieldTexCoordsOffset
                          MaterialProperties = Unchecked.defaultof<_>
                          RenderType = ForwardRenderType (0.0f, 0.0f)
                          SurfaceDescriptors = [|fieldMetadata.FieldUntraversableSurfaceDescriptor|]
                          Bounds = fieldMetadata.FieldBounds })
            let fieldTraversableView =
                Render3d
                    (RenderUserDefinedStaticModel
                        { Absolute = false
                          ModelMatrix = m4Identity
                          Presence = Omnipresent
                          InsetOpt = Some fieldTexCoordsOffset
                          MaterialProperties = Unchecked.defaultof<_>
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
                              Presence = Omnipresent
                              InsetOpt = None
                              MaterialProperties = Unchecked.defaultof<_>
                              RenderType = ForwardRenderType (0.0f, -1.0f)
                              SurfaceDescriptors = [|highlightDescriptor|]
                              Bounds = highlightDescriptor.Bounds })
                | None -> View.empty
            Views [|fieldUntraversableView; fieldTraversableView; fieldCursorView|]