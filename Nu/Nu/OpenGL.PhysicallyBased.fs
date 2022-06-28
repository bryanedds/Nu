// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module PhysicallyBased =

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

    /// Describes a renderable physically-based surface.
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
                OpenGL.Hl.Assert ()

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
                OpenGL.Hl.Assert ()

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
                OpenGL.Hl.Assert ()

                // create index buffer
                let indexBuffer = OpenGL.Gl.GenBuffer ()
                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
                try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
                finally indexDataPtr.Free ()
                OpenGL.Hl.Assert ()

                // finalize vao
                OpenGL.Gl.BindVertexArray 0u
                OpenGL.Hl.Assert ()

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

        // make physically-based geometry
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
            match Texture.TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, albedoFilePath) with
            | Right albedoTexture ->
                match Texture.TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, metalnessFilePath) with
                | Right metalnessTexture ->
                    match Texture.TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, roughnessFilePath) with
                    | Right roughnessTexture ->
                        match Texture.TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, normalFilePath) with
                        | Right normalTexture ->
                            match Texture.TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, ambientOcclusionFilePath) with
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
                                else
                                    (true,
                                        ((Texture.TextureMetadata.empty, 0u),
                                         (Texture.TextureMetadata.empty, 0u),
                                         (Texture.TextureMetadata.empty, 0u),
                                         (Texture.TextureMetadata.empty, 0u),
                                         (Texture.TextureMetadata.empty, 0u)))
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

    let CreatePhysicallyBasedShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath

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
        let shader = Shader.CreateShaderFromFilePath shaderFilePath

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
        OpenGL.Gl.DepthFunc OpenGL.DepthFunction.Lequal
        OpenGL.Gl.Enable OpenGL.EnableCap.DepthTest
        OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
        OpenGL.Hl.Assert ()

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
        OpenGL.Hl.Assert ()

        // update models buffer
        let modelsFieldsPtr = GCHandle.Alloc (modelsFields, GCHandleType.Pinned)
        try OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, geometry.ModelBuffer)
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (modelsCount * 16 * sizeof<single>), modelsFieldsPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.DynamicDraw)
        finally modelsFieldsPtr.Free ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
        OpenGL.Hl.Assert ()

        // setup geometry
        OpenGL.Gl.BindVertexArray geometry.PhysicallyBasedVao
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        OpenGL.Hl.Assert ()

        // draw geometry
        OpenGL.Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, OpenGL.DrawElementsType.UnsignedInt, nativeint 0, modelsCount)
        OpenGL.Hl.Assert ()

        // teardown geometry
        OpenGL.Gl.BindVertexArray 0u
        OpenGL.Hl.Assert ()

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
        OpenGL.Hl.Assert ()

        // teardown state
        OpenGL.Gl.Disable OpenGL.EnableCap.CullFace
        OpenGL.Gl.Disable OpenGL.EnableCap.DepthTest
        OpenGL.Gl.DepthFunc OpenGL.DepthFunction.Less
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
        OpenGL.Hl.Assert ()

        // setup geometry
        OpenGL.Gl.BindVertexArray geometry.PhysicallyBasedVao
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        OpenGL.Hl.Assert ()

        // draw geometry
        OpenGL.Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, OpenGL.DrawElementsType.UnsignedInt, nativeint 0)
        OpenGL.Hl.Assert ()

        // teardown geometry
        OpenGL.Gl.BindVertexArray 0u
        OpenGL.Hl.Assert ()

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