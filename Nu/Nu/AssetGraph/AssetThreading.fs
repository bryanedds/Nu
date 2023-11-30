// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Threading.Tasks
open Prime

type TextureDataFuture = Task<Either<string, OpenGL.Texture.TextureMetadata * nativeint * IDisposable>>

type ModelFuture = Task<Either<string, Assimp.Scene * OpenGL.PrimitiveType * single Memory * int Memory * Box3>>

type TextureEir = Either<TextureDataFuture, OpenGL.Texture.TextureMetadata * uint>

type StaticModelEir = Either<ModelFuture, OpenGL.PhysicallyBased.PhysicallyBasedModel>

type AnimatedModelEir = Either<ModelFuture, OpenGL.PhysicallyBased.PhysicallyBasedModel>