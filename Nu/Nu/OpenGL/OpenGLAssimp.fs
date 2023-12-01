// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Collections.Generic
open System.Threading.Tasks
open Prime
open Nu

[<RequireQualifiedAccess>]
module Assimp =

    /// A parallelizable task for loading assimp scenes into memory.
    type AssimpSceneLoadTask =
        Task<Either<string, string * Assimp.Scene>>

    /// Memoizes assimp scene loads.
    type [<ReferenceEquality>] AssimpSceneMemo =
        { AssimpScenes : Dictionary<string, Assimp.Scene> }

        /// Make an assimp scene memoizer.
        static member make () =
            { AssimpScenes = Dictionary HashIdentity.Structural }