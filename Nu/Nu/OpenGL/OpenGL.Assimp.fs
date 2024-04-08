// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Collections.Generic
open Prime
open Nu

[<RequireQualifiedAccess>]
module Assimp =

    /// Memoizes assimp scene loads.
    /// TODO: P1: encapsulate and rename from memo to client.
    type [<ReferenceEquality>] AssimpSceneMemo =
        { AssimpScenes : Dictionary<string, Assimp.Scene> }

        /// Make an assimp scene memoizer.
        static member make () =
            { AssimpScenes = Dictionary HashIdentity.Structural }