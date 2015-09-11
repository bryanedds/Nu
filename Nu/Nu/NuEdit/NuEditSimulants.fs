// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2015.

namespace NuEdit
open System
open Prime
open Nu
open NuEdit

[<RequireQualifiedAccess>]
module Simulants =

    let EditorScreen = !> Constants.Engine.DefaultScreenName
    let DefaultEditorGroup = EditorScreen => Constants.Engine.DefaultGroupName