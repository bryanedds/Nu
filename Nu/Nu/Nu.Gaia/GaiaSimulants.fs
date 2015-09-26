// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Gaia
open System
open Prime
open Nu
open Nu.Gaia

[<RequireQualifiedAccess>]
module Simulants =

    let EditorScreen = !> Constants.Engine.DefaultScreenName
    let DefaultEditorGroup = EditorScreen => Constants.Engine.DefaultGroupName