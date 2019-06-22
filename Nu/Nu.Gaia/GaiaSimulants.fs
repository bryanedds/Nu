// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu.Gaia
open System
open Prime
open Nu
open Nu.Gaia

[<AutoOpen>]
module Simulants =

    let EditorScreen = Screen Constants.Engine.DefaultScreenName
    let DefaultEditorLayer = EditorScreen / Constants.Engine.DefaultLayerName