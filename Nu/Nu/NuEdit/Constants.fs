// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2014.

namespace NuEdit
open Prime
open Nu
module Constants =

    let EditorScreenName = "EditorScreen"
    let EditorScreenAddress = !* EditorScreenName
    let EditorGroupName = "EditorGroup"
    let EditorGroupAddress = EditorScreenAddress @+ [EditorGroupName]
