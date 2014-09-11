// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2014.

namespace NuEdit
open Prime
open Nu
module NuEditConstants =

    let EditorScreenName = "EditorScreen"
    let EditorScreenAddress = addr EditorScreenName
    let EditorGroupName = "EditorGroup"
    let EditorGroupAddress = EditorScreenAddress @+ [EditorGroupName]
