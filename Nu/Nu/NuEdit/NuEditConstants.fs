// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2014.

namespace NuEdit
open Prime
open Nu.NuCore
module NuEditConstants =

    let EditorScreenAddress = addr "EditorScreen"
    let EditorGroupName = Lun.make "EditorGroup"
    let EditorGroupAddress = EditorScreenAddress @ [EditorGroupName]