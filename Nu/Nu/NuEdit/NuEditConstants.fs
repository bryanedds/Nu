// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2014.

namespace NuEdit
open Prime
open Nu
module NuEditConstants =

    let EditorScreenAddress = addr "EditorScreen"
    let EditorGroupName = "EditorGroup"
    let EditorGroupAddress = addrlist EditorScreenAddress [EditorGroupName]