// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2014.

namespace NuEdit
open System
open Prime
open Nu
module Constants =

    let EditorScreenName = "EditorScreen"
    let EditorScreenAddress = stoa<obj> EditorScreenName
    let AddEntityKey = Guid.NewGuid ()
    let RemovingEntityKey = Guid.NewGuid ()