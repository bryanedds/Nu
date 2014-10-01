// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2014.

namespace NuEdit
open System
open Prime
open Nu
module Constants =

    let EditorScreenName = "EditorScreen"
    let EditorScreenAddress = !* EditorScreenName
    let AddEntityKey = Guid.NewGuid ()
    let RemovingEntityKey = Guid.NewGuid ()