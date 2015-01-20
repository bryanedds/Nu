// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2015.

namespace NuEdit
open System
open Prime
open Nu
module Constants =

    let EditorScreenName = "EditorScreen"
    let EditorScreenAddress = stoa<Screen> EditorScreenName
    let AddEntityKey = World.makeSubscriptionKey ()
    let RemovingEntityKey = World.makeSubscriptionKey ()