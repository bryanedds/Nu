// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2015.

namespace NuEdit
open System
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
module Constants =

    let EditorScreenName = "EditorScreen"
    let EditorScreen = Screen.proxy <| ntoa EditorScreenName
    let EditorGroupName = DefaultGroupName
    let EditorGroup = Group.proxy <| satoga EditorScreen.ScreenAddress EditorGroupName
    let AddEntityKey = World.makeSubscriptionKey ()
    let RemovingEntityKey = World.makeSubscriptionKey ()