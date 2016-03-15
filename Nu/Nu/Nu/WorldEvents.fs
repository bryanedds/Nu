// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Prime
open Nu

/// The data for a mouse move event.
type [<StructuralEquality; NoComparison>] MouseMoveData =
    { Position : Vector2 }

/// The data for a mouse button event.
type [<StructuralEquality; NoComparison>] MouseButtonData =
    { Position : Vector2
      Button : MouseButton
      Down : bool }

/// The data for a keyboard key event.
type [<StructuralEquality; NoComparison>] KeyboardKeyData =
    { ScanCode : int
      Repeated : bool
      Down : bool }

/// The data for a collision event.
type [<StructuralEquality; NoComparison>] CollisionData =
    { Normal : Vector2
      Speed : single
      Collidee : Entity }

/// The data for a change in the world's ambient state.
/// NOTE: I couldn't give its field the more normal name of 'OldWorld' due to field name conflicts with the more
/// pervasive ParticipantChangeData type.
type [<StructuralEquality; NoComparison>] AmbientStateChangeData = 
    { OldWorldWithOldState : World }

[<RequireQualifiedAccess>]
module Events =

    let Any = Prime.Events.Any
    let Update = ntoa<unit> !!"Update"
    let Select = ntoa<unit> !!"Select"
    let Deselect = ntoa<unit> !!"Deselect"
    let Down = ntoa<unit> !!"Down"
    let Up = ntoa<unit> !!"Up"
    let Click = ntoa<unit> !!"Click"
    let On = ntoa<unit> !!"On"
    let Off = ntoa<unit> !!"Off"
    let Touch = ntoa<Vector2> !!"Touch"
    let Untouch = ntoa<Vector2> !!"Untouch"
    let Mouse = ntoa<obj> !!"Mouse"
    let MouseMove = Mouse -<- ntoa<MouseMoveData> !!"Move"
    let MouseDrag = Mouse -<- ntoa<MouseMoveData> !!"Drag"
    let MouseLeft = Mouse -<- ntoa<MouseButtonData> !!"Left"
    let MouseCenter = Mouse -<- ntoa<MouseButtonData> !!"Center"
    let MouseRight = Mouse -<- ntoa<MouseButtonData> !!"Right"
    let MouseX1 = Mouse -<- ntoa<MouseButtonData> !!"X1"
    let MouseX2 = Mouse -<- ntoa<MouseButtonData> !!"X2"
    let MouseLeftDown = MouseLeft -|- ntoa !!"Down"
    let MouseLeftUp = MouseLeft -|- ntoa !!"Up"
    let MouseLeftChange = MouseLeft -|- ntoa !!"Change"
    let MouseCenterDown = MouseCenter -|- ntoa !!"Down"
    let MouseCenterUp = MouseCenter -|- ntoa !!"Up"
    let MouseCenterChange = MouseCenter -|- ntoa !!"Change"
    let MouseRightDown = MouseRight -|- ntoa !!"Down"
    let MouseRightUp = MouseRight -|- ntoa !!"Up"
    let MouseRightChange = MouseRight -|- ntoa !!"Change"
    let MouseX1Down = MouseX1 -|- ntoa !!"Down"
    let MouseX1Up = MouseX1 -|- ntoa !!"Up"
    let MouseX1Change = MouseX1 -|- ntoa !!"Change"
    let MouseX2Down = MouseX2 -|- ntoa !!"Down"
    let MouseX2Up = MouseX2 -|- ntoa !!"Up"
    let MouseX2Change = MouseX2 -|- ntoa !!"Change"
    let KeyboardKey = ntoa<obj> !!"KeyboardKey"
    let KeyboardKeyDown = Mouse -<- ntoa<KeyboardKeyData> !!"Down"
    let KeyboardKeyUp = Mouse -<- ntoa<KeyboardKeyData> !!"Up"
    let KeyboardKeyChange = Mouse -<- ntoa<KeyboardKeyData> !!"Change"
    let Collision = ntoa<CollisionData> !!"Collision"
    let Incoming = ntoa<unit> !!"Incoming"
    let IncomingStart = Incoming -|- ntoa !!"Start"
    let IncomingFinish = Incoming -|- ntoa !!"Finish"
    let Outgoing = ntoa<unit> !!"Outgoing"
    let OutgoingStart = Outgoing -|- ntoa !!"Start"
    let OutgoingFinish = Outgoing -|- ntoa !!"Finish"
    let AmbientState = ntoa<obj> !!"AmbientState"
    let AmbientStateChange = AmbientState -<- ntoa<AmbientStateChangeData> !!"Change"
    let Game = ntoa<obj> !!"Game"
    let GameChange = Game -<- ntoa<ParticipantChangeData<Game, World>> !!"Change"
    let Screen = ntoa<obj> !!"Screen"
    let ScreenAdd = Screen -<- ntoa<unit> !!"Add"
    let ScreenRemoving = Screen -<- ntoa<unit> !!"Removing"
    let ScreenChange = Screen -<- ntoa<ParticipantChangeData<Screen, World>> !!"Change"
    let Group = ntoa<obj> !!"Group"
    let GroupAdd = Group -<- ntoa<unit> !!"Add"
    let GroupRemoving = Group -<- ntoa<unit> !!"Removing"
    let GroupChange = Group -<- ntoa<ParticipantChangeData<Group, World>> !!"Change"
    let Entity = ntoa<obj> !!"Entity"
    let EntityAdd = Entity -<- ntoa<unit> !!"Add"
    let EntityRemoving = Entity -<- ntoa<unit> !!"Removing"
    let EntityChange = Entity -<- ntoa<ParticipantChangeData<Entity, World>> !!"Change"