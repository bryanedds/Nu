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

[<RequireQualifiedAccess>]
module Events =

    let Wildcard = Prime.Events.Wildcard
    let Subscribe = stoa<obj Address> "Subscribe/Event"
    let Unsubscribe = stoa<obj Address> "Unsubscribe/Event"
    let Update = stoa<unit> "Update/Event"
    let PostUpdate = stoa<unit> "PostUpdate/Event"
    let Select = stoa<unit> "Select/Event"
    let Deselect = stoa<unit> "Deselect/Event"
    let Click = stoa<unit> "Click/Event"
    let Down = stoa<unit> "Down/Event"
    let Up = stoa<unit> "Up/Event"
    let On = stoa<unit> "On/Event"
    let Off = stoa<unit> "Off/Event"
    let Touch = stoa<Vector2> "Touch/Event"
    let Untouch = stoa<Vector2> "Untouch/Event"
    let MouseMove = stoa<MouseMoveData> "Mouse/Move/Event"
    let MouseDrag = stoa<MouseMoveData> "Mouse/Drag/Event"
    let MouseLeftChange = stoa<MouseButtonData> "Mouse/Left/Change/Event"
    let MouseLeftDown = stoa<MouseButtonData> "Mouse/Left/Down/Event"
    let MouseLeftUp = stoa<MouseButtonData> "Mouse/Left/Up/Event"
    let MouseCenterChange = stoa<MouseButtonData> "Mouse/Center/Change/Event"
    let MouseCenterDown = stoa<MouseButtonData> "Mouse/Center/Down/Event"
    let MouseCenterUp = stoa<MouseButtonData> "Mouse/Center/Up/Event"
    let MouseRightChange = stoa<MouseButtonData> "Mouse/Right/Change/Event"
    let MouseRightDown = stoa<MouseButtonData> "Mouse/Right/Down/Event"
    let MouseRightUp = stoa<MouseButtonData> "Mouse/Right/Up/Event"
    let MouseX1Change = stoa<MouseButtonData> "Mouse/X1/Change/Event"
    let MouseX1Down = stoa<MouseButtonData> "Mouse/X1/Down/Event"
    let MouseX1Up = stoa<MouseButtonData> "Mouse/X1/Up/Event"
    let MouseX2Change = stoa<MouseButtonData> "Mouse/X2/Change/Event"
    let MouseX2Down = stoa<MouseButtonData> "Mouse/X2/Down/Event"
    let MouseX2Up = stoa<MouseButtonData> "Mouse/X2/Up/Event"
    let KeyboardKeyChange = stoa<KeyboardKeyData> "KeyboardKey/Change/Event"
    let KeyboardKeyDown = stoa<KeyboardKeyData> "KeyboardKey/Down/Event"
    let KeyboardKeyUp = stoa<KeyboardKeyData> "KeyboardKey/Up/Event"
    let Collision = stoa<CollisionData> "Collision/Event"
    let IncomingStart = stoa<unit> "Incoming/Start/Event"
    let IncomingFinish = stoa<unit> "Incoming/Finish/Event"
    let OutgoingStart = stoa<unit> "Outgoing/Start/Event"
    let OutgoingFinish = stoa<unit> "Outgoing/Finish/Event"
    let AssetsReload = stoa<unit> "Assets/Reload/Event"
    let GameChange propertyName = stoa<ParticipantChangeData<Game, World>> ("Game/Change/" + propertyName + "/Event")
    let ScreenAdd = stoa<unit> "Screen/Add/Event"
    let ScreenRemoving = stoa<unit> "Screen/Removing/Event"
    let ScreenChange propertyName = stoa<ParticipantChangeData<Screen, World>> ("Screen/Change/" + propertyName + "/Event")
    let GroupAdd = stoa<unit> "Group/Add/Event"
    let GroupRemoving = stoa<unit> "Group/Removing/Event"
    let GroupChange propertyName = stoa<ParticipantChangeData<Group, World>> ("Group/Change/" + propertyName + "/Event")
    let EntityAdd = stoa<unit> "Entity/Add/Event"
    let EntityRemoving = stoa<unit> "Entity/Removing/Event"
    let EntityChange propertyName = stoa<ParticipantChangeData<Entity, World>> ("Entity/Change/" + propertyName + "/Event")