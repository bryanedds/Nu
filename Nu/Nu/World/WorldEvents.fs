// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime

/// The data for a mouse move event.
type MouseMoveData =
    { Position : Vector2 }

/// The data for a mouse button event.
type MouseButtonData =
    { Position : Vector2
      Button : MouseButton
      Down : bool }

/// The data for a mouse wheel event.
type MouseWheelData =
    { Travel : single }

/// The data for a keyboard key event.
type KeyboardKeyData =
    { KeyboardKey : KeyboardKey
      Repeated : bool
      Down : bool }

/// The data for a gamepad button event.
type GamepadDirectionData =
    { GamepadDirection : GamepadDirection }

/// The data for a gamepad button event.
type GamepadButtonData =
    { GamepadButton : GamepadButton
      Down : bool }

/// The data for a text input event.
type TextInputData =
    { TextInput : char }

/// The data for a physics integration event.
type IntegrationData =
    { /// The integration messages sourced from a physics engine.
      /// Do NOT change the content of this collection as it is exposed as a SArray for speed.
      IntegrationMessages : IntegrationMessage SArray }

/// The data of a body transform event.
type TransformData =
    { BodyId : BodyId
      Position : Vector3
      Rotation : Quaternion }

/// The data for a collision event.
type BodyCollisionData =
    { BodyShapeCollider : BodyShapeIndex
      BodyShapeCollidee : BodyShapeIndex
      Normal : Vector3 }

/// The implicit data for a separation event.
/// Unfortunately, due to the fact that physics system itself does not raise separation events until the following
/// frame, we need both an implicit and explicit body separation representation and the user MUST handle both!
type BodySeparationImplicitData =
    { BodyId : BodyId }

/// The explicit data for a separation event.
/// Unfortunately, due to the fact that physics system itself does not raise separation events until the following
/// frame, we need both an implicit and explicit body separation representation and the user MUST handle both!
type BodySeparationExplicitData =
    { BodyShapeSeparator : BodyShapeIndex
      BodyShapeSeparatee : BodyShapeIndex }

/// Tje data for describing a change in transform.
type BodyTransformData =
    { BodyCenter : Vector3
      BodyRotation : Quaternion
      BodyLinearVelocity : Vector3
      BodyAngularVelocity : Vector3 }

/// The data for a life cycle event.
type LifeCycleData =
    | RegisterData of Simulant
    | UnregisteringData of Simulant
    | MountOptChangeData of Entity Relation option * Entity Relation option * Entity

/// The data for describing a mounting or unmounting event.
type MountData =
    { Mount : Entity
      Mounter : Entity }

[<RequireQualifiedAccess>]
module Events =

    let RegisterEvent = stoa<unit> "Register/Event"
    let UnregisteringEvent = stoa<unit> "Unregistering/Event"
    let ChangeEvent propertyName = rtoa<ChangeData> [|"Change"; propertyName; "Event"|]
    let LifeCycleEvent simulantTypeName = rtoa<LifeCycleData> [|"LifeCycle"; simulantTypeName; "Event"|]
    let PreUpdateEvent = stoa<unit> "PreUpdate/Event"
    let UpdateEvent = stoa<unit> "Update/Event"
    let PostUpdateEvent = stoa<unit> "PostUpdate/Event"
    let TimeUpdateEvent = stoa<unit> "TimeUpdate/Event"
    let SelectEvent = stoa<unit> "Select/Event"
    let DeselectingEvent = stoa<unit> "Deselecting/Event"
    let MountEvent = stoa<MountData> "Mount/Event"
    let UnmountEvent = stoa<MountData> "Unmount/Event"
    let IntegrationEvent = stoa<IntegrationData> "Integration/Event"
    let BodyAddingEvent = stoa<BodyId> "Body/Adding/Event"
    let BodyRemovingEvent = stoa<BodyId> "Body/Removing/Event"
    let BodyCollisionEvent = stoa<BodyCollisionData> "BodyCollision/Event"
    let BodySeparationImplicitEvent = stoa<BodySeparationImplicitData> "BodySeparationImplicit/Event"
    let BodySeparationExplicitEvent = stoa<BodySeparationExplicitData> "BodySeparationExplicit/Event"
    let BodyTransformEvent = stoa<BodyTransformData> "BodyTransform/Event"
    let ClickEvent = stoa<unit> "Click/Event"
    let DownEvent = stoa<unit> "Down/Event"
    let UpEvent = stoa<unit> "Up/Event"
    let ToggleEvent = stoa<bool> "Toggle/Event"
    let ToggledEvent = stoa<unit> "Toggled/Event"
    let UntoggledEvent = stoa<unit> "Untoggled/Event"
    let DialEvent = stoa<bool> "Dial/Event"
    let DialedEvent = stoa<unit> "Dialed/Event"
    let UndialedEvent = stoa<unit> "Undialed/Event"
    let TouchEvent = stoa<Vector2> "Touch/Event"
    let TouchingEvent = stoa<Vector2> "Touching/Event"
    let UntouchEvent = stoa<Vector2> "Untouch/Event"
    let IncomingStartEvent = stoa<unit> "Incoming/Start/Event"
    let IncomingFinishEvent = stoa<unit> "Incoming/Finish/Event"
    let OutgoingStartEvent = stoa<unit> "Outgoing/Start/Event"
    let OutgoingFinishEvent = stoa<unit> "Outgoing/Finish/Event"
    let MouseMoveEvent = stoa<MouseMoveData> "Mouse/Move/Event"
    let MouseDragEvent = stoa<MouseMoveData> "Mouse/Drag/Event"
    let MouseWheelEvent = stoa<MouseWheelData> "Mouse/Wheel/Event"
    let MouseLeftChangeEvent = stoa<MouseButtonData> "Mouse/Left/Change/Event"
    let MouseLeftDownEvent = stoa<MouseButtonData> "Mouse/Left/Down/Event"
    let MouseLeftUpEvent = stoa<MouseButtonData> "Mouse/Left/Up/Event"
    let MouseMiddleChangeEvent = stoa<MouseButtonData> "Mouse/Middle/Change/Event"
    let MouseMiddleDownEvent = stoa<MouseButtonData> "Mouse/Middle/Down/Event"
    let MouseMiddleUpEvent = stoa<MouseButtonData> "Mouse/Middle/Up/Event"
    let MouseRightChangeEvent = stoa<MouseButtonData> "Mouse/Right/Change/Event"
    let MouseRightDownEvent = stoa<MouseButtonData> "Mouse/Right/Down/Event"
    let MouseRightUpEvent = stoa<MouseButtonData> "Mouse/Right/Up/Event"
    let MouseX1ChangeEvent = stoa<MouseButtonData> "Mouse/X1/Change/Event"
    let MouseX1DownEvent = stoa<MouseButtonData> "Mouse/X1/Down/Event"
    let MouseX1UpEvent = stoa<MouseButtonData> "Mouse/X1/Up/Event"
    let MouseX2ChangeEvent = stoa<MouseButtonData> "Mouse/X2/Change/Event"
    let MouseX2DownEvent = stoa<MouseButtonData> "Mouse/X2/Down/Event"
    let MouseX2UpEvent = stoa<MouseButtonData> "Mouse/X2/Up/Event"
    let KeyboardKeyChangeEvent = stoa<KeyboardKeyData> "KeyboardKey/Change/Event"
    let KeyboardKeyDownEvent = stoa<KeyboardKeyData> "KeyboardKey/Down/Event"
    let KeyboardKeyUpEvent = stoa<KeyboardKeyData> "KeyboardKey/Up/Event"
    let GamepadDirectionChangeEvent (index : int) = rtoa<GamepadDirectionData> [|"Gamepad"; "Direction" + string index + "Change"; "Event"|]
    let GamepadButtonChangeEvent (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Change"; "Event"|]
    let GamepadButtonDownEvent (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Down"; "Event"|]
    let GamepadButtonUpEvent (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Up"; "Event"|]
    let TextInputEvent = stoa<TextInputData> "TextInput/Event"
    let AssetsReloadEvent = stoa<unit> "Assets/Reload/Event"