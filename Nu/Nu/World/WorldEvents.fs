﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open Prime

/// The data for a life-cycle event.
type LifeCycleEventData =
    | RegisterData of Simulant
    | UnregisteringData of Simulant
    | MountOptChangeData of Entity Relation option * Entity Relation option * Entity

/// The data for a screen selection event.
type SelectionEventData =
    | Select
    | IncomingStart
    | IncomingFinish
    | OutgoingStart
    | OutgoingFinish
    | Deselecting

/// The data for a change in a simulant.
type KeyedValueChangeData =
    { Key : string
      PreviousOpt : obj option
      ValueOpt : obj option }

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

/// The data for a text edit event.
type TextEditData =
    { Text : string
      Cursor : int }

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

/// The data for a penetration event.
type BodyPenetrationData =
    { BodyShapePenetrator : BodyShapeIndex
      BodyShapePenetratee : BodyShapeIndex
      Normal : Vector3 }

/// The explicit data for a separation event.
/// Unfortunately, due to the fact that physics system itself does not raise separation events until the following
/// frame, we need both an implicit and explicit body separation representation and the user MUST handle both!
type BodySeparationExplicitData =
    { BodyShapeSeparator : BodyShapeIndex
      BodyShapeSeparatee : BodyShapeIndex }

/// The implicit data for a separation event.
/// Unfortunately, due to the fact that physics system itself does not raise separation events until the following
/// frame, we need both an implicit and explicit body separation representation and the user MUST handle both!
type BodySeparationImplicitData =
    { BodyId : BodyId }

/// Tje data for describing a change in transform.
type BodyTransformData =
    { BodyCenter : Vector3
      BodyRotation : Quaternion
      BodyLinearVelocity : Vector3
      BodyAngularVelocity : Vector3 }

/// The data for a physics body event.
type BodyEventData =
    | BodyPenetrationData of BodyPenetrationData
    | BodySeparationExplicitData of BodySeparationExplicitData
    | BodySeparationImplicitData of BodySeparationImplicitData
    | BodyTransformData of BodyTransformData

/// The data of a body joint break event.
type BodyJointBreakData =
    { BodyJointId : BodyJointId
      BreakingPoint : single
      BreakingOverflow : single }

/// The data for describing a mounting or unmounting event.
type MountData =
    { Mount : Entity
      Mounter : Entity }

/// The data for describing an animation trigger event.
type SpineSkeletonAnimationTriggerData =
    | SpineSkeletonAnimationStartData of Spine.TrackEntry
    | SpineSkeletonAnimationInterruptData of Spine.TrackEntry
    | SpineSkeletonAnimationCompleteData of Spine.TrackEntry
    | SpineSkeletonAnimationEndData of Spine.TrackEntry
    | SpineSkeletonAnimationEventData of Spine.TrackEntry * Spine.Event

[<RequireQualifiedAccess>]
module Events =

    let RegisterEvent = stoa<unit> "Register/Event"
    let UnregisteringEvent = stoa<unit> "Unregistering/Event"
    let ChangeEvent propertyName = rtoa<ChangeData> [|"Change"; propertyName; "Event"|]
    let LifeCycleEvent simulantTypeName = rtoa<LifeCycleEventData> [|"LifeCycle"; simulantTypeName; "Event"|]
    let PreUpdateEvent = stoa<unit> "PreUpdate/Event"
    let UpdateEvent = stoa<unit> "Update/Event"
    let PostUpdateEvent = stoa<unit> "PostUpdate/Event"
    let TimeUpdateEvent = stoa<unit> "TimeUpdate/Event"
    let KeyedValueChangeEvent key = rtoa<KeyedValueChangeData> [|"KeyedValue"; key; "Change"; "Event"|]
    let SelectEvent = stoa<unit> "Select/Event"
    let DeselectingEvent = stoa<unit> "Deselecting/Event"
    let IncomingStartEvent = stoa<unit> "Incoming/Start/Event"
    let IncomingFinishEvent = stoa<unit> "Incoming/Finish/Event"
    let OutgoingStartEvent = stoa<unit> "Outgoing/Start/Event"
    let OutgoingFinishEvent = stoa<unit> "Outgoing/Finish/Event"
    let MountEvent = stoa<MountData> "Mount/Event"
    let UnmountEvent = stoa<MountData> "Unmount/Event"
    let IntegrationEvent = stoa<IntegrationData> "Integration/Event"
    let BodyAddingEvent = stoa<BodyId> "Body/Adding/Event"
    let BodyRemovingEvent = stoa<BodyId> "Body/Removing/Event"
    let BodyPenetrationEvent = stoa<BodyPenetrationData> "BodyPenetration/Event"
    let BodySeparationExplicitEvent = stoa<BodySeparationExplicitData> "BodySeparationExplicit/Event"
    let BodySeparationImplicitEvent = stoa<BodySeparationImplicitData> "BodySeparationImplicit/Event"
    let BodyTransformEvent = stoa<BodyTransformData> "BodyTransform/Event"
    let BodyJointBreakEvent = stoa<BodyJointBreakData> "BodyJointBreak/Event"
    let SpineSkeletonAnimationTriggerEvent = stoa<SpineSkeletonAnimationTriggerData> "SpineSkeletonAnimationTrigger/Event"
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
    let TextEditEvent = stoa<TextEditData> "TextEdit/Event"
    let FocusEvent = stoa<unit> "Focus/Event"
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
    let CodeReloadEvent = stoa<unit> "Code/Reload/Event"
    let ExitRequestEvent = stoa<unit> "ExitRequest/Event"