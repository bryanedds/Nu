// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open System.Collections.Concurrent
open Prime

/// The data for a life-cycle event.
type LifeCycleEventData =
    | RegisterData of Simulant
    | UnregisteringData of Simulant
    | MountOptChangeData of Entity Address option * Entity Address option * Entity

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
      Caret : int }

/// The data for a physics integration event.
type IntegrationData =
    { /// The integration messages sourced from a physics engine.
      /// Do NOT change the content of this collection as it is exposed as an SArray for speed.
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

/// The data for an explicit separation event.
/// Unfortunately, due to the fact that physics system itself does not raise separation events until the following
/// frame, we need both an implicit and explicit body separation representation and the user MUST handle both!
type BodySeparationExplicitData =
    { BodyShapeSeparator : BodyShapeIndex
      BodyShapeSeparatee : BodyShapeIndex }

/// The data for an implicit separation event.
/// Unfortunately, due to the fact that physics system itself does not raise separation events until the following
/// frame, we need both an implicit and explicit body separation representation and the user MUST handle both!
type BodySeparationImplicitData =
    { BodyId : BodyId }

/// The data for describing a change in transform.
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

/// Engine and simulation events that come with Nu.
[<RequireQualifiedAccess>]
module Events =

    /// Raised when a simulant is registered.
    let RegisterEvent = stoa<unit> "Register/Event"

    /// Raised when a simulant is unregistered.
    let UnregisteringEvent = stoa<unit> "Unregistering/Event"

    /// Raised when a simulant's property is changed.
    let ChangeEvent propertyName = rtoa<ChangeData> [|"Change"; propertyName; "Event"|]

    /// Raised when a simulant is registered, unregistered, or has its mounting state changed.
    let LifeCycleEvent simulantTypeName = rtoa<LifeCycleEventData> [|"LifeCycle"; simulantTypeName; "Event"|]

    /// Raised when pre-update processing on a simulant occurs (except on entities for efficiency).
    let PreUpdateEvent = stoa<unit> "PreUpdate/Event"

    /// Raised when update processing on a simulant occurs (except on entities for efficiency).
    let UpdateEvent = stoa<unit> "Update/Event"

    /// Raised when post-update processing on a simulant occurs (except on entities for efficiency).
    let PostUpdateEvent = stoa<unit> "PostUpdate/Event"

    /// Raised when the engine's representation of current time changes.
    let TimeUpdateEvent = stoa<unit> "TimeUpdate/Event"

    /// Raised when a key-value pair is changed in the world.
    let KeyedValueChangeEvent key = rtoa<KeyedValueChangeData> [|"KeyedValue"; key; "Change"; "Event"|]

    /// Raised when a screen is selected.
    let SelectEvent = stoa<unit> "Select/Event"

    /// Raised when a screen is deselected.
    let DeselectingEvent = stoa<unit> "Deselecting/Event"

    /// Raised when a screen has just started transitioning in.
    let IncomingStartEvent = stoa<unit> "Incoming/Start/Event"

    /// Raised when a screen has just finished transitioning in.
    let IncomingFinishEvent = stoa<unit> "Incoming/Finish/Event"

    /// Raised when a screen has just started transitioning out.
    let OutgoingStartEvent = stoa<unit> "Outgoing/Start/Event"

    /// Raised when a screen has just finished transitioning out.
    let OutgoingFinishEvent = stoa<unit> "Outgoing/Finish/Event"

    /// Raised when an entity is mounted.
    let MountEvent = stoa<MountData> "Mount/Event"

    /// Raised when an entity is unmounted.
    let UnmountEvent = stoa<MountData> "Unmount/Event"

    /// Raised when physics integration occurs, which is when a physics engine processes its messages.
    let IntegrationEvent = stoa<IntegrationData> "Integration/Event"

    /// Raised when a body is added to a physics system.
    let BodyAddingEvent = stoa<BodyId> "Body/Adding/Event"

    /// Raised when a body is removed from a physics system.
    let BodyRemovingEvent = stoa<BodyId> "Body/Removing/Event"

    /// Raised when a physics body is penetrated.
    let BodyPenetrationEvent = stoa<BodyPenetrationData> "BodyPenetration/Event"

    /// Raised when a physics body is separated explicitly by a physics engine itegration.
    let BodySeparationExplicitEvent = stoa<BodySeparationExplicitData> "BodySeparationExplicit/Event"

    /// Raised when a physics body is separated implicitly, such as by its containing entity being destroyed.
    let BodySeparationImplicitEvent = stoa<BodySeparationImplicitData> "BodySeparationImplicit/Event"

    /// Raised when a physics body is transformed in space.
    let BodyTransformEvent = stoa<BodyTransformData> "BodyTransform/Event"

    /// Raised when a physics joint is broken.
    let BodyJointBreakEvent = stoa<BodyJointBreakData> "BodyJointBreak/Event"

    /// Raised when 2d gravity is changed.
    let Gravity2dChange = stoa<ChangeData> "Gravity2d/Change/Event"

    /// Raised when 3d gravity is changed.
    let Gravity3dChange = stoa<ChangeData> "Gravity3d/Change/Event"

    /// Raised when a fluid emitter updates.
    let FluidEmitterUpdateEvent = stoa<FluidEmitterMessage> "FluidEmitterUpdate/Event"

    /// Raised when a Spine skeleton animation event is triggered.
    let SpineSkeletonAnimationTriggerEvent = stoa<SpineSkeletonAnimationTriggerData> "SpineSkeletonAnimationTrigger/Event"

    /// Raised when a button is clicked.
    let ClickEvent = stoa<unit> "Click/Event"

    /// Raised when a button is pressed.
    let DownEvent = stoa<unit> "Down/Event"

    /// Raised when a button is released.
    let UpEvent = stoa<unit> "Up/Event"

    /// Raised when a button is toggled.
    let ToggleEvent = stoa<bool> "Toggle/Event"

    /// Raised when a button is pressed.
    let ToggledEvent = stoa<unit> "Toggled/Event"

    /// Raised when a button is released.
    let UntoggledEvent = stoa<unit> "Untoggled/Event"

    /// Raised when a radio button is dialed.
    let DialEvent = stoa<bool> "Dial/Event"

    /// Raised when a dial is pressed.
    let DialedEvent = stoa<unit> "Dialed/Event"

    /// Raised when a dial is released.
    let UndialedEvent = stoa<unit> "Undialed/Event"

    /// Raised while a feeler is touched.
    let TouchEvent = stoa<Vector2> "Touch/Event"

    /// Raised when a feeler is initially touched.
    let TouchingEvent = stoa<Vector2> "Touching/Event"

    /// Raised when a feeler is no longer touched.
    let UntouchEvent = stoa<Vector2> "Untouch/Event"

    /// Raised when the content of a text gui is edited.
    let TextEditEvent = stoa<TextEditData> "TextEdit/Event"

    /// Raised when a text gui gains focus.
    let FocusEvent = stoa<unit> "Focus/Event"

    /// Raised when mouse movement occurs.
    let MouseMoveEvent = stoa<MouseMoveData> "Mouse/Move/Event"

    /// Raised when mouse movement occurs while a mouse button is down.
    let MouseDragEvent = stoa<MouseMoveData> "Mouse/Drag/Event"

    /// Raised when the mouse wheel is scrolled.
    let MouseWheelEvent = stoa<MouseWheelData> "Mouse/Wheel/Event"

    /// Raised when the left mouse button is pressed or released.
    let MouseLeftChangeEvent = stoa<MouseButtonData> "Mouse/Left/Change/Event"

    /// Raised when the left mouse button is pressed.
    let MouseLeftDownEvent = stoa<MouseButtonData> "Mouse/Left/Down/Event"

    /// Raised when the left mouse button is released.
    let MouseLeftUpEvent = stoa<MouseButtonData> "Mouse/Left/Up/Event"

    /// Raised when the middle mouse button is pressed or released.
    let MouseMiddleChangeEvent = stoa<MouseButtonData> "Mouse/Middle/Change/Event"

    /// Raised when the middle mouse button is pressed.
    let MouseMiddleDownEvent = stoa<MouseButtonData> "Mouse/Middle/Down/Event"

    /// Raised when the middle mouse button is released.
    let MouseMiddleUpEvent = stoa<MouseButtonData> "Mouse/Middle/Up/Event"

    /// Raised when the right mouse button is pressed or released.
    let MouseRightChangeEvent = stoa<MouseButtonData> "Mouse/Right/Change/Event"

    /// Raised when the right mouse button is pressed.
    let MouseRightDownEvent = stoa<MouseButtonData> "Mouse/Right/Down/Event"

    /// Raised when the right mouse button is released.
    let MouseRightUpEvent = stoa<MouseButtonData> "Mouse/Right/Up/Event"

    /// Raised when the X1 mouse button is pressed or released.
    let MouseX1ChangeEvent = stoa<MouseButtonData> "Mouse/X1/Change/Event"

    /// Raised when the X1 mouse button is pressed.
    let MouseX1DownEvent = stoa<MouseButtonData> "Mouse/X1/Down/Event"

    /// Raised when the X1 mouse button is released.
    let MouseX1UpEvent = stoa<MouseButtonData> "Mouse/X1/Up/Event"

    /// Raised when the X2 mouse button is pressed or released.
    let MouseX2ChangeEvent = stoa<MouseButtonData> "Mouse/X2/Change/Event"

    /// Raised when the X2 mouse button is pressed.
    let MouseX2DownEvent = stoa<MouseButtonData> "Mouse/X2/Down/Event"

    /// Raised when the X2 mouse button is released.
    let MouseX2UpEvent = stoa<MouseButtonData> "Mouse/X2/Up/Event"

    /// Raised when a keyboard key is pressed or released.
    let KeyboardKeyChangeEvent = stoa<KeyboardKeyData> "KeyboardKey/Change/Event"

    /// Raised when a keyboard key is pressed.
    let KeyboardKeyDownEvent = stoa<KeyboardKeyData> "KeyboardKey/Down/Event"

    /// Raised when a keyboard key is released.
    let KeyboardKeyUpEvent = stoa<KeyboardKeyData> "KeyboardKey/Up/Event"

    /// Raised when a gamepad direction is changed.
    let GamepadDirectionChangeEvent (index : int) = rtoa<GamepadDirectionData> [|"Gamepad"; "Direction" + string index + "Change"; "Event"|]

    /// Raised when a gamepad button is pressed or released.
    let GamepadButtonChangeEvent (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Change"; "Event"|]

    /// Raised when a gamepad direction is pressed.
    let GamepadButtonDownEvent (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Down"; "Event"|]

    /// Raised when a gamepad direction is released.
    let GamepadButtonUpEvent (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Up"; "Event"|]

    /// Raised when text input is received from the OS.
    let TextInputEvent = stoa<TextInputData> "TextInput/Event"

    /// Raised when game assets are reloaded.
    let AssetsReloadEvent = stoa<unit> "Assets/Reload/Event"

    /// Raised when game code is reloaded.
    let CodeReloadEvent = stoa<unit> "Code/Reload/Event"

    /// Raised when the OS issues an exit request to the application.
    let ExitRequestEvent = stoa<unit> "ExitRequest/Event"