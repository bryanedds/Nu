// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

/// The data for a mouse move event.
type MouseMoveData =
    { Position : Vector2 }

/// The data for a mouse button event.
type MouseButtonData =
    { Position : Vector2
      Button : MouseButton
      Down : bool }

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
    { BodyShapeCollider : ShapeIndex
      BodyShapeCollidee : ShapeIndex
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
    { BodyShapeSeparator : ShapeIndex
      BodyShapeSeparatee : ShapeIndex }

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

    let Wildcard = Prime.Events.Wildcard
    let Register = stoa<unit> "Register/Event"
    let Unregistering = stoa<unit> "Unregistering/Event"
    let Change propertyName = rtoa<ChangeData> [|"Change"; propertyName; "Event"|]
    let LifeCycle simulantTypeName = rtoa<LifeCycleData> [|"LifeCycle"; simulantTypeName; "Event"|]
    let PreUpdate = stoa<unit> "PreUpdate/Event"
    let Update = stoa<unit> "Update/Event"
    let PostUpdate = stoa<unit> "PostUpdate/Event"
    let Render = stoa<unit> "Render/Event"
    let Select = stoa<unit> "Select/Event"
    let Deselecting = stoa<unit> "Deselecting/Event"
    let Mount = stoa<MountData> "Mount/Event"
    let Unmount = stoa<MountData> "Unmount/Event"
    let Integration = stoa<IntegrationData> "Integration/Event"
    let BodyAdding = stoa<BodyId> "Body/Adding/Event"
    let BodyRemoving = stoa<BodyId> "Body/Removing/Event"
    let BodyCollision = stoa<BodyCollisionData> "BodyCollision/Event"
    let BodySeparationImplicit = stoa<BodySeparationImplicitData> "BodySeparationImplicit/Event"
    let BodySeparationExplicit = stoa<BodySeparationExplicitData> "BodySeparationExplicit/Event"
    let BodyTransform = stoa<BodyTransformData> "BodyTransform/Event"
    let Click = stoa<unit> "Click/Event"
    let Down = stoa<unit> "Down/Event"
    let Up = stoa<unit> "Up/Event"
    let Toggle = stoa<bool> "Toggle/Event"
    let Toggled = stoa<unit> "Toggled/Event"
    let Untoggled = stoa<unit> "Untoggled/Event"
    let Dial = stoa<bool> "Dial/Event"
    let Dialed = stoa<unit> "Dialed/Event"
    let Undialed = stoa<unit> "Undialed/Event"
    let Touch = stoa<Vector2> "Touch/Event"
    let Touching = stoa<Vector2> "Touching/Event"
    let Untouch = stoa<Vector2> "Untouch/Event"
    let IncomingStart = stoa<unit> "Incoming/Start/Event"
    let IncomingFinish = stoa<unit> "Incoming/Finish/Event"
    let OutgoingStart = stoa<unit> "Outgoing/Start/Event"
    let OutgoingFinish = stoa<unit> "Outgoing/Finish/Event"
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
    let GamepadDirectionChange (index : int) = rtoa<GamepadDirectionData> [|"Gamepad"; "Direction" + string index + "Change"; "Event"|]
    let GamepadButtonChange (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Change"; "Event"|]
    let GamepadButtonDown (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Down"; "Event"|]
    let GamepadButtonUp (index : int) = rtoa<GamepadButtonData> [|"Gamepad"; "Button" + string index + "Up"; "Event"|]
    let AssetsReload = stoa<unit> "Assets/Reload/Event"