namespace Nu
open System
open OpenTK
open Nu
open Nu.Constants

module WorldConstants =

    let GameAddress = Address<Game>.empty
    let DefaultScreenAddress = ntoa<Screen> DefaultGroupName
    let DefaultGroupAddress = satoga DefaultScreenAddress DefaultGroupName
    let DefaultEntityAddress = gatoea DefaultGroupAddress DefaultEntityName
    let AnyEventAddress = stoa<obj> "*"
    let TickEventAddress = stoa<unit> "Tick"
    let AddEventAddress = stoa<unit> "Add"
    let RemovingEventAddress = stoa<unit> "Removing"
    let IncomingStartEventAddress = stoa<unit> "Incoming/Start"
    let OutgoingStartEventAddress = stoa<unit> "Outgoing/Start"
    let IncomingFinishEventAddress = stoa<unit> "Incoming/Finish"
    let OutgoingFinishEventAddress = stoa<unit> "Outgoing/Finish"
    let SelectEventAddress = stoa<unit> "Select"
    let DeselectEventAddress = stoa<unit> "Deselect"
    let DownEventAddress = stoa<unit> "Down"
    let UpEventAddress = stoa<unit> "Up"
    let ClickEventAddress = stoa<unit> "Click"
    let OnEventAddress = stoa<unit> "On"
    let OffEventAddress = stoa<unit> "Off"
    let TouchEventAddress = stoa<Vector2> "Touch"
    let UntouchEventAddress = stoa<Vector2> "Untouch"
    let MouseEventAddress = stoa<obj> "Mouse"
    let MouseMoveEventAddress = MouseEventAddress -<- stoa<MouseMoveData> "Move"
    let MouseDragEventAddress = MouseEventAddress -<- stoa<MouseMoveData> "Drag"
    let MouseLeftEventAddress = MouseEventAddress -<- stoa<MouseButtonData> "Left"
    let MouseCenterEventAddress = MouseEventAddress -<- stoa<MouseButtonData> "Center"
    let MouseRightEventAddress = MouseEventAddress -<- stoa<MouseButtonData> "Right"
    let MouseX1EventAddress = MouseEventAddress -<- stoa<MouseButtonData> "X1"
    let MouseX2EventAddress = MouseEventAddress -<- stoa<MouseButtonData> "X2"
    let MouseLeftDownEventAddress = MouseLeftEventAddress -|- stoa "Down"
    let MouseLeftUpEventAddress = MouseLeftEventAddress -|- stoa "Up"
    let MouseLeftChangeEventAddress = MouseLeftEventAddress -|- stoa "Change"
    let MouseCenterDownEventAddress = MouseCenterEventAddress -|- stoa "Down"
    let MouseCenterUpEventAddress = MouseCenterEventAddress -|- stoa "Up"
    let MouseCenterChangeEventAddress = MouseCenterEventAddress -|- stoa "Change"
    let MouseRightDownEventAddress = MouseRightEventAddress -|- stoa "Down"
    let MouseRightUpEventAddress = MouseRightEventAddress -|- stoa "Up"
    let MouseRightChangeEventAddress = MouseRightEventAddress -|- stoa "Change"
    let MouseX1DownEventAddress = MouseX1EventAddress -|- stoa "Down"
    let MouseX1UpEventAddress = MouseX1EventAddress -|- stoa "Up"
    let MouseX1ChangeEventAddress = MouseX1EventAddress -|- stoa "Change"
    let MouseX2DownEventAddress = MouseX2EventAddress -|- stoa "Down"
    let MouseX2UpEventAddress = MouseX2EventAddress -|- stoa "Up"
    let MouseX2ChangeEventAddress = MouseX2EventAddress -|- stoa "Change"
    let KeyboardKeyEventAddress = stoa<obj> "KeyboardKey"
    let KeyboardKeyDownEventAddress = MouseEventAddress -<- stoa<KeyboardKeyData> "Down"
    let KeyboardKeyUpEventAddress = MouseEventAddress -<- stoa<KeyboardKeyData> "Up"
    let KeyboardKeyChangeEventAddress = MouseEventAddress -<- stoa<KeyboardKeyData> "Change"
    let CollisionEventAddress = stoa<CollisionData> "Collision"
    let SimulantChangeEventAddress = stoa<Simulant SimulantChangeData> "Change"
    let GameChangeEventAddress = Address.changeType<Simulant SimulantChangeData, Game SimulantChangeData> SimulantChangeEventAddress
    let ScreenChangeEventAddress = Address.changeType<Simulant SimulantChangeData, Screen SimulantChangeData> SimulantChangeEventAddress
    let GroupChangeEventAddress = Address.changeType<Simulant SimulantChangeData, Group SimulantChangeData> SimulantChangeEventAddress
    let EntityChangeEventAddress = Address.changeType<Simulant SimulantChangeData, Entity SimulantChangeData> SimulantChangeEventAddress
    let DefaultDissolveImage = { PackageName = DefaultPackageName; AssetName = "Image8" }