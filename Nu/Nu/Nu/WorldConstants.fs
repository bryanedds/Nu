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
    let AnyEventAddress = ntoa<obj> "*"
    let TickEventAddress = ntoa<unit> "Tick"
    let AddEventAddress = ntoa<unit> "Add"
    let RemovingEventAddress = ntoa<unit> "Removing"
    let SelectEventAddress = ntoa<unit> "Select"
    let DeselectEventAddress = ntoa<unit> "Deselect"
    let DownEventAddress = ntoa<unit> "Down"
    let UpEventAddress = ntoa<unit> "Up"
    let ClickEventAddress = ntoa<unit> "Click"
    let OnEventAddress = ntoa<unit> "On"
    let OffEventAddress = ntoa<unit> "Off"
    let TouchEventAddress = ntoa<Vector2> "Touch"
    let UntouchEventAddress = ntoa<Vector2> "Untouch"
    let MouseEventAddress = ntoa<obj> "Mouse"
    let MouseMoveEventAddress = MouseEventAddress -<- ntoa<MouseMoveData> "Move"
    let MouseDragEventAddress = MouseEventAddress -<- ntoa<MouseMoveData> "Drag"
    let MouseLeftEventAddress = MouseEventAddress -<- ntoa<MouseButtonData> "Left"
    let MouseCenterEventAddress = MouseEventAddress -<- ntoa<MouseButtonData> "Center"
    let MouseRightEventAddress = MouseEventAddress -<- ntoa<MouseButtonData> "Right"
    let MouseX1EventAddress = MouseEventAddress -<- ntoa<MouseButtonData> "X1"
    let MouseX2EventAddress = MouseEventAddress -<- ntoa<MouseButtonData> "X2"
    let MouseLeftDownEventAddress = MouseLeftEventAddress -|- ntoa "Down"
    let MouseLeftUpEventAddress = MouseLeftEventAddress -|- ntoa "Up"
    let MouseLeftChangeEventAddress = MouseLeftEventAddress -|- ntoa "Change"
    let MouseCenterDownEventAddress = MouseCenterEventAddress -|- ntoa "Down"
    let MouseCenterUpEventAddress = MouseCenterEventAddress -|- ntoa "Up"
    let MouseCenterChangeEventAddress = MouseCenterEventAddress -|- ntoa "Change"
    let MouseRightDownEventAddress = MouseRightEventAddress -|- ntoa "Down"
    let MouseRightUpEventAddress = MouseRightEventAddress -|- ntoa "Up"
    let MouseRightChangeEventAddress = MouseRightEventAddress -|- ntoa "Change"
    let MouseX1DownEventAddress = MouseX1EventAddress -|- ntoa "Down"
    let MouseX1UpEventAddress = MouseX1EventAddress -|- ntoa "Up"
    let MouseX1ChangeEventAddress = MouseX1EventAddress -|- ntoa "Change"
    let MouseX2DownEventAddress = MouseX2EventAddress -|- ntoa "Down"
    let MouseX2UpEventAddress = MouseX2EventAddress -|- ntoa "Up"
    let MouseX2ChangeEventAddress = MouseX2EventAddress -|- ntoa "Change"
    let KeyboardKeyEventAddress = ntoa<obj> "KeyboardKey"
    let KeyboardKeyDownEventAddress = MouseEventAddress -<- ntoa<KeyboardKeyData> "Down"
    let KeyboardKeyUpEventAddress = MouseEventAddress -<- ntoa<KeyboardKeyData> "Up"
    let KeyboardKeyChangeEventAddress = MouseEventAddress -<- ntoa<KeyboardKeyData> "Change"
    let CollisionEventAddress = ntoa<CollisionData> "Collision"
    let IncomingEventAddress = ntoa<unit> "Incoming"
    let IncomingStartEventAddress = IncomingEventAddress -|- ntoa "Start"
    let IncomingFinishEventAddress = IncomingEventAddress -|- ntoa "Finish"
    let OutgoingEventAddress = ntoa<unit> "Outgoing"
    let OutgoingStartEventAddress = OutgoingEventAddress -|- ntoa "Start"
    let OutgoingFinishEventAddress = OutgoingEventAddress -|- ntoa "Finish"
    let WorldStateEventAddress = ntoa<WorldStateChangeData> "WorldState"
    let WorldStateChangeEventAddress = WorldStateEventAddress -|- ntoa "Change"
    let SimulantEventAddress = ntoa<Simulant SimulantChangeData> "Simulant"
    let SimulantChangeEventAddress = SimulantEventAddress -|- ntoa "Change"
    let GameChangeEventAddress = Address.changeType<Simulant SimulantChangeData, Game SimulantChangeData> SimulantChangeEventAddress
    let ScreenChangeEventAddress = Address.changeType<Simulant SimulantChangeData, Screen SimulantChangeData> SimulantChangeEventAddress
    let GroupChangeEventAddress = Address.changeType<Simulant SimulantChangeData, Group SimulantChangeData> SimulantChangeEventAddress
    let EntityChangeEventAddress = Address.changeType<Simulant SimulantChangeData, Entity SimulantChangeData> SimulantChangeEventAddress
    let DefaultDissolveImage = { PackageName = DefaultPackageName; AssetName = "Image8" }