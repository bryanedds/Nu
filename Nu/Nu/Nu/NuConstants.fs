namespace Nu
open System
open OpenTK
open Nu
open Nu.NuCore
module NuConstants =

    let DesiredFps = 60
    let ScreenClearing = ColorClear (255uy, 255uy, 255uy)
    let PhysicsStepRate = 1.0f / single DesiredFps
    let PhysicsToPixelRatio = 64.0f
    let PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio
    let NormalDensity = 10.0f // NOTE: this seems to be a stable density for Farseer
    let Gravity = Vector2 (0.0f, 9.80665f) * PhysicsToPixelRatio
    let TimeToFadeOutSongMs = 5000
    let RadiansToDegrees = 57.2957795
    let DegreesToRadians = 1.0 / RadiansToDegrees
    let RadiansToDegreesF = single RadiansToDegrees
    let DegreesToRadiansF = single DegreesToRadians
    let DefaultEntitySize = 64.0f
    let DefaultEntityRotation = 0.0f
    let TickAddress = addr "Tick"
    let MouseDragAddress = addr "Mouse/Drag"
    let MouseMoveAddress = addr "Mouse/Move"
    let MouseLeftAddress = addr "Mouse/Left"
    let MouseCenterAddress = addr "Mouse/Center"
    let MouseRightAddress = addr "Mouse/Right"
    let DownMouseLeftAddress = straddr "Down" MouseLeftAddress
    let DownMouseCenterAddress = straddr "Down" MouseCenterAddress
    let DownMousRightAddress = straddr "Down" MouseRightAddress
    let UpMouseLeftAddress = straddr "Up" MouseLeftAddress
    let UpMouseCenterAddress = straddr "Up" MouseCenterAddress
    let UpMouseRightAddress = straddr "Up" MouseRightAddress
    let FinishedIncomingAddressPart = addr "Finished/Incoming"
    let FinishedOutgoingAddressPart = addr "Finished/Outgoing"
    let GamePublishingPriority = Single.MaxValue
    let ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let GroupPublishingPriority = ScreenPublishingPriority * 0.5f