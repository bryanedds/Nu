namespace Nu
open OpenTK
module Constants =

    let DesiredFps = 60
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