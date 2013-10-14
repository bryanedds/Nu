module Nu.Constants
open Microsoft.Xna
open Nu.FrameRate

let Gravity = Framework.Vector2 (0.0f, 9.80665f)
let PhysicsStepRate = 1.0f / (single DesiredFps)
let PhysicsToPixelRatio = 64.0f
let PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio