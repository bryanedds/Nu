module Nu.Constants
open Microsoft.Xna
open Nu.FrameRate

let Gravity = Framework.Vector2 (0.0f, 10.0f)
let PhysicsStepRate = 1.0f / (single DesiredFps)