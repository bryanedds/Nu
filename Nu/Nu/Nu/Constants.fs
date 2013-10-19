module Nu.Constants
open Microsoft.Xna

(* WIDSOM: frame limits should be a factor of 60, and frame rates should be a factor that. Updates
should receive an int multiplier of that frame rate factor. Dynamic frame rate degradation should
last at least one second. *)

let DesiredFps = 60
let PhysicsStepRate = 1.0f / single DesiredFps
let PhysicsToPixelRatio = 64.0f
let PixelToPhysicsRatio = 1.0f / PhysicsToPixelRatio
let NormalDensity = 10.0f // NOTE: this seems to be a stable density for Farseer
let Gravity = Framework.Vector2 (0.0f, 9.80665f)
let TimeToFadeOutSongMs = 5000
let RadiansToDegrees = 57.2957795