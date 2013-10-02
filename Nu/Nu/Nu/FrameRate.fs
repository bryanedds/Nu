module Nu.FrameRate

(* WIDSOM: frame limits should be a factor of 60, and frame rates should be a factor that. Updates
should receive an int multiplier of that frame rate factor. Dynamic frame rate degradation should
last at least one second. *)

let DesiredFps = 60 / 2 // assume 30fps for potentially slow but predicatable GC1s

type FrameSkip =
    | Full = 1
    | Half = 2
    | Third = 3
    | Fourth = 4
    | Fifth = 5
    | Count = 5

// TODO: avoid using arrays! match lookups on literals are just as fast
(*let MinFrameTimes =
    Array.init
        (int FrameSkip.Count)
        (fun fs -> 1000 / (int (enum fs : FrameSkip)))*)