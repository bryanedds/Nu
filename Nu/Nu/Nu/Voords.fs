namespace Nu
module Voords =

    (* WISDOM: Virtual coordinates are necessary, but they should be 1-to-1, 1-to-2, or 2-to-1 on the
    device most likely to be used. Devices with other ratios will just have to suffer aliasing. *)

    // Virtual resolution based on iPhone 4 in horizontal mode (since I'm guessing this is our most
    // likely preferred device).
    let VesolutionX = 960.0f
    let VesolutionY = 640.0f