namespace Nu
open System
open Nu
module Program =

    (* WISDOM - Dealing with different device resolutions - Instead of rendering each component
    scaled to a back-buffer of a varying size, render each component unscalws to a off-screen
    buffer of a static size and then blit that with scaling to the back-buffer. *)

    (* WISDOM: From benchmarks. it looks like our mobile target will cost us anywhere from a 25% to
    50% decrease in speed as compared to the dev machine. However, this can be mitigated in a few
    ways with approximate speed-ups -

    ? gain - Run in 64-bit mode on Windows (https://twitter.com/pgatilov/status/523343373634371584)
    2x gain - Run app at 30fps instead of 60
    2x gain - put physics and rendering each in another process
    1.5x gain - compile with .NET Native
    ? gain - quadtree culling to avoid unecessary render descriptor queries
    1.3x gain - store loaded assets in a Dictionary<string, Dictionary>> rather than a Map<string, Map>>, or...
    1.3x gain - alternatively, use short-term memoization with a temporary dictionary to cache asset queries during rendering / playing / etc.
    1.2x gain - optimize locality of address usage
    1.2x gain - render tiles layers to their own buffer so that each whole layer can be blitted directly with a single draw call (though this might cause overdraw).
    ? gain - avoid rendering clear tiles! *)

    (* WISDOM: Program types and behavior should be closed where possible and open where necessary. *)

    (* WISDOM: On avoiding threads where possible...
    
    Beyond the cases where persistent threads are absolutely required or where transient threads
    implement embarassingly parallel processes, threads should be AVOIDED as a rule.
    
    If it were the case that physics were processed on a separate hardware component and thereby
    ought to be run on a separate persistent thread, then the proper way to approach the problem of
    physics system queries is to copy the relevant portion of the physics state from the PPU to main
    memory every frame. This way, queries against the physics state can be done IMMEDIATELY with no
    need for complex intermediate states (albeit against a physics state that is one frame old). *)

    (* WISDOM & TODO: the more generalized code is, the less ways in which it can be wrong.
    
    Due to that fact, much of Nu, including Addresses, Xtensions, serialization & reflection code,
    as well as the purely functional event system, could be generalized and moved out to Prime.
    
    This non-trivial refactoring task should be undertaken as soon as time permits. *)

    (* WISDOM: On threading physics...
    
    A simulation that would put physics on another thread should likely do so in a different app
    domain with communication via .NET remoting to make 100% sure that no sharing is happening. *)

    (* IDEA: Simplified networking...

    For networking, perhaps instead of having a useful Game value that synchronizes across players,
    the true value of the world will be on one machine, and only messages like input will come from
    players and messages for rendering / audio will go back to them.

    Perhaps not realistic, but just an idea. *)
    
    // apparently a side-effect is needed to avoid the empty program warning
    Console.Write "Running Nu.exe"