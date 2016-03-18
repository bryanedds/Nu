// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Prime
open global.Nu // TODO: see if this req'd explicit qualification is due to a compiler bug
open global.Nu.Descriptors
module Program =

    (* DISCUSSION - On Nu's authoring story...

    Instead of using a general purpose scripting language for authoring tasks in Nu, we use a small
    set of domain-specific languages. For example, the simulant system uses XML, as does the
    overlay and asset graph systems. The effect system uses symbolic expressions, and simulation
    interactions are defined with chains.

    system          | language      | editor
    -----------------------------------------------
    overlay         | s-exprs       | Visual Studio
    asset graph     | s-exprs       | Visual Studio
    simulants defs  | xml           | Gaia
    event filtering | s-exprs       | Gaia
    effect system   | s-exprs       | Aether (TBA)
    mind (TBA)      | s-exprs       | Pheobe (TBA) - http://www.cs.uu.nl/research/techreps/repo/CS-2013/2013-003.pdf
    interactions    | F# (chains)   | Visual Studio
    subsystems      | F#            | Visual Studio
    dispatchers     | F#            | Visual Studio

    The advantages and limitations that fall out of this is as such -

    Most of these systems are interpreted, and unlike code in F#, allow for hot-reloading for
    optimal authoring experiences. For these systems, however, no static checking is in place,
    allowing for trivial syntactic errors.

    For the system that isn't interpreted, a strong type system is in place to make sure complex
    data-flow dependencies are made explicit and checked with good error messages. For this system,
    however, no hot-reloading is possible, negatively impacting the authoring experience.

    The trade-offs for each given domain does seem to be appropriate. While the simulant system
    MUST be run-time in order to be WYSIWYG editable, the interaction system isn't too badly
    affected by the need for program restarts, and benefits proportinately from having an
    expressive type system.
    
    That being said, I really, really, really, really want Edit and Continue for interaction
    authoring to get the best of both worlds -
    https://www.reddit.com/r/fsharp/comments/3mdklt/can_someone_with_the_requisite_insight_summarize/ *)

    (* WISDOM - Dealing with different device resolutions - Instead of rendering each component
    scaled to a back-buffer of a varying size, render each component unscaled to an off-screen
    buffer of a static size and then blit that with scaling to the back-buffer. NOTE: this only
    applies to 2D ~ will not apply to 3D once implemented in Nu (for obvious reasons). *)

    (* WISDOM: Performance concerns remain a long-standing subject of interest. However, this can
    be mitigated in a few ways with approximate speed-ups -

    2x gain - put physics and rendering each in another process.
    2x gain - run at 30fps instead of 60.
    1.?x gain - compile with .NET Native or Mono AOT.
    1.?x gain - upgrade to F# 4.1.
    1.?x gain - target x64 instead of x86.
    1.2x gain - store loaded assets (and asset metadata?) in a Dictionary<string, Dictionary>> rather than a Map<string, Map>>, or...
    1.2x gain - alternatively, use short-term memoization with a temporary dictionary to cache asset queries during rendering / playing / etc.
    1.5x gain - leverage a proper sprite batching system.
    1.?x gain - avoid rendering clear tiles! *)

    (* WISDOM: On avoiding threads where possible...
    
    Beyond the cases where persistent threads are absolutely required or where transient threads
    implement embarassingly parallel processes, threads should be AVOIDED as a rule.
    
    If it were the case that physics were processed on a separate hardware component and thereby
    ought to be run on a separate persistent thread, then the proper way to approach the problem of
    physics system queries is to copy the relevant portion of the physics state from the PPU to main
    memory every frame. This way, queries against the physics state can be done IMMEDIATELY with no
    need for complex intermediate states (albeit against a physics state that is one frame old). *)

    (* WISDOM: On threading physics...
    
    A simulation that would put physics on another thread should likely do so in a different app
    domain with communication via .NET remoting to make 100% sure that no sharing is happening.
    This should keep debugging easy and even possibly give a boost to GC latency what with
    spreading collection pauses across two separate collectors.
    
    NOTE: AppDomains are discontinued in .NET Core -
    https://blogs.msdn.microsoft.com/dotnet/2016/02/10/porting-to-net-core/
    It is suggested instead to run in separate processes, which is fine. *)

    (* WISDOM: Keep all animation frame numbers even. That way, you can simply halve them if you
    need to move the app from 60fps to 30fps. *)

    (* IDEA: Simplified networking...

    For networking, perhaps instead of having a useful Game value that synchronizes across players,
    the true value of the world will be on one machine, and only messages like input will come from
    players and messages for rendering / audio will go back to them.

    Perhaps not realistic, but just an idea. *)

    (* IDEA: it was suggested that time-travel debugging a la Elm or http://vimeo.com/36579366
    would be appropriate to this engine given its pure functional nature. *)

    (* TODO: investigate Gaia extensibility mechanism. *)

    let [<EntryPoint; STAThread>] main _ =
        Console.WriteLine "Running Nu.exe"
        Nu.init false
        let group =
            Group<GroupDispatcher>
                [Field? Name !!"Group"]
                [Entity<EntityDispatcher>
                    [Field? Name !!"Jim"
                     Field? Size Vector2.One]
                 Entity<EntityDispatcher>
                    [Field? Name !!"Bob"
                     Field? Position Vector2.Zero]]
        Console.WriteLine (SymbolIndex.prettyPrint ^ scstring group)
        Constants.Engine.SuccessExitCode