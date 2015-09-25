// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open global.Nu
open Nu.Tests
module Program =

    (* TODO: investigate NuEdit extensibility mechanism. *)

    (* WISDOM: Keep all animation frame numbers even. That way, you can simply halve them if you
    need to move the app from 60fps to 30fps. *)

    (* NOTE - On Nu's authoring story...

    Instead of using a general purpose scripting language for authoring tasks in Nu, we use a small
    set of domain-specific languages. For example, the simulant system uses XML, as does the
    overlay and asset graph systems. The effect system uses algebraic expressions, and simulation
    interactions are defined with chains.

    system          | language      | editor
    -----------------------------------------------
    simulant        | xml           | NuEdit
    overlay         | xml           | vs xml editor
    asset graph     | xml           | vs xml editor
    effect          | a-exprs       | NuEffect (TBA)
    decision (AI)   | a-exprs (TBA) | NuDecision (TBA)
    interaction     | chains (F#)   | vs F# editor

    The advantages and limitations that fall out of this is as such -

    Most of these systems are interpreted, and unlike code in F#, allow for hot-reloading for
    optimal authoring experiences. For these systems, however, no static checking is in place,
    allowing for trivial syntactic errors to proliferate.

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

    (* NOTE - On having huge, populated worlds in Nu...
    
    Say you have a large world in your game with 10000 entities that all need ticking. You can
    not, and should not, keep all of these entities ticking all the time when 90%+ are out of the
    range of the player's purview.

    So, you have to use the concept of 'tick beacons' to remove and add entity tick subscriptions.
    For example, a large map will be divided up into 10x10 areas. In each area is a beacon that is
    always observing the player's position. When they player gets within the distance of a given
    beacon, that beacon will query the screen's entity tree for related entities, and then enable
    their tick subscriptions. And vice versa, as the player moves away from a beacon, it will be
    responsible for disabling said subscriptions.
    
    Finally, entities that can move will have to change their beacon association as they get much
    closer to one beacon than another, or will need some way to keep themselves from moving that
    far in the first place.*)

    (* WISDOM - Dealing with different device resolutions - Instead of rendering each component
    scaled to a back-buffer of a varying size, render each component unscaled to an off-screen
    buffer of a static size and then blit that with scaling to the back-buffer. NOTE: this only
    applies to 2D ~ will not apply to 3D once implemented in Nu (for obvious reasons). *)

    (* WISDOM: From benchmarks. it looks like our mobile target will cost us anywhere from a 25% to
    50% decrease in speed as compared to the dev machine. However, this can be mitigated in a few
    ways with approximate speed-ups -

    2x gain - Run app at 30fps instead of 60
    2x gain - put physics and rendering each in another process
    1.5x gain - compile with .NET Native or Mono AOT
    1.3x gain - store loaded assets in a Dictionary<string, Dictionary>> rather than a Map<string, Map>>, or...
    1.3x gain - alternatively, use short-term memoization with a temporary dictionary to cache asset queries during rendering / playing / etc.
    1.2x gain - optimize locality of address usage
    1.2x gain - render tiles layers to their own buffer so that each whole layer can be blitted directly with a single draw call (though this might cause overdraw).
    ? gain - avoid rendering clear tiles! *)

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
    spreading collection pauses across two separate collectors. *)

    (* IDEA: Simplified networking...

    For networking, perhaps instead of having a useful Game value that synchronizes across players,
    the true value of the world will be on one machine, and only messages like input will come from
    players and messages for rendering / audio will go back to them.

    Perhaps not realistic, but just an idea. *)

    (* IDEA: it was suggested that time-travel debugging a la Elm or http://vimeo.com/36579366
    would be appropriate to this engine given its pure functional nature. However, due to the
    imperative nature of the physics system, it could be problematic. However again, that doesn't
    mean the idea isn't worth pursuing for while it might not work perfectly in all usage
    scenarios, it may well be of sufficient value. Additionally, on the possible occasion that the
    current physics engine be replaceable with pure functional one, improvements to the feature may
    be implementable in time. *)

    (* IDEA: Faster feedback / iteration times with dynamic Nu plug-ins -
    
    Edit & Continue is a God-send to languages that support it. Unfortunately, F# does not.
    
    However, it was pointed out to me by Andrea Magnorsky that some amount of hot-swapping of F#
    code is currently acheived in the Onikira: Demon Killer by cordoning F# code behind dynamically
    loaded .NET assemibles. This seems like it would also be applicable with Nu currently since
    it also uses a plug-in model. On the other hand, I was informed that step-debugging for this
    hot-loaded code was not yet working (and I'm not sure if it could without further
    investigation).

    Regardless, the approach of dynamic Nu plug-ins to enable hot-swapping of F# code looks to be
    the correct path forward. *)

    (* IDEA: Perhaps the constants needed to specify specific simulants and assets in code can be
    type-provided rather than being authored manually, hopefully in a dynamic way. Not sure if this
    would payoff to justify the add implementation complexity and increasing the 'magicality' of
    the user experience. *)

    (* IDEA: Perhaps an FRP Signal abstraction could be build on top of the Observation type...? *)

    let [<EntryPoint; STAThread>] main _ =
        Console.Write "Running Nu.exe"
        EffectTests.readEffectWorks ()
        Constants.Engine.SuccessExitCode