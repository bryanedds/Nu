// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module WorldTests =

    let TestValueKey = Gen.id
    let TestFilePath = "TestFile.nugame"
    let StringEvent = stoa<string> "String/Event"
    let Jim = Simulants.DefaultLayer / "Jim"
    let Bob = Simulants.DefaultLayer / "Bob"

    let [<Fact>] runOneFrameThenCleanUp () =
        let worldConfig = WorldConfig.defaultConfig
        let world = World.makeEmpty worldConfig
        World.run4 (fun world -> World.getTickTime world < 1L) SdlDeps.empty Running world

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeDefault ()
        let handleEvent = fun evt world -> (Cascade, World.addKeyedValue TestValueKey evt.Subscriber world)
        let world = World.subscribe handleEvent StringEvent Simulants.DefaultEntity world
        let world = World.publish String.Empty StringEvent EventTrace.empty Simulants.Game false world
        Assert.Equal<Simulant> (Simulants.DefaultEntity :> Simulant, World.getKeyedValue TestValueKey world)

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.Name) DefaultOverlay Simulants.DefaultLayer world |> snd
        let world = World.createEntity (Some Bob.Name) DefaultOverlay Simulants.DefaultLayer world |> snd
        let world = !-- Bob.Visible --- Stream.map not -|> Jim.Visible $ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.Name) DefaultOverlay Simulants.DefaultLayer world |> snd
        let world = World.createEntity (Some Bob.Name) DefaultOverlay Simulants.DefaultLayer world |> snd
        let world = !-- Bob.Visible -/> Jim.Visible $ world
        let world = !-- Jim.Visible -|> Bob.Visible $ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)

// TODO: move this to its own file.
module EcsTests =

    type [<NoEquality; NoComparison; Struct>] private TransformIntersection =
        { mutable RefCount : int
          Transform : Transform ComponentRef }
        interface Component with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value

    type private ExampleJunctioned () =
        inherit SystemJunctioned<TransformIntersection, World>
            [|typeof<Transform>.Name|]

        override this.Junction junctions entityId ecs =
            { RefCount = 0
              Transform = junction<Transform, _> junctions entityId ecs }

        override this.Disjunction junctions entityId ecs =
            disjunction<Transform, _> junctions entityId ecs

        override this.Update _ world =
            for i = 0 to this.Components.Length - 1 do
                let comp = &this.Components.[i]
                let transform = &comp.Transform.Value
                transform.Enabled <- false
                transform.Absolute <- false
            world