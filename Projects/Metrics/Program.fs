namespace Metrics
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open Nu.ForgeOperators
open Nu.Ecs

type [<NoEquality; NoComparison; Struct>] StaticSpriteComponent =
    { mutable Active : bool
      mutable Entity : Entity
      mutable Sprite : Image AssetTag }
    interface StaticSpriteComponent Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<NoEquality; NoComparison; Struct>] Position =
    { mutable Active : bool
      mutable Position : Vector2 }
    interface Position Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<NoEquality; NoComparison; Struct>] Velocity =
    { mutable Active : bool
      mutable Velocity : Vector2 }
    interface Velocity Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<NoEquality; NoComparison; Struct>] Shake =
    { mutable Active : bool
      mutable Origin : Vector2
      mutable Offset : Vector2 }
    interface Shake Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

#if FACETED
type MetricsEntityDispatcher () =
    inherit EntityDispatcher2d (false, false)

  #if !ECS
    static member Facets =
        [typeof<StaticSpriteFacet>]

    override this.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.03f) world
  #endif
#else
type MetricsEntityDispatcher () =
    inherit StaticModelDispatcher ()
  #if !ECS
    override this.Update (entity, world) =
        entity.SetAngles (v3 0.0f 0.0f ((entity.GetAngles world).Z + 0.05f)) world
  #endif
#endif

#if !ELMISH
type MyGameDispatcher () =
    inherit GameDispatcher<unit, unit, unit> (())

    let Fps = Simulants.Default.Group / "Fps"

    override this.Register (game, world) =
        let world = base.Register (game, world)
        let (screen, world) = World.createScreen (Some Simulants.Default.Screen.Name) world
#if ECS
        // get ecs
        let ecs = screen.GetEcs world

        // create movers query
        let movers = Query.make<Position, Velocity> ecs

        // create shakers query
        let shakers = Query.make<Position, Shake> ecs

        // create 1M movers the slow way
        let world =
            Seq.fold (fun world _ ->
                let mover = ecs.MakeEntity ()
                let world = mover.Register { Active = true; Position = v2Zero } world
                let world = mover.Register { Active = true; Velocity = v2One } world
                world)
                world (Seq.init 1000000 id)

        // create 3M more movers (goal: 60FPS, current: 60FPS)
        let moverComponents = [{ Active = true; Position = v2Zero } :> obj; { Active = true; Velocity = v2One } :> obj]
        let moverArchetypeId = ArchetypeId.make (moverComponents, Map.empty)
        let world = ecs.RegisterEntities true 3000000 moverComponents moverArchetypeId world |> snd

        // create 40 shakers
        let shakerArchetypeId = ArchetypeId.make<Position, Shake> (subterms = Map.empty)
        let shakerComponents = [{ Active = true; Position = v2Zero } :> obj; { Active = true; Origin = v2Zero; Offset = v2One } :> obj]
        let world = ecs.RegisterEntities true 4000 shakerComponents shakerArchetypeId world |> snd

        // define update for movers
        movers.SubscribeIteration (EcsEvents.Update, fun position velocity world ->
            position.Position.X <- position.Position.X + velocity.Velocity.X
            position.Position.Y <- position.Position.Y + velocity.Velocity.Y
            world)

        // define update for shakers
        shakers.SubscribeIteration (EcsEvents.Update, fun position shake world ->
            position.Position.X <- shake.Origin.X + Gen.randomf1 shake.Offset.X
            position.Position.Y <- shake.Origin.Y + Gen.randomf1 shake.Offset.Y
            world)

        // [| mutable P : Vector2; mutable V : Vector2 |]       8M
        //
        // { mutable P : Vector2; mutable V : Vector2 }         5M / 1.25M when placement randomized
        //
        // [| [| componentRef P |] [| componentRef V |] |]      4M / 3M #if !SYSTEM_ITERATION
        //
        // [| [| ref P |] [| ref V |] |]                        2.5M
        //
        // { mutable P : P; mutable V : V }                     2M / 250K when placement randomized
        //
        // out-of-place entities * ops                          50K * 5 = 250K
        //
        // flat array -> 8M, partitioned array -> 2M, partitioned array tree -> ???, randomized cyclic graph -> 250K

        // raw, manually managed array of structs               inits fast, runs fast. optimal but inflexible.
        //
        // uncorrelated components                              inits medium fast (int HashSet op may be required), runs fast. slight flexibility increase.
        //
        // correlated components                                inits slow, runs medium fast. suboptimal, but very flexible.

        // elmish                                               1,000's
        //
        // classic                                              10,000's
        //
        // ecs                                                  100,000's
        //
        // compute shaders                                      1,000,000's

#endif
        let world = World.createGroup (Some Simulants.Default.Group.Name) Simulants.Default.Screen world |> snd
        let world = World.createEntity<FpsDispatcher> (Some Fps.Surnames) DefaultOverlay Simulants.Default.Group world |> snd
        let world = Fps.SetPosition (v3 200.0f -250.0f 0.0f) world
#if !ECS
        let positions = // 19,663 entity positions (goal: 60FPS, current: 54FPS)
            seq {
                for i in 0 .. 52 do
                    for j in 0 .. 52 do
                        for k in 0 .. 6 do
                            yield v3 (single i * 4.5f + single k * 0.5f) (single j * 4.5f + single k * 0.5f) -205.0f }
        let world =
            Seq.foldi (fun i world position ->
                let (entity, world) = World.createEntity<MetricsEntityDispatcher> (Some [|string Gen.id64|]) NoOverlay Simulants.Default.Group world
                let world = entity.SetPresence Omnipresent world
                let world = entity.SetPosition (position + v3 -118.0f -118.0f 0.0f) world
                world)
                world positions
#endif
        let world = World.selectScreen IdlingState screen world
        world
#else
type ElmishEntityDispatcher () =
    inherit EntityDispatcher3d<StaticModel AssetTag, unit, unit> (true, false, Assets.Default.StaticModel)

    override this.View (staticModel, entity, world) =
        let mutable transform = entity.GetTransform world
        let staticModelMatrix = transform.AffineMatrix
        View.Render3d (RenderStaticModelMessage (false, staticModelMatrix, ValueNone, Unchecked.defaultof<_>, DeferredRenderType, staticModel))

    override this.GetQuickSize (entity, world) =
        let staticModel = entity.GetModelGeneric world
        let bounds = (Metadata.getStaticModelMetadata staticModel).Bounds
        let boundsExtended = bounds.Combine bounds.Mirror
        boundsExtended.Size

type [<ReferenceEquality>] Ints =
    { Ints : Map<int, int> }
    static member init n =
        { Ints = Seq.init n (fun a -> (a, a)) |> Map.ofSeq }
    static member inc ints =
        { Ints = ints.Ints |> Map.map (fun _ v -> inc v) }

type [<ReferenceEquality>] Intss =
    { Intss : Map<int, Ints> }
    static member init n =
        { Intss = Seq.init n (fun a -> (a, Ints.init n)) |> Map.ofSeq }
    static member inc intss =
        { Intss = intss.Intss |> Map.map (fun k v -> if k % 1 = 0 then Ints.inc v else v) }

type ElmishGameDispatcher () =
    inherit GameForger<Intss, int, unit> (Intss.init 100) // 10,000 elmish entities (goal: steady 60FPS, current: unsteady 38FPS)

    override this.Message (intss, message, _, _) =
        match message with
        | 0 -> just (Intss.inc intss)
        | _ -> just intss

    override this.Forge (intss, game) =
        Forge.game
            [game.UpdateEvent ==> msg 0]
            [Forge.screen Simulants.Default.Screen.Name Vanilla []
                [for (i, ints) in intss.Intss.Pairs do
                    yield Forge.group (string i) []
                        [for (j, int) in ints.Ints.Pairs do
                            yield Forge.entity<ElmishEntityDispatcher> (string j)
                                [Entity.Presence == Omnipresent
                                 Entity.Position == v3 (single i * 5.0f - 250.0f) (single j * 2.5f - 125.0f) -250.0f
                                 Entity.Scale == v3Dup (single (int % 10)) * 0.5f]]
                 yield Forge.group "Fps" []
                    [Forge.fps "Fps" [Entity.Position == v3 200.0f -250.0f 0.0f]]]]

#if ELMISH_AND_ECS
    override this.Register (game, world) =

        // call base
        let world = base.Register (game, world)

        // get ecs
        let screen = Simulants.Default.Screen
        let ecs = screen.GetEcs world

        // create component stores
        let _ = ecs.RegisterStore (CorrelatedStore<EntityId, World> ecs)
        let _ = ecs.RegisterStore (CorrelatedStore<Position, World> ecs)
        let _ = ecs.RegisterStore (CorrelatedStore<Velocity, World> ecs)
        let _ = ecs.RegisterStore (CorrelatedStore<Shake, World> ecs)

        // create movers system
        let movers = ecs.RegisterSystem (System<Position, Velocity, World> ecs)

        // create shakers system
        let shakers = ecs.RegisterSystem (System<EntityId, Position, Shake, World> ecs)

        // create 4M movers (goal: 60FPS, current: 60FPS)
        let world =
            Seq.fold (fun world _ ->
                movers.Allocate { Active = true; Position = v2Zero } { Active = true; Velocity = v2One } world |> snd')
                world (Seq.init 1000000 id)

        // create 4000 shakers
        let world =
            Seq.fold (fun world _ ->
                let struct (entityId, world) = movers.NextEntityId world
                shakers.Allocate { Active = true; EntityId = entityId } { Active = true; Position = v2Zero } { Active = true; Origin = v2Zero; Offset = v2One } world |> snd')
                world (Seq.init 4000 id)

        // define update for movers
        ecs.Subscribe EcsEvents.Update $ fun _ _ _ ->
            movers.Iterate $
                new Statement<_, _, _> (fun position velocity world ->
                    position.Position.X <- position.Position.X + velocity.Velocity.X
                    position.Position.Y <- position.Position.Y + velocity.Velocity.Y
                    world)

        // define update for shakers
        ecs.Subscribe EcsEvents.Update $ fun _ _ _ ->
            shakers.Iterate $
                new Statement<_, _, _, _> (fun _ position shake world ->
                    position.Position.X <- shake.Origin.X + Gen.randomf1 shake.Offset.X
                    position.Position.Y <- shake.Origin.Y + Gen.randomf1 shake.Offset.Y
                    world)

        world
#endif
#endif

#if TESTBED
type [<ReferenceEquality>] StringsOpt =
    { StringsOpt : Map<int, string> option }

type TestBedGameDispatcher () =
    inherit GameDispatcher<StringsOpt, unit, unit> ({ StringsOpt = None })

    override this.Channel (_, game) =
        [game.UpdateEvent => msg ()]

    override this.Message (stringsOpt, message, _, world) =
        match message with
        | () ->
            match World.getUpdateTime world % 4L with
            | 0L -> just { StringsOpt = None }
            | 1L -> just { StringsOpt = None }
            | 2L -> just { StringsOpt = Some (Map.ofList [(0, "0"); (1, "1"); (2, "2")]) }
            | 3L -> just { StringsOpt = Some (Map.ofList [(0, "3"); (1, "4"); (2, "5")]) }
            | _ -> failwithumf ()

    override this.Content (stringsOpt, _) =
        [Content.screen Gen.name Vanilla []
            [Content.group Gen.name []
                [Content.entityOpt stringsOpt (fun stringsOpt _ -> stringsOpt.StringsOpt) $ fun strings _ ->
                    Content.panel Gen.name []
                        [Content.entities strings constant constant $ fun i str _ ->
                            Content.text Gen.name
                                [Entity.Position == v2 (single i * 120.0f - 180.0f) 0.0f
                                 Entity.Text <== str]]]]]
#endif

#if PHANTOM
type [<ReferenceEquality>] Phantom =
    { mutable PhantomTransform : Transform
      PhantomLinearVelocity : Vector2
      PhantomAngularVelocity : single
      PhantomImage : Image AssetTag }
    static member init i =
        let x = single i * 0.05f + Gen.randomf - (single Constants.Render.VirtualResolutionX * 0.5f)
        let y = Gen.randomf - (single Constants.Render.VirtualResolutionY * 0.5f)
        { PhantomTransform = { Transform.makeDefault () with Position = v2 x y; Size = v2 9.0f 9.0f }
          PhantomLinearVelocity = v2 0.0f (Gen.randomf * 0.5f)
          PhantomAngularVelocity = Gen.randomf * 0.1f
          PhantomImage =  Assets.Default.CharacterIdleImage }
    static member move phantom =
        phantom.PhantomTransform.Position <- phantom.PhantomTransform.Position + phantom.PhantomLinearVelocity
        phantom.PhantomTransform.Rotation <- phantom.PhantomTransform.Rotation + phantom.PhantomAngularVelocity

type [<ReferenceEquality>] Phantoms =
    { Phantoms : Dictionary<Guid, Phantom> }
    static member init n =
        let phantoms = seq { for i in 0 .. n - 1 do yield (Gen.id, Phantom.init i) } |> dictPlus
        { Phantoms = phantoms }
    static member move phantoms =
        for entry in phantoms.Phantoms do
            Phantom.move entry.Value

type PhantomGameDispatcher () =
    inherit GameDispatcher<Phantoms, unit, unit> (Phantoms.init 20000)

    override this.Channel (_, game) =
        [game.UpdateEvent => cmd ()]

    override this.Command (phantoms, message, _, world) =
        match message with
        | () ->
            Phantoms.move phantoms
            just world

    override this.Content (_, _) =
        [Content.screen "Screen" Vanilla []
            [Content.group "Group" []
                [Content.fps "Fps" [Entity.Position == v2 200.0f -250.0f]]]]

    override this.View (phantoms, _, _) =
        Render
            (0.0f,
             single Constants.Render.VirtualResolutionY * -0.5f,
             Assets.Default.Empty,
             RenderCallback
                 (fun viewAbsolute viewRelative eyeCenter eyeSize renderer ->
                    let sdlRenderer = renderer :?> SdlRenderer
                    for phantom in phantoms.Phantoms do
                        SdlRenderer.renderSprite
                            viewAbsolute viewRelative eyeCenter eyeSize
                            phantom.Value.PhantomTransform (v2Dup 0.5f) None phantom.Value.PhantomImage Color.One Color.Zero FlipNone
                            sdlRenderer))
#endif

type MetricsPlugin () =
    inherit NuPlugin ()

/// This program exists to take metrics on Nu's performance.
module Program =

    let [<EntryPoint; STAThread>] main _ =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
#if FUNCTIONAL
        let worldConfig = { WorldConfig.defaultConfig with Imperative = false; SdlConfig = sdlConfig }
#else
        let worldConfig = { WorldConfig.defaultConfig with Imperative = true; SdlConfig = sdlConfig }
#endif
        Nu.init worldConfig.NuConfig
        World.run worldConfig (MetricsPlugin ())