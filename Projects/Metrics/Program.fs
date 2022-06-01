namespace Metrics
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open Nu.Declarative

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

  #if !ECS_HYBRID && !ECS
    static member Facets =
        [typeof<StaticSpriteFacet>]

    override this.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.03f) world
  #endif

  #if ECS_HYBRID
    override this.Register (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let _ : Guid = ecs.RegisterCorrelated<StaticSpriteComponent> { Active = false; Entity = entity; Sprite = Assets.Default.Image4 } (entity.GetId world)
        world

    override this.Unregister (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let _ : bool = ecs.UnregisterCorrelated<StaticSpriteComponent> (entity.GetId world)
        world
  #endif
#else
type MetricsEntityDispatcher () =
  #if ECS_HYBRID
    inherit EntityDispatcher2d (false, false)
  #else
    inherit EntityDispatcher2d (false, true)
  #endif

  #if !ECS_HYBRID && !ECS
    override this.Update (entity, world) =
        entity.SetAngles (v3 0.0f 0.0f ((entity.GetAngles world).Z + 0.05f)) world

    override this.Actualize (entity, world) =
        let staticImage = Assets.Default.Image
        let mutable transform = entity.GetTransform world
        let renderMessage =
            RenderLayeredMessage2d
                { Elevation = transform.Elevation
                  Horizon = transform.Position.Y
                  AssetTag = AssetTag.generalize staticImage
                  RenderDescriptor =
                    SpriteDescriptor
                        { Transform = transform
                          InsetOpt = ValueNone
                          Image = staticImage
                          Color = Color.One
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone }}
        World.enqueueRenderMessage2d renderMessage world
  #endif

  #if ECS_HYBRID
    override this.Register (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let id = UInt64.Parse entity.Name
        ecs.RegisterCorrelated<StaticSpriteComponent> false { Active = false; Entity = entity; Sprite = Assets.Default.Image4 } id world

    override this.Unregister (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let id = UInt64.Parse entity.Name
        let struct (_, world) = ecs.UnregisterCorrelated<StaticSpriteComponent> id world
        world
  #endif
#endif

type MyGameDispatcher () =
    inherit GameDispatcher<unit, unit, unit> (())

    let Fps = Simulants.Default.Group / "Fps"

    override this.Register (game, world) =
        let world = base.Register (game, world)
        let (screen, world) = World.createScreen (Some Simulants.Default.Screen.Name) world
#if ECS_HYBRID
        // grab ecs from current screen
        let ecs = screen.GetEcs world

        // create static sprite store
        let _ = ecs.RegisterStore (CorrelatedStore<StaticSpriteComponent, World> ecs)
#endif
#if ECS
        // get ecs
        let ecs = screen.GetEcs world

        // create movers query
        let movers = ecs.RegisterQuery (Query<World>.create<Position, Velocity> (Exclude.zero, Subquery.zero))

        // create shakers query
        let shakers = ecs.RegisterQuery (Query<World>.create<EntityId, Position, Shake> (Exclude.zero, Subquery.zero))

        // create 4M movers (goal: 60FPS, current: 60FPS)
        let world =
            Seq.fold (fun world _ ->
                let mover = ecs.EntityRef
                let world = ecs.RegisterComponent { Active = true; Position = v2Zero } mover world
                let world = ecs.RegisterComponent { Active = true; Velocity = v2One } mover world
                mover.Set { Active = true; Velocity = v2One }
                world)
                world (Seq.init 4000000 id)

        // create 4000 shakers
        let world =
            Seq.fold (fun world _ ->
                let shaker = ecs.EntityRef
                let world = ecs.RegisterComponent { Active = true; EntityId = shaker } shaker world
                let world = ecs.RegisterComponent { Active = true; Position = v2Zero } shaker world
                let world = ecs.RegisterComponent { Active = true; Origin = v2Zero; Offset = v2One } shaker world
                world)
                world (Seq.init 4000 id)

        // define update for movers
        ecs.Subscribe EcsEvents.Update $ fun _ _ ->
            movers.Iterate<_, _, _> $
                new Statement<_, _, _> (fun position velocity world ->
                    position.Position.X <- position.Position.X + velocity.Velocity.X
                    position.Position.Y <- position.Position.Y + velocity.Velocity.Y
                    world)

        // define update for shakers
        ecs.Subscribe EcsEvents.Update $ fun _ _ ->
            shakers.Iterate<_, _, _> $
                new Statement<_, _, _> (fun position shake world ->
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
        let positions = // 25,281 entity positions (goal: 60FPS, current: 58FPS)
            seq {
                for i in 0 .. 52 do
                    for j in 0 .. 52 do
                        for k in 0 .. 8 do
                            yield v3 (single i * 12.0f + single k) (single j * 12.0f + single k) 0.0f }
        let world =
            Seq.foldi (fun i world position ->
                let (entity, world) = World.createEntity<MetricsEntityDispatcher> (Some [|string Gen.id64|]) NoOverlay Simulants.Default.Group world
                let world = entity.SetOmnipresent true world
                let world = entity.SetPosition (position + v3 -450.0f -265.0f 0.0f) world
                let world = entity.SetSize (v3One * 8.0f) world
                world)
                world positions
#endif
        let world = World.selectScreen IdlingState Simulants.Default.Screen world
#if ECS_HYBRID
        // define update for static sprites
        ecs.Subscribe EcsEvents.Update $ fun _ _ _ world ->
            for components in ecs.GetComponentArrays<StaticSpriteComponent> () do
                for i in 0 .. components.Length - 1 do
                    let comp = &components.[i]
                    if comp.Active then
                        let state = comp.Entity.State world
                        state.Rotation <- state.Rotation + 0.03f
            world

        // define actualize for static sprites
        let _ =
            ecs.SubscribePlus Gen.id32 EcsEvents.Actualize $ fun _ _ _ world ->
                let messages = List ()
                for components in ecs.GetComponentArrays<StaticSpriteComponent> () do
                    for i in 0 .. components.Length - 1 do
                        let comp = &components.[i]
                        if comp.Active then
                            let state = comp.Entity.State world
                            if state.Visible then
                                let spriteDescriptor = SpriteDescriptor { Transform = state.Transform; Absolute = state.Absolute; Offset = Vector2.Zero; InsetOpt = None; Image = comp.Sprite; Color = Color.One; Blend = Transparent; Glow = Color.Zero; Flip = FlipNone }
                                let layeredMessage = { Elevation = state.Elevation; PositionY = state.Position.Y; AssetTag = AssetTag.generalize comp.Sprite; RenderDescriptor = spriteDescriptor }
                                messages.Add layeredMessage
                World.enqueueRenderLayeredMessages messages world
#else
        ignore screen
#endif
        world

#if ELMISH
type ElmishEntityDispatcher () =
    inherit EntityDispatcher2d<Image AssetTag, unit, unit> (true, false, Assets.Default.Image)

    override this.View (staticImage, entity, world) =
        let mutable transform = entity.GetTransform world
        View.Render2d
            (transform.Elevation,
             transform.Position.Y,
             AssetTag.generalize staticImage,
             SpriteDescriptor
                { Transform = transform
                  InsetOpt = ValueNone
                  Image = staticImage
                  Color = Color.One
                  Blend = Transparent
                  Glow = Color.Zero
                  Flip = FlipNone })

#if PROPERTIES
type ElmishGameDispatcher () =
    inherit GameDispatcher<int, int, unit> (0)

    override this.Channel (_, game) =
        [game.UpdateEvent => msg 0]

    override this.Message (int, message, _, _) =
        match message with
        | 0 -> just (inc int)
        | _ -> just int

    override this.Content (int, _) =
        [Content.screen Gen.name Vanilla []
            [Content.group Gen.name []
                [Content.entity<ElmishEntityDispatcher> Gen.name
                    (seq {
                        yield Entity.Omnipresent == true
                        for index in 0 .. 100000 do yield Entity.Size <== int --> fun int -> v2 (single (int % 12)) (single (index % 12)) } |> // 100,000 property bindings (goal: 60FPS, current: 40FPS)
                        Seq.toList)]
             Content.group Gen.name []
                [Content.fps "Fps" [Entity.Position == v2 200.0f -250.0f]]]]
#else
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
    inherit GameDispatcher<Intss, int, unit> (Intss.init 81) // 6,561 elmish entities (goal: 60FPS w/o Stalls, current: 60FPS w/o Stalls)

    override this.Channel (_, game) =
        [game.UpdateEvent => msg 0]

    override this.Message (intss, message, _, _) =
        match message with
        | 0 -> just (Intss.inc intss)
        | _ -> just intss

    override this.Content (intss, _) =
        [Content.screen Simulants.Default.Screen.Name Vanilla []
            [Content.groups intss (fun intss _ -> intss.Intss) $ fun i intss _ ->
                Content.group (string i) []
                    [Content.entities intss (fun ints _ -> ints.Ints) $ fun j int _ ->
                        Content.entity<ElmishEntityDispatcher> (string j)
                            [Entity.Omnipresent == true
                             Entity.Position == v3 (single i * 10.0f - 480.0f) (single j * 5.0f - 272.0f) 0.0f
                             Entity.Size <== int --> fun int -> v3 (single (int % 10)) (single (int % 10)) 0.0f]]
             Content.group Gen.name []
                [Content.fps "Fps" [Entity.Position == v3 200.0f -250.0f 0.0f]]]]

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
#if ELMISH
    override this.StandAloneConfig = typeof<ElmishGameDispatcher>
#else
  #if PHANTOM
    override this.StandAloneConfig = typeof<PhantomGameDispatcher>
  #else
    #if TESTBED
    override this.StandAloneConfig = typeof<TestBedGameDispatcher>
    #else
    override this.StandAloneConfig = typeof<MyGameDispatcher>
    #endif
  #endif
#endif

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