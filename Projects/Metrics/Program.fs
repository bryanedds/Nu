namespace Metrics
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open Nu.Declarative

#if ECS_HYBRID
type [<NoEquality; NoComparison; Struct>] StaticSpriteComponent =
    { mutable Active : bool
      mutable Entity : Entity
      mutable Sprite : Image AssetTag }
    interface StaticSpriteComponent Component with
        member this.TypeName = nameof StaticSpriteComponent
        member this.Active with get () = this.Active and set value = this.Active <- value
#endif

#if ECS
type [<NoEquality; NoComparison; Struct>] Velocity =
    { mutable Active : bool
      mutable Velocity : Vector2 }
    interface Velocity Component with
        member this.TypeName = nameof Velocity
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<NoEquality; NoComparison; Struct>] Position =
    { mutable Active : bool
      mutable Position : Vector2 }
    interface Position Component with
        member this.TypeName = nameof Position
        member this.Active with get () = this.Active and set value = this.Active <- value
#endif

#if FACETED
type MetricsEntityDispatcher () =
    inherit EntityDispatcher ()

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
    inherit EntityDispatcher ()
  #else
    inherit EntityDispatcher<Image AssetTag, unit, unit> (Assets.Default.Image)
  #endif

  #if !ECS_HYBRID && !ECS
    override this.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.03f) world

    override this.View (staticImage, entity, world) =
        let transform = entity.GetTransform world
        View.Render
            (transform.Elevation,
             transform.Position.Y,
             AssetTag.generalize staticImage,
             SpriteDescriptor
                { Transform = transform
                  Offset = Vector2.Zero
                  Absolute = false
                  InsetOpt = None
                  Image = staticImage
                  Color = colWhite
                  Blend = Transparent
                  Glow = colZero
                  Flip = FlipNone })
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
#endif

//type P = { mutable Active : bool; mutable P : Vector2 }
//type V = { mutable Active : bool; mutable V : Vector2 }
//type O = { mutable Active : bool; mutable P : P; mutable V : V }

type MyGameDispatcher () =
    inherit GameDispatcher<unit, unit, unit> (())

    let Fps = Simulants.DefaultGroup / "Fps"

    override this.Register (game, world) =
        let world = base.Register (game, world)
        let (screen, world) = World.createScreen (Some Simulants.DefaultScreen.Name) world
#if ECS_HYBRID
        // grab ecs from current screen
        let ecs = screen.GetEcs world

        // create static sprite system
        ecs.RegisterSystem (SystemCorrelated<StaticSpriteComponent, World> ecs)
#endif
#if ECS
        // get ecs
        let ecs = screen.GetEcs world

        // create systems
        let positionSystem = ecs.RegisterSystem (SystemCorrelated<Position, World> ecs)
        let velocitySystem = ecs.RegisterSystem (SystemCorrelated<Velocity, World> ecs)

        // create query
        let query = ecs.RegisterQuery (Query<Position, Velocity, World> ecs)

        //// create object references
        //let count = 2500000
        //let ps = Array.init count (fun _ -> { Active = true; P = v2Zero })
        //let vs = Array.init count (fun _ -> { Active = true; V = v2Zero })
        //let objs = Array.init count (fun i -> { Active = true; P = ps.[i]; V = vs.[i]})
        ////let objs = Array.init count (fun _ -> { P = { P = v2Zero }; V = { V = v2One }})
        //
        ////// randomize elements in memory
        ////for i in 0 .. objs.Length - 1 do
        ////    objs.[i] <- objs.[Gen.random1 (objs.Length - 1)]
        ////    objs.[i].P <- objs.[Gen.random1 (objs.Length - 1)].P
        ////    objs.[i].V <- objs.[Gen.random1 (objs.Length - 1)].V
        //
        //// define update for out-of-place movers
        //ecs.Subscribe EcsEvents.Update $ fun _ _ _ ->
        //    for obj in objs do
        //        if  obj.Active then
        //            obj.P.P.X <- obj.P.P.X + obj.V.V.X
        //            obj.P.P.Y <- obj.P.P.Y + obj.V.V.Y

        // create 3M movers (goal: 60FPS, current: 60FPS)
        for _ in 0 .. 3000000 - 1 do
            let entityId = Gen.id
            let _ = positionSystem.RegisterCorrelated false Unchecked.defaultof<Position> entityId
            let _ = velocitySystem.RegisterCorrelated false { Active = true; Velocity = v2One } entityId
            ()

        // define update for query
        ecs.Subscribe EcsEvents.Update $ fun _ _ _ ->
            query.Iter (fun position velocity ->
                position.Position.X <- position.Position.X + velocity.Velocity.X
                position.Position.Y <- position.Position.Y + velocity.Velocity.Y)

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
        let world = World.createGroup (Some Simulants.DefaultGroup.Name) Simulants.DefaultScreen world |> snd
        let world = World.createEntity<FpsDispatcher> (Some Fps.Name) DefaultOverlay Simulants.DefaultGroup world |> snd
        let world = Fps.SetPosition (v2 200.0f -250.0f) world
#if !ECS
        let positions = // 19,663 entity positions (goal: 60FPS, current: 52FPS)
            seq {
                for i in 0 .. 52 do
                    for j in 0 .. 52 do
                        for k in 0 .. 6 do
                            yield v2 (single i * 12.0f + single k) (single j * 12.0f + single k) }
        let world =
            Seq.fold (fun world position ->
                let (entity, world) = World.createEntity<MetricsEntityDispatcher> None NoOverlay Simulants.DefaultGroup world
                let world = entity.SetOmnipresent true world
                let world = entity.SetPosition (position + v2 -450.0f -265.0f) world
                let world = entity.SetSize (v2One * 8.0f) world
                world)
                world positions
#endif
        let world = World.selectScreen IdlingState Simulants.DefaultScreen world
#if ECS_HYBRID
        // define update for static sprites
        ecs.Subscribe EcsEvents.Update $ fun _ _ _ ->
            for components in ecs.GetComponentArrays<StaticSpriteComponent> () do
                for i in 0 .. components.Length - 1 do
                    let comp = &components.[i]
                    if comp.Active then
                        let state = comp.Entity.State world
                        state.Rotation <- state.Rotation + 0.03f

        // define actualize for static sprites
        ecs.SubscribePlus Gen.id EcsEvents.Actualize $ fun _ _ _ world ->
            let messages = List ()
            for components in ecs.GetComponentArrays<StaticSpriteComponent> () do
                for i in 0 .. components.Length - 1 do
                    let comp = &components.[i]
                    if comp.Active then
                        let state = comp.Entity.State world
                        if state.Visible then
                            let spriteDescriptor = SpriteDescriptor { Transform = state.Transform; Absolute = state.Absolute; Offset = Vector2.Zero; InsetOpt = None; Image = comp.Sprite; Color = Color.White; Blend = Transparent; Glow = Color.Zero; Flip = FlipNone }
                            let layeredMessage = { Elevation = state.Elevation; PositionY = state.Position.Y; AssetTag = AssetTag.generalize comp.Sprite; RenderDescriptor = spriteDescriptor }
                            messages.Add layeredMessage
            World.enqueueRenderLayeredMessages messages world
#else
        ignore screen
#endif
        world

#if ELMISH
type ElmishEntityDispatcher () =
    inherit EntityDispatcher<Image AssetTag, unit, unit> (Assets.Default.Image)

    override this.View (staticImage, entity, world) =
        let transform = entity.GetTransform world
        View.Render
            (transform.Elevation,
             transform.Position.Y,
             AssetTag.generalize staticImage,
             SpriteDescriptor
                { Transform = transform
                  Offset = Vector2.Zero
                  Absolute = false
                  InsetOpt = None
                  Image = staticImage
                  Color = colWhite
                  Blend = Transparent
                  Glow = colZero
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
                        for index in 0 .. 49999 do yield Entity.Size <== int --> fun int -> v2 (single (int % 12)) (single (index % 12)) } |> // 50,000 property bindings (goal: 60FPS, currently: 59FPS)
                        Seq.toList)]
             Content.group Gen.name []
                [Content.fps "Fps" [Entity.Position == v2 200.0f -250.0f]]]]
#else
type [<ReferenceEquality>] Ints =
    { Ints : Map<int, int> }
    static member init n =
        { Ints = Seq.init n (fun a -> (a, a)) |> Map.ofSeq }
    static member inc ints =
        { Ints = ints.Ints |> Seq.map (fun kvp -> (kvp.Key, inc kvp.Value)) |> Map.ofSeq }

type [<ReferenceEquality>] Intss =
    { Intss : Map<int, Ints> }
    static member init n =
        { Intss = Seq.init n (fun a -> (a, Ints.init n)) |> Map.ofSeq }
    static member inc intss =
        { Intss = intss.Intss |> Seq.map (fun kvp -> (kvp.Key, Ints.inc kvp.Value)) |> Map.ofSeq }

type ElmishGameDispatcher () =
    inherit GameDispatcher<Intss, int, unit> (Intss.init 80) // 6400 ints (goal: 60FPS, current 55FPS)

    override this.Channel (_, game) =
        [game.UpdateEvent => msg 0]

    override this.Message (intss, message, _, _) =
        match message with
        | 0 -> just (Intss.inc intss)
        | _ -> just intss

    override this.Content (intss, _) =
        [Content.screen Gen.name Vanilla []
            [Content.groups intss (fun intss _ -> intss.Intss) constant $ fun i intss _ ->
                Content.group (string i) []
                    [Content.entities intss (fun ints _ -> ints.Ints) constant $ fun j int _ ->
                        Content.entity<ElmishEntityDispatcher> (string j)
                            [Entity.Omnipresent == true
                             Entity.Position == v2 (single i * 12.0f - 480.0f) (single j * 12.0f - 272.0f)
                             Entity.Size <== int --> fun int -> v2 (single (int % 12)) (single (int % 12))]]
             Content.group Gen.name []
                [Content.fps "Fps" [Entity.Position == v2 200.0f -250.0f]]]]
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
                            phantom.Value.PhantomTransform (v2Dup 0.5f) None phantom.Value.PhantomImage colWhite colZero FlipNone
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