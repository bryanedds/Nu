namespace Metrics
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open Prime
open Nu
open Nu.Ecs

type [<Struct>] StaticSpriteComponent =
    { mutable Active : bool
      mutable Entity : Entity
      mutable Sprite : Image AssetTag }
    interface StaticSpriteComponent Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<Struct>] Position =
    { mutable Active : bool
      mutable Position : Vector2 }
    interface Position Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<Struct>] Velocity =
    { mutable Active : bool
      mutable Velocity : Vector2 }
    interface Velocity Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<Struct>] Shake =
    { mutable Active : bool
      mutable Origin : Vector2
      mutable Offset : Vector2 }
    interface Shake Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type MetricsEntityDispatcher () =
    inherit Entity3dDispatcher<StaticModel AssetTag, Message, Command> (false, Assets.Default.StaticModel)

#if !MMCC
    override this.Update (entity, world) =
        entity.SetAngles (v3 0.0f 0.0f ((entity.GetAngles world).Z + 0.05f)) world
#endif

    override this.Render (renderPass, entity, world) =
        let staticModel = entity.GetModelGeneric world
        let mutable transform = entity.GetTransform world
        let affineMatrix = transform.AffineMatrix
        let presence = transform.Presence
        let properties = MaterialProperties.empty
        World.renderStaticModelFast (false, &affineMatrix, presence, ValueNone, &properties, staticModel, DeferredRenderType, renderPass, world)

    override this.GetAttributesInferred (entity, world) =
        let staticModel = entity.GetModelGeneric world
        let bounds = (Metadata.getStaticModelMetadata staticModel).Bounds
        AttributesInferred.make bounds.Size bounds.Center

#if !MMCC
type MyGameDispatcher () =
    inherit GameDispatcher ()

    override this.Register (game, world) =
        let world = base.Register (game, world)
        let (screen, world) = World.createScreen (Some "Screen") world
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

        // mmcc                                                 1,000's
        //
        // classic                                              10,000's
        //
        // ecs                                                  100,000's
        //
        // compute shaders                                      1,000,000's
#endif
        let (group, world) = World.createGroup (Some "Default") screen world
        let (fps, world) = World.createEntity<FpsDispatcher> DefaultOverlay (Some [|"Fps"|]) group world
        let world = World.createEntity<SkyBoxDispatcher> DefaultOverlay None group world |> snd
        let world = fps.SetPosition (v3 200.0f -250.0f 0.0f) world
#if !ECS
        let positions =
            [|for i in 0 .. dec 50 do
                for j in 0 .. dec 50 do
                    for k in 0 .. dec 20 do
                        yield v3 (single i * 0.5f) (single j * 0.5f) (single k * 0.5f)|]
        let world =
            Array.fold (fun world position ->
                let (entity, world) = World.createEntity<MetricsEntityDispatcher> NoOverlay (Some [|string Gen.id64|]) group world
                let world = entity.SetPresence Omnipresent world
                let world = entity.SetPosition (position + v3 -12.5f -12.5f -20.0f) world
                let world = entity.SetScale (v3Dup 0.1f) world
                world)
                world positions
#endif
        let world = World.selectScreen (IdlingState world.GameTime) screen world
        world
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

type Message =
    | Inc
    | Nop
    interface Nu.Message

type MmccGameDispatcher () =
    inherit GameDispatcher<Intss, Message, Command> (Intss.init 120) // 14,400 MMCC entities (goal: steady 60FPS, current: steady 60FPS)

    override this.Initialize (_, _) =
        [Game.UpdateEvent => Inc]

    override this.Message (intss, message, _, _) =
        match message with
        | Inc -> just (Intss.inc intss)
        | Nop -> just intss

    override this.Content (intss, _) =
        [Content.screen "Screen" Vanilla []
            [|for (i, ints) in intss.Intss.Pairs' do
                Content.group (string i) []
                    [|for (j, int) in ints.Ints.Pairs' do
                        Content.entity<MetricsEntityDispatcher> (string j)
                            [Entity.Position == v3 (single i * 4.25f - 250.0f) (single j * 2.25f - 125.0f) -250.0f
                             Entity.Scale := v3Dup (single (int % 10)) * 0.5f
                             Entity.Presence == Omnipresent]|]
              Content.group "Fps" []
                [Content.fps "Fps" [Entity.Position := v3 200.0f -250.0f 0.0f]]|]]
#endif

type MetricsPlugin () =
    inherit NuPlugin ()

/// This program exists to take metrics on Nu's performance.
module Program =

    let [<EntryPoint; STAThread>] main _ =
        Nu.init ()
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
#if FUNCTIONAL
        let worldConfig = { WorldConfig.defaultConfig with Imperative = false; SdlConfig = sdlConfig }
#else
        let worldConfig = { WorldConfig.defaultConfig with Imperative = true; SdlConfig = sdlConfig }
#endif
        World.run worldConfig (MetricsPlugin ())