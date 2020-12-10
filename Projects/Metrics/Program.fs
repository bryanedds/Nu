namespace Metrics
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open Nu.Declarative

#if ECS
type [<NoEquality; NoComparison; Struct>] StaticSpriteComponent =
    { mutable Active : bool
      mutable Entity : Entity
      mutable Sprite : Image AssetTag }
    interface StaticSpriteComponent Component with
        member this.Active with get () = this.Active and set value = this.Active <- value
        member this.AllocateJunctions _ = [||]
        member this.ResizeJunctions _ _ _ = ()
        member this.MoveJunctions _ _ _ _ = ()
        member this.Junction _ _ _ = this
        member this.Disjunction _ _ _ = ()
#endif

#if ECS_PURE
type [<NoEquality; NoComparison; Struct>] Velocity =
    { mutable Active : bool
      mutable Velocity : Vector2 }
    interface Velocity Component with
        member this.Active with get () = this.Active and set value = this.Active <- value
        member this.AllocateJunctions _ = [||]
        member this.ResizeJunctions _ _ _ = ()
        member this.MoveJunctions _ _ _ _ = ()
        member this.Junction _ _ _ = this
        member this.Disjunction _ _ _ = ()

type [<NoEquality; NoComparison; Struct>] Position =
    { mutable Active : bool
      mutable Position : Vector2 }
    interface Position Component with
        member this.Active with get () = this.Active and set value = this.Active <- value
        member this.AllocateJunctions _ = [||]
        member this.ResizeJunctions _ _ _ = ()
        member this.MoveJunctions _ _ _ _ = ()
        member this.Junction _ _ _ = this
        member this.Disjunction _ _ _ = ()

type [<NoEquality; NoComparison; Struct>] Mover =
    { mutable Active : bool
      mutable Velocity : Velocity ComponentRef
      mutable Position : Position ComponentRef }
    interface Mover Component with
        member this.Active with get () = this.Active and set value = this.Active <- value
        member this.AllocateJunctions ecs = [|ecs.AllocateArray<Velocity> (); ecs.AllocateArray<Position> ()|]
        member this.ResizeJunctions size junctions ecs = ecs.ResizeJunction<Velocity> size junctions.[0]; ecs.ResizeJunction<Position> size junctions.[1]
        member this.MoveJunctions src dst junctions ecs = ecs.MoveJunction<Velocity> src dst junctions.[0]; ecs.MoveJunction<Position> src dst junctions.[1]
        member this.Junction index junctions ecs = { id this with Velocity = ecs.Junction<Velocity> index junctions.[0]; Position = ecs.Junction<Position> index junctions.[1] }
        member this.Disjunction index junctions ecs = ecs.Disjunction<Velocity> index junctions.[0]; ecs.Disjunction<Position> index junctions.[1]
#endif

type MetricsEntityDispatcher () =
    inherit EntityDispatcher ()

#if !ECS && !ECS_PURE
    static member Facets =
        [typeof<StaticSpriteFacet>]
#endif

#if REACTIVE
    static member Properties =
        [define Entity.PublishChanges true]
#endif

#if !ECS && !ECS_PURE
    override this.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.03f) world
#endif

#if ECS
    override this.Register (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let _ : Guid = ecs.RegisterCorrelated<StaticSpriteComponent> { Active = false; Entity = entity; Sprite = Assets.DefaultImage4 } (entity.GetId world)
        world

    override this.Unregister (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let _ : bool = ecs.UnregisterCorrelated<StaticSpriteComponent> (entity.GetId world)
        world
#endif

type MyGameDispatcher () =
    inherit GameDispatcher<unit, unit, unit> (())

    let Fps = Simulants.DefaultLayer / "Fps"

    override this.Register (game, world) =
        let world = base.Register (game, world)
        let (screen, world) = World.createScreen (Some Simulants.DefaultScreen.Name) world
#if ECS
        // grab ecs from current screen
        let ecs = screen.GetEcs world

        // create static sprite system
        ecs.RegisterSystem (SystemCorrelated<StaticSpriteComponent, World> ecs)
#endif
#if ECS_PURE
        // get ecs
        let ecs = screen.GetEcs world

        // entity count
        let entityCount = int (Math.Pow (2.0, 22.0)) // ~4M entities

        // create systems
        ecs.RegisterSystem (SystemCorrelated<Velocity, World> ecs)
        ecs.RegisterSystem (SystemCorrelated<Position, World> ecs)
        ecs.RegisterSystem (SystemCorrelated<Mover, World> ecs)

        // create movers
        for _ in 0 .. entityCount - 1 do
            let entityId = ecs.RegisterCorrelated Unchecked.defaultof<Mover> Gen.id
            let mover = ecs.IndexCorrelated<Mover> entityId
            mover.Index.Velocity.Index.Velocity <- v2One

        // define update for movers
        let _ = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            for components in ecs.GetComponentArrays<Mover> () do
                for i in 0 .. components.Length - 1 do
                    let mutable comp = &components.[i]
                    if comp.Active then
                        let velocity = &comp.Velocity.Index
                        let position = &comp.Position.Index
                        position.Position.X <- position.Position.X + velocity.Velocity.X
                        position.Position.Y <- position.Position.Y + velocity.Velocity.Y
            world)
#endif
        let world = World.createLayer (Some Simulants.DefaultLayer.Name) Simulants.DefaultScreen world |> snd
        let world = World.createEntity<FpsDispatcher> (Some Fps.Name) DefaultOverlay Simulants.DefaultLayer world |> snd
        let world = Fps.SetPosition (v2 200.0f -250.0f) world
#if !ECS_PURE
        let indices = // 9,900 entities
            seq {
                for i in 0 .. 74 do
                    for j in 0 .. 43 do
                        for k in 0 .. 2 do
                            yield v2 (single i * 12.0f + single k) (single j * 12.0f + single k) }
        let world =
            Seq.fold (fun world position ->
                let (entity, world) = World.createEntity<MetricsEntityDispatcher> None DefaultOverlay Simulants.DefaultLayer world
#if !REACTIVE
                let world = entity.Optimize world
#endif
                let world = entity.SetPosition (position + v2 -450.0f -265.0f) world
                entity.SetSize (v2One * 8.0f) world)
                world indices
#endif
        let world = World.selectScreen Simulants.DefaultScreen world
#if ECS
        // define update for static sprites
        let _ = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            for components in ecs.GetComponentArrays<StaticSpriteComponent> () do
                for i in 0 .. components.Length - 1 do
                    let mutable comp = &components.[i]
                    if comp.Active then
                        let entity = comp.Entity.State world
                        entity.Rotation <- entity.Rotation + 0.03f
            world)

        // define actualize for static sprites
        let _ = ecs.Subscribe EcsEvents.Actualize (fun _ _ _ world ->
            let messages = List ()
            for components in ecs.GetComponentArrays<StaticSpriteComponent> () do
                for i in 0 .. components.Length - 1 do
                    let mutable comp = &components.[i]
                    if comp.Active then
                        let entity = comp.Entity.State world
                        if entity.Visible then
                            let spriteDescriptor = SpriteDescriptor { Transform = entity.Transform; Offset = Vector2.Zero; InsetOpt = None; Image = comp.Sprite; Color = Color.White; Glow = Color.Zero; Flip = FlipNone }
                            let message = LayeredDescriptorMessage { Elevation = entity.Elevation; PositionY = entity.Position.Y; AssetTag = AssetTag.generalize comp.Sprite; RenderDescriptor = spriteDescriptor }
                            messages.Add message
            World.enqueueRenderMessages messages world)
#else
        ignore screen
#endif
        world

#if ELMISH
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
    inherit GameDispatcher<Intss, int, unit> (Intss.init 1)

    override this.Channel (_, game) =
        [game.UpdateEvent => msg 0]

    override this.Message (intss, message, _, _) =
        match message with
        | 0 -> just (Intss.inc intss)
        | _ -> just intss

    override this.Content (intss, _) =
        [Content.screen "Screen" Vanilla []
            [Content.layers intss (fun intss -> intss.Intss) constant (fun i intss _ ->
                Content.layer (string i) []
                    [Content.entities intss (fun ints -> ints.Ints) constant (fun j int _ ->
                        Content.staticSprite (string j)
                            (List.ofSeq
                                (seq {
                                    yield Entity.Imperative == true
                                    yield Entity.Omnipresent == true
                                    yield Entity.Position == v2 (single i * 16.0f - 480.0f) (single j * 16.0f - 272.0f)
                                    for i in 1 .. 21000 do yield Entity.Size <== int --> fun int -> v2 (single (int % i)) (single (int % i)) })))])
             Content.layer "Layer" []
                [Content.fps "Fps" [Entity.Position == v2 200.0f -250.0f]]]]
#endif

type MetricsPlugin () =
    inherit NuPlugin ()
#if ELMISH
    override this.GetGameDispatcher () = typeof<ElmishGameDispatcher>
#else
    override this.GetGameDispatcher () = typeof<MyGameDispatcher>
#endif

/// This program exists to take metrics on Nu's performance.
module Program =

    let [<EntryPoint; STAThread>] main _ =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        Nu.init worldConfig.NuConfig
        World.run worldConfig (MetricsPlugin ())