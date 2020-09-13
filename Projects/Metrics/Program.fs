namespace Metrics
open System
open System.Collections.Generic
open Prime
open Nu
open Nu.Declarative

#if ECS
type [<NoEquality; NoComparison; Struct>] StaticSpriteComponent =
    { mutable RefCount : int
      mutable Entity : Entity
      mutable Sprite : Image AssetTag }
    interface StaticSpriteComponent Component with
        member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
        member this.SystemNames = [||]
        member this.Junction _ _ _ = this
        member this.Disjunction _ _ _ = ()
#endif
#if ECS_PURE
type [<NoEquality; NoComparison; Struct>] Velocity =
    { mutable RefCount : int
      mutable Velocity : Vector2 }
    interface Velocity Component with
        member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
        member this.SystemNames = [||]
        member this.Junction _ _ _ = this
        member this.Disjunction _ _ _ = ()
type [<NoEquality; NoComparison; Struct>] Position =
    { mutable RefCount : int
      mutable Position : Vector2 }
    interface Position Component with
        member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
        member this.SystemNames = [||]
        member this.Junction _ _ _ = this
        member this.Disjunction _ _ _ = ()
type [<NoEquality; NoComparison; Struct>] Mover =
    { mutable RefCount : int
      mutable Velocity : Velocity ComponentRef
      mutable Position : Position ComponentRef }
    interface Mover Component with
        member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
        member this.SystemNames = [|"Velocity"; "Position"|]
        member this.Junction systems registration ecs = { id this with Velocity = ecs.Junction<Velocity> registration systems.[0]; Position = ecs.Junction<Position> registration systems.[1] }
        member this.Disjunction systems entityId ecs = ecs.Disjunction<Velocity> entityId systems.[0]; ecs.Disjunction<Position> entityId systems.[1]
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
        let _ : Guid = ecs.RegisterCorrelated<StaticSpriteComponent> { RefCount = 0; Entity = entity; Sprite = AssetTag.make Assets.DefaultPackageName "Image4" } typeof<StaticSpriteComponent>.Name (entity.GetId world)
        world

    override this.Unregister (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let _ : bool = ecs.UnregisterCorrelated<StaticSpriteComponent> typeof<StaticSpriteComponent>.Name (entity.GetId world)
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
        let staticSprites = ecs.RegisterSystem (SystemCorrelated<StaticSpriteComponent, World> ())
#endif
#if ECS_PURE
        // get ecs
        let ecs = screen.GetEcs world

        // entity count
        let entityCount = 4000000

        // create systems
        let velocities = ecs.RegisterSystem (SystemCorrelated<Velocity, World> entityCount)
        let positions = ecs.RegisterSystem (SystemCorrelated<Position, World> entityCount)
        let movers = ecs.RegisterSystem (SystemCorrelated<Mover, World> entityCount)

        // create junctions
        for _ in 0 .. entityCount - 1 do
            let entityId = ecs.RegisterCorrelated<Mover> Unchecked.defaultof<Mover> typeof<Mover>.Name Gen.id
            let mover = ecs.IndexCorrelated<Mover> typeof<Mover>.Name entityId
            mover.Index.Velocity.Index.Velocity <- v2One

        // define update for movers
        let _ = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            let arr = movers.Components.Array
            for i in 0 .. arr.Length - 1 do
                let comp = &arr.[i]
                if comp.RefCount > 0 then
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
            let comps = staticSprites.Components
            for i in 0 .. comps.Length - 1 do
                let comp = &comps.[i]
                if comp.RefCount > 0 then
                    let entity = comp.Entity.State world
                    entity.Rotation <- entity.Rotation + 0.03f
            world)

        // define actualize for static sprites
        let _ = ecs.Subscribe EcsEvents.Actualize (fun _ _ _ world ->
            let messages = List ()
            let comps = staticSprites.Components
            for i in 0 ..comps.Length - 1 do
                let comp = &comps.[i]
                if comp.RefCount > 0 then
                    let entity = comp.Entity.State world
                    if entity.Visible then
                        let spriteDescriptor = SpriteDescriptor { Transform = entity.Transform; Offset = Vector2.Zero; InsetOpt = None; Image = comp.Sprite; Color = Vector4.One; Glow = Vector4.Zero; Flip = FlipNone }
                        let message = LayeredDescriptorMessage { Depth = entity.Depth; PositionY = entity.Position.Y; AssetTag = AssetTag.generalize comp.Sprite; RenderDescriptor = spriteDescriptor }
                        messages.Add message
            World.enqueueRenderMessages messages world)
#else
        ignore screen
#endif
        world

#if ELMISH
type ElmishGameDispatcher () =
    inherit GameDispatcher<int list list, int, unit> (List.init 32 (fun _ -> List.init 32 id)) // 1024 Elmish entities

    override this.Channel (_, game) =
        [game.UpdateEvent => msg 0]

    override this.Message (model, message, _, _) =
        match message with
        | 0 ->
            let model = List.map (List.map inc) model
            just model
        | _ -> just model

    override this.Content (model, _) =
        [Content.screen "Screen" Vanilla []
            [Content.layers model id constant (fun i ints _ ->
                Content.layer (scstring i) []
                    [Content.entities ints id constant (fun j int _ ->
                        Content.label (scstring j)
                            [Entity.Size <== int --> fun int -> v2 (single int) (single int)
                             Entity.Position == v2 (single i * 16.0f - 480.0f) (single j * 16.0f - 272.0f)])])
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