namespace Metrics
open System
open System.Collections.Generic
open Prime
open Nu
open Nu.Declarative

type MetricsEntityDispatcher () =
    inherit EntityDispatcher ()

#if !OPTIMIZED && !ECS
    static member Facets =
        [typeof<StaticSpriteFacet>]
#endif

#if REACTIVE
    static member Properties =
        [define Entity.PublishChanges true]
#endif

#if OPTIMIZED
    static member Properties =
        [define Entity.Imperative true // makes updates faster by using mutation
         define Entity.Omnipresent true // makes updates faster by not touching the entity tree
         define (Entity.StaticData ()) // makes user-defined properties faster by using local data
            { DesignerType = typeof<Image AssetTag>
              DesignerValue = asset<Image> Assets.DefaultPackageName "Image4" }]
#endif

#if !ECS
    override this.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.03f) world
#endif

#if ECS
    override this.Register (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let _ : Guid = ecs.RegisterJunctioned<StaticSpriteComponent> { RefCount = 0; EntityComponent = Unchecked.defaultof<_>; Sprite = AssetTag.make Assets.DefaultPackageName Assets.DefaultImageName } typeof<StaticSpriteComponent>.Name (Alloc (entity.GetId world))
        world

    override this.Unregister (entity, world) =
        let ecs = entity.Parent.Parent.GetEcs world
        let _ : bool = ecs.UnregisterJunctioned<StaticSpriteComponent> typeof<StaticSpriteComponent>.Name (entity.GetId world)
        world
#endif

#if OPTIMIZED
    override this.Actualize (entity, world) =
        let transform = entity.GetTransform world
        let image = entity.GetStaticData world
        World.enqueueRenderMessage
            (LayeredDescriptorMessage
                { Depth = transform.Depth
                  PositionY = transform.Position.Y
                  AssetTag = AssetTag.generalize image
                  RenderDescriptor =
                      SpriteDescriptor
                        { Transform = transform
                          Offset = Vector2.Zero
                          InsetOpt = None
                          Image = image
                          Color = Vector4.One
                          Glow = Vector4.Zero
                          Flip = FlipNone }})
            world
#endif

type MyGameDispatcher () =
    inherit GameDispatcher<unit, unit, unit> (())

    let Fps = Simulants.DefaultLayer / "Fps"

    override this.Register (game, world) =
        let world = base.Register (game, world)
        let (screen, world) = World.createScreen (Some Simulants.DefaultScreen.Name) world
        let world = World.createLayer (Some Simulants.DefaultLayer.Name) Simulants.DefaultScreen world |> snd
        let world = World.createEntity<FpsDispatcher> (Some Fps.Name) DefaultOverlay Simulants.DefaultLayer world |> snd
        let world = Fps.SetPosition (v2 200.0f -250.0f) world
        let indices = // 9,900 entities
            seq {
                for i in 0 .. 74 do
                    for j in 0 .. 43 do
                        for k in 0 .. 2 do
                            yield v2 (single i * 12.0f + single k) (single j * 12.0f + single k) }
        let world =
            Seq.fold (fun world position ->
                let (entity, world) = World.createEntity<MetricsEntityDispatcher> None DefaultOverlay Simulants.DefaultLayer world
                let world = entity.SetPosition (position + v2 -450.0f -265.0f) world
                entity.SetSize (v2One * 8.0f) world)
                world indices
        let world = World.selectScreen Simulants.DefaultScreen world
#if ECS
        let ecs = screen.GetEcs world
        let staticSprites = ecs.IndexSystem<SystemCorrelated<StaticSpriteComponent, World>> typeof<StaticSpriteComponent>.Name
        let _ = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            let world = ref world
            let (arr, last) = staticSprites.Iter
            for i = 0 to last do
                let comp = &arr.[i]
                if  comp.Active then
                    let entity = comp.EntityComponent.Index.Entity
                    world := entity.SetRotation (entity.GetRotation world.Value + 0.03f) world.Value
            world.Value)
        let _ = ecs.Subscribe EcsEvents.Actualize (fun _ _ _ world ->
            let viewBounds = World.getViewBounds false world
            let messages = List<RenderMessage> ()
            let (arr, last) = staticSprites.Iter
            for i = 0 to last do
                let comp = &arr.[i]
                if  comp.Active then
                    let entity = comp.EntityComponent.Index.Entity
                    let transform = entity.GetTransform world
                    if  transform.Visible &&
                        Math.isBoundsInBounds (v4Bounds transform.Position transform.Size) viewBounds then
                        messages.Add
                            (LayeredDescriptorMessage
                                { Depth = transform.Depth
                                  PositionY = transform.Position.Y
                                  AssetTag = AssetTag.generalize comp.Sprite
                                  RenderDescriptor = SpriteDescriptor { Transform = transform; Offset = Vector2.Zero; InsetOpt = None; Image = comp.Sprite; Color = Vector4.One; Glow = Vector4.Zero; Flip = FlipNone }})
            World.enqueueRenderMessages messages world)
#endif
        world

type ElmishGameDispatcher () =
    inherit GameDispatcher<int list list, int, unit> (List.init 33 (fun _ -> List.init 33 id)) // 990 Elmish entities

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