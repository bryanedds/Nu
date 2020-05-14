namespace Metrics
open System
open Prime
open Nu
open Nu.Declarative

type MetricsEntityDispatcher () =
    inherit EntityDispatcher ()

#if !OPTIMIZE
    static member Facets =
        [typeof<StaticSpriteFacet>]
#endif

#if REACTIVE
    static member Properties =
        [define Entity.PublishChanges true]
#endif

#if OPTIMIZE
    static member Properties =
        [define Entity.Imperative true // makes updates faster by using mutation
         define Entity.Omnipresent true // makes updates faster by not touching the entity tree
         define Entity.IgnoreLayer true // makes actualization faster by not querying the containing layer
         define (Entity.StaticData ()) // makes user-defined properties faster by using local data
            { DesignerType = typeof<Image AssetTag>
              DesignerValue = asset<Image> Assets.DefaultPackageName "Image4" }]
#endif

    override this.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.03f) world

#if OPTIMIZE
    override this.Actualize (entity, world) =
        let position = entity.GetPosition world
        let image = entity.GetStaticData world
        World.enqueueRenderMessage
            (RenderDescriptorMessage
                (LayerableDescriptor
                    { Depth = entity.GetDepthLayered world
                      AssetTag = image
                      PositionY = position.Y
                      LayeredDescriptor =
                      SpriteDescriptor
                        { Position = position
                          Size = entity.GetSize world
                          Rotation = entity.GetRotation world
                          Offset = Vector2.Zero
                          ViewType = entity.GetViewType world
                          InsetOpt = None
                          Image = image
                          Color = Vector4.One
                          Glow = Vector4.Zero
                          Flip = FlipNone }}))
            world
#endif

type MyGameDispatcher () =
    inherit GameDispatcher<unit, unit, unit> (())

    let Fps = Simulants.DefaultLayer / "Fps"

    override this.Register (game, world) =
        let world = base.Register (game, world)
        let world = World.createScreen (Some Simulants.DefaultScreen.Name) world |> snd
        let world = World.createLayer (Some Simulants.DefaultLayer.Name) Simulants.DefaultScreen world |> snd
        let world = World.createEntity<FpsDispatcher> (Some Fps.Name) DefaultOverlay Simulants.DefaultLayer world |> snd
        let world = Fps.SetPosition (v2 200.0f -250.0f) world
        let indices = // 13,200 entities
            seq {
                for i in 0 .. 74 do
                    for j in 0 .. 43 do
                        for k in 0 .. 3 do
                            yield v2 (single i * 12.0f + single k) (single j * 12.0f + single k) }
        let world =
            Seq.fold (fun world position ->
                let (entity, world) = World.createEntity<MetricsEntityDispatcher> None DefaultOverlay Simulants.DefaultLayer world
                let world = entity.SetPosition (position + v2 -450.0f -265.0f) world
                entity.SetSize (v2One * 8.0f) world)
                world indices
        World.selectScreen Simulants.DefaultScreen world

type MetricsPlugin () =
    inherit NuPlugin ()
    override this.GetGameDispatcher () = typeof<MyGameDispatcher>

/// This program exists to take metrics on Nu's performance.
module Program =

    let [<EntryPoint; STAThread>] main _ =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        Nu.init worldConfig.NuConfig
        World.run worldConfig (MetricsPlugin ())