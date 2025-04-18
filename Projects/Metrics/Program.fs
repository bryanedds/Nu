﻿namespace Metrics
open System
open System.IO
open System.Numerics
open Prime
open Nu

type MetricsEntityDispatcher () =
    inherit Entity3dDispatcher<StaticModel AssetTag, Message, Command> (false, false, false, Assets.Default.StaticModel)

#if !MMCC
    override this.Update (entity, world) =
        entity.SetAngles (v3 0.0f 0.0f ((entity.GetAngles world).Z + 0.05f)) world
#endif

    override this.Render (renderPass, entity, world) =
        let staticModel = entity.GetModelGeneric world
        let mutable transform = entity.GetTransform world
        let affineMatrix = transform.AffineMatrix
        let castShadow = transform.CastShadow
        let presence = transform.Presence
        let properties = MaterialProperties.empty
        World.renderStaticModelFast (&affineMatrix, castShadow, presence, ValueNone, &properties, staticModel, LessThanOrEqualTest, DeferredRenderType, renderPass, world)

    override this.GetAttributesInferred (entity, world) =
        let staticModel = entity.GetModelGeneric world
        let bounds = (Metadata.getStaticModelMetadata staticModel).Bounds
        AttributesInferred.important bounds.Size bounds.Center

#if MMCC
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

type MmccGameMessage =
    | Inc
    interface Message

type MmccGameDispatcher () =
    inherit GameDispatcher<Intss, MmccGameMessage, Command> (Intss.init 115) // 13,225 MMCC entities (goal: 60FPS, current: 60FPS)

    override this.Definitions (_, _) =
        [Game.UpdateEvent => Inc]

    override this.Message (intss, message, _, _) =
        match message with
        | Inc -> just (Intss.inc intss)

    override this.Content (intss, _) =
        [Content.screen "Screen" Vanilla []
            [for (i, ints) in intss.Intss.Pairs' do
                Content.group (string i) []
                    [for (j, int) in ints.Ints.Pairs' do
                        Content.entity<MetricsEntityDispatcher> (string j)
                            [Entity.Presence == Omnipresent
                             Entity.Position == v3 (single i * 4.25f - 245.0f) (single j * 2.25f - 125.0f) -250.0f
                             Entity.Scale := v3Dup (single (int % 10)) * 0.5f]]
             Content.group "Other" []
                [Content.skyBox "SkyBox" []
                 Content.fps "Fps" [Entity.Position := v3 134.0f -168.0f 0.0f]]]]

    override this.Update (game, world) =
        let world = base.Update (game, world)        
        if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world
        then World.exit world
        else world
#else
type MyGameDispatcher () =
#if IMNUI
    inherit GameDispatcherImNui ()

    static let Positions = // 15,000 entities (goal: 60FPS, current 55FPS)
        [|for i in 0 .. dec 50 do
            for j in 0 .. dec 50 do
                for k in 0 .. dec 6 do
                    yield v3 (single i * 0.5f) (single j * 0.5f) (single k * 0.5f)|]

    override this.Process (_, world) =
        let (_, world) = World.beginScreen "Screen" true Vanilla [] world
        let world = World.beginGroup "Group" [] world
        let world = World.doFps "Fps" [Entity.Position .= v3 134.0f -168.0f 0.0f] world
        let world = World.doSkyBox "SkyBox" [] world
        let world =
            Array.foldi (fun i world position ->
                World.doEntity<MetricsEntityDispatcher> (string i)
                    [Entity.Presence .= Omnipresent
                     Entity.Position .= position + v3 -12.5f -12.5f -20.0f
                     Entity.Scale .= v3Dup 0.1f] world)
                world Positions
        let world = World.endGroup world
        let world = World.endScreen world
        world
#else
    inherit GameDispatcher ()

    static let Positions = // 40,000 entities (goal: 60FPS, current 55FPS)
        [|for i in 0 .. dec 50 do
            for j in 0 .. dec 50 do
                for k in 0 .. dec 16 do
                    yield v3 (single i * 0.5f) (single j * 0.5f) (single k * 0.5f)|]
    
    override this.Register (_, world) =
        let (screen, world) = World.createScreen (Some "Screen") world
        let (group, world) = World.createGroup (Some "Group") screen world
        let (fps, world) = World.createEntity<FpsDispatcher> DefaultOverlay (Some [|"Fps"|]) group world
        let world = fps.SetPosition (v3 134.0f -168.0f 0.0f) world
        let world = World.createEntity<SkyBoxDispatcher> DefaultOverlay None group world |> snd
        let world =
            Array.fold (fun world position ->
                let (entity, world) = World.createEntity<MetricsEntityDispatcher> NoOverlay (Some [|string Gen.id64|]) group world
                let world = entity.SetPresence Omnipresent world
                let world = entity.SetPosition (position + v3 -12.5f -12.5f -20.0f) world
                let world = entity.SetScale (v3Dup 0.1f) world
                world)
                world Positions
        World.selectScreen (IdlingState world.GameTime) screen world
#endif

    override this.Update (_, world) =
        if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world
        then World.exit world
        else world
#endif

type MetricsPlugin () =
    inherit NuPlugin ()

/// This program exists to take metrics on Nu's performance.
module Program =

    let [<EntryPoint; STAThread>] main _ =
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        Nu.init ()
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Metrics" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
#if FUNCTIONAL
        let worldConfig = { WorldConfig.defaultConfig with Imperative = false; SdlConfig = sdlConfig }
#else
        let worldConfig = { WorldConfig.defaultConfig with Imperative = true; SdlConfig = sdlConfig }
#endif
        World.run worldConfig (MetricsPlugin ())