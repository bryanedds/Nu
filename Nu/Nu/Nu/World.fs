module Nu.World
open System
open FSharpx
open FSharpx.Lens.Operators
open SDL2
open OpenTK
open Nu.Core
open Nu.Constants
open Nu.Math
open Nu.Physics
open Nu.Rendering
open Nu.Input
open Nu.Audio
open Nu.Game
open Nu.Sdl

// TODO: consider converting these from slash-delimited strings
let MouseLeft0 = [Lun.make "mouse"; Lun.make "left"; Lun.make "0"]
let DownMouseLeft0 = Lun.make "down" :: MouseLeft0
let UpMouseLeft0 = Lun.make "up" :: MouseLeft0
let TestScreenAddress = [Lun.make "testScreen"]
let TestGroup = TestScreenAddress @ [Lun.make "testGroup"]
let TestButton = TestGroup @ [Lun.make "testButton"]
let DownTestButton = Lun.make "down" :: TestButton
let UpTestButton = Lun.make "up" :: TestButton
let ClickTestButton = Lun.make "click" :: TestButton

// WISDOM: On avoiding threads where possible...
//
// Beyond the cases where persistent threads are absolutely required or where transient threads
// implement embarassingly parallel processes, threads should be AVOIDED as a rule.
//
// If it were the case that physics were processed on a separate hardware component and thereby
// ought to be run on a separate persistent thread, then the proper way to approach the problem of
// physics system queries is to copy the relevant portion of the physics state from the PPU to main
// memory every frame. This way, queries against the physics state can be done IMMEDIATELY with no
// need for complex intermediate states (albeit against a physics state that is one frame old).

/// Describes data relevant to specific event messages.
type [<ReferenceEquality>] MessageData =
    | MouseButtonData of Vector2 * MouseButton
    | OtherData of obj
    | NoData

/// A generic message for the Nu game engine.
/// A reference type.
type [<ReferenceEquality>] Message =
    { Handled : bool
      Data : MessageData }

/// Describes a game message subscription.
/// A reference type.
type [<ReferenceEquality>] Subscription =
    Subscription of (Address -> Address -> Message -> World -> World)

/// A map of game message subscriptions.
/// A reference type due to the reference-typeness of Subscription.
and Subscriptions = Map<Address, Map<Address, Subscription>>

/// The world, in a functional programming sense.
/// A reference type with some value semantics.
and [<ReferenceEquality>] World =
    { Game : Game
      Subscriptions : Subscriptions
      MouseState : MouseState
      AudioPlayer : AudioPlayer
      Renderer : Renderer
      Integrator : Integrator
      AudioMessages : AudioMessage rQueue
      RenderMessages : RenderMessage rQueue
      PhysicsMessages : PhysicsMessage rQueue
      Components : IWorldComponent list }
    with
        static member game =
            { Get = fun this -> this.Game
              Set = fun game this -> { this with Game = game }}

        static member mouseState =
            { Get = fun this -> this.MouseState
              Set = fun mouseState this -> { this with MouseState = mouseState }}
        
        static member optActiveScreenAddress =
            World.game >>| Game.optActiveScreenAddress
        
        static member screen (address : Address) =
            World.game >>| Game.screen address
        
        static member group (address : Address) =
            World.game >>| Game.group address
        
        static member entity (address : Address) =
            World.game >>| Game.entity address
        
        static member entityGui (address : Address) =
            World.game >>| Game.entityGui address
        
        static member entityGuiButton (address : Address) =
            World.game >>| Game.entityGuiButton address
        
        static member optEntity (address : Address) =
            World.game >>| Game.optEntity address
        
        static member optEntityGui (address : Address) =
            World.game >>| Game.optEntityGui address
        
        static member optEntityGuiButton (address : Address) =
            World.game >>| Game.optEntityGuiButton address

/// Enables components that open the world for extension.
and IWorldComponent =
    interface
        abstract member GetAudioDescriptors : World -> AudioDescriptor list
        abstract member GetRenderDescriptors : World -> RenderDescriptor list
        // TODO: abstract member GetRenderMessages : World -> RenderMessage rQueue
        // TODO: abstract member GetPhysicsMessages : World -> PhysicsMessage rQueue
        // TODO: abstract member HandleIntegrationMessages : IntegrationMessage rQueue -> World -> World
        end

/// Publish a message to the given address.
let publish address message world : World =
    let optSubMap = Map.tryFind address world.Subscriptions
    match optSubMap with
    | None -> world
    | Some subMap ->
        Map.fold
            (fun world2 subscriber (Subscription subscription) -> subscription address subscriber message world2)
            world
            subMap

/// Subscribe to messages at the given address.
let subscribe address subscriber subscription world : World =
    let sub = Subscription subscription
    let subs = world.Subscriptions
    let optSubMap = Map.tryFind address subs
    { world with
        Subscriptions =
            match optSubMap with
            | None -> let newSubMap = Map.singleton subscriber sub in Map.add address newSubMap subs
            | Some subMap -> let newSubMap = Map.add subscriber sub subMap in Map.add address newSubMap subs }

/// Unsubscribe to messages at the given address.
let unsubscribe address subscriber world : World =
    let subs = world.Subscriptions
    let optSubMap = Map.tryFind address subs
    match optSubMap with
    | None -> world
    | Some subMap ->
        let subMap2 = Map.remove subscriber subMap in
        let subscriptions2 = Map.add address subMap2 subs
        { world with Subscriptions = subscriptions2 }

/// Execute a procedure within the context of a given subscription at the given address.
let withSubscription address subscription subscriber procedure world : World =
    let world2 = subscribe address subscriber subscription world
    let world3 = procedure world2
    unsubscribe address subscriber world3

let getComponentAudioDescriptors (world : World) : AudioDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetAudioDescriptors world :: descs) [] world.Components // TODO: get audio descriptors
    List.collect (fun descs -> descs) descriptorLists

let getAudioDescriptors (world : World) : AudioDescriptor rQueue =
    let componentDescriptors = getComponentAudioDescriptors world
    let worldDescriptors = [] // TODO: get audio descriptors
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

/// Play the world.
let play (world : World) : World =
    let audioMessages = world.AudioMessages
    let audioDescriptors = getAudioDescriptors world
    let audioPlayer = world.AudioPlayer
    let newWorld = { world with AudioMessages = [] }
    Audio.play audioMessages audioDescriptors audioPlayer
    newWorld

let getComponentRenderDescriptors (world : World) : RenderDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetRenderDescriptors world :: descs) [] world.Components // TODO: get render descriptors
    List.collect (fun descs -> descs) descriptorLists

let getWorldRenderDescriptors world =
    match get world World.optActiveScreenAddress with
    | None -> []
    | Some activeScreenAddress ->
        let activeScreen = get world (World.screen activeScreenAddress)
        let groups = LunTrie.toValueSeq activeScreen.Groups
        let optDescriptorSeqs =
            Seq.map
                (fun group ->
                    let entities = LunTrie.toValueSeq group.Entities
                    Seq.map
                        (fun entity ->
                            match entity.EntitySemantic with
                            | Gui gui ->
                                match gui.GuiSemantic with
                                | Button button -> Some (SpriteDescriptor { Position = gui.Position; Size = gui.Size; Sprite = if button.IsDown then button.DownSprite else button.UpSprite })
                                | Label label -> Some (SpriteDescriptor { Position = gui.Position; Size = gui.Size; Sprite = label.Sprite })
                            | Actor actor ->
                                match actor.ActorSemantic with
                                | Avatar -> None
                                | Item -> None)
                        entities)
                groups
        let optDescriptors = Seq.concat optDescriptorSeqs
        let descriptors = Seq.definitize optDescriptors
        List.ofSeq descriptors

let getRenderDescriptors (world : World) : RenderDescriptor rQueue =
    let componentDescriptors = getComponentRenderDescriptors world
    let worldDescriptors = getWorldRenderDescriptors world
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

/// Render the world.
let render (world : World) : World =
    let renderMessages = world.RenderMessages
    let renderDescriptors = getRenderDescriptors world
    let renderer = world.Renderer
    let renderer2 = Rendering.render renderMessages renderDescriptors renderer
    let world2 = {{ world with RenderMessages = [] } with Renderer = renderer2 }
    world2

/// Handle physics integration messages.
let handleIntegrationMessages integrationMessages world : World =
    world // TODO: handle integration messages

/// Integrate the world.
let integrate (world : World) : World =
    let integrationMessages = Physics.integrate world.PhysicsMessages world.Integrator
    let world2 = { world with PhysicsMessages = [] }
    handleIntegrationMessages integrationMessages world2

let createTestGame () =
    
    let testButton =
        { IsDown = false
          UpSprite = { AssetName = Lun.make "Image"; PackageName = Lun.make "Misc" }
          DownSprite = { AssetName = Lun.make "Image2"; PackageName = Lun.make "Misc" }
          ClickSound = { Volume = 1.0f; AssetName = Lun.make "Sound"; PackageName = Lun.make "Misc" }}

    let testButtonGui =
        { Position = Vector2 100.0f
          Size = Vector2 512.0f // TODO: look this up from bitmap file
          GuiSemantic = Button testButton }

    let testButtonEntity =
        { ID = getNuId ()
          IsEnabled = true
          IsVisible = true
          EntitySemantic = Gui testButtonGui }
          
    let testGroup =
        { ID = getNuId ()
          IsEnabled = true
          IsVisible = true
          Entities = LunTrie.singleton (Lun.make "testButton") testButtonEntity }

    let testScreen =
        { ID = getNuId ()
          IsEnabled = true
          IsVisible = true
          Groups = LunTrie.singleton (Lun.make "testGroup") testGroup
          ScreenSemantic = TestScreen { Unused = () }}

    { ID = getNuId ()
      IsEnabled = false
      Screens = LunTrie.singleton (Lun.make "testScreen") testScreen
      OptActiveScreenAddress = Some TestScreenAddress }

let createTestWorld sdlDeps =

    let testWorld =
        { Game = createTestGame ()
          Subscriptions = Map.empty
          MouseState = { MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
          AudioPlayer = makeAudioPlayer ()
          Renderer = makeRenderer sdlDeps.RenderContext
          Integrator = makeIntegrator Gravity
          AudioMessages = []
          RenderMessages = []
          PhysicsMessages = []
          Components = [] }

    let hintRenderingPackageUse =
        { FileName = "AssetGraph.xml"
          PackageName = "Misc"
          HRPU = () }

    let testWorld2 = { testWorld with RenderMessages = HintRenderingPackageUse hintRenderingPackageUse :: testWorld.RenderMessages }
    
    let testWorld3 =
        subscribe
            DownMouseLeft0
            TestButton
            (fun address subscriber message world ->
                match message.Data with
                | MouseButtonData (mousePosition, _) ->
                    let (entity, gui, button) = get world (World.entityGuiButton subscriber)
                    if entity.IsEnabled && entity.IsVisible then
                        if isInBox3 mousePosition gui.Position gui.Size then
                            let button2 = { button with IsDown = true }
                            let world2 = set (entity, gui, button2) world (World.entityGuiButton subscriber)
                            publish DownTestButton { Handled = false; Data = NoData } world2
                        else world
                    else world
                | _ -> failwith ("Expected MouseClickData from address '" + str address + "'."))
            testWorld2

    subscribe
        UpMouseLeft0
        TestButton
        (fun address subscriber message world ->
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let (entity, gui, button) = get world (World.entityGuiButton subscriber)
                if entity.IsEnabled && entity.IsVisible then
                    let world2 =
                        let button2 = { button with IsDown = false }
                        let world2b = set (entity, gui, button2) world (World.entityGuiButton subscriber)
                        publish UpTestButton { Handled = false; Data = NoData } world2b
                    if isInBox3 mousePosition gui.Position gui.Size then publish ClickTestButton { Handled = false; Data = NoData } world2
                    else world2
                else world
            | _ -> failwith ("Expected MouseClickData from address '" + str address + "'."))
        testWorld3

let run sdlConfig =
    runSdl
        (fun sdlDeps ->
            createTestWorld sdlDeps)
        (fun refEvent world ->
            let event = refEvent.Value
            match event.``type`` with
            | SDL.SDL_EventType.SDL_QUIT ->
                (false, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                if event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (Vector2 (single event.button.x, single event.button.y), MouseLeft)
                    let world2 = set { world.MouseState with MouseLeftDown = true } world World.mouseState
                    let world3 = publish DownMouseLeft0 { Handled = false; Data = messageData } world2
                    (true, world3)
                else (true, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                let mouseState = world.MouseState
                if mouseState.MouseLeftDown && event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (Vector2 (single event.button.x, single event.button.y), MouseLeft)
                    let world2 = set { world.MouseState with MouseLeftDown = false } world World.mouseState
                    let world3 = publish UpMouseLeft0 { Handled = false; Data = messageData } world2
                    (true, world3)
                else (true, world)
            | _ ->
                (true, world))
        (fun world ->
            let world2 = integrate world
            (true, world2))
        (fun world ->
            let world2 = render world
            play world2) // TODO: put play in its own callback!
        (fun world ->
            { world with Renderer = handleRenderExit world.Renderer })
        sdlConfig