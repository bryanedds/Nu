module Nu.World
open System
open FSharpx
open SDL2
open OpenTK
open Nu.Physics
open Nu.Rendering
open Nu.Input
open Nu.Audio
open Nu.Game
open Nu.Sdl

// TODO: consider converting these from slash-delimited strings
let MouseLeft0 = [Lun.make "mouse"; Lun.make "left"; Lun.make "0"]
let ClickMouseLeft0 = Lun.make "click" :: MouseLeft0
let DownMouseLeft0 = Lun.make "down" :: MouseLeft0
let UpMouseLeft0 = Lun.make "up" :: MouseLeft0
let TestScreenAddress = [Lun.make "testScreen"]
let TestGroup = TestScreenAddress @ [Lun.make "testGroup"]
let TestButton = TestGroup @ [Lun.make "testButton"]
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
and Subscriptions =
    Map<Address, Map<Address, Subscription>>

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
      Components : IWorldComponent list
      SdlDeps : SdlDeps }

/// Enables components that open the world for extension.
and IWorldComponent =
    interface
        abstract member GetAudioDescriptors : World -> AudioDescriptor list
        abstract member GetRenderDescriptors : World -> RenderDescriptor list
        // TODO: abstract member GetRenderMessages : World -> RenderMessage rQueue
        // TODO: abstract member GetPhysicsMessages : World -> PhysicsMessage rQueue
        // TODO: abstract member HandleIntegrationMessages : IntegrationMessage rQueue -> World -> World
        end

let find4 successFn errorFn address (world : World) =
    // TODO: forward to implemention in Nu.Game
    match address with
    | [] -> errorFn ("Invalid address '" + str address + "'.")
    | head :: tail ->
        let optScreen = LunTrie.tryFind head world.Game.Screens
        match optScreen with
        | None -> errorFn ("No screen at address '" + str address + "'.")
        | Some screen ->
            match tail with
            | [] -> successFn (screen :> obj)
            | head :: tail ->
                let optGroup = LunTrie.tryFind head screen.Groups
                match optGroup with
                | None -> errorFn ("No group at address '" + str address + "'.")
                | Some group ->
                    match tail with
                    | [] -> successFn (group :> obj)
                    | head :: tail ->
                        let optEntity = LunTrie.tryFind head group.Entities
                        match optEntity with
                        | None -> errorFn ("No entity at address '" + str address + "'.")
                        | Some entity -> successFn (entity :> obj)
                
/// Find an element at the given address, failing with exception otherwise.
let inline findInWorld requirement address (world : World) =
    findInGame requirement address world.Game

/// Try to find an element at the given address.
let inline tryFindInWorld address (world : World) =
    tryFindInGame address world.Game

/// Set an element at the given address.
let inline setInWorld address (element : 'e when 'e : not struct) (world : World) : World =
    { world with Game = setInGame address element world.Game }

/// Remove an element at the given address.
let inline removeInWorld address (world : World) : World =
    { world with Game = removeInGame address world.Game }

/// Lens for a particular address in a world.
let forAddress requirement address =
    { Get = findInWorld requirement address
      Set = 
          function
          | Some value -> setInWorld address value
          | None -> removeInWorld address }

let withButton4 fn errorFn address world : World =
    let result = findInWorld requireEntity address world
    match result.OptEntity.Value.EntitySemantic with
    | Gui gui ->
        match gui.GuiSemantic with
        | Button button -> fn result gui button
        | _ -> errorFn ("Expecting button at address '" + str address + "'.")
    | _ -> errorFn ("Expecting gui at address '" + str address + "'.")

let withButton fn address world =
    withButton4 fn failwith address world

let tryWithButton fn address world =
    withButton4 fn (fun _ -> world) address world

/// Publish a message to the given address.
let publish address message world : World =
    let optSubMap = Map.tryFind address world.Subscriptions
    match optSubMap with
    | None -> world
    | Some subMap ->
        Map.fold
            (fun newWorld subscriber (Subscription subscription) -> subscription address subscriber message newWorld)
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
        let newSubMap = Map.remove subscriber subMap in
        let newSubscriptions = Map.add address newSubMap subs
        { world with Subscriptions = newSubscriptions }

/// Execute a procedure within the context of a given subscription at the given address.
let withSubscription address subscription subscriber procedure world : World =
    let newWorld = subscribe address subscriber subscription world
    let newWorld2 = procedure newWorld
    unsubscribe address subscriber newWorld2

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
    match world.Game.OptActiveScreen with
    | None -> []
    | Some activeScreenAddress ->
        let optActiveScreen = (tryFindInWorld activeScreenAddress world).OptScreen
        match optActiveScreen with
        | None -> debug ("Could not find active screen with address '" + str activeScreenAddress + "'."); []
        | Some activeScreen ->
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
    let newRenderer = Rendering.render renderMessages renderDescriptors renderer
    let newWorld = {{ world with RenderMessages = [] } with Renderer = newRenderer }
    newWorld

let handleRenderExit (world : World) : World =
    let renderer = world.Renderer
    let renderAssetTries = LunTrie.toValueSeq renderer.RenderAssetMap
    let renderAssets = Seq.collect LunTrie.toValueSeq renderAssetTries
    for renderAsset in renderAssets do
        match renderAsset with
        | TextureAsset texture -> () // apparently there is no need to free textures in SDL
    let newRenderer = { renderer with RenderAssetMap = LunTrie.empty }
    { world with Renderer = newRenderer }

/// Handle physics integration messages.
let handleIntegrationMessages integrationMessages world : World =
    world // TODO: handle integration messages

/// Integrate the world.
let integrate (world : World) : World =
    let integrationMessages = Physics.integrate world.PhysicsMessages world.Integrator
    let newWorld = { world with PhysicsMessages = [] }
    handleIntegrationMessages integrationMessages newWorld

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
          Enabled = true
          Visible = true
          EntitySemantic = Gui testButtonGui }
          
    let testGroup =
        { ID = getNuId ()
          Enabled = true
          Visible = true
          Entities = LunTrie.singleton (Lun.make "testButton") testButtonEntity }

    let testScreen =
        { ID = getNuId ()
          Enabled = true
          Visible = true
          Groups = LunTrie.singleton (Lun.make "testGroup") testGroup
          ScreenSemantic = TestScreen { Unused = () }}

    { ID = getNuId ()
      Enabled = false
      Screens = LunTrie.singleton (Lun.make "testScreen") testScreen
      OptActiveScreen = Some TestScreenAddress }

let createTestWorld sdlDeps =

    let testWorld =
        { Game = createTestGame ()
          Subscriptions = Map.empty
          MouseState = { MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
          AudioPlayer = { AudioContext = () }
          Renderer = { RenderContext = sdlDeps.RenderContext; RenderAssetMap = LunTrie.empty }
          Integrator = { PhysicsContext = () }
          AudioMessages = []
          RenderMessages = []
          PhysicsMessages = []
          Components = []
          SdlDeps = sdlDeps }

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
                | MouseButtonData (position, button) ->
                    withButton
                        (fun findResult gui button ->
                            if position.X >= gui.Position.X &&
                               position.X < gui.Position.X + gui.Size.X &&
                               position.Y >= gui.Position.Y &&
                               position.Y < gui.Position.Y + gui.Size.Y
                            then
                                let newButton = { button with IsDown = true }
                                let newGui = { gui with GuiSemantic = Button newButton }
                                let newEntity = { findResult.OptEntity.Value with EntitySemantic = Gui newGui }
                                let newWorld = setInWorld subscriber newEntity world
                                publish ClickTestButton { Handled = false; Data = NoData } newWorld
                            else world)
                        subscriber
                        world
                | _ -> failwith ("Expected MouseClickData from address '" + str address + "'."))
            testWorld2

    subscribe
        ClickMouseLeft0
        TestButton
        (fun address subscriber message world ->
            match message.Data with
            | MouseButtonData (position, button) ->
                withButton
                    (fun findResult gui button ->
                        if position.X >= gui.Position.X &&
                           position.X < gui.Position.X + gui.Size.X &&
                           position.Y >= gui.Position.Y &&
                           position.Y < gui.Position.Y + gui.Size.Y
                        then
                            let newButton = { button with IsDown = false }
                            let newGui = { gui with GuiSemantic = Button newButton }
                            let newEntity = { findResult.OptEntity.Value with EntitySemantic = Gui newGui }
                            let newWorld = setInWorld subscriber newEntity world
                            publish ClickTestButton { Handled = false; Data = NoData } newWorld
                        else world)
                    subscriber
                    world
            | _ -> failwith ("Expected MouseClickData from address '" + str address + "'."))
        testWorld3

let run sdlConfig =
    runSdl
        (fun sdlDeps ->
            createTestWorld sdlDeps)
        (fun refEvent sdlDeps world ->
            let event = refEvent.Value
            match event.``type`` with
            | SDL.SDL_EventType.SDL_QUIT ->
                (false, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                let mouseState = world.MouseState
                if event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (Vector2 (single event.button.x, single event.button.y), MouseLeft)
                    let newMouseState = { mouseState with MouseLeftDown = true }
                    let newWorld = { world with MouseState = newMouseState }
                    let newWorld2 = publish DownMouseLeft0 { Handled = false; Data = messageData } newWorld
                    (true, newWorld2)
                else (true, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                let mouseState = world.MouseState
                if mouseState.MouseLeftDown && event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (Vector2 (single event.button.x, single event.button.y), MouseLeft)
                    let newMouseState = { mouseState with MouseLeftDown = false }
                    let newWorld = { world with MouseState = newMouseState }
                    let newWorld2 = publish UpMouseLeft0 { Handled = false; Data = messageData } newWorld
                    let newWorld3 = publish ClickMouseLeft0 { Handled = false; Data = messageData } newWorld2
                    (true, newWorld3)
                else (true, world)
            | _ ->
                (true, world))
        (fun sdlDeps world ->
            let newWorld = integrate world
            (true, newWorld))
        (fun sdlDeps world ->
            let newWorld = render world
            play newWorld)
        (fun sdlDeps world ->
            handleRenderExit world)
        sdlConfig