module Nu.World
open System
open FSharpx
open SDL2
open OpenTK
open Nu.Physics
open Nu.Rendering
open Nu.Audio
open Nu.Game
open Nu.Sdl

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

/// Specifies the address of an element in a world.
/// Note that, similar to Mu, the last node of the address is the name of the event (such as "clicked").
/// Also note that subscribing to a partial address results in listening to all messages whose
/// beginning address nodes match the partial address (sort of a wild-card).
/// A value type.
type Address = Lun list

/// A generic message for the Nu game engine.
/// A reference type.
type [<ReferenceEquality>] Message =
    { Handled : bool
      Publisher : obj
      Data : obj }

/// Describes a game message subscription.
/// A reference type.
type [<ReferenceEquality>] Subscription =
    Subscription of (World -> Address -> Message -> World)

/// A map of game message subscriptions.
/// A reference type due to the reference-typeness of Subscription.
and Subscriptions =
    Map<Address, Map<IComparable, Subscription>>

/// The world, in a functional programming sense.
/// A reference type with some value semantics.
and [<ReferenceEquality>] World =
    { Game : Game
      Subscriptions : Subscriptions
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

/// Try to find an element at the given address.
let inline tryFind address (world : World) : obj option =
    None // TODO: forward to implemention in Nu.Game

/// Try to find an element at the given address.
let inline tryFindAs<'e when 'e : not struct> address (world : World) : 'e option =
    let optElement = tryFind address world
    match optElement with
    | None -> None
    | Some element ->
        match element with
        | :? 'e as elementAsE -> Some elementAsE
        | _ -> None

/// Set an element at the given address.
let inline set address (element : 'e when 'e : not struct) (world : World) : World =
    world // TODO: forward to implemention in Nu.Game

/// Remove an element at the given address.
let inline remove address (world : World) : World =
    world // TODO: forward to implemention in Nu.Game

/// Try to find an entity at the given address.
let inline tryFindEntity address world : Entity option =
    None // TODO: forward to implemention in Nu.Game

/// Try to find an actor at the given address.
let inline tryFindActor address world : Actor option =
    None // TODO: forward to implemention in Nu.Game

/// Publish a message to the given address.
let publish address message world : World =
    let optSubMap = Map.tryFind address world.Subscriptions
    match optSubMap with
    | None -> world
    | Some subMap ->
        Map.fold
            (fun newWorld subscriber (Subscription subscription) -> subscription newWorld address message)
            world
            subMap

/// Subscribe to messages at the given address.
let subscribe address subscription subscriber world : World =
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
    let newWorld = subscribe address subscription subscriber world
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
    let audioDescriptors = getAudioDescriptors world
    let audioMessages = world.AudioMessages
    let audioPlayer = world.AudioPlayer
    let newWorld = { world with AudioMessages = [] }
    Audio.play audioMessages audioDescriptors audioPlayer
    newWorld

let getComponentRenderDescriptors (world : World) : RenderDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetRenderDescriptors world :: descs) [] world.Components // TODO: get render descriptors
    List.collect (fun descs -> descs) descriptorLists

let getRenderDescriptors (world : World) : RenderDescriptor rQueue =
    let componentDescriptors = getComponentRenderDescriptors world
    let worldDescriptors = [] // TODO: get render descriptors
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

/// Render the world.
let render (world : World) : World =
    let renderDescriptors = getRenderDescriptors world
    let renderMessages = world.RenderMessages
    let renderer = world.Renderer
    let newWorld = { world with RenderMessages = [] }
    Rendering.render renderMessages renderDescriptors renderer
    newWorld

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
        { IsUp = true
          UpSprite = { AssetName = "Image"; PackageName = "Misc" }
          DownSprite = { AssetName = "Image"; PackageName = "Misc" }
          ClickSound = { Volume = 1.0f; AssetName = "Sound"; PackageName = "Misc" }}

    let testButtonGui =
        { Position = Vector2.Zero
          Size = Vector2 32.0f
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
      ActiveScreen = None }

let run sdlConfig =
    runSdl
        (fun sdlDeps ->
            { Game = createTestGame ()
              Subscriptions = Map.empty
              AudioPlayer = { AudioContext = () }
              Renderer = { RenderContext = () }
              Integrator = { PhysicsContext = () }
              AudioMessages = []
              RenderMessages = []
              PhysicsMessages = []
              Components = []
              SdlDeps = sdlDeps })
        (fun event sdlDeps world ->
            match event.Value.``type`` with
            | SDL.SDL_EventType.SDL_QUIT -> (false, world)
            | _ -> (true, world))
        (fun sdlDeps world ->
            let newWorld = integrate world
            (true, newWorld))
        (fun sdlDeps world ->
            let newWorld = render world
            play newWorld)
        sdlConfig