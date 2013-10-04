module Nu.Simulation
open System
open System.Collections.Generic
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
open Nu.Simulants
open Nu.Sdl

let MouseLeftAddress = [Lun.make "mouse"; Lun.make "left"]
let DownMouseLeftAddress = Lun.make "down" :: MouseLeftAddress
let UpMouseLeftAddress = Lun.make "up" :: MouseLeftAddress

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
and Subscriptions = Dictionary<Address, Dictionary<Address, Subscription>>

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
    let optSubDict = Dictionary.tryFind address world.Subscriptions
    match optSubDict with
    | None -> world
    | Some subDict ->
        Seq.fold
            (fun world2 (subKvp : KeyValuePair<_, _>) ->
                match (subKvp.Key, subKvp.Value) with
                | (subscriber, Subscription subscription) ->
                    subscription address subscriber message world)
            world
            subDict

/// Subscribe to messages at the given address.
let subscribe address subscriber subscription world : World =
    let sub = Subscription subscription
    let subs = world.Subscriptions
    let optSubDict = Dictionary.tryFind address subs
    match optSubDict with
    | None -> subs.Add (address, Dictionary.singleton (subscriber, sub))
    | Some subDict -> subDict.Add (subscriber, sub)
    world

/// Unsubscribe to messages at the given address.
let unsubscribe address subscriber world : World =
    let subs = world.Subscriptions
    let optSubDict = Dictionary.tryFind address subs
    match optSubDict with
    | None ->
        debug ("No such subscription found '" + str address + "' / '" + str subscriber + "'.")
        world
    | Some subDict ->
        if not (subDict.Remove subscriber) then
            debug ("No such subscription found '" + str address + "' / '" + str subscriber + "'.")
        world

/// Execute a procedure within the context of a given subscription at the given address.
let withSubscription address subscription subscriber procedure world : World =
    let world2 = subscribe address subscriber subscription world
    let world3 = procedure world2
    unsubscribe address subscriber world3

let getEntityC address world =
    getEntityFromGame address world.Game

let getEntity<'e when 'e :> Entity> address world =
    getEntityFromGame<'e> address world.Game

let tryGetEntityC address world =
    tryGetEntityFromGame address world.Game

let tryGetEntity<'e when 'e :> Entity> address world =
    tryGetEntityFromGame<'e> address world.Game

let addEntity entity address world : World =
    addEntityToGame entity address world.Game
    world

let removeEntity address world : World =
    removeEntityFromGame address world.Game
    world

let unregisterButton address world : World =
    let world2 = unsubscribe DownMouseLeftAddress address world
    unsubscribe UpMouseLeftAddress address world2

let registerButton address world : World =
    let world_ =
        subscribe
            DownMouseLeftAddress
            address
            (fun address subscriber message world ->
                match message.Data with
                | MouseButtonData (mousePosition, _) ->
                    let button = getEntity<Button> subscriber world
                    if button.IsEnabled && button.IsVisible then
                        if isInBox3 mousePosition button.Position button.Size then
                            button.IsDown <- true
                            publish (Lun.make "down" :: subscriber) { Handled = false; Data = NoData } world
                        else world
                    else world
                | _ -> failwith ("Expected MouseClickData from address '" + str address + "'."))
            world
    subscribe
        UpMouseLeftAddress
        address
        (fun address subscriber message world ->
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let button = getEntity<Button> subscriber world
                if button.IsEnabled && button.IsVisible then
                    button.IsDown <- false
                    let world2 = publish (Lun.make "up" :: subscriber) { Handled = false; Data = NoData } world
                    if isInBox3 mousePosition button.Position button.Size
                    then publish (Lun.make "click" :: subscriber) { Handled = false; Data = NoData } world2
                    else world2
                else world
            | _ -> failwith ("Expected MouseClickData from address '" + str address + "'."))
        world_

let addButton button address world : World =
    let world2 = addEntity button address world
    registerButton address world2

let removeButton address world : World =
    let world2 = unregisterButton address world
    removeEntity address world2

let addLabel label address world : World =
    addEntity label address world

let removeLabel address world : World =
    removeEntity address world

let unregisterBlock block address world : World =
    let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block.PhysicsId }
    { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

let registerBlock (block : Block) address world : World =
    let bodyCreateMessage =
        BodyCreateMessage
            { EntityAddress = address
              PhysicsId = block.PhysicsId
              Shape = BoxShape { Center = Vector2.Zero; Extent = block.Size * 0.5f }
              Position = block.Position + block.Size * 0.5f
              Rotation = block.Rotation
              Density = block.Density
              BodyType = block.BodyType }
    { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }

let addBlock block address world : World =
    let world2 = addEntity block address world
    registerBlock block address world2

let removeBlock block address world : World =
    let world2 = unregisterBlock block address world
    removeEntity address world2

let getGroupC address world =
    getGroupFromGame address world.Game

let getGroup<'g when 'g :> Group> address world =
    getGroupFromGame<'g> address world.Game

let tryGetGroupC address world =
    tryGetGroupFromGame address world.Game

let tryGetGroup<'g when 'g :> Group> address world =
    tryGetGroupFromGame<'g> address world.Game

let addGroup group address world : World =
    addGroupToGame group address world.Game
    world

let removeGroup address world : World =
    removeGroupFromGame address world.Game
    world

let getScreenC address world =
    getScreenFromGame address world.Game

let getScreen<'s when 's :> Screen> address world =
    getScreenFromGame<'s> address world.Game

let tryGetScreenC address world =
    tryGetScreenFromGame address world.Game

let tryGetScreen<'s when 's :> Screen> address world =
    tryGetScreenFromGame<'s> address world.Game

let addScreen screen address world : World =
    addScreenToGame screen address world.Game
    world

let removeScreen address world : World =
    removeScreenFromGame address world.Game
    world

let getComponentAudioDescriptors world : AudioDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetAudioDescriptors world :: descs) [] world.Components // TODO: get audio descriptors
    List.collect (fun descs -> descs) descriptorLists

let getAudioDescriptors world : AudioDescriptor rQueue =
    let componentDescriptors = getComponentAudioDescriptors world
    let worldDescriptors = [] // TODO: get audio descriptors
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

/// Play the world.
let play world : World =
    let audioMessages = world.AudioMessages
    let audioDescriptors = getAudioDescriptors world
    let audioPlayer = world.AudioPlayer
    let newWorld = { world with AudioMessages = [] }
    Audio.play audioMessages audioDescriptors audioPlayer
    newWorld

let getComponentRenderDescriptors world : RenderDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetRenderDescriptors world :: descs) [] world.Components // TODO: get render descriptors
    List.collect (fun descs -> descs) descriptorLists

let getWorldRenderDescriptors world =
    match world.Game.OptActiveScreenAddress with
    | None -> []
    | Some activeScreenAddress ->
        let activeScreen = getScreen activeScreenAddress world
        let groups = activeScreen.Groups.Values
        let optDescriptorSeqs =
            Seq.map
                (fun (group : Group) ->
                    let entities = group.Entities.Values
                    Seq.map
                        (fun (entity : Entity) ->
                            match entity with
                            | :? Button as button -> Some (SpriteDescriptor { Position = button.Position; Size = button.Size; Sprite = if button.IsDown then button.DownSprite else button.UpSprite })
                            | :? Label as label -> Some (SpriteDescriptor { Position = label.Position; Size = label.Size; Sprite = label.Sprite })
                            | :? Block as block -> Some (SpriteDescriptor { Position = block.Position; Size = block.Size; Sprite = block.Sprite })
                            | _ -> None)
                        entities)
                groups
        let optDescriptors = Seq.concat optDescriptorSeqs
        let descriptors = Seq.definitize optDescriptors
        List.ofSeq descriptors

let getRenderDescriptors world : RenderDescriptor rQueue =
    let componentDescriptors = getComponentRenderDescriptors world
    let worldDescriptors = getWorldRenderDescriptors world
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

/// Render the world.
let render world : World =
    let renderMessages = world.RenderMessages
    let renderDescriptors = getRenderDescriptors world
    let renderer = world.Renderer
    let renderer2 = Rendering.render renderMessages renderDescriptors renderer
    let world2 = {{ world with RenderMessages = [] } with Renderer = renderer2 }
    world2

let handleIntegrationMessage world integrationMessage : World =
    match integrationMessage with
    | BodyTransformMessage bodyTransformMessage ->
        let actor = getEntity<Actor> bodyTransformMessage.EntityAddress world
        actor.Position <- bodyTransformMessage.Position - actor.Size * 0.5f
        actor.Rotation <- bodyTransformMessage.Rotation
        world
    | BodyCollisionMessage bodyCollisionMessage -> world // TODO: play collision sound

/// Handle physics integration messages.
let handleIntegrationMessages integrationMessages world : World =
    List.fold handleIntegrationMessage world integrationMessages

/// Integrate the world.
let integrate world : World =
    let integrationMessages = Physics.integrate world.PhysicsMessages world.Integrator
    let world2 = { world with PhysicsMessages = [] }
    handleIntegrationMessages integrationMessages world2

let run2 createWorld sdlConfig =
    runSdl
        (fun sdlDeps ->
            createWorld sdlDeps)
        (fun refEvent world ->
            let event = refEvent.Value
            match event.``type`` with
            | SDL.SDL_EventType.SDL_QUIT ->
                (false, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                if event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (Vector2 (single event.button.x, single event.button.y), MouseLeft)
                    let world2 = { world with MouseState = { world.MouseState with MouseLeftDown = true }}
                    let world3 = publish DownMouseLeftAddress { Handled = false; Data = messageData } world2
                    (true, world3)
                else (true, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                let mouseState = world.MouseState
                if mouseState.MouseLeftDown && event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (Vector2 (single event.button.x, single event.button.y), MouseLeft)
                    let world2 = { world with MouseState = { world.MouseState with MouseLeftDown = false }}
                    let world3 = publish UpMouseLeftAddress { Handled = false; Data = messageData } world2
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

let run sdlConfig =
    run2
        (fun sdlDeps ->
            { Game = Game (getNuId ())
              Subscriptions = Dictionary ()
              MouseState = { MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
              AudioPlayer = makeAudioPlayer ()
              Renderer = makeRenderer sdlDeps.RenderContext
              Integrator = makeIntegrator Gravity
              AudioMessages = []
              RenderMessages = []
              PhysicsMessages = []
              Components = [] })
        sdlConfig