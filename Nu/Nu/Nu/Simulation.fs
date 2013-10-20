module Nu.Simulation
open System
open FSharpx
open FSharpx.Lens.Operators
open SDL2
open OpenTK
open TiledSharp
open Nu.Core
open Nu.Constants
open Nu.Math
open Nu.Physics
open Nu.Rendering
open Nu.AssetMetadataMap
open Nu.Input
open Nu.Audio
open Nu.Sdl
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game

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
    | CollisionData of Vector2 * single * Address
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
      AssetMetadataMap : AssetMetadataMap
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
        
        static member entity (address : Address) =
            World.game >>| Game.entity address
        
        static member optEntity (address : Address) =
            World.game >>| Game.optEntity address
        
        static member entityGui (address : Address) =
            World.game >>| Game.entityGui address
        
        static member optEntityGui (address : Address) =
            World.game >>| Game.optEntityGui address
        
        static member entityGuiButton (address : Address) =
            World.game >>| Game.entityGuiButton address
        
        static member optEntityGuiButton (address : Address) =
            World.game >>| Game.optEntityGuiButton address
        
        static member entityGuiLabel (address : Address) =
            World.game >>| Game.entityGuiLabel address
        
        static member optEntityGuiLabel (address : Address) =
            World.game >>| Game.optEntityGuiLabel address
        
        static member entityGuiTextBox (address : Address) =
            World.game >>| Game.entityGuiTextBox address
        
        static member optEntityGuiTextBox (address : Address) =
            World.game >>| Game.optEntityGuiTextBox address
        
        static member entityGuiToggle (address : Address) =
            World.game >>| Game.entityGuiToggle address
        
        static member optEntityGuiToggle (address : Address) =
            World.game >>| Game.optEntityGuiToggle address
        
        static member entityActor (address : Address) =
            World.game >>| Game.entityActor address
        
        static member optEntityActor (address : Address) =
            World.game >>| Game.optEntityActor address
        
        static member entityActorBlock (address : Address) =
            World.game >>| Game.entityActorBlock address
        
        static member optEntityActorBlock (address : Address) =
            World.game >>| Game.optEntityActorBlock address
        
        static member entityActorTileMap (address : Address) =
            World.game >>| Game.entityActorTileMap address
        
        static member optEntityActorTileMap (address : Address) =
            World.game >>| Game.optEntityActorTileMap address
        
        static member group (address : Address) =
            World.game >>| Game.group address
        
        static member optGroup (address : Address) =
            World.game >>| Game.optGroup address
        
        static member screen (address : Address) =
            World.game >>| Game.screen address
        
        static member optScreen (address : Address) =
            World.game >>| Game.optScreen address
        
        static member optActiveScreenAddress =
            World.game >>| Game.optActiveScreenAddress

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

let unregisterEntityGuiButton address world : World =
    world |>
        unsubscribe DownMouseLeftAddress address |>
        unsubscribe UpMouseLeftAddress address

let handleButtonEventDownMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let (entity, gui, button) = get world (World.entityGuiButton subscriber)
        if entity.IsEnabled && entity.IsVisible then
            if isInBox3 mousePosition gui.Position gui.Size then
                let button2 = { button with IsDown = true }
                let world2 = set (entity, gui, button2) world (World.entityGuiButton subscriber)
                publish (Lun.make "down" :: subscriber) { Handled = false; Data = NoData } world2
            else world
        else world
    | _ -> failwith ("Expected MouseClickData from address '" + str address + "'.")
    
let handleButtonEventUpMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let (entity, gui, button) = get world (World.entityGuiButton subscriber)
        if entity.IsEnabled && entity.IsVisible then
            let world_ =
                let button_ = { button with IsDown = false }
                let world_ = set (entity, gui, button_) world (World.entityGuiButton subscriber)
                publish (Lun.make "up" :: subscriber) { Handled = false; Data = NoData } world_
            if isInBox3 mousePosition gui.Position gui.Size then
                let world_ = publish (Lun.make "click" :: subscriber) { Handled = false; Data = NoData } world_
                let sound = PlaySound { Volume = 1.0f; Sound = button.ClickSound }
                { world_ with AudioMessages = sound :: world_.AudioMessages }
            else world_
        else world
    | _ -> failwith ("Expected MouseClickData from address '" + str address + "'.")

let registerEntityGuiButton address world : World =
    let optOldEntityGuiButton = get world (World.optEntityGuiButton address)
    let world_ = if optOldEntityGuiButton.IsSome then unregisterEntityGuiButton address world else world
    let world_ = subscribe DownMouseLeftAddress address handleButtonEventDownMouseLeft world_
    subscribe UpMouseLeftAddress address handleButtonEventUpMouseLeft world_

let addEntityGuiButton entityGuiButton address world : World =
    let world2 = registerEntityGuiButton address world
    set entityGuiButton world2 (World.entityGuiButton address)

let removeEntityGuiButton address world : World =
    let world2 = set None world (World.optEntityGuiButton address)
    unregisterEntityGuiButton address world2

let addEntityGuiLabel entityGuiLabel address world : World =
    set entityGuiLabel world (World.entityGuiLabel address)

let removeEntityGuiLabel address world : World =
    set None world (World.optEntityGuiLabel address)

let addEntityGuiTextBox entityGuiTextBox address world : World =
    set entityGuiTextBox world (World.entityGuiTextBox address)

let removeEntityGuiTextBox address world : World =
    set None world (World.optEntityGuiTextBox address)

let unregisterEntityGuiToggle address world : World =
    world |>
        unsubscribe DownMouseLeftAddress address |>
        unsubscribe UpMouseLeftAddress address

let handleToggleEventDownMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let (entity, gui, toggle) = get world (World.entityGuiToggle subscriber)
        if entity.IsEnabled && entity.IsVisible then
            if isInBox3 mousePosition gui.Position gui.Size then
                let toggle2 = { toggle with IsPressed = true }
                set (entity, gui, toggle2) world (World.entityGuiToggle subscriber)
            else world
        else world
    | _ -> failwith ("Expected MouseClickData from address '" + str address + "'.")
    
let handleToggleEventUpMouseLeft address subscriber message world =
    match message.Data with
    | MouseButtonData (mousePosition, _) ->
        let (entity, gui, toggle) = get world (World.entityGuiToggle subscriber)
        if entity.IsEnabled && entity.IsVisible then
            let toggle_ = { toggle with IsPressed = false }
            if isInBox3 mousePosition gui.Position gui.Size then
                let toggle_ = { toggle_ with IsOn = not toggle_.IsOn }
                let world_ = set (entity, gui, toggle_) world (World.entityGuiToggle subscriber)
                let messageType = if toggle.IsOn then "on" else "off"
                let world_ = publish (Lun.make messageType :: subscriber) { Handled = false; Data = NoData } world_
                let sound = PlaySound { Volume = 1.0f; Sound = toggle.ToggleSound }
                { world_ with AudioMessages = sound :: world_.AudioMessages }
            else set (entity, gui, toggle_) world (World.entityGuiToggle subscriber)
        else world
    | _ -> failwith ("Expected MouseClickData from address '" + str address + "'.")

let registerEntityGuiToggle address world : World =
    let optOldEntityGuiToggle = get world (World.optEntityGuiToggle address)
    let world_ = if optOldEntityGuiToggle.IsSome then unregisterEntityGuiToggle address world else world
    let world_ = subscribe DownMouseLeftAddress address handleToggleEventDownMouseLeft world_
    subscribe UpMouseLeftAddress address handleToggleEventUpMouseLeft world_

let addEntityGuiToggle entityGuiToggle address world : World =
    let world2 = registerEntityGuiToggle address world
    set entityGuiToggle world2 (World.entityGuiToggle address)

let unregisterEntityActorBlock (entity, actor, block) address world : World =
    let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block.PhysicsId }
    { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

let registerEntityActorBlock (entity, actor, block) address world : World =
    let optOldEntityActorBlock = get world (World.optEntityActorBlock address)
    let world2 =
        match optOldEntityActorBlock with
        | None -> world
        | Some oldEntityActorBlock -> unregisterEntityActorBlock oldEntityActorBlock address world
    let bodyCreateMessage =
        BodyCreateMessage
            { EntityAddress = address
              PhysicsId = block.PhysicsId
              Shape = BoxShape { Center = Vector2.Zero; Extent = actor.Size * 0.5f }
              Position = actor.Position + actor.Size * 0.5f
              Rotation = actor.Rotation
              Density = block.Density
              BodyType = block.BodyType }
    { world2 with PhysicsMessages = bodyCreateMessage :: world2.PhysicsMessages }

let addEntityActorBlock entityActorBlock address world : World =
    let world2 = registerEntityActorBlock entityActorBlock address world
    set entityActorBlock world2 (World.entityActorBlock address)

let removeEntityActorBlock entityActorBlock address world : World =
    let world2 = set None world (World.optEntityActorBlock address)
    unregisterEntityActorBlock entityActorBlock address world2

let unregisterEntityActorTileMap (entity, actor, tileMap) address world : World =
    let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = tileMap.PhysicsIds.[0] }
    { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

let registerEntityActorTileMap (entity, actor, tileMap) address world : World =
    let optOldEntityActorTileMap = get world (World.optEntityActorTileMap address)
    let world2 =
        match optOldEntityActorTileMap with
        | None -> world
        | Some oldEntityActorTileMap -> unregisterEntityActorTileMap oldEntityActorTileMap address world
    (*let bodyCreateMessage =
        BodyCreateMessage
            { EntityAddress = address
              PhysicsId = tileMap.PhysicsIds.[0]
              Shape = BoxShape { Center = Vector2.Zero; Extent = actor.Size * 0.5f }
              Position = actor.Position + actor.Size * 0.5f
              Rotation = actor.Rotation
              Density = tileMap.Density
              BodyType = BodyType.Static }
    { world2 with PhysicsMessages = bodyCreateMessage :: world2.PhysicsMessages }*)
    world2

let addEntityActorTileMap entityActorTileMap address world : World =
    let world2 = registerEntityActorTileMap entityActorTileMap address world
    set entityActorTileMap world2 (World.entityActorTileMap address)

let removeEntityActorTileMap entityActorTileMap address world : World =
    let world2 = set None world (World.optEntityActorTileMap address)
    unregisterEntityActorTileMap entityActorTileMap address world2

let addGroup group address world : World =
    set group world (World.group address)

let removeGroup address world : World =
    set None world (World.optGroup address)

let addScreen screen address world : World =
    set screen world (World.screen address)

let removeScreen address world : World =
    set None world (World.optScreen address)

let getComponentAudioDescriptors world : AudioDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetAudioDescriptors world :: descs) [] world.Components // TODO: get audio descriptors
    List.collect (fun descs -> descs) descriptorLists

let getAudioDescriptors world : AudioDescriptor rQueue =
    let componentDescriptors = getComponentAudioDescriptors world
    let worldDescriptors = [] // TODO: get audio descriptors when there are some
    worldDescriptors @ componentDescriptors // NOTE: pretty inefficient

/// Play the world's audio.
let play world : World =
    let audioMessages = world.AudioMessages
    let audioDescriptors = getAudioDescriptors world
    let audioPlayer = world.AudioPlayer
    let world_ = { world with AudioMessages = [] }
    let world_ = { world_ with AudioPlayer = Audio.play audioMessages audioDescriptors audioPlayer }
    world_

let getComponentRenderDescriptors world : RenderDescriptor rQueue =
    let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetRenderDescriptors world :: descs) [] world.Components // TODO: get render descriptors
    List.collect (fun descs -> descs) descriptorLists

let getEntityRenderDescriptors entity =
    match entity.EntitySemantic with
    | Gui gui ->
        match gui.GuiSemantic with
        | Button button -> [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = if button.IsDown then button.DownSprite else button.UpSprite }; Depth = gui.Depth })]
        | Label label -> [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = label.LabelSprite }; Depth = gui.Depth })]
        | TextBox textBox ->
            [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = textBox.BoxSprite }; Depth = gui.Depth })
             LayerableDescriptor (LayeredTextDescriptor { Descriptor = { Text = textBox.Text; Position = gui.Position + textBox.TextOffset; Size = gui.Size - textBox.TextOffset; Font = textBox.TextFont; Color = textBox.TextColor }; Depth = gui.Depth })]
        | Toggle toggle -> [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = if toggle.IsOn || toggle.IsPressed then toggle.OnSprite else toggle.OffSprite }; Depth = gui.Depth })]
    | Actor actor ->
        match actor.ActorSemantic with
        | Block block -> [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position; Size = actor.Size; Rotation = actor.Rotation; Sprite = block.Sprite }; Depth = actor.Depth })]
        | Avatar _ -> [] // TODO: implement Avatar
        | TileMap tileMap ->
            let map = tileMap.TmxMap
            let mapWidth = map.Width
            let tileWidth = map.TileWidth
            let tileHeight = map.TileHeight
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
            let tileSetSprite = tileMap.TileMapMetadata.[0] // MAGIC_VALUE: for same reason as above
            let layers = List.ofSeq map.Layers
            let tileLayers =
                List.map
                    (fun (layer : TmxLayer) ->
                        let tiles = List.ofSeq layer.Tiles
                        List.mapi
                            (fun n tile ->
                                let (i, j) = (n % mapWidth, n / mapWidth)
                                let position = Vector2 (actor.Position.X + single (tileWidth * i), actor.Position.Y + single (tileHeight * j))
                                let size = Vector2 (single tileWidth, single tileHeight)
                                let gid = layer.Tiles.[n].Gid - tileSet.FirstGid
                                let gidPosition = gid * tileWidth
                                let optTileSetWidth = tileSet.Image.Width
                                let tileSetWidth = optTileSetWidth.Value
                                let tileSetPosition = Vector2 (single <| gidPosition % tileSetWidth, single <| gidPosition / tileSetWidth * tileHeight)
                                LayerableDescriptor (LayeredTileDescriptor { Descriptor = { TileSetPosition = tileSetPosition; Position = position; Size = size; Rotation = actor.Rotation; TileSetSprite = tileSetSprite }; Depth = actor.Depth }))
                            tiles)
                    layers
            List.concat tileLayers

let getGroupRenderDescriptors group =
    let entities = Map.toValueSeq group.Entities
    Seq.map getEntityRenderDescriptors entities

let getWorldRenderDescriptors world =
    match get world World.optActiveScreenAddress with
    | None -> []
    | Some activeScreenAddress ->
        let activeScreen = get world (World.screen activeScreenAddress)
        let groups = Map.toValueSeq activeScreen.Groups
        let descriptorSeqLists = Seq.map getGroupRenderDescriptors groups
        let descriptorSeq = Seq.concat descriptorSeqLists
        List.concat descriptorSeq

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
        let (entity, actor) = get world (World.entityActor bodyTransformMessage.EntityAddress)
        let actor2 = {{ actor with Position = bodyTransformMessage.Position - actor.Size * 0.5f }
                              with Rotation = bodyTransformMessage.Rotation }
        set (entity, actor2) world (World.entityActor bodyTransformMessage.EntityAddress)
    | BodyCollisionMessage bodyCollisionMessage ->
        let collisionAddress = Lun.make "collision" :: bodyCollisionMessage.EntityAddress
        let collisionData = CollisionData (bodyCollisionMessage.Normal, bodyCollisionMessage.Speed, bodyCollisionMessage.EntityAddress2)
        let collisionMessage = { Handled = false; Data = collisionData }
        publish collisionAddress collisionMessage world

/// Handle physics integration messages.
let handleIntegrationMessages integrationMessages world : World =
    List.fold handleIntegrationMessage world integrationMessages

/// Integrate the world.
let integrate world : World =
    let integrationMessages = Physics.integrate world.PhysicsMessages world.Integrator
    let world2 = { world with PhysicsMessages = [] }
    handleIntegrationMessages integrationMessages world2

let run2 tryCreateWorld sdlConfig =
    runSdl
        (fun sdlDeps -> tryCreateWorld sdlDeps)
        (fun refEvent world ->
            let event = refEvent.Value
            match event.``type`` with
            | SDL.SDL_EventType.SDL_QUIT -> (false, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                if event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (Vector2 (single event.button.x, single event.button.y), MouseLeft)
                    let world2 = set { world.MouseState with MouseLeftDown = true } world World.mouseState
                    let world3 = publish DownMouseLeftAddress { Handled = false; Data = messageData } world2
                    (true, world3)
                else (true, world)
            | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                let mouseState = world.MouseState
                if mouseState.MouseLeftDown && event.button.button = byte SDL.SDL_BUTTON_LEFT then
                    let messageData = MouseButtonData (Vector2 (single event.button.x, single event.button.y), MouseLeft)
                    let world2 = set { world.MouseState with MouseLeftDown = false } world World.mouseState
                    let world3 = publish UpMouseLeftAddress { Handled = false; Data = messageData } world2
                    (true, world3)
                else (true, world)
            | _ -> (true, world))
        (fun world -> let world2 = integrate world in (true, world2))
        (fun world -> render world)
        (fun world -> play world)
        (fun world -> { world with Renderer = handleRenderExit world.Renderer })
        sdlConfig

let run sdlConfig =
    run2
        (fun sdlDeps ->
            let game = {
                Id = getNuId ()
                IsEnabled = true
                Screens = Map.empty
                OptActiveScreenAddress = None }
            let world = {
                Game = game
                Subscriptions = Map.empty
                MouseState = { MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
                AudioPlayer = makeAudioPlayer ()
                Renderer = makeRenderer sdlDeps.RenderContext
                Integrator = makeIntegrator Gravity
                AssetMetadataMap = Map.empty
                AudioMessages = []
                RenderMessages = []
                PhysicsMessages = []
                Components = [] }
            Right world)
        sdlConfig