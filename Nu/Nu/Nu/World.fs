namespace Nu
open System
open System.IO
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open FSharpx
open FSharpx.Lens.Operators
open SDL2
open OpenTK
open TiledSharp
open Xtension
open Nu
open Nu.Core
open Nu.Constants
open Nu.Math
open Nu.Physics
open Nu.Rendering
open Nu.Metadata
open Nu.Audio
open Nu.Sdl
open Nu.DomainModel
open Nu.CameraModule
open Nu.EntityModule
open Nu.GroupModule
open Nu.ScreenModule
open Nu.GameModule
module WorldModule =

    let TickAddress = addr "Tick"
    let MouseDragAddress = addr "Mouse/Drag"
    let MouseMoveAddress = addr "Mouse/Move"
    let MouseLeftAddress = addr "Mouse/Left"
    let MouseCenterAddress = addr "Mouse/Center"
    let MouseRightAddress = addr "Mouse/Right"
    let DownMouseLeftAddress = straddr "Down" MouseLeftAddress
    let DownMouseCenterAddress = straddr "Down" MouseCenterAddress
    let DownMousRightAddress = straddr "Down" MouseRightAddress
    let UpMouseLeftAddress = straddr "Up" MouseLeftAddress
    let UpMouseCenterAddress = straddr "Up" MouseCenterAddress
    let UpMouseRightAddress = straddr "Up" MouseRightAddress
    let GamePublishingPriority = Single.MaxValue
    let ScreenPublishingPriority = GamePublishingPriority * 0.5f
    let GroupPublishingPriority = ScreenPublishingPriority * 0.5f
    let FinishedIncomingAddressPart = addr "Finished/Incoming"
    let FinishedOutgoingAddressPart = addr "Finished/Outgoing"
    let FieldFeelerName = Lun.make "Feeler"
    let FieldAvatarName = Lun.make "Avatar"

    /// Initialize Nu's various type converters.
    /// Must be called for reflection to work in Nu.
    let initTypeConverters () =
        initMathConverters ()
        initAudioConverters ()
        initRenderConverters ()
    
    /// Mark a message as handled.
    let handle message =
        { Handled = true; Data = message.Data }

    let isAddressSelected address world =
        let optScreenAddress = get world worldOptSelectedScreenAddressLens
        match (address, optScreenAddress) with
        | ([], _) -> true
        | (_, None) -> false
        | (_, Some []) -> false
        | (addressHead :: addressTail, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

    let sortFstAsc (priority, _) (priority2, _) =
        if priority = priority2 then 0
        elif priority > priority2 then -1
        else 1

    let getPublishingPriority simulant world =
        match simulant with
        | Game _ -> GamePublishingPriority
        | Screen _ -> ScreenPublishingPriority
        | Group _ -> GroupPublishingPriority
        | Entity entity -> getPickingPriority world entity

    let getSimulant address world =
        match address with
        | [] -> Game <| world.Game
        | [_] as screenAddress -> Screen <| get world (worldScreenLens screenAddress)
        | [_; _] as groupAddress -> Group <| get world (worldGroupLens groupAddress)
        | [_; _; _] as entityAddress -> Entity <| get world (worldEntityLens entityAddress)
        | _ -> failwith <| "Invalid simulant address '" + str address + "'."

    let getSimulants subscriptions world =
        List.map (fun (address, _) -> getSimulant address world) subscriptions

    let pickingSort entities world =
        let priorities = List.map (getPickingPriority world) entities
        let prioritiesAndEntities = List.zip priorities entities
        let prioritiesAndEntitiesSorted = List.sortWith sortFstAsc prioritiesAndEntities
        List.map snd prioritiesAndEntitiesSorted

    let tryPick (position : Vector2) entities world =
        let entitiesSorted = pickingSort entities world
        List.tryFind
            (fun entity ->
                let transform = getEntityTransform (Some world.Camera) world entity
                position.X >= transform.Position.X &&
                    position.X < transform.Position.X + transform.Size.X &&
                    position.Y >= transform.Position.Y &&
                    position.Y < transform.Position.Y + transform.Size.Y)
            entitiesSorted

    let subscriptionSort subscriptions world =
        let simulants = getSimulants subscriptions world
        let priorities = List.map (fun simulant -> getPublishingPriority simulant world) simulants
        let prioritiesAndSubscriptions = List.zip priorities subscriptions
        let prioritiesAndSubscriptionsSorted = List.sortWith sortFstAsc prioritiesAndSubscriptions
        List.map snd prioritiesAndSubscriptionsSorted

    /// Publish a message to the given address.
    let publish address message world : bool * World =
        let optSubList = Map.tryFind address world.Subscriptions
        match optSubList with
        | None -> (true, world)
        | Some subList ->
            let subListSorted = subscriptionSort subList world
            let (_, keepRunning, world'') =
                List.foldWhile
                    (fun (message', keepRunning', world'3) (subscriber, (Subscription subscription)) ->
                        if message'.Handled || not keepRunning' then None
                        elif isAddressSelected subscriber world'3 then Some (subscription address subscriber message' world'3)
                        else Some (message', keepRunning', world'3))
                    (message, true, world)
                    subListSorted
            (keepRunning, world'')

    /// Subscribe to messages at the given address.
    let subscribe address subscriber subscription world =
        let sub = Subscription subscription
        let subs = world.Subscriptions
        let optSubList = Map.tryFind address subs
        { world with
            Subscriptions =
                match optSubList with
                | None -> Map.add address [(subscriber, sub)] subs
                | Some subList -> Map.add address ((subscriber, sub) :: subList) subs }

    /// Unsubscribe to messages at the given address.
    let unsubscribe address subscriber world =
        let subs = world.Subscriptions
        let optSubList = Map.tryFind address subs
        match optSubList with
        | None -> world
        | Some subList ->
            let subList' = List.remove (fun (address, _) -> address = subscriber) subList
            let subscriptions' = Map.add address subList' subs
            { world with Subscriptions = subscriptions' }

    /// Execute a procedure within the context of a given subscription at the given address.
    let withSubscription address subscription subscriber procedure world =
        let world' = subscribe address subscriber subscription world
        let world'' = procedure world'
        unsubscribe address subscriber world''
    
    let handleEventAsSwallow _ _ message world =
        (handle message, true, world)

    let handleEventAsExit _ _ message world =
        (handle message, false, world)

    // TODO: consider turning this into a lens, and removing the screenStateLens
    let getScreenState address world =
        let screen = get world <| worldScreenLens address
        get screen screenStateLens

    // TODO: consider turning this into a lens, and removing the screenStateLens
    let setScreenState address state world =
        let screen = set state (get world <| worldScreenLens address) screenStateLens
        let world' = set screen world <| worldScreenLens address
        match state with
        | IdlingState ->
            world' |>
                unsubscribe DownMouseLeftAddress address |>
                unsubscribe UpMouseLeftAddress address
        | IncomingState | OutgoingState ->
            world' |>
                subscribe DownMouseLeftAddress address handleEventAsSwallow |>
                subscribe UpMouseLeftAddress address handleEventAsSwallow

    let transitionScreen address world =
        let world' = setScreenState address IncomingState world
        set (Some address) world' worldOptSelectedScreenAddressLens

    let transitionScreenHandler address _ _ message world =
        let world' = transitionScreen address world
        (handle message, true, world')

    let handleFinishedScreenOutgoing screenAddress destScreenAddress address subscriber message world =
        let world' = unsubscribe address subscriber world
        let world'' = transitionScreen destScreenAddress world'
        (handle message, true, world'')

    let handleEventAsScreenTransition screenAddress destScreenAddress address subscriber message world =
        let world' = subscribe (FinishedOutgoingAddressPart @ screenAddress) [] (handleFinishedScreenOutgoing screenAddress destScreenAddress) world
        let optSelectedScreenAddress = get world' worldOptSelectedScreenAddressLens
        match optSelectedScreenAddress with
        | None ->
            trace <| "Program Error: Could not handle click as screen transition due to no selected screen."
            (handle message, true, world)
        | Some selectedScreenAddress ->
            let world'' = setScreenState selectedScreenAddress OutgoingState world'
            (handle message, true, world'')

    let updateTransition1 transition =
        if transition.Ticks = transition.Lifetime then ({ transition with Ticks = 0 }, true)
        else ({ transition with Ticks = transition.Ticks + 1 }, false)

    let updateTransition update world : bool * World =
        let (keepRunning, world') =
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None -> (true, world)
            | Some selectedScreenAddress ->
                let screenState = getScreenState selectedScreenAddress world
                match screenState with
                | IncomingState ->
                    // TODO: remove duplication with below
                    let selectedScreen = get world <| worldScreenLens selectedScreenAddress
                    let incoming = get selectedScreen incomingLens
                    let (incoming', finished) = updateTransition1 incoming
                    let selectedScreen' = set incoming' selectedScreen incomingLens
                    let world'' = set selectedScreen' world <| worldScreenLens selectedScreenAddress
                    let world'3 = setScreenState selectedScreenAddress (if finished then IdlingState else IncomingState) world''
                    if finished then
                        publish
                            (FinishedIncomingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world'3
                    else (true, world'3)
                | OutgoingState ->
                    let selectedScreen = get world <| worldScreenLens selectedScreenAddress
                    let outgoing = get selectedScreen outgoingLens
                    let (outgoing', finished) = updateTransition1 outgoing
                    let selectedScreen' = set outgoing' selectedScreen outgoingLens
                    let world'' = set selectedScreen' world <| worldScreenLens selectedScreenAddress
                    let world'3 = setScreenState selectedScreenAddress (if finished then IdlingState else OutgoingState) world''
                    if finished then
                        publish
                            (FinishedOutgoingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world'3
                    else (true, world'3)
                | IdlingState -> (true, world)
        if keepRunning then update world'
        else (keepRunning, world')

    let handleButtonEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world <| worldEntityLens subscriber
            if button.Enabled && button.Visible then
                if isInBox3 mousePosition button.Position button.Size then
                    let button' = button?IsDown <- true
                    let world' = set button' world <| worldEntityLens subscriber
                    let (keepRunning, world'') = publish (straddr "Down" subscriber) { Handled = false; Data = NoData } world'
                    (handle message, keepRunning, world'')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleButtonEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world <| worldEntityLens subscriber
            if button.Enabled && button.Visible then
                let (keepRunning, world') =
                    let button' = button?IsDown <- false
                    let world'' = set button' world <| worldEntityLens subscriber
                    publish (straddr "Up" subscriber) { Handled = false; Data = NoData } world''
                if keepRunning && isInBox3 mousePosition button.Position button.Size && button?IsDown () then
                    let (keepRunning', world'') = publish (straddr "Click" subscriber) { Handled = false; Data = NoData } world'
                    let sound = PlaySound { Volume = 1.0f; Sound = button?ClickSound () }
                    let world'3 = { world'' with AudioMessages = sound :: world''.AudioMessages }
                    (handle message, keepRunning', world'3)
                else (message, keepRunning, world')
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let handleToggleEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let toggle = get world <| worldEntityLens subscriber
            if toggle.Enabled && toggle.Visible then
                if isInBox3 mousePosition toggle.Position toggle.Size then
                    let toggle' = toggle?IsPressed <- true
                    let world' = set toggle' world <| worldEntityLens subscriber
                    (handle message, true, world')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleToggleEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let toggle = get world <| worldEntityLens subscriber
            if toggle.Enabled && toggle.Visible && toggle?IsPressed () then
                let toggle' = toggle?IsPressed <- false
                if isInBox3 mousePosition toggle'.Position toggle'.Size then
                    let toggle'' = toggle'?IsOn <- not <| toggle'?IsOn ()
                    let world' = set toggle'' world <| worldEntityLens subscriber
                    let messageType = if toggle''?IsOn () then "On" else "Off"
                    let (keepRunning, world'') = publish (straddr messageType subscriber) { Handled = false; Data = NoData } world'
                    let sound = PlaySound { Volume = 1.0f; Sound = toggle''?ToggleSound () }
                    let world'3 = { world'' with AudioMessages = sound :: world''.AudioMessages }
                    (handle message, keepRunning, world'3)
                else
                    let world' = set toggle' world <| worldEntityLens subscriber
                    (message, true, world')
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let handleFeelerEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) as mouseButtonData ->
            let feeler = get world <| worldEntityLens subscriber
            if feeler.Enabled && feeler.Visible then
                if isInBox3 mousePosition feeler.Position feeler.Size then
                    let feeler' = feeler?IsTouched <- true
                    let world' = set feeler' world <| worldEntityLens subscriber
                    let (keepRunning, world'') = publish (straddr "Touch" subscriber) { Handled = false; Data = mouseButtonData } world'
                    (handle message, keepRunning, world'')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleFeelerEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData _ ->
            let feeler = get world <| worldEntityLens subscriber
            if feeler.Enabled && feeler.Visible then
                let feeler' = feeler?IsTouched <- false
                let world' = set feeler' world <| worldEntityLens subscriber
                let (keepRunning, world'') = publish (straddr "Release" subscriber) { Handled = false; Data = NoData } world'
                (handle message, keepRunning, world'')
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let registerBlockPhysics address (block : Entity) world =
        let block' = block?PhysicsId <- getPhysicsId block.Id
        let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  PhysicsId = block'?PhysicsId ()
                  Shape =
                    BoxShape
                        { Extent = block'.Size * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Restitution = 0.0f
                              FixedRotation = false
                              LinearDamping = 5.0f
                              AngularDamping = 5.0f }}
                  Position = block'.Position + block'.Size * 0.5f
                  Rotation = block'.Rotation
                  Density = block'?Density ()
                  BodyType = block'?BodyType () }
        let world' = { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }
        (block', world')

    let unregisterBlockPhysics address (block : Entity) world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block?PhysicsId () }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let registerAvatarPhysics address (avatar : Entity) world =
        let avatar' = avatar?PhysicsId <- getPhysicsId avatar.Id
        let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  PhysicsId = avatar'?PhysicsId ()
                  Shape =
                    CircleShape
                        { Radius = avatar'.Size.X * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Restitution = 0.0f
                              FixedRotation = true
                              LinearDamping = 10.0f
                              AngularDamping = 0.0f }}
                  Position = avatar'.Position + avatar'.Size * 0.5f
                  Rotation = avatar'.Rotation
                  Density = avatar'?Density ()
                  BodyType = BodyType.Dynamic }
        let world' = { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }
        (avatar', world')

    let unregisterAvatarPhysics address (avatar : Entity) world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = avatar?PhysicsId () }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let registerTilePhysics tileMap tmd tld address n (world, physicsIds) tile =
        let td = makeTileData tileMap tmd tld n
        match td.OptTileSetTile with
        | None -> (world, physicsIds)
        | Some tileSetTile when not <| tileSetTile.Properties.ContainsKey "c" -> (world, physicsIds)
        | Some tileSetTile ->
            let physicsId = getPhysicsId tileMap.Id
            let boxShapeProperties =
                { Center = Vector2.Zero
                  Restitution = 0.0f
                  FixedRotation = true
                  LinearDamping = 0.0f
                  AngularDamping = 0.0f }
            let bodyCreateMessage =
                BodyCreateMessage
                    { EntityAddress = address
                      PhysicsId = physicsId
                      Shape = BoxShape { Extent = Vector2 (single <| fst tmd.TileSize, single <| snd tmd.TileSize) * 0.5f; Properties = boxShapeProperties }
                      Position = Vector2 (single <| fst td.TilePosition + fst tmd.TileSize / 2, single <| snd td.TilePosition + snd tmd.TileSize / 2)
                      Rotation = tileMap.Rotation
                      Density = tileMap?Density ()
                      BodyType = BodyType.Static }
            let world' = { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }
            (world', physicsId :: physicsIds)

    let registerTileMapPhysics address tileMap world =
        let collisionLayer = 0 // MAGIC_VALUE: assumption
        let tmd = makeTileMapData tileMap
        let tld = makeTileLayerData tileMap tmd collisionLayer
        let (world', physicsIds) = Seq.foldi (registerTilePhysics tileMap tmd tld address) (world, []) tld.Tiles
        let tileMap' = tileMap?PhysicsIds <- physicsIds
        (tileMap', world')

    let unregisterTilePhysics world physicsId =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = physicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let unregisterTileMapPhysics address tileMap world =
        List.fold unregisterTilePhysics world <| tileMap?PhysicsIds ()

    // TODO: try to use normal F# call conventions rather than param tuples
    type EntityDispatcher () =
        class

            abstract member Init : Entity -> Entity
            default this.Init entity = entity

            abstract member Register : Address * Entity * World -> Entity * World
            default this.Register (address, entity, world) = (entity, world)

            abstract member Unregister : Address * Entity * World -> World
            default this.Unregister (address, entity, world) = world

            abstract member PropagatePhysics : Address * Entity * World -> World
            default this.PropagatePhysics (address, entity, world) = world

            abstract member ReregisterPhysicsHack : Address * Entity * World -> World
            default this.ReregisterPhysicsHack (groupAddress, entity, world) = world

            abstract member HandleBodyTransformMessage : BodyTransformMessage * Address * Entity * World -> World
            default this.HandleBodyTransformMessage (message, address, entity, world) = world

            abstract member GetRenderDescriptors : Vector2 * Entity * IXDispatcherContainer -> RenderDescriptor list
            default this.GetRenderDescriptors (view, entity, dispatcherContainer) = []

            abstract member GetQuickSize : Entity * World -> Vector2
            default this.GetQuickSize (entity, world) = Vector2 DefaultEntitySize

            end

    type ButtonDispatcher () =
        inherit EntityDispatcher ()
            
            override this.Init button =
                let button' = base.Init button
                let button'' = { button with IsTransformRelative = false }
                (((button''
                    ?IsDown <- false)
                    ?UpSprite <- { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" })
                    ?DownSprite <- { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" })
                    ?ClickSound <- { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

            override this.Register (address, button, world) =
                let world' =
                    world |>
                    subscribe DownMouseLeftAddress address handleButtonEventDownMouseLeft |>
                    subscribe UpMouseLeftAddress address handleButtonEventUpMouseLeft
                (button, world')

            override this.Unregister (address, button, world) =
                world |>
                    unsubscribe DownMouseLeftAddress address |>
                    unsubscribe UpMouseLeftAddress address

            override this.GetRenderDescriptors (view, button, dispatcherContainer) =
                if not button.Visible then []
                else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = button.Position; Size = button.Size; Rotation = 0.0f; Sprite = (if button?IsDown () then button?DownSprite () else button?UpSprite ()); Color = Vector4.One }; Depth = button.Depth })]

            override this.GetQuickSize (button, world) =
                let sprite = button?UpSprite ()
                getTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap

    type LabelDispatcher () =
        inherit EntityDispatcher ()
            
            override this.Init label =
                let label' = base.Init label
                let label'' = { label with IsTransformRelative = false }
                label''?LabelSprite <- { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

            override this.GetRenderDescriptors (view, label, dispatcherContainer) =
                if not label.Visible then []
                else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = label.Position; Size = label.Size; Rotation = 0.0f; Sprite = label?LabelSprite (); Color = Vector4.One }; Depth = label.Depth })]

            override this.GetQuickSize (label, world) =
                let sprite = label?LabelSprite ()
                getTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap

    type TextBoxDispatcher () =
        inherit EntityDispatcher ()
            
            override this.Init textBox =
                let textBox' = base.Init textBox
                let textBox'' = { textBox with IsTransformRelative = false }
                ((((textBox''
                        ?BoxSprite <- { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" })
                        ?Text <- String.Empty)
                        ?TextFont <- { FontAssetName = Lun.make "Font"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" })
                        ?TextOffset <- Vector2.Zero)
                        ?TextColor <- Vector4.One

            override this.GetRenderDescriptors (view, textBox, dispatcherContainer) =
                if not textBox.Visible then []
                else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = textBox.Position; Size = textBox.Size; Rotation = 0.0f; Sprite = textBox?BoxSprite (); Color = Vector4.One }; Depth = textBox.Depth })
                      LayerableDescriptor (LayeredTextDescriptor { Descriptor = { Text = textBox?Text (); Position = textBox.Position + textBox?TextOffset (); Size = textBox.Size - textBox?TextOffset (); Font = textBox?TextFont (); Color = textBox?TextColor () }; Depth = textBox.Depth })]

            override this.GetQuickSize (textBox, world) =
                let sprite = textBox?BoxSprite ()
                getTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap

    type ToggleDispatcher () =
        inherit EntityDispatcher ()

            override this.Init toggle =
                let toggle' = base.Init toggle
                let toggle'' = { toggle with IsTransformRelative = false }
                ((((toggle''
                        ?IsOn <- false)
                        ?IsPressed <- false)
                        ?OffSprite <- { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" })
                        ?OnSprite <- { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" })
                        ?ToggleSound <- { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

            override this.Register (address, label, world) =
                let world' =
                    world |>
                    subscribe DownMouseLeftAddress address handleToggleEventDownMouseLeft |>
                    subscribe UpMouseLeftAddress address handleToggleEventUpMouseLeft
                (label, world')

            override this.Unregister (address, label, world) =
                world |>
                    unsubscribe DownMouseLeftAddress address |>
                    unsubscribe UpMouseLeftAddress address

            override this.GetRenderDescriptors (view, toggle, dispatcherContainer) =
                if not toggle.Visible then []
                else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = toggle.Position; Size = toggle.Size; Rotation = 0.0f; Sprite = (if toggle?IsOn () || toggle?IsPressed () then toggle?OnSprite () else toggle?OffSprite ()); Color = Vector4.One }; Depth = toggle.Depth })]

            override this.GetQuickSize (toggle, world) =
                let sprite = toggle?OffSprite ()
                getTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap

    type FeelerDispatcher () =
        inherit EntityDispatcher ()

            override this.Init feeler =
                let feeler' = base.Init feeler
                let feeler'' = { feeler with IsTransformRelative = false }
                feeler''?IsTouched <- false

            override this.Register (address, textBox, world) =
                let world' =
                    world |>
                    subscribe DownMouseLeftAddress address handleFeelerEventDownMouseLeft |>
                    subscribe UpMouseLeftAddress address handleFeelerEventUpMouseLeft
                (textBox, world)

            override this.Unregister (address, textBox, world) =
                world |>
                    unsubscribe UpMouseLeftAddress address |>
                    unsubscribe DownMouseLeftAddress address

            override this.GetQuickSize (feeler, world) =
                Vector2 64.0f

    type BlockDispatcher () =
        inherit EntityDispatcher ()

            override this.Init block =
                let block' = base.Init block
                let block'' = { block with IsTransformRelative = true }
                (((block''
                    ?PhysicsId <- InvalidPhysicsId)
                    ?Density <- NormalDensity)
                    ?BodyType <- BodyType.Dynamic)
                    ?Sprite <- { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

            override this.Register (address, block, world) =
                registerBlockPhysics address block world

            override this.Unregister (address, block, world) =
                unregisterBlockPhysics address block world
            
            override this.PropagatePhysics (address, block, world) =
                let (block', world') = world |> unregisterBlockPhysics address block |> registerBlockPhysics address block
                set block' world' <| worldEntityLens address

            override this.ReregisterPhysicsHack (groupAddress, block, world) =
                let address = addrstr groupAddress block.Name
                let world' = unregisterBlockPhysics address block world
                let (block', world'') = registerBlockPhysics address block world'
                set block' world'' <| worldEntityLens address

            override this.HandleBodyTransformMessage (message, address, block, world) =
                let block' = { block with Position = message.Position - block.Size * 0.5f // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                                          Rotation = message.Rotation }
                set block' world <| worldEntityLens message.EntityAddress
            
            override this.GetRenderDescriptors (view, block, dispatcherContainer) =
                if not block.Visible then []
                else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = block.Position - view; Size = block.Size; Rotation = block.Rotation; Sprite = block?Sprite (); Color = Vector4.One }; Depth = block.Depth })]

            override this.GetQuickSize (block, world) =
                let sprite = block?Sprite ()
                getTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap

    type AvatarDispatcher () =
        inherit EntityDispatcher ()

            override this.Init avatar =
                let avatar' = base.Init avatar
                let avatar'' = { avatar with IsTransformRelative = true }
                ((avatar''
                    ?PhysicsId <- InvalidPhysicsId)
                    ?Density <- NormalDensity)
                    ?Sprite <- { SpriteAssetName = Lun.make "Image7"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }

            override this.Register (address, avatar, world) =
                registerAvatarPhysics address avatar world

            override this.Unregister (address, avatar, world) =
                unregisterAvatarPhysics address avatar world
            
            override this.PropagatePhysics (address, avatar, world) =
                let (avatar', world') = world |> unregisterAvatarPhysics address avatar |> registerAvatarPhysics address avatar
                set avatar' world' <| worldEntityLens address

            override this.ReregisterPhysicsHack (groupAddress, avatar, world) =
                let address = addrstr groupAddress avatar.Name
                let world' = unregisterAvatarPhysics address avatar world
                let (avatar', world'') = registerAvatarPhysics address avatar world'
                set avatar' world'' <| worldEntityLens address

            override this.HandleBodyTransformMessage (message, address, avatar, world) =
                let avatar' = { avatar with Position = message.Position - avatar.Size * 0.5f // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                                            Rotation = message.Rotation }
                set avatar' world <| worldEntityLens message.EntityAddress

            override this.GetRenderDescriptors (view, avatar, dispatcherContainer) =
                if not avatar.Visible then []
                else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = avatar.Position - view; Size = avatar.Size; Rotation = avatar.Rotation; Sprite = avatar?Sprite (); Color = Vector4.One }; Depth = avatar.Depth })]

            override this.GetQuickSize (avatar, world) =
                let sprite = avatar?Sprite ()
                getTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap

    type TileMapDispatcher () =
        inherit EntityDispatcher ()

            override this.Init tileMap =
                let tmxMap = TmxMap "Assets/Default/TileMap.tmx"
                let tileMap' = base.Init tileMap
                let tileMap'' = { tileMap with IsTransformRelative = true }
                ((((tileMap''
                        ?PhysicsIds <- [])
                        ?Density <- NormalDensity)
                        ?TileMapAsset <- { TileMapAssetName = Lun.make "TileMap"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" })
                        ?TmxMap <- tmxMap)
                        ?TileMapSprites <- [{ SpriteAssetName = Lun.make "TileSet"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }]

            override this.Register (address, tileMap, world) =
                registerTileMapPhysics address tileMap world

            override this.Unregister (address, tileMap, world) =
                unregisterTileMapPhysics address tileMap world
            
            override this.PropagatePhysics (address, tileMap, world) =
                let (tileMap', world') = world |> unregisterTileMapPhysics address tileMap |> registerTileMapPhysics address tileMap
                set tileMap' world' <| worldEntityLens address

            override this.ReregisterPhysicsHack (groupAddress, tileMap, world) =
                let address = addrstr groupAddress tileMap.Name
                let world' = unregisterTileMapPhysics address tileMap world
                let (tileMap', world'') = registerTileMapPhysics address tileMap world'
                set tileMap' world'' <| worldEntityLens address

            override this.GetRenderDescriptors (view, tileMap, dispatcherContainer) =
                if not tileMap.Visible then []
                else
                    let map = tileMap?TmxMap () : TmxMap
                    let layers = List.ofSeq map.Layers
                    List.mapi
                        (fun i (layer : TmxLayer) ->
                            let layeredTileLayerDescriptor =
                                LayeredTileLayerDescriptor
                                    { Descriptor =
                                        { Position = tileMap.Position - view
                                          Size = tileMap.Size
                                          Rotation = tileMap.Rotation
                                          MapSize = Vector2 (single map.Width, single map.Height)
                                          Tiles = layer.Tiles
                                          TileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                                          TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                          TileSetSprite = List.head <| tileMap?TileMapSprites () } // MAGIC_VALUE: for same reason as above
                                      Depth = tileMap.Depth + single i * 2.0f } // MAGIC_VALUE: assumption
                            LayerableDescriptor layeredTileLayerDescriptor)
                        layers

            override this.GetQuickSize (tileMap, world) =
                let map = tileMap?TmxMap () : TmxMap
                Vector2 (single <| map.Width * map.TileWidth, single <| map.Height * map.TileHeight)

    let registerEntity address entity world =
        entity?Register (address, entity, world)

    let unregisterEntity address world =
        let entity = get world <| worldEntityLens address
        entity?Unregister world

    let removeEntity address world =
        let world' = unregisterEntity address world
        set None world' <| worldOptEntityLens address

    let removeEntities address world =
        let entities = get world <| worldEntitiesLens address
        Map.fold
            (fun world' entityName _ -> removeEntity (address @ [entityName]) world')
            world
            entities

    let addEntity address entity world =
        let world' =
            match get world <| worldOptEntityLens address with
            | None -> world
            | Some _ -> removeEntity address world
        let (entity', world'') = registerEntity address entity world'
        set entity' world'' <| worldEntityLens address

    let addEntities address entities world =
        List.fold
            (fun world' entity -> addEntity (addrstr address entity.Name) entity world')
            world
            entities

    let adjustFieldCamera groupAddress world =
        let avatarAddress = groupAddress @ [FieldAvatarName]
        let entity = get world <| worldEntityLens avatarAddress
        let camera = { world.Camera with EyePosition = entity.Position + entity.Size * 0.5f }
        { world with Camera = camera }

    let adjustFieldCameraHandler groupAddress _ _ message world =
        (message, true, adjustFieldCamera groupAddress world)

    let moveFieldAvatarHandler groupAddress _ _ message world =
        let feelerAddress = groupAddress @ [FieldFeelerName]
        let feeler = get world <| worldEntityLens feelerAddress
        if feeler?IsTouched () then
            let avatarAddress = groupAddress @ [FieldAvatarName]
            let avatar = get world <| worldEntityLens avatarAddress
            let camera = world.Camera
            let view = getInverseViewF camera
            let mousePositionWorld = world.MouseState.MousePosition + view
            let avatarCenter = avatar.Position + avatar.Size * 0.5f
            let impulseVector = (mousePositionWorld - avatarCenter) * 5.0f
            let applyImpulseMessage = { PhysicsId = avatar?PhysicsId (); Impulse = impulseVector }
            let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
            (message, true, world')
        else (message, true, world)

    type GroupDispatcher () =
        class
        
            abstract member Register : Address * Group * Entity list * World -> World
            default this.Register (address, _, entities, world) = addEntities address entities world

            abstract member Unregister : Address * Group * World -> World
            default this.Unregister (address, _, world) = removeEntities address world

            end

    type OmniFieldGroupDispatcher () =
        inherit GroupDispatcher () with
        
            override this.Register (address, omniBattleGroup, entities, world) =
                let world_ = subscribe TickAddress [] (moveFieldAvatarHandler address) world
                let world_ = subscribe TickAddress [] (adjustFieldCameraHandler address) world_
                let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
                let world_ = base.Register (address, omniBattleGroup, entities, world_)
                adjustFieldCamera address world_

            override this.Unregister (address, omniFieldGroup, world) =
                let world_ = unsubscribe TickAddress [] world
                let world_ = unsubscribe TickAddress [] world_
                base.Unregister (address, omniFieldGroup, world_)

            end

    type OmniBattleGroupDispatcher () =
        inherit GroupDispatcher () with

            override this.Register (address, omniBattleGroup, entities, world) =
                let world' = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
                base.Register (address, omniBattleGroup, entities, world')

            override this.Unregister (address, omniBattleGroup, world) =
                base.Unregister (address, omniBattleGroup, world)

            end

    let registerGroup address (group : Group) entities world =
        group?Register (address, group, entities, world)

    let unregisterGroup address world =
        let group = get world <| worldGroupLens address
        group?Unregister (address, group, world)

    let removeGroup address world =
        let world' = unregisterGroup address world
        set None world' (worldOptGroupLens address)

    let removeGroups address world =
        let groups = get world <| worldGroupsLens address
        Map.fold
            (fun world' groupName _ -> removeGroup (address @ [groupName]) world')
            world
            groups

    let addGroup address (group : Group, entities) world =
        let world' =
            match get world <| worldOptGroupLens address with
            | None -> world
            | Some _ -> removeGroup address world
        let world'' = registerGroup address group entities world'
        set group world'' <| worldGroupLens address

    let addGroups address groupDescriptors world =
        List.fold
            (fun world' (groupName, group, entities) -> addGroup (address @ [groupName]) (group, entities) world')
            world
            groupDescriptors

    type TransitionDispatcher () =
        class
            end

    type ScreenDispatcher () =
        class
        
            abstract member Register : Address * Screen * ((Lun * Group * Entity list) list) * World -> World
            default this.Register (address, _, groupDescriptors, world) =
                addGroups address groupDescriptors world

            abstract member Unregister : Address * Screen * World -> World
            default this.Unregister (address, _, world) =
                removeGroups address world

            end

    let registerScreen address screen groupDescriptors world =
        screen?Register (address, screen, groupDescriptors, world)

    let unregisterScreen address world =
        let screen = get world <| worldScreenLens address
        screen?Unregister (address, screen, world)

    let removeScreen address world =
        let world' = unregisterScreen address world
        set None world' (worldOptScreenLens address)

    let addScreen address screen groupDescriptors world =
        let world' =
            match get world <| worldOptScreenLens address with
            | None -> world
            | Some _ -> removeScreen address world
        let world'' = registerScreen address screen groupDescriptors world'
        set screen world'' (worldScreenLens address)

    let rec handleSplashScreenIdleTick idlingTime ticks address subscriber message world =
        let world' = unsubscribe address subscriber world
        if ticks < idlingTime then
            let world'' = subscribe address subscriber (handleSplashScreenIdleTick idlingTime <| ticks + 1) world'
            (message, true, world'')
        else
            let optSelectedScreenAddress = get world' worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None ->
                trace "Program Error: Could not handle splash screen tick due to no selected screen."
                (message, false, world)
            | Some selectedScreenAddress ->
                let world'' = setScreenState selectedScreenAddress OutgoingState world'
                (message, true, world'')

    let handleSplashScreenIdle idlingTime address subscriber message world =
        let world' = subscribe TickAddress subscriber (handleSplashScreenIdleTick idlingTime 0) world
        (handle message, true, world')

    let addSplashScreen handleFinishedOutgoing address incomingTime idlingTime outgoingTime sprite world =
        let splashScreen = makeDissolveScreen incomingTime outgoingTime
        let splashGroup = makeDefaultGroup ()
        let splashLabel = makeDefaultEntity (Lun.make typeof<LabelDispatcher>.Name) (Some "SplashLabel") world
        let splashLabel' = { splashLabel with Entity.Size = world.Camera.EyeSize }
        let splashLabel'' = splashLabel'?LabelSprite <- (sprite : Sprite)
        let world' = addScreen address splashScreen [(Lun.make "SplashGroup", splashGroup, [splashLabel''])] world
        let world'' = subscribe (FinishedIncomingAddressPart @ address) address (handleSplashScreenIdle idlingTime) world'
        subscribe (FinishedOutgoingAddressPart @ address) address handleFinishedOutgoing world''

    let createDissolveScreenFromFile groupFileName groupName incomingTime outgoingTime screenAddress world =
        let screen = makeDissolveScreen incomingTime outgoingTime
        let (group, entities) = loadGroupFile groupFileName world
        addScreen screenAddress screen [(groupName, group, entities)] world

    type GameDispatcher () =
        class
            abstract member Register : Game * World -> World
            default this.Register (_, world) = world
            end

    type OmniGameDispatcher () =
        inherit GameDispatcher ()
        
            override this.Register (omniGame, world) =
                let dispatchers =
                    Map.addMany
                        [|Lun.make typeof<OmniBattleGroupDispatcher>.Name, OmniBattleGroupDispatcher () :> obj
                          Lun.make typeof<OmniFieldGroupDispatcher>.Name, OmniFieldGroupDispatcher () :> obj|]
                        world.Dispatchers
                { world with Dispatchers = dispatchers }

    let tryCreateEmptyWorld sdlDeps extData =
        match tryGenerateAssetMetadataMap "AssetGraph.xml" with
        | Left errorMsg -> Left errorMsg
        | Right assetMetadataMap ->
            let defaultDispatchers =
                Map.ofArray
                    [|Lun.make typeof<EntityDispatcher>.Name, EntityDispatcher () :> obj
                      Lun.make typeof<GroupDispatcher>.Name, GroupDispatcher () :> obj
                      Lun.make typeof<TransitionDispatcher>.Name, TransitionDispatcher () :> obj
                      Lun.make typeof<ScreenDispatcher>.Name, ScreenDispatcher () :> obj
                      Lun.make typeof<GameDispatcher>.Name, GameDispatcher () :> obj
                      // TODO: reemove these when editor has a way to specify the GameDispatcher
                      Lun.make typeof<OmniBattleGroupDispatcher>.Name, OmniBattleGroupDispatcher () :> obj
                      Lun.make typeof<OmniFieldGroupDispatcher>.Name, OmniFieldGroupDispatcher () :> obj
                      Lun.make typeof<OmniGameDispatcher>.Name, OmniGameDispatcher () :> obj|]
            let world =
                { Game = { Id = getNuId (); OptSelectedScreenAddress = None; Xtension = { OptXTypeName = Some <| Lun.make typeof<GameDispatcher>.Name; XFields = Map.empty }}
                  Screens = Map.empty
                  Groups = Map.empty
                  Entities = Map.empty
                  Camera = { EyePosition = Vector2.Zero; EyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) }
                  Subscriptions = Map.empty
                  MouseState = { MousePosition = Vector2.Zero; MouseDowns = Set.empty }
                  AudioPlayer = makeAudioPlayer ()
                  Renderer = makeRenderer sdlDeps.RenderContext
                  Integrator = makeIntegrator Gravity
                  AssetMetadataMap = assetMetadataMap
                  AudioMessages = [HintAudioPackageUse { FileName = "AssetGraph.xml"; PackageName = "Default"; HAPU = () }]
                  RenderMessages = [HintRenderingPackageUse { FileName = "AssetGraph.xml"; PackageName = "Default"; HRPU = () }]
                  PhysicsMessages = []
                  Dispatchers = defaultDispatchers
                  ExtData = extData }
            Right world

    let reregisterPhysicsHack groupAddress world =
        let entities = get world <| worldEntitiesLens groupAddress
        Map.fold (fun world _ entity -> entity?ReregisterPhysicsHack (groupAddress, entity, world)) world entities

    /// Play the world's audio.
    let play world =
        let audioMessages = world.AudioMessages
        let world' = { world with AudioMessages = [] }
        { world' with AudioPlayer = Nu.Audio.play audioMessages world.AudioPlayer }

    let getGroupRenderDescriptors camera dispatcherContainer entities =
        let view = getInverseView camera
        let entityValues = Map.toValueSeq entities
        Seq.map (fun entity -> entity?GetRenderDescriptors (view, entity, dispatcherContainer)) entityValues

    let getTransitionRenderDescriptors camera dispatcherContainer transition =
        match transition.OptDissolveSprite with
        | None -> []
        | Some dissolveSprite ->
            let progress = single transition.Ticks / single transition.Lifetime
            let alpha = match transition.Type with Incoming -> 1.0f - progress | Outgoing -> progress
            let color = Vector4 (Vector3.One, alpha)
            [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = Vector2.Zero; Size = camera.EyeSize; Rotation = 0.0f; Sprite = dissolveSprite; Color = color }; Depth = Single.MaxValue })]

    let getRenderDescriptors world =
        match get world worldOptSelectedScreenAddressLens with
        | None -> []
        | Some activeScreenAddress ->
            let optGroupMap = Map.tryFind activeScreenAddress.[0] world.Entities
            match optGroupMap with
            | None -> []
            | Some groupMap ->
                let entityMaps = List.fold List.flipCons [] <| Map.toValueList groupMap
                let descriptorSeqs = List.map (getGroupRenderDescriptors world.Camera world) entityMaps
                let descriptorSeq = Seq.concat descriptorSeqs
                let descriptors = List.concat descriptorSeq
                let activeScreen = get world (worldScreenLens activeScreenAddress)
                match activeScreen.State with
                | IncomingState -> descriptors @ getTransitionRenderDescriptors world.Camera world activeScreen.Incoming
                | OutgoingState -> descriptors @ getTransitionRenderDescriptors world.Camera world activeScreen.Outgoing
                | IdlingState -> descriptors

    /// Render the world.
    let render world =
        let renderMessages = world.RenderMessages
        let renderDescriptors = getRenderDescriptors world
        let renderer = world.Renderer
        let renderer' = Nu.Rendering.render renderMessages renderDescriptors renderer
        { world with RenderMessages = []; Renderer = renderer' }

    let handleIntegrationMessage (keepRunning, world) integrationMessage : bool * World =
        if not keepRunning then (keepRunning, world)
        else
            match integrationMessage with
            | BodyTransformMessage bodyTransformMessage -> 
                let entityAddress = bodyTransformMessage.EntityAddress
                let entity = get world <| worldEntityLens entityAddress
                (keepRunning, entity?HandleBodyTransformMessage (bodyTransformMessage, entityAddress, entity, world))
            | BodyCollisionMessage bodyCollisionMessage ->
                let collisionAddress = straddr "Collision" bodyCollisionMessage.EntityAddress
                let collisionData = CollisionData (bodyCollisionMessage.Normal, bodyCollisionMessage.Speed, bodyCollisionMessage.EntityAddress2)
                let collisionMessage = { Handled = false; Data = collisionData }
                publish collisionAddress collisionMessage world

    /// Handle physics integration messages.
    let handleIntegrationMessages integrationMessages world : bool * World =
        List.fold handleIntegrationMessage (true, world) integrationMessages

    /// Integrate the world.
    let integrate world =
        let integrationMessages = Nu.Physics.integrate world.PhysicsMessages world.Integrator
        let world' = { world with PhysicsMessages = [] }
        handleIntegrationMessages integrationMessages world'

    let run4 tryCreateWorld handleUpdate handleRender sdlConfig =
        runSdl
            (fun sdlDeps ->
                match tryCreateWorld sdlDeps with
                | Left _ as left -> left
                | Right world -> Right <| world.Game?Register (world.Game, world))
            (fun refEvent world ->
                let event = !refEvent
                match event.``type`` with
                | SDL.SDL_EventType.SDL_QUIT -> (false, world)
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = Vector2 (single event.button.x, single event.button.y)
                    let world' = { world with MouseState = { world.MouseState with MousePosition = mousePosition }}
                    if Set.contains MouseLeft world'.MouseState.MouseDowns then publish MouseDragAddress { Handled = false; Data = MouseMoveData mousePosition } world'
                    else publish MouseMoveAddress { Handled = false; Data = MouseButtonData (mousePosition, MouseLeft) } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mouseButton = makeMouseButton event.button.button
                    let world' = { world with MouseState = { world.MouseState with MouseDowns = Set.add mouseButton world.MouseState.MouseDowns }}
                    let messageAddress = addr ("Down/Mouse" </> str mouseButton)
                    let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                    publish messageAddress { Handled = false; Data = messageData } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mouseState = world.MouseState
                    let mouseButton = makeMouseButton event.button.button
                    if Set.contains mouseButton mouseState.MouseDowns then
                        let world' = { world with MouseState = { world.MouseState with MouseDowns = Set.remove mouseButton world.MouseState.MouseDowns }}
                        let messageAddress = addr ("Up/Mouse" </> str mouseButton)
                        let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                        publish messageAddress { Handled = false; Data = messageData } world'
                    else (true, world)
                | _ -> (true, world))
            (fun world ->
                let (keepRunning, world') = integrate world
                if not keepRunning then (keepRunning, world')
                else
                    let (keepRunning', world'') = publish TickAddress { Handled = false; Data = NoData } world'
                    if not keepRunning' then (keepRunning', world'')
                    else handleUpdate world'')
            (fun world -> let world' = render world in handleRender world')
            (fun world -> play world)
            (fun world -> { world with Renderer = handleRenderExit world.Renderer })
            sdlConfig

    let run tryCreateWorld handleUpdate sdlConfig =
        run4 tryCreateWorld handleUpdate id sdlConfig