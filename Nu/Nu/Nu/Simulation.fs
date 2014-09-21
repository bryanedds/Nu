// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module InterativityModule =

    /// Describes the game engine's current level of 'interactivity'.
    type Interactivity =
        | Gui
        | GuiAndPhysics
        | GuiAndPhysicsAndGamePlay

[<RequireQualifiedAccess>]
module Interactivity =

    /// Query that the engine is in game-playing mode.
    let isGamePlaying interactivity =
        match interactivity with
        | Gui -> false
        | GuiAndPhysics -> false
        | GuiAndPhysicsAndGamePlay -> true

    /// Query that the physics system is running.
    let isPhysicsRunning interactivity =
        match interactivity with
        | Gui -> false
        | GuiAndPhysics -> true
        | GuiAndPhysicsAndGamePlay -> true

[<AutoOpen>]
module TransitionTypeModule =

    /// The type of a screen transition. Incoming means a new screen is being shown, and Outgoing
    /// means an existing screen being hidden.
    type [<StructuralEquality; NoComparison>] TransitionType =
        | Incoming
        | Outgoing

[<AutoOpen>]
module ScreenStateModule =

    /// The state of a screen in regards to its transitions.
    type [<StructuralEquality; NoComparison>] ScreenState =
        | IncomingState
        | OutgoingState
        | IdlingState

[<AutoOpen>]
module SimModule =

    /// The state of one of a screen's transitions.
    type [<CLIMutable; StructuralEquality; NoComparison>] Transition =
        { TransitionLifetime : int64
          TransitionTicks : int64
          TransitionType : TransitionType
          OptDissolveImage : Image option }
    
        static member make transitionType =
            { TransitionLifetime = 0L
              TransitionTicks = 0L
              TransitionType = transitionType
              OptDissolveImage = None }

    /// The data for a mouse move event.
    type [<StructuralEquality; NoComparison>] MouseMoveData =
        { Position : Vector2 }

    /// The data for a mouse button event.
    type [<StructuralEquality; NoComparison>] MouseButtonData =
        { Position : Vector2
          Button : MouseButton }

    /// The data for a keyboard key event.
    type [<StructuralEquality; NoComparison>] KeyboardKeyData =
        { ScanCode : uint32 }

    /// The data for a collision event.
    type [<StructuralEquality; NoComparison>] CollisionData =
        { Normal : Vector2
          Speed : single
          Collidee : Address }

    /// The data for an entity change event.
    type [<StructuralEquality; NoComparison>] EntityChangeData =
        { OldEntity : Entity }

    /// The data for a user-defined event.
    and [<StructuralEquality; NoComparison>] OtherData =
        { Obj : obj }

    /// The data for a user-defined event.
    and NoData = unit

    /// The data for an event.
    and [<ReferenceEquality>] EventData =
        | MouseMoveData of MouseMoveData
        | MouseButtonData of MouseButtonData
        | KeyboardKeyData of KeyboardKeyData
        | CollisionData of CollisionData
        | EntityChangeData of EntityChangeData
        | OtherData of OtherData
        | NoData of NoData

    /// An event used by Nu's purely functional event system.
    and [<ReferenceEquality>] Event =
        { Name : Address
          SubscriberAddress : Address
          Subscriber : Simulant
          PublisherAddress : Address
          OptPublisher : Simulant option
          Data : EventData }

    /// Describes whether an event has been resolved or should be propagated.
    and EventHandling =
        | Resolved
        | Propagate

    /// Describes a game event subscription.
    and [<ReferenceEquality>] Subscription =
        | ExitSub
        | SwallowSub
        | ScreenTransitionSub of desinationScreen : Address
        | ScreenTransitionFromSplashSub of desinationScreen : Address
        | CustomSub of (Event -> World -> EventHandling * World)

    /// An entry into the world's subscription map.
    and SubscriptionEntry = Guid * Address * Subscription

    /// A map of event subscriptions.
    and SubscriptionEntries = Map<Address, SubscriptionEntry list>

    /// Abstracts over a subscription sorting procedure.
    and SubscriptionSorter = SubscriptionEntry list -> World -> SubscriptionEntry list

    /// A map of subscription keys to unsubscription data.
    and UnsubscriptionEntries = Map<Guid, Address * Address>

    /// A task to be completed at the given time, with time being represented by the world's tick
    /// field.
    and [<ReferenceEquality>] Task =
        { ScheduledTime : int64
          Operation : World -> World }

    /// The default dispatcher for entities.
    and EntityDispatcher () =

        static member FieldDefinitions =
            [define? Position Vector2.Zero
             define? Depth 0.0f
             define? Size DefaultEntitySize
             define? Rotation 0.0f
             define? Visible true]

        abstract member Register : Address * Entity * World -> Entity * World
        default dispatcher.Register (_, entity, world) = (entity, world)

        abstract member Unregister : Address * Entity * World -> Entity * World
        default dispatcher.Unregister (_, entity, world) = (entity, world)

        abstract member PropagatePhysics : Address * Entity * World -> World
        default dispatcher.PropagatePhysics (_, _, world) = world

        abstract member GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors (_, _) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = Vector2.One

        abstract member GetPickingPriority : Entity * World -> single
        default dispatcher.GetPickingPriority (entity, _) = entity.Depth

    /// The default dispatcher for groups.
    and GroupDispatcher () =

        abstract member Register : Address * Group * World -> Group * World
        default dispatcher.Register (_, group, world) = (group, world)

        abstract member Unregister : Address * Group * World -> Group * World
        default dispatcher.Unregister (_, group, world) = (group, world)

    /// The default dispatcher for screens.
    and ScreenDispatcher () =

        abstract member Register : Address * Screen * World -> Screen * World
        default dispatcher.Register (_, screen, world) = (screen, world)

        abstract member Unregister : Address * Screen * World -> Screen * World
        default dispatcher.Unregister (_, screen, world) = (screen, world)

    /// The default dispatcher for games.
    and GameDispatcher () =

        abstract member Register : Game * World -> Game * World
        default dispatcher.Register (game, world) = (game, world)

    /// Dynamically augments an entity's behavior in a composable way.
    and Facet () =

        abstract member Register : Address * Entity * World -> Entity * World
        default facet.Register (address, entity, world) =
            let world = facet.RegisterPhysics (address, entity, world)
            (entity, world)

        abstract member Unregister : Address * Entity * World -> Entity * World
        default facet.Unregister (address, entity, world) =
            let world = facet.UnregisterPhysics (address, entity, world)
            (entity, world)

        abstract member RegisterPhysics : Address * Entity * World -> World
        default facet.RegisterPhysics (_, _, world) = world

        abstract member UnregisterPhysics : Address * Entity * World -> World
        default facet.UnregisterPhysics (_, _, world) = world

        abstract member PropagatePhysics : Address * Entity * World -> World
        default facet.PropagatePhysics (_, _, world) = world

        abstract member GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default facet.GetRenderDescriptors (_, _) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default facet.GetQuickSize (_, _) = DefaultEntitySize

    /// The type around which the whole game engine is based! Used in combination with dispatchers
    /// to implement things like buttons, avatars, blocks, and things of that sort.
    and [<CLIMutable; StructuralEquality; NoComparison>] Entity =
        { Id : Guid
          Name : string
          Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Depth : single // NOTE: will become part of position if Nu gets 3d capabilities
          Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Rotation : single // NOTE: will become a Vector3 if Nu gets 3d capabilities
          Visible : bool
          ViewType : ViewType
          DispatcherNp : EntityDispatcher
          FacetNames : string list
          FacetsNp : Facet list
          OptOverlayName : string option
          Xtension : Xtension } // TODO: now that there are field descriptors, consider making their persistence configurable with data instead of name-suffixing.

        static member (?) (this : Entity, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : Entity, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    /// Forms logical groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Group =
        { Id : Guid
          Name : string
          DispatcherNp : GroupDispatcher
          Xtension : Xtension }

        static member (?) (this : Group, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : Group, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    /// The screen type that allows transitioning to and fro other screens, and also hosts the
    /// currently interactive groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Screen =
        { Id : Guid
          Name : string
          ScreenState : ScreenState
          Incoming : Transition
          Outgoing : Transition
          DispatcherNp : ScreenDispatcher
          Xtension : Xtension }

        static member (?) (this : Screen, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : Screen, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    /// The game type that hosts the various screens used to navigate through a game.
    and [<CLIMutable; StructuralEquality; NoComparison>] Game =
        { Id : Guid
          Name : string
          OptSelectedScreenAddress : Address option
          DispatcherNp : GameDispatcher
          Xtension : Xtension }

        static member (?) (this : Game, memberName) =
            Xtension.(?) (this.Xtension, memberName)

        static member (?<-) (this : Game, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    /// Abstracts over the simulation types (Game, Screen, Group, Entity).
    and [<StructuralEquality; NoComparison>] Simulant =
        | Game of Game
        | Screen of Screen
        | Group of Group
        | Entity of Entity

    /// The world's components.
    and [<ReferenceEquality>] Components =
        { AudioPlayer : AudioPlayer
          Renderer : Renderer
          Integrator : Integrator
          Overlayer : Overlayer
          EntityDispatchers : Map<string, EntityDispatcher>
          GroupDispatchers : Map<string, GroupDispatcher>
          ScreenDispatchers : Map<string, ScreenDispatcher>
          GameDispatchers : Map<string, GameDispatcher>
          Facets : Map<string, Facet> }

    /// The world's message queues.
    and [<ReferenceEquality>] MessageQueues =
        { AudioMessages : AudioMessage rQueue
          RenderingMessages : RenderMessage rQueue
          PhysicsMessages : PhysicsMessage rQueue }

    /// The world's higher order facilities.
    and [<ReferenceEquality>] Callbacks =
        { Tasks : Task list
          Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries }

    /// The world's state.
    and [<ReferenceEquality>] State =
        { TickTime : int64
          Liveness : Liveness
          Interactivity : Interactivity
          AssetMetadataMap : AssetMetadataMap
          AssetGraphFileName : string
          OverlayFileName : string
          UserState : obj }

    /// The world, in a functional programming sense. Hosts the game object, the dependencies
    /// needed to implement a game, messages to by consumed by the various engine sub-systems,
    /// and general configuration data.
    and [<ReferenceEquality>] World =
        { Game : Game
          Screens : Map<string, Screen>
          Groups : Map<string, Map<string, Group>>
          Entities : Map<string, Map<string, Map<string, Entity>>>
          Camera : Camera
          Components : Components
          MessageQueues : MessageQueues
          Callbacks : Callbacks
          State : State }

    /// Provides a way to make user-defined components.
    and UserComponentFactory () =
        abstract MakeEntityDispatchers : unit -> Map<string, EntityDispatcher>
        default this.MakeEntityDispatchers () = Map.empty
        abstract MakeGroupDispatchers : unit -> Map<string, GroupDispatcher>
        default this.MakeGroupDispatchers () = Map.empty
        abstract MakeScreenDispatchers : unit -> Map<string, ScreenDispatcher>
        default this.MakeScreenDispatchers () = Map.empty
        abstract MakeGameDispatchers : unit -> Map<string, GameDispatcher>
        default this.MakeGameDispatchers () = Map.empty
        abstract MakeFacets : unit -> Map<string, Facet>
        default this.MakeFacets () = Map.empty

[<RequireQualifiedAccess>]
module EventData =

    /// A convenience function to forcibly extract mouse movement data from an event data abstraction.
    let toMouseMoveData data = match data with MouseMoveData d -> d | _ -> failwith <| "Expected MouseMoveData from event data '" + string data + "'."
    
    /// A convenience function to forcibly extract mouse button data from an event data abstraction.
    let toMouseButtonData data = match data with MouseButtonData d -> d | _ -> failwith <| "Expected MouseButtonData from event data '" + string data + "'."
    
    /// A convenience function to forcibly extract keyboard key data from an event data abstraction.
    let toKeyboardKeyData data = match data with KeyboardKeyData d -> d | _ -> failwith <| "Expected KeyboardKeyData from event data '" + string data + "'."

    /// A convenience function to forcibly extract collision data from an event data abstraction.
    let toCollisionData data = match data with CollisionData d -> d | _ -> failwith <| "Expected CollisionData from event data '" + string data + "'."

    /// A convenience function to forcibly extract entity change data from an event data abstraction.
    let toEntityChangeData data = match data with EntityChangeData d -> d | _ -> failwith <| "Expected EntityChangeData from event data '" + string data + "'."

    /// A convenience function to forcibly extract user-defined data from an event data abstraction.
    let toOtherData data = match data with OtherData d -> d | _ -> failwith <| "Expected OtherData from event data '" + string data + "'."

    /// A convenience function to forcibly extract no data from an event data abstraction.
    let toNoData data = match data with NoData d -> d | _ -> failwith <| "Expected NoData from event data '" + string data + "'."

    /// A convenience function to forcibly extract data from an event data abstraction.
    let toGeneric<'d> eventData =
        let typeName = typeof<'d>.Name
        match typeName with
        | "MouseMoveData" -> toMouseMoveData eventData :> obj :?> 'd
        | "MouseButtonData" -> toMouseButtonData eventData :> obj :?> 'd
        | "KeyboardKeyData" -> toKeyboardKeyData eventData :> obj :?> 'd
        | "CollisionData" -> toCollisionData eventData :> obj :?> 'd
        | "EntityChangeData" -> toEntityChangeData eventData :> obj :?> 'd
        | "OtherData" -> toOtherData eventData :> obj :?> 'd
        | "Unit" -> toNoData eventData :> obj :?> 'd
        | "Object" -> eventData :> obj :?> 'd
        | _ -> failwith <| "Invalid event data type '" + typeName + "'."

[<RequireQualifiedAccess>]
module World =

    /// Publish an event.
    let mutable publish = Unchecked.defaultof<SubscriptionSorter -> Address -> Address -> EventData -> World -> World>
    
    /// Publish an event.
    let mutable publish4 = Unchecked.defaultof<Address -> Address -> EventData -> World -> World>
    
    /// Subscribe to an event.
    let mutable subscribe = Unchecked.defaultof<Guid -> Address -> Address -> Subscription -> World -> World>
    
    /// Subscribe to an event.
    let mutable subscribe4 = Unchecked.defaultof<Address -> Address -> Subscription -> World -> World>
    
    /// Unsubscribe from an event.
    let mutable unsubscribe = Unchecked.defaultof<Guid -> World -> World>
    
    /// Keep active a subscription for the duration of a procedure.
    let mutable withSubscription = Unchecked.defaultof<Address -> Address -> Subscription -> (World -> World) -> World -> World>
    
    /// Keep active a subscription for the lifetime of a simulant.
    let mutable observe = Unchecked.defaultof<Address -> Address -> Subscription -> World -> World>

    /// Get all of a world's dispatchers.
    let getDispatchers world =
        Map.map Map.objectify world.Components.EntityDispatchers ^^
        Map.map Map.objectify world.Components.GroupDispatchers ^^
        Map.map Map.objectify world.Components.ScreenDispatchers ^^
        Map.map Map.objectify world.Components.GameDispatchers

    /// Transform a bunch of simulants in the context of a world.
    let transformSimulants transform parentAddress simulants world : Map<string, 's> * World =
        Map.fold
            (fun (simulants, world) simulantName (simulant : 's) ->
                let (simulant, world) = transform (parentAddress @+ [simulantName]) simulant world
                (Map.add simulantName simulant simulants, world))
            (Map.empty, world)
            simulants

    /// Set the Camera field of the world.
    let setCamera camera world =
        { world with Camera = camera }

    /// Set the AudioPlayer field of the world.
    let internal setAudioPlayer audioPlayer world =
        let components = { world.Components with AudioPlayer = audioPlayer }
        { world with Components = components }

    /// Set the Renderer field of the world.
    let internal setRenderer renderer world =
        let components = { world.Components with Renderer = renderer }
        { world with Components = components }

    /// Set the Integrator field of the world.
    let internal setIntegrator integrator world =
        let components = { world.Components with Integrator = integrator }
        { world with Components = components }

    /// Set the Overlayer field of the world.
    let internal setOverlayer overlayer world =
        let components = { world.Components with Overlayer = overlayer }
        { world with Components = components }

    /// Set the EntityDispatchers field of the world.
    let internal setEntityDispatchers dispatchers world =
        let components = { world.Components with EntityDispatchers = dispatchers }
        { world with Components = components }

    /// Set the GroupDispatchers field of the world.
    let internal setGroupDispatchers dispatchers world =
        let components = { world.Components with GroupDispatchers = dispatchers }
        { world with Components = components }

    /// Set the ScreenDispatchers field of the world.
    let internal setScreenDispatchers dispatchers world =
        let components = { world.Components with ScreenDispatchers = dispatchers }
        { world with Components = components }

    /// Set the GameDispatchers field of the world.
    let internal setGameDispatchers dispatchers world =
        let components = { world.Components with GameDispatchers = dispatchers }
        { world with Components = components }

    /// Set the Facets field of the world.
    let internal setFacets facets world =
        let components = { world.Components with Facets = facets }
        { world with Components = components }

    /// Clear the audio messages.
    let clearAudioMessages world =
        let messageQueues = { world.MessageQueues with AudioMessages = [] }
        { world with MessageQueues = messageQueues }

    /// Clear the rendering messages.
    let clearRenderingMessages world =
        let messageQueues = { world.MessageQueues with RenderingMessages = [] }
        { world with MessageQueues = messageQueues }

    /// Clear the physics messages.
    let clearPhysicsMessages world =
        let messageQueues = { world.MessageQueues with PhysicsMessages = [] }
        { world with MessageQueues = messageQueues }

    /// Add a physics message to the world.
    let addPhysicsMessage message world =
        let messageQueues = { world.MessageQueues with PhysicsMessages = message :: world.MessageQueues.PhysicsMessages }
        { world with MessageQueues = messageQueues }

    /// Add a rendering message to the world.
    let addRenderingMessage message world =
        let messageQueues = { world.MessageQueues with RenderingMessages = message :: world.MessageQueues.RenderingMessages }
        { world with MessageQueues = messageQueues }

    /// Add an audio message to the world.
    let addAudioMessage message world =
        let messageQueues = { world.MessageQueues with AudioMessages = message :: world.MessageQueues.AudioMessages }
        { world with MessageQueues = messageQueues }

    /// Add a task to be executed by the engine at the specified task tick.
    let addTask task world =
        let callbacks = { world.Callbacks with Tasks = task :: world.Callbacks.Tasks }
        { world with Callbacks = callbacks }

    /// Add multiple task to be executed by the engine at the specified task tick.
    let addTasks tasks world =
        let callbacks = { world.Callbacks with Tasks = tasks @ world.Callbacks.Tasks }
        { world with Callbacks = callbacks }

    /// Restore tasks to be executed by the engine at the specified task tick.
    let restoreTasks tasks world =
        let callbacks = { world.Callbacks with Tasks = world.Callbacks.Tasks @ tasks }
        { world with Callbacks = callbacks }

    /// Clear all tasks.
    let clearTasks world =
        let callbacks = { world.Callbacks with Tasks = [] }
        { world with Callbacks = callbacks }

    /// Add an event subscription.
    let addSubscription eventName subscription world =
        let callbacks = { world.Callbacks with Subscriptions = Map.add eventName subscription world.Callbacks.Subscriptions }
        { world with Callbacks = callbacks }

    /// Add an event unsubscription.
    let addUnsubscription subscriptionKey unsubscription world =
        let callbacks = { world.Callbacks with Unsubscriptions = Map.add subscriptionKey unsubscription world.Callbacks.Unsubscriptions }
        { world with Callbacks = callbacks }

    /// Remove an event subscription.
    let removeSubscription eventName world =
        let callbacks = { world.Callbacks with Subscriptions = Map.remove eventName world.Callbacks.Subscriptions }
        { world with Callbacks = callbacks }

    /// Remove an event unsubscription.
    let removeUnsubscription subscriptionKey world =
        let callbacks = { world.Callbacks with Unsubscriptions = Map.remove subscriptionKey world.Callbacks.Unsubscriptions }
        { world with Callbacks = callbacks }

    /// Increment the TickTime field of the world.
    let incrementTickTime world =
        let state = { world.State with TickTime = world.State.TickTime + 1L }
        { world with State = state }

    /// Place the world into a state such that the app will exit at the end of the current frame.
    let exit world =
        let state = { world.State with Liveness = Exiting }
        { world with State = state }

    /// Query that the engine is in game-playing mode.
    let isGamePlaying world = Interactivity.isGamePlaying world.State.Interactivity

    /// Query that the physics system is running.
    let isPhysicsRunning world = Interactivity.isPhysicsRunning world.State.Interactivity

    /// Set the level of the world's interactivity.
    let setInteractivity interactivity world =
        let state = { world.State with Interactivity = interactivity }
        { world with State = state }

    /// Set the AssetMetadataMap field of the world.
    let setAssetMetadataMap assetMetadataMap world =
        let state = { world.State with AssetMetadataMap = assetMetadataMap }
        { world with State = state }

    /// Set the UserState field of the world.
    let setUserState userState world =
        let state = { world.State with UserState = userState }
        { world with State = state }

[<AutoOpen>]
module WorldInputModule =

    type World with

        /// Convert a MouseButton to SDL's representation.
        static member toSdlMouseButton mouseButton =
            MouseState.toSdlButton mouseButton

        /// Convert SDL's representation of a mouse button to a MouseButton.
        static member toNuMouseButton mouseButton =
            MouseState.toNuButton mouseButton

        /// Query that the given mouse button is down.
        static member isMouseButtonDown mouseButton (_ : World) =
            MouseState.isButtonDown mouseButton

        /// Get the position of the mouse.
        static member getMousePosition (_ : World) =
            MouseState.getPosition ()

        /// Get the position of the mouse in floating-point coordinates.
        static member getMousePositionF (_ : World) =
            MouseState.getPositionF ()

        /// Query that the given keyboard key is down.
        static member isKeyboardKeyDown scanCode (_ : World) =
            KeyboardState.isKeyDown scanCode

        // TODO: implement getKeyboardModifierState.

[<AutoOpen>]
module WorldPhysicsModule =

    type World with

        /// Does the world contain the body with the given physics id?
        static member bodyExists physicsId world =
            Physics.bodyExists physicsId world.Components.Integrator

        /// Get the contact normals of the body with the given physics id.
        static member getBodyContactNormals physicsId world =
            Physics.getBodyContactNormals physicsId world.Components.Integrator

        /// Get the linear velocity of the body with the given physics id.
        static member getBodyLinearVelocity physicsId world =
            Physics.getBodyLinearVelocity physicsId world.Components.Integrator

        /// Get the contact normals where the body with the given physics id is touching the ground.
        static member getBodyGroundContactNormals physicsId world =
            Physics.getBodyGroundContactNormals physicsId world.Components.Integrator

        /// Try to get a contact normal where the body with the given physics id is touching the ground.
        static member getOptBodyGroundContactNormal physicsId world =
            Physics.getOptBodyGroundContactNormal physicsId world.Components.Integrator

        /// Try to get a contact tangent where the body with the given physics id is touching the ground.
        static member getOptBodyGroundContactTangent physicsId world =
            Physics.getOptBodyGroundContactTangent physicsId world.Components.Integrator

        /// Query that the body with the give physics id is on the ground.
        static member isBodyOnGround physicsId world =
            Physics.isBodyOnGround physicsId world.Components.Integrator

        /// Send a message to the physics system to create a body with the given physics id.
        static member createBody entityAddress physicsId position rotation bodyProperties world =
            let createBodyMessage = CreateBodyMessage { EntityAddress = entityAddress; PhysicsId = physicsId; Position = position; Rotation = rotation; BodyProperties = bodyProperties }
            World.addPhysicsMessage createBodyMessage world

        /// Send a message to the physics system to destroy a body with the given physics id.
        static member destroyBody physicsId world =
            let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
            World.addPhysicsMessage destroyBodyMessage world

        /// Send a message to the physics system to set the position of a body with the given physics id.
        static member setBodyPosition position physicsId world =
            let setBodyPositionMessage = SetBodyPositionMessage { PhysicsId = physicsId; Position = position }
            World.addPhysicsMessage setBodyPositionMessage world

        /// Send a message to the physics system to set the rotation of a body with the given physics id.
        static member setBodyRotation rotation physicsId world =
            let setBodyRotationMessage = SetBodyRotationMessage { PhysicsId = physicsId; Rotation = rotation }
            World.addPhysicsMessage setBodyRotationMessage world

        /// Send a message to the physics system to set the linear velocity of a body with the given physics id.
        static member setBodyLinearVelocity linearVelocity physicsId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { PhysicsId = physicsId; LinearVelocity = linearVelocity }
            World.addPhysicsMessage setBodyLinearVelocityMessage world

        /// Send a message to the physics system to apply linear impulse to a body with the given physics id.
        static member applyBodyLinearImpulse linearImpulse physicsId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { PhysicsId = physicsId; LinearImpulse = linearImpulse }
            World.addPhysicsMessage applyBodyLinearImpulseMessage world

        /// Send a message to the physics system to apply force to a body with the given physics id.
        static member applyBodyForce force physicsId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { PhysicsId = physicsId; Force = force }
            World.addPhysicsMessage applyBodyForceMessage world

[<AutoOpen>]
module WorldRenderingModule =

    type World with

        /// Hint that a rendering asset package with the given name should be loaded. Should be
        /// used to avoid loading assets at inconvenient times (such as in the middle of game play!)
        static member hintRenderingPackageUse packageName world =
            let hintRenderingPackageUseMessage = HintRenderingPackageUseMessage { PackageName = packageName }
            World.addRenderingMessage hintRenderingPackageUseMessage world
            
        /// Hint that a rendering package should be unloaded since its assets will not be used
        /// again (or until specified via World.hintRenderingPackageUse).
        static member hintRenderingPackageDisuse packageName world =
            let hintRenderingPackageDisuseMessage = HintRenderingPackageDisuseMessage { PackageName = packageName }
            World.addRenderingMessage hintRenderingPackageDisuseMessage world
            
        /// Send a message to the renderer to reload its rendering assets.
        static member reloadRenderingAssets world =
            let reloadRenderingAssetsMessage = ReloadRenderingAssetsMessage
            World.addRenderingMessage reloadRenderingAssetsMessage world

[<AutoOpen>]
module WorldAudioModule =

    type World with

        /// Send a message to the audio system to play a song.
        static member playSong song volume timeToFadeOutSongMs world =
            let playSongMessage = PlaySongMessage { Song = song; Volume = volume; TimeToFadeOutSongMs = timeToFadeOutSongMs }
            World.addAudioMessage playSongMessage world

        /// Send a message to the audio system to play a song.
        static member playSong6 songAssetName packageName volume timeToFadeOutSongMs world =
            let song = { SongAssetName = songAssetName; PackageName = packageName }
            World.playSong song volume timeToFadeOutSongMs world

        /// Send a message to the audio system to play a sound.
        static member playSound sound volume world =
            let playSoundMessage = PlaySoundMessage { Sound = sound; Volume = volume }
            World.addAudioMessage playSoundMessage world

        /// Send a message to the audio system to play a sound.
        static member playSound5 soundAssetName packageName volume world =
            let sound = { SoundAssetName = soundAssetName; PackageName = packageName }
            World.playSound sound volume world

        /// Send a message to the audio system to fade out a song.
        static member fadeOutSong timeToFadeOutSongMs world =
            let fadeOutSongMessage = FadeOutSongMessage timeToFadeOutSongMs
            World.addAudioMessage fadeOutSongMessage world

        /// Send a message to the audio system to stop a song.
        static member stopSong world =
            World.addAudioMessage StopSongMessage world
            
        /// Hint that an audio asset package with the given name should be loaded. Should be used
        /// to avoid loading assets at inconvenient times (such as in the middle of game play!)
        static member hintAudioPackageUse packageName world =
            let hintAudioPackageUseMessage = HintAudioPackageUseMessage { PackageName = packageName }
            World.addAudioMessage hintAudioPackageUseMessage world
            
        /// Hint that an audio package should be unloaded since its assets will not be used again
        /// (or until specified via a HintAudioPackageUseMessage).
        static member hintAudioPackageDisuse packageName world =
            let hintAudioPackageDisuseMessage = HintAudioPackageDisuseMessage { PackageName = packageName }
            World.addAudioMessage hintAudioPackageDisuseMessage world

        /// Send a message to the audio player to reload its audio assets.
        static member reloadAudioAssets world =
            let reloadAudioAssetsMessage = ReloadAudioAssetsMessage
            World.addAudioMessage reloadAudioAssetsMessage world

[<RequireQualifiedAccess>]
module Simulant =

    let getOptChild optChildFinder address parent =
        let optChild = optChildFinder address parent
        match optChild with
        | Some child -> Some child
        | None -> None

    let setOptChild addChild removeChild address parent optChild =
        match optChild with
        | Some child -> addChild address parent child
        | None -> removeChild address parent

    let getChild optChildFinder address parent =
        Option.get <| optChildFinder address parent

    let setChild childAdder childRemover address parent child =
        setOptChild childAdder childRemover address parent (Some child)

    let toEntity simulant =
        match simulant with
        | Entity entity -> entity
        | Group _ | Screen _ | Game _ -> failwith "Invalid conversion of simulant to entity."

    let toGroup simulant =
        match simulant with
        | Group group -> group
        | Entity _ | Screen _ | Game _ -> failwith "Invalid conversion of simulant to group."

    let toScreen simulant =
        match simulant with
        | Screen screen -> screen
        | Entity _ | Group _ | Game _ -> failwith "Invalid conversion of simulant to screen."

    let toGame simulant =
        match simulant with
        | Game game -> game
        | Entity _ | Group _ | Screen _ -> failwith "Invalid conversion of simulant to game."

    let toGeneric<'s> simulant =
        let s = typeof<'s>
        // OPTIMIZATION: Entity type is most common and therefore checked first.
        if s = typeof<Entity> then toEntity simulant :> obj :?> 's
        elif s = typeof<Group> then toGroup simulant :> obj :?> 's
        elif s = typeof<Screen> then toScreen simulant :> obj :?> 's
        elif s = typeof<Game> then toGame simulant :> obj :?> 's
        elif s = typeof<obj> then simulant :> obj :?> 's
        else failwith <| "Invalid simulation type '" + s.Name + "'."

[<RequireQualifiedAccess>]
module Event =

    /// Unwrap commonly-useful values of an event.
    let unwrap<'s, 'd> event =
        let subscriber = Simulant.toGeneric<'s> event.Subscriber
        let eventData = EventData.toGeneric<'d> event.Data
        (event.SubscriberAddress, subscriber, eventData)