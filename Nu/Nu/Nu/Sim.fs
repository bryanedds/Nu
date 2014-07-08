// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.Collections.Generic
open System.Reflection
open System.Xml
open SDL2
open OpenTK
open Prime
open Nu

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

// WISDOM: On threading physics...
//
// A simulation that would put physics on another thread should likely do so in a different app
// domain with communication via .NET remoting to make 100% sure that no sharing is happening.

[<AutoOpen>]
module InterativityModule =

    type Interactivity =
        | Gui
        | GuiAndPhysics
        | GuiAndPhysicsAndGamePlay

[<RequireQualifiedAccess>]
module Interactivity =

    let gamePlaying interactivity =
        match interactivity with
        | Gui -> false
        | GuiAndPhysics -> false
        | GuiAndPhysicsAndGamePlay -> true

    let physicsRunning interactivity =
        match interactivity with
        | Gui -> false
        | GuiAndPhysics -> true
        | GuiAndPhysicsAndGamePlay -> true

[<AutoOpen>]
module EventDataModule =

    /// Describes data relevant to specific events.
    type [<ReferenceEquality>] EventData =
        | MouseMoveData of Vector2
        | MouseButtonData of Vector2 * MouseButton
        | CollisionData of Vector2 * single * Address
        | OtherData of obj
        | NoData

[<AutoOpen>]
module EventModule =

    /// A generic event for the Nu game engine.
    /// A reference type.
    type [<ReferenceEquality>] Event =
        { Name : Address
          Publisher : Address
          Subscriber : Address
          Data : EventData }

[<AutoOpen>]
module EventHandledModule =

    type EventHandled =
        | Handled
        | Unhandled

[<AutoOpen>]
module TransitionTypeModule =

    type [<StructuralEquality; NoComparison>] TransitionType =
        | Incoming
        | Outgoing

[<AutoOpen>]
module ScreenStateModule =

    type [<StructuralEquality; NoComparison>] ScreenState =
        | IncomingState
        | OutgoingState
        | IdlingState

[<AutoOpen>]
module SimModule =

    type [<CLIMutable; StructuralEquality; NoComparison>] Entity =
        { Id : Guid
          Name : string
          Visible : bool
          Xtension : Xtension }

        static member (?) (this : Entity, memberName) =
            fun args ->
                Xtension.(?) (this.Xtension, memberName) args

        static member (?<-) (this : Entity, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member dispatchesAs dispatcherTargetType entity dispatcherContainer =
            Xtension.dispatchesAs dispatcherTargetType entity.Xtension dispatcherContainer

    type [<CLIMutable; StructuralEquality; NoComparison>] Group =
        { Id : Guid
          Xtension : Xtension }

        static member (?) (this : Group, memberName) =
            fun args ->
                Xtension.(?) (this.Xtension, memberName) args

        static member (?<-) (this : Group, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member dispatchesAs dispatcherTargetType group dispatcherContainer =
            Xtension.dispatchesAs dispatcherTargetType group.Xtension dispatcherContainer

    type [<CLIMutable; StructuralEquality; NoComparison>] Transition =
        { TransitionLifetime : int64
          TransitionTicks : int64
          TransitionType : TransitionType
          OptDissolveSprite : Sprite option }

    type [<CLIMutable; StructuralEquality; NoComparison>] Screen =
        { Id : Guid
          State : ScreenState
          Incoming : Transition
          Outgoing : Transition
          Xtension : Xtension }

        static member (?) (this : Screen, memberName) =
            fun args ->
                Xtension.(?) (this.Xtension, memberName) args

        static member (?<-) (this : Screen, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member dispatchesAs dispatcherTargetType screen dispatcherContainer =
            Xtension.dispatchesAs dispatcherTargetType screen.Xtension dispatcherContainer

    type [<CLIMutable; StructuralEquality; NoComparison>] Game =
        { Id : Guid
          OptSelectedScreenAddress : Address option
          Xtension : Xtension }

        static member (?) (this : Game, memberName) =
            fun args ->
                Xtension.(?) (this.Xtension, memberName) args

        static member (?<-) (this : Game, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member dispatchesAs dispatcherTargetType game dispatcherContainer =
            Xtension.dispatchesAs dispatcherTargetType game.Xtension dispatcherContainer

    type [<StructuralEquality; NoComparison>] Simulant =
        | Game of Game
        | Screen of Screen
        | Group of Group
        | Entity of Entity

    type [<ReferenceEquality>] Task =
        { Time : int64
          Operation : World -> World }

    /// Describes a game event subscription.
    /// A reference type.
    and [<ReferenceEquality>] Subscription =
        | ExitSub
        | SwallowSub
        | ScreenTransitionSub of Address (*desinationScreen*)
        | ScreenTransitionFromSplashSub of Address (*desinationScreen*)
        | CustomSub of (Event -> World -> EventHandled * World)

    /// TODO: document
    and SubscriptionEntry = Guid * Address * Subscription

    /// A map of event subscriptions.
    /// A reference type due to the reference-typeness of Subscription.
    and SubscriptionEntries = Map<Address, SubscriptionEntry list>

    /// TODO: document
    and SubscriptionSorter = SubscriptionEntry list -> World -> SubscriptionEntry list

    /// A map of subscription keys to unsubscription data.
    and UnsubscriptionEntries = Map<Guid, Address * Address>

    /// The world, in a functional programming sense.
    /// A reference type with some value semantics.
    and [<ReferenceEquality>] World =
        { Game : Game
          Screens : Map<string, Screen>
          Groups : Map<string, Map<string, Group>>
          Entities : Map<string, Map<string, Map<string, Entity>>>
          Ticks : int64
          Liveness : Liveness
          Interactivity : Interactivity
          Camera : Camera
          Tasks : Task list
          Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          MouseState : MouseState
          AudioPlayer : AudioPlayer
          Renderer : Renderer
          Integrator : Integrator
          AssetMetadataMap : AssetMetadataMap
          AudioMessages : AudioMessage rQueue
          RenderMessages : RenderMessage rQueue
          PhysicsMessages : PhysicsMessage rQueue
          Dispatchers : XDispatchers
          ExtData : obj } // TODO: consider if this is still the right approach in the context of the new Xtension stuff

        interface IXDispatcherContainer with
            member this.GetDispatchers () = this.Dispatchers
            end

[<RequireQualifiedAccess>]
module Sim =

    let getOptChild optChildFinder address parent =
        let optChild = optChildFinder address parent
        match optChild with
        | None -> None
        | Some child -> Some child

    let setOptChild addChild removeChild address parent optChild =
        match optChild with
        | None -> removeChild address parent
        | Some child -> addChild address parent child

    let getChild optChildFinder address parent =
        Option.get <| optChildFinder address parent

    let setChild childAdder childRemover address parent child =
        setOptChild childAdder childRemover address parent (Some child)

    let withSimulant worldSimulantLens fn address world =
        let simulant = get world <| worldSimulantLens address
        let simulant = fn simulant
        set simulant world <| worldSimulantLens address

    let withSimulantAndWorld worldSimulantLens fn address world =
        let simulant = get world <| worldSimulantLens address
        let (simulant, world) = fn simulant
        set simulant world <| worldSimulantLens address

    let tryWithSimulant worldOptSimulantLens worldSimulantLens fn address world =
        let optSimulant = get world <| worldOptSimulantLens address
        match optSimulant with
        | None -> world
        | Some simulant ->
            let simulant = fn simulant
            set simulant world <| worldSimulantLens address

    let tryWithSimulantAndWorld worldOptSimulantLens worldSimulantLens fn address world =
        let optSimulant = get world <| worldOptSimulantLens address
        match optSimulant with
        | None -> world
        | Some simulant ->
            let (simulant, world) = fn simulant
            set simulant world <| worldSimulantLens address

    // NOTE: these must be defined in a module like this rather than in a World type extension
    let mutable publish = Unchecked.defaultof<SubscriptionSorter -> Address -> Address -> EventData -> World -> World>
    let mutable publish4 = Unchecked.defaultof<Address -> Address -> EventData -> World -> World>
    let mutable subscribe = Unchecked.defaultof<Guid -> Address -> Address -> Subscription -> World -> World>
    let mutable subscribe4 = Unchecked.defaultof<Address -> Address -> Subscription -> World -> World>
    let mutable unsubscribe = Unchecked.defaultof<Guid -> World -> World>
    let mutable withSubscription = Unchecked.defaultof<Address -> Address -> Subscription -> (World -> World) -> World -> World>
    let mutable observe = Unchecked.defaultof<Address -> Address -> Subscription -> World -> World>

[<AutoOpen>]
module WorldSimModule =

    type World with

        static member physicsRunning world =
            Interactivity.physicsRunning world.Interactivity

        static member gamePlaying world =
            Interactivity.gamePlaying world.Interactivity

        // NOTE: function forwarding
        static member publish publishSort eventName publisher eventData world = Sim.publish publishSort eventName publisher eventData world
        static member publish4 eventName publisher eventData world = Sim.publish4 eventName publisher eventData world
        static member subscribe subscriptionKey eventName subscriber subscription world = Sim.subscribe subscriptionKey eventName subscriber subscription world
        static member subscribe4 eventName subscriber subscription world = Sim.subscribe4 eventName subscriber subscription world
        static member unsubscribe subscriptionKey world = Sim.unsubscribe subscriptionKey world
        static member withSubscription eventName subscriber subscription procedure world = Sim.withSubscription eventName subscriber subscription procedure world
        static member observe eventName subscriber subscription world = Sim.observe eventName subscriber subscription world