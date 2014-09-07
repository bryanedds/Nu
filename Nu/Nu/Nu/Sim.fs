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
open Nu.NuConstants

[<AutoOpen>]
module InterativityModule =

    /// Describes the current state of the game engine's level of 'interactivity'.
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

    /// The data for an entity collision event.
    type [<StructuralEquality; NoComparison>] EntityCollisionData =
        { Normal : Vector2
          Speed : single
          Collidee : Address }

    /// The data for an entity change event.
    type [<StructuralEquality; NoComparison>] EntityChangeData =
        { OldEntity : Entity }

    /// The data for a user-defined event.
    and [<StructuralEquality; NoComparison>] OtherData =
        { Obj : obj }

    /// The data for an event.
    and [<ReferenceEquality>] EventData =
        | MouseMoveData of MouseMoveData
        | MouseButtonData of MouseButtonData
        | KeyboardKeyData of KeyboardKeyData
        | EntityCollisionData of EntityCollisionData
        | EntityChangeData of EntityChangeData
        | OtherData of OtherData
        | NoData

    /// An event used by Nu's purely functional event system.
    and [<ReferenceEquality>] Event =
        { Name : Address
          Publisher : Address
          Subscriber : Address
          Data : EventData }

    /// Describes whether or not an event has been handled and should therefore no longer be
    /// propagated.
    and EventHandled =
        | Handled
        | Unhandled

    /// Describes a game event subscription.
    and [<ReferenceEquality>] Subscription =
        | ExitSub
        | SwallowSub
        | ScreenTransitionSub of Address (*desinationScreen*)
        | ScreenTransitionFromSplashSub of Address (*desinationScreen*)
        | CustomSub of (Event -> World -> EventHandled * World)

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

    /// Dynamically augments an entity's behavior in a composable way.
    and [<AbstractClass>] Facet () =

        static member getName facet =
            let facetType = facet.GetType ()
            facetType.Name

        static member areFacetsCompatible facet facet2 =
            let facetFieldDefinitions = NuCore.getFieldDefinitionNames facet
            let facet2FieldDefinitions = NuCore.getFieldDefinitionNames facet2
            let intersection = List.intersect facetFieldDefinitions facet2FieldDefinitions
            Set.isEmpty intersection

        abstract member AttachFields : Entity -> Entity
        default facet.AttachFields entity = entity
        
        abstract member DetachFields : Entity -> Entity
        default facet.DetachFields entity = entity

        abstract member RegisterPhysics : Entity * Address * World -> World
        default facet.RegisterPhysics (_, _, world) = world

        abstract member UnregisterPhysics : Entity * Address * World -> World
        default facet.UnregisterPhysics (_, _, world) = world
        
        abstract member PropagatePhysics : Entity * Address * World -> World
        default facet.PropagatePhysics (_, _, world) = world
        
        abstract member HandleBodyTransformMessage : Entity * Address * BodyTransformMessage * World -> World
        default facet.HandleBodyTransformMessage (_, _, _, world) = world
        
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
          OptOverlayName : string option
          Xtension : Xtension
          FacetNames : string list
          FacetsNp : Facet list } // TODO: now that there are field descriptors, consider making their persistence configurable with data instead of name-suffixing.

        static member (?) (this : Entity, memberName) =
            fun args ->
                Xtension.(?) (this.Xtension, memberName) args

        static member (?<-) (this : Entity, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

        static member dispatchesAs dispatcherTargetType entity dispatcherContainer =
            Xtension.dispatchesAs dispatcherTargetType entity.Xtension dispatcherContainer

        static member setPosition position (entity : Entity) =
             { entity with Position = position }

        static member setDepth depth (entity : Entity) =
             { entity with Depth = depth }

        static member setSize size (entity : Entity) =
             { entity with Size = size }

        static member setRotation rotation (entity : Entity) =
             { entity with Rotation = rotation }

        static member setVisible visible (entity : Entity) =
             { entity with Visible = visible }

        static member setFacetNames facets (entity : Entity) =
             { entity with FacetNames = facets }

        static member setFacetsNp facets (entity : Entity) =
             { entity with FacetsNp = facets }

        static member attachFields fieldDefinitions (entity : Entity) =
            let entity = { entity with Id = entity.Id } // hacky copy. Copied in case property is set in-place.
            List.fold
                (fun entity (fieldName, _ : Type, fieldExpr) ->
                    let fieldValue = FieldExpression.eval fieldExpr
                    match typeof<Entity>.GetPropertyWritable fieldName with
                    | null ->
                        let xtension = { entity.Xtension with XFields = Map.add fieldName fieldValue entity.Xtension.XFields }
                        { entity with Xtension = xtension }
                    | property ->
                        property.SetValue (entity, fieldValue)
                        entity)
                entity
                fieldDefinitions

        static member detachFields fieldDefinitions entity =
            List.fold
                (fun entity (fieldName, _ : Type, _ : FieldExpression) ->
                    match typeof<Entity>.GetPropertyWritable fieldName with
                    | null ->
                        let xtension = { entity.Xtension with XFields = Map.remove fieldName entity.Xtension.XFields }
                        { entity with Xtension = xtension }
                    | _ -> entity)
                entity
                fieldDefinitions

        static member isFacetCompatible facet entity =
            let facetFieldNames = NuCore.getFieldDefinitionNames facet
            let entityFieldNames = Map.toKeyList entity.Xtension.XFields
            let intersection = List.intersect facetFieldNames entityFieldNames
            Set.isEmpty intersection

    /// Forms logical groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Group =
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

    /// The screen type that allows transitioning to and fro other screens, and also hosts the
    /// currently interactive groups of entities.
    and [<CLIMutable; StructuralEquality; NoComparison>] Screen =
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

    /// The game type that hosts the various screens used to navigate through a game.
    and [<CLIMutable; StructuralEquality; NoComparison>] Game =
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

    /// The world, in a functional programming sense. Hosts the game object, the dependencies
    /// needed to implement a game, messages to by consumed by the various engine sub-systems,
    /// and general configuration data.
    ///
    /// TODO: see if the size of this type might have a non-trivial impact on performance.
    and [<ReferenceEquality>] World =
        { Game : Game
          Screens : Map<string, Screen>
          Groups : Map<string, Map<string, Group>>
          Entities : Map<string, Map<string, Map<string, Entity>>>
          TickTime : int64
          Liveness : Liveness
          Interactivity : Interactivity
          Camera : Camera
          Tasks : Task list
          Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          AudioPlayer : AudioPlayer
          Renderer : Renderer
          Integrator : Integrator
          AssetMetadataMap : AssetMetadataMap
          Overlayer : Overlayer
          AudioMessages : AudioMessage rQueue
          RenderMessages : RenderMessage rQueue
          PhysicsMessages : PhysicsMessage rQueue
          Dispatchers : XDispatchers
          Facets : Map<string, Facet>
          AssetGraphFileName : string
          OverlayFileName : string
          ExtData : obj }
        interface IXDispatcherContainer with
            member this.GetDispatchers () = this.Dispatchers
            end

    /// Abstracts over the simulation types (Game, Screen, Group, Entity).
    type [<StructuralEquality; NoComparison>] Simulant =
        | Game of Game
        | Screen of Screen
        | Group of Group
        | Entity of Entity

[<RequireQualifiedAccess>]
module EventData =

    // TODO: provide rest of convenience functions

    /// A convenience function to forcibly extract mouse movement data from an event data abstraction.
    let toMouseMoveData data = match data with MouseMoveData d -> d | _ -> failwith <| "Expected MouseMoveData from event data '" + string data + "'."
    
    /// A convenience function to forcibly extract mouse button data from an event data abstraction.
    let toMouseButtonData data = match data with MouseButtonData d -> d | _ -> failwith <| "Expected MouseButtonData from event data '" + string data + "'."
    
    /// A convenience function to forcibly extract entity collision data from an event data abstraction.
    let toEntityCollisionData data = match data with EntityCollisionData d -> d | _ -> failwith <| "Expected EntityCollisionData from event data '" + string data + "'."
    
    /// A convenience function to forcibly extract user-defined data from an event data abstraction.
    let toOtherData data = match data with OtherData d -> d | _ -> failwith <| "Expected OtherData from event data '" + string data + "'."

module World =

    /// Query that the engine is in game-playing mode.
    let isGamePlaying world = Interactivity.isGamePlaying world.Interactivity

    /// Query that the physics system is running.
    let isPhysicsRunning world = Interactivity.isPhysicsRunning world.Interactivity

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

        /// Send a message to the physics system to create a body with the given physics id.
        static member createBody entityAddress physicsId position rotation bodyProperties world =
            let createBodyMessage = CreateBodyMessage { EntityAddress = entityAddress; PhysicsId = physicsId; Position = position; Rotation = rotation; BodyProperties = bodyProperties }
            { world with PhysicsMessages = createBodyMessage :: world.PhysicsMessages }

        /// Send a message to the physics system to destroy a body with the given physics id.
        static member destroyBody physicsId world =
            let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
            { world with PhysicsMessages = destroyBodyMessage :: world.PhysicsMessages }

        /// Send a message to the physics system to set the position of a body with the given physics id.
        static member setPosition position physicsId world =
            let setPositionMessage = SetPositionMessage { PhysicsId = physicsId; Position = position }
            { world with PhysicsMessages = setPositionMessage :: world.PhysicsMessages }

        /// Send a message to the physics system to set the rotation of a body with the given physics id.
        static member setRotation rotation physicsId world =
            let setRotationMessage = SetRotationMessage { PhysicsId = physicsId; Rotation = rotation }
            { world with PhysicsMessages = setRotationMessage :: world.PhysicsMessages }

        /// Send a message to the physics system to set the linear velocity of a body with the given physics id.
        static member setLinearVelocity linearVelocity physicsId world =
            let setLinearVelocityMessage = SetLinearVelocityMessage { PhysicsId = physicsId; LinearVelocity = linearVelocity }
            { world with PhysicsMessages = setLinearVelocityMessage :: world.PhysicsMessages }

        /// Send a message to the physics system to apply linear impulse to a body with the given physics id.
        static member applyLinearImpulse linearImpulse physicsId world =
            let applyLinearImpulseMessage = ApplyLinearImpulseMessage { PhysicsId = physicsId; LinearImpulse = linearImpulse }
            { world with PhysicsMessages = applyLinearImpulseMessage :: world.PhysicsMessages }

        /// Send a message to the physics system to apply force to a body with the given physics id.
        static member applyForce force physicsId world =
            let applyForceMessage = ApplyForceMessage { PhysicsId = physicsId; Force = force }
            { world with PhysicsMessages = applyForceMessage :: world.PhysicsMessages }

[<AutoOpen>]
module WorldRenderingModule =

    type World with

        /// Hint that a rendering asset package with the given name should be loaded. Should be
        /// used to avoid loading assets at inconvenient times (such as in the middle of game play!)
        static member hintRenderingPackageUse packageName world =
            let hintRenderingPackageUseMessage = HintRenderingPackageUseMessage { PackageName = packageName }
            { world with RenderMessages = hintRenderingPackageUseMessage :: world.RenderMessages }
            
        /// Hint that a rendering package should be unloaded since its assets will not be used
        /// again (or until specified via World.hintRenderingPackageUse).
        static member hintRenderingPackageDisuse packageName world =
            let hintRenderingPackageDisuseMessage = HintRenderingPackageDisuseMessage { PackageName = packageName }
            { world with RenderMessages = hintRenderingPackageDisuseMessage :: world.RenderMessages }
            
        /// Send a message to the renderer to reload its rendering assets.
        static member reloadRenderingAssets world =
            let reloadRenderingAssetsMessage = ReloadRenderingAssetsMessage
            { world with RenderMessages = reloadRenderingAssetsMessage :: world.RenderMessages }

[<AutoOpen>]
module WorldAudioModule =

    type World with

        /// Send a message to the audio system to play a song.
        static member playSong song volume timeToFadeOutSongMs world =
            let playSongMessage = PlaySongMessage { Song = song; Volume = volume; TimeToFadeOutSongMs = timeToFadeOutSongMs }
            { world with AudioMessages = playSongMessage :: world.AudioMessages }

        /// Send a message to the audio system to play a song.
        static member playSong6 songAssetName packageName volume timeToFadeOutSongMs world =
            let song = { SongAssetName = songAssetName; PackageName = packageName }
            World.playSong song volume timeToFadeOutSongMs world

        /// Send a message to the audio system to play a sound.
        static member playSound sound volume world =
            let playSoundMessage = PlaySoundMessage { Sound = sound; Volume = volume }
            { world with AudioMessages = playSoundMessage :: world.AudioMessages }

        /// Send a message to the audio system to play a sound.
        static member playSound5 soundAssetName packageName volume world =
            let sound = { SoundAssetName = soundAssetName; PackageName = packageName }
            World.playSound sound volume world

        /// Send a message to the audio system to fade out a song.
        static member fadeOutSong timeToFadeOutSongMs world =
            let fadeOutSongMessage = FadeOutSongMessage timeToFadeOutSongMs
            { world with AudioMessages = fadeOutSongMessage :: world.AudioMessages }

        /// Send a message to the audio system to stop a song.
        static member stopSong world =
            { world with AudioMessages = StopSongMessage :: world.AudioMessages }
            
        /// Hint that an audio asset package with the given name should be loaded. Should be used
        /// to avoid loading assets at inconvenient times (such as in the middle of game play!)
        static member hintAudioPackageUse packageName world =
            let hintAudioPackageUseMessage = HintAudioPackageUseMessage { PackageName = packageName }
            { world with AudioMessages = hintAudioPackageUseMessage :: world.AudioMessages }
            
        /// Hint that an audio package should be unloaded since its assets will not be used again
        /// (or until specified via a HintAudioPackageUseMessage).
        static member hintAudioPackageDisuse packageName world =
            let hintAudioPackageDisuseMessage = HintAudioPackageDisuseMessage { PackageName = packageName }
            { world with AudioMessages = hintAudioPackageDisuseMessage :: world.AudioMessages }

        /// Send a message to the audio player to reload its audio assets.
        static member reloadAudioAssets world =
            let reloadAudioAssetsMessage = ReloadAudioAssetsMessage
            { world with AudioMessages = reloadAudioAssetsMessage :: world.AudioMessages }

[<RequireQualifiedAccess>]
module Sim =

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

    let withSimulant getSimulant setSimulant fn address world : World =
        let simulant = getSimulant address world
        let simulant = fn simulant
        setSimulant address simulant world

    let withSimulantAndWorld getSimulant setSimulant fn address world : World =
        let simulant = getSimulant address world
        let (simulant, world) = fn simulant
        setSimulant address simulant world

    let tryWithSimulant getOptSimulant setSimulant fn address world : World =
        let optSimulant = getOptSimulant address world
        match optSimulant with
        | Some simulant ->
            let simulant = fn simulant
            setSimulant address simulant world
        | None -> world

    let tryWithSimulantAndWorld getOptSimulant setSimulant fn address world : World =
        let optSimulant = getOptSimulant address world
        match optSimulant with
        | Some simulant ->
            let (simulant, world) = fn simulant
            setSimulant address simulant world
        | None -> world