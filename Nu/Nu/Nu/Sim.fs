// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.
namespace Nu
open System
open System.Collections.Generic
open System.Reflection
open System.Xml
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.NuCore
open Nu.NuMath

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

[<AutoOpen>]
module SimModule =

    /// Describes data relevant to specific event messages.
    type [<ReferenceEquality>] MessageData =
        | MouseMoveData of Vector2
        | MouseButtonData of Vector2 * MouseButton
        | CollisionData of Vector2 * single * Address
        | OtherData of obj
        | NoData

    /// A generic message for the Nu game engine.
    /// A reference type.
    type [<ReferenceEquality>] Message =
        { Handled : bool
          Data : MessageData }

    type [<CLIMutable; StructuralEquality; NoComparison>] Entity =
        { Id : Guid
          Name : string
          Enabled : bool
          Visible : bool
          FacetNamesNs : string list // Marked with 'Ns' (Non-serializable) for now as I've yet to make F# lists serializable.
          Xtension : Xtension }

        static member (?) (this : Entity, memberName) =
            fun args ->
                Xtension.(?) (this, this.Xtension, memberName) args

        static member (?<-) (this : Entity, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    // TODO: move this tile map stuff elsewhere

    type [<StructuralEquality; NoComparison>] TileMapData =
        { Map : TmxMap
          MapSize : int * int
          TileSize : int * int
          TileSizeF : Vector2
          TileMapSize : int * int
          TileMapSizeF : Vector2
          TileSet : TmxTileset
          TileSetSize : int * int }
      
    type [<StructuralEquality; NoComparison>] TileLayerData =
        { Layer : TmxLayer
          Tiles : TmxLayerTile List }
      
    type [<StructuralEquality; NoComparison>] TileData =
        { Tile : TmxLayerTile
          I : int
          J : int
          Gid : int
          GidPosition : int
          Gid2 : int * int
          OptTileSetTile : TmxTilesetTile option
          TilePosition : int * int }

    type [<CLIMutable; StructuralEquality; NoComparison>] Group =
        { Id : Guid
          FacetNamesNs : string list
          Xtension : Xtension }

        static member (?) (this : Group, memberName) =
            fun args ->
                Xtension.(?) (this, this.Xtension, memberName) args

        static member (?<-) (this : Group, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    type GroupDescriptor =
        string * Group * Entity list

    type [<StructuralEquality; NoComparison>] TransitionType =
        | Incoming
        | Outgoing

    type [<CLIMutable; StructuralEquality; NoComparison>] Transition =
        { Id : Guid
          Lifetime : int
          Ticks : int
          Type : TransitionType
          OptDissolveSprite : Sprite option
          FacetNamesNs : string list
          Xtension : Xtension }

        static member (?) (this : Transition, memberName) =
            fun args ->
                Xtension.(?) (this, this.Xtension, memberName) args

        static member (?<-) (this : Transition, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    type [<StructuralEquality; NoComparison>] ScreenState =
        | IncomingState
        | OutgoingState
        | IdlingState

    type [<CLIMutable; StructuralEquality; NoComparison>] Screen =
        { Id : Guid
          State : ScreenState
          Incoming : Transition
          Outgoing : Transition
          FacetNamesNs : string list
          Xtension : Xtension }

        static member (?) (this : Screen, memberName) =
            fun args ->
                Xtension.(?) (this, this.Xtension, memberName) args

        static member (?<-) (this : Screen, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    type [<CLIMutable; StructuralEquality; NoComparison>] Game =
        { Id : Guid
          OptSelectedScreenAddress : Address option
          FacetNamesNs : string list
          Xtension : Xtension }

        static member (?) (this : Game, memberName) =
            fun args ->
                Xtension.(?) (this, this.Xtension, memberName) args

        static member (?<-) (this : Game, memberName, value) =
            let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    type [<StructuralEquality; NoComparison>] Simulant =
        | Game of Game
        | Screen of Screen
        | Group of Group
        | Entity of Entity

    /// Describes a game message subscription.
    /// In addition to CustomSubs, allows for data-driven subscriptions to accomodate a visual event editor should
    /// one be implemented.
    /// A reference type.
    type [<ReferenceEquality>] Subscription =
        | ExitSub
        | SwallowSub
        | ScreenTransitionSub of Address (*desinationScreen*)
        | CustomSub of (Address (*event*) -> Address (*publisher*) -> Address (*subscriber*) -> Message -> World -> (Message * bool * World))

    /// A map of game message subscriptions.
    /// A reference type due to the reference-typeness of Subscription.
    and Subscriptions = Map<Address, (Address * Subscription) list>

    /// The world, in a functional programming sense.
    /// A reference type with some value semantics.
    and [<ReferenceEquality>] World =
        { Game : Game
          Screens : Map<string, Screen>
          Groups : Map<string, Map<string, Group>>
          Entities : Map<string, Map<string, Map<string, Entity>>>
          Ticks : uint64
          Camera : Camera
          Subscriptions : Subscriptions
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

module Sim =

    let handleMessage message =
        { Handled = true; Data = message.Data }

    let mouseToScreen (position : Vector2) camera =
        let positionScreen =
            Vector2 (
                position.X - camera.EyeSize.X * 0.5f,
                -(position.Y - camera.EyeSize.Y * 0.5f)) // negation for right-handedness
        positionScreen

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

    let withSimulant worldSimulantLens fn address (world : World) =
        let simulant = get world <| worldSimulantLens address
        let simulant' = fn simulant
        set simulant' world <| worldSimulantLens address

    let withSimulantAndWorld worldSimulantLens fn address (world : World) =
        let simulant = get world <| worldSimulantLens address
        let (simulant', world') = fn simulant
        set simulant' world' <| worldSimulantLens address

    let withOptSimulant worldOptSimulantLens fn address (world : World) =
        let optSimulant = get world <| worldOptSimulantLens address
        let optSimulant' = fn optSimulant
        set optSimulant' world <| worldOptSimulantLens address

    let withOptSimulantAndWorld worldOptSimulantLens fn address (world : World) =
        let optSimulant = get world <| worldOptSimulantLens address
        let (optSimulant', world') = fn optSimulant
        set optSimulant' world' <| worldOptSimulantLens address

    let tryWithSimulant worldOptSimulantLens worldSimulantLens fn address (world : World) =
        let optSimulant = get world <| worldOptSimulantLens address
        match optSimulant with
        | None -> world
        | Some simulant ->
            let simulant' = fn simulant
            set simulant' world <| worldSimulantLens address

    let tryWithSimulantAndWorld worldOptSimulantLens worldSimulantLens fn address (world : World) =
        let optSimulant = get world <| worldOptSimulantLens address
        match optSimulant with
        | None -> world
        | Some simulant ->
            let (simulant', world') = fn simulant
            set simulant' world' <| worldSimulantLens address