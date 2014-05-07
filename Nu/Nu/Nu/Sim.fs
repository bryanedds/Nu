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
          Xtension : Xtension }

        static member (?) (this : Entity, memberName) =
            fun args ->
                (?) this.Xtension memberName args

        static member (?<-) (this : Entity, memberName, value) =
            let xtension = Xtension.op_DynamicAssignment (this.Xtension, memberName, value)
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
          Xtension : Xtension }

        static member (?) (this : Group, memberName) =
            fun args ->
                (?) this.Xtension memberName args

        static member (?<-) (this : Group, memberName, value) =
            let xtension = Xtension.op_DynamicAssignment (this.Xtension, memberName, value)
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
          Xtension : Xtension }

        static member (?) (this : Transition, memberName) =
            fun args ->
                (?) this.Xtension memberName args

        static member (?<-) (this : Transition, memberName, value) =
            let xtension = Xtension.op_DynamicAssignment (this.Xtension, memberName, value)
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
          Xtension : Xtension }

        static member (?) (this : Screen, memberName) =
            fun args ->
                (?) this.Xtension memberName args

        static member (?<-) (this : Screen, memberName, value) =
            let xtension = Xtension.op_DynamicAssignment (this.Xtension, memberName, value)
            { this with Xtension = xtension }

    type [<CLIMutable; StructuralEquality; NoComparison>] Game =
        { Id : Guid
          OptSelectedScreenAddress : Address option
          Xtension : Xtension }

        static member (?) (this : Game, memberName) =
            fun args ->
                (?) this.Xtension memberName args

        static member (?<-) (this : Game, memberName, value) =
            let xtension = Xtension.op_DynamicAssignment (this.Xtension, memberName, value)
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
          Dispatchers : IXDispatchers
          ExtData : obj } // TODO: consider if this is still the right approach in the context of the new Xtension stuff

        interface IXDispatcherContainer with
            member this.GetDispatchers () = this.Dispatchers
            end

    type Entity with

        (* XFields *)
        [<XField>] member this.Position with get () = this?Position () : Vector2
        member this.SetPosition (value : Vector2) : Entity = this?Position <- value
        [<XField>] member this.Depth with get () = this?Depth () : single
        member this.SetDepth (value : single) : Entity = this?Depth <- value
        [<XField>] member this.Rotation with get () = this?Rotation () : single
        member this.SetRotation (value : single) : Entity = this?Rotation <- value
        [<XField>] member this.Size with get () = this?Size () : Vector2
        member this.SetSize (value : Vector2) : Entity = this?Size <- value

        (* XDispatches *)
        member this.Init (dispatcherContainer : IXDispatcherContainer) : Entity = this?Init (this, dispatcherContainer)
        member this.Register (address : Address, world : World) : Entity * World = this?Register (address, this, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, this, world)
        member this.PropagatePhysics (address : Address, world : World) : World = this?PropagatePhysics (address, this, world)
        member this.ReregisterPhysicsHack (address : Address, world : World) : World = this?ReregisterPhysicsHack (address, this, world)
        member this.HandleBodyTransformMessage (message : BodyTransformMessage, address : Address, world : World) : World = this?HandleBodyTransformMessage (message, address, this, world)
        member this.GetRenderDescriptors (viewAbsolute : Matrix3, viewRelative : Matrix3, world : World) : RenderDescriptor list = this?GetRenderDescriptors (viewAbsolute, viewRelative, this, world)
        member this.GetQuickSize (world : World) : Vector2 = this?GetQuickSize (this, world)
        member this.IsTransformRelative (world : World) : bool = this?IsTransformRelative (this, world)

    type Group with
        member this.Register (address : Address, entities : Entity list, world : World) : World = this?Register (address, this, entities, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, this, world)

    type Transition with
        end

    type Screen with
        member this.Register (address : Address, groupDescriptors : GroupDescriptor list, world : World) : World = this?Register (address, this, groupDescriptors, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, this, world)

    type Game with
        member this.Register (world : World) : World = this?Register (this, world)

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

    let activateGameDispatcher assemblyFileName gameDispatcherFullName world =
        let assembly = Assembly.LoadFrom assemblyFileName
        let gameDispatcherType = assembly.GetType gameDispatcherFullName
        let gameDispatcherShortName = gameDispatcherType.Name
        let gameDispatcher = Activator.CreateInstance gameDispatcherType
        let dispatchers = Map.add gameDispatcherShortName gameDispatcher world.Dispatchers
        let world' = { world with Dispatchers = dispatchers }
        let world'' = { world' with Game = { world'.Game with Xtension = { world'.Game.Xtension with OptXTypeName = Some gameDispatcherShortName }}}
        world''.Game.Register world''