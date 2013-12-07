namespace Nu
open System
open System.Collections.Generic
open SDL2
open OpenTK
open TiledSharp
open Nu.Core

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

type [<StructuralEquality; NoComparison; CLIMutable>] Entity =
    { Id : Id
      Name : string
      Enabled : bool
      Visible : bool }

type [<StructuralEquality; NoComparison; CLIMutable>] Gui =
    { Entity : Entity
      Position : Vector2
      Depth : single
      Size : Vector2 }

type [<StructuralEquality; NoComparison; CLIMutable>] Button =
    { Gui : Gui
      IsDown : bool
      UpSprite : Sprite
      DownSprite : Sprite
      ClickSound : Sound }

type [<StructuralEquality; NoComparison; CLIMutable>] Label =
    { Gui : Gui
      LabelSprite : Sprite }

type [<StructuralEquality; NoComparison; CLIMutable>] TextBox =
    { Gui : Gui
      BoxSprite : Sprite
      Text : string
      TextFont : Font
      TextOffset : Vector2
      TextColor : Vector4 }

type [<StructuralEquality; NoComparison; CLIMutable>] Toggle =
    { Gui : Gui
      IsOn : bool
      IsPressed : bool
      OffSprite : Sprite
      OnSprite : Sprite
      ToggleSound : Sound }

type [<StructuralEquality; NoComparison; CLIMutable>] Feeler =
    { Gui : Gui
      IsTouched : bool }

type [<StructuralEquality; NoComparison; CLIMutable>] Actor =
    { Entity : Entity
      Position : Vector2
      Depth : single
      Size : Vector2
      Rotation : single }
      
type [<StructuralEquality; NoComparison; CLIMutable>] Block =
    { Actor : Actor
      PhysicsId : Id
      Density : single
      BodyType : BodyType
      Sprite : Sprite }

type [<StructuralEquality; NoComparison; CLIMutable>] Avatar =
    { Actor : Actor
      PhysicsId : Id
      Density : single
      Sprite : Sprite }
      
type [<StructuralEquality; NoComparison; CLIMutable>] TileMap =
    { Actor : Actor
      PhysicsIds : Id list
      Density : single
      TileMapAsset : TileMapAsset
      TmxMap : TmxMap
      TileMapSprites : Sprite list }

type [<StructuralEquality; NoComparison>] EntityModel =
    | Button of Button
    | Label of Label
    | TextBox of TextBox
    | Toggle of Toggle
    | Feeler of Feeler
    | Block of Block
    | Avatar of Avatar
    | TileMap of TileMap

type [<StructuralEquality; NoComparison>] TileMapData =
    { Map : TmxMap
      MapSize : int * int
      TileSize : int * int
      TileSizeF : Vector2
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
      TilePosition : int * int
      TileSetPosition : int * int }

type [<StructuralEquality; NoComparison; CLIMutable>] Group =
    { Id : Id }

type [<StructuralEquality; NoComparison; CLIMutable>] OmniFieldGroup =
    { Group : Group }

type [<StructuralEquality; NoComparison; CLIMutable>] OmniBattleGroup =
    { Group : Group }

type [<StructuralEquality; NoComparison>] GroupModel =
    | Group of Group
    | OmniFieldGroup of OmniFieldGroup
    | OmniBattleGroup of OmniBattleGroup

type [<StructuralEquality; NoComparison>] TransitionType =
    | Incoming
    | Outgoing

type [<StructuralEquality; NoComparison; CLIMutable>] Transition =
    { Id : Id
      Lifetime : int
      Ticks : int
      Type : TransitionType }

type [<StructuralEquality; NoComparison; CLIMutable>] Dissolve =
    { Transition : Transition
      Sprite : Sprite }

type [<StructuralEquality; NoComparison>] TransitionModel =
    | Transition of Transition
    | Dissolve of Dissolve

type [<StructuralEquality; NoComparison>] ScreenState =
    | IncomingState
    | OutgoingState
    | IdlingState

type [<StructuralEquality; NoComparison; CLIMutable>] Screen =
    { Id : Id
      State : ScreenState
      IncomingModel : TransitionModel
      OutgoingModel : TransitionModel }

type [<StructuralEquality; NoComparison; CLIMutable>] OmniBattleScreen =
    { Screen : Screen
      Battle : OmniBattle }

type [<StructuralEquality; NoComparison>] ScreenModel =
    | Screen of Screen
    | OmniBattleScreen of OmniBattleScreen

type [<StructuralEquality; NoComparison; CLIMutable>] Game =
    { Id : Id
      OptSelectedScreenModelAddress : Address option }

type [<StructuralEquality; NoComparison; CLIMutable>] OmniGame =
    { Game : Game
      OptPlayer : OmniPlayer option }
        
type [<StructuralEquality; NoComparison>] GameModel =
    | Game of Game
    | OmniGame of OmniGame

/// Describes a game message subscription.
/// A reference type.
type [<ReferenceEquality>] Subscription =
    Subscription of (Address -> Address -> Message -> World -> (Message * bool * World))

/// A map of game message subscriptions.
/// A reference type due to the reference-typeness of Subscription.
and Subscriptions = Map<Address, (Address * Subscription) list>

/// The world, in a functional programming sense.
/// A reference type with some value semantics.
and [<ReferenceEquality>] World =
    { GameModel : GameModel
      ScreenModels : Map<Lun, ScreenModel>
      GroupModels : Map<Lun, Map<Lun, GroupModel>>
      EntityModels : Map<Lun, Map<Lun, Map<Lun, EntityModel>>>
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
      Components : IWorldComponent list
      ExtData : obj }

/// Enables components that open the world for extension.
and IWorldComponent =
    interface
        abstract member GetAudioDescriptors : World -> AudioDescriptor list
        abstract member GetRenderDescriptors : World -> RenderDescriptor list
        // TODO: abstract member GetRenderMessages : World -> RenderMessage rQueue
        // TODO: abstract member GetPhysicsMessages : World -> PhysicsMessage rQueue
        // TODO: abstract member HandleIntegrationMessages : IntegrationMessage rQueue -> World -> World
        end

type [<StructuralEquality; NoComparison>] Simulant =
    | EntityModel of EntityModel
    | GroupModel of GroupModel
    | ScreenModel of ScreenModel
    | GameModel of GameModel