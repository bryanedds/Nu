namespace Nu
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
open Nu.Metadata
open Nu.Audio
open Nu.Sdl
open Nu.DomainModel
open Nu.Entities
open Nu.Groups
open Nu.Screens
open Nu.Games
open Nu.CameraModule

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

type [<StructuralEquality; NoComparison>] Simulant =
    | EntityModel of EntityModel
    | GroupModel of GroupModel
    | ScreenModel of ScreenModel
    | GameModel of GameModel

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
      WScreenModels : Map<Lun, ScreenModel> // TODO: remove 'W'
      WGroupModels : Map<Lun, Map<Lun, GroupModel>> // TODO: remove 'W'
      WEntityModels : Map<Lun, Map<Lun, Map<Lun, EntityModel>>> // TODO: remove 'W'
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
    
module Sim =

    let TickAddress = addr "tick"
    let MouseDragAddress = addr "mouse/drag"
    let MouseMoveAddress = addr "mouse/move"
    let MouseLeftAddress = addr "mouse/left"
    let MouseCenterAddress = addr "mouse/center"
    let MouseRightAddress = addr "mouse/right"
    let DownMouseLeftAddress = straddr "down" MouseLeftAddress
    let DownMouseCenterAddress = straddr "down" MouseCenterAddress
    let DownMousRightAddress = straddr "down" MouseRightAddress
    let UpMouseLeftAddress = straddr "up" MouseLeftAddress
    let UpMouseCenterAddress = straddr "up" MouseCenterAddress
    let UpMouseRightAddress = straddr "up" MouseRightAddress
    let GameModelPublishingPriority = Single.MaxValue
    let ScreenModelPublishingPriority = GameModelPublishingPriority * 0.5f
    let GroupModelPublishingPriority = ScreenModelPublishingPriority * 0.5f
    let FinishedIncomingAddressPart = addr "finished/incoming"
    let FinishedOutgoingAddressPart = addr "finished/outgoing"
    let FieldFeelerName = Lun.make "feeler"
    let FieldAvatarName = Lun.make "avatar"

    // Entity Lenses ////////////////////////////////////////////////////////////////

    let getGuiTransform (gui : Gui) =
        { Transform.Position = gui.Position
          Depth = gui.Depth
          Size = gui.Size
          Rotation = 0.0f }

    let getActorTransform (actor : Actor) =
        { Transform.Position = actor.Position
          Depth = actor.Depth
          Size = actor.Size
          Rotation = actor.Rotation }

    let getActorTransformRelative (view : Vector2) (actor : Actor) =
        { Transform.Position = actor.Position - view
          Depth = actor.Depth
          Size = actor.Size
          Rotation = actor.Rotation }

    let getEntityModelQuickSize assetMetadataMap entityModel =
        match entityModel with
        | Button button -> getTextureSizeAsVector2 button.UpSprite.SpriteAssetName button.UpSprite.PackageName assetMetadataMap
        | Label label -> getTextureSizeAsVector2 label.LabelSprite.SpriteAssetName label.LabelSprite.PackageName assetMetadataMap
        | TextBox textBox -> getTextureSizeAsVector2 textBox.BoxSprite.SpriteAssetName textBox.BoxSprite.PackageName assetMetadataMap
        | Toggle toggle -> getTextureSizeAsVector2 toggle.OffSprite.SpriteAssetName toggle.OnSprite.PackageName assetMetadataMap
        | Feeler feeler -> Vector2 64.0f
        | Block block -> getTextureSizeAsVector2 block.Sprite.SpriteAssetName block.Sprite.PackageName assetMetadataMap
        | Avatar avatar -> getTextureSizeAsVector2 avatar.Sprite.SpriteAssetName avatar.Sprite.PackageName assetMetadataMap
        | TileMap tileMap -> Vector2 (single <| tileMap.TmxMap.Width * tileMap.TmxMap.TileWidth, single <| tileMap.TmxMap.Height * tileMap.TmxMap.TileHeight)

    let entityLens =
        { Get = fun entityModel ->
            match entityModel with
            | Button button -> button.Gui.Entity
            | Label label -> label.Gui.Entity
            | TextBox textBox -> textBox.Gui.Entity
            | Toggle toggle -> toggle.Gui.Entity
            | Feeler feeler -> feeler.Gui.Entity
            | Block block -> block.Actor.Entity
            | Avatar avatar -> avatar.Actor.Entity
            | TileMap tileMap -> tileMap.Actor.Entity
          Set = fun entity entityModel ->
            match entityModel with
            | Button button -> Button { button with Gui = { button.Gui with Entity = entity }}
            | Label label -> Label { label with Gui = { label.Gui with Entity = entity }}
            | TextBox textBox -> TextBox { textBox with Gui = { textBox.Gui with Entity = entity }}
            | Toggle toggle -> Toggle { toggle with Gui = { toggle.Gui with Entity = entity }}
            | Feeler feeler -> Feeler { feeler with Gui = { feeler.Gui with Entity = entity }}
            | Block block -> Block { block with Actor = { block.Actor with Entity = entity }}
            | Avatar avatar -> Avatar { avatar with Actor = { avatar.Actor with Entity = entity }}
            | TileMap tileMap -> TileMap { tileMap with Actor = { tileMap.Actor with Entity = entity }}}

    let entityIdLens =
        { Get = fun entityModel -> (get entityModel entityLens).Id
          Set = fun value entityModel -> set { get entityModel entityLens with Id = value } entityModel entityLens}

    let entityNameLens =
        { Get = fun entityModel -> (get entityModel entityLens).Name
          Set = fun value entityModel -> set { get entityModel entityLens with Name = value } entityModel entityLens}

    let entityEnabledLens =
        { Get = fun entityModel -> (get entityModel entityLens).Enabled
          Set = fun value entityModel -> set { get entityModel entityLens with Enabled = value } entityModel entityLens}

    let entityVisibleLens =
        { Get = fun entityModel -> (get entityModel entityLens).Visible
          Set = fun value entityModel -> set { get entityModel entityLens with Visible = value } entityModel entityLens}

    let optGuiLens =
        { Get = fun entityModel ->
            match entityModel with
            | Button button -> Some button.Gui
            | Label label -> Some label.Gui
            | TextBox textBox -> Some textBox.Gui
            | Toggle toggle -> Some toggle.Gui
            | Feeler feeler -> Some feeler.Gui
            | Block _ | Avatar _ | TileMap _ -> None
          Set = fun optGui entityModel ->
            let gui = Option.get optGui
            match entityModel with
            | Button button -> Button { button with Gui = gui }
            | Label label -> Label { label with Gui = gui }
            | TextBox textBox -> TextBox { textBox with Gui = gui }
            | Toggle toggle -> Toggle { toggle with Gui = gui }
            | Feeler feeler -> Feeler { feeler with Gui = gui }
            | Block _ | Avatar _ | TileMap _ -> failwith "Entity is not a gui." }

    let guiLens =
        { Get = fun entityModel -> Option.get (get entityModel optGuiLens)
          Set = fun gui entityModel -> set (Some gui) entityModel optGuiLens }

    let guiSep (gui : Gui) =
        (gui, gui.Entity)
    
    let guiCmb (gui : Gui, entity) =
        { gui with Entity = entity }

    let optButtonLens =
        { Get = fun entityModel -> match entityModel with Button button -> Some button | _ -> None
          Set = fun optButton entityModel -> Button <| Option.get optButton }

    let buttonLens =
        { Get = fun entityModel -> Option.get (get entityModel optButtonLens)
          Set = fun button entityModel -> set (Some button) entityModel optButtonLens }

    let buttonSep (button : Button) =
        (button, button.Gui, button.Gui.Entity)
    
    let buttonCmb (button : Button, gui, entity) =
        { button with Gui = { gui with Entity = entity }}

    let optLabelLens =
        { Get = fun entityModel -> match entityModel with Label label -> Some label | _ -> None
          Set = fun optLabel entityModel -> Label <| Option.get optLabel }

    let labelLens =
        { Get = fun entityModel -> Option.get (get entityModel optLabelLens)
          Set = fun label entityModel -> set (Some label) entityModel optLabelLens }

    let labelSep (label : Label) =
        (label, label.Gui, label.Gui.Entity)
    
    let labelCmb (label : Label, gui, entity) =
        { label with Gui = { gui with Entity = entity }}

    let optTextBoxLens =
        { Get = fun entityModel -> match entityModel with TextBox textBox -> Some textBox | _ -> None
          Set = fun optTextBox entityModel -> TextBox <| Option.get optTextBox }

    let textBoxLens =
        { Get = fun entityModel -> Option.get (get entityModel optTextBoxLens)
          Set = fun textBox entityModel -> set (Some textBox) entityModel optTextBoxLens }

    let textBoxSep (textBox : TextBox) =
        (textBox, textBox.Gui, textBox.Gui.Entity)
    
    let textBoxCmb (textBox : TextBox, gui, entity) =
        { textBox with Gui = { gui with Entity = entity }}

    let optToggleLens =
        { Get = fun entityModel -> match entityModel with Toggle toggle -> Some toggle | _ -> None
          Set = fun optToggle entityModel -> Toggle <| Option.get optToggle }

    let toggleLens =
        { Get = fun entityModel -> Option.get (get entityModel optToggleLens)
          Set = fun toggle entityModel -> set (Some toggle) entityModel optToggleLens }

    let toggleSep (toggle : Toggle) =
        (toggle, toggle.Gui, toggle.Gui.Entity)
    
    let toggleCmb (toggle : Toggle, gui, entity) =
        { toggle with Gui = { gui with Entity = entity }}

    let optFeelerLens =
        { Get = fun entityModel -> match entityModel with Feeler feeler -> Some feeler | _ -> None
          Set = fun optFeeler entityModel -> Feeler <| Option.get optFeeler }

    let feelerLens =
        { Get = fun entityModel -> Option.get (get entityModel optFeelerLens)
          Set = fun feeler entityModel -> set (Some feeler) entityModel optFeelerLens }

    let feelerSep (feeler : Feeler) =
        (feeler, feeler.Gui, feeler.Gui.Entity)
    
    let feelerCmb (feeler : Feeler, gui, entity) =
        { feeler with Gui = { gui with Entity = entity }}

    let optActorLens =
        { Get = fun entityModel ->
            match entityModel with
            | Button _
            | Label _
            | TextBox _
            | Toggle _
            | Feeler _ -> None
            | Block block -> Some block.Actor
            | Avatar avatar -> Some avatar.Actor
            | TileMap tileMap -> Some tileMap.Actor
          Set = fun optActor entityModel ->
            let actor = Option.get optActor
            match entityModel with
            | Button _
            | Label _
            | TextBox _
            | Toggle _
            | Feeler _ -> failwith "EntityModel is not an actor."
            | Block block -> Block { block with Actor = actor }
            | Avatar avatar -> Avatar { avatar with Actor = actor }
            | TileMap tileMap -> TileMap { tileMap with Actor = actor }}

    let actorLens =
        { Get = fun entityModel -> Option.get (get entityModel optActorLens)
          Set = fun actor entityModel -> set (Some actor) entityModel optActorLens }
        
    let actorSep (actor : Actor) =
        actor.Entity
    
    let actorCmb (actor : Actor, entity) =
        { actor with Entity = entity }

    let optBlockLens =
        { Get = fun entityModel -> match entityModel with Block block -> Some block | _ -> None
          Set = fun optBlock entityModel -> Block <| Option.get optBlock }

    let blockLens =
        { Get = fun entityModel -> Option.get (get entityModel optBlockLens)
          Set = fun block entityModel -> set (Some block) entityModel optBlockLens }

    let blockSep (block : Block) =
        (block, block.Actor, block.Actor.Entity)
    
    let blockCmb (block : Block, actor, entity) =
        { block with Actor = { actor with Entity = entity }}

    let optAvatarLens =
        { Get = fun entityModel -> match entityModel with Avatar avatar -> Some avatar | _ -> None
          Set = fun optAvatar entityModel -> Avatar <| Option.get optAvatar }

    let avatarLens =
        { Get = fun entityModel -> Option.get (get entityModel optAvatarLens)
          Set = fun avatar entityModel -> set (Some avatar) entityModel optAvatarLens }

    let avatarSep (avatar : Avatar) =
        (avatar, avatar.Actor, avatar.Actor.Entity)
    
    let avatarCmb (avatar : Avatar, actor, entity) =
        { avatar with Actor = { actor with Entity = entity }}

    let optTileMapLens =
        { Get = fun entityModel -> match entityModel with TileMap tileMap -> Some tileMap | _ -> None
          Set = fun optTileMap entityModel -> TileMap <| Option.get optTileMap }

    let tileMapLens =
        { Get = fun entityModel -> Option.get (get entityModel optTileMapLens)
          Set = fun tileMap entityModel -> set (Some tileMap) entityModel optTileMapLens }

    let tileMapSep (tileMap : TileMap) =
        (tileMap, tileMap.Actor, tileMap.Actor.Entity)
    
    let tileMapCmb (tileMap : TileMap, actor, entity) =
        { tileMap with Actor = { actor with Entity = entity }}
        
    let worldOptEntityModelFinder (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.WEntityModels
        match optGroupMap with
        | None -> None
        | Some groupMap ->
            let optEntityMap = Map.tryFind address.[1] groupMap
            match optEntityMap with
            | None -> None
            | Some entityMap -> Map.tryFind address.[2] entityMap

    let worldEntityModelAdder (address : Address) world (child : EntityModel) =
        let optGroupMap = Map.tryFind address.[0] world.WEntityModels
        match optGroupMap with
        | None ->
            let entityMap = Map.singleton address.[2] child
            let groupMap = Map.singleton address.[1] entityMap
            { world with WEntityModels = Map.add address.[0] groupMap world.WEntityModels }
        | Some groupMap ->
            let optEntityMap = Map.tryFind address.[1] groupMap
            match optEntityMap with
            | None ->
                let entityMap = Map.singleton address.[2] child
                let groupMap' = Map.add address.[1] entityMap groupMap
                { world with WEntityModels = Map.add address.[0] groupMap' world.WEntityModels }
            | Some entityMap ->
                let entityMap' = Map.add address.[2] child entityMap
                let groupMap' = Map.add address.[1] entityMap' groupMap
                { world with WEntityModels = Map.add address.[0] groupMap' world.WEntityModels }

    let worldEntityModelRemover (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.WEntityModels
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let optEntityMap = Map.tryFind address.[1] groupMap
            match optEntityMap with
            | None -> world
            | Some entityMap ->
                let entityMap' = Map.remove address.[2] entityMap
                let groupMap' = Map.add address.[1] entityMap' groupMap
                { world with WEntityModels = Map.add address.[0] groupMap' world.WEntityModels }

    let getWorldEntityModelWithLens address world lens =
        get (getChild worldOptEntityModelFinder address world) lens

    let setWorldEntityModelWithLens child address world lens =
        let entity = getChild worldOptEntityModelFinder address world
        let entity' = set child entity lens
        setChild worldEntityModelAdder worldEntityModelRemover address world entity'

    let getWorldOptEntityModelWithLens address world lens =
        let optChild = getOptChild worldOptEntityModelFinder address world
        match optChild with
        | None -> None
        | Some child -> Some (get child lens)

    let setWorldOptEntityModelWithLens optChild address world lens =
        match optChild with
        | None -> setOptChild worldEntityModelAdder worldEntityModelRemover address world None
        | Some child ->
            let optChildModel = getOptChild worldOptEntityModelFinder address world
            match optChildModel with
            | None -> failwith "Cannot change a non-existent entity."
            | Some childModel ->
                let childModel' = set child childModel lens
                setChild worldEntityModelAdder worldEntityModelRemover address world childModel'

    let worldEntityModelLens address =
        { Get = fun world -> Option.get <| worldOptEntityModelFinder address world
          Set = fun entity world -> worldEntityModelAdder address world entity }

    let worldOptEntityModelLens address =
        { Get = fun world -> worldOptEntityModelFinder address world
          Set = fun optEntity world -> match optEntity with None -> worldEntityModelRemover address world | Some entity -> worldEntityModelAdder address world entity }

    let worldEntityModelsLens address =
        { Get = fun world ->
            match address with
            | [screenLun; groupLun] ->
                match Map.tryFind screenLun world.WEntityModels with
                | None -> Map.empty
                | Some groupMap ->
                    match Map.tryFind groupLun groupMap with
                    | None -> Map.empty
                    | Some entityMap -> entityMap
            | _ -> failwith <| "Invalid entity model address '" + str address + "'."
          Set = fun entityModels world ->
            match address with
            | [screenLun; groupLun] ->
                match Map.tryFind screenLun world.WEntityModels with
                | None -> { world with WEntityModels = Map.add screenLun (Map.singleton groupLun entityModels) world.WEntityModels }
                | Some groupMap ->
                    match Map.tryFind groupLun groupMap with
                    | None -> { world with WEntityModels = Map.add screenLun (Map.add groupLun entityModels groupMap) world.WEntityModels }
                    | Some entityMap -> { world with WEntityModels = Map.add screenLun (Map.add groupLun (Map.addMany (Map.toSeq entityModels) entityMap) groupMap) world.WEntityModels }
            | _ -> failwith <| "Invalid entity model address '" + str address + "'." }

    let worldEntityLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world entityLens
          Set = fun entity world -> setWorldEntityModelWithLens entity address world entityLens }

    let worldOptEntityLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world entityLens
          Set = fun optEntity world -> setWorldOptEntityModelWithLens optEntity address world entityLens }

    let worldOptGuiLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world guiLens
          Set = fun optGui world -> setWorldOptEntityModelWithLens optGui address world guiLens }

    let worldButtonLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world buttonLens
          Set = fun button world -> setWorldEntityModelWithLens button address world buttonLens }

    let worldOptButtonLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world buttonLens
          Set = fun button world -> setWorldOptEntityModelWithLens button address world buttonLens }

    let worldLabelLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world labelLens
          Set = fun label world -> setWorldEntityModelWithLens label address world labelLens }

    let worldOptLabelLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world labelLens
          Set = fun label world -> setWorldOptEntityModelWithLens label address world labelLens }

    let worldTextBoxLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world textBoxLens
          Set = fun textBox world -> setWorldEntityModelWithLens textBox address world textBoxLens }

    let worldOptTextBoxLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world textBoxLens
          Set = fun textBox world -> setWorldOptEntityModelWithLens textBox address world textBoxLens }

    let worldToggleLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world toggleLens
          Set = fun toggle world -> setWorldEntityModelWithLens toggle address world toggleLens }

    let worldOptToggleLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world toggleLens
          Set = fun toggle world -> setWorldOptEntityModelWithLens toggle address world toggleLens }

    let worldFeelerLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world feelerLens
          Set = fun feeler world -> setWorldEntityModelWithLens feeler address world feelerLens }

    let worldOptFeelerLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world feelerLens
          Set = fun feeler world -> setWorldOptEntityModelWithLens feeler address world feelerLens }

    let worldActorLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world actorLens
          Set = fun actor world -> setWorldEntityModelWithLens actor address world actorLens }

    let worldOptActorLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world optActorLens
          Set = fun optActor world -> setWorldOptEntityModelWithLens optActor address world optActorLens }

    let worldBlockLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world blockLens
          Set = fun block world -> setWorldEntityModelWithLens block address world blockLens }

    let worldOptBlockLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world blockLens
          Set = fun block world -> setWorldOptEntityModelWithLens block address world blockLens }

    let worldAvatarLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world avatarLens
          Set = fun avatar world -> setWorldEntityModelWithLens avatar address world avatarLens }

    let worldOptAvatarLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world avatarLens
          Set = fun avatar world -> setWorldOptEntityModelWithLens avatar address world avatarLens }

    let worldTileMapLens address =
        { Get = fun world -> getWorldEntityModelWithLens address world tileMapLens
          Set = fun tileMap world -> setWorldEntityModelWithLens tileMap address world tileMapLens }

    let worldOptTileMapLens address =
        { Get = fun world -> getWorldOptEntityModelWithLens address world tileMapLens
          Set = fun tileMap world -> setWorldOptEntityModelWithLens tileMap address world tileMapLens }

    // TODO: turn into a lens
    let getEntityModelTransform optCamera entityModel =
        let view = match optCamera with None -> Vector2.Zero | Some camera -> getInverseViewF camera
        match entityModel with
        | Button button -> getGuiTransform button.Gui
        | Label label -> getGuiTransform label.Gui
        | TextBox textBox -> getGuiTransform textBox.Gui
        | Toggle toggle -> getGuiTransform toggle.Gui
        | Feeler feeler -> getGuiTransform feeler.Gui
        | Block block -> getActorTransformRelative view block.Actor
        | Avatar avatar -> getActorTransformRelative view avatar.Actor
        | TileMap tileMap -> getActorTransformRelative view tileMap.Actor

    // TODO: turn into a lens
    let setGuiTransform positionSnap rotationSnap (transform : Transform) entityModel lens =
        let transform' = snapTransform positionSnap rotationSnap transform
        let gui_ = get entityModel lens
        let gui_ = { gui_ with Gui.Position = transform'.Position; Depth = transform'.Depth; Size = transform'.Size }
        set gui_ entityModel lens

    // TODO: turn into a lens
    let setActorTransform positionSnap rotationSnap (transform : Transform) entityModel lens =
        let transform' = snapTransform positionSnap rotationSnap transform
        let actor_ = get entityModel lens
        let actor_ = { actor_ with Actor.Position = transform'.Position
                                   Depth = transform'.Depth
                                   Size = transform'.Size
                                   Rotation = transform'.Rotation }
        set actor_ entityModel lens

    // TODO: turn into a lens
    let setActorTransformRelative (view : Vector2) positionSnap rotationSnap (transform : Transform) entityModel lens =
        let transform' = { transform with Position = transform.Position + view }
        setActorTransform positionSnap rotationSnap transform' entityModel lens

    // TODO: turn into a lens
    let setEntityModelTransform optCamera positionSnap rotationSnap transform entityModel =
        let view = match optCamera with None -> Vector2.Zero | Some camera -> getInverseViewF camera
        match entityModel with
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> setGuiTransform positionSnap rotationSnap transform entityModel guiLens
        | Block _
        | Avatar _
        | TileMap _ -> setActorTransformRelative view positionSnap rotationSnap transform entityModel actorLens

    let getPickingPriority entityModel =
        let transform = getEntityModelTransform None entityModel
        transform.Depth

    // Group Lenses /////////////////////////////////////////////////////////////////

    let groupLens =
        { Get = fun groupModel ->
            match groupModel with
            | Group group -> group
            | OmniFieldGroup omniFieldGroup -> omniFieldGroup.Group
            | OmniBattleGroup omniBattleGroup -> omniBattleGroup.Group
          Set = fun group groupModel ->
            match groupModel with
            | Group _ -> Group group
            | OmniFieldGroup omniFieldGroup -> OmniFieldGroup { omniFieldGroup with Group = group }
            | OmniBattleGroup omniBattleGroup -> OmniBattleGroup { omniBattleGroup with Group = group }}

    let groupIdLens =
        { Get = fun groupModel -> (get groupModel groupLens).Id
          Set = fun value groupModel -> set { get groupModel groupLens with Id = value } groupModel groupLens}

    let worldOptGroupModelFinder (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.WGroupModels
        match optGroupMap with
        | None -> None
        | Some groupMap -> Map.tryFind address.[1] groupMap

    let worldGroupModelAdder (address : Address) world (child : GroupModel) =
        let optGroupMap = Map.tryFind address.[0] world.WGroupModels
        match optGroupMap with
        | None ->
            { world with WGroupModels = Map.singleton address.[0] <| Map.singleton address.[1] child }
        | Some groupMap ->
            let groupMap' = Map.add address.[1] child groupMap
            { world with WGroupModels = Map.add address.[0] groupMap' world.WGroupModels }

    let worldGroupModelRemover (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.WGroupModels
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let groupMap' = Map.remove address.[1] groupMap
            { world with WGroupModels = Map.add address.[0] groupMap' world.WGroupModels }

    let getWorldGroupModelWithLens address world lens =
        get (getChild worldOptGroupModelFinder address world) lens

    let setWorldGroupModelWithLens child address world lens =
        let group = getChild worldOptGroupModelFinder address world
        let group' = set child group lens
        setChild worldGroupModelAdder worldGroupModelRemover address world group'

    let getWorldOptGroupModelWithLens address world lens =
        let optChild = getOptChild worldOptGroupModelFinder address world
        match optChild with None -> None | Some child -> Some (get child lens)

    let setWorldOptGroupModelWithLens optChild address world lens =
        match optChild with
        | None -> setOptChild worldGroupModelAdder worldGroupModelRemover address world None
        | Some child ->
            let optChildModel = getOptChild worldOptGroupModelFinder address world
            match optChildModel with
            | None -> failwith "Cannot change a non-existent group."
            | Some childModel ->
                let childModel' = set child childModel lens
                setChild worldGroupModelAdder worldGroupModelRemover address world childModel'

    let worldGroupModelLens address =
        { Get = fun world -> Option.get <| worldOptGroupModelFinder address world
          Set = fun group world -> worldGroupModelAdder address world group }

    let worldOptGroupModelLens address =
        { Get = fun world -> worldOptGroupModelFinder address world
          Set = fun optGroup world -> match optGroup with None -> worldGroupModelRemover address world | Some entity -> worldGroupModelAdder address world entity }

    let worldGroupModelsLens address =
        { Get = fun world ->
            match address with
            | [screenLun] ->
                match Map.tryFind screenLun world.WGroupModels with
                | None -> Map.empty
                | Some groupMap -> groupMap
            | _ -> failwith <| "Invalid group model address '" + str address + "'."
          Set = fun groupModels world ->
            match address with
            | [screenLun] ->
                match Map.tryFind screenLun world.WGroupModels with
                | None -> { world with WGroupModels = Map.add screenLun groupModels world.WGroupModels }
                | Some groupMap -> { world with WGroupModels = Map.add screenLun (Map.addMany (Map.toSeq groupModels) groupMap) world.WGroupModels }
            | _ -> failwith <| "Invalid group model address '" + str address + "'." }

    let worldGroupLens address =
        { Get = fun world -> getWorldGroupModelWithLens address world groupLens
          Set = fun group world -> setWorldGroupModelWithLens group address world groupLens }

    let worldOptGroupLens address =
        { Get = fun world -> getWorldOptGroupModelWithLens address world groupLens
          Set = fun optGroup world -> setWorldOptGroupModelWithLens optGroup address world groupLens }

    // Screen Lenses ////////////////////////////////////////////////////////////////

    let transitionLens =
        { Get = fun transitionModel ->
            match transitionModel with
            | Transition transition -> transition
            | Dissolve dissolve -> dissolve.Transition
          Set = fun transition transitionModel ->
            match transitionModel with
            | Transition _ -> Transition transition
            | Dissolve dissolve -> Dissolve { dissolve with Transition = transition }}

    let transitionIdLens =
        { Get = fun transitionModel -> (get transitionModel transitionLens).Id
          Set = fun value transitionModel -> set { get transitionModel transitionLens with Id = value } transitionModel transitionLens }

    let transitionLifetimeLens =
        { Get = fun transitionModel -> (get transitionModel transitionLens).Lifetime
          Set = fun value transitionModel -> set { get transitionModel transitionLens with Lifetime = value } transitionModel transitionLens }

    let transitionTicksLens =
        { Get = fun transitionModel -> (get transitionModel transitionLens).Ticks
          Set = fun value transitionModel -> set { get transitionModel transitionLens with Ticks = value } transitionModel transitionLens }

    let transitionTypeLens =
        { Get = fun transitionModel -> (get transitionModel transitionLens).Type
          Set = fun value transitionModel -> set { get transitionModel transitionLens with Type = value } transitionModel transitionLens }

    let screenLens =
        { Get = fun screenModel ->
            match screenModel with
            | Screen screen -> screen
            | OmniBattleScreen omniBattleScreen -> omniBattleScreen.Screen
          Set = fun screen screenModel ->
            match screenModel with
            | Screen _ -> Screen screen
            | OmniBattleScreen omniBattleScreen -> OmniBattleScreen { omniBattleScreen with Screen = screen }}

    let screenIdLens =
        { Get = fun screenModel -> (get screenModel screenLens).Id
          Set = fun value screenModel -> set { get screenModel screenLens with Id = value } screenModel screenLens }

    let screenStateLens =
        { Get = fun screenModel -> (get screenModel screenLens).State
          Set = fun value screenModel -> set { get screenModel screenLens with State = value } screenModel screenLens }

    let screenIncomingModelLens =
        { Get = fun screenModel -> (get screenModel screenLens).IncomingModel
          Set = fun value screenModel -> set { get screenModel screenLens with IncomingModel = value } screenModel screenLens }

    let screenOutgoingModelLens =
        { Get = fun screenModel -> (get screenModel screenLens).OutgoingModel
          Set = fun value screenModel -> set { get screenModel screenLens with OutgoingModel = value } screenModel screenLens }

    let incomingModelLens =
        { Get = fun screenModel -> (get screenModel screenLens).IncomingModel
          Set = fun incoming screenModel -> set { (get screenModel screenLens) with IncomingModel = incoming } screenModel screenLens }

    let outgoingModelLens =
        { Get = fun screenModel -> (get screenModel screenLens).OutgoingModel
          Set = fun outgoing screenModel -> set { (get screenModel screenLens) with OutgoingModel = outgoing } screenModel screenLens }
       
    let worldOptScreenModelFinder (address : Address)  world =
        Map.tryFind address.[0] world.WScreenModels

    let worldScreenModelAdder (address : Address) world (child : ScreenModel) =
        { world with WScreenModels = Map.add address.[0] child world.WScreenModels }

    let worldScreenModelRemover (address : Address)  world =
        { world with WScreenModels = Map.remove address.[0] world.WScreenModels }

    let getWorldScreenModelWithLens address world lens =
        get (getChild worldOptScreenModelFinder address world) lens

    let setWorldScreenModelWithLens child address world lens =
        let screen = getChild worldOptScreenModelFinder address world
        let screen' = set child screen lens
        setChild worldScreenModelAdder worldScreenModelRemover address world screen'

    let getWorldOptScreenModelWithLens address world lens =
        let optChild = getOptChild worldOptScreenModelFinder address world
        match optChild with None -> None | Some child -> Some (get child lens)

    let setWorldOptScreenModelWithLens optChild address world lens =
        match optChild with
        | None -> setOptChild worldScreenModelAdder worldScreenModelRemover address world None
        | Some child ->
            let optChildModel = getOptChild worldOptScreenModelFinder address world
            match optChildModel with
            | None -> failwith "Cannot change a non-existent screen."
            | Some childModel ->
                let childModel' = set child childModel lens
                setChild worldScreenModelAdder worldScreenModelRemover address world childModel'

    let worldScreenModelLens address =
        { Get = fun world -> Option.get <| worldOptScreenModelFinder address world
          Set = fun screen world -> worldScreenModelAdder address world screen }

    let worldOptScreenModelLens address =
        { Get = fun world -> worldOptScreenModelFinder address world
          Set = fun optScreen world -> match optScreen with None -> worldScreenModelRemover address world | Some screen -> worldScreenModelAdder address world screen }

    let worldScreenModelsLens address =
        { Get = fun world ->
            match address with
            | [] -> world.WScreenModels
            | _ -> failwith <| "Invalid screen model address '" + str address + "'."
          Set = fun screenModels world ->
            match address with
            | [] -> { world with WScreenModels = Map.addMany (Map.toSeq screenModels) world.WScreenModels }
            | _ -> failwith <| "Invalid screen model address '" + str address + "'." }

    let worldScreenLens address =
        { Get = fun world -> getWorldScreenModelWithLens address world screenLens
          Set = fun screen world -> setWorldScreenModelWithLens screen address world screenLens }

    let worldOptScreenLens address =
        { Get = fun world -> getWorldOptScreenModelWithLens address world screenLens
          Set = fun optScreen world -> setWorldOptScreenModelWithLens optScreen address world screenLens }

    let worldIncomingModelLens address = worldScreenModelLens address >>| incomingModelLens
    let worldOutgoingModelLens address = worldScreenModelLens address >>| outgoingModelLens

    // Game Lenses //////////////////////////////////////////////////////////////////

    let gameLens =
        { Get = fun world ->
            match world.GameModel with
            | Game game -> game
            | OmniGame omniGame -> omniGame.Game
          Set = fun game world ->
            match world.GameModel with
            | Game _ -> { world with GameModel = Game game }
            | OmniGame omniGame -> { world with GameModel = OmniGame { omniGame with Game = game }}}

    let gameIdLens =
        { Get = fun world -> (get world gameLens).Id
          Set = fun value world -> set { get world gameLens with Id = value } world gameLens }

    let worldOptSelectedScreenModelAddressLens =
        { Get = fun world -> (get world gameLens).OptSelectedScreenModelAddress
          Set = fun value world -> set { (get world gameLens) with OptSelectedScreenModelAddress = value } world gameLens}

    let worldOptSelectedScreenModelLens =
        { Get = fun world ->
            let optSelectedScreenModelAddress = get world worldOptSelectedScreenModelAddressLens
            match optSelectedScreenModelAddress with
            | None -> None
            | Some selectedScreenModelAddress -> get world <| worldOptScreenModelLens selectedScreenModelAddress
          Set = fun screen world ->
            let optSelectedScreenModelAddress = get world worldOptSelectedScreenModelAddressLens
            match optSelectedScreenModelAddress with
            | None -> failwith "Cannot set a non-existent screen."
            | Some selectedScreenModelAddress -> set screen.Value world <| worldScreenModelLens selectedScreenModelAddress }

    // World Lenses /////////////////////////////////////////////////////////////////

    let gameModelLens =
        { Get = fun world -> world.GameModel
          Set = fun gameModel world -> { world with GameModel = gameModel }}
      
    let cameraLens =
        { Get = fun world -> world.Camera
          Set = fun camera world -> { world with Camera = camera }}

    let mouseStateLens =
        { Get = fun world -> world.MouseState
          Set = fun mouseState world -> { world with MouseState = mouseState }}

    let worldGameLens =
        { Get = fun world -> get world gameLens
          Set = fun game world -> set game world gameLens }

    /////////////////////////////////////////////////////////////////////////////////
    
    let tryCreateEmptyWorld sdlDeps extData =
        match tryGenerateAssetMetadataMap "AssetGraph.xml" with
        | Left errorMsg -> Left errorMsg
        | Right assetMetadataMap ->
            let world =
                { GameModel = Game { Id = getNuId (); OptSelectedScreenModelAddress = None }
                  WScreenModels = Map.empty
                  WGroupModels = Map.empty
                  WEntityModels = Map.empty
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
                  Components = []
                  ExtData = extData }
            Right world

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
        let optScreenAddress = (get world worldGameLens).OptSelectedScreenModelAddress
        match (address, optScreenAddress) with
        | ([], _) -> true
        | (_, None) -> false
        | (_, Some []) -> false
        | (addressHead :: addressTail, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

    let sortFstAsc (priority, _) (priority2, _) =
        if priority = priority2 then 0
        elif priority > priority2 then -1
        else 1

    let getPublishingPriority simulant =
        match simulant with
        | GameModel _ -> GameModelPublishingPriority
        | ScreenModel _ -> ScreenModelPublishingPriority
        | GroupModel _ -> GroupModelPublishingPriority
        | EntityModel entityModel -> getPickingPriority entityModel

    let getSimulant address world =
        match address with
        | [] -> GameModel <| get world gameModelLens
        | [_] as screenAddress -> ScreenModel <| get world (worldScreenModelLens screenAddress)
        | [_; _] as groupAddress -> GroupModel <| get world (worldGroupModelLens groupAddress)
        | [_; _; _] as entityAddress -> EntityModel <| get world (worldEntityModelLens entityAddress)
        | _ -> failwith <| "Invalid simulant address '" + str address + "'."

    let getSimulants subscriptions world =
        List.map (fun (address, _) -> getSimulant address world) subscriptions

    let pickingSort entityModels world =
        let priorities = List.map getPickingPriority entityModels
        let prioritiesAndEntityModels = List.zip priorities entityModels
        let prioritiesAndEntityModelsSorted = List.sortWith sortFstAsc prioritiesAndEntityModels
        List.map snd prioritiesAndEntityModelsSorted

    let tryPick (position : Vector2) entityModels world =
        let entityModelsSorted = pickingSort entityModels world
        List.tryFind
            (fun entityModel ->
                let transform = getEntityModelTransform (Some world.Camera) entityModel
                position.X >= transform.Position.X &&
                    position.X < transform.Position.X + transform.Size.X &&
                    position.Y >= transform.Position.Y &&
                    position.Y < transform.Position.Y + transform.Size.Y)
            entityModelsSorted

    let subscriptionSort subscriptions world =
        let simulants = getSimulants subscriptions world
        let priorities = List.map getPublishingPriority simulants
        let prioritiesAndSubscriptions = List.zip priorities subscriptions
        let prioritiesAndSubscriptionsSorted = List.sortWith sortFstAsc prioritiesAndSubscriptions
        List.map snd prioritiesAndSubscriptionsSorted

    /// Publish a message to the given address.
    let publish address message world_ : bool * World =
        let optSubList = Map.tryFind address world_.Subscriptions
        match optSubList with
        | None -> (true, world_)
        | Some subList ->
            let subListSorted = subscriptionSort subList world_
            let (_, keepRunning, world_) =
                List.foldWhile
                    (fun (message', keepRunning', world_) (subscriber, (Subscription subscription)) ->
                        if message'.Handled || not keepRunning' then None
                        elif isAddressSelected subscriber world_ then Some (subscription address subscriber message' world_)
                        else Some (message', keepRunning', world_))
                    (message, true, world_)
                    subListSorted
            (keepRunning, world_)

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
    let getScreenModelState address world =
        let screenModel = get world <| worldScreenModelLens address
        get screenModel screenStateLens

    let setScreenModelState state address world =
        let screenModel = set state (get world <| worldScreenModelLens address) screenStateLens
        let world' = set screenModel world <| worldScreenModelLens address
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
        let world' = setScreenModelState IncomingState address world
        set (Some address) world' worldOptSelectedScreenModelAddressLens

    let transitionScreenHandler address _ _ message world =
        let world' = transitionScreen address world
        (handle message, true, world')

    let handleFinishedScreenOutgoing screenAddress destScreenAddress address subscriber message world =
        let world' = unsubscribe address subscriber world
        let world'' = transitionScreen destScreenAddress world'
        (handle message, true, world'')

    let handleEventAsScreenTransition screenAddress destScreenAddress address subscriber message world =
        let world' = subscribe (FinishedOutgoingAddressPart @ screenAddress) [] (handleFinishedScreenOutgoing screenAddress destScreenAddress) world
        let optSelectedScreenAddress = get world' worldOptSelectedScreenModelAddressLens
        match optSelectedScreenAddress with
        | None ->
            trace <| "Program Error: Could not handle click as screen transition due to no selected screen model."
            (handle message, true, world)
        | Some selectedScreenAddress ->
            let world'' = setScreenModelState OutgoingState selectedScreenAddress world'
            (handle message, true, world'')

    let updateTransition1 transitionModel =
        let transition = get transitionModel transitionLens
        let (transition', finished) =
            if transition.Ticks = transition.Lifetime then ({ transition with Ticks = 0 }, true)
            else ({ transition with Ticks = transition.Ticks + 1 }, false)
        let transitionModel' = set transition' transitionModel transitionLens
        (transitionModel', finished)

    let updateTransition update world_ : bool * World =
        let (keepRunning, world_) =
            let optSelectedScreenAddress = get world_ worldOptSelectedScreenModelAddressLens
            match optSelectedScreenAddress with
            | None -> (true, world_)
            | Some selectedScreenAddress ->
                let screenModelState = getScreenModelState selectedScreenAddress world_
                match screenModelState with
                | IncomingState ->
                    // TODO: remove duplication with below
                    let selectedScreenModel = get world_ <| worldScreenModelLens selectedScreenAddress
                    let incomingModel = get selectedScreenModel incomingModelLens
                    let (incomingModel', finished) = updateTransition1 incomingModel
                    let selectedScreenModel' = set incomingModel' selectedScreenModel incomingModelLens
                    let world_ = set selectedScreenModel' world_ <| worldScreenModelLens selectedScreenAddress
                    let world_ = setScreenModelState (if finished then IdlingState else IncomingState) selectedScreenAddress world_
                    if finished then
                        publish
                            (FinishedIncomingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world_
                    else (true, world_)
                | OutgoingState ->
                    let selectedScreenModel = get world_ <| worldScreenModelLens selectedScreenAddress
                    let outgoingModel = get selectedScreenModel outgoingModelLens
                    let (outgoingModel', finished) = updateTransition1 outgoingModel
                    let selectedScreenModel' = set outgoingModel' selectedScreenModel outgoingModelLens
                    let world_ = set selectedScreenModel' world_ <| worldScreenModelLens selectedScreenAddress
                    let world_ = setScreenModelState (if finished then IdlingState else OutgoingState) selectedScreenAddress world_
                    if finished then
                        publish
                            (FinishedOutgoingAddressPart @ selectedScreenAddress)
                            { Handled = false; Data = NoData }
                            world_
                    else (true, world_)
                | IdlingState -> (true, world_)
        if keepRunning then update world_
        else (keepRunning, world_)

    let unregisterButton address world =
        world |>
            unsubscribe DownMouseLeftAddress address |>
            unsubscribe UpMouseLeftAddress address

    let handleButtonEventDownMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world_ (worldButtonLens subscriber)
            if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
                if isInBox3 mousePosition button.Gui.Position button.Gui.Size then
                    let button' = { button with IsDown = true }
                    let world_ = set button' world_ (worldButtonLens subscriber)
                    let (keepRunning, world_) = publish (straddr "down" subscriber) { Handled = false; Data = NoData } world_
                    (handle message, keepRunning, world_)
                else (message, true, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleButtonEventUpMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let button = get world_ (worldButtonLens subscriber)
            if button.Gui.Entity.Enabled && button.Gui.Entity.Visible then
                let (keepRunning_, world_) =
                    let button' = { button with IsDown = false }
                    let world_ = set button' world_ (worldButtonLens subscriber)
                    publish (straddr "up" subscriber) { Handled = false; Data = NoData } world_
                if keepRunning_ && isInBox3 mousePosition button.Gui.Position button.Gui.Size && button.IsDown then
                    let (keepRunning_, world_) = publish (straddr "click" subscriber) { Handled = false; Data = NoData } world_
                    let sound = PlaySound { Volume = 1.0f; Sound = button.ClickSound }
                    let world_ = { world_ with AudioMessages = sound :: world_.AudioMessages }
                    (handle message, keepRunning_, world_)
                else (message, keepRunning_, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let registerButton address world =
        let optOldButton = get world (worldOptButtonLens address)
        let world' = if optOldButton.IsSome then unregisterButton address world else world
        let world'' = subscribe DownMouseLeftAddress address handleButtonEventDownMouseLeft world'
        subscribe UpMouseLeftAddress address handleButtonEventUpMouseLeft world''

    let addButton address button world =
        let world' = registerButton address world
        set (Button button) world' (worldEntityModelLens address)

    let removeButton address world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterButton address world'

    let addLabel address label world =
        set (Label label) world (worldEntityModelLens address)

    let removeLabel address world =
        set None world (worldOptEntityModelLens address)

    let addTextBox address textBox world =
        set (TextBox textBox) world (worldEntityModelLens address)

    let removeTextBox address world =
        set None world (worldOptEntityModelLens address)

    let unregisterToggle address world =
        world |>
            unsubscribe DownMouseLeftAddress address |>
            unsubscribe UpMouseLeftAddress address

    let handleToggleEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let toggle = get world (worldToggleLens subscriber)
            if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible then
                if isInBox3 mousePosition toggle.Gui.Position toggle.Gui.Size then
                    let toggle' = { toggle with IsPressed = true }
                    let world' = set toggle' world (worldToggleLens subscriber)
                    (handle message, true, world')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleToggleEventUpMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) ->
            let toggle = get world (worldToggleLens subscriber)
            if toggle.Gui.Entity.Enabled && toggle.Gui.Entity.Visible && toggle.IsPressed then
                let toggle' = { toggle with IsPressed = false }
                if isInBox3 mousePosition toggle'.Gui.Position toggle'.Gui.Size then
                    let toggle'' = { toggle' with IsOn = not toggle'.IsOn }
                    let world' = set toggle'' world (worldToggleLens subscriber)
                    let messageType = if toggle''.IsOn then "on" else "off"
                    let (keepRunning, world'') = publish (straddr messageType subscriber) { Handled = false; Data = NoData } world'
                    let sound = PlaySound { Volume = 1.0f; Sound = toggle''.ToggleSound }
                    let world''' = { world'' with AudioMessages = sound :: world''.AudioMessages }
                    (handle message, keepRunning, world''')
                else
                    let world' = set toggle' world (worldToggleLens subscriber)
                    (message, true, world')
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")

    let registerToggle address world =
        let world' = subscribe DownMouseLeftAddress address handleToggleEventDownMouseLeft world
        subscribe UpMouseLeftAddress address handleToggleEventUpMouseLeft world'

    let addToggle address toggle world =
        let world' = registerToggle address world
        set (Toggle toggle) world' (worldEntityModelLens address)

    let removeToggle address world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterToggle address world'

    let unregisterFeeler address world =
        world |>
            unsubscribe UpMouseLeftAddress address |>
            unsubscribe DownMouseLeftAddress address

    let handleFeelerEventDownMouseLeft address subscriber message world =
        match message.Data with
        | MouseButtonData (mousePosition, _) as mouseButtonData ->
            let feeler = get world (worldFeelerLens subscriber)
            if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
                if isInBox3 mousePosition feeler.Gui.Position feeler.Gui.Size then
                    let feeler' = { feeler with IsTouched = true }
                    let world' = set feeler' world (worldFeelerLens subscriber)
                    let (keepRunning, world'') = publish (straddr "touch" subscriber) { Handled = false; Data = mouseButtonData } world'
                    (handle message, keepRunning, world'')
                else (message, true, world)
            else (message, true, world)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let handleFeelerEventUpMouseLeft address subscriber message world_ =
        match message.Data with
        | MouseButtonData _ ->
            let feeler = get world_ (worldFeelerLens subscriber)
            if feeler.Gui.Entity.Enabled && feeler.Gui.Entity.Visible then
                let feeler' = { feeler with IsTouched = false }
                let world_ = set feeler' world_ (worldFeelerLens subscriber)
                let (keepRunning, world_) = publish (straddr "release" subscriber) { Handled = false; Data = NoData } world_
                (handle message, keepRunning, world_)
            else (message, true, world_)
        | _ -> failwith ("Expected MouseButtonData from address '" + str address + "'.")
    
    let registerFeeler address world =
        let world' = subscribe DownMouseLeftAddress address handleFeelerEventDownMouseLeft world
        subscribe UpMouseLeftAddress address handleFeelerEventUpMouseLeft world'

    let addFeeler address feeler world =
        let world' = registerFeeler address world
        set (Feeler feeler) world' (worldEntityModelLens address)

    let removeFeeler address world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterFeeler address world'

    let unregisterBlockPhysics address (block : Block) world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = block.PhysicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let registerBlockPhysics address (block : Block) world =
        let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  PhysicsId = block.PhysicsId
                  Shape =
                    BoxShape
                        { Extent = block.Actor.Size * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Restitution = 0.0f
                              FixedRotation = false
                              LinearDamping = 5.0f
                              AngularDamping = 5.0f }}
                  Position = block.Actor.Position + block.Actor.Size * 0.5f
                  Rotation = block.Actor.Rotation
                  Density = block.Density
                  BodyType = block.BodyType }
        { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }

    let addBlock address block world =
        let world' = registerBlockPhysics address block world
        set (Block block) world' (worldEntityModelLens address)

    let removeBlock address block world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterBlockPhysics address block world'

    let unregisterAvatarPhysics address avatar world =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = avatar.PhysicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let registerAvatarPhysics address avatar world =
        let bodyCreateMessage =
            BodyCreateMessage
                { EntityAddress = address
                  PhysicsId = avatar.PhysicsId
                  Shape =
                    CircleShape
                        { Radius = avatar.Actor.Size.X * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Restitution = 0.0f
                              FixedRotation = true
                              LinearDamping = 10.0f
                              AngularDamping = 0.0f }}
                  Position = avatar.Actor.Position + avatar.Actor.Size * 0.5f
                  Rotation = avatar.Actor.Rotation
                  Density = avatar.Density
                  BodyType = BodyType.Dynamic }
        { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }

    let addAvatar address avatar world =
        let world' = registerAvatarPhysics address avatar world
        set (Avatar avatar) world' (worldEntityModelLens address)

    let removeAvatar address avatar world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterAvatarPhysics address avatar world'

    let unregisterTilePhysics world physicsId =
        let bodyDestroyMessage = BodyDestroyMessage { PhysicsId = physicsId }
        { world with PhysicsMessages = bodyDestroyMessage :: world.PhysicsMessages }

    let unregisterTileMapPhysics address tileMap world =
        List.fold unregisterTilePhysics world tileMap.PhysicsIds

    let registerTilePhysics tileMap tmd tld address n (world, physicsIds) tile =
        let td = makeTileData tileMap tmd tld n
        match td.OptTileSetTile with
        | None -> (world, physicsIds)
        | Some tileSetTile when not <| tileSetTile.Properties.ContainsKey "c" -> (world, physicsIds)
        | Some tileSetTile ->
            let physicsId = getPhysicsId ()
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
                      Rotation = tileMap.Actor.Rotation
                      Density = tileMap.Density
                      BodyType = BodyType.Static }
            let world' = { world with PhysicsMessages = bodyCreateMessage :: world.PhysicsMessages }
            (world', physicsId :: physicsIds)

    let registerTileMapPhysics address tileMap world =
        let collisionLayer = 0 // MAGIC_VALUE: assumption
        let tmd = makeTileMapData tileMap
        let tld = makeTileLayerData tileMap tmd collisionLayer
        let (world', physicsIds) = Seq.foldi (registerTilePhysics tileMap tmd tld address) (world, []) tld.Tiles
        let tileMap' = { tileMap with PhysicsIds = physicsIds }
        (tileMap', world')

    let addTileMap address tileMap world =
        let (tileMap', world') = registerTileMapPhysics address tileMap world
        set (TileMap tileMap') world' (worldEntityModelLens address)

    let removeTileMap address tileMap world =
        let world' = set None world (worldOptEntityModelLens address)
        unregisterTileMapPhysics address tileMap world'

    let addEntityModel address entityModel world =
        match entityModel with
        | Button button -> addButton address button world
        | Label label -> addLabel address label world
        | TextBox textBox -> addTextBox address textBox world
        | Toggle toggle -> addToggle address toggle world
        | Feeler feeler -> addFeeler address feeler world
        | Block block -> addBlock address block world
        | Avatar avatar -> addAvatar address avatar world
        | TileMap tileMap -> addTileMap address tileMap world

    let addEntityModels address entityModels world =
        List.fold
            (fun world' entityModel ->
                let entity = get entityModel entityLens
                addEntityModel (addrstr address entity.Name) entityModel world')
            world
            entityModels

    let removeEntityModel address world =
        let entityModel = get world <| worldEntityModelLens address
        match entityModel with
        | Button button -> removeButton address world
        | Label label -> removeLabel address world
        | TextBox textBox -> removeTextBox address world
        | Toggle toggle -> removeToggle address world
        | Feeler feeler -> removeFeeler address world
        | Block block -> removeBlock address block world
        | Avatar avatar -> removeAvatar address avatar world
        | TileMap tileMap -> removeTileMap address tileMap world

    let removeEntityModels address world =
        let entityModels = get world <| worldEntityModelsLens address
        Map.fold
            (fun world' entityModelName _ -> removeEntityModel (address @ [entityModelName]) world')
            world
            entityModels

    let propagateEntityModelPhysics address entityModel world =
        match entityModel with
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> world
        | Block block -> world |> unregisterBlockPhysics address block |> registerBlockPhysics address block
        | Avatar avatar -> world |> unregisterAvatarPhysics address avatar |> registerAvatarPhysics address avatar
        | TileMap tileMap -> snd <| (world |> unregisterTileMapPhysics address tileMap |> registerTileMapPhysics address tileMap)

    let addGroup address group entityModels world =
        let world' = set (Group group) world (worldGroupModelLens address)
        addEntityModels address entityModels world'

    let removeGroup address world =
        let world' = removeEntityModels address world
        set None world' (worldOptGroupModelLens address)

    let adjustFieldCamera groupAddress world =
        let avatarAddress = groupAddress @ [FieldAvatarName]
        let actor = get world <| worldActorLens avatarAddress
        let camera = { world.Camera with EyePosition = actor.Position + actor.Size * 0.5f }
        { world with Camera = camera }

    let adjustFieldCameraHandler groupAddress _ _ message world =
        (message, true, adjustFieldCamera groupAddress world)

    let moveFieldAvatarHandler groupAddress _ _ message world =
        let feelerAddress = groupAddress @ [FieldFeelerName]
        let feeler = get world (worldFeelerLens feelerAddress)
        if feeler.IsTouched then
            let avatarAddress = groupAddress @ [FieldAvatarName]
            let avatar = get world <| worldAvatarLens avatarAddress
            let camera = world.Camera
            let view = getInverseViewF camera
            let mousePositionWorld = world.MouseState.MousePosition + view
            let actorCenter = avatar.Actor.Position + avatar.Actor.Size * 0.5f
            let impulseVector = (mousePositionWorld - actorCenter) * 5.0f
            let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
            let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
            (message, true, world')
        else (message, true, world)

    let addFieldGroup address (omniFieldGroup : OmniFieldGroup) entityModels world_ =
        let world_ = subscribe TickAddress [] (moveFieldAvatarHandler address) world_
        let world_ = subscribe TickAddress [] (adjustFieldCameraHandler address) world_
        let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
        let world_ = set (OmniFieldGroup omniFieldGroup) world_ (worldGroupModelLens address)
        let world_ = addEntityModels address entityModels world_
        adjustFieldCamera address world_

    let removeFieldGroup address world_ =
        let world_ = unsubscribe TickAddress [] world_
        let world_ = unsubscribe TickAddress [] world_
        let world_ = removeEntityModels address world_
        set None world_ (worldOptGroupModelLens address)

    let addBattleGroup address (omniBattleGroup : OmniBattleGroup) entityModels world_ =
        let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
        let world_ = set (OmniBattleGroup omniBattleGroup) world_ (worldGroupModelLens address)
        addEntityModels address entityModels world_

    let removeBattleGroup address world_ =
        let world_ = removeEntityModels address world_
        set None world_ (worldOptGroupModelLens address)

    let addGroupModel address groupModel entityModels world =
        match groupModel with
        | Group group -> addGroup address group entityModels world
        | OmniFieldGroup omniFieldGroup -> addFieldGroup address omniFieldGroup entityModels world
        | OmniBattleGroup omniBattleGroup -> addBattleGroup address omniBattleGroup entityModels world

    let removeGroupModel address world =
        let groupModel = get world <| worldGroupModelLens address
        match groupModel with
        | Group _ -> removeGroup address world
        | OmniFieldGroup _ -> removeFieldGroup address world
        | OmniBattleGroup _ -> removeBattleGroup address world

    let addGroupModels address groupDescriptors world =
        List.fold
            (fun world' (groupName, groupModel, entityModels) -> addGroupModel (address @ [groupName]) groupModel entityModels world')
            world
            groupDescriptors

    let removeGroupModels address world =
        let groupModels = get world <| worldGroupModelsLens address
        Map.fold
            (fun world' groupModelName _ -> removeGroupModel (address @ [groupModelName]) world')
            world
            groupModels

    let addScreen address screen groupDescriptors world =
        let world' = set (Screen screen) world (worldScreenModelLens address)
        addGroupModels address groupDescriptors world'

    let removeScreen address world =
        let world' = removeGroupModels address world
        set None world' (worldOptScreenModelLens address)

    let addOmniBattleScreen address omniBattleScreen groupDescriptors world =
        let world' = set (OmniBattleScreen omniBattleScreen) world (worldScreenModelLens address)
        addGroupModels address groupDescriptors world'

    let removeOmniBattleScreen address world =
        let world' = removeGroupModels address world
        set None world' (worldOptScreenModelLens address)

    let addScreenModel address screenModel groupDescriptor world =
        match screenModel with
        | Screen screen -> addScreen address screen groupDescriptor world
        | OmniBattleScreen omniBattleScreen -> addOmniBattleScreen address omniBattleScreen groupDescriptor world

    let removeScreenModel address world =
        let screenModel = get world <| worldScreenModelLens address
        match screenModel with
        | Screen screen -> removeScreen address world
        | OmniBattleScreen omniBattleScreen -> removeOmniBattleScreen address world

    let removeScreenModels world =
        Map.fold
            (fun world' screenModelName _ -> removeScreenModel [screenModelName] world')
            world
            world.WScreenModels

    let rec handleSplashScreenIdleTick idlingTime ticks address subscriber message world =
        let world' = unsubscribe address subscriber world
        if ticks < idlingTime then
            let world'' = subscribe address subscriber (handleSplashScreenIdleTick idlingTime <| ticks + 1) world'
            (message, true, world'')
        else
            let optSelectedScreenAddress = get world' worldOptSelectedScreenModelAddressLens
            match optSelectedScreenAddress with
            | None ->
                trace "Program Error: Could not handle splash screen tick due to no selected screen model."
                (message, false, world)
            | Some selectedScreenAddress ->
                let world'' = setScreenModelState OutgoingState selectedScreenAddress world'
                (message, true, world'')

    let handleSplashScreenIdle idlingTime address subscriber message world =
        let world' = subscribe TickAddress subscriber (handleSplashScreenIdleTick idlingTime 0) world
        (handle message, true, world')

    let addSplashScreen handleFinishedOutgoing address incomingTime idlingTime outgoingTime sprite world =
        let splashScreenModel = makeDissolveScreen incomingTime outgoingTime
        let splashGroupModel = Group <| makeDefaultGroup ()
        let splashLabel = Label { Gui = { makeDefaultGui (Some "splashLabel") with Size = world.Camera.EyeSize }; LabelSprite = sprite }
        let world' = addScreen address splashScreenModel [(Lun.make "splashGroup", splashGroupModel, [splashLabel])] world
        let world'' = subscribe (FinishedIncomingAddressPart @ address) address (handleSplashScreenIdle idlingTime) world'
        subscribe (FinishedOutgoingAddressPart @ address) address handleFinishedOutgoing world''

    let createDissolveScreenFromFile groupModelFileName groupModelName incomingTime outgoingTime screenAddress world =
        let screenModel = Screen <| makeDissolveScreen incomingTime outgoingTime
        let (groupModel, entityModels) = loadGroupModelFile groupModelFileName world
        addScreenModel screenAddress screenModel [(groupModelName, groupModel, entityModels)] world

    let reregisterPhysicsHack4 groupAddress world _ entityModel =
        match entityModel with
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> world
        | Block block -> registerBlockPhysics (addrstr groupAddress block.Actor.Entity.Name) block world
        | Avatar avatar -> registerAvatarPhysics (addrstr groupAddress avatar.Actor.Entity.Name) avatar world
        | TileMap tileMap -> 
            let tileMapAddress = addrstr groupAddress tileMap.Actor.Entity.Name
            let (tileMap', world') = registerTileMapPhysics tileMapAddress tileMap world
            set tileMap' world' <| worldTileMapLens tileMapAddress

    let reregisterPhysicsHack groupAddress world =
        let entityModels = get world <| worldEntityModelsLens groupAddress
        Map.fold (reregisterPhysicsHack4 groupAddress) world entityModels

    let getComponentAudioDescriptors world : AudioDescriptor rQueue =
        let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetAudioDescriptors world :: descs) [] world.Components // TODO: get audio descriptors
        List.collect (fun descs -> descs) descriptorLists

    let getAudioDescriptors world : AudioDescriptor rQueue =
        let componentDescriptors = getComponentAudioDescriptors world
        let worldDescriptors = [] // TODO: get audio descriptors when there are some
        componentDescriptors @ worldDescriptors // NOTE: pretty inefficient

    /// Play the world's audio.
    let play world =
        let audioMessages = world.AudioMessages
        let audioDescriptors = getAudioDescriptors world
        let audioPlayer = world.AudioPlayer
        let world' = { world with AudioMessages = [] }
        { world' with AudioPlayer = Nu.Audio.play audioMessages audioDescriptors audioPlayer }

    let getComponentRenderDescriptors world : RenderDescriptor rQueue =
        let descriptorLists = List.fold (fun descs (comp : IWorldComponent) -> comp.GetRenderDescriptors world :: descs) [] world.Components // TODO: get render descriptors
        List.collect (fun descs -> descs) descriptorLists

    let getEntityRenderDescriptors view entity =
        match entity with
        | Button button ->
            let (_, gui, entity) = buttonSep button
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = (if button.IsDown then button.DownSprite else button.UpSprite); Color = Vector4.One }; Depth = gui.Depth })]
        | Label label ->
            let (_, gui, entity) = labelSep label
            if not label.Gui.Entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = label.LabelSprite; Color = Vector4.One }; Depth = gui.Depth })]
        | TextBox textBox ->
            let (_, gui, entity) = textBoxSep textBox
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = textBox.BoxSprite; Color = Vector4.One }; Depth = gui.Depth })
                  LayerableDescriptor (LayeredTextDescriptor { Descriptor = { Text = textBox.Text; Position = gui.Position + textBox.TextOffset; Size = gui.Size - textBox.TextOffset; Font = textBox.TextFont; Color = textBox.TextColor }; Depth = gui.Depth })]
        | Toggle toggle ->
            let (_, gui, entity) = toggleSep toggle
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = gui.Position; Size = gui.Size; Rotation = 0.0f; Sprite = (if toggle.IsOn || toggle.IsPressed then toggle.OnSprite else toggle.OffSprite); Color = Vector4.One }; Depth = gui.Depth })]
        | Feeler _ ->
            []
        | Block block ->
            let (_, actor, entity) = blockSep block
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position - view; Size = actor.Size; Rotation = actor.Rotation; Sprite = block.Sprite; Color = Vector4.One }; Depth = actor.Depth })]
        | Avatar avatar ->
            let (_, actor, entity) = avatarSep avatar
            if not entity.Visible then []
            else [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = actor.Position - view; Size = actor.Size; Rotation = actor.Rotation; Sprite = avatar.Sprite; Color = Vector4.One }; Depth = actor.Depth })]
        | TileMap tileMap ->
            let (_, actor, entity) = tileMapSep tileMap
            if not entity.Visible then []
            else
                let map = tileMap.TmxMap
                let layers = List.ofSeq map.Layers
                List.mapi
                    (fun i (layer : TmxLayer) ->
                        let layeredTileLayerDescriptor =
                            LayeredTileLayerDescriptor
                                { Descriptor =
                                    { Position = tileMap.Actor.Position - view
                                      Size = tileMap.Actor.Size
                                      Rotation = actor.Rotation
                                      MapSize = Vector2 (single map.Width, single map.Height)
                                      Tiles = layer.Tiles
                                      TileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                                      TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                      TileSetSprite = tileMap.TileMapSprites.[0] } // MAGIC_VALUE: for same reason as above
                                  Depth = actor.Depth + single i * 2.0f } // MAGIC_VALUE: assumption
                        LayerableDescriptor layeredTileLayerDescriptor)
                    layers

    let getGroupModelRenderDescriptors camera entityModels =
        let view = getInverseView camera
        let entitModelValues = Map.toValueSeq entityModels
        Seq.map (getEntityRenderDescriptors view) entitModelValues

    let getTransitionModelRenderDescriptors camera transitionModel =
        match transitionModel with
        | Transition _ -> []
        | Dissolve dissolve ->
            let transition = dissolve.Transition
            let progress = single transition.Ticks / single transition.Lifetime
            let alpha = match transition.Type with Incoming -> 1.0f - progress | Outgoing -> progress
            let color = Vector4 (Vector3.One, alpha)
            [LayerableDescriptor (LayeredSpriteDescriptor { Descriptor = { Position = Vector2.Zero; Size = camera.EyeSize; Rotation = 0.0f; Sprite = dissolve.Sprite; Color = color }; Depth = Single.MaxValue })]

    let getWorldRenderDescriptors world =
        match get world worldOptSelectedScreenModelAddressLens with
        | None -> []
        | Some activeScreenAddress ->
            let optGroupMap = Map.tryFind activeScreenAddress.[0] world.WEntityModels
            match optGroupMap with
            | None -> []
            | Some groupMap ->
                let entityMaps = List.fold List.flipCons [] <| Map.toValueList groupMap
                let descriptorSeqs = List.map (getGroupModelRenderDescriptors world.Camera) entityMaps
                let descriptorSeq = Seq.concat descriptorSeqs
                let descriptors = List.concat descriptorSeq
                let activeScreen = get world (worldScreenLens activeScreenAddress)
                match activeScreen.State with
                | IncomingState -> descriptors @ getTransitionModelRenderDescriptors world.Camera activeScreen.IncomingModel
                | OutgoingState -> descriptors @ getTransitionModelRenderDescriptors world.Camera activeScreen.OutgoingModel
                | IdlingState -> descriptors

    let getRenderDescriptors world : RenderDescriptor rQueue =
        let componentDescriptors = getComponentRenderDescriptors world
        let worldDescriptors = getWorldRenderDescriptors world
        componentDescriptors @ worldDescriptors // NOTE: pretty inefficient

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
                let entityModel = get world (worldEntityModelLens bodyTransformMessage.EntityAddress)
                match entityModel with
                | Button _
                | Label _
                | TextBox _
                | Toggle _
                | Feeler _ ->
                    debug "Unexpected gui match in Nu.Sim.handleIntegrationMessage."
                    (keepRunning, world)
                | Block _
                | Avatar _ ->
                    let actor = get entityModel actorLens
                    let actor' = { actor with Position = bodyTransformMessage.Position - actor.Size * 0.5f // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                                              Rotation = bodyTransformMessage.Rotation }
                    let world' = set actor' world <| worldActorLens bodyTransformMessage.EntityAddress
                    (keepRunning, world')
                | TileMap _ ->
                    // nothing to do here for tile map
                    (keepRunning, world)
            | BodyCollisionMessage bodyCollisionMessage ->
                let collisionAddress = straddr "collision" bodyCollisionMessage.EntityAddress
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
            (fun sdlDeps -> tryCreateWorld sdlDeps)
            (fun refEvent world ->
                let event = !refEvent
                match event.``type`` with
                | SDL.SDL_EventType.SDL_QUIT -> (false, world)
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = Vector2 (single event.button.x, single event.button.y)
                    let world' = set { world.MouseState with MousePosition = mousePosition } world mouseStateLens
                    if Set.contains MouseLeft world'.MouseState.MouseDowns then publish MouseDragAddress { Handled = false; Data = MouseMoveData mousePosition } world'
                    else publish MouseMoveAddress { Handled = false; Data = MouseButtonData (mousePosition, MouseLeft) } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mouseButton = makeMouseButton event.button.button
                    let world' = set { world.MouseState with MouseDowns = Set.add mouseButton world.MouseState.MouseDowns } world mouseStateLens
                    let messageAddress = addr ("down/mouse" </> str mouseButton)
                    let messageData = MouseButtonData (world'.MouseState.MousePosition, mouseButton)
                    publish messageAddress { Handled = false; Data = messageData } world'
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mouseState = world.MouseState
                    let mouseButton = makeMouseButton event.button.button
                    if Set.contains mouseButton mouseState.MouseDowns then
                        let world' = set { world.MouseState with MouseDowns = Set.remove mouseButton world.MouseState.MouseDowns } world mouseStateLens
                        let messageAddress = addr ("up/mouse" </> str mouseButton)
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