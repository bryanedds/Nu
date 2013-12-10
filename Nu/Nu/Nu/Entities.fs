namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open FSharpx
open FSharpx.Lens.Operators
open OpenTK
open TiledSharp
open Nu
open Nu.Core
open Nu.Constants
open Nu.Math
open Nu.Physics
open Nu.Audio
open Nu.Rendering
open Nu.Metadata
open Nu.DomainModel
open Nu.CameraModule
module Entities =

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
        let optGroupMap = Map.tryFind address.[0] world.EntityModels
        match optGroupMap with
        | None -> None
        | Some groupMap ->
            let optEntityMap = Map.tryFind address.[1] groupMap
            match optEntityMap with
            | None -> None
            | Some entityMap -> Map.tryFind address.[2] entityMap

    let worldEntityModelAdder (address : Address) world (child : EntityModel) =
        let optGroupMap = Map.tryFind address.[0] world.EntityModels
        match optGroupMap with
        | None ->
            let entityMap = Map.singleton address.[2] child
            let groupMap = Map.singleton address.[1] entityMap
            { world with EntityModels = Map.add address.[0] groupMap world.EntityModels }
        | Some groupMap ->
            let optEntityMap = Map.tryFind address.[1] groupMap
            match optEntityMap with
            | None ->
                let entityMap = Map.singleton address.[2] child
                let groupMap' = Map.add address.[1] entityMap groupMap
                { world with EntityModels = Map.add address.[0] groupMap' world.EntityModels }
            | Some entityMap ->
                let entityMap' = Map.add address.[2] child entityMap
                let groupMap' = Map.add address.[1] entityMap' groupMap
                { world with EntityModels = Map.add address.[0] groupMap' world.EntityModels }

    let worldEntityModelRemover (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.EntityModels
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let optEntityMap = Map.tryFind address.[1] groupMap
            match optEntityMap with
            | None -> world
            | Some entityMap ->
                let entityMap' = Map.remove address.[2] entityMap
                let groupMap' = Map.add address.[1] entityMap' groupMap
                { world with EntityModels = Map.add address.[0] groupMap' world.EntityModels }

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
                match Map.tryFind screenLun world.EntityModels with
                | None -> Map.empty
                | Some groupMap ->
                    match Map.tryFind groupLun groupMap with
                    | None -> Map.empty
                    | Some entityMap -> entityMap
            | _ -> failwith <| "Invalid entity model address '" + str address + "'."
          Set = fun entityModels world ->
            match address with
            | [screenLun; groupLun] ->
                match Map.tryFind screenLun world.EntityModels with
                | None -> { world with EntityModels = Map.add screenLun (Map.singleton groupLun entityModels) world.EntityModels }
                | Some groupMap ->
                    match Map.tryFind groupLun groupMap with
                    | None -> { world with EntityModels = Map.add screenLun (Map.add groupLun entityModels groupMap) world.EntityModels }
                    | Some entityMap -> { world with EntityModels = Map.add screenLun (Map.add groupLun (Map.addMany (Map.toSeq entityModels) entityMap) groupMap) world.EntityModels }
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

    let makeTileMapData tileMap =
        let actor = tileMap.Actor
        let map = tileMap.TmxMap
        let mapSize = (map.Width, map.Height)
        let tileSize = (map.TileWidth, map.TileHeight)
        let tileSizeF = Vector2 (single <| fst tileSize, single <| snd tileSize)
        let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
        let optTileSetWidth = tileSet.Image.Width
        let optTileSetHeight = tileSet.Image.Height
        let tileSetSize = (optTileSetWidth.Value / fst tileSize, optTileSetHeight.Value / snd tileSize)
        { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

    let makeTileLayerData tileMap tmd (layerIndex : int) =
        let layer = tmd.Map.Layers.[layerIndex]
        let tiles = layer.Tiles
        { Layer = layer; Tiles = tiles }

    let makeTileData tileMap tmd tld n =
        let (i, j) = (n % fst tmd.MapSize, n / snd tmd.MapSize)
        let tile = tld.Tiles.[n]
        let gid = tile.Gid - tmd.TileSet.FirstGid
        let gidPosition = gid * fst tmd.TileSize
        let gid2 = (gid % fst tmd.TileSetSize, gid / snd tmd.TileSetSize)
        let tilePosition = (int tileMap.Actor.Position.X + (fst tmd.TileSize * i), int tileMap.Actor.Position.Y + (snd tmd.TileSize * j))
        let optTileSetTile = Seq.tryFind (fun (tileSetTile' : TmxTilesetTile) -> tile.Gid - 1 = tileSetTile'.Id) tmd.TileSet.Tiles
        let tileSetPosition = (gidPosition % fst tmd.TileSetSize, gidPosition / snd tmd.TileSetSize * snd tmd.TileSize)
        { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile; TileSetPosition = tileSetPosition }

    let makeDefaultEntity optName =
        let id = getNuId ()
        { Id = id
          Name = match optName with None -> str id | Some name -> name
          Enabled = true
          Visible = true }

    let makeDefaultGui optName =
        { Gui.Entity = makeDefaultEntity optName
          Position = Vector2.Zero
          Depth = 0.0f
          Size = Vector2.One }

    let makeDefaultActor optName =
        { Actor.Entity = makeDefaultEntity optName
          Position = Vector2.Zero
          Depth = 0.0f
          Size = Vector2.One
          Rotation = 0.0f }

    let makeDefaultEntityModel typeName optName =
        let assemblyName = (Assembly.GetExecutingAssembly ()).FullName
        let entityModel = (Activator.CreateInstance (assemblyName, typeName, false, BindingFlags.Instance ||| BindingFlags.NonPublic, null, [|null|], null, null)).Unwrap () :?> EntityModel
        match entityModel with
        | Button _ ->
            Button
                { Gui = makeDefaultGui optName
                  IsDown = false
                  UpSprite = { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
                  DownSprite = { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
                  ClickSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
        | Label _ ->
            Label
                { Gui = makeDefaultGui optName
                  LabelSprite = { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
        | TextBox _ ->
            TextBox
                { Gui = makeDefaultGui optName
                  BoxSprite = { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
                  Text = String.Empty
                  TextFont = { FontAssetName = Lun.make "Font"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
                  TextOffset = Vector2.Zero
                  TextColor = Vector4.One }
        | Toggle _ ->
            Toggle
                { Gui = makeDefaultGui optName
                  IsOn = false
                  IsPressed = false
                  OffSprite = { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
                  OnSprite = { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
                  ToggleSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
        | Feeler _ ->
            Feeler
                { Gui = makeDefaultGui optName
                  IsTouched = false }
        | Block _ ->
            Block
                { Actor = makeDefaultActor optName
                  PhysicsId = getPhysicsId ()
                  Density = NormalDensity
                  BodyType = BodyType.Dynamic
                  Sprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
        | Avatar _ ->
            Avatar
                { Actor = makeDefaultActor optName
                  PhysicsId = getPhysicsId ()
                  Density = NormalDensity
                  Sprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
        | TileMap _ ->
            let tmxMap = TmxMap "Assets/Default/TileMap.tmx"
            TileMap
                { Actor = makeDefaultActor optName
                  PhysicsIds = []
                  Density = NormalDensity
                  TileMapAsset = { TileMapAssetName = Lun.make "TileMap"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
                  TmxMap = tmxMap
                  TileMapSprites = [{ SpriteAssetName = Lun.make "TileSet"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }]}

    let writeEntityModelToXml (writer : XmlWriter) entityModel =
        writer.WriteStartElement typeof<EntityModel>.Name
        match entityModel with
        | Button button -> writeModelPropertiesMany writer "Nu.EntityModel+Button" [button :> obj; button.Gui :> obj; button.Gui.Entity :> obj]
        | Label label -> writeModelPropertiesMany writer "Nu.EntityModel+Label" [label :> obj; label.Gui :> obj; label.Gui.Entity :> obj]
        | TextBox textBox -> writeModelPropertiesMany writer "Nu.EntityModel+TextBox" [textBox :> obj; textBox.Gui :> obj; textBox.Gui.Entity :> obj]
        | Toggle toggle -> writeModelPropertiesMany writer "Nu.EntityModel+Toggle" [toggle :> obj; toggle.Gui :> obj; toggle.Gui.Entity :> obj]
        | Feeler feeler -> writeModelPropertiesMany writer "Nu.EntityModel+Feeler" [feeler :> obj; feeler.Gui :> obj; feeler.Gui.Entity :> obj]
        | Block block -> writeModelPropertiesMany writer "Nu.EntityModel+Block" [block :> obj; block.Actor :> obj; block.Actor.Entity :> obj]
        | Avatar avatar -> writeModelPropertiesMany writer "Nu.EntityModel+Avatar" [avatar :> obj; avatar.Actor :> obj; avatar.Actor.Entity :> obj]
        | TileMap tileMap -> writeModelPropertiesMany writer "Nu.EntityModel+TileMap" [tileMap :> obj; tileMap.Actor :> obj; tileMap.Actor.Entity :> obj]
        writer.WriteEndElement ()

    let loadEntityModelFromXml (entityModelNode : XmlNode) =
        let entityModelTypeNode = entityModelNode.Item "ModelType"
        let entityModelTypeName = entityModelTypeNode.InnerText
        let entityModel = makeDefaultEntityModel entityModelTypeName None // TODO: consider setting the name here
        match entityModel with
        | Button button -> setModelProperties4<Button, Gui, Entity> (fun obj -> obj.Gui) (fun obj -> obj.Gui.Entity) entityModelNode button
        | Label label -> setModelProperties4<Label, Gui, Entity> (fun obj -> obj.Gui) (fun obj -> obj.Gui.Entity) entityModelNode label
        | TextBox textBox -> setModelProperties4<TextBox, Gui, Entity> (fun obj -> obj.Gui) (fun obj -> obj.Gui.Entity) entityModelNode textBox
        | Toggle toggle -> setModelProperties4<Toggle, Gui, Entity> (fun obj -> obj.Gui) (fun obj -> obj.Gui.Entity) entityModelNode toggle
        | Feeler feeler -> setModelProperties4<Feeler, Gui, Entity> (fun obj -> obj.Gui) (fun obj -> obj.Gui.Entity) entityModelNode feeler
        | Block block -> setModelProperties4<Block, Actor, Entity> (fun obj -> obj.Actor) (fun obj -> obj.Actor.Entity) entityModelNode block
        | Avatar avatar -> setModelProperties4<Avatar, Actor, Entity> (fun obj -> obj.Actor) (fun obj -> obj.Actor.Entity) entityModelNode avatar
        | TileMap tileMap -> setModelProperties4<TileMap, Actor, Entity> (fun obj -> obj.Actor) (fun obj -> obj.Actor.Entity) entityModelNode tileMap
        entityModel