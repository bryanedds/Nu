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
open Nu.Core
open Nu.Constants
open Nu.Math
open Nu.Physics
open Nu.Audio
open Nu.Rendering
open Nu.Metadata
open Nu.DomainModel
open Nu.CameraModule

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

module Entities =

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