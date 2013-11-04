module Nu.Entity
open System
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
open Nu.Physics
open Nu.Audio
open Nu.Rendering
open Nu.AssetMetadata
open Nu.DomainModel

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
    static member sep (gui : Gui) = (gui, gui.Entity)
    static member cmb (gui : Gui, entity) = { gui with Entity = entity }

type [<StructuralEquality; NoComparison; CLIMutable>] Button =
    { Gui : Gui
      IsDown : bool
      UpSprite : Sprite
      DownSprite : Sprite
      ClickSound : Sound }
    static member sep (button : Button) = (button, button.Gui, button.Gui.Entity)
    static member cmb (button : Button, gui, entity) = { button with Gui = { gui with Entity = entity }}

type [<StructuralEquality; NoComparison; CLIMutable>] Label =
    { Gui : Gui
      LabelSprite : Sprite }
    static member sep (label : Label) = (label, label.Gui, label.Gui.Entity)
    static member cmb (label : Label, gui, entity) = { label with Gui = { gui with Entity = entity }}

type [<StructuralEquality; NoComparison; CLIMutable>] TextBox =
    { Gui : Gui
      BoxSprite : Sprite
      Text : string
      TextFont : Font
      TextOffset : Vector2
      TextColor : Vector4 }
    static member sep (textBox : TextBox) = (textBox, textBox.Gui, textBox.Gui.Entity)
    static member cmb (textBox : TextBox, gui, entity) = { textBox with Gui = { gui with Entity = entity }}

type [<StructuralEquality; NoComparison; CLIMutable>] Toggle =
    { Gui : Gui
      IsOn : bool
      IsPressed : bool
      OffSprite : Sprite
      OnSprite : Sprite
      ToggleSound : Sound }
    static member sep (toggle : Toggle) = (toggle, toggle.Gui, toggle.Gui.Entity)
    static member cmb (toggle : Toggle, gui, entity) = { toggle with Gui = { gui with Entity = entity }}

type [<StructuralEquality; NoComparison; CLIMutable>] Feeler =
    { Gui : Gui
      IsTouched : bool }
    static member sep (feeler : Feeler) = (feeler, feeler.Gui, feeler.Gui.Entity)
    static member cmb (feeler : Feeler, gui, entity) = { feeler with Gui = { gui with Entity = entity }}

type [<StructuralEquality; NoComparison; CLIMutable>] Actor =
    { Entity : Entity
      Position : Vector2
      Depth : single
      Size : Vector2
      Rotation : single }      
    static member sep (actor : Actor) = actor.Entity
    static member cmb (actor : Actor, entity) = { actor with Entity = entity }
      
type [<StructuralEquality; NoComparison; CLIMutable>] Block =
    { Actor : Actor
      PhysicsId : Id
      Density : single
      BodyType : BodyType
      Sprite : Sprite }
    static member sep (block : Block) = (block, block.Actor, block.Actor.Entity)
    static member cmb (block : Block, actor, entity) = { block with Actor = { actor with Entity = entity }}

type [<StructuralEquality; NoComparison; CLIMutable>] Avatar =
    { Actor : Actor
      PhysicsId : Id
      Density : single
      Sprite : Sprite }
    static member sep (avatar : Avatar) = (avatar, avatar.Actor, avatar.Actor.Entity)
    static member cmb (avatar : Avatar, actor, entity) = { avatar with Actor = { actor with Entity = entity }}
      
type [<StructuralEquality; NoComparison; CLIMutable>] TileMap =
    { Actor : Actor
      //PhysicsIds : Id list
      Density : single
      TileMapAsset : TileMapAsset
      TmxMap : TmxMap
      TileMapMetadata : Sprite list }
    static member sep (tileMap : TileMap) = (tileMap, tileMap.Actor, tileMap.Actor.Entity)
    static member cmb (tileMap : TileMap, actor, entity) = { tileMap with Actor = { actor with Entity = entity }}

type [<StructuralEquality; NoComparison>] EntityModel =
    | Button of Button
    | Label of Label
    | TextBox of TextBox
    | Toggle of Toggle
    | Feeler of Feeler
    | Block of Block
    | Avatar of Avatar
    | TileMap of TileMap

    static member entity =
        { Get = fun this ->
            match this with
            | Button button -> button.Gui.Entity
            | Label label -> label.Gui.Entity
            | TextBox textBox -> textBox.Gui.Entity
            | Toggle toggle -> toggle.Gui.Entity
            | Feeler feeler -> feeler.Gui.Entity
            | Block block -> block.Actor.Entity
            | Avatar avatar -> avatar.Actor.Entity
            | TileMap tileMap -> tileMap.Actor.Entity
          Set = fun entity this ->
            match this with
            | Button button -> Button { button with Gui = { button.Gui with Entity = entity }}
            | Label label -> Label { label with Gui = { label.Gui with Entity = entity }}
            | TextBox textBox -> TextBox { textBox with Gui = { textBox.Gui with Entity = entity }}
            | Toggle toggle -> Toggle { toggle with Gui = { toggle.Gui with Entity = entity }}
            | Feeler feeler -> Feeler { feeler with Gui = { feeler.Gui with Entity = entity }}
            | Block block -> Block { block with Actor = { block.Actor with Entity = entity }}
            | Avatar avatar -> Avatar { avatar with Actor = { avatar.Actor with Entity = entity }}
            | TileMap tileMap -> TileMap { tileMap with Actor = { tileMap.Actor with Entity = entity }}}

    static member optGui =
        { Get = fun this ->
            match this with
            | Button button -> Some button.Gui
            | Label label -> Some label.Gui
            | TextBox textBox -> Some textBox.Gui
            | Toggle toggle -> Some toggle.Gui
            | Feeler feeler -> Some feeler.Gui
            | Block _ | Avatar _ | TileMap _ -> None
          Set = fun optGui this ->
            let gui = Option.get optGui
            match this with
            | Button button -> Button { button with Gui = gui }
            | Label label -> Label { label with Gui = gui }
            | TextBox textBox -> TextBox { textBox with Gui = gui }
            | Toggle toggle -> Toggle { toggle with Gui = gui }
            | Feeler feeler -> Feeler { feeler with Gui = gui }
            | Block _ | Avatar _ | TileMap _ -> failwith "Entity is not a gui." }

    static member gui =
        { Get = fun this -> Option.get (get this EntityModel.optGui)
          Set = fun gui this -> set (Some gui) this EntityModel.optGui }

    static member optButton =
        { Get = fun this -> match this with Button button -> Some button | _ -> None
          Set = fun optButton this -> Button <| Option.get optButton }

    static member button =
        { Get = fun this -> Option.get (get this EntityModel.optButton)
          Set = fun button this -> set (Some button) this EntityModel.optButton }

    static member optLabel =
        { Get = fun this -> match this with Label label -> Some label | _ -> None
          Set = fun optLabel this -> Label <| Option.get optLabel }

    static member label =
        { Get = fun this -> Option.get (get this EntityModel.optLabel)
          Set = fun label this -> set (Some label) this EntityModel.optLabel }

    static member optTextBox =
        { Get = fun this -> match this with TextBox textBox -> Some textBox | _ -> None
          Set = fun optTextBox this -> TextBox <| Option.get optTextBox }

    static member textBox =
        { Get = fun this -> Option.get (get this EntityModel.optTextBox)
          Set = fun textBox this -> set (Some textBox) this EntityModel.optTextBox }

    static member optToggle =
        { Get = fun this -> match this with Toggle toggle -> Some toggle | _ -> None
          Set = fun optToggle this -> Toggle <| Option.get optToggle }

    static member toggle =
        { Get = fun this -> Option.get (get this EntityModel.optToggle)
          Set = fun toggle this -> set (Some toggle) this EntityModel.optToggle }

    static member optFeeler =
        { Get = fun this -> match this with Feeler feeler -> Some feeler | _ -> None
          Set = fun optFeeler this -> Feeler <| Option.get optFeeler }

    static member feeler =
        { Get = fun this -> Option.get (get this EntityModel.optFeeler)
          Set = fun feeler this -> set (Some feeler) this EntityModel.optFeeler }

    static member optActor =
        { Get = fun this ->
            match this with
            | Button _ | Label _ | TextBox _ | Toggle _ | Feeler _ -> None
            | Block block -> Some block.Actor
            | Avatar avatar -> Some avatar.Actor
            | TileMap tileMap -> Some tileMap.Actor
          Set = fun optActor this ->
            let actor = Option.get optActor
            match this with
            | Button _ | Label _ | TextBox _ | Toggle _ | Feeler _ -> failwith "EntityModel is not an actor."
            | Block block -> Block { block with Actor = actor }
            | Avatar avatar -> Avatar { avatar with Actor = actor }
            | TileMap tileMap -> TileMap { tileMap with Actor = actor }}

    static member actor =
        { Get = fun this -> Option.get (get this EntityModel.optActor)
          Set = fun actor this -> set (Some actor) this EntityModel.optActor }

    static member optBlock =
        { Get = fun this -> match this with Block block -> Some block | _ -> None
          Set = fun optBlock this -> Block <| Option.get optBlock }

    static member block =
        { Get = fun this -> Option.get (get this EntityModel.optBlock)
          Set = fun block this -> set (Some block) this EntityModel.optBlock }

    static member optAvatar =
        { Get = fun this -> match this with Avatar avatar -> Some avatar | _ -> None
          Set = fun optAvatar this -> Avatar <| Option.get optAvatar }

    static member avatar =
        { Get = fun this -> Option.get (get this EntityModel.optAvatar)
          Set = fun avatar this -> set (Some avatar) this EntityModel.optAvatar }

    static member optTileMap =
        { Get = fun this -> match this with TileMap tileMap -> Some tileMap | _ -> None
          Set = fun optTileMap this -> TileMap <| Option.get optTileMap }

    static member tileMap =
        { Get = fun this -> Option.get (get this EntityModel.optTileMap)
          Set = fun tileMap this -> set (Some tileMap) this EntityModel.optTileMap }

let makeDefaultEntity () =
    let id = getNuId ()
    { Id = id
      Name = str id
      Enabled = true
      Visible = true }

let makeDefaultGui () =
    { Gui.Entity = makeDefaultEntity ()
      Position = Vector2.Zero
      Depth = 0.0f
      Size = Vector2.One }

let makeDefaultActor () =
    { Actor.Entity = makeDefaultEntity ()
      Position = Vector2.Zero
      Depth = 0.0f
      Size = Vector2.One
      Rotation = 0.0f }

let makeDefaultEntityModel typeName =
    let entityModel = (Activator.CreateInstance ("Nu", typeName, false, BindingFlags.Instance ||| BindingFlags.NonPublic, null, [|null|], null, null)).Unwrap () :?> EntityModel
    match entityModel with
    | Button _ ->
        Button
            { Gui = makeDefaultGui ()
              IsDown = false
              UpSprite = { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              DownSprite = { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              ClickSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
    | Label _ ->
        Label
            { Gui = makeDefaultGui ()
              LabelSprite = { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
    | TextBox _ ->
        TextBox
            { Gui = makeDefaultGui ()
              BoxSprite = { SpriteAssetName = Lun.make "Image4"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              Text = String.Empty
              TextFont = { FontAssetName = Lun.make "Font"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              TextOffset = Vector2.Zero
              TextColor = Vector4.One }
    | Toggle _ ->
        Toggle
            { Gui = makeDefaultGui ()
              IsOn = false
              IsPressed = false
              OffSprite = { SpriteAssetName = Lun.make "Image"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              OnSprite = { SpriteAssetName = Lun.make "Image2"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              ToggleSound = { SoundAssetName = Lun.make "Sound"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
    | Feeler _ ->
        Feeler
            { Gui = makeDefaultGui ()
              IsTouched = false }
    | Block _ ->
        Block
            { Actor = makeDefaultActor ()
              PhysicsId = getPhysicsId ()
              Density = NormalDensity
              BodyType = BodyType.Dynamic
              Sprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
    | Avatar _ ->
        Avatar
            { Actor = makeDefaultActor ()
              PhysicsId = getPhysicsId ()
              Density = NormalDensity
              Sprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }}
    | TileMap _ ->
        let tmxMap = TmxMap "TileMap.tmx"
        TileMap
            { Actor = makeDefaultActor ()
              Density = NormalDensity
              TileMapAsset = { TileMapAssetName = Lun.make "TileMap"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              TmxMap = tmxMap
              TileMapMetadata = [{ SpriteAssetName = Lun.make "TileSet"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }]}

let writeEntityModelToXml (writer : XmlWriter) entityModel =
    writer.WriteStartElement typeof<EntityModel>.Name
    match entityModel with
    | Button button -> writeModelPropertiesMany writer "Nu.Entity+EntityModel+Button" [button :> obj; button.Gui :> obj; button.Gui.Entity :> obj]
    | Label label -> writeModelPropertiesMany writer "Nu.Entity+EntityModel+Label" [label :> obj; label.Gui :> obj; label.Gui.Entity :> obj]
    | TextBox textBox -> writeModelPropertiesMany writer "Nu.Entity+EntityModel+TextBox" [textBox :> obj; textBox.Gui :> obj; textBox.Gui.Entity :> obj]
    | Toggle toggle -> writeModelPropertiesMany writer "Nu.Entity+EntityModel+Toggle" [toggle :> obj; toggle.Gui :> obj; toggle.Gui.Entity :> obj]
    | Feeler feeler -> writeModelPropertiesMany writer "Nu.Entity+EntityModel+Feeler" [feeler :> obj; feeler.Gui :> obj; feeler.Gui.Entity :> obj]
    | Block block -> writeModelPropertiesMany writer "Nu.Entity+EntityModel+Block" [block :> obj; block.Actor :> obj; block.Actor.Entity :> obj]
    | Avatar avatar -> writeModelPropertiesMany writer "Nu.Entity+EntityModel+Avatar" [avatar :> obj; avatar.Actor :> obj; avatar.Actor.Entity :> obj]
    | TileMap tileMap -> writeModelPropertiesMany writer "Nu.Entity+EntityModel+TileMap" [tileMap :> obj; tileMap.Actor :> obj; tileMap.Actor.Entity :> obj]
    writer.WriteEndElement ()

let loadEntityModelFromXml (entityModelNode : XmlNode) =
    let entityModelTypeNode = entityModelNode.Item "ModelType"
    let entityModelTypeName = entityModelTypeNode.InnerText
    let entityModel = makeDefaultEntityModel entityModelTypeName
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