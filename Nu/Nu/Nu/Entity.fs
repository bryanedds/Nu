module Nu.Entity
open System
open System.Reflection
open System.Xml
open System.Xml.Serialization
open FSharpx
open FSharpx.Lens.Operators
open OpenTK
open TiledSharp
open Nu.Core
open Nu.Physics
open Nu.Audio
open Nu.Rendering
open Nu.DomainModel

type [<StructuralEquality; NoComparison; CLIMutable>] Entity =
    { Id : Id
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
      PhysicsIds : Id list
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

(*let writeEntityToXml (writer : XmlWriter) entity =
    writer.WriteStartElement typeof<Entity>.Name
    writeNuProperties writer entity
    match entity.Subtype with
    | Gui gui ->
        writeSubtypeToXml writer "Nu.Entity+Gui" "Nu.Entity+EntitySubtype+Gui" "" gui
        match gui.SubSubtype with
        | Button button -> writeSubtypeToXml writer "Nu.Entity+Button" "Nu.Entity+GuiSubtype+Button" "Sub" button
        | Label label -> writeSubtypeToXml writer "Nu.Entity+Label" "Nu.Entity+GuiSubtype+Label" "Sub" label
        | TextBox textBox -> writeSubtypeToXml writer "Nu.Entity+TextBox" "Nu.Entity+GuiSubtype+TextBox" "Sub" textBox
        | Toggle toggle -> writeSubtypeToXml writer "Nu.Entity+Toggle" "Nu.Entity+GuiSubtype+Toggle" "Sub" toggle
        | Feeler feeler -> writeSubtypeToXml writer "Nu.Entity+Feeler" "Nu.Entity+GuiSubtype+Feeler" "Sub" feeler
    | Actor actor ->
        writeSubtypeToXml writer "Nu.Entity+Actor" "Nu.Entity+EntitySubtype+Actor" "" actor
        match actor.SubSubtype with
        | Block block -> writeSubtypeToXml writer "Nu.Entity+Block" "Nu.Entity+ActorSubtype+Block" "Sub" block
        | Avatar avatar -> writeSubtypeToXml writer "Nu.Entity+Avatar" "Nu.Entity+ActorSubtype+Avatar" "Sub" avatar
        | TileMap tileMap -> writeSubtypeToXml writer "Nu.Entity+TileMap" "Nu.Entity+ActorSubtype+TileMap" "Sub" tileMap
    writer.WriteEndElement ()

let readEntityFromXml (reader : XmlReader) =

    // read start
    reader.ReadStartElement "Entity"

    // read simple properties
    let entityAssemblyName = "Nu"
    let entityType = typeof<Entity>
    let entity = (Activator.CreateInstance (entityAssemblyName, entityType.FullName)).Unwrap () :?> Entity
    readNuProperties reader entity

    // read subtype
    let subtypeRecord = readSubtypeFromXml reader entityAssemblyName "" entity

    // read sub-subtype
    ignore <| readSubtypeFromXml reader entityAssemblyName "Sub" subtypeRecord

    // read end
    reader.ReadEndElement ()*)