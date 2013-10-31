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
open Nu.DataModel

type [<StructuralEquality; NoComparison; CLIMutable>] EntityRcd =
    { Id : Id
      Enabled : bool
      Visible : bool }

type [<StructuralEquality; NoComparison; CLIMutable>] Gui =
    { EntityRcd : EntityRcd
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
    { EntityRcd : EntityRcd
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
      TileMapMetadata : Sprite list }

type [<StructuralEquality; NoComparison>] Entity =
    | Button of Button
    | Label of Label
    | TextBox of TextBox
    | Toggle of Toggle
    | Feeler of Feeler
    | Block of Block
    | Avatar of Avatar
    | TileMap of TileMap

    static member entity =
        { Get = fun (this : Entity) -> this
          Set = fun entity _ -> entity }

    static member entityRcd =
        { Get = fun this ->
            match this with
            | Button button -> button.Gui.EntityRcd
            | Label label -> label.Gui.EntityRcd
            | TextBox textBox -> textBox.Gui.EntityRcd
            | Toggle toggle -> toggle.Gui.EntityRcd
            | Feeler feeler -> feeler.Gui.EntityRcd
            | Block block -> block.Actor.EntityRcd
            | Avatar avatar -> avatar.Actor.EntityRcd
            | TileMap tileMap -> tileMap.Actor.EntityRcd
          Set = fun entityRcd this ->
            match this with
            | Button button -> Button { button with Gui = { button.Gui with EntityRcd = entityRcd }}
            | Label label -> Label { label with Gui = { label.Gui with EntityRcd = entityRcd }}
            | TextBox textBox -> TextBox { textBox with Gui = { textBox.Gui with EntityRcd = entityRcd }}
            | Toggle toggle -> Toggle { toggle with Gui = { toggle.Gui with EntityRcd = entityRcd }}
            | Feeler feeler -> Feeler { feeler with Gui = { feeler.Gui with EntityRcd = entityRcd }}
            | Block block -> Block { block with Actor = { block.Actor with EntityRcd = entityRcd }}
            | Avatar avatar -> Avatar { avatar with Actor = { avatar.Actor with EntityRcd = entityRcd }}
            | TileMap tileMap -> TileMap { tileMap with Actor = { tileMap.Actor with EntityRcd = entityRcd }}}

    static member optGui =
        { Get = fun this ->
            match this with
            | Button button -> Some (button.Gui, button.Gui.EntityRcd)
            | Label label -> Some (label.Gui, label.Gui.EntityRcd)
            | TextBox textBox -> Some (textBox.Gui, textBox.Gui.EntityRcd)
            | Toggle toggle -> Some (toggle.Gui, toggle.Gui.EntityRcd)
            | Feeler feeler -> Some (feeler.Gui, feeler.Gui.EntityRcd)
            | Block _ | Avatar _ | TileMap _ -> None
          Set = fun optGui this ->
            let (gui, entityRcd) = Option.get optGui
            match this with
            | Button button -> Button { button with Gui = { gui with EntityRcd = entityRcd }}
            | Label label -> Label { label with Gui = { gui with EntityRcd = entityRcd }}
            | TextBox textBox -> TextBox { textBox with Gui = { gui with EntityRcd = entityRcd }}
            | Toggle toggle -> Toggle { toggle with Gui = { gui with EntityRcd = entityRcd }}
            | Feeler feeler -> Feeler { feeler with Gui = { gui with EntityRcd = entityRcd }}
            | Block _ | Avatar _ | TileMap _ -> failwith "Entity is not a gui." }

    static member gui =
        { Get = fun this -> Option.get (get this Entity.optGui)
          Set = fun gui this -> set (Some gui) this Entity.optGui }

    static member optButton =
        { Get = fun this -> match this with Button button -> Some (button, button.Gui, button.Gui.EntityRcd) | _ -> None
          Set = fun optButton this -> let (button, gui, entityRcd) = Option.get optButton in Button { button with Gui = { gui with EntityRcd = entityRcd }}}

    static member button =
        { Get = fun this -> Option.get (get this Entity.optButton)
          Set = fun button this -> set (Some button) this Entity.optButton }

    static member optLabel =
        { Get = fun this -> match this with Label label -> Some (label, label.Gui, label.Gui.EntityRcd) | _ -> None
          Set = fun optLabel this -> let (label, gui, entityRcd) = Option.get optLabel in Label { label with Gui = { gui with EntityRcd = entityRcd }}}

    static member label =
        { Get = fun this -> Option.get (get this Entity.optLabel)
          Set = fun label this -> set (Some label) this Entity.optLabel }

    static member optTextBox =
        { Get = fun this -> match this with TextBox textBox -> Some (textBox, textBox.Gui, textBox.Gui.EntityRcd) | _ -> None
          Set = fun optTextBox this -> let (textBox, gui, entityRcd) = Option.get optTextBox in TextBox { textBox with Gui = { gui with EntityRcd = entityRcd }}}

    static member textBox =
        { Get = fun this -> Option.get (get this Entity.optTextBox)
          Set = fun textBox this -> set (Some textBox) this Entity.optTextBox }

    static member optToggle =
        { Get = fun this -> match this with Toggle toggle -> Some (toggle, toggle.Gui, toggle.Gui.EntityRcd) | _ -> None
          Set = fun optToggle this -> let (toggle, gui, entityRcd) = Option.get optToggle in Toggle { toggle with Gui = { gui with EntityRcd = entityRcd }}}

    static member toggle =
        { Get = fun this -> Option.get (get this Entity.optToggle)
          Set = fun toggle this -> set (Some toggle) this Entity.optToggle }

    static member optFeeler =
        { Get = fun this -> match this with Feeler feeler -> Some (feeler, feeler.Gui, feeler.Gui.EntityRcd) | _ -> None
          Set = fun optFeeler this -> let (feeler, gui, entityRcd) = Option.get optFeeler in Feeler { feeler with Gui = { gui with EntityRcd = entityRcd }}}

    static member feeler =
        { Get = fun this -> Option.get (get this Entity.optFeeler)
          Set = fun feeler this -> set (Some feeler) this Entity.optFeeler }

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
            | Button _ | Label _ | TextBox _ | Toggle _ | Feeler _ -> failwith "Entity is not an actor."
            | Block block -> Block { block with Actor = actor }
            | Avatar avatar -> Avatar { avatar with Actor = actor }
            | TileMap tileMap -> TileMap { tileMap with Actor = actor }}

    static member actor =
        { Get = fun this -> Option.get (get this Entity.optActor)
          Set = fun actor this -> set (Some actor) this Entity.optActor }

    static member optBlock =
        { Get = fun this -> match this with Block block -> Some (block, block.Actor, block.Actor.EntityRcd) | _ -> None
          Set = fun optBlock this -> let (block, actor, entityRcd) = Option.get optBlock in Block { block with Actor = { actor with EntityRcd = entityRcd }}}

    static member block =
        { Get = fun this -> Option.get (get this Entity.optBlock)
          Set = fun block this -> set (Some block) this Entity.optBlock }

    static member optAvatar =
        { Get = fun this -> match this with Avatar avatar -> Some (avatar, avatar.Actor, avatar.Actor.EntityRcd) | _ -> None
          Set = fun optAvatar this -> let (avatar, actor, entityRcd) = Option.get optAvatar in Avatar { avatar with Actor = { actor with EntityRcd = entityRcd }}}

    static member avatar =
        { Get = fun this -> Option.get (get this Entity.optAvatar)
          Set = fun avatar this -> set (Some avatar) this Entity.optAvatar }

    static member optTileMap =
        { Get = fun this -> match this with TileMap tileMap -> Some (tileMap, tileMap.Actor, tileMap.Actor.EntityRcd) | _ -> None
          Set = fun optTileMap this -> let (tileMap, actor, entityRcd) = Option.get optTileMap in TileMap { tileMap with Actor = { actor with EntityRcd = entityRcd }}}

    static member tileMap =
        { Get = fun this -> Option.get (get this Entity.optTileMap)
          Set = fun tileMap this -> set (Some tileMap) this Entity.optTileMap }

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