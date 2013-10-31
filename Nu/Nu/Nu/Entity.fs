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

type [<StructuralEquality; NoComparison; CLIMutable>] Button =
    { IsDown : bool
      UpSprite : Sprite
      DownSprite : Sprite
      ClickSound : Sound }

type [<StructuralEquality; NoComparison; CLIMutable>] Label =
    { LabelSprite : Sprite }

type [<StructuralEquality; NoComparison; CLIMutable>] TextBox =
    { BoxSprite : Sprite
      Text : string
      TextFont : Font
      TextOffset : Vector2
      TextColor : Vector4 }

type [<StructuralEquality; NoComparison; CLIMutable>] Toggle =
    { IsOn : bool
      IsPressed : bool
      OffSprite : Sprite
      OnSprite : Sprite
      ToggleSound : Sound }

type [<StructuralEquality; NoComparison; CLIMutable>] Feeler =
    { IsTouched : bool }

/// An algabraically-closed semantics for game gui elements.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] GuiSubtype =
    | Button of Button
    | Label of Label
    | TextBox of TextBox
    | Toggle of Toggle
    | Feeler of Feeler
 // | ...additional controls
 // | UserDefinedGui of IUserDefinedGui (* this would give us more open gui semantics, but perhaps at the cost of its value semantics...  *)

/// A game gui element.
/// A serializable value type.
type [<StructuralEquality; NoComparison; CLIMutable>] Gui =
    { Position : Vector2
      Depth : single
      Size : Vector2
      SubSubtype : GuiSubtype }

    static member button =
        { Get = fun this -> match this.SubSubtype with Button button -> button | _ -> failwith "Gui is not a button."
          Set = fun button this -> { this with SubSubtype = Button button }}

    static member optButton =
        { Get = fun this -> match this.SubSubtype with Button button -> Some button | _ -> None
          Set = fun optButton this -> match optButton with None -> failwith "Cannot set subtype to None." | Some button -> { this with SubSubtype = Button button }}
    
    static member label =
        { Get = fun this -> match this.SubSubtype with Label label -> label | _ -> failwith "Gui is not a label."
          Set = fun label this -> { this with SubSubtype = Label label }}

    static member optLabel =
        { Get = fun this -> match this.SubSubtype with Label label -> Some label | _ -> None
          Set = fun optButton this -> match optButton with None -> failwith "Cannot set subtype to None." | Some label -> { this with SubSubtype = Label label }}
    
    static member textBox =
        { Get = fun this -> match this.SubSubtype with TextBox textBox -> textBox | _ -> failwith "Gui is not a textBox."
          Set = fun textBox this -> { this with SubSubtype = TextBox textBox }}

    static member optTextBox =
        { Get = fun this -> match this.SubSubtype with TextBox textBox -> Some textBox | _ -> None
          Set = fun optButton this -> match optButton with None -> failwith "Cannot set subtype to None." | Some textBox -> { this with SubSubtype = TextBox textBox }}
    
    static member toggle =
        { Get = fun this -> match this.SubSubtype with Toggle toggle -> toggle | _ -> failwith "Gui is not a toggle."
          Set = fun toggle this -> { this with SubSubtype = Toggle toggle }}

    static member optToggle =
        { Get = fun this -> match this.SubSubtype with Toggle toggle -> Some toggle | _ -> None
          Set = fun optButton this -> match optButton with None -> failwith "Cannot set subtype to None." | Some toggle -> { this with SubSubtype = Toggle toggle }}
    
    static member feeler =
        { Get = fun this -> match this.SubSubtype with Feeler feeler -> feeler | _ -> failwith "Gui is not a feeler."
          Set = fun feeler this -> { this with SubSubtype = Feeler feeler }}

    static member optFeeler =
        { Get = fun this -> match this.SubSubtype with Feeler feeler -> Some feeler | _ -> None
          Set = fun optButton this -> match optButton with None -> failwith "Cannot set subtype to None." | Some feeler -> { this with SubSubtype = Feeler feeler }}

type [<StructuralEquality; NoComparison; CLIMutable>] Block =
    { PhysicsId : Id
      Density : single
      BodyType : BodyType
      Sprite : Sprite }

type [<StructuralEquality; NoComparison; CLIMutable>] Avatar =
    { PhysicsId : Id
      Density : single
      Sprite : Sprite }
      
type [<StructuralEquality; NoComparison; CLIMutable>] TileMap =
    { PhysicsIds : Id list
      Density : single
      TileMapAsset : TileMapAsset
      TmxMap : TmxMap
      TileMapMetadata : Sprite list }

/// An algabraically-closed semantics for game actors.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] ActorSubtype =
    | Block of Block
    | Avatar of Avatar
    | TileMap of TileMap
 // | ...additional actors
 // | UserDefinedActor of IUserDefinedActor (* this would be one way to get open actor semantics, but perhaps at the cost of its value semantics... *)

/// A game actor.
/// A serializable value type.
type [<StructuralEquality; NoComparison; CLIMutable>] Actor =
    { Position : Vector2
      Depth : single
      Size : Vector2
      Rotation : single
      SubSubtype : ActorSubtype }

    static member block =
        { Get = fun this -> match this.SubSubtype with Block block -> block | _ -> failwith "Actor is not a block."
          Set = fun block this -> { this with SubSubtype = Block block }}

    static member optBlock =
        { Get = fun this -> match this.SubSubtype with Block block -> Some block | _ -> None
          Set = fun optBlock this -> match optBlock with None -> failwith "Cannot set subtype to None." | Some block -> { this with SubSubtype = Block block }}
          
    static member avatar =
        { Get = fun this -> match this.SubSubtype with Avatar avatar -> avatar | _ -> failwith "Actor is not a avatar."
          Set = fun avatar this -> { this with SubSubtype = Avatar avatar }}

    static member optAvatar =
        { Get = fun this -> match this.SubSubtype with Avatar avatar -> Some avatar | _ -> None
          Set = fun optAvatar this -> match optAvatar with None -> failwith "Cannot set subtype to None." | Some avatar -> { this with SubSubtype = Avatar avatar }}

    static member tileMap =
        { Get = fun this -> match this.SubSubtype with TileMap tileMap -> tileMap | _ -> failwith "Actor is not a tileMap."
          Set = fun tileMap this -> { this with SubSubtype = TileMap tileMap }}

    static member optTileMap =
        { Get = fun this -> match this.SubSubtype with TileMap tileMap -> Some tileMap | _ -> None
          Set = fun optTileMap this -> match optTileMap with None -> failwith "Cannot set subtype to None." | Some tileMap -> { this with SubSubtype = TileMap tileMap }}

/// An algabraically-closed semantics for game entities.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] EntitySubtype =
    | Gui of Gui
    | Actor of Actor
 // | Actor3d of Actor3d

/// A game entity.
/// A serializable value type.
type [<StructuralEquality; NoComparison; CLIMutable>] Entity =
    { Id : Id
      Enabled : bool
      Visible : bool
      Subtype : EntitySubtype }

    static member gui =
        { Get = fun this -> match this.Subtype with Gui gui -> gui | _ -> failwith "Entity is not a gui."
          Set = fun gui this -> { this with Subtype = Gui gui }}
    
    static member optGui =
        { Get = fun this -> match this.Subtype with Gui gui -> Some gui | _ -> None
          Set = fun optGui this -> match optGui with None -> failwith "Cannot set subtype to None." | Some gui -> set gui this Entity.gui }
    
    static member guiButton =
        { Get = fun this -> let gui = get this Entity.gui in (gui, get gui Gui.button)
          Set = fun (gui, button) this -> let newGui = set button Gui.button in set gui this Entity.gui }
    
    static member optGuiButton =
        { Get = fun this ->
            let optGui = get this Entity.optGui
            match optGui with
            | None -> None
            | Some gui ->
                let optButton = get gui Gui.optButton
                match optButton with
                | None -> None
                | Some button -> Some (gui, button)
          Set = fun optGuiButton this ->
            match optGuiButton with
            | None -> failwith "Cannot set subtype to None."
            | Some guiButton -> set guiButton this Entity.guiButton }
    
    static member guiLabel =
        { Get = fun this -> let gui = get this Entity.gui in (gui, get gui Gui.label)
          Set = fun (gui, label) this -> let newGui = set label Gui.label in set gui this Entity.gui }
    
    static member optGuiLabel =
        { Get = fun this ->
            let optGui = get this Entity.optGui
            match optGui with
            | None -> None
            | Some gui ->
                let optLabel = get gui Gui.optLabel
                match optLabel with
                | None -> None
                | Some label -> Some (gui, label)
          Set = fun optGuiLabel this ->
            match optGuiLabel with
            | None -> failwith "Cannot set subtype to None."
            | Some guiLabel -> set guiLabel this Entity.guiLabel }
    
    static member guiTextBox =
        { Get = fun this -> let gui = get this Entity.gui in (gui, get gui Gui.textBox)
          Set = fun (gui, textBox) this -> let newGui = set textBox Gui.textBox in set gui this Entity.gui }
    
    static member optGuiTextBox =
        { Get = fun this ->
            let optGui = get this Entity.optGui
            match optGui with
            | None -> None
            | Some gui ->
                let optTextBox = get gui Gui.optTextBox
                match optTextBox with
                | None -> None
                | Some textBox -> Some (gui, textBox)
          Set = fun optGuiTextBox this ->
            match optGuiTextBox with
            | None -> failwith "Cannot set subtype to None."
            | Some guiTextBox -> set guiTextBox this Entity.guiTextBox }
    
    static member guiToggle =
        { Get = fun this -> let gui = get this Entity.gui in (gui, get gui Gui.toggle)
          Set = fun (gui, toggle) this -> let newGui = set toggle Gui.toggle in set gui this Entity.gui }
    
    static member optGuiToggle =
        { Get = fun this ->
            let optGui = get this Entity.optGui
            match optGui with
            | None -> None
            | Some gui ->
                let optToggle = get gui Gui.optToggle
                match optToggle with
                | None -> None
                | Some toggle -> Some (gui, toggle)
          Set = fun optGuiToggle this ->
            match optGuiToggle with
            | None -> failwith "Cannot set subtype to None."
            | Some guiToggle -> set guiToggle this Entity.guiToggle }
    
    static member guiFeeler =
        { Get = fun this -> let gui = get this Entity.gui in (gui, get gui Gui.feeler)
          Set = fun (gui, feeler) this -> let newGui = set feeler Gui.feeler in set gui this Entity.gui }
    
    static member optGuiFeeler =
        { Get = fun this ->
            let optGui = get this Entity.optGui
            match optGui with
            | None -> None
            | Some gui ->
                let optFeeler = get gui Gui.optFeeler
                match optFeeler with
                | None -> None
                | Some feeler -> Some (gui, feeler)
          Set = fun optGuiFeeler this ->
            match optGuiFeeler with
            | None -> failwith "Cannot set subtype to None."
            | Some guiFeeler -> set guiFeeler this Entity.guiFeeler }

    static member actor =
        { Get = fun this -> match this.Subtype with Actor actor -> actor | _ -> failwith "Entity is not an actor."
          Set = fun actor this -> { this with Subtype = Actor actor }}
    
    static member optActor =
        { Get = fun this -> match this.Subtype with Actor actor -> Some actor | _ -> None
          Set = fun optActor this -> match optActor with None -> failwith "Cannot set subtype to None." | Some actor -> set actor this Entity.actor }
    
    static member actorBlock =
        { Get = fun this -> let actor = get this Entity.actor in (actor, get actor Actor.block)
          Set = fun (actor, block) this -> let newActor = set block Actor.block in set actor this Entity.actor }
    
    static member optActorBlock =
        { Get = fun this ->
            let optActor = get this Entity.optActor
            match optActor with
            | None -> None
            | Some actor ->
                let optBlock = get actor Actor.optBlock
                match optBlock with
                | None -> None
                | Some block -> Some (actor, block)
          Set = fun optActorBlock this ->
            match optActorBlock with
            | None -> failwith "Cannot set subtype to None."
            | Some actorBlock -> set actorBlock this Entity.actorBlock }
    
    static member actorAvatar =
        { Get = fun this -> let actor = get this Entity.actor in (actor, get actor Actor.avatar)
          Set = fun (actor, avatar) this -> let newActor = set avatar Actor.avatar in set actor this Entity.actor }
    
    static member optActorAvatar =
        { Get = fun this ->
            let optActor = get this Entity.optActor
            match optActor with
            | None -> None
            | Some actor ->
                let optAvatar = get actor Actor.optAvatar
                match optAvatar with
                | None -> None
                | Some avatar -> Some (actor, avatar)
          Set = fun optActorAvatar this ->
            match optActorAvatar with
            | None -> failwith "Cannot set subtype to None."
            | Some actorAvatar -> set actorAvatar this Entity.actorAvatar }
    
    static member actorTileMap =
        { Get = fun this -> let actor = get this Entity.actor in (actor, get actor Actor.tileMap)
          Set = fun (actor, tileMap) this -> let newActor = set tileMap Actor.tileMap in set actor this Entity.actor }
    
    static member optActorTileMap =
        { Get = fun this ->
            let optActor = get this Entity.optActor
            match optActor with
            | None -> None
            | Some actor ->
                let optTileMap = get actor Actor.optTileMap
                match optTileMap with
                | None -> None
                | Some tileMap -> Some (actor, tileMap)
          Set = fun optActorTileMap this ->
            match optActorTileMap with
            | None -> failwith "Cannot set subtype to None."
            | Some actorTileMap -> set actorTileMap this Entity.actorTileMap }

let writeEntityToXml (writer : XmlWriter) entity =
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
    reader.ReadEndElement ()