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
open Nu.Math
open Nu.Physics
open Nu.Audio
open Nu.Rendering
open Nu.AssetMetadata
open Nu.DomainModel
open Nu.Camera

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
        
let actorSep (actor : Actor) = actor.Entity
let actorCmb (actor : Actor, entity) = { actor with Entity = entity }
      
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

type [<StructuralEquality; NoComparison>] EntityModel =
    | Button of Button
    | Label of Label
    | TextBox of TextBox
    | Toggle of Toggle
    | Feeler of Feeler
    | Block of Block
    | Avatar of Avatar
    | TileMap of TileMap

let entityModelEntity =
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

let guiSep (gui : Gui) = (gui, gui.Entity)
let guiCmb (gui : Gui, entity) = { gui with Entity = entity }

let entityModelOptGui =
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

let entityModelGui =
    { Get = fun this -> Option.get (get this entityModelOptGui)
      Set = fun gui this -> set (Some gui) this entityModelOptGui }

let buttonSep (button : Button) = (button, button.Gui, button.Gui.Entity)
let buttonCmb (button : Button, gui, entity) = { button with Gui = { gui with Entity = entity }}

let entityModelOptButton =
    { Get = fun this -> match this with Button button -> Some button | _ -> None
      Set = fun optButton this -> Button <| Option.get optButton }

let entityModelButton =
    { Get = fun this -> Option.get (get this entityModelOptButton)
      Set = fun button this -> set (Some button) this entityModelOptButton }

let labelSep (label : Label) = (label, label.Gui, label.Gui.Entity)
let labelCmb (label : Label, gui, entity) = { label with Gui = { gui with Entity = entity }}

let entityModelOptLabel =
    { Get = fun this -> match this with Label label -> Some label | _ -> None
      Set = fun optLabel this -> Label <| Option.get optLabel }

let entityModelLabel =
    { Get = fun this -> Option.get (get this entityModelOptLabel)
      Set = fun label this -> set (Some label) this entityModelOptLabel }

let textBoxSep (textBox : TextBox) = (textBox, textBox.Gui, textBox.Gui.Entity)
let textBoxCmb (textBox : TextBox, gui, entity) = { textBox with Gui = { gui with Entity = entity }}

let entityModelOptTextBox =
    { Get = fun this -> match this with TextBox textBox -> Some textBox | _ -> None
      Set = fun optTextBox this -> TextBox <| Option.get optTextBox }

let entityModelTextBox =
    { Get = fun this -> Option.get (get this entityModelOptTextBox)
      Set = fun textBox this -> set (Some textBox) this entityModelOptTextBox }

let toggleSep (toggle : Toggle) = (toggle, toggle.Gui, toggle.Gui.Entity)
let toggleCmb (toggle : Toggle, gui, entity) = { toggle with Gui = { gui with Entity = entity }}

let entityModelOptToggle =
    { Get = fun this -> match this with Toggle toggle -> Some toggle | _ -> None
      Set = fun optToggle this -> Toggle <| Option.get optToggle }

let entityModelToggle =
    { Get = fun this -> Option.get (get this entityModelOptToggle)
      Set = fun toggle this -> set (Some toggle) this entityModelOptToggle }

let feelerSep (feeler : Feeler) = (feeler, feeler.Gui, feeler.Gui.Entity)
let feelerCmb (feeler : Feeler, gui, entity) = { feeler with Gui = { gui with Entity = entity }}

let entityModelOptFeeler =
    { Get = fun this -> match this with Feeler feeler -> Some feeler | _ -> None
      Set = fun optFeeler this -> Feeler <| Option.get optFeeler }

let entityModelFeeler =
    { Get = fun this -> Option.get (get this entityModelOptFeeler)
      Set = fun feeler this -> set (Some feeler) this entityModelOptFeeler }

let entityModelOptActor =
    { Get = fun this ->
        match this with
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> None
        | Block block -> Some block.Actor
        | Avatar avatar -> Some avatar.Actor
        | TileMap tileMap -> Some tileMap.Actor
      Set = fun optActor this ->
        let actor = Option.get optActor
        match this with
        | Button _
        | Label _
        | TextBox _
        | Toggle _
        | Feeler _ -> failwith "EntityModel is not an actor."
        | Block block -> Block { block with Actor = actor }
        | Avatar avatar -> Avatar { avatar with Actor = actor }
        | TileMap tileMap -> TileMap { tileMap with Actor = actor }}

let entityModelActor =
    { Get = fun this -> Option.get (get this entityModelOptActor)
      Set = fun actor this -> set (Some actor) this entityModelOptActor }

let blockSep (block : Block) = (block, block.Actor, block.Actor.Entity)
let blockCmb (block : Block, actor, entity) = { block with Actor = { actor with Entity = entity }}

let entityModelOptBlock =
    { Get = fun this -> match this with Block block -> Some block | _ -> None
      Set = fun optBlock this -> Block <| Option.get optBlock }

let entityModelBlock =
    { Get = fun this -> Option.get (get this entityModelOptBlock)
      Set = fun block this -> set (Some block) this entityModelOptBlock }

let avatarSep (avatar : Avatar) = (avatar, avatar.Actor, avatar.Actor.Entity)
let avatarCmb (avatar : Avatar, actor, entity) = { avatar with Actor = { actor with Entity = entity }}

let entityModelOptAvatar =
    { Get = fun this -> match this with Avatar avatar -> Some avatar | _ -> None
      Set = fun optAvatar this -> Avatar <| Option.get optAvatar }

let entityModelAvatar =
    { Get = fun this -> Option.get (get this entityModelOptAvatar)
      Set = fun avatar this -> set (Some avatar) this entityModelOptAvatar }

let tileMapSep (tileMap : TileMap) = (tileMap, tileMap.Actor, tileMap.Actor.Entity)
let tileMapCmb (tileMap : TileMap, actor, entity) = { tileMap with Actor = { actor with Entity = entity }}

let entityModelOptTileMap =
    { Get = fun this -> match this with TileMap tileMap -> Some tileMap | _ -> None
      Set = fun optTileMap this -> TileMap <| Option.get optTileMap }

let entityModelTileMap =
    { Get = fun this -> Option.get (get this entityModelOptTileMap)
      Set = fun tileMap this -> set (Some tileMap) this entityModelOptTileMap }

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
              PhysicsIds = []
              Density = NormalDensity
              TileMapAsset = { TileMapAssetName = Lun.make "TileMap"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }
              TmxMap = tmxMap
              TileMapMetadata = [{ SpriteAssetName = Lun.make "TileSet"; PackageName = Lun.make "Default"; PackageFileName = "AssetGraph.xml" }]}

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

let getPickingPriority entityModel =
    match entityModel with
    | Button button -> button.Gui.Depth
    | Label label -> label.Gui.Depth
    | TextBox textBox -> textBox.Gui.Depth
    | Toggle toggle -> toggle.Gui.Depth
    | Feeler feeler -> feeler.Gui.Depth
    | Block block -> block.Actor.Depth
    | Avatar avatar -> avatar.Actor.Depth
    | TileMap tileMap -> tileMap.Actor.Depth

let getEntityModelTransform relativeToView camera entityModel =
    let view = if relativeToView then inverseViewF camera else Vector2.Zero
    match entityModel with
    | Button button -> getGuiTransform button.Gui
    | Label label -> getGuiTransform label.Gui
    | TextBox textBox -> getGuiTransform textBox.Gui
    | Toggle toggle -> getGuiTransform toggle.Gui
    | Feeler feeler -> getGuiTransform feeler.Gui
    | Block block -> getActorTransformRelative view block.Actor
    | Avatar avatar -> getActorTransformRelative view avatar.Actor
    | TileMap tileMap -> getActorTransformRelative view tileMap.Actor

let setGuiTransform positionSnap rotationSnap (transform_ : Transform) entityModel lens =
    let transform_ = snapTransform positionSnap rotationSnap transform_
    let gui_ = get entityModel lens
    let gui_ = {{{ gui_ with Gui.Position = transform_.Position }
                        with Depth = transform_.Depth }
                        with Size = transform_.Size }
    set gui_ entityModel lens

let setActorTransform positionSnap rotationSnap (transform_ : Transform) entityModel lens =
    let transform_ = snapTransform positionSnap rotationSnap transform_
    let actor_ = get entityModel lens
    let actor_ = {{{{ actor_ with Actor.Position = transform_.Position }
                             with Depth = transform_.Depth }
                             with Size = transform_.Size }
                             with Rotation = transform_.Rotation }
    set actor_ entityModel lens

let setActorTransformRelative (view : Vector2) positionSnap rotationSnap (transform_ : Transform) entityModel lens =
    let transform_ = { transform_ with Position = transform_.Position + view }
    setActorTransform positionSnap rotationSnap transform_ entityModel lens

let setEntityModelTransform relativeToView camera positionSnap rotationSnap transform entityModel =
    let view = if relativeToView then inverseViewF camera else Vector2.Zero
    match entityModel with
    | Button _
    | Label _
    | TextBox _
    | Toggle _
    | Feeler _ -> setGuiTransform positionSnap rotationSnap transform entityModel entityModelGui
    | Block _
    | Avatar _
    | TileMap _ -> setActorTransformRelative view positionSnap rotationSnap transform entityModel entityModelActor

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