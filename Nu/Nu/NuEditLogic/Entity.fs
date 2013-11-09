module NuEditLogic.Entity
open System
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Serialization
open Nu.Core
open Nu.Entity
open Nu.Group
open Nu.Simulation

// TODO: rename this file

let getEntityModelTypes (entityModel : EntityModel) =
    match entityModel with
    | Button _ -> [typeof<Button>; typeof<Gui>; typeof<Entity>]
    | Label _ -> [typeof<Label>; typeof<Gui>; typeof<Entity>]
    | TextBox _ -> [typeof<TextBox>; typeof<Gui>; typeof<Entity>]
    | Toggle _ -> [typeof<Toggle>; typeof<Gui>; typeof<Entity>]
    | Feeler _ -> [typeof<Feeler>; typeof<Gui>; typeof<Entity>]
    | Block _ -> [typeof<Block>; typeof<Actor>; typeof<Entity>]
    | Avatar _ -> [typeof<Avatar>; typeof<Actor>; typeof<Entity>]
    | TileMap _ -> [typeof<TileMap>; typeof<Actor>; typeof<Entity>]

let containsProperty<'m> (property : PropertyInfo) =
    typeof<'m>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property

let getValue (property : PropertyInfo) (entityModel : EntityModel) (lens : FSharpx.Lens<EntityModel, 'm>) =
    if containsProperty<'m> property then property.GetValue ( get entityModel lens )
    elif containsProperty<Gui> property then property.GetValue (get entityModel guiLens)
    elif containsProperty<Actor> property then property.GetValue (get entityModel actorLens)
    else property.GetValue (get entityModel entityLens)

let getEntityModelPropertyValue (property : PropertyInfo) (entityModel : EntityModel) =
    match entityModel with
    | Button _ -> getValue property entityModel buttonLens
    | Label _ -> getValue property entityModel labelLens
    | TextBox _ -> getValue property entityModel textBoxLens
    | Toggle _ -> getValue property entityModel toggleLens
    | Feeler _ -> getValue property entityModel feelerLens
    | Block _ -> getValue property entityModel blockLens
    | Avatar _ -> getValue property entityModel avatarLens
    | TileMap _ -> getValue property entityModel tileMapLens

let setEntityModelPropertyValue address (property : PropertyInfo) value world =
    let entityModelLens = worldEntityModelLens address
    let entityModel_ = get world entityModelLens
    let entityModel_ =
        // TODO: so much code duplication, make me wanna slap your momma!
        match entityModel_ with
        | Button button_ ->
            let button_ = { button_ with Gui = button_.Gui } // NOTE: this is just a hacky way to copy a record in lieu of reflection
            if typeof<Button>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then let _ = property.SetValue (button_, value) in Button button_
            else
                let gui_ = { button_.Gui with Position = button_.Gui.Position } // NOTE: hacky copy
                if typeof<Gui>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (gui_, value) in Button { button_ with Gui = gui_ }
                else
                    let entity_ = { gui_.Entity with Id = gui_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Button { button_ with Gui = { gui_ with Entity = entity_ }}
        | Label label_ ->
            let label_ = { label_ with Gui = label_.Gui } // NOTE: hacky copy
            if typeof<Button>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then let _ = property.SetValue (label_, value) in Label label_
            else
                let gui_ = { label_.Gui with Position = label_.Gui.Position } // NOTE: hacky copy
                if typeof<Gui>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (gui_, value) in Label { label_ with Gui = gui_ }
                else
                    let entity_ = { gui_.Entity with Id = gui_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Label { label_ with Gui = { gui_ with Entity = entity_ }}
        | TextBox textBox_ ->
            let textBox_ = { textBox_ with Gui = textBox_.Gui } // NOTE: hacky copy
            if typeof<TextBox>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then let _ = property.SetValue (textBox_, value) in TextBox textBox_
            else
                let gui_ = { textBox_.Gui with Position = textBox_.Gui.Position } // NOTE: hacky copy
                if typeof<Gui>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (gui_, value) in TextBox { textBox_ with Gui = gui_ }
                else
                    let entity_ = { gui_.Entity with Id = gui_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    TextBox { textBox_ with Gui = { gui_ with Entity = entity_ }}
        | Toggle toggle_ ->
            let toggle_ = { toggle_ with Gui = toggle_.Gui } // NOTE: hacky copy
            if typeof<Toggle>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then let _ = property.SetValue (toggle_, value) in Toggle toggle_
            else
                let gui_ = { toggle_.Gui with Position = toggle_.Gui.Position } // NOTE: hacky copy
                if typeof<Gui>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (gui_, value) in Toggle { toggle_ with Gui = gui_ }
                else
                    let entity_ = { gui_.Entity with Id = gui_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Toggle { toggle_ with Gui = { gui_ with Entity = entity_ }}
        | Feeler feeler_ ->
            let feeler_ = { feeler_ with Gui = feeler_.Gui } // NOTE: hacky copy
            if typeof<Feeler>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then let _ = property.SetValue (feeler_, value) in Feeler feeler_
            else
                let gui_ = { feeler_.Gui with Position = feeler_.Gui.Position } // NOTE: hacky copy
                if typeof<Gui>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (gui_, value) in Feeler { feeler_ with Gui = gui_ }
                else
                    let entity_ = { gui_.Entity with Id = gui_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Feeler { feeler_ with Gui = { gui_ with Entity = entity_ }}
        | Block block_ ->
            let block_ = { block_ with Actor = block_.Actor } // NOTE: hacky copy
            if typeof<Block>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then let _ = property.SetValue (block_, value) in Block block_
            else
                let actor_ = { block_.Actor with Position = block_.Actor.Position } // NOTE: hacky copy
                if typeof<Actor>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (actor_, value) in Block { block_ with Actor = actor_ }
                else
                    let entity_ = { actor_.Entity with Id = actor_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Block { block_ with Actor = { actor_ with Entity = entity_ }}
        | Avatar avatar_ ->
            let avatar_ = { avatar_ with Actor = avatar_.Actor } // NOTE: hacky copy
            if typeof<Avatar>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then let _ = property.SetValue (avatar_, value) in Avatar avatar_
            else
                let actor_ = { avatar_.Actor with Position = avatar_.Actor.Position } // NOTE: hacky copy
                if typeof<Actor>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (actor_, value) in Avatar { avatar_ with Actor = actor_ }
                else
                    let entity_ = { actor_.Entity with Id = actor_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Avatar { avatar_ with Actor = { actor_ with Entity = entity_ }}
        | TileMap tileMap_ ->
            let tileMap_ = { tileMap_ with Actor = tileMap_.Actor } // NOTE: hacky copy
            if typeof<TileMap>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
            then let _ = property.SetValue (tileMap_, value) in TileMap tileMap_
            else
                let actor_ = { tileMap_.Actor with Position = tileMap_.Actor.Position } // NOTE: hacky copy
                if typeof<Actor>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (actor_, value) in TileMap { tileMap_ with Actor = actor_ }
                else
                    let entity_ = { actor_.Entity with Id = actor_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    TileMap { tileMap_ with Actor = { actor_ with Entity = entity_ }}
    set entityModel_ world entityModelLens

let writeFile fileName world =
    use file = File.Open (fileName, FileMode.Create)
    let writerSettings = XmlWriterSettings ()
    writerSettings.Indent <- true
    use writer = XmlWriter.Create (file, writerSettings)
    writer.WriteStartDocument ()
    writer.WriteStartElement "Root"
    let testGroupModel = get world <| worldGroupModelLens Test.GroupModelAddress
    writeGroupModelToXml writer testGroupModel
    writer.WriteEndElement ()
    writer.WriteEndDocument ()

let readFile (fileName : string) world =
    let document = XmlDocument ()
    document.Load fileName
    let rootNode = document.Item "Root"
    let testGroupModelNode = rootNode.FirstChild
    let (testGroupModel, testEntityModels) = loadGroupModelFromXml testGroupModelNode
    let world_ = removeGroupModel Test.GroupModelAddress world
    let world_ = addGroupModel Test.GroupModelAddress testGroupModel world_
    addEntityModels testEntityModels Test.GroupModelAddress world_