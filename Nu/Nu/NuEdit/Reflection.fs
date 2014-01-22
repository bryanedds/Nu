namespace NuEdit
open System
open System.Reflection
open Nu
open Nu.Core
open Nu.Entities
open Nu.GroupModule
open Nu.WorldModule
open NuEdit.Constants
module Reflection =

    let getEntityModelTypes (entityModel : EntityModel) =
        match entityModel with
        | CustomEntity _ -> [typeof<Entity>]
        | Button _ -> [typeof<Button>; typeof<Entity>]
        | Label _ -> [typeof<Label>; typeof<Entity>]
        | TextBox _ -> [typeof<TextBox>; typeof<Entity>]
        | Toggle _ -> [typeof<Toggle>; typeof<Entity>]
        | Feeler _ -> [typeof<Feeler>; typeof<Entity>]
        | Block _ -> [typeof<Block>; typeof<Entity>]
        | Avatar _ -> [typeof<Avatar>; typeof<Entity>]
        | TileMap _ -> [typeof<TileMap>; typeof<Entity>]

    let containsProperty<'m> (property : PropertyInfo) =
        typeof<'m>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property

    let getValue (property : PropertyInfo) (entityModel : EntityModel) (lens : FSharpx.Lens<EntityModel, 'm>) =
        if containsProperty<'m> property then property.GetValue ( get entityModel lens )
        else property.GetValue (get entityModel entityLens)

    let getEntityModelPropertyValue (property : PropertyInfo) (entityModel : EntityModel) =
        match entityModel with
        | CustomEntity _ -> getValue property entityModel customEntityLens
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
            | CustomEntity customEntity_ ->
                let customEntity_ = { customEntity_ with Id = customEntity_.Id } // NOTE: hacky copy
                property.SetValue (customEntity_, value)
                CustomEntity customEntity_
            | Button button_ ->
                let button_ = { button_ with Entity = button_.Entity } // NOTE: hacky copy
                if typeof<Button>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (button_, value) in Button button_
                else
                    let entity_ = { button_.Entity with Id = button_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Button { button_ with Entity = entity_ }
            | Label label_ ->
                let label_ = { label_ with Entity = label_.Entity } // NOTE: hacky copy
                if typeof<Label>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (label_, value) in Label label_
                else
                    let entity_ = { label_.Entity with Id = label_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Label { label_ with Entity = entity_ }
            | TextBox textBox_ ->
                let textBox_ = { textBox_ with Entity = textBox_.Entity } // NOTE: hacky copy
                if typeof<TextBox>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (textBox_, value) in TextBox textBox_
                else
                    let entity_ = { textBox_.Entity with Id = textBox_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    TextBox { textBox_ with Entity = entity_ }
            | Toggle toggle_ ->
                let toggle_ = { toggle_ with Entity = toggle_.Entity } // NOTE: hacky copy
                if typeof<Toggle>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (toggle_, value) in Toggle toggle_
                else
                    let entity_ = { toggle_.Entity with Id = toggle_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Toggle { toggle_ with Entity = entity_ }
            | Feeler feeler_ ->
                let feeler_ = { feeler_ with Entity = feeler_.Entity } // NOTE: hacky copy
                if typeof<Feeler>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (feeler_, value) in Feeler feeler_
                else
                    let entity_ = { feeler_.Entity with Id = feeler_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Feeler { feeler_ with Entity = entity_ }
            | Block block_ ->
                let block_ = { block_ with Entity = block_.Entity } // NOTE: hacky copy
                if typeof<Block>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (block_, value) in Block block_
                else
                    let entity_ = { block_.Entity with Id = block_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Block { block_ with Entity = entity_ }
            | Avatar avatar_ ->
                let avatar_ = { avatar_ with Entity = avatar_.Entity } // NOTE: hacky copy
                if typeof<Avatar>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (avatar_, value) in Avatar avatar_
                else
                    let entity_ = { avatar_.Entity with Id = avatar_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    Avatar { avatar_ with Entity = entity_ }
            | TileMap tileMap_ ->
                let tileMap_ = { tileMap_ with Entity = tileMap_.Entity } // NOTE: hacky copy
                if typeof<TileMap>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property
                then let _ = property.SetValue (tileMap_, value) in TileMap tileMap_
                else
                    let entity_ = { tileMap_.Entity with Id = tileMap_.Entity.Id } // NOTE: hacky copy
                    property.SetValue (entity_, value)
                    TileMap { tileMap_ with Entity = entity_ }
        set entityModel_ world entityModelLens

    let writeFile fileName world =
        let editorGroup = get world <| worldGroupLens EditorGroupAddress
        let editorEntityModels = get world <| worldEntityModelsLens EditorGroupAddress
        writeGroupFile editorGroup editorEntityModels fileName world

    let loadFile (fileName : string) world =
        let world' = removeGroup EditorGroupAddress world
        let testGroupDescriptor = loadGroupFile fileName world'
        addGroup EditorGroupAddress testGroupDescriptor world'