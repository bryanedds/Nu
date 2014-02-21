namespace NuEdit
open System
open System.Reflection
open Nu
open Nu.Core
open Nu.Entities
open Nu.GroupModule
open Nu.WorldModule
open NuEdit.Constants

type EntityPropertyInfo =
    | XFieldDescriptor of XFieldDescriptor
    | PropertyInfo of PropertyInfo

module Reflection =

    let getEntityTypes entity =
        match entity with
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

    let getValue property entity (lens : FSharpx.Lens<_, 'm>) =
        match property with
        | XFieldDescriptor x ->
            let entity = get entity entityLens
            let xtension = entity.Xtension
            Map.find x.FieldName xtension.XFields
        | PropertyInfo p ->
            if containsProperty<'m> p then p.GetValue (get entity lens)
            else p.GetValue (get entity entityLens)

    let getEntityPropertyValue property entity =
        match entity with
        | Button _ -> getValue property entity buttonLens
        | Label _ -> getValue property entity labelLens
        | TextBox _ -> getValue property entity textBoxLens
        | Toggle _ -> getValue property entity toggleLens
        | Feeler _ -> getValue property entity feelerLens
        | Block _ -> getValue property entity blockLens
        | Avatar _ -> getValue property entity avatarLens
        | TileMap _ -> getValue property entity tileMapLens

    let setEntityPropertyValue address property value world =
        let entityLens = worldEntityLens address
        let entity_ = get world entityLens
        match property with
        | XFieldDescriptor x ->
            let entity = get entity_ entityLens
            let xFields = Map.add x.FieldName value entity.Xtension.XFields
            let entity' = { entity with Xtension = { entity.Xtension with XFields = xFields }}
            set entity' world <| worldEntityLens address
        | PropertyInfo p ->
            let entity_ =
                // TODO: so much code duplication, make me wanna slap your momma!
                match entity_ with
                | Button button_ ->
                    let button_ = { button_ with Entity = button_.Entity } // NOTE: hacky copy
                    if typeof<Button>.GetProperty (p.Name, BindingFlags.Instance ||| BindingFlags.Public) = p
                    then let _ = p.SetValue (button_, value) in Button button_
                    else
                        let entity_ = { button_.Entity with Id = button_.Entity.Id } // NOTE: hacky copy
                        p.SetValue (entity_, value)
                        Button { button_ with Entity = entity_ }
                | Label label_ ->
                    let label_ = { label_ with Entity = label_.Entity } // NOTE: hacky copy
                    if typeof<Label>.GetProperty (p.Name, BindingFlags.Instance ||| BindingFlags.Public) = p
                    then let _ = p.SetValue (label_, value) in Label label_
                    else
                        let entity_ = { label_.Entity with Id = label_.Entity.Id } // NOTE: hacky copy
                        p.SetValue (entity_, value)
                        Label { label_ with Entity = entity_ }
                | TextBox textBox_ ->
                    let textBox_ = { textBox_ with Entity = textBox_.Entity } // NOTE: hacky copy
                    if typeof<TextBox>.GetProperty (p.Name, BindingFlags.Instance ||| BindingFlags.Public) = p
                    then let _ = p.SetValue (textBox_, value) in TextBox textBox_
                    else
                        let entity_ = { textBox_.Entity with Id = textBox_.Entity.Id } // NOTE: hacky copy
                        p.SetValue (entity_, value)
                        TextBox { textBox_ with Entity = entity_ }
                | Toggle toggle_ ->
                    let toggle_ = { toggle_ with Entity = toggle_.Entity } // NOTE: hacky copy
                    if typeof<Toggle>.GetProperty (p.Name, BindingFlags.Instance ||| BindingFlags.Public) = p
                    then let _ = p.SetValue (toggle_, value) in Toggle toggle_
                    else
                        let entity_ = { toggle_.Entity with Id = toggle_.Entity.Id } // NOTE: hacky copy
                        p.SetValue (entity_, value)
                        Toggle { toggle_ with Entity = entity_ }
                | Feeler feeler_ ->
                    let feeler_ = { feeler_ with Entity = feeler_.Entity } // NOTE: hacky copy
                    if typeof<Feeler>.GetProperty (p.Name, BindingFlags.Instance ||| BindingFlags.Public) = p
                    then let _ = p.SetValue (feeler_, value) in Feeler feeler_
                    else
                        let entity_ = { feeler_.Entity with Id = feeler_.Entity.Id } // NOTE: hacky copy
                        p.SetValue (entity_, value)
                        Feeler { feeler_ with Entity = entity_ }
                | Block block_ ->
                    let block_ = { block_ with Entity = block_.Entity } // NOTE: hacky copy
                    if typeof<Block>.GetProperty (p.Name, BindingFlags.Instance ||| BindingFlags.Public) = p
                    then let _ = p.SetValue (block_, value) in Block block_
                    else
                        let entity_ = { block_.Entity with Id = block_.Entity.Id } // NOTE: hacky copy
                        p.SetValue (entity_, value)
                        Block { block_ with Entity = entity_ }
                | Avatar avatar_ ->
                    let avatar_ = { avatar_ with Entity = avatar_.Entity } // NOTE: hacky copy
                    if typeof<Avatar>.GetProperty (p.Name, BindingFlags.Instance ||| BindingFlags.Public) = p
                    then let _ = p.SetValue (avatar_, value) in Avatar avatar_
                    else
                        let entity_ = { avatar_.Entity with Id = avatar_.Entity.Id } // NOTE: hacky copy
                        p.SetValue (entity_, value)
                        Avatar { avatar_ with Entity = entity_ }
                | TileMap tileMap_ ->
                    let tileMap_ = { tileMap_ with Entity = tileMap_.Entity } // NOTE: hacky copy
                    if typeof<TileMap>.GetProperty (p.Name, BindingFlags.Instance ||| BindingFlags.Public) = p
                    then let _ = p.SetValue (tileMap_, value) in TileMap tileMap_
                    else
                        let entity_ = { tileMap_.Entity with Id = tileMap_.Entity.Id } // NOTE: hacky copy
                        p.SetValue (entity_, value)
                        TileMap { tileMap_ with Entity = entity_ }
            set entity_ world entityLens

    let writeFile fileName world =
        let editorGroup = get world <| worldGroupLens EditorGroupAddress
        let editorEntitys = get world <| worldEntitysLens EditorGroupAddress
        writeGroupFile editorGroup editorEntitys fileName world

    let loadFile fileName world =
        let world' = removeGroup EditorGroupAddress world
        let testGroupDescriptor = loadGroupFile fileName world'
        addGroup EditorGroupAddress testGroupDescriptor world'