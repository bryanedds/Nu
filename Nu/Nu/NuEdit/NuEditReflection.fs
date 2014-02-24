namespace NuEdit
open System
open System.Reflection
open Nu
open Nu.NuCore
open Nu.Entity
open Nu.Group
open Nu.World
open NuEdit.NuEditConstants

[<AutoOpen>]
module NuEditReflectionModule =

    type EntityProperty =
        | EntityXFieldDescriptor of XFieldDescriptor
        | EntityPropertyInfo of PropertyInfo

module NuEditReflection =

    let containsProperty<'t> (property : PropertyInfo) =
        typeof<'t>.GetProperty (property.Name, BindingFlags.Instance ||| BindingFlags.Public) = property

    let getEntityPropertyValue property (entity : Entity) =
        match property with
        | EntityXFieldDescriptor x ->
            let xtension = entity.Xtension
            Map.find x.FieldName xtension.XFields
        | EntityPropertyInfo p ->
            if containsProperty<Entity> p then p.GetValue entity
            else p.GetValue entity

    let setEntityPropertyValue address property value world =
        let entity = get world <| worldEntityLens address
        match property with
        | EntityXFieldDescriptor x ->
            let xFields = Map.add x.FieldName value entity.Xtension.XFields
            let entity' = { entity with Xtension = { entity.Xtension with XFields = xFields }}
            set entity' world <| worldEntityLens address
        | EntityPropertyInfo p ->
            let entity' = { entity with Id = entity.Id } // NOTE: hacky copy
            p.SetValue (entity', value)
            set entity' world <| worldEntityLens address

    let writeFile fileName world =
        let editorGroup = get world <| worldGroupLens EditorGroupAddress
        let editorEntities = get world <| worldEntitiesLens EditorGroupAddress
        writeGroupFile editorGroup editorEntities fileName world

    let loadFile fileName world =
        let world' = removeGroup EditorGroupAddress world
        let (group, entities, world'') = loadGroupFile fileName world' true
        addGroup EditorGroupAddress group entities world''