// NuEdit - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2014.

namespace NuEdit
open System
open System.Reflection
open Prime
open Nu
open NuEdit

[<AutoOpen>]
module NuEditReflectionModule =

    type EntityProperty =
        | EntityXFieldDescriptor of XFieldDescriptor
        | EntityPropertyInfo of PropertyInfo

module NuEditReflection =

    let containsProperty<'t> (property : PropertyInfo) =
        let properties = typeof<'t>.GetProperties (property.Name, BindingFlags.Instance ||| BindingFlags.Public)
        Seq.exists (fun item -> item = property) properties

    let getEntityPropertyValue property (entity : Entity) =
        match property with
        | EntityXFieldDescriptor x ->
            let xtension = entity.Xtension
            Map.find x.FieldName xtension.XFields
        | EntityPropertyInfo p ->
            if containsProperty<Entity> p then p.GetValue entity
            else p.GetValue entity

    let setEntityPropertyValue address property value world =
        let entity = get world <| World.worldEntity address
        match property with
        | EntityXFieldDescriptor x ->
            let xFields = Map.add x.FieldName value entity.Xtension.XFields
            let entity = { entity with Xtension = { entity.Xtension with XFields = xFields }}
            set entity world <| World.worldEntity address
        | EntityPropertyInfo p ->
            let entity = { entity with Id = entity.Id } // NOTE: hacky copy
            p.SetValue (entity, value)
            set entity world <| World.worldEntity address

    let saveFile fileName world =
        let editorGroup = get world <| World.worldGroup NuEditConstants.EditorGroupAddress
        let editorEntities = get world <| World.worldEntities NuEditConstants.EditorGroupAddress
        World.saveGroupToFile editorGroup editorEntities fileName world

    let loadFile fileName world =
        let world = World.removeGroup NuEditConstants.EditorGroupAddress world
        let (group, entities) = World.loadGroupFromFile fileName false world
        World.addGroup NuEditConstants.EditorGroupAddress group entities world