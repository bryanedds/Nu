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
        let entity = World.getEntity address world
        match property with
        | EntityXFieldDescriptor x ->
            let xFields = Map.add x.FieldName value entity.Xtension.XFields
            let entity = Entity.setXtension { entity.Xtension with XFields = xFields } entity
            World.setEntity address entity world
        | EntityPropertyInfo p ->
            let entity = Entity.setId entity.Id entity // NOTE: hacky copy
            p.SetValue (entity, value)
            World.setEntity address entity world

    let saveFile fileName world =
        let editorGroup = World.getGroup NuEditConstants.EditorGroupAddress world
        let editorEntities = World.getEntities NuEditConstants.EditorGroupAddress world
        World.saveGroupToFile editorGroup editorEntities fileName world

    let loadFile fileName world =
        let world = World.removeGroupImmediate NuEditConstants.EditorGroupAddress world
        let (group, entities) = World.loadGroupFromFile fileName world
        World.addGroup NuEditConstants.EditorGroupAddress group entities world