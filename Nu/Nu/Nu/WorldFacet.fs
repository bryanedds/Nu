// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Reflection
open System.Xml
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldFacetModule =

    type World with

        static member private tryGetFacet facetName world =
            let facets = World.getFacets world
            match Map.tryFind facetName facets with
            | Some facet -> Right facet
            | None -> Left ^ "Invalid facet name '" + facetName + "'."

        static member private isFacetCompatibleWithEntity entityDispatcherMap facet (entityState : EntityState) =
            // Note a facet is incompatible with any other facet if it contains any fields that has
            // the same name but a different type.
            let facetType = facet.GetType ()
            let facetFieldDefinitions = Reflection.getFieldDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entityState then
                List.notExists
                    (fun definition ->
                        match Xtension.tryGetField definition.FieldName entityState.Xtension with
                        | Some field -> field.GetType () <> definition.FieldType
                        | None -> false)
                    facetFieldDefinitions
            else false

        static member private getFacetNamesToAdd oldFacetNames newFacetNames =
            Set.difference newFacetNames oldFacetNames

        static member private getFacetNamesToRemove oldFacetNames newFacetNames =
            Set.difference oldFacetNames newFacetNames

        static member private getEntityFieldDefinitionNamesToDetach entityState facetToRemove =

            // get the field definition name counts of the current, complete entity
            let fieldDefinitions = Reflection.getReflectiveFieldDefinitionMap entityState
            let fieldDefinitionNameCounts = Reflection.getFieldDefinitionNameCounts fieldDefinitions

            // get the field definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetFieldDefinitions = Map.singleton facetType.Name ^ Reflection.getFieldDefinitions facetType
            let facetFieldDefinitionNameCounts = Reflection.getFieldDefinitionNameCounts facetFieldDefinitions

            // compute the difference of the counts
            let finalFieldDefinitionNameCounts =
                Map.map
                    (fun fieldName fieldCount ->
                        match Map.tryFind fieldName facetFieldDefinitionNameCounts with
                        | Some facetFieldCount -> fieldCount - facetFieldCount
                        | None -> fieldCount)
                    fieldDefinitionNameCounts

            // build a set of all field names where the final counts are negative
            Map.fold
                (fun fieldNamesToDetach fieldName fieldCount ->
                    if fieldCount = 0
                    then Set.add fieldName fieldNamesToDetach
                    else fieldNamesToDetach)
                Set.empty
                finalFieldDefinitionNameCounts

        static member private tryRemoveFacet facetName entityState optEntity world =
            match List.tryFind (fun facet -> getTypeName facet = facetName) entityState.FacetsNp with
            | Some facet ->
                let (entityState, world) =
                    match optEntity with
                    | Some entity ->
                        let world = facet.Unregister (entity, world)
                        let entityState = World.getEntityState entity world
                        (entityState, world)
                    | None -> (entityState, world)
                let entityState = { entityState with FacetNames = Set.remove facetName entityState.FacetNames }
                let entityState = { entityState with FacetsNp = List.remove ((=) facet) entityState.FacetsNp }
                let fieldNames = World.getEntityFieldDefinitionNamesToDetach entityState facet
                Reflection.detachFieldsViaNames fieldNames entityState // hacky copy elided
                match optEntity with
                | Some entity ->
                    let oldWorld = world
                    let world = World.setEntityStateWithoutEvent entityState entity world
                    let world = World.updateEntityInEntityTree entity oldWorld world
                    let world = World.publishEntityChange entityState entity oldWorld world
                    Right (World.getEntityState entity world, world)
                | None -> Right (entityState, world)
            | None -> Left ^ "Failure to remove facet '" + facetName + "' from entity."

        static member private tryAddFacet facetName (entityState : EntityState) optEntity world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let entityState = { entityState with FacetNames = Set.add facetName entityState.FacetNames }
                    let entityState = { entityState with FacetsNp = facet :: entityState.FacetsNp }
                    Reflection.attachFields facet entityState // hacky copy elided
                    match optEntity with
                    | Some entity ->
                        let oldWorld = world
                        let world = World.setEntityStateWithoutEvent entityState entity world
                        let world = World.updateEntityInEntityTree entity oldWorld world
                        let world = World.publishEntityChange entityState entity oldWorld world
                        let world = facet.Register (entity, world)
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else Left ^ "Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Name + "'."
            | Left error -> Left error

        static member private tryRemoveFacets facetNamesToRemove entityState optEntity world =
            Set.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entityState, world) -> World.tryRemoveFacet facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToRemove

        static member private tryAddFacets facetNamesToAdd entityState optEntity world =
            Set.fold
                (fun eitherEntityStateWorld facetName ->
                    match eitherEntityStateWorld with
                    | Right (entityState, world) -> World.tryAddFacet facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToAdd

        static member internal trySetFacetNames facetNames entityState optEntity world =
            let facetNamesToRemove = World.getFacetNamesToRemove entityState.FacetNames facetNames
            let facetNamesToAdd = World.getFacetNamesToAdd entityState.FacetNames facetNames
            match World.tryRemoveFacets facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal trySynchronizeFacetsToNames oldFacetNames entityState optEntity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames entityState.FacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames entityState.FacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal attachIntrinsicFacetsViaNames (entityState : EntityState) world =
            let dispatchers = world.Dispatchers
            let entityState = { entityState with Id = entityState.Id } // hacky copy
            Reflection.attachIntrinsicFacets dispatchers.EntityDispatchers dispatchers.Facets entityState.DispatcherNp entityState
            entityState