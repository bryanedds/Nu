// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Reflection
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
            // Note a facet is incompatible with any other facet if it contains any properties that has
            // the same name but a different type.
            let facetType = facet.GetType ()
            let facetPropertyDefinitions = Reflection.getPropertyDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entityState then
                List.notExists
                    (fun definition ->
                        match Xtension.tryGetProperty definition.PropertyName entityState.Xtension with
                        | Some property -> property.GetType () <> definition.PropertyType
                        | None -> false)
                    facetPropertyDefinitions
            else false

        static member private getFacetNamesToAdd oldFacetNames newFacetNames =
            Set.difference newFacetNames oldFacetNames

        static member private getFacetNamesToRemove oldFacetNames newFacetNames =
            Set.difference oldFacetNames newFacetNames

        static member private getEntityPropertyDefinitionNamesToDetach entityState facetToRemove =

            // get the property definition name counts of the current, complete entity
            let PropertyDefinitions = Reflection.getReflectivePropertyDefinitionMap entityState
            let propertyDefinitionNameCounts = Reflection.getPropertyDefinitionNameCounts PropertyDefinitions

            // get the property definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetPropertyDefinitions = Map.singleton facetType.Name ^ Reflection.getPropertyDefinitions facetType
            let facetPropertyDefinitionNameCounts = Reflection.getPropertyDefinitionNameCounts facetPropertyDefinitions

            // compute the difference of the counts
            let finalPropertyDefinitionNameCounts =
                Map.map
                    (fun propertyName propertyCount ->
                        match Map.tryFind propertyName facetPropertyDefinitionNameCounts with
                        | Some facetPropertyCount -> propertyCount - facetPropertyCount
                        | None -> propertyCount)
                    propertyDefinitionNameCounts

            // build a set of all property names where the final counts are negative
            Map.fold
                (fun propertyNamesToDetach propertyName propertyCount ->
                    if propertyCount = 0
                    then Set.add propertyName propertyNamesToDetach
                    else propertyNamesToDetach)
                Set.empty
                finalPropertyDefinitionNameCounts

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
                let facetNames = Set.remove facetName entityState.FacetNames
                let facetsNp = List.remove ((=) facet) entityState.FacetsNp
                let entityState = { entityState with FacetNames = facetNames }
                let entityState = { entityState with FacetsNp = facetsNp }
                let propertyNames = World.getEntityPropertyDefinitionNamesToDetach entityState facet
                let entityState = Reflection.detachPropertiesViaNames EntityState.copy propertyNames entityState
                match optEntity with
                | Some entity ->
                    let oldWorld = world
                    let world = World.setEntityStateWithoutEvent entityState entity world
                    let world = World.updateEntityInEntityTree entity oldWorld world
                    let world = World.publishEntityChange entityState entity oldWorld world
                    Right (World.getEntityState entity world, world)
                | None -> Right (entityState, world)
            | None -> let _ = World.choose world in Left ^ "Failure to remove facet '" + facetName + "' from entity."

        static member private tryAddFacet facetName (entityState : EntityState) optEntity world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let facetNames = Set.add facetName entityState.FacetNames
                    let facetsNp = facet :: entityState.FacetsNp
                    let entityState = { entityState with FacetNames = facetNames }
                    let entityState = { entityState with FacetsNp = facetsNp }
                    let entityState = Reflection.attachProperties EntityState.copy facet entityState
                    match optEntity with
                    | Some entity ->
                        let oldWorld = world
                        let world = World.setEntityStateWithoutEvent entityState entity world
                        let world = World.updateEntityInEntityTree entity oldWorld world
                        let world = World.publishEntityChange entityState entity oldWorld world
                        let world = facet.Register (entity, world)
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else let _ = World.choose world in Left ^ "Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Name + "'."
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

        static member internal attachIntrinsicFacetsViaNames entityState world =
            let entityDispatchers = World.getEntityDispatchers world
            let facets = World.getFacets world
            Reflection.attachIntrinsicFacets EntityState.copy entityDispatchers facets entityState.DispatcherNp entityState