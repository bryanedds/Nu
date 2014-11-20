namespace Nu
open System
open System.ComponentModel
open System.Collections.Generic
open System.Reflection
open System.Xml
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module ReflectionModule =

    /// An evaluatable expression for defining a field.
    type [<NoEquality; NoComparison>] FieldExpr =
        | Constant of obj
        | Variable of (unit -> obj)
        static member eval expr =
            match expr with
            | Constant value -> value
            | Variable fn -> fn ()

    /// The definition of a data-driven field.
    type [<NoEquality; NoComparison>] FieldDefinition =
        { FieldName : string
          FieldType : Type
          FieldExpr : FieldExpr }

        static member private validate fieldName (fieldType : Type) (_ : FieldExpr) =
            if fieldName = "FacetNames" then failwith "FacetNames cannot be an intrinsic field."
            if fieldName = "OptOverlayName" then failwith "OptOverlayName cannot be an intrinsic field."
            if Array.exists (fun gta -> gta = typeof<obj>) fieldType.GenericTypeArguments then
                failwith <|
                    "Generic field definition lacking type information for field '" + fieldName + "'. " +
                    "Use explicit typing on all values that carry incomplete type information such as empty lists, empty sets, and none options,."

        static member make fieldName fieldType fieldExpr =
            FieldDefinition.validate fieldName fieldType fieldExpr
            { FieldName = fieldName; FieldType = fieldType; FieldExpr = fieldExpr }

    /// In tandem with the define literal, grants a nice syntax to define constant fields.
    type DefineConstant =
        { DefineConstant : unit }
        static member (?) (_, fieldName) =
            fun (constant : 'c) ->
                FieldDefinition.make fieldName typeof<'c> (Constant constant)

    /// In tandem with the variable literal, grants a nice syntax to define variable fields.
    type DefineVariable =
        { DefineVariable : unit }
        static member (?) (_, fieldName) =
            fun (variable : unit -> 'v) ->
                FieldDefinition.make fieldName typeof<'v> (Variable (fun () -> variable () :> obj))

    /// In tandem with the DefineConstant type, grants a nice syntax to define constant fields.
    let define = { DefineConstant = () }

    /// In tandem with the DefineVariable type, grants a nice syntax to define variable fields.
    let variable = { DefineVariable = () }

[<RequireQualifiedAccess>]
module Reflection =

    let private fieldDefinitionsCache =
        Dictionary<Type, FieldDefinition list> ()

    /// Get the type name of a target.
    let getTypeName target =
        let targetType = target.GetType ()
        targetType.Name

    /// Get the concrete base types of a type excepting the object type.
    let rec getBaseTypesExceptObject (aType : Type) =
        match aType.BaseType with
        | null -> []
        | baseType ->
            if baseType <> typeof<obj>
            then baseType :: getBaseTypesExceptObject baseType
            else []

    /// Queries that the target type offers dispatcher behavior congruent to the given dispatcher.
    let dispatchesAs (dispatcherTargetType : Type) dispatcher =
        let dispatcherType = dispatcher.GetType ()
        let result =
            dispatcherTargetType = dispatcherType ||
            dispatcherType.IsSubclassOf dispatcherTargetType
        result

    /// Attach fields from the given definitions to a target.
    let attachFieldsViaDefinitions fieldDefinitions target =
        let targetType = target.GetType ()
        for fieldDefinition in fieldDefinitions do
            let fieldValue = FieldExpr.eval fieldDefinition.FieldExpr
            match targetType.GetPropertyWritable fieldDefinition.FieldName with
            | null ->
                match targetType.GetPropertyWritable "Xtension" with
                | null -> failwith <| "Invalid field '" + fieldDefinition.FieldName + "' for target type '" + targetType.Name + "'."
                | xtensionProperty ->
                    match xtensionProperty.GetValue target with
                    | :? Xtension as xtension ->
                        let xField = { FieldValue = fieldValue; FieldType = fieldDefinition.FieldType }
                        let xFields = Map.add fieldDefinition.FieldName xField xtension.XFields
                        let xtension = { xtension with XFields = xFields }
                        xtensionProperty.SetValue (target, xtension)
                    | _ -> failwith <| "Invalid field '" + fieldDefinition.FieldName + "' for target type '" + targetType.Name + "'."
            | property -> property.SetValue (target, fieldValue)

    /// Detach fields from a target.
    let detachFieldsViaNames fieldNames target =
        let targetType = target.GetType ()
        for fieldName in fieldNames do
            match targetType.GetPropertyWritable fieldName with
            | null ->
                match targetType.GetPropertyWritable "Xtension" with
                | null -> failwith <| "Invalid field '" + fieldName + "' for target type '" + targetType.Name + "'."
                | xtensionProperty ->
                    match xtensionProperty.GetValue target with
                    | :? Xtension as xtension ->
                        let xfields = Map.remove fieldName xtension.XFields
                        let xtension = { xtension with XFields = xfields }
                        xtensionProperty.SetValue (target, xtension)
                    | _ -> failwith <| "Invalid field '" + fieldName + "' for target type '" + targetType.Name + "'."
            | _ -> ()

    /// Get the field definitions of a target type not considering inheritance.
    /// OPTIMIZATION: Memoized for efficiency since FieldDefinitions properties will likely return
    /// a newly constructed list.
    let getFieldDefinitionsNoInherit (targetType : Type) =
        let refFieldDefinitions = ref []
        if fieldDefinitionsCache.TryGetValue (targetType, refFieldDefinitions) then !refFieldDefinitions
        else
            let fieldDefinitions =
                match targetType.GetProperty ("FieldDefinitions", BindingFlags.Static ||| BindingFlags.Public) with
                | null -> []
                | fieldDefinitionsProperty ->
                    match fieldDefinitionsProperty.GetValue null with
                    | :? (obj list) as definitions when List.isEmpty definitions -> []
                    | :? (FieldDefinition list) as definitions -> definitions
                    | _ -> failwith <| "FieldDefinitions property for type '" + targetType.Name + "' must be of type FieldDefinition list."
            fieldDefinitionsCache.Add (targetType, fieldDefinitions)
            fieldDefinitions

    /// Get the intrinsic facet names of a target type not considering inheritance.
    let getIntrinsicFacetNamesNoInherit (targetType : Type) =
        match targetType.GetProperty ("IntrinsicFacetNames", BindingFlags.Static ||| BindingFlags.Public) with
        | null -> []
        | intrinsicFacetNamesProperty ->
            let intrinsicFacetNames = intrinsicFacetNamesProperty.GetValue null
            match intrinsicFacetNames with
            | :? (obj list) as intrinsicFacetNames when List.isEmpty intrinsicFacetNames -> []
            | :? (string list) as intrinsicFacetNames -> intrinsicFacetNames
            | _ -> failwith <| "IntrinsicFacetNames property for type '" + targetType.Name + "' must be of type string list."

    /// Get the field definitions of a target type.
    let getFieldDefinitions (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let fieldDefinitionLists = List.map (fun aType -> getFieldDefinitionsNoInherit aType) targetTypes
        let fieldDefinitionLists = List.rev fieldDefinitionLists
        List.concat fieldDefinitionLists

    /// Get the intrinsic facet names of a target type.
    let getIntrinsicFacetNames (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let intrinsicFacetNamesLists = List.map (fun aType -> getIntrinsicFacetNamesNoInherit aType) targetTypes
        let intrinsicFacetNamesLists = List.rev intrinsicFacetNamesLists
        List.concat intrinsicFacetNamesLists

    /// Get the names of the field definitions of a target type.
    let getFieldDefinitionNames targetType =
        let fieldDefinitions = getFieldDefinitions targetType
        List.map (fun fieldDefinition -> fieldDefinition.FieldName) fieldDefinitions

    /// Get a map of the counts of the field definitions names.
    let getFieldDefinitionNameCounts fieldDefinitions =
        Map.fold
            (fun map _ definitions ->
                List.fold
                    (fun map definition ->
                        let definitionName = definition.FieldName
                        match Map.tryFind definitionName map with
                        | Some count -> Map.add definitionName (count + 1) map
                        | None -> Map.add definitionName 1 map)
                    map
                    definitions)
            Map.empty
            fieldDefinitions

    /// Attach source's fields to a target.
    let attachFields source target =
        let sourceType = source.GetType ()
        let fieldDefinitions = getFieldDefinitions sourceType
        attachFieldsViaDefinitions fieldDefinitions target

    /// Detach source's fields to a target.
    let detachFields source target =
        let sourceType = source.GetType ()
        let fieldNames = getFieldDefinitionNames sourceType
        detachFieldsViaNames fieldNames target

    /// Check for facet compatibility with the target's dispatcher.
    let isFacetTypeCompatibleWithDispatcher dispatcherMap (facetType : Type) (target : obj) =
        let targetType = target.GetType ()
        match facetType.GetProperty ("RequiredDispatcherName", BindingFlags.Static ||| BindingFlags.Public) with
        | null -> true
        | reqdDispatcherNameProperty ->
            match reqdDispatcherNameProperty.GetValue null with
            | :? string as reqdDispatcherName ->
                match Map.tryFind reqdDispatcherName dispatcherMap with
                | Some reqdDispatcher ->
                    let reqdDispatcherType = reqdDispatcher.GetType ()
                    match targetType.GetProperty "DispatcherNp" with
                    | null -> false
                    | dispatcherNpProperty ->
                        let dispatcher = dispatcherNpProperty.GetValue target
                        dispatchesAs reqdDispatcherType dispatcher
                | None -> failwith <| "Could not find required dispatcher '" + reqdDispatcherName + "' in dispatcher map."
            | _ -> failwith <| "Static member 'RequiredDispatcherName' for facet '" + facetType.Name + "' is not of type string."

    /// Check for facet compatibility with the target's dispatcher.
    let isFacetCompatibleWithDispatcher dispatcherMap (facet : obj) (target : obj) =
        let facetType = facet.GetType ()
        let facetTypes = facetType :: getBaseTypesExceptObject facetType
        List.forall
            (fun facetType -> isFacetTypeCompatibleWithDispatcher dispatcherMap facetType target)
            facetTypes

    /// Attach intrinsic facets to a target by their names.
    let attachIntrinsicFacetsViaNames dispatcherMap facetMap facetNames (target : obj) =
        let facets =
            List.map
                (fun facetName ->
                    match Map.tryFind facetName facetMap with
                    | Some facet -> facet
                    | None -> failwith <| "Could not find facet '" + facetName + "' in facet map.")
                facetNames
        let targetType = target.GetType ()
        match targetType.GetPropertyWritable "FacetsNp" with
        | null -> failwith <| "Could not attach facet to type '" + targetType.Name + "'."
        | facetsNpProperty ->
            List.iter
                (fun facet ->
                    if not <| isFacetCompatibleWithDispatcher dispatcherMap facet target then
                        failwith <| "Facet of type '" + getTypeName facet + "' is not compatible with target '" + acstring target + "'.")
                facets
            facetsNpProperty.SetValue (target, facets)
            List.iter (fun facet -> attachFields facet target) facets

    /// Attach source's intrinsic facets to a target.
    let attachIntrinsicFacets dispatcherMap facetMap source target =
        let sourceType = source.GetType ()
        let instrinsicFacetNames = getIntrinsicFacetNames sourceType
        attachIntrinsicFacetsViaNames dispatcherMap facetMap instrinsicFacetNames target

    /// Create intrinsic overlays.
    let createIntrinsicOverlays hasFacetNamesField usesFacets sourceTypes =

        // get the unique, decomposed source types
        let sourceTypeHashSet = HashSet ()
        for sourceType in sourceTypes do
            for sourceTypeDecomposed in sourceType :: getBaseTypesExceptObject sourceType do
                ignore <| sourceTypeHashSet.Add sourceTypeDecomposed
        let sourceTypes = List.ofSeq sourceTypeHashSet

        // get the descriptors needed to construct the overlays
        let overlayDescriptors =
            List.map
                (fun (sourceType : Type) ->
                    let optBaseName = if sourceType.BaseType <> typeof<obj> then Some sourceType.BaseType.Name else None
                    let fieldDefinitions = getFieldDefinitionsNoInherit sourceType
                    let hasFacetNamesField = hasFacetNamesField sourceType
                    let optIntrinsicFacetNames =
                        if usesFacets sourceType
                        then Some <| getIntrinsicFacetNamesNoInherit sourceType
                        else None
                    (sourceType.Name, optBaseName, optIntrinsicFacetNames, fieldDefinitions, hasFacetNamesField))
                sourceTypes

        // create a document to house the overlay nodes
        let document = XmlDocument ()
        let root = document.CreateElement RootNodeName

        // construct the overlay nodes
        for (overlayName, optBaseName, optFacetNames, definitions, hasFacetNamesField) in overlayDescriptors do

            // make an empty overlay node
            let overlayNode = document.CreateElement overlayName

            // combine the overlay's include names into a single list
            let includeNames =
                match (optBaseName, optFacetNames) with
                | (Some baseName, Some facetNames) -> baseName :: facetNames
                | (Some baseName, None) -> [baseName]
                | (None, Some facetNames) -> facetNames
                | (None, None) -> []

            // construct the "includes" attribute
            match includeNames with
            | _ :: _ ->
                let includeAttribute = document.CreateAttribute IncludeAttributeName
                includeAttribute.InnerText <- AlgebraicDescriptor.convertToString includeNames
                ignore <| overlayNode.Attributes.Append includeAttribute
            | _ -> ()

            // construct the field nodes
            for definition in definitions do
                let fieldNode = document.CreateElement definition.FieldName
                match definition.FieldExpr with
                | Constant constant ->
                    let converter = AlgebraicConverter definition.FieldType
                    fieldNode.InnerText <- converter.ConvertToString constant
                    ignore <| overlayNode.AppendChild fieldNode
                | Variable _ -> ()

            // construct the "FacetNames" node if needed
            if hasFacetNamesField then
                let facetNamesNode = document.CreateElement "FacetNames"
                facetNamesNode.InnerText <- AlgebraicDescriptor.convertToString []
                ignore <| overlayNode.AppendChild facetNamesNode

            // append the overlay node
            ignore <| root.AppendChild overlayNode

        // append the root node
        ignore <| document.AppendChild root
        document