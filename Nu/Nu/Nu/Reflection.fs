namespace Nu
open System
open System.ComponentModel
open System.Collections.Generic
open System.Reflection
open System.Xml
open Prime
open Nu.NuConstants

[<AutoOpen>]
module ReflectionModule =

    /// An evaluatable expression for defining an XField.
    type [<NoEquality; NoComparison>] FieldExpression =
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
          FieldExpression : FieldExpression }

        static member private validate fieldName (fieldType : Type) (_ : FieldExpression) =
            if fieldName = "FacetNames" then failwith "FacetNames cannot be an intrinsic field."
            if fieldName = "OptOverlayName" then failwith "OptOverlayName cannot be an intrinsic field."
            if Array.exists (fun gta -> gta = typeof<obj>) fieldType.GenericTypeArguments then
                failwith <|
                    "Generic field definition lacking too much type information for field '" + fieldName + "'. " +
                    "Use explicit type annotations on all values that carry incomplete type information such as empty lists."

        static member make fieldName fieldType fieldExpression =
            FieldDefinition.validate fieldName fieldType fieldExpression
            { FieldName = fieldName; FieldType = fieldType; FieldExpression = fieldExpression }

    /// In tandem with the define literal, grants a nice syntax to define constant XFields.
    type DefineConstant =
        { DefineConstant : unit }
        static member (?) (_, fieldName) =
            fun (constant : 'c) ->
                FieldDefinition.make fieldName typeof<'c> (Constant constant)

    /// In tandem with the variable literal, grants a nice syntax to define variable XFields.
    type DefineVariable =
        { DefineVariable : unit }
        static member (?) (_, fieldName) =
            fun (variable : unit -> 'v) ->
                FieldDefinition.make fieldName typeof<'v> (Variable (fun () -> variable () :> obj))

    /// In tandem with the DefineConstant type, grants a nice syntax to define constant XFields.
    let define = { DefineConstant = () }

    /// In tandem with the DefineLiteral type, grants a nice syntax to define variable XFields.
    let variable = { DefineVariable = () }

[<RequireQualifiedAccess>]
module Reflection =

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

    /// Attach fields to a target.
    let attachFields fieldDefinitions target =
        let targetType = target.GetType ()
        List.iter
            (fun fieldDefinition ->
                let fieldValue = FieldExpression.eval fieldDefinition.FieldExpression
                match targetType.GetPropertyWritable fieldDefinition.FieldName with
                | null ->
                    match targetType.GetPropertyWritable "Xtension" with
                    | null -> ()
                    | xtensionProperty ->
                        match xtensionProperty.GetValue target with
                        | :? Xtension as xtension ->
                            let xfields = Map.add fieldDefinition.FieldName fieldValue xtension.XFields
                            let xtension = { xtension with XFields = xfields }
                            xtensionProperty.SetValue (target, xtension)
                        | _ -> ()
                | property -> property.SetValue (target, fieldValue))
            fieldDefinitions

    /// Detach fields from a target.
    let detachFields fieldDefinitions target =
        let targetType = target.GetType ()
        List.iter
            (fun fieldDefinition ->
                match targetType.GetPropertyWritable fieldDefinition.FieldName with
                | null ->
                    match targetType.GetPropertyWritable "Xtension" with
                    | null -> ()
                    | xtensionProperty ->
                        match xtensionProperty.GetValue target with
                        | :? Xtension as xtension ->
                            let xfields = Map.remove fieldDefinition.FieldName xtension.XFields
                            let xtension = { xtension with XFields = xfields }
                            xtensionProperty.SetValue (target, xtension)
                        | _ -> ()
                | _ -> ())
            fieldDefinitions

    /// Get the field definitions of a target type not considering inheritance.
    let getFieldDefinitionsNoInherit (targetType : Type) =
        match targetType.GetProperty ("FieldDefinitions", BindingFlags.Static ||| BindingFlags.Public) with
        | null -> failwith <| "Could not get static property FieldDefinitions from type '" + targetType.Name + "'."
        | fieldDefinitionsProperty ->
            let fieldDefinitions = fieldDefinitionsProperty.GetValue null
            match fieldDefinitions with
            | :? (obj list) as fieldDefinitions when List.isEmpty fieldDefinitions -> []
            | :? (FieldDefinition list) as fieldDefinitions -> fieldDefinitions
            | _ -> failwith <| "FieldDefinitions property for type '" + targetType.Name + "' must be of type FieldDefinition list."

    /// Get the intrinsic facet names of a target type not considering inheritance.
    let getIntrinsicFacetNamesNoInherit (targetType : Type) =
        match targetType.GetProperty ("IntrinsicFacetNames", BindingFlags.Static ||| BindingFlags.Public) with
        | null -> failwith <| "Could not get static property IntrinsicFacetNames from type '" + targetType.Name + "'."
        | intrinsicFacetNamesProperty ->
            let intrinsicFacetNames = intrinsicFacetNamesProperty.GetValue null
            match intrinsicFacetNames with
            | :? (obj list) as intrinsicFacetNames when List.isEmpty intrinsicFacetNames -> []
            | :? (string list) as intrinsicFacetNames -> intrinsicFacetNames
            | _ -> failwith <| "IntrinsicFacetNames property for type '" + targetType.Name + "' must be of type string list."

    /// Get the field definitions of a target type.
    let getFieldDefinitions (targetType : Type) =
        let baseTypes = targetType :: getBaseTypesExceptObject targetType
        let fieldDefinitionLists = List.map (fun aType -> getFieldDefinitionsNoInherit aType) baseTypes
        let fieldDefinitionLists = List.rev fieldDefinitionLists
        List.concat fieldDefinitionLists

    /// Get the intrinsic facet names of a target type.
    let getIntrinsicFacetNames (targetType : Type) =
        let baseTypes = targetType :: getBaseTypesExceptObject targetType
        let intrinsicFacetNamesLists = List.map (fun aType -> getIntrinsicFacetNamesNoInherit aType) baseTypes
        let intrinsicFacetNamesLists = List.rev intrinsicFacetNamesLists
        List.concat intrinsicFacetNamesLists

    /// Get the names of the field definition of a target type.
    let getFieldDefinitionNames targetType =
        let fieldDefinitions = getFieldDefinitions targetType
        List.map (fun fieldDefinition -> fieldDefinition.FieldName) fieldDefinitions

    /// Attach source's fields to a target.
    let attachFieldsFromSource source target =
        let sourceType = source.GetType ()
        let fieldDefinitions = getFieldDefinitions sourceType
        attachFields fieldDefinitions target

    /// Detach source's fields to a target.
    let detachFieldsFromSource source target =
        let sourceType = source.GetType ()
        let fieldDefinitions = getFieldDefinitions sourceType
        detachFields fieldDefinitions target

    /// Attach intrinsic facets to a target.
    let attachIntrinsicFacets facetNames target facetMap =
        let facets =
            List.map
                (fun facetName ->
                    match Map.tryFind facetName facetMap with
                    | Some facet -> facet
                    | None -> failwith <| "Could not locate facet '" + facetName + "'.")
                facetNames
        let targetType = target.GetType ()
        match targetType.GetPropertyWritable "FacetsNp" with
        | null -> failwith <| "Could not attach facet to type '" + targetType.Name + "'."
        | property ->
            property.SetValue (target, facets)
            List.iter
                (fun facet -> attachFieldsFromSource facet target)
                facets

    /// Attach source's intrinsic facets to a target.
    let attachIntrinsicFacetsFromSource source target facets =
        let sourceType = source.GetType ()
        let instrinsicFacetNames = getIntrinsicFacetNames sourceType
        attachIntrinsicFacets instrinsicFacetNames target facets

    /// Create intrinsic overlays.
    let createIntrinsicOverlays hasFacetNamesField usesFacets sourceTypes =

        // get the unique, decomposed source types
        let sourceTypeHashSet = HashSet ()
        List.iter
            (fun sourceType ->
                let sourceTypes = sourceType :: getBaseTypesExceptObject sourceType
                List.iter (fun sourceType -> ignore <| sourceTypeHashSet.Add sourceType) sourceTypes)
            sourceTypes
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
        List.iter
            (fun (overlayName, optBaseName, optFacetNames, definitions, hasFacetNamesField) ->

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
                    includeAttribute.InnerText <- string includeNames
                    ignore <| overlayNode.Attributes.Append includeAttribute
                | _ -> ()

                // construct the field nodes
                List.iter
                    (fun definition ->
                        let fieldNode = document.CreateElement definition.FieldName
                        match definition.FieldExpression with
                        | Constant constant ->
                            let converter = TypeDescriptor.GetConverter definition.FieldType
                            fieldNode.InnerText <- converter.ConvertToString constant
                            ignore <| overlayNode.AppendChild fieldNode
                        | Variable _ -> ())
                    definitions

                // construct the "FacetNames" node if needed
                if hasFacetNamesField then
                    let facetNamesNode = document.CreateElement "FacetNames"
                    facetNamesNode.InnerText <- string []
                    ignore <| overlayNode.AppendChild facetNamesNode

                // append the overlay node
                ignore <| root.AppendChild overlayNode)
            overlayDescriptors

        // append the root node
        ignore <| document.AppendChild root
        document