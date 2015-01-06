namespace Nu
open System
open System.ComponentModel
open System.Collections
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

        /// Make a field definition.
        static member make fieldName fieldType fieldExpr =
            FieldDefinition.validate fieldName fieldType fieldExpr
            { FieldName = fieldName; FieldType = fieldType; FieldExpr = fieldExpr }

    /// In tandem with the define literal, grants a nice syntax to define constant fields.
    type DefineConstant =
        { DefineConstant : unit }
        
        /// Some magic syntax for composing constant fields.
        static member (?) (_, fieldName) =
            fun (constant : 'c) ->
                FieldDefinition.make fieldName typeof<'c> <| Constant constant

    /// In tandem with the variable literal, grants a nice syntax to define variable fields.
    type DefineVariable =
        { DefineVariable : unit }

        /// Some magic syntax for composing variable fields.
        static member (?) (_, fieldName) =
            fun (variable : unit -> 'v) ->
                FieldDefinition.make fieldName typeof<'v> <| Variable (fun () -> variable () :> obj)

    /// In tandem with the DefineConstant type, grants a nice syntax to define constant fields.
    let define = { DefineConstant = () }

    /// In tandem with the DefineVariable type, grants a nice syntax to define variable fields.
    let variable = { DefineVariable = () }

[<RequireQualifiedAccess>]
module Reflection =

    let private FieldDefinitionsCache =
        Dictionary<Type, FieldDefinition list> ()

    /// Get the type name of a target.
    let getTypeName target =
        let targetType = target.GetType ()
        targetType.Name

    /// Is a property with the given name persistent?
    let isPropertyPersistentByName (propertyName : string) =
        not <| propertyName.EndsWith "Id" && // don't write an Id
        not <| propertyName.EndsWith "Ids" && // don't write multiple Ids
        not <| propertyName.EndsWith "Np" // don't write non-persistent properties

    /// Is a property with the given name persistent?
    let isPropertyPersistent target (property : PropertyInfo) =
        isPropertyPersistentByName property.Name &&
        not (
            property.Name = NameFieldName &&
            property.PropertyType = typeof<string> &&
            fst <| Guid.TryParse (property.GetValue target :?> string))

    /// Query that the dispatcher has behavior congruent to the given type.
    let dispatchesAs (dispatcherTargetType : Type) dispatcher =
        let dispatcherType = dispatcher.GetType ()
        let result =
            dispatcherTargetType = dispatcherType ||
            dispatcherType.IsSubclassOf dispatcherTargetType
        result

    /// Get the concrete base types of a type excepting the object type.
    let rec getBaseTypesExceptObject (aType : Type) =
        match aType.BaseType with
        | null -> []
        | baseType ->
            if baseType <> typeof<obj>
            then baseType :: getBaseTypesExceptObject baseType
            else []

    /// Get the field definitions of a target type not considering inheritance.
    /// OPTIMIZATION: Memoized for efficiency since FieldDefinitions properties will likely return
    /// a newly constructed list.
    let getFieldDefinitionsNoInherit (targetType : Type) =
        match FieldDefinitionsCache.TryGetValue targetType with
        | (true, fieldDefinitions) -> fieldDefinitions
        | (false, _) ->
            let fieldDefinitions =
                match targetType.GetProperty ("FieldDefinitions", BindingFlags.Static ||| BindingFlags.Public) with
                | null -> []
                | fieldDefinitionsProperty ->
                    match fieldDefinitionsProperty.GetValue null with
                    | :? (obj list) as definitions when List.isEmpty definitions -> []
                    | :? (FieldDefinition list) as definitions -> definitions
                    | _ -> failwith <| "FieldDefinitions property for type '" + targetType.Name + "' must be of type FieldDefinition list."
            FieldDefinitionsCache.Add (targetType, fieldDefinitions)
            fieldDefinitions

    /// Get the field definitions of a target type.
    let getFieldDefinitions (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let fieldDefinitionLists = List.map (fun aType -> getFieldDefinitionsNoInherit aType) targetTypes
        let fieldDefinitionLists = List.rev fieldDefinitionLists
        List.concat fieldDefinitionLists

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
                    | null -> failwith <| "Target '" + acstring target + "' does not implement dispatching in a compatible way."
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

    /// Get all the reflective field containers of a target, including dispatcher and / or facets.
    let getReflectiveFieldContainers target =
        let targetType = target.GetType ()
        let optDispatcher =
            match targetType.GetProperty "DispatcherNp" with
            | null -> None
            | dispatcherNpProperty -> Some <| objectify ^^ dispatcherNpProperty.GetValue target
        let optFacets =
            match targetType.GetProperty "FacetsNp" with
            | null -> None
            | facetsNpProperty ->
                let facets = facetsNpProperty.GetValue target :?> IEnumerable |> enumerable<obj> |> List.ofSeq
                Some facets
        match (optDispatcher, optFacets) with
        | (Some dispatcher, Some facets) -> dispatcher :: facets
        | (Some dispatcher, None) -> [dispatcher]
        | (None, Some facets) -> facets
        | (None, None) -> []

    /// Get all the reflective container types of a target, including dispatcher and / or facet types.
    let getReflectiveFieldContainerTypes target =
        let fieldContainers = getReflectiveFieldContainers target
        List.map getType fieldContainers

    /// Get all the reflective field definitions of a type, including those of its dispatcher and /
    /// or facets, organized in a map from the containing type's name to the field definition.
    let getReflectiveFieldDefinitionMap target =
        let containerTypes = getReflectiveFieldContainerTypes target
        Map.ofListBy (fun (aType : Type) -> (aType.Name, getFieldDefinitions aType)) containerTypes

    /// Get all the unique reflective field definitions of a type, including those of its
    /// dispatcher and / or facets.
    let getReflectiveFieldDefinitions target =
        let types = getReflectiveFieldContainerTypes target
        let fieldDefinitionLists = List.map getFieldDefinitions types
        let fieldDefinitions = List.concat fieldDefinitionLists
        let fieldDefinitions = Map.ofListBy (fun field -> (field.FieldName, field)) fieldDefinitions
        Map.toValueList fieldDefinitions

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

    /// Get the intrinsic facet names of a target type.
    let getIntrinsicFacetNames (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let intrinsicFacetNamesLists = List.map (fun aType -> getIntrinsicFacetNamesNoInherit aType) targetTypes
        let intrinsicFacetNamesLists = List.rev intrinsicFacetNamesLists
        List.concat intrinsicFacetNamesLists

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
                    if not <| isFacetCompatibleWithDispatcher dispatcherMap facet target
                    then failwith <| "Facet of type '" + getTypeName facet + "' is not compatible with target '" + acstring target + "'."
                    else ())
                facets
            facetsNpProperty.SetValue (target, facets)
            List.iter (fun facet -> attachFields facet target) facets

    /// Attach source's intrinsic facets to a target.
    let attachIntrinsicFacets dispatcherMap facetMap source target =
        let sourceType = source.GetType ()
        let instrinsicFacetNames = getIntrinsicFacetNames sourceType
        attachIntrinsicFacetsViaNames dispatcherMap facetMap instrinsicFacetNames target

    /// Read dispatcherName from an xml node.
    let readDispatcherName defaultDispatcherName (node : XmlNode) =
        match node.Attributes.[DispatcherNameAttributeName] with
        | null -> defaultDispatcherName
        | dispatcherNameAttribute -> dispatcherNameAttribute.InnerText

    /// Read opt overlay name from an xml node.
    let readOptOverlayName (node : XmlNode) =
        let optOverlayNameStr = node.InnerText
        AlgebraicDescriptor.convertFromString optOverlayNameStr typeof<string option> :?> string option

    /// Read facet names from an xml node.
    let readFacetNames (node : XmlNode) =
        let facetNamesStr = node.InnerText
        AlgebraicDescriptor.convertFromString facetNamesStr typeof<string list> :?> string list

    /// Try to read just the target's OptOverlayName from Xml.
    let tryReadOptOverlayNameToTarget (targetNode : XmlNode) target =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        let optOptOverlayNameProperty =
            Array.tryFind
                (fun (property : PropertyInfo) ->
                    property.Name = "OptOverlayName" &&
                    property.PropertyType = typeof<string option> &&
                    property.CanWrite)
                targetProperties
        match optOptOverlayNameProperty with
        | Some optOverlayNameProperty ->
            match targetNode.[optOverlayNameProperty.Name] with
            | null -> ()
            | optOverlayNameNode ->
                let optOverlayName = readOptOverlayName optOverlayNameNode
                optOverlayNameProperty.SetValue (target, optOverlayName)
        | None -> ()

    /// Read just the target's FacetNames from Xml.
    let readFacetNamesToTarget (targetNode : XmlNode) target =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        let facetNamesProperty =
            Array.find
                (fun (property : PropertyInfo) ->
                    property.Name = "FacetNames" &&
                    property.PropertyType = typeof<string list> &&
                    property.CanWrite)
                targetProperties
        match targetNode.[facetNamesProperty.Name] with
        | null -> ()
        | facetNamesNode ->
            let facetNames = readFacetNames facetNamesNode
            facetNamesProperty.SetValue (target, facetNames)

    /// Attempt to read a target's .NET property from Xml.
    let tryReadDotNetPropertyToTarget (property : PropertyInfo) (targetNode : XmlNode) (target : 'a) =
        match targetNode.SelectSingleNode property.Name with
        | null -> ()
        | fieldNode ->
            let fieldValueStr = fieldNode.InnerText
            let converter = AlgebraicConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let fieldValue = converter.ConvertFromString fieldValueStr
                property.SetValue (target, fieldValue)

    /// Read all of a target's .NET properties from Xml (except OptOverlayName and FacetNames).
    let readDotNetPropertiesToTarget (targetNode : XmlNode) target =
        let properties = (target.GetType ()).GetPropertiesWritable ()
        for property in properties do
            if  property.Name <> "FacetNames" &&
                property.Name <> "OptOverlayName" &&
                isPropertyPersistentByName property.Name then
                tryReadDotNetPropertyToTarget property targetNode target

    /// Read one of a target's XFields.
    let readXField (targetNode : XmlNode) (target : obj) targetXFields fieldDefinition =
        let targetType = target.GetType ()
        if Seq.notExists
            (fun (property : PropertyInfo) -> property.Name = fieldDefinition.FieldName)
            (targetType.GetProperties ()) then
            match targetNode.SelectSingleNode fieldDefinition.FieldName with
            | null -> targetXFields
            | fieldNode ->
                let converter = AlgebraicConverter fieldDefinition.FieldType
                if converter.CanConvertFrom typeof<string> then
                    let xField = { FieldValue = converter.ConvertFromString fieldNode.InnerText; FieldType = fieldDefinition.FieldType }
                    Map.add fieldDefinition.FieldName xField targetXFields
                else debug <| "Cannot convert string '" + fieldNode.InnerText + "' to type '" + fieldDefinition.FieldType.Name + "'."; targetXFields
        else targetXFields

    /// Read a target's XFields.
    let readXFields targetXFields (targetNode : XmlNode) (target : obj) =
        let fieldDefinitions = getReflectiveFieldDefinitions target
        List.fold (readXField targetNode target) targetXFields fieldDefinitions

    /// Read a target's Xtension.
    let readXtensionToTarget (targetNode : XmlNode) (target : obj) =
        let targetType = target.GetType ()
        match targetType.GetProperty "Xtension" with
        | null -> debug "Target does not support xtensions due to missing Xtension field."
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let xFields = readXFields xtension.XFields targetNode target
                let xtension = { xtension with XFields = xFields }
                xtensionProperty.SetValue (target, xtension)
            | _ -> debug "Target does not support xtensions due to Xtension field having unexpected type."

    /// Read all of a target's properties from Xml (except OptOverlayName and FacetNames).
    let readPropertiesToTarget targetNode target =
        readDotNetPropertiesToTarget targetNode target
        readXtensionToTarget targetNode target

    /// Write an Xtension to Xml.
    /// NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
    /// XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
    let writeXtension shouldWriteProperty (writer : XmlWriter) xtension =
        for xFieldKvp in xtension.XFields do
            let xFieldName = xFieldKvp.Key
            let xFieldType = xFieldKvp.Value.FieldType
            let xFieldValue = xFieldKvp.Value.FieldValue
            if  isPropertyPersistentByName xFieldName &&
                shouldWriteProperty xFieldName xFieldType xFieldValue then
                let xFieldValueStr = (AlgebraicConverter xFieldType).ConvertToString xFieldValue
                writer.WriteStartElement xFieldName
                writer.WriteString xFieldValueStr
                writer.WriteEndElement ()

    /// Write all of a target's properties to Xml.
    /// NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
    /// XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
    let writePropertiesFromTarget shouldWriteProperty (writer : XmlWriter) (target : 'a) =
        let targetType = target.GetType ()
        let properties = targetType.GetProperties ()
        for property in properties do
            let propertyValue = property.GetValue target
            match propertyValue with
            | :? Xtension as xtension -> writeXtension shouldWriteProperty writer xtension
            | _ ->
                if  isPropertyPersistent target property &&
                    shouldWriteProperty property.Name property.PropertyType propertyValue then
                    let converter = AlgebraicConverter property.PropertyType
                    let valueStr = converter.ConvertToString propertyValue
                    writer.WriteElementString (property.Name, valueStr)

    /// Create intrinsic overlays.
    let createIntrinsicOverlays hasFacetNamesField sourceTypes =

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
                    let includeNames = if sourceType.BaseType <> typeof<obj> then [sourceType.BaseType.Name] else []
                    let fieldDefinitions = getFieldDefinitionsNoInherit sourceType
                    let hasFacetNamesField = hasFacetNamesField sourceType
                    (sourceType.Name, includeNames, fieldDefinitions, hasFacetNamesField))
                sourceTypes

        // create a document to house the overlay nodes
        let document = XmlDocument ()
        let root = document.CreateElement RootNodeName

        // construct the overlay nodes
        for (overlayName, includeNames, definitions, hasFacetNamesField) in overlayDescriptors do

            // make an empty overlay node
            let overlayNode = document.CreateElement overlayName

            // construct the "includes" attribute
            match includeNames with
            | _ :: _ ->
                let includesAttribute = document.CreateAttribute IncludesAttributeName
                includesAttribute.InnerText <- AlgebraicDescriptor.convertToString includeNames
                ignore <| overlayNode.Attributes.Append includesAttribute
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