// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.ComponentModel
open System.Collections
open System.Collections.Generic
open System.Reflection
open Prime
open Nu

/// An evaluatable expression for defining a field.
type [<NoEquality; NoComparison>] FieldExpr =
    | DefineExpr of obj
    | VariableExpr of (unit -> obj)

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FieldExpr =

    /// Evaluate a field expression.
    let eval expr =
        match expr with
        | DefineExpr value -> value
        | VariableExpr fn -> fn ()

/// The definition of a data-driven field.
type [<NoEquality; NoComparison>] FieldDefinition =
    { FieldName : string
      FieldType : Type
      FieldExpr : FieldExpr }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FieldDefinition =

    /// Validate a field definition.
    let validate fieldDefinition =
        if fieldDefinition.FieldName = "FacetNames" then failwith "FacetNames cannot be an intrinsic field."
        if fieldDefinition.FieldName = "OptOverlayName" then failwith "OptOverlayName cannot be an intrinsic field."
        if Array.exists (fun gta -> gta = typeof<obj>) fieldDefinition.FieldType.GenericTypeArguments then
            failwith ^
                "Generic field definition lacking type information for field '" + fieldDefinition.FieldName + "'. " +
                "Use explicit typing on all values that carry incomplete type information such as empty lists, empty sets, and none options."

    /// Make a field definition.
    let make fieldName fieldType fieldExpr =
        { FieldName = fieldName; FieldType = fieldType; FieldExpr = fieldExpr }

    /// Make a field definition, validating it in the process.
    let makeValidated fieldName fieldType fieldExpr =
        let result = make fieldName fieldType fieldExpr
        validate result
        result

/// In tandem with the define literal, grants a nice syntax to define value fields.
type ValueDefinition =
    { ValueDefinition : unit }
    
    /// Some magic syntax for composing value fields.
    static member (?) (_, fieldName) =
        fun (value : 'v) ->
            FieldDefinition.makeValidated fieldName typeof<'v> ^ DefineExpr value

/// In tandem with the variable literal, grants a nice syntax to define variable fields.
type VariableDefinition =
    { VariableDefinition : unit }

    /// Some magic syntax for composing variable fields.
    static member (?) (_, fieldName) =
        fun (variable : unit -> 'v) ->
            FieldDefinition.makeValidated fieldName typeof<'v> ^ VariableExpr (fun () -> variable () :> obj)

/// In tandem with the define literal, grants a nice syntax to define field descriptors.
type FieldDescriptor =
    { FieldDescriptor : unit }
    
    /// Some magic syntax for composing value fields.
    static member (?) (_, fieldName : string) =
        fun (value : 'v) ->
            (fieldName, symbolize value)

[<AutoOpen>]
module ReflectionModule =

    /// In tandem with the ValueDefinition type, grants a nice syntax to define value fields.
    let Define = { ValueDefinition = () }

    /// In tandem with the VariableDefinition type, grants a nice syntax to define variable fields.
    let Variable = { VariableDefinition = () }
    
    /// In tandem with the FieldDescriptor type, grants a nice syntax to define field descriptors.
    let Field = { FieldDescriptor = () }

[<RequireQualifiedAccess>]
module Reflection =

    let private FieldDefinitionsCache =
        Dictionary<Type, FieldDefinition list> () // wonder if HashIdentity.Reference would work?

    /// Derive a simulant name from an optional name.
    /// TODO: see if we can improve the name of this function.
    let deriveName optName id =
        match optName with
        | Some name -> name
        | None -> Name.make ^ scstring id

    /// Derive a simulant id and name from an optional name.
    /// TODO: see if we can improve the name of this function.
    let deriveIdAndName optName =
        let id = makeGuid ()
        let name = deriveName optName id
        (id, name)

    /// Is a property with the given name persistent?
    let isPropertyPersistentByName (propertyName : string) =
        not ^ propertyName.EndsWith ("Id", StringComparison.Ordinal) && // don't write an Id
        not ^ propertyName.EndsWith ("Ids", StringComparison.Ordinal) && // don't write multiple Ids
        not ^ propertyName.EndsWith ("Np", StringComparison.Ordinal) // don't write non-persistent properties

    /// Is the property of the given target persistent?
    let isPropertyPersistent (property : PropertyInfo) (target : obj) =
        isPropertyPersistentByName property.Name &&
        not
            (property.Name = Constants.Engine.NameFieldName &&
             property.PropertyType = typeof<Name> &&
             fst ^ Guid.TryParse (property.GetValue target :?> Name |> Name.getNameStr))

    /// Query that the dispatcher has behavior congruent to the given type.
    let dispatchesAs (dispatcherTargetType : Type) (dispatcher : obj) =
        let dispatcherType = dispatcher.GetType ()
        let result =
            dispatcherTargetType = dispatcherType ||
            dispatcherType.IsSubclassOf dispatcherTargetType
        result

    /// Get the concrete base types of a type excepting the object type.
    let rec getBaseTypesExceptObject (targetType : Type) =
        match targetType.BaseType with
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
                    | _ -> failwith ^ "FieldDefinitions property for type '" + targetType.Name + "' must be of type FieldDefinition list."
            FieldDefinitionsCache.Add (targetType, fieldDefinitions)
            fieldDefinitions

    /// Get the field definitions of a target type.
    let getFieldDefinitions (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let fieldDefinitionLists = List.map (fun ty -> getFieldDefinitionsNoInherit ty) targetTypes
        let fieldDefinitionLists = List.rev fieldDefinitionLists
        List.concat fieldDefinitionLists

    /// Get the names of the field definitions of a target type.
    let getFieldDefinitionNames (targetType : Type) =
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
                    | null -> failwith ^ "Target '" + scstring target + "' does not implement dispatching in a compatible way."
                    | dispatcherNpProperty ->
                        let dispatcher = dispatcherNpProperty.GetValue target
                        dispatchesAs reqdDispatcherType dispatcher
                | None -> failwith ^ "Could not find required dispatcher '" + reqdDispatcherName + "' in dispatcher map."
            | _ -> failwith ^ "Static member 'RequiredDispatcherName' for facet '" + facetType.Name + "' is not of type string."

    /// Check for facet compatibility with the target's dispatcher.
    let isFacetCompatibleWithDispatcher dispatcherMap (facet : obj) (target : obj) =
        let facetType = facet.GetType ()
        let facetTypes = facetType :: getBaseTypesExceptObject facetType
        List.forall
            (fun facetType -> isFacetTypeCompatibleWithDispatcher dispatcherMap facetType target)
            facetTypes

    /// Get all the reflective field containers of a target, including dispatcher and / or facets.
    let getReflectiveFieldContainers (target : obj) =
        let targetType = target.GetType ()
        let optDispatcher =
            match targetType.GetProperty "DispatcherNp" with
            | null -> None
            | dispatcherNpProperty -> Some ^ objectify ^ dispatcherNpProperty.GetValue target
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
    let getReflectiveFieldContainerTypes (target : obj) =
        let fieldContainers = getReflectiveFieldContainers target
        List.map getType fieldContainers

    /// Get all the reflective field definitions of a type, including those of its dispatcher and /
    /// or facets, organized in a map from the containing type's name to the field definition.
    let getReflectiveFieldDefinitionMap (target : obj) =
        let containerTypes = getReflectiveFieldContainerTypes target
        Map.ofListBy (fun (ty : Type) -> (ty.Name, getFieldDefinitions ty)) containerTypes

    /// Get all the unique reflective field definitions of a type, including those of its
    /// dispatcher and / or facets.
    let getReflectiveFieldDefinitions (target : obj) =
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
            | _ -> failwith ^ "IntrinsicFacetNames property for type '" + targetType.Name + "' must be of type string list."

    /// Get the intrinsic facet names of a target type.
    let getIntrinsicFacetNames (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let intrinsicFacetNamesLists = List.map (fun ty -> getIntrinsicFacetNamesNoInherit ty) targetTypes
        let intrinsicFacetNamesLists = List.rev intrinsicFacetNamesLists
        List.concat intrinsicFacetNamesLists

    /// Attach fields from the given definitions to a target.
    let attachFieldsViaDefinitions fieldDefinitions (target : obj) =
        let targetType = target.GetType ()
        for fieldDefinition in fieldDefinitions do
            let fieldValue = FieldExpr.eval fieldDefinition.FieldExpr
            match targetType.GetPropertyWritable fieldDefinition.FieldName with
            | null ->
                match targetType.GetPropertyWritable "Xtension" with
                | null -> failwith ^ "Invalid field '" + fieldDefinition.FieldName + "' for target type '" + targetType.Name + "'."
                | xtensionProperty ->
                    match xtensionProperty.GetValue target with
                    | :? Xtension as xtension ->
                        let xField = { FieldValue = fieldValue; FieldType = fieldDefinition.FieldType }
                        let xtension = Xtension.attachField fieldDefinition.FieldName xField xtension
                        xtensionProperty.SetValue (target, xtension)
                    | _ -> failwith ^ "Invalid field '" + fieldDefinition.FieldName + "' for target type '" + targetType.Name + "'."
            | property -> property.SetValue (target, fieldValue)

    /// Detach fields from a target.
    let detachFieldsViaNames fieldNames (target : obj) =
        let targetType = target.GetType ()
        for fieldName in fieldNames do
            match targetType.GetPropertyWritable fieldName with
            | null ->
                match targetType.GetPropertyWritable "Xtension" with
                | null -> failwith ^ "Invalid field '" + fieldName + "' for target type '" + targetType.Name + "'."
                | xtensionProperty ->
                    match xtensionProperty.GetValue target with
                    | :? Xtension as xtension ->
                        let xtension = Xtension.detachField fieldName xtension
                        xtensionProperty.SetValue (target, xtension)
                    | _ -> failwith ^ "Invalid field '" + fieldName + "' for target type '" + targetType.Name + "'."
            | _ -> ()

    /// Attach source's fields to a target.
    let attachFields source (target : obj) =
        let sourceType = source.GetType ()
        let fieldDefinitions = getFieldDefinitions sourceType
        attachFieldsViaDefinitions fieldDefinitions target

    /// Detach source's fields to a target.
    let detachFields source (target : obj) =
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
                    | None -> failwith ^ "Could not find facet '" + facetName + "' in facet map.")
                facetNames
        let targetType = target.GetType ()
        match targetType.GetPropertyWritable "FacetsNp" with
        | null -> failwith ^ "Could not attach facet to type '" + targetType.Name + "'."
        | facetsNpProperty ->
            List.iter
                (fun facet ->
                    if not ^ isFacetCompatibleWithDispatcher dispatcherMap facet target
                    then failwith ^ "Facet of type '" + getTypeName facet + "' is not compatible with target '" + scstring target + "'."
                    else ())
                facets
            facetsNpProperty.SetValue (target, facets)
            List.iter (fun facet -> attachFields facet target) facets

    /// Attach source's intrinsic facets to a target.
    let attachIntrinsicFacets dispatcherMap facetMap (source : obj) (target : obj) =
        let sourceType = source.GetType ()
        let instrinsicFacetNames = getIntrinsicFacetNames sourceType
        attachIntrinsicFacetsViaNames dispatcherMap facetMap instrinsicFacetNames target

    /// Try to read just the target's OptOverlayName from field descriptors.
    let tryReadOptOverlayNameToTarget fieldDescriptors (target : obj) =
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
            match Map.tryFind optOverlayNameProperty.Name fieldDescriptors with
            | Some optOverlayNameSymbol ->
                let optOverlayName = valueize<string option> optOverlayNameSymbol
                optOverlayNameProperty.SetValue (target, optOverlayName)
            | None -> ()
        | None -> ()

    /// Read just the target's FacetNames from field descriptors.
    let readFacetNamesToTarget fieldDescriptors (target : obj) =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        let facetNamesProperty =
            Array.find
                (fun (property : PropertyInfo) ->
                    property.Name = "FacetNames" &&
                    property.PropertyType = typeof<string Set> &&
                    property.CanWrite)
                targetProperties
        match Map.tryFind facetNamesProperty.Name fieldDescriptors with
        | Some facetNamesSymbol ->
            let facetNames = valueize<string list> facetNamesSymbol
            facetNamesProperty.SetValue (target, facetNames)
        | None -> ()

    /// Attempt to read a target's .NET property from field descriptors.
    let tryReadPropertyToTarget (property : PropertyInfo) fieldDescriptors (target : obj) =
        match Map.tryFind property.Name fieldDescriptors with
        | Some fieldSymbol ->
            let converter = SymbolicConverter property.PropertyType
            if converter.CanConvertFrom typeof<Symbol> then
                let fieldValue = converter.ConvertFrom fieldSymbol
                property.SetValue (target, fieldValue)
        | None -> ()

    /// Read all of a target's .NET properties from field descriptors (except OptOverlayName and FacetNames).
    let readPropertiesToTarget fieldDescriptors (target : obj) =
        let properties = (target.GetType ()).GetPropertiesWritable ()
        for property in properties do
            if  property.Name <> "FacetNames" &&
                property.Name <> "OptOverlayName" &&
                isPropertyPersistentByName property.Name then
                tryReadPropertyToTarget property fieldDescriptors target

    /// Read one of a target's XFields from field descriptors.
    let readXField xtension fieldDescriptors (target : obj) fieldDefinition =
        let targetType = target.GetType ()
        if Seq.notExists
            (fun (property : PropertyInfo) -> property.Name = fieldDefinition.FieldName)
            (targetType.GetProperties ()) then
            match Map.tryFind fieldDefinition.FieldName fieldDescriptors with
            | Some fieldSymbol ->
                let converter = SymbolicConverter fieldDefinition.FieldType
                if converter.CanConvertFrom typeof<Symbol> then
                    let xField = { FieldValue = converter.ConvertFrom fieldSymbol; FieldType = fieldDefinition.FieldType }
                    Xtension.attachField fieldDefinition.FieldName xField xtension
                else
                    Log.debug ^ "Cannot convert field '" + scstring fieldSymbol + "' to type '" + fieldDefinition.FieldType.Name + "'."
                    xtension
            | None -> xtension
        else xtension

    /// Read a target's XFields from field descriptors.
    let readXFields xtension fieldDescriptors (target : obj) =
        let fieldDefinitions = getReflectiveFieldDefinitions target
        List.fold (fun xtension -> readXField xtension fieldDescriptors target) xtension fieldDefinitions

    /// Read a target's Xtension from field descriptors.
    let readXtensionToTarget fieldDescriptors (target : obj) =
        let targetType = target.GetType ()
        match targetType.GetProperty "Xtension" with
        | null -> Log.debug "Target does not support xtensions due to missing Xtension field."
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let xtension = readXFields xtension fieldDescriptors target
                xtensionProperty.SetValue (target, xtension)
            | _ -> Log.debug "Target does not support xtensions due to Xtension field having unexpected type."

    /// Read all of a target's member values from field descriptors (except OptOverlayName and FacetNames).
    let readMemberValuesToTarget fieldDescriptors (target : obj) =
        readPropertiesToTarget fieldDescriptors target
        readXtensionToTarget fieldDescriptors target

    /// Write an Xtension to fields descriptors.
    let writeXtension shouldWriteProperty fieldDescriptors xtension =
        Seq.fold (fun fieldDescriptors (xFieldName, (xField : XField)) ->
            let xFieldType = xField.FieldType
            let xFieldValue = xField.FieldValue
            if  isPropertyPersistentByName xFieldName &&
                shouldWriteProperty xFieldName xFieldType xFieldValue then
                let xFieldSymbol = (SymbolicConverter xFieldType).ConvertTo (xFieldValue, typeof<Symbol>) :?> Symbol
                Map.add xFieldName xFieldSymbol fieldDescriptors
            else fieldDescriptors)
            fieldDescriptors
            (Xtension.toSeq xtension)

    /// Write all of a target's member values to field descriptors.
    let writeMemberValuesFromTarget shouldWriteProperty fieldDescriptors (target : obj) =
        let targetType = target.GetType ()
        let properties = targetType.GetProperties ()
        Seq.fold (fun fieldDescriptors (property : PropertyInfo) ->
            match property.GetValue target with
            | :? Xtension as xtension ->
                writeXtension shouldWriteProperty fieldDescriptors xtension
            | propertyValue ->
                if  isPropertyPersistent property target &&
                    shouldWriteProperty property.Name property.PropertyType propertyValue then
                    let valueSymbol = (SymbolicConverter property.PropertyType).ConvertTo (propertyValue, typeof<Symbol>) :?> Symbol
                    Map.add property.Name valueSymbol fieldDescriptors
                else fieldDescriptors)
            fieldDescriptors
            properties

    /// Create intrinsic overlays.
    let createIntrinsicOverlays requiresFacetNames sourceTypes =

        // get the unique, decomposed source types
        let sourceTypeHashSet = HashSet () // wonder if HashIdentity.Reference would work?
        for sourceType in sourceTypes do
            for sourceTypeDecomposed in sourceType :: getBaseTypesExceptObject sourceType do
                sourceTypeHashSet.Add sourceTypeDecomposed |> ignore
        let sourceTypes = List.ofSeq sourceTypeHashSet

        // get the descriptors needed to construct the overlays
        let overlayDescriptors =
            List.map
                (fun (sourceType : Type) ->
                    let includeNames = if sourceType.BaseType <> typeof<obj> then [sourceType.BaseType.Name] else []
                    let fieldDefinitions = getFieldDefinitionsNoInherit sourceType
                    let requiresFacetNames = requiresFacetNames sourceType
                    (sourceType.Name, includeNames, fieldDefinitions, requiresFacetNames))
                sourceTypes

        // create the intrinsic overlays with the above descriptors
        let overlays =
            List.foldBack
                (fun (overlayName, includeNames, fieldDefinitions, requiresFacetNames) overlays ->
                    let overlayProperties =
                        List.foldBack
                            (fun fieldDefinition overlayProperties ->
                                match fieldDefinition.FieldExpr with
                                | DefineExpr value ->
                                    let converter = SymbolicConverter fieldDefinition.FieldType
                                    let overlayProperty = converter.ConvertTo (value, typeof<Symbol>) :?> Symbol
                                    Map.add fieldDefinition.FieldName overlayProperty overlayProperties
                                | VariableExpr _ -> overlayProperties)
                            fieldDefinitions
                            Map.empty
                    let overlayProperties =
                        if requiresFacetNames
                        then Map.add "FacetNames" (Symbols []) overlayProperties
                        else overlayProperties
                    let overlay =
                        { OverlayName = overlayName
                          OverlayIncludeNames = includeNames
                          OverlayProperties = overlayProperties }
                    Map.add overlayName overlay overlays)
                overlayDescriptors
                Map.empty
        
        // fin
        overlays