// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Reflection
open Prime
open Nu

/// An evaluatable expression for defining a property.
type [<NoEquality; NoComparison>] PropertyExpr =
    | DefineExpr of obj
    | VariableExpr of (unit -> obj)

    /// Evaluate a property expression.
    static member eval expr =
        match expr with
        | DefineExpr value -> value
        | VariableExpr fn -> fn ()

/// The definition of a data-driven property.
type [<NoEquality; NoComparison>] PropertyDefinition =
    { PropertyName : string
      PropertyType : Type
      PropertyExpr : PropertyExpr }

    /// Validate a property definition.
    static member validate propertyDefinition =
        if propertyDefinition.PropertyName = "FacetNames" then failwith "FacetNames cannot be an intrinsic property."
        if propertyDefinition.PropertyName = "OverlayNameOpt" then failwith "OverlayNameOpt cannot be an intrinsic property."
        if Array.exists (fun gta -> gta = typeof<obj>) propertyDefinition.PropertyType.GenericTypeArguments then
            failwith ^
                "Generic property definition lacking type information for property '" + propertyDefinition.PropertyName + "'. " +
                "Use explicit typing on all values that carry incomplete type information such as empty lists, empty sets, and none options."

    /// Make a property definition.
    static member make propertyName propertyType propertyExpr =
        { PropertyName = propertyName; PropertyType = propertyType; PropertyExpr = propertyExpr }

    /// Make a property definition, validating it in the process.
    static member makeValidated propertyName propertyType propertyExpr =
        let result = PropertyDefinition.make propertyName propertyType propertyExpr
        PropertyDefinition.validate result
        result

/// In tandem with the define literal, grants a nice syntax to define value properties.
type ValueDefinition =
    { ValueDefinition : unit }
    
    /// Some magic syntax for composing value properties.
    static member (?) (_, propertyName) =
        fun (value : 'v) ->
            PropertyDefinition.makeValidated propertyName typeof<'v> (DefineExpr value)

/// In tandem with the variable literal, grants a nice syntax to define variable properties.
type VariableDefinition =
    { VariableDefinition : unit }

    /// Some magic syntax for composing variable properties.
    static member (?) (_, propertyName) =
        fun (variable : unit -> 'v) ->
            PropertyDefinition.makeValidated propertyName typeof<'v> (VariableExpr (fun () -> variable () :> obj))

/// In tandem with the property literal, grants a nice syntax to denote properties.
type PropertyDescriptor =
    { PropertyDescriptor : unit }
    
    /// Some magic syntax for composing value properties.
    static member inline (?) (_, propertyName : string) =
        propertyName

[<AutoOpen>]
module ReflectionModule =

    /// In tandem with the ValueDefinition type, grants a nice syntax to define value properties.
    let Define = { ValueDefinition = () }

    /// In tandem with the VariableDefinition type, grants a nice syntax to define variable properties.
    let Variable = { VariableDefinition = () }
    
    /// In tandem with the PropertyDescriptor type, grants a nice syntax to denote properties.
    let Property = { PropertyDescriptor = () }

[<RequireQualifiedAccess>]
module Reflection =

    let private PropertyDefinitionsCache =
        Dictionary<Type, PropertyDefinition list> () // wonder if HashIdentity.Reference would work?

    /// Derive a simulant name from an optional name.
    /// TODO: see if we can improve the name of this function.
    let deriveName nameOpt id =
        match nameOpt with
        | Some name -> name
        | None -> !!(scstring<Guid> id)

    /// Derive a simulant id and name from an optional name.
    let deriveIdAndName nameOpt =
        let id = makeGuid ()
        let name = deriveName nameOpt id
        (id, name)

    /// Is a property with the given name persistent?
    let isPropertyPersistentByName (propertyName : string) =
        not ^ propertyName.EndsWith ("Id", StringComparison.Ordinal) && // don't write an Id
        not ^ propertyName.EndsWith ("Ids", StringComparison.Ordinal) && // don't write multiple Ids
        not ^ propertyName.EndsWith ("Lc", StringComparison.Ordinal) && // don't write last-checked properties
        not ^ propertyName.EndsWith ("Np", StringComparison.Ordinal) // don't write non-persistent properties

    /// Is the property of the given target persistent?
    let isPropertyPersistent (property : PropertyInfo) (target : 'a) =
        isPropertyPersistentByName property.Name &&
        not
            (property.Name = Constants.Engine.NamePropertyName &&
             property.PropertyType = typeof<Name> &&
             fst ^ Guid.TryParse (property.GetValue target :?> Name |> Name.getNameStr))

    /// Check that the dispatcher has behavior congruent to the given type.
    let dispatchesAs (dispatcherTargetType : Type) (dispatcher : 'a) =
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

    /// Get the property definitions of a target type not considering inheritance.
    /// OPTIMIZATION: Memoized for efficiency since PropertyDefinitions properties will likely return
    /// a newly constructed list.
    let getPropertyDefinitionsNoInherit (targetType : Type) =
        match PropertyDefinitionsCache.TryGetValue targetType with
        | (true, propertyDefinitions) -> propertyDefinitions
        | (false, _) ->
            let propertyDefinitions =
                match targetType.GetProperty ("PropertyDefinitions", BindingFlags.Static ||| BindingFlags.Public) with
                | null -> []
                | propertyDefinitionsProperty ->
                    match propertyDefinitionsProperty.GetValue null with
                    | :? (obj list) as definitions when List.isEmpty definitions -> []
                    | :? (PropertyDefinition list) as definitions -> definitions
                    | _ -> failwith ^ "PropertyDefinitions property for type '" + targetType.Name + "' must be of type PropertyDefinition list."
            PropertyDefinitionsCache.Add (targetType, propertyDefinitions)
            propertyDefinitions

    /// Get the property definitions of a target type.
    let getPropertyDefinitions (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let propertyDefinitionLists = List.map (fun ty -> getPropertyDefinitionsNoInherit ty) targetTypes
        let propertyDefinitionLists = List.rev propertyDefinitionLists
        List.concat propertyDefinitionLists

    /// Get the names of the property definitions of a target type.
    let getPropertyDefinitionNames (targetType : Type) =
        let propertyDefinitions = getPropertyDefinitions targetType
        List.map (fun propertyDefinition -> propertyDefinition.PropertyName) propertyDefinitions

    /// Get a map of the counts of the property definitions names.
    let getPropertyDefinitionNameCounts propertyDefinitions =
        Map.fold
            (fun map (_ : string) definitions ->
                List.fold
                    (fun map definition ->
                        let definitionName = definition.PropertyName
                        match Map.tryFind definitionName map with
                        | Some count -> Map.add definitionName (count + 1) map
                        | None -> Map.add definitionName 1 map)
                    map
                    definitions)
            Map.empty
            propertyDefinitions

    /// Get all the reflective property containers of a target, including dispatcher and / or facets.
    let getReflectivePropertyContainers (target : 'a) =
        let targetType = target.GetType ()
        let dispatcherOpt =
            match targetType.GetProperty "DispatcherNp" with
            | null -> None
            | dispatcherNpProperty -> Some (dispatcherNpProperty.GetValue target)
        let facetsOpt =
            match targetType.GetProperty "FacetsNp" with
            | null -> None
            | facetsNpProperty ->
                let facets = facetsNpProperty.GetValue target :?> IEnumerable |> enumerable<obj> |> List.ofSeq
                Some facets
        match (dispatcherOpt, facetsOpt) with
        | (Some dispatcher, Some facets) -> dispatcher :: facets
        | (Some dispatcher, None) -> [dispatcher]
        | (None, Some facets) -> facets
        | (None, None) -> []

    /// Get all the reflective container types of a target, including dispatcher and / or facet types.
    let getReflectivePropertyContainerTypes (target : 'a) =
        let propertyContainers = getReflectivePropertyContainers target
        List.map getType propertyContainers

    /// Get all the reflective property definitions of a type, including those of its dispatcher and /
    /// or facets, organized in a map from the containing type's name to the property definition.
    let getReflectivePropertyDefinitionMap (target : 'a) =
        let containerTypes = getReflectivePropertyContainerTypes target
        Map.ofListBy (fun (ty : Type) -> (ty.Name, getPropertyDefinitions ty)) containerTypes

    /// Get all the unique reflective property definitions of a type, including those of its
    /// dispatcher and / or facets.
    let getReflectivePropertyDefinitions (target : 'a) =
        let types = getReflectivePropertyContainerTypes target
        let propertyDefinitionLists = List.map getPropertyDefinitions types
        let propertyDefinitions = List.concat propertyDefinitionLists
        let propertyDefinitions = Map.ofListBy (fun property -> (property.PropertyName, property)) propertyDefinitions
        Map.toValueList propertyDefinitions
        
    /// Try to read the target's member property from property descriptors.
    let private tryReadMemberProperty propertyDescriptors (property : PropertyInfo) target =
        match Map.tryFind property.Name propertyDescriptors with
        | Some (propertySymbol : Symbol) ->
            let converter = SymbolicConverter property.PropertyType
            if converter.CanConvertFrom typeof<Symbol> then
                let propertyValue = converter.ConvertFrom propertySymbol
                property.SetValue (target, propertyValue)
        | None -> ()
        
    /// Read one of a target's xtension properties from property descriptors.
    let private readXProperty xtension propertyDescriptors (target : 'a) propertyDefinition =
        let targetType = target.GetType ()
        if Seq.notExists
            (fun (property : PropertyInfo) -> property.Name = propertyDefinition.PropertyName)
            (targetType.GetProperties ()) then
            match Map.tryFind propertyDefinition.PropertyName propertyDescriptors with
            | Some (propertySymbol : Symbol) ->
                let converter = SymbolicConverter propertyDefinition.PropertyType
                if converter.CanConvertFrom typeof<Symbol> then
                    let xProperty = { PropertyValue = converter.ConvertFrom propertySymbol; PropertyType = propertyDefinition.PropertyType }
                    Xtension.attachProperty propertyDefinition.PropertyName xProperty xtension
                else
                    Log.debug ^ "Cannot convert property '" + scstring propertySymbol + "' to type '" + propertyDefinition.PropertyType.Name + "'."
                    xtension
            | None -> xtension
        else xtension
        
    /// Read a target's xtension properties from property descriptors.
    let private readXProperties xtension propertyDescriptors (target : 'a) =
        let propertyDefinitions = getReflectivePropertyDefinitions target
        List.fold (fun xtension -> readXProperty xtension propertyDescriptors target) xtension propertyDefinitions
        
    /// Read a target's Xtension from property descriptors.
    let private readXtension (copyTarget : 'a -> 'a) propertyDescriptors target =
        let target = copyTarget target
        let targetType = target.GetType ()
        match targetType.GetProperty "Xtension" with
        | null ->
            Log.debug "Target does not support xtensions due to missing Xtension property."
            target
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let xtension = readXProperties xtension propertyDescriptors target
                xtensionProperty.SetValue (target, xtension)
                target
            | _ ->
                Log.debug "Target does not support Xtensions due to Xtension property having unexpected type."
                target

    /// Try to read just the target's OverlayNameOpt from property descriptors.
    let tryReadOverlayNameOptToTarget (copyTarget : 'a -> 'a) propertyDescriptors target =
        let target = copyTarget target
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        let overlayNameOptPropertyOpt =
            Array.tryFind
                (fun (property : PropertyInfo) ->
                    property.Name = "OverlayNameOpt" &&
                    property.PropertyType = typeof<string option> &&
                    property.CanWrite)
                targetProperties
        match overlayNameOptPropertyOpt with
        | Some overlayNameOptProperty ->
            match Map.tryFind overlayNameOptProperty.Name propertyDescriptors with
            | Some overlayNameOptSymbol ->
                let overlayNameOpt = valueize<string option> overlayNameOptSymbol
                overlayNameOptProperty.SetValue (target, overlayNameOpt)
                target
            | None -> target
        | None -> target

    /// Read just the target's FacetNames from property descriptors.
    let readFacetNamesToTarget (copyTarget : 'a -> 'a) propertyDescriptors target =
        let target = copyTarget target
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        let facetNamesProperty =
            Array.find
                (fun (property : PropertyInfo) ->
                    property.Name = "FacetNames" &&
                    property.PropertyType = typeof<string Set> &&
                    property.CanWrite)
                targetProperties
        match Map.tryFind facetNamesProperty.Name propertyDescriptors with
        | Some facetNamesSymbol ->
            let facetNames = valueize<string Set> facetNamesSymbol
            facetNamesProperty.SetValue (target, facetNames)
            target
        | None -> target

    /// Read all of a target's member properties from property descriptors (except OverlayNameOpt and FacetNames).
    let readMemberPropertiesToTarget (copyTarget : 'a -> 'a) propertyDescriptors target =
        let target = copyTarget target
        let properties = (target.GetType ()).GetPropertiesWritable ()
        for property in properties do
            if  property.Name <> "FacetNames" &&
                property.Name <> "OverlayNameOpt" &&
                isPropertyPersistentByName property.Name then
                tryReadMemberProperty propertyDescriptors property target
        target

    /// Read all of a target's property values from property descriptors (except OverlayNameOpt and FacetNames).
    let readPropertiesToTarget (copyTarget : 'a -> 'a) propertyDescriptors target =
        let target = readMemberPropertiesToTarget copyTarget propertyDescriptors target
        readXtension copyTarget propertyDescriptors target
        
    /// Write an Xtension to property descriptors.
    let private writeXtension shouldWriteProperty propertyDescriptors xtension =
        Seq.fold (fun propertyDescriptors (xPropertyName, (xProperty : XProperty)) ->
            let xPropertyType = xProperty.PropertyType
            let xPropertyValue = xProperty.PropertyValue
            if  isPropertyPersistentByName xPropertyName &&
                shouldWriteProperty xPropertyName xPropertyType xPropertyValue then
                let xPropertySymbol = (SymbolicConverter xPropertyType).ConvertTo (xPropertyValue, typeof<Symbol>) :?> Symbol
                Map.add xPropertyName xPropertySymbol propertyDescriptors
            else propertyDescriptors)
            propertyDescriptors
            (Xtension.toSeq xtension)
            
    /// Write a member property value to a property descriptors.
    let private writeMemberProperty (propertyValue : obj) (property : PropertyInfo) shouldWriteProperty propertyDescriptors (target : 'a) =
        if  isPropertyPersistent property target &&
            shouldWriteProperty property.Name property.PropertyType propertyValue then
            let valueSymbol = (SymbolicConverter property.PropertyType).ConvertTo (propertyValue, typeof<Symbol>) :?> Symbol
            Map.add property.Name valueSymbol propertyDescriptors
        else propertyDescriptors

    /// Write all of a target's property values to property descriptors.
    let writePropertiesFromTarget shouldWriteProperty propertyDescriptors (target : 'a) =
        let targetType = target.GetType ()
        let properties = targetType.GetProperties ()
        Seq.fold (fun propertyDescriptors (property : PropertyInfo) ->
            match property.GetValue target with
            | :? Xtension as xtension -> writeXtension shouldWriteProperty propertyDescriptors xtension
            | propertyValue -> writeMemberProperty propertyValue property shouldWriteProperty propertyDescriptors target)
            propertyDescriptors
            properties

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

    /// Attach properties from the given definitions to a target.
    let attachPropertiesViaDefinitions (copyTarget : 'a -> 'a) propertyDefinitions target =
        let target = copyTarget target
        let targetType = target.GetType ()
        for propertyDefinition in propertyDefinitions do
            let propertyValue = PropertyExpr.eval propertyDefinition.PropertyExpr
            match targetType.GetPropertyWritable propertyDefinition.PropertyName with
            | null ->
                match targetType.GetPropertyWritable "Xtension" with
                | null -> failwith ^ "Invalid property '" + propertyDefinition.PropertyName + "' for target type '" + targetType.Name + "'."
                | xtensionProperty ->
                    match xtensionProperty.GetValue target with
                    | :? Xtension as xtension ->
                        let xProperty = { PropertyValue = propertyValue; PropertyType = propertyDefinition.PropertyType }
                        let xtension = Xtension.attachProperty propertyDefinition.PropertyName xProperty xtension
                        xtensionProperty.SetValue (target, xtension)
                    | _ -> failwith ^ "Invalid property '" + propertyDefinition.PropertyName + "' for target type '" + targetType.Name + "'."
            | property -> property.SetValue (target, propertyValue)
        target

    /// Detach properties from a target.
    let detachPropertiesViaNames (copyTarget : 'a -> 'a) propertyNames target =
        let target = copyTarget target
        let targetType = target.GetType ()
        for propertyName in propertyNames do
            match targetType.GetPropertyWritable propertyName with
            | null ->
                match targetType.GetPropertyWritable "Xtension" with
                | null -> failwith ^ "Invalid property '" + propertyName + "' for target type '" + targetType.Name + "'."
                | xtensionProperty ->
                    match xtensionProperty.GetValue target with
                    | :? Xtension as xtension ->
                        let xtension = Xtension.detachProperty propertyName xtension
                        xtensionProperty.SetValue (target, xtension)
                    | _ -> failwith ^ "Invalid property '" + propertyName + "' for target type '" + targetType.Name + "'."
            | _ -> ()
        target

    /// Attach source's properties to a target.
    let attachProperties (copyTarget : 'a -> 'a) (source : 'b) target =
        let sourceType = source.GetType ()
        let propertyDefinitions = getPropertyDefinitions sourceType
        attachPropertiesViaDefinitions copyTarget propertyDefinitions target

    /// Detach source's properties to a target.
    let detachProperties (copyTarget : 'a -> 'a) (source : 'b) target =
        let sourceType = source.GetType ()
        let propertyNames = getPropertyDefinitionNames sourceType
        detachPropertiesViaNames copyTarget propertyNames target

    /// Check for facet compatibility with the target's dispatcher.
    let isFacetTypeCompatibleWithDispatcher dispatcherMap (facetType : Type) (target : 'a) =
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
    let isFacetCompatibleWithDispatcher dispatcherMap (facet : 'b) (target : 'a) =
        let facetType = facet.GetType ()
        let facetTypes = facetType :: getBaseTypesExceptObject facetType
        List.forall
            (fun facetType -> isFacetTypeCompatibleWithDispatcher dispatcherMap facetType target)
            facetTypes

    /// Attach intrinsic facets to a target by their names.
    let attachIntrinsicFacetsViaNames (copyTarget : 'a -> 'a) dispatcherMap facetMap facetNames target =
        let target = copyTarget target
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
            List.fold (fun target facet -> attachProperties copyTarget facet target) target facets

    /// Attach source's intrinsic facets to a target.
    let attachIntrinsicFacets (copyTarget : 'a -> 'a) dispatcherMap facetMap (source : 'b) target =
        let sourceType = source.GetType ()
        let instrinsicFacetNames = getIntrinsicFacetNames sourceType
        attachIntrinsicFacetsViaNames copyTarget dispatcherMap facetMap instrinsicFacetNames target

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
                    let propertyDefinitions = getPropertyDefinitionsNoInherit sourceType
                    let requiresFacetNames = requiresFacetNames sourceType
                    (sourceType.Name, includeNames, propertyDefinitions, requiresFacetNames))
                sourceTypes

        // create the intrinsic overlays with the above descriptors
        let overlays =
            List.map
                (fun (overlayName, includeNames, propertyDefinitions, requiresFacetNames) ->
                    let overlayProperties =
                        List.foldBack
                            (fun propertyDefinition overlayProperties ->
                                match propertyDefinition.PropertyExpr with
                                | DefineExpr value ->
                                    let converter = SymbolicConverter propertyDefinition.PropertyType
                                    let overlayProperty = converter.ConvertTo (value, typeof<Symbol>) :?> Symbol
                                    Map.add propertyDefinition.PropertyName overlayProperty overlayProperties
                                | VariableExpr _ -> overlayProperties)
                            propertyDefinitions
                            Map.empty
                    let overlayProperties =
                        if requiresFacetNames
                        then Map.add "FacetNames" (Symbols ([], None)) overlayProperties
                        else overlayProperties
                    { OverlayName = overlayName
                      OverlayIncludeNames = includeNames
                      OverlayProperties = overlayProperties })
                overlayDescriptors

        // fin
        overlays