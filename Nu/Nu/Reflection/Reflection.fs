// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Reflection
open Prime

[<RequireQualifiedAccess>]
module Reflection =

    let mutable private Initialized = false

    /// A dictionary of all loaded assemblies.
    let private AssembliesLoaded =
        Dictionary<string, Assembly> StringComparer.Ordinal

    let private PropertyDefinitionsCache =
        Dictionary<Type, PropertyDefinition list> HashIdentity.Structural

    /// A dictionary of properties that are never serialized.
    let private NonPersistentPropertyNames =
        dictPlus
            StringComparer.Ordinal
            [// simulant properties
             ("Dispatcher", true)
             ("Content", true)
             ("Protected", true)
             ("Order", true)
             ("Id", true)

             // game properties
             ("Eye3dFrustumInterior", true)
             ("Eye3dFrustumExterior", true)
             ("Eye3dFrustumImposter", true)

             // screen properties
             ("TransitionState", true)

             // entity properties
             ("Facets", true)
             ("Surnames", true)
             ("PerimeterCenter", true)
             ("PerimeterBottom", true)
             ("PerimeterBottomLeft", true)
             ("PerimeterMin", true)
             ("PerimeterMax", true)
             ("PerimeterCenterLocal", true)
             ("PerimeterBottomLocal", true)
             ("PerimeterBottomLeftLocal", true)
             ("PerimeterMinLocal", true)
             ("PerimeterMaxLocal", true)
             ("RotationMatrix", true)
             ("Angles", true)
             ("AnglesLocal", true)
             ("Degrees", true)
             ("DegreesLocal", true)
             ("AffineMatrix", true)
             ("PerimeterUnscaled", true)
             ("Perimeter", true)
             ("Bounds", true)
             ("Imperative", true)
             ("PublishChangeEvents", true)
             ("PublishPreUpdates", true)
             ("PublishUpdates", true)
             ("PublishPostUpdates", true)
             ("Mounted", true)
             ("Is2d", true)
             ("Is3d", true)
             ("Physical", true)
             ("Optimized", true)]

    /// Configure a property to be non-persistent.
    let internal initPropertyNonPersistent nonPersistent propertyName =
        NonPersistentPropertyNames.[propertyName] <- nonPersistent

    /// Is a property with the given name not persistent?
    let isPropertyNonPersistentByName (propertyName : string) =
        match NonPersistentPropertyNames.TryGetValue propertyName with
        | (true, result) -> result
        | (false, _) ->
            let result = false
            NonPersistentPropertyNames.[propertyName] <- result
            result

    /// Is the property of the given target not persistent?
    let isPropertyNonPersistent (property : PropertyInfo) (target : 'a) =
        isPropertyNonPersistentByName property.Name ||
        (property.Name = Constants.Engine.NamePropertyName &&
         property.PropertyType = typeof<string> &&
         Gen.isNameGenerated (property.GetValue target :?> string))

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
    /// OPTIMIZATION: Memoized for efficiency since Properties will likely return a newly constructed list.
    let getPropertyDefinitionsNoInherit (targetType : Type) =
        match PropertyDefinitionsCache.TryGetValue targetType with
        | (true, definitions) -> definitions
        | (false, _) ->
            let definitionsPropertyOpt =
                match targetType.GetProperty (Constants.Engine.PropertiesPropertyName, BindingFlags.Static ||| BindingFlags.Public) with
                | null -> None
                | definitionsProperty -> Some definitionsProperty
            let definitions =
                match definitionsPropertyOpt with
                | Some definitionsProperty ->
                    match definitionsProperty.GetValue null with
                    | :? (obj list) as definitions when List.isEmpty definitions -> []
                    | :? (PropertyDefinition list) as definitions -> definitions
                    | _ -> failwith ("Properties property for type '" + targetType.Name + "' must be of type PropertyDefinition list.")
                | None -> []
            PropertyDefinitionsCache.Add (targetType, definitions)
            definitions

    /// Get the property definitions of a target type.
    let getPropertyDefinitions (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let definitionLists = List.map (fun ty -> getPropertyDefinitionsNoInherit ty) targetTypes
        let definitionLists = List.rev definitionLists
        List.concat definitionLists

    /// Get the names of the property definitions of a target type.
    let getPropertyNames (targetType : Type) =
        let definitions = getPropertyDefinitions targetType
        List.map (fun (definition : PropertyDefinition) -> definition.PropertyName) definitions

    /// Get a map of the counts of the property definitions names.
    let getPropertyNameCounts definitions =
        Map.fold (fun map (_ : string) definitions ->
            List.fold (fun map (definition : PropertyDefinition) ->
                let definitionName = definition.PropertyName
                match Map.tryFind definitionName map with
                | Some count -> Map.add definitionName (count + 1) map
                | None -> Map.add definitionName 1 map)
                map definitions)
            Map.empty definitions

    /// Get all the reflective property containers of a target, including dispatcher and / or facets.
    let getReflectivePropertyContainers (target : 'a) =
        let targetType = target.GetType ()
        let dispatcherOpt =
            match targetType.GetProperty Constants.Engine.DispatcherPropertyName with
            | null -> None
            | dispatcherProperty -> Some (dispatcherProperty.GetValue target)
        let facetsOpt =
            match targetType.GetProperty Constants.Engine.FacetsPropertyName with
            | null -> None
            | facetsProperty ->
                let facets = facetsProperty.GetValue target :?> IEnumerable |> enumerable<obj> |> List.ofSeq
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
        target |>
        getReflectivePropertyContainerTypes |>
        List.map getPropertyDefinitions |>
        List.concat |>
        Map.ofListBy (fun definition -> (definition.PropertyName, definition))

    /// A hack to retreive a simplified generic type name
    let getSimplifiedTypeNameHack (ty : Type) =
        let typeName = ty.Name
        let genericTypes = ty.GetGenericArguments ()
        let genericTypeNameStrs = Array.map (fun (ty : Type) -> ty.Name) genericTypes
        let genericTypeNamesStr = "<" + String.concat ", " genericTypeNameStrs + ">"
        typeName.Replace ("`" + scstring (Array.length genericTypeNameStrs), genericTypeNamesStr)

    /// Try to read the target's member property from property descriptors.
    let private tryReadMemberProperty propertyDescriptors (property : PropertyInfo) target =
        if property.PropertyType = typeof<DesignerProperty> then
            match Map.tryFind property.Name propertyDescriptors with
            | Some propertySymbol ->
                match propertySymbol with
                | Symbols ([Text (str, _); _], _) when notNull (Type.GetType str) ->
                    let converter = SymbolicConverter (false, None, property.PropertyType)
                    if converter.CanConvertFrom typeof<Symbol> then
                        let propertyValue = converter.ConvertFrom propertySymbol
                        property.SetValue (target, propertyValue)
                    else Log.debug ("Cannot convert property '" + scstring propertySymbol + "' to type '" + property.PropertyType.Name + "'.")
                | _ -> ()
            | None -> ()
        elif property.Name = Constants.Engine.TransformPropertyName &&
             property.PropertyType = typeof<Transform> then
             () // nothing to do here since the custom .NET properties will take care of this...
        else
            match Map.tryFind property.Name propertyDescriptors with
            | Some (propertySymbol : Symbol) ->
                let converter = SymbolicConverter (false, None, property.PropertyType)
                if converter.CanConvertFrom typeof<Symbol> then
                    let propertyValue = converter.ConvertFrom propertySymbol
                    property.SetValue (target, propertyValue)
            | None -> ()

    /// Read one of a target's xtension properties.
    let private readXtensionProperty
        (xtension : Xtension)
        (propertyDefinitions : Map<string, PropertyDefinition>)
        (target : 'a)
        (propertyName : string)
        (propertySymbol : Symbol) =
        let targetType = target.GetType ()
        if Array.notExists (fun (property : PropertyInfo) -> property.Name = propertyName) (targetType.GetProperties ()) then
            match Map.tryFind propertyName propertyDefinitions with
            | Some propertyDefinition ->
                let converter = SymbolicConverter (false, None, propertyDefinition.PropertyType)
                if converter.CanConvertFrom typeof<Symbol> then
                    let property = { PropertyType = propertyDefinition.PropertyType; PropertyValue = converter.ConvertFrom propertySymbol }
                    Xtension.attachProperty propertyName property xtension
                else Log.debug ("Cannot convert property '" + scstring propertySymbol + "' to type '" + propertyDefinition.PropertyType.Name + "'."); xtension
            | None ->
                match propertySymbol with
                | Symbols ([Text (str, _); _], _) when notNull (Type.GetType str) ->
                    let propertyType = typeof<DesignerProperty>
                    let converter = SymbolicConverter (false, None, propertyType)
                    if converter.CanConvertFrom typeof<Symbol> then
                        let property = { PropertyType = propertyType; PropertyValue = converter.ConvertFrom propertySymbol }
                        Xtension.attachProperty propertyName property xtension
                    else Log.debug ("Cannot convert property '" + scstring propertySymbol + "' to type '" + propertyType.Name + "'."); xtension
                | _ -> xtension
        else xtension

    /// Read a target's xtension properties from property descriptors.
    let private readXtensionProperties xtension (propertyDescriptors : Map<string, Symbol>) (target : 'a) =
        let definitions = getReflectivePropertyDefinitions target
        Map.fold
            (fun xtension -> readXtensionProperty xtension definitions target)
            xtension
            propertyDescriptors

    /// Read a target's Xtension from property descriptors.
    let private readXtension (copyTarget : 'a -> 'a) propertyDescriptors target =
        let target = copyTarget target
        let targetType = target.GetType ()
        match targetType.GetProperty Constants.Engine.XtensionPropertyName with
        | null ->
            Log.debug "Target does not support Xtensions due to missing Xtension property."
            target
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let xtension = readXtensionProperties xtension propertyDescriptors target
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
            Array.tryFind (fun (property : PropertyInfo) ->
                property.Name = Constants.Engine.OverlayNameOptPropertyName &&
                property.PropertyType = typeof<string option> &&
                property.CanWrite)
                targetProperties
        match overlayNameOptPropertyOpt with
        | Some overlayNameOptProperty ->
            match Map.tryFind overlayNameOptProperty.Name propertyDescriptors with
            | Some overlayNameOptSymbol ->
                let overlayNameOpt = symbolToValue<string option> overlayNameOptSymbol
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
            Array.find (fun (property : PropertyInfo) ->
                property.Name = Constants.Engine.FacetNamesPropertyName &&
                property.PropertyType = typeof<string Set> &&
                property.CanWrite)
                targetProperties
        match Map.tryFind facetNamesProperty.Name propertyDescriptors with
        | Some facetNamesSymbol ->
            let facetNames = symbolToValue<string Set> facetNamesSymbol
            facetNamesProperty.SetValue (target, facetNames)
            target
        | None -> target

    /// Read all of a target's member properties from property descriptors (except OverlayNameOpt and FacetNames).
    let readMemberPropertiesToTarget (copyTarget : 'a -> 'a) propertyDescriptors target =
        let target = copyTarget target
        let properties = (target.GetType ()).GetPropertiesWritable ()
        for property in properties do
            if  property.Name <> Constants.Engine.FacetNamesPropertyName &&
                property.Name <> Constants.Engine.OverlayNameOptPropertyName &&
                not (isPropertyNonPersistentByName property.Name) then
                tryReadMemberProperty propertyDescriptors property target
        target

    /// Read all of a target's property values from property descriptors (except OverlayNameOpt and FacetNames).
    let readPropertiesToTarget (copyTarget : 'a -> 'a) propertyDescriptors target =
        let target = readMemberPropertiesToTarget copyTarget propertyDescriptors target
        readXtension copyTarget propertyDescriptors target

    /// Write an Xtension to property descriptors.
    let private writeXtension shouldWriteProperty propertyDescriptors xtension =
        Seq.fold (fun propertyDescriptors (propertyName, (property : Property)) ->
            let propertyType = property.PropertyType
            let propertyValue = property.PropertyValue
            if  propertyType <> typeof<ComputedProperty> &&
                not (isPropertyNonPersistentByName propertyName) &&
                shouldWriteProperty propertyName propertyType propertyValue then
                let converter = SymbolicConverter (false, None, propertyType)
                let propertySymbol = converter.ConvertTo (propertyValue, typeof<Symbol>) :?> Symbol
                Map.add propertyName propertySymbol propertyDescriptors
            else propertyDescriptors)
            propertyDescriptors
            (Xtension.toSeq xtension)

    /// Write a member property value to a property descriptors.
    let private writeMemberProperty (propertyValue : obj) (property : PropertyInfo) shouldWriteProperty propertyDescriptors (target : 'a) =
        if  not (isPropertyNonPersistent property target) &&
            shouldWriteProperty property.Name property.PropertyType propertyValue then // HACK: never write degrees as those are only intended for the editor.
            if  property.Name = Constants.Engine.TransformPropertyName &&
                property.PropertyType = typeof<Transform> then
                propertyDescriptors // nothing to do here since the custom .NET properties will take care of this...
            else
                let converter = SymbolicConverter (false, None, property.PropertyType)
                let valueSymbol = converter.ConvertTo (propertyValue, typeof<Symbol>) :?> Symbol
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

    /// Get the intrinsic facet of a target type not considering inheritance.
    let getIntrinsicFacetsNoInherit (targetType : Type) =
        let intrinsicFacetsPropertyOpt =
            match targetType.GetProperty (Constants.Engine.FacetsPropertyName, BindingFlags.Static ||| BindingFlags.Public) with
            | null -> None
            | intrinsicFacetsProperty -> Some intrinsicFacetsProperty
        match intrinsicFacetsPropertyOpt with
        | Some intrinsicFacetsProperty ->
            let intrinsicFacets = intrinsicFacetsProperty.GetValue null
            match intrinsicFacets with
            | :? (obj list) as intrinsicFacets when List.isEmpty intrinsicFacets -> []
            | :? (Type list) as intrinsicFacets -> intrinsicFacets
            | _ -> failwith ("Facets property for type '" + targetType.Name + "' must be of type 'Type list'.")
        | None -> []

    /// Get the intrinsic facet names of a target type not considering inheritance.
    let getIntrinsicFacetNamesNoInherit targetType =
        targetType |>
        getIntrinsicFacetsNoInherit |>
        List.map (fun (ty : Type) -> ty.Name)

    /// Get the intrinsic facet names of a target type.
    let getIntrinsicFacetNames (targetType : Type) =
        let targetTypes = targetType :: getBaseTypesExceptObject targetType
        let intrinsicFacetNamesLists = List.map (fun ty -> getIntrinsicFacetNamesNoInherit ty) targetTypes
        let intrinsicFacetNamesLists = List.rev intrinsicFacetNamesLists
        List.concat intrinsicFacetNamesLists

    /// Attach properties from the given definitions to a target.
    let attachPropertiesViaDefinitions (copyTarget : 'a -> 'a) definitions target world =
        let target = copyTarget target
        let targetType = target.GetType ()
        match targetType.GetPropertyWritable Constants.Engine.XtensionPropertyName with
        | null -> failwith "Target does not support Xtensions due to missing Xtension property."
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let mutable xtension = xtension
                for definition in definitions do
                    let propertyValue = PropertyExpr.eval definition.PropertyExpr world
                    match targetType.GetPropertyWritable definition.PropertyName with
                    | null ->
                        let property = { PropertyType = definition.PropertyType; PropertyValue = propertyValue }
                        xtension <- Xtension.attachProperty definition.PropertyName property xtension
                    | propertyInfo -> propertyInfo.SetValue (target, propertyValue)
                xtensionProperty.SetValue (target, xtension)
            | _ -> failwith "Target does not support Xtensions due to missing Xtension property."
        target

    /// Detach properties from a target.
    let detachPropertiesViaNames (copyTarget : 'a -> 'a) propertyNames target =
        let target = copyTarget target
        let targetType = target.GetType ()
        match targetType.GetPropertyWritable Constants.Engine.XtensionPropertyName with
        | null -> failwith "Target does not support Xtensions due to missing Xtension property."
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let mutable xtension = xtension
                for propertyName in propertyNames do
                    match targetType.GetPropertyWritable propertyName with
                    | null ->
                        xtension <- Xtension.detachProperty propertyName xtension
                        xtensionProperty.SetValue (target, xtension)
                    | _ -> failwith ("Invalid property '" + propertyName + "' for target type '" + targetType.Name + "'.")
            | _ -> ()
        target

    /// Attach source's properties to a target.
    let attachProperties (copyTarget : 'a -> 'a) (source : 'b) target world =
        let sourceType = source.GetType ()
        let definitions = getPropertyDefinitions sourceType
        attachPropertiesViaDefinitions copyTarget definitions target world

    /// Detach source's properties to a target.
    let detachProperties (copyTarget : 'a -> 'a) (source : 'b) target =
        let sourceType = source.GetType ()
        let propertyNames = getPropertyNames sourceType
        detachPropertiesViaNames copyTarget propertyNames target

    /// Check for facet compatibility with the target's dispatcher.
    let isFacetTypeCompatibleWithDispatcher dispatcherMap (facetType : Type) (target : 'a) =
        let targetType = target.GetType ()
        match facetType.GetProperty (Constants.Engine.RequiredDispatcherPropertyName, BindingFlags.Static ||| BindingFlags.Public) with
        | null -> true
        | reqdDispatcherNameProperty ->
            match reqdDispatcherNameProperty.GetValue null with
            | :? string as reqdDispatcherName ->
                match Map.tryFind reqdDispatcherName dispatcherMap with
                | Some reqdDispatcher ->
                    let reqdDispatcherType = reqdDispatcher.GetType ()
                    match targetType.GetProperty Constants.Engine.DispatcherPropertyName with
                    | null -> failwith ("Target '" + scstring target + "' does not implement dispatching in a compatible way.")
                    | dispatcherProperty ->
                        let dispatcher = dispatcherProperty.GetValue target
                        dispatchesAs reqdDispatcherType dispatcher
                | None -> failwith ("Could not find required dispatcher '" + reqdDispatcherName + "' in dispatcher map.")
            | _ -> failwith ("Static member 'RequiredDispatcherName' for facet '" + facetType.Name + "' is not of type string.")

    /// Check for facet compatibility with the target's dispatcher.
    let isFacetCompatibleWithDispatcher dispatcherMap (facet : 'b) (target : 'a) =
        let facetType = facet.GetType ()
        let facetTypes = facetType :: getBaseTypesExceptObject facetType
        List.forall
            (fun facetType -> isFacetTypeCompatibleWithDispatcher dispatcherMap facetType target)
            facetTypes

    /// Attach intrinsic facets to a target by their names.
    let attachIntrinsicFacetsViaNames (copyTarget : 'a -> 'a) dispatcherMap facetMap facetNames target world =
        let target = copyTarget target
        let facets =
            List.map (fun facetName ->
                match Map.tryFind facetName facetMap with
                | Some facet -> facet
                | None -> failwith ("Could not find facet '" + facetName + "' in facet map."))
                facetNames |>
            List.toArray
        let targetType = target.GetType ()
        match targetType.GetPropertyWritable Constants.Engine.FacetsPropertyName with
        | null -> failwith ("Could not attach facet to type '" + targetType.Name + "'.")
        | facetsProperty ->
            Array.iter (fun facet ->
                if not (isFacetCompatibleWithDispatcher dispatcherMap facet target)
                then failwith ("Facet of type '" + getTypeName facet + "' is not compatible with target '" + scstring target + "'.")
                else ())
                facets
            facetsProperty.SetValue (target, facets)
            Array.fold (fun target facet -> attachProperties copyTarget facet target world) target facets

    /// Attach source's intrinsic facets to a target.
    let attachIntrinsicFacets (copyTarget : 'a -> 'a) dispatcherMap facetMap (source : 'b) target world =
        let sourceType = source.GetType ()
        let instrinsicFacetNames = getIntrinsicFacetNames sourceType
        attachIntrinsicFacetsViaNames copyTarget dispatcherMap facetMap instrinsicFacetNames target world

    /// Initialize backing data utilized by reflection module.
    let init () =

        // initialize once
        if not Initialized then

            // process loading assemblies
            AppDomain.CurrentDomain.AssemblyLoad.Add (fun args ->
                AssembliesLoaded.[args.LoadedAssembly.FullName] <- args.LoadedAssembly)
            AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler (fun _ args ->
                snd (AssembliesLoaded.TryGetValue args.Name)))

            // process existing assemblies
            for assembly in AppDomain.CurrentDomain.GetAssemblies () do
                AssembliesLoaded.[assembly.FullName] <- assembly

namespace Prime
open Nu
    
/// In tandem with the define literal, grants a nice syntax to define value properties.
type [<NoEquality; NoComparison>] ValueDescription =
    { NonPersistentDescription : unit }
        
    /// Some magic syntax for composing value properties.
    static member (?) (_ : 'a, propertyName) =
        fun (value : 'a) ->
            Reflection.initPropertyNonPersistent true propertyName
            Define? propertyName value

[<AutoOpen>]
module ReflectionSyntax =

    /// In tandem with the ValueDescription type, grants a nice syntax to define value properties.
    let NonPersistent = { NonPersistentDescription = () }