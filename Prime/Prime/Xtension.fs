// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open Prime

/// An attribute to specify the default value of an XProperty.
type [<AttributeUsage (AttributeTargets.Class); AllowNullLiteral>] XDefaultValueAttribute (defaultValue : obj) =
    inherit Attribute ()
    member this.DefaultValue = defaultValue
    
/// Describes an XProperty.
type [<StructuralEquality; NoComparison>] XPropertyDescriptor =
    { PropertyName : string
      PropertyType : Type }

/// An Xtension property.
/// TODO: P1: make this a struct record in next version of F#.
type [<StructuralEquality; NoComparison>] XProperty =
    { mutable PropertyType : Type
      mutable PropertyValue : obj }

/// A map of XProperties.
/// NOTE: Xtension uses UMap because it's slightly faster when used in the Nu game engine, but
/// it's not necessarily the right decision in other contexts. However, I'm sticking with this
/// choice since the performance of Nu trumps other usages for now.
type XProperties = UMap<string, XProperty>

[<AutoOpen>]
module XtensionModule =

    /// Xtensions (and their supporting types) are a dynamic, functional, and convenient way
    /// to implement dynamic properties.
    type [<NoEquality; NoComparison>] Xtension =
        private
            { Properties : XProperties
              CanDefault : bool
              Sealed : bool
              Imperative : bool }

        /// Get the default value of an instance of type 'a taking into account XDefaultValue decorations.
        static member private getDefaultValue () : 'a =
            let defaultPropertyType = typeof<'a>
            let defaultValueAttributeOpt =
                defaultPropertyType.GetCustomAttributes (typeof<XDefaultValueAttribute>, true) |>
                Array.map (fun attr -> attr :?> XDefaultValueAttribute) |>
                Array.tryHead
            match defaultValueAttributeOpt with
            | Some defaultValueAttribute ->
                match defaultValueAttribute.DefaultValue with
                | :? 'a as defaultValue -> defaultValue
                | _ as defaultValue ->
                    let defaultValueType = defaultValue.GetType ()
                    let converter = SymbolicConverter (false, defaultValueType)
                    if converter.CanConvertFrom defaultPropertyType
                    then converter.ConvertFrom defaultValue :?> 'a
                    else failwith ^ "Cannot convert '" + scstring defaultValue + "' to type '" + defaultPropertyType.Name + "'."
            | None -> Unchecked.defaultof<'a>

        /// Try to get the default value for a given xtension member, returning None when defaulting is disallowed.
        static member private tryGetDefaultValue (this : Xtension) propertyName : 'a =
            if this.CanDefault then Xtension.getDefaultValue ()
            else failwith ^ "Xtension property '" + propertyName + "' does not exist and no default is permitted because CanDefault is false."

        /// The dynamic look-up operator for an Xtension.
        /// Example:
        ///     let parallax = xtn?Parallax : single
        static member (?) (xtension, propertyName) : 'a =

            // check if dynamic member is an existing property
            let propertyOpt = UMap.tryFindFast propertyName xtension.Properties
            if FOption.isSome propertyOpt then
                
                // return property directly if the return type matches, otherwise the default value for that type
                let property = FOption.get propertyOpt
                match property.PropertyValue with
                | :? 'a as propertyValue -> propertyValue
                | _ -> failwith ^ "Xtension property '" + propertyName + "' of type '" + property.PropertyType.Name + "' is not of the expected type '" + typeof<'a>.Name + "'."

            else

                // presume we're looking for a property that doesn't exist, so try to get the default value
                Xtension.tryGetDefaultValue xtension propertyName

        /// The dynamic assignment operator for an Xtension.
        /// Example:
        ///     let xtn = xtn.Position <- Vector2 (4.0, 5.0).
        static member (?<-) (xtension, propertyName, value : 'a) =
            let propertyOpt = UMap.tryFindFast propertyName xtension.Properties
            if FOption.isSome propertyOpt then
                let property = FOption.get propertyOpt
                if xtension.Sealed && property.PropertyType <> typeof<'a> then failwith "Cannot change the type of a sealed Xtension's property."
                if xtension.Imperative then property.PropertyValue <- value :> obj; xtension
                else
                    let property = { property with PropertyValue = value :> obj }
                    let properties = UMap.add propertyName property xtension.Properties
                    { xtension with Properties = properties }
            else
                if xtension.Sealed then failwith "Cannot add property to a sealed Xtension."
                let property = { PropertyType = typeof<'a>; PropertyValue = value :> obj }
                let properties = UMap.add propertyName property xtension.Properties
                { xtension with Properties = properties }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Xtension =
    
        /// Make an extension.
        let make properties canDefault isSealed imperative =
            { Properties = properties
              CanDefault = canDefault
              Sealed = isSealed
              Imperative = imperative }
    
        /// An Xtension that cannot default, is sealed, and is imperative.
        let makeImperative () = make (UMap.makeEmpty None) false true true
    
        /// An Xtension that can default, isn't sealed, and isn't imperative.
        let empty = make (UMap.makeEmpty None) true false false
    
        /// An Xtension that cannot default, is sealed, and isn't imperative.
        let safe = make (UMap.makeEmpty None) false true false
    
        /// An Xtension that cannot default, isn't sealed, and isn't imperative.
        let mixed = make (UMap.makeEmpty None) false false false

        /// Whether the extension uses mutation.
        let getImperative xtension = xtension.Imperative

        /// Whether the extension uses mutation.
        let setImperative imperative xtension = { xtension with Imperative = imperative }
    
        /// Try to get a property from an xtension.
        let tryGetProperty name xtension = UMap.tryFind name xtension.Properties
    
        /// Get a property from an xtension.
        let getProperty name xtension = UMap.find name xtension.Properties
    
        /// Set a property on an Xtension.
        let trySetProperty name property xtension =
            match UMap.tryFind name xtension.Properties with
            | Some property' ->
                if xtension.Imperative then
                    property'.PropertyType <- property.PropertyType
                    property'.PropertyValue <- property.PropertyValue
                    (true, xtension)
                else (true, { xtension with Properties = UMap.add name property xtension.Properties })
            | None ->
                if not xtension.Sealed
                then (true, { xtension with Properties = UMap.add name property xtension.Properties })
                else (false, xtension)
    
        /// Set a property on an Xtension.
        let setProperty name property xtension =
            match trySetProperty name property xtension with
            | (true, xtension) -> xtension
            | (false, _) -> failwith "Cannot add property to a sealed Xtension."

        /// Attach a property to an Xtension.
        let attachProperty name property xtension = { xtension with Properties = UMap.add name property xtension.Properties }
    
        /// Attach multiple properties to an Xtension.
        let attachProperties namesAndProperties xtension = { xtension with Properties = UMap.addMany namesAndProperties xtension.Properties }
    
        /// Detach a property from an Xtension.
        let detachProperty name xtension = { xtension with Properties = UMap.remove name xtension.Properties }
    
        /// Detach multiple properties from an Xtension.
        let detachProperties names xtension = { xtension with Properties = UMap.removeMany names xtension.Properties }
    
        /// Convert an xtension to a sequence of its entries.
        let toSeq xtension = xtension.Properties :> _ seq

        /// Convert an xtension to a sequence of its entries.
        let ofSeq seq = attachProperties seq empty

/// Xtensions (and their supporting types) are a dynamic, functional, and convenient way
/// to implement dynamic properties.
type Xtension = XtensionModule.Xtension