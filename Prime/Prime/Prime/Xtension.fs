// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Reflection
open System.ComponentModel
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
type [<StructuralEquality; NoComparison>] XProperty =
    { PropertyValue : obj
      PropertyType : Type }

/// A map of XProperties.
type XProperties = Vmap<string, XProperty>

[<AutoOpen>]
module XtensionModule =

    /// Xtensions (and their supporting types) are a dynamic, functional, and semi-convenient way
    /// to implement dynamic properties.
    type [<NoEquality; NoComparison>] Xtension =
        private
            { Properties : XProperties
              CanDefault : bool
              Sealed : bool }

        /// Get the default value of an instance of type 'r taking into account XDefaultValue decorations.
        static member private getDefaultValue () : 'r =
            let defaultPropertyType = typeof<'r>
            let optDefaultValueAttribute =
                defaultPropertyType.GetCustomAttributes (typeof<XDefaultValueAttribute>, true) |>
                Seq.map (fun attr -> attr :?> XDefaultValueAttribute) |>
                Seq.tryHead
            match optDefaultValueAttribute with
            | Some defaultValueAttribute ->
                match defaultValueAttribute.DefaultValue with
                | :? 'r as defaultValue -> defaultValue
                | _ as defaultValue ->
                    let defaultValueType = defaultValue.GetType ()
                    let converter = SymbolicConverter defaultValueType
                    if converter.CanConvertFrom defaultPropertyType
                    then converter.ConvertFrom defaultValue :?> 'r
                    else failwith ^ "Cannot convert '" + scstring defaultValue + "' to type '" + defaultPropertyType.Name + "'."
            | None -> Unchecked.defaultof<'r>

        /// Try to get the default value for a given xtension member, returning None when defaulting is disallowed.
        static member private tryGetDefaultValue (this : Xtension) propertyName : 'r =
            if this.CanDefault then Xtension.getDefaultValue ()
            else failwith ^ "Xtension property '" + propertyName + "' does not exist and no default is permitted because CanDefault is false."

        /// The dynamic look-up operator for an Xtension.
        /// Example:
        ///     let parallax = xtn?Parallax : single
        static member (?) (xtension, propertyName) : 'r =

            // check if dynamic member is an existing property
            match Vmap.tryFind propertyName xtension.Properties with
            | Some property ->
                
                // return property directly if the return type matches, otherwise the default value for that type
                match property.PropertyValue with
                | :? 'r as propertyValue -> propertyValue
                | _ -> failwith ^ "Xtension property '" + propertyName + "' of type '" + property.PropertyType.Name + "' is not of the expected type '" + typeof<'r>.Name + "'."

            | None ->

                // presume we're looking for a property that doesn't exist, so try to get the default value
                Xtension.tryGetDefaultValue xtension propertyName

        /// The dynamic assignment operator for an Xtension.
        /// Example:
        ///     let xtn = xtn.Position <- Vector2 (4.0, 5.0).
        static member (?<-) (xtension, propertyName, value : 'a) =
            // TODO: consider writing a 'Map.addDidContainKey' function to efficently add and return a
            // result that the key was already contained.
            if xtension.Sealed && not ^ Vmap.containsKey propertyName xtension.Properties
            then failwith "Cannot add property to a sealed Xtension."
            else
                let properties = Vmap.add propertyName { PropertyValue = value :> obj; PropertyType = typeof<'a> } xtension.Properties
                { xtension with Properties = properties }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Xtension =
    
        /// Make an extension with custom safety.
        let make properties canDefault isSealed = { Properties = properties; CanDefault = canDefault; Sealed = isSealed }
    
        /// An Xtension that can default and isn't sealed.
        let empty = make (Vmap.makeEmpty ()) true false
    
        /// An Xtension that cannot default and is sealed.
        let safe = make (Vmap.makeEmpty ()) false true
    
        /// An Xtension that cannot default and isn't sealed.
        let mixed = make (Vmap.makeEmpty ()) false false
    
        /// Get a property from an xtension.
        let getProperty name xtension = Vmap.find name xtension.Properties
    
        /// Try to get a property from an xtension.
        let tryGetProperty name xtension = Vmap.tryFind name xtension.Properties
    
        /// Attach a property to an Xtension.
        let attachProperty name property xtension = { xtension with Properties = Vmap.add name property xtension.Properties }
    
        /// Attach multiple properties to an Xtension.
        let attachProperties namesAndProperties xtension = { xtension with Properties = Vmap.addMany namesAndProperties xtension.Properties }
    
        /// Detach a property from an Xtension.
        let detachProperty name xtension = { xtension with Properties = Vmap.remove name xtension.Properties }
    
        /// Detach multiple properties from an Xtension.
        let detachProperties names xtension = { xtension with Properties = Vmap.removeMany names xtension.Properties }
    
        /// Convert an xtension to a sequence of its entries.
        let toSeq xtension = xtension.Properties :> _ seq
    
        /// Convert an xtension to a sequence of its entries.
        let ofSeq seq = attachProperties seq empty