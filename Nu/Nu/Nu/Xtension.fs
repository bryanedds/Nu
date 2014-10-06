// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Nu
open System
open System.Reflection
open System.ComponentModel
open Microsoft.FSharp.Reflection
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module XtensionModule =

    /// An attribute to specify the default value of an XField.
    [<AttributeUsage (AttributeTargets.Class)>]
    type XDefaultValueAttribute (defaultValue : obj) =
        inherit Attribute ()
        member this.DefaultValue = defaultValue
        
    /// Describes an XField.
    type XFieldDescriptor =
        { FieldName : string
          TypeName : string } // the .NET type name

    /// An indexible collection of XFields.
    type XFields = Map<string, obj>

    /// Xtensions (and their supporting types) are a dynamic, functional, and semi-convenient way
    /// to implement dynamic fields.
    type [<StructuralEquality; NoComparison>] Xtension =
        { XFields : XFields
          CanDefault : bool
          Sealed : bool }

        /// Get the default value of an instance of type 'r taking into account XDefaultValue decorations.
        static member private getDefaultValue () : 'r =
            let defaultFieldType = typeof<'r>
            let optDefaultValueAttribute = Seq.tryHead <| defaultFieldType.GetCustomAttributes<XDefaultValueAttribute> ()
            match optDefaultValueAttribute with
            | Some defaultValueAttribute ->
                match defaultValueAttribute.DefaultValue with
                | :? 'r as defaultValue -> defaultValue
                | _ as defaultValue ->
                    let converter = TypeDescriptor.GetConverter defaultFieldType
                    let defaultValueType = defaultValue.GetType ()
                    if converter.CanConvertFrom defaultValueType then converter.ConvertFrom defaultValue :?> 'r
                    else failwith <| "Cannot convert '" + string defaultValue + "' to type '" + defaultFieldType.Name + "'."
            | None -> Unchecked.defaultof<'r>

        /// Try to get the default value for a given xtension member, returning None when defaulting is disallowed.
        static member private tryGetDefaultValue (this : Xtension) memberName : 'r =
            if this.CanDefault then Xtension.getDefaultValue ()
            else failwith <| "The Xtension field '" + memberName + "' does not exist and no default is permitted because CanDefault is false."

        /// The dynamic look-up operator for an Xtension.
        /// Example -   let parallax = entity?Parallax : single
        static member (?) (xtension, memberName) : 'r =

                // check if dynamic member is an existing field
                match Map.tryFind memberName xtension.XFields with
                | Some field ->
                    
                    // return field directly if the return type matches, otherwise the default value for that type
                    match field with
                    | :? 'r as fieldValue -> fieldValue
                    | _ -> Xtension.tryGetDefaultValue xtension memberName

                | None ->

                    // presume we're looking for a field that doesn't exist, so try to get the default value
                    Xtension.tryGetDefaultValue xtension memberName

        /// The dynamic assignment operator for an Xtension.
        /// Example - let entity = entity.Position <- Vector2 (4.0, 5.0).
        static member (?<-) (xtension, fieldName, value) =
    #if DEBUG
            // nop'ed outside of debug mode for efficiency
            // TODO: consider writing a 'Map.addDidContainKey' function to efficently add and return a
            // result that the key was already contained.
            if xtension.Sealed && not <| Map.containsKey fieldName xtension.XFields
            then failwith "Cannot add field to a sealed Xtension."
            else
    #endif
                let xFields = Map.add fieldName (value :> obj) xtension.XFields
                { xtension with XFields = xFields }

[<RequireQualifiedAccess>]
module Xtension =

    /// An Xtension that can default and isn't sealed.
    let empty = { XFields = Map.empty; CanDefault = true; Sealed = false }

    /// An Xtension that cannot default and is sealed.
    let safe = { XFields = Map.empty; CanDefault = false; Sealed = true }

    /// An Xtension that cannot default and isn't sealed.
    let mixed = { XFields = Map.empty; CanDefault = false; Sealed = false }

    /// Make an extension with custom safety.
    let make canDefault isSealed = { XFields = Map.empty; CanDefault = canDefault; Sealed = isSealed }