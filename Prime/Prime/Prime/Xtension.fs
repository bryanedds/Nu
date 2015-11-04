// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Reflection
open System.ComponentModel
open Prime

/// An attribute to specify the default value of an XField.
[<AttributeUsage (AttributeTargets.Class)>]
type XDefaultValueAttribute (defaultValue : obj) =
    inherit Attribute ()
    member this.DefaultValue = defaultValue
    
/// Describes an XField.
type [<StructuralEquality; NoComparison>] XFieldDescriptor =
    { FieldName : string
      FieldType : Type }

/// An Xtension field.
type [<StructuralEquality; NoComparison>] XField =
    { FieldValue : obj
      FieldType : Type }

/// An indexible collection of XFields.
type XFields = Vmap<string, XField>

/// Xtensions (and their supporting types) are a dynamic, functional, and semi-convenient way
/// to implement dynamic fields.
/// TODO: use DebuggerTypeProxyAttribute to make xFields easier to browse in the debugger.
type [<NoEquality; NoComparison>] Xtension =
    { XFields : XFields
      CanDefault : bool
      Sealed : bool }

    /// Get the default value of an instance of type 'r taking into account XDefaultValue decorations.
    static member private getDefaultValue () : 'r =
        let defaultFieldType = typeof<'r>
        let optDefaultValueAttribute = Seq.tryHead ^ defaultFieldType.GetCustomAttributes<XDefaultValueAttribute> ()
        match optDefaultValueAttribute with
        | Some defaultValueAttribute ->
            match defaultValueAttribute.DefaultValue with
            | :? 'r as defaultValue -> defaultValue
            | _ as defaultValue ->
                let defaultValueType = defaultValue.GetType ()
                let converter = AlgebraicConverter defaultValueType
                if converter.CanConvertFrom defaultFieldType
                then converter.ConvertFrom defaultValue :?> 'r
                else failwith ^ "Cannot convert '" + acstring defaultValue + "' to type '" + defaultFieldType.Name + "'."
        | None -> Unchecked.defaultof<'r>

    /// Try to get the default value for a given xtension member, returning None when defaulting is disallowed.
    static member private tryGetDefaultValue (this : Xtension) memberName : 'r =
        if this.CanDefault then Xtension.getDefaultValue ()
        else failwith ^ "Xtension field '" + memberName + "' does not exist and no default is permitted because CanDefault is false."

    /// The dynamic look-up operator for an Xtension.
    /// Example:
    ///     let parallax = entity?Parallax : single
    static member (?) (xtension, memberName) : 'r =

        // check if dynamic member is an existing field
        match Vmap.tryFind memberName xtension.XFields with
        | Some field ->
            
            // return field directly if the return type matches, otherwise the default value for that type
            match field.FieldValue with
            | :? 'r as fieldValue -> fieldValue
            | _ -> failwith ^ "Xtension field '" + memberName + "' of type '" + field.FieldType.Name + "' is not of the expected type '" + typeof<'r>.Name + "'."

        | None ->

            // presume we're looking for a field that doesn't exist, so try to get the default value
            Xtension.tryGetDefaultValue xtension memberName

    /// The dynamic assignment operator for an Xtension.
    /// Example:
    ///     let entity = entity.Position <- Vector2 (4.0, 5.0).
    static member (?<-) (xtension, fieldName, value : 'a) =
#if DEBUG
        // NOTE: nop'ed outside of debug mode for efficiency
        // TODO: consider writing a 'Map.addDidContainKey' function to efficently add and return a
        // result that the key was already contained.
        if xtension.Sealed && not ^ Vmap.containsKey fieldName xtension.XFields
        then failwith "Cannot add field to a sealed Xtension."
        else
#endif
            let xFields = Vmap.add fieldName { FieldValue = value :> obj; FieldType = typeof<'a> } xtension.XFields
            { xtension with XFields = xFields }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Xtension =

    let [<Literal>] private FieldMapDepth = 3

    /// An Xtension that can default and isn't sealed.
    let empty = { XFields = Vmap.makeEmpty (KeyEq strEq) FieldMapDepth; CanDefault = true; Sealed = false }

    /// An Xtension that cannot default and is sealed.
    let safe = { XFields = Vmap.makeEmpty (KeyEq strEq) FieldMapDepth; CanDefault = false; Sealed = true }

    /// An Xtension that cannot default and isn't sealed.
    let mixed = { XFields = Vmap.makeEmpty (KeyEq strEq) FieldMapDepth; CanDefault = false; Sealed = false }

    /// Make an extension with custom safety.
    let make canDefault isSealed = { XFields = Vmap.makeEmpty (KeyEq strEq) FieldMapDepth; CanDefault = canDefault; Sealed = isSealed }