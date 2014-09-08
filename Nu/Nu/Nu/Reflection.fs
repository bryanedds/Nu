namespace Nu
open System
open System.Reflection
open Prime

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
            if fieldName = "OptOverlayName" then
                failwith "OptOverlayName cannot be an intrinsic field."
            if fieldName = "FacetNames" then
                failwith "FacetNames cannot be an intrinsic field."
            if Array.exists (fun gta -> gta = typeof<obj>) fieldType.GenericTypeArguments then
                failwith <|
                    "Generic field definition lacking too much type information for field '" + fieldName + "'. " +
                    "Use explicit type annotations on all values that carry incomplete type information."
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
    let rec private getBaseTypesExceptObject (aType : Type) =
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
    let getFieldDefinitionsNoInheritance (targetType : Type) =
        match targetType.GetProperty ("FieldDefinitions", BindingFlags.Static ||| BindingFlags.Public) with
        | null -> failwith <| "Could not get static property FieldDefinitions from type '" + targetType.Name + "'."
        | fieldDefinitionsProperty ->
            let fieldDefinitions = fieldDefinitionsProperty.GetValue null
            match fieldDefinitions with
            | :? (obj list) as fieldDefinitions when List.isEmpty fieldDefinitions -> []
            | :? (FieldDefinition list) as fieldDefinitions -> fieldDefinitions
            | _ -> failwith <| "FieldDefinitions property for type '" + targetType.Name + "' must be of type FieldDefinition list."

    /// Get the field definitions of a target type.
    let getFieldDefinitions (targetType : Type) =
        let baseTypes = targetType :: getBaseTypesExceptObject targetType
        let fieldDefinitionLists = List.map (fun aType -> getFieldDefinitionsNoInheritance aType) baseTypes
        List.concat fieldDefinitionLists

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