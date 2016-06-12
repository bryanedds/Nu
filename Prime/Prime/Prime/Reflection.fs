// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open System.Reflection
open System.IO
open Microsoft.FSharp.Reflection

/// Along with the Label binding, is used to elaborate the name of a target without using a
/// string literal.
type LabelName =
    { DummyField : unit }
    static member (?) (_, name) = name

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module LabelName =

    /// Along with the LabelName type, is used to elaborate the name of a target without
    /// using a string literal.
    ///
    /// Usage:
    ///     let fieldName = Label?MyFieldName
    let Label = { DummyField = () }
    
    /// Label for module names.
    /// Needed since we can't utter something like typeof<MyModule>.
    let Module = Label

module Type =

    /// Try to get an existing type with the given unqualified name. Time-intensive.
    let TryGetTypeUnqualified name =
        match Type.GetType name with
        | null ->
            let allAssemblies = AppDomain.CurrentDomain.GetAssemblies ()
            let types =
                Seq.choose
                    (fun (assembly : Assembly) ->
                        match assembly.GetType name with
                        | null -> None
                        | ty -> Some ty)
                    allAssemblies
            Seq.tryFind (fun _ -> true) types
        | ty -> Some ty

    /// Get an existing type with the given unqualified name. Time-intensive.
    let GetTypeUnqualified name =
        match TryGetTypeUnqualified name with
        | Some ty -> ty
        | None -> failwith ^ "Could not find type with unqualified name '" + name + "'."

    /// Get the first property that is signalled to be preferred by the 'preference' predicate.
    let GetPropertyByPreference (preference, properties) =
        let optPreferred = Seq.tryFind preference properties
        if Seq.isEmpty properties then null
        else
            match optPreferred with
            | Some preferred -> preferred
            | None -> Seq.head properties

[<AutoOpen>]
module TypeExtension =

    /// Type extension for Type.
    type Type with

        /// Get the default value for a type.
        /// Never returns null.
        member this.GetDefaultValue () =
            if this.IsPrimitive then Activator.CreateInstance this
            elif this = typeof<string> then "" :> obj
            elif this.Name = typedefof<_ array>.Name then Array.empty :> obj
            elif this.Name = typedefof<_ list>.Name then List.empty :> obj
            elif this.Name = typedefof<_ Set>.Name then Set.empty :> obj
            elif this.Name = typedefof<Map<_, _>>.Name then Map.empty :> obj
            elif this.Name = typedefof<Vmap<_, _>>.Name then Vmap.makeEmpty () :> obj
            elif FSharpType.IsUnion this then
                let unionCases = FSharpType.GetUnionCases this
                if (unionCases.[0].GetFields ()).Length = 0
                then FSharpValue.MakeUnion (unionCases.[0], [||])
                else failwithumf ()
            elif isNotNull ^ this.GetConstructor [||] then Activator.CreateInstance ()
            else failwithumf ()

        /// Get the type descriptor for this type as returned by the global TypeDescriptor.
        member this.GetTypeDescriptor () =
            (TypeDescriptor.GetProvider this).GetTypeDescriptor this

        /// Try to get a custom type converter for the given type.
        member this.TryGetCustomTypeConverter () =
            let globalConverterAttributes =
                seq {
                    for attribute in TypeDescriptor.GetAttributes this do
                        match attribute with
                        | :? TypeConverterAttribute as tca -> yield tca
                        | _ -> () }
            let typeConverterAttributes =
                this.GetCustomAttributes (typeof<TypeConverterAttribute>, true) |>
                Seq.map (fun attr -> attr :?> TypeConverterAttribute) |>
                Seq.append globalConverterAttributes
            if not ^ Seq.isEmpty typeConverterAttributes then
                let typeConverterAttribute = Seq.head typeConverterAttributes
                let typeConverterTypeName = typeConverterAttribute.ConverterTypeName
                let typeConverterType = Type.GetType typeConverterTypeName
                match typeConverterType.GetConstructor [|typeof<Type>|] with
                | null -> (typeConverterType.GetConstructor [||]).Invoke [||] :?> TypeConverter |> Some
                | constructor1 -> constructor1.Invoke [|this|] :?> TypeConverter |> Some
            else None

        /// Get a property with the given name that can be written to, or null.
        member this.GetPropertyWritable propertyName =
            let optProperty =
                Seq.tryFind
                    (fun (property : PropertyInfo) -> property.Name = propertyName && property.CanWrite)
                    (this.GetProperties ())
            match optProperty with
            | Some property -> property
            | None -> null

        /// Get all the properties with the given name.
        member this.GetProperties propertyName =
            Seq.filter
                (fun (property : PropertyInfo) -> property.Name = propertyName)
                (this.GetProperties ())

        /// Get all the properties that can be written to.
        member this.GetPropertiesWritable () =
            Seq.filter
                (fun (property : PropertyInfo) -> property.CanWrite)
                (this.GetProperties ())

        /// Get all the properties with the give name that can be written to.
        member this.GetPropertiesWritable propertyName =
            Seq.filter
                (fun (property : PropertyInfo) -> property.Name = propertyName && property.CanWrite)
                (this.GetProperties ())

        /// Get the first property with the given name that is signalled to be preferred by the 'preference' predicate.
        member this.GetPropertyByPreference (preference, propertyName) =
            let properties = this.GetProperties propertyName
            Type.GetPropertyByPreference (preference, properties)

        /// Get the property with the given name, preferring the variant that can be written to, or null if none found.
        member this.GetPropertyPreferWritable propertyName =
            this.GetPropertyByPreference ((fun (property : PropertyInfo) -> property.CanWrite), propertyName)

        /// Get all the properties that are signalled to be preferred by the 'preference' predicate.
        member this.GetPropertiesByPreference preference =
            let propertiesGrouped =
                Seq.groupBy
                    (fun (property : PropertyInfo) -> property.Name)
                    (this.GetProperties ())
            let optProperties =
                Seq.map
                    (fun (_, properties) -> Type.GetPropertyByPreference (preference, properties))
                    propertiesGrouped
            Seq.filter (fun optProperty -> optProperty <> null) optProperties

        /// Get all the properties, preferring those that can be written to if there is a name clash.
        member this.GetPropertiesPreferWritable () =
            this.GetPropertiesByPreference (fun (property : PropertyInfo) -> property.CanWrite)