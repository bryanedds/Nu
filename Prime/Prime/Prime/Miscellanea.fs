// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Diagnostics
open System.ComponentModel
open System.Reflection
open System.IO

/// A generalized identification code.
type Id = int64

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Id =

    /// The invalid Id.
    let [<Literal>] InvalidId = 0L

    /// Make a function that gets a unique number.
    /// TODO: place a mutex lock in this
    /// TODO: see if returned function can be optimized by minimizing dereferences
    let makeIdMaker () =
        let id = ref InvalidId
        let makeId =
            (fun () ->
                id := !id + 1L
                if !id = 0L then Debug.Fail "Id counter overflowed (flipped back to zero). Big trouble likely ahead!"
                !id)
        makeId

/// Along with the Symbol binding, is used to elaborate the name of a symbol without using a
/// string literal.
type SymbolName =
    { DummyField : unit }
    static member (?) (_, name) = name

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SymbolName =

    /// Along with the SymbolName type, is used to elaborate the name of a field without using a
    /// string literal.
    ///
    /// Usage:
    ///     let fieldName = Symbol?MySymbolName
    let Symbol = { DummyField = () }
    
    /// Symbol alias for module names.
    /// Needed since we can't utter something like typeof<MyModule>.
    let Module = Symbol

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
                        | aType -> Some aType)
                    allAssemblies
            Seq.tryFind (fun _ -> true) types
        | aType -> Some aType

    /// Get an existing type with the given unqualified name. Time-intensive.
    let GetTypeUnqualified name =
        match TryGetTypeUnqualified name with
        | Some aType -> aType
        | None -> failwith ^ "Could not find type with unqualified name '" + name + "'."

    /// TODO: document!
    let GetPropertyByPreference (preference, properties) =
        let optPreferred = Seq.tryFind preference properties
        if Seq.isEmpty properties then null
        else
            match optPreferred with
            | Some preferred -> preferred
            | None -> Seq.head properties

[<AutoOpen>]
module TypeModule =

    /// Type extension for Type.
    type Type with

        /// Try to get a custom type converter for the given type.
        member this.TryGetCustomTypeConverter () =
            // TODO: check for custom type converters in the TypeDescriptor attributes as well
            let typeConverterAttributes = this.GetCustomAttributes<TypeConverterAttribute> ()
            if not ^ Seq.isEmpty typeConverterAttributes then
                let typeConverterAttribute = Seq.head typeConverterAttributes
                let typeConverterTypeName = typeConverterAttribute.ConverterTypeName
                let typeConverterType = Type.GetType typeConverterTypeName
                match typeConverterType.GetConstructor [|typeof<Type>|] with
                | null -> Some ^ (typeConverterType.GetConstructor [||]).Invoke [||]
                | constructor1 -> Some ^ constructor1.Invoke [|this|]
            else None

        member this.GetPropertyWritable propertyName =
            let optProperty =
                Seq.tryFind
                    (fun (property : PropertyInfo) -> property.Name = propertyName && property.CanWrite)
                    (this.GetProperties ())
            match optProperty with
            | Some property -> property
            | None -> null

        member this.GetProperties propertyName =
            Seq.filter
                (fun (property : PropertyInfo) -> property.Name = propertyName)
                (this.GetProperties ())

        member this.GetPropertiesWritable () =
            Seq.filter
                (fun (property : PropertyInfo) -> property.CanWrite)
                (this.GetProperties ())

        member this.GetPropertiesWritable propertyName =
            Seq.filter
                (fun (property : PropertyInfo) -> property.Name = propertyName && property.CanWrite)
                (this.GetProperties ())

        member this.GetPropertyByPreference (preference, propertyName) =
            let properties = this.GetProperties propertyName
            Type.GetPropertyByPreference (preference, properties)

        member this.GetPropertyPreferWritable propertyName =
            this.GetPropertyByPreference ((fun (property : PropertyInfo) -> property.CanWrite), propertyName)

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

        member this.GetPropertiesPreferWritable () =
            this.GetPropertiesByPreference (fun (property : PropertyInfo) -> property.CanWrite)