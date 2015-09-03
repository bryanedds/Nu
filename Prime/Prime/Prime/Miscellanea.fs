// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Diagnostics
open System.ComponentModel
open System.Reflection
open System.IO

[<AutoOpen>]
module Miscellanea =

    /// The tautology function.
    /// No matter what you pass it, it evaluates to true.
    let inline tautology _ = true

    /// The tautology function with two arguments.
    /// No matter what you pass it, it evaluates to true.
    let inline tautology2 _ _ = true

    /// The tautology function with three arguments.
    /// No matter what you pass it, it evaluates to true.
    let inline tautology3 _ _ _ = true

    /// The absurdity function.
    /// No matter what you pass it, it evaluates to false.
    let inline absurdity _ = false

    /// The absurdity function with two arguments.
    /// No matter what you pass it, it evaluates to false.
    let inline absurdity2 _ _ = false

    /// Convert any value to an obj.
    let inline objectify x = x :> obj

    /// Flip two function parameters.
    let inline flip f x y = f y x

    /// Transforms a function by flipping the order of its arguments.
    let inline flip3 f x y z = f z x y

    /// Transforms a function by flipping the order of its arguments.
    let inline flip4 f x y z w = f w x y z

    /// Test for null.
    let inline isNull x = match x with null -> true | _ -> false

    /// Fail with an unexpected match failure.
    let inline failwithumf () = failwith "Unexpected match failure."

    /// Convert any value to its type.
    let inline getType x = x.GetType ()

    /// Get the fields of a type.
    let inline getFields (t : Type) = t.GetFields (BindingFlags.Instance ||| BindingFlags.Public)

    /// Get the value of a field.
    let inline getFieldValue (f : FieldInfo) (x : obj) = f.GetValue x

    /// Get the properties of a type.
    let inline getProperties (t : Type) = t.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)

    /// Get the value of a property.
    let inline getPropertyValue indices (p : PropertyInfo) (x : obj) = p.GetValue (x, indices)

    /// Test for reference equality.
    let inline referenceEquals (x : 'a) (y : 'a) = obj.ReferenceEquals (x, y)

    /// Test just the value parts of a type for equality.
    /// NOTE: This function uses mad reflection, so is extremely slow, and should not be used in tight loops.
    let rec similar (x : obj) (y : obj) =
        if isNull x then isNull y
        elif isNull y then false
        elif referenceEquals (getType x) (getType y) then
            let aType = getType x
            if  aType.IsValueType ||
                aType = typeof<string> then
                x = y
            else if aType.IsSubclassOf typeof<Stream> then
                // NOTE: Stream has a screwed up contract that its Length property can throw if seeking is not
                // supported. They should have returned nullable int instead, but nooooo....
                true
            else
                let fieldsSimilar =
                    aType
                    |> getFields
                    |> Array.forall (fun i -> similar (getFieldValue i x) (getFieldValue i y))
                let propertiesSimilar =
                    aType
                    |> getProperties
                    |> Array.filter (fun p -> (p.GetIndexParameters ()).Length = 0)
                    |> Array.forall (fun i -> similar (getPropertyValue null i x) (getPropertyValue null i y))
                fieldsSimilar && propertiesSimilar
        else false

    /// A generic identification code type.
    type Id = int64

    /// The invalid Id.
    let [<Literal>] InvalidId = 0L

    /// Apply a function recursively a number of times.
    let rec doTimes fn arg times =
        if times < 0 then failwith "Cannot call doTimes with times < 0."
        elif times = 0 then arg
        else doTimes fn (fn arg) (times - 1)

    /// Perform an operation until a predicate passes.
    let rec doUntil op pred =
        if not (pred ()) then
            op ()
            doUntil op pred

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

    /// Add a custom TypeConverter to an existing type.
    let assignTypeConverter<'t, 'c> () =
        ignore (TypeDescriptor.AddAttributes (typeof<'t>, TypeConverterAttribute typeof<'c>))

    /// Short-hand for linq enumerable cast.
    let enumerable<'a> enumeratable =
        System.Linq.Enumerable.Cast<'a> enumeratable

    /// Convert a couple of ints to a Guid value.
    /// It is the user's responsibility to ensure uniqueness when using the resulting Guids.
    let intsToGuid m n =
        let bytes = Array.create<byte> 8 (byte 0)
        Guid (m, int16 (n >>> 16), int16 n, bytes)

    /// Sequences two functions like Haskell ($).
    let inline (^) f g = f g

    /// Test for reference equality.
    let inline (==) (x : 'a) (y : 'a) = referenceEquals x y

    /// Test just the value parts of a type for equality.
    let inline (===) (x : 'a) (y : 'a) = similar x y

    /// Test two strings for binary equality.
    let inline ($) (s : string) (s2 : string) = s.Equals s2

    /// Combine the contents of two maps, taking an item from the second map in the case of a key
    /// conflict.
    let inline (@@) map map2 =
        Map.fold
            (fun map key value -> Map.add key value map)
            map
            map2

    /// Along with the Symbol binding, is used to elaborate the name of a symbol without using a
    /// string literal.
    type SymbolName =
        { DummyField : unit }
        static member (?) (_, name) = name

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