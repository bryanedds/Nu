// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open System.Diagnostics
open System.Reflection
open System.IO

[<AutoOpen>]
module Operators =

    /// The tautology function.
    /// No matter what you pass it, it evaluates to true.
    let tautology _ = true

    /// The tautology function with two arguments.
    /// No matter what you pass it, it evaluates to true.
    let tautology2 _ _ = true

    /// The tautology function with three arguments.
    /// No matter what you pass it, it evaluates to true.
    let tautology3 _ _ _ = true

    /// The absurdity function.
    /// No matter what you pass it, it evaluates to false.
    let absurdity _ = false

    /// The absurdity function with two arguments.
    /// No matter what you pass it, it evaluates to false.
    let absurdity2 _ _ = false

    /// Convert any value to an obj.
    let objectify x = x :> obj

    /// Curry up two values.
    let inline curry f x y = f (x, y)

    /// Uncurry two values.
    let inline uncurry f (x, y) = f x y

    /// Transforms a function by flipping the order of its arguments.
    let inline flip f x y = f y x

    /// Transforms a function by flipping the order of its arguments.
    let inline flip3 f x y z = f z x y

    /// Transforms a function by flipping the order of its arguments.
    let inline flip4 f x y z w = f w x y z

    /// Test for null.
    let inline isNull x = match x with null -> true | _ -> false

    /// Test for non-null.
    let inline isNotNull x = match x with null -> false | _ -> true

    /// Test that the given type has null as an actual value.
    let isNullTrueValue (ty : Type) =
        ty.GetCustomAttributes(typeof<CompilationRepresentationAttribute>, true) |>
        Array.map (fun (attr : obj) -> attr :?> CompilationRepresentationAttribute) |>
        Array.exists (fun attr -> int attr.Flags &&& int CompilationRepresentationFlags.UseNullAsTrueValue <> 0)

    /// Convert a nullable value into an option.
    let inline denull x = match x with null -> None | _ -> Some x

    /// Test for string equality.
    let inline strEq str str2 = String.Equals (str, str2, StringComparison.Ordinal)

    /// Compare two strings.
    let inline strCmp str str2 = String.Compare (str, str2, StringComparison.Ordinal)

    /// Get the .NET type of a target.
    let inline getType target = target.GetType ()

    /// Get the .NET type name of a target.
    let inline getTypeName target = (getType target).Name

    /// Get the fields of a type.
    let inline getFields (t : Type) = t.GetFields (BindingFlags.Instance ||| BindingFlags.Public)

    /// Get the value of a field.
    let inline getFieldValue (f : FieldInfo) (x : obj) = f.GetValue x

    /// Get the properties of a type.
    let inline getProperties (t : Type) = t.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)

    /// Get the value of a property.
    let inline getPropertyValue indices (p : PropertyInfo) (x : obj) = p.GetValue (x, indices)

    /// Test for equality, usually faster than (=).
    let inline fastEq (x : 'a) (y : 'a) = System.IEquatable<'a>.Equals (x, y)

    /// Test for reference equality.
    let inline refEq (x : 'a) (y : 'a) = obj.ReferenceEquals (x, y)

    /// Test just the value parts of a type for equality.
    /// NOTE: This function uses mad reflection, so is extremely slow, and should not be used in tight loops.
    let rec similar (x : obj) (y : obj) =
        if isNull x then isNull y
        elif isNull y then false
        elif refEq (getType x) (getType y) then
            let ty = getType x
            if  ty.IsValueType ||
                ty = typeof<string> then
                x = y
            else if ty.IsSubclassOf typeof<Stream> then
                // NOTE: Stream has a screwed up contract that its Length property can throw if seeking is not
                // supported. They should have returned nullable int instead, but nooooo....
                true
            else
                let fieldsSimilar =
                    ty
                    |> getFields
                    |> Array.forall (fun i -> similar (getFieldValue i x) (getFieldValue i y))
                let propertiesSimilar =
                    ty
                    |> getProperties
                    |> Array.filter (fun p -> (p.GetIndexParameters ()).Length = 0)
                    |> Array.forall (fun i -> similar (getPropertyValue null i x) (getPropertyValue null i y))
                fieldsSimilar && propertiesSimilar
        else false

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

    /// Add a custom TypeConverter to an existing type.
    let assignTypeConverter<'t, 'c> () =
        ignore (TypeDescriptor.AddAttributes (typeof<'t>, TypeConverterAttribute typeof<'c>))

    /// Short-hand for linq enumerable cast.
    let inline enumerable<'a> enumeratable =
        System.Linq.Enumerable.Cast<'a> enumeratable

    /// Get the enumerator for a sequence.
    let inline enumerator (enumeratable : _ seq) =
        enumeratable.GetEnumerator ()

    /// Determine if a string is a guid.
    let inline isGuid str =
        fst (Guid.TryParse str)

    /// Make a Guid.
    let inline makeGuid () =
        Guid.NewGuid ()

    /// Make a Guid from a couple of ints.
    /// It is the user's responsibility to ensure uniqueness when using the resulting Guids.
    let makeGuidFromInts m n =
        let bytes = Array.create<byte> 8 (byte 0)
        Guid (m, int16 (n >>> 16), int16 n, bytes)

    /// Fail with an unexpected match failure.
    let failwithumf () =
        let stackTrace = StackTrace ()
        let frame = stackTrace.GetFrame 1
        let meth = frame.GetMethod ()
        let line = frame.GetFileLineNumber ()
        let fileName = frame.GetFileName ()
        failwithf "Unexpected match failure in '%s' on line %i in file %s." meth.Name line fileName

    /// As close as we can get to F# implicits.
    let inline implicit arg = (^a : (static member op_Implicit : ^b -> ^a) arg)

    /// Sequences two functions like Haskell ($).
    let inline (^) f g = f g

    /// Test for equality, usually faster than (=).
    let inline (==) (x : 'a) (y : 'a) = fastEq x y

    /// Test just the value parts of a type for equality. Reflective and slow.
    let inline (===) (x : 'a) (y : 'a) = similar x y