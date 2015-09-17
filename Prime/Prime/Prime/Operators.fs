// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Diagnostics
open System.ComponentModel
open System.Reflection
open System.IO

[<AutoOpen>]
module Operators =

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

    /// Tuple up two values.
    let inline tuple f x y = f (x, y)

    /// Untuple two values within function context f.
    let inline untuple f (x, y) = f x y

    /// Transforms a function by flipping the order of its arguments.
    let inline flip f x y = f y x

    /// Transforms a function by flipping the order of its arguments.
    let inline flip3 f x y z = f z x y

    /// Transforms a function by flipping the order of its arguments.
    let inline flip4 f x y z w = f w x y z

    /// Test for null.
    let inline isNull x = match x with null -> true | _ -> false

    /// Test for non-null.
    let inline isNotNull x = not (isNull x)

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
    let enumerable<'a> enumeratable =
        System.Linq.Enumerable.Cast<'a> enumeratable

    /// Convert a couple of ints to a Guid value.
    /// It is the user's responsibility to ensure uniqueness when using the resulting Guids.
    let intsToGuid m n =
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

    /// Sequences two functions like Haskell ($).
    let inline (^) f g = f g

    /// Test for reference equality.
    let inline (==) (x : 'a) (y : 'a) = referenceEquals x y

    /// Test just the value parts of a type for equality.
    let inline (===) (x : 'a) (y : 'a) = similar x y

    /// Combine the contents of two maps, taking an item from the second map in the case of a key
    /// conflict.
    let inline (@@) map map2 =
        Map.fold
            (fun map key value -> Map.add key value map)
            map
            map2