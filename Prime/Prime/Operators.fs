// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open System.ComponentModel
open System.Diagnostics
open System.Reflection
open System.IO
open Prime

[<AutoOpen>]
module Operators =

    /// The constant function.
    /// No matter what you pass it, it evaluates to a constant value.
    let constant a _ = a

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

    /// Curry up two values.
    let inline curry f a b = f (a, b)

    /// Uncurry two values.
    let inline uncurry f (a, b) = f a b

    /// Transforms a function by flipping the order of its arguments.
    let inline flip f a b = f b a

    /// Transforms a function by flipping the order of its arguments.
    let inline flip3 f a b c = f c a b

    /// Transforms a function by flipping the order of its arguments.
    let inline flip4 f a b c d = f d a b c

    /// Test for null.
    let inline isNull a = match a with null -> true | _ -> false

    /// Test for non-null.
    let inline isNotNull a = match a with null -> false | _ -> true

    /// Test that the given type has null as an actual value.
    let isNullTrueValue (ty : Type) =
        ty.GetCustomAttributes(typeof<CompilationRepresentationAttribute>, true) |>
        Array.map (fun (attr : obj) -> attr :?> CompilationRepresentationAttribute) |>
        Array.exists (fun attr -> int attr.Flags &&& int CompilationRepresentationFlags.UseNullAsTrueValue <> 0)

    /// Get the .NET type of a target.
    let inline getType target = target.GetType ()

    /// Get the .NET type name of a target.
    let inline getTypeName target = (getType target).Name

    /// Get the properties of a type.
    let inline getProperties (t : Type) = t.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)

    /// Test for string equality.
    let inline strEq str str2 = String.Equals (str, str2, StringComparison.Ordinal)

    /// Compare two strings.
    let inline strCmp str str2 = String.Compare (str, str2, StringComparison.Ordinal)

    /// Test for reference equality.
    let inline refEq (a : 'a) (b : 'a) = obj.ReferenceEquals (a, b)

    /// Test for equality, usually faster than (=).
    /// TODO: make sure this always generates code equally fast or faster.
    let inline fastEq (a : 'a) (b : 'a) = LanguagePrimitives.GenericEquality a b

    /// Short-hand for linq enumerable cast.
    let inline enumerable<'a> enumeratable =
        System.Linq.Enumerable.Cast<'a> enumeratable

    /// Get the enumerator for a sequence.
    let inline enumerator (enumeratable : _ seq) =
        enumeratable.GetEnumerator ()

    /// Add a custom TypeConverter to an existing type.
    let assignTypeConverter<'t, 'c> () =
        TypeDescriptor.AddAttributes (typeof<'t>, TypeConverterAttribute typeof<'c>) |> ignore

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

    /// Fail with a 'NotImplementedException'.
    let failwithnie () =
        let stackTrace = StackTrace ()
        let frame = stackTrace.GetFrame 1
        let meth = frame.GetMethod ()
        let line = frame.GetFileLineNumber ()
        let fileName = frame.GetFileName ()
        raise (NotImplementedException (sprintf "Not implemented exception in '%s' on line %i in file %s." meth.Name line fileName))

    /// Sequences two functions like Haskell ($).
    let inline (^) f g = f g