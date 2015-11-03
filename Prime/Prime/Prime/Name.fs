// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Prime
open System
open System.ComponentModel
open System.Reflection
open Prime

/// Converts Name types.
type NameConverter (targetType : Type) =
    inherit TypeConverter ()
    
    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = targetType
        
    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            let toStringMethod = targetType.GetMethod "ToString"
            toStringMethod.Invoke (source, null)
        elif destType = targetType then source
        else failwith "Invalid NameConverter conversion to source."
        
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = targetType
        
    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as nameStr ->
            let makeFunction = targetType.GetMethod ("make", BindingFlags.Static ||| BindingFlags.Public)
            makeFunction.Invoke (null, [|nameStr|])
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failwith "Invalid NameConverter conversion from source."

/// A name for optimized look-up in hashing containers.
/// OPTIMIZATION: OptHashCode is lazy for speed.
type [<CustomEquality; CustomComparison; TypeConverter (typeof<NameConverter>)>] Name =
    private
        { NameStr : string
          mutable OptHashCode : int option }

    /// Make a name from a non-empty string without whitespace.
    static member make (nameStr : string) =
        match nameStr with
        | _ when nameStr.IndexOfAny [|'\n'; '\r'; '\t'; ' '|] <> -1 -> failwith "Invalid name; must have no whitespace characters."
        | _ -> { NameStr = nameStr; OptHashCode = None }

    /// Hash a Name.
    static member hash name =
        match name.OptHashCode with
        | Some hashCode -> hashCode
        | None ->
            let hashCode = name.NameStr.GetHashCode ()
            name.OptHashCode <- Some hashCode
            hashCode

    /// Equate Names.
    static member equals name name2 =
        String.Equals (name.NameStr, name2.NameStr, StringComparison.Ordinal)

    /// Compare Names.
    static member compare name name2 =
        String.Compare (name.NameStr, name2.NameStr, StringComparison.Ordinal)

    interface Name IComparable with
        member this.CompareTo that =
            Name.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? Name as that -> Name.compare this that
            | _ -> failwith "Invalid Name comparison (comparee not of type Name)."

    interface Name IEquatable with
        member this.Equals that =
            Name.equals this that

    override this.Equals that =
        match that with
        | :? Name as that -> Name.equals this that
        | _ -> false

    override this.GetHashCode () =
        Name.hash this

    override this.ToString () =
        this.NameStr

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Name =

    /// Get the name of a name key.
    let getNameStr name =
        name.NameStr

    let join sep names =
        let nameStrs = Seq.map getNameStr names
        let namesStr = String.Join (sep, nameStrs)
        Name.make namesStr

    let split sep name =
        name |>
        getNameStr |>
        (fun nameStr -> nameStr.Split sep) |>
        Array.map Name.make |>
        Seq.ofArray

    let empty =
        Name.make String.Empty

[<AutoOpen>]
module NameOperators =

    /// Convert a name string to a name.
    let inline (!!) nameStr =
        Name.make nameStr