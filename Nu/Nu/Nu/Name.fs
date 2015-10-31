// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open Prime

/// A name for optimized look-up in hashing containers.
/// OPTIMIZATION: OptHashCode is lazy for speed.
/// TODO: implement custom comparison.
type [<CustomEquality; NoComparison>] Name =
    private
        { NameStr : string
          mutable OptHashCode : int option }

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

    /// Make a name from a non-empty name string.
    let make nameStr =
        match nameStr with
        | "" -> failwith "Invalid name; must be a non-empty string."
        | _ when nameStr.IndexOfAny [|'\n'; '\r'; '\t'; ' '; '/'|] <> -1 -> failwith "Invalid name; must have no whitespace or '/' characters."
        | _ -> { NameStr = nameStr; OptHashCode = None }

    /// Get the name of a name key.
    let getNameStr name =
        name.NameStr

[<AutoOpen>]
module NameOperators =

    /// Convert a name string to a name.
    let inline name nameStr =
        Name.make nameStr