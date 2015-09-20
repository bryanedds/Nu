// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open Prime
open Nu

/// A name key for optimized look-up in hashing containers.
/// OPTIMIZATION: OptHashCode is lazy for speed.
/// TODO: implement custom comparison.
type [<CustomEquality; NoComparison>] NameKey =
    private
        { Name : string
          mutable OptHashCode : int option }

    /// Hash a NameKey.
    static member hash nameKey =
        match nameKey.OptHashCode with
        | Some hashCode -> hashCode
        | None ->
            let hashCode = nameKey.Name.GetHashCode ()
            nameKey.OptHashCode <- Some hashCode
            hashCode

    /// Equate NameKeys.
    static member equals nameKey nameKey2 =
        String.Equals (nameKey.Name, nameKey2.Name, StringComparison.Ordinal)

    interface NameKey IEquatable with
        member this.Equals that =
            NameKey.equals this that

    override this.Equals that =
        match that with
        | :? NameKey as that -> NameKey.equals this that
        | _ -> false

    override this.GetHashCode () =
        NameKey.hash this

    override this.ToString () =
        this.Name

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NameKey =

    /// Make a name key from a non-empty name string.
    let make name =
        match name with
        | "" -> failwith "Invalid name; must be a non-empty string."
        | _ when name.Contains "/" -> failwith "Invalid name; must have no '/' characters."
        | _ -> { Name = name; OptHashCode = None }

    /// Get the name of a name key.
    let getName nameKey =
        nameKey.Name