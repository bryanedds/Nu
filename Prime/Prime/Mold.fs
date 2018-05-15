namespace Prime

[<RequireQualifiedAccess>]
module Mold =

    /// Uniquely identifies a type member.
    type MemberPath =
        { TypeName : string
          MemberName : string }

    /// Describes the means through which a member is instantiated.
    type [<NoEquality; NoComparison>] InstantiationRule =
        | Constant of obj
        | Variable of (unit -> obj)

    /// Describes the means through which member are instantiated.
    type InstantiationRules =
        Map<MemberPath option, InstantiationRule>

    /// A primitive mold type.
    type Primitive =
        | Unit
        | Boolean
        | Byte
        | Char
        | String
        | Int32
        | Int64
        | Single
        | Double
        | Decimal
        | Enum of string * string array
        | DateTime
        | IPAddress
        | Guid

    /// Describes an F# type in a normalized fashion.
    type Mold =
        | Primitive of MemberPath option * Primitive
        | Tuple of MemberPath option * string * Mold array
        | Record of MemberPath option * string * (string * Mold) array
        | Union of MemberPath option * string * (string * Mold array) array
        | Option of MemberPath option * Mold
        | List of MemberPath option * Mold
        | Set of MemberPath option * Mold
        | Map of MemberPath option * Mold * Mold
        | Array of MemberPath option * Mold
        | Unidentified of MemberPath option

        static member getMemberPathOpt mold =
            match mold with
            | Primitive (pathOpt, _)
            | Tuple (pathOpt, _, _)
            | Record (pathOpt, _, _)
            | Union (pathOpt, _, _)
            | Option (pathOpt, _)
            | List (pathOpt, _)
            | Set (pathOpt, _)
            | Map (pathOpt, _, _)
            | Array (pathOpt, _)
            | Unidentified pathOpt -> pathOpt

/// Uniquely identifies a type member.
type MemberPath = Mold.MemberPath

/// Describes the means through which a member is instantiated.
type InstantiationRule = Mold.InstantiationRule

/// Describes the means through which member are instantiated.
type InstantiationRules = Mold.InstantiationRules

/// A primitive mold type.
type Primitive = Mold.Primitive

/// Describes an F# type in a normalized fashion.
type Mold = Mold.Mold
