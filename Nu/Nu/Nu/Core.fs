module Nu.Core

/// A generic identification code type.
/// OPTIMIZATION: A 32-bit type rather than 64 as a way to save performance on 32-bit platforms.
/// Note that this optimization is speculative.
/// A serializable value type.
type ID = uint32

/// A 2D vector that stands-in for the one that will come from a .NET library / wrapper.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] Vector2 =
    { X : single
      Y : single }

