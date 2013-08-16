module Nu.Core

/// A generic identification code type.
/// OPTIMIZATION: A 32-bit type rather than 64 as a way to save performance on 32-bit platforms.
/// Note that this optimization is speculative.
/// A serializable value type.
type ID = uint32