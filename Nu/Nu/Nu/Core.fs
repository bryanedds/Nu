module Nu.Core

/// A generic identification code type.
/// A serializable value type.
type ID = int64

/// Specifies the address of an element in a game.
/// Note that subscribing to a partial address results in listening to all messages whose
/// beginning address nodes match the partial address (sort of a wild-card).
/// A value type.
type Address = Lun list

[<AutoOpen>]
module RQueue =

    /// Type alias to emphasize that messages arrive in a reversed queue since I am too lazy to write an immutable queue directly.
    type 'a rQueue = 'a list