module Nu.Core

/// A generic identification code type.
/// A serializable value type.
type Id = int64

/// Specifies the address of an element in a game.
/// Note that subscribing to a partial address results in listening to all messages whose
/// beginning address nodes match the partial address (sort of a wild-card).
/// A value type.
type Address = Lun list

/// Create a Nu Id.
/// NOTE: Ironically, not purely functional (TODO: maybe address this?)
let getNuId = createGetNextId ()