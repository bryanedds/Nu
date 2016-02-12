// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime

/// Type alias to emphasize that messages arrive in a reversed queue since I am
/// too lazy to write an immutable queue directly.
///
/// TODO: replace all usage with Queue from FSharpx.Collections.
type 'a rQueue = 'a list