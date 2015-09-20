// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu

/// Type alias to emphasize that messages arrive in a reversed queue since I am
/// too lazy to write an immutable queue directly.
///
/// TODO: replace all usage with Queue from FSharpx.Collections.
type 'a rQueue = 'a list