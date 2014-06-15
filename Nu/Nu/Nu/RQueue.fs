// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu

[<AutoOpen>]
module RQueueModule =

    /// Type alias to emphasize that messages arrive in a reversed queue since I am
    /// too lazy to write an immutable queue directly.
    type 'a rQueue = 'a list