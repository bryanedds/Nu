namespace Nu

[<AutoOpen>]
module RQueue =

    /// Type alias to emphasize that messages arrive in a reversed queue since I am
    /// too lazy to write an immutable queue directly.
    type 'a rQueue = 'a list