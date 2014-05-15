namespace BlazeVector
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module BlazeDispatchersModule =

    /// The custom type for BlazeVector's game dispatcher.
    /// Currently just a placeholder as it doesn't yet have any special implementation.
    type BlazeGameDispatcher () =
        inherit GameDispatcher ()