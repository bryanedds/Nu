// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu

/// An struct-based representation of a pair.
/// This type would seem redundant with struct (,) and KeyValuePair, but it's not. This type is unique in that allows
/// you to take a reference to its Fst and Snd fields. Unfortunately, there seems to be no way to do that with
/// struct (,) or KeyValuePair.
/// NOTE: StructPair doesn't allow client assemblies to reference its fields, so it's not properly usable outside of
/// Nu's engine code, unfortunately.
type [<Struct>] StructPair<'a, 'b> =
    { Fst : 'a; Snd : 'b }

[<RequireQualifiedAccess>]
module StructPair =

    /// Make a StructPair value.
    let make<'a, 'b> (fst : 'a) (snd : 'b) =
        { Fst = fst; Snd = snd }

    /// Get the fst value in a StructPair.
    let inline fst<'a, 'b> (pair : StructPair<'a, 'b>) =
        pair.Fst

    /// Get the snd value in a StructPair.
    let inline snd<'a, 'b> (pair : StructPair<'a, 'b>) =
        pair.Snd

    /// Replace StructPair member fst.
    let inline withFst<'a, 'b> fst (pair : StructPair<'a, 'b>) =
        make fst pair.Snd

    /// Replace StructPair member snd.
    let inline withSnd<'a, 'b> snd (pair : StructPair<'a, 'b>) =
        make pair.Fst snd

    /// Map over StructPair member fst.
    let inline mapFst<'a, 'b> mapper (pair : StructPair<'a, 'b>) =
        make (mapper pair.Fst) pair.Snd

    /// Map over StructPair member snd.
    let inline mapSnd<'a, 'b> mapper (pair : StructPair<'a, 'b>) =
        make pair.Fst (mapper pair.Snd)

[<AutoOpen>]
module StructPairOperators =

    /// Make a StructPair value.
    let inline pairValue<'a, 'b> a b =
        StructPair.make<'a, 'b> a b