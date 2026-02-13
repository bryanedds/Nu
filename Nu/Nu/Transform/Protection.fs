// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Numerics
open Prime

/// Describes the protection category of a simulant.
type [<StructuralEquality; StructuralComparison; Struct>] Protection =

    /// A simulant that is unprotected from deletion in editor.
    | Unprotected

    /// A simulant that is protected from deletion in editor but still serializes.
    | ManualProtection

    /// A simulant that is protected from deletion in editor and does not serialize due to its declarative nature.
    | DeclarativeProtection