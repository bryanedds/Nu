// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Debug
open System
module internal World =

    /// The latest value of the world for debugging in an IDE. Not to be used for anything else.
    let mutable internal Latest = obj ()
    let mutable internal viewGame = fun (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewScreen = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewGroup = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())
    let mutable internal viewEntity = fun (_ : obj) (_ : obj) -> Array.create 0 (String.Empty, obj ())