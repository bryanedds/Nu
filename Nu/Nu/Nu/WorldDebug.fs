// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Debug
module internal World =

    /// The latest value of the world for debugging in an IDE. Not to be used for anything else.
    let mutable internal Latest = obj ()
    let mutable internal viewGameProperties = fun (_ : obj) -> Map.empty<string, obj>
    let mutable internal viewGameXProperties = fun (_ : obj) -> Map.empty<string, obj>
    let mutable internal viewScreenProperties = fun (_ : obj) (_ : obj) -> Map.empty<string, obj>
    let mutable internal viewScreenXProperties = fun (_ : obj) (_ : obj) -> Map.empty<string, obj>
    let mutable internal viewGroupProperties = fun (_ : obj) (_ : obj) -> Map.empty<string, obj>
    let mutable internal viewGroupXProperties = fun (_ : obj) (_ : obj) -> Map.empty<string, obj>
    let mutable internal viewEntityProperties = fun (_ : obj) (_ : obj) -> Map.empty<string, obj>
    let mutable internal viewEntityXProperties = fun (_ : obj) (_ : obj) -> Map.empty<string, obj>