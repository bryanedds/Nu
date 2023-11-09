// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

type [<StructuralEquality; NoComparison>] CharacterPlus =
    { UpdateTime : int64
      Character : Character }

    static member make time character =
        { UpdateTime = time
          Character = character }

    static member empty =
        { UpdateTime = 0L
          Character = Character.empty }