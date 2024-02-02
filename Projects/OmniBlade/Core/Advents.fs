// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

type Advent =
    | Opened of Guid
    | ShadeRecruited
    | MaelRecruited
    | RiainRecruited
    | PericRecruited
    | MadTrixterDefeated
    | HeavyArmorosDefeated
    | AraneaImplicitumDefeated
    | TombSealed
    | ForestSealed
    | FactorySealed
    | SwampSealed
    | LakeSealed
    | CastlePlusSealed
    | FactoryPlusSealed
    | VolcanoSealed
    | KylaAdvent of int
    | OranAdvent of int
    | NarahAdvent of int
    | SiphonieAdvent of int
    | JinnAdvent of int

[<RequireQualifiedAccess>]
module Advents =

    let empty =
        Set.empty<Advent>

    let initial =
        Set.ofList
            [TombSealed
             ForestSealed
             FactorySealed
             SwampSealed
             LakeSealed
             CastlePlusSealed
             FactoryPlusSealed
             VolcanoSealed
             KylaAdvent 0
             OranAdvent 0
             NarahAdvent 0
             SiphonieAdvent 0
             JinnAdvent 0]