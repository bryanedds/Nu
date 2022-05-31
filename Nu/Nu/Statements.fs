// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime

/// The base component type of an Ecs.
type Component<'c when 'c : struct and 'c :> 'c Component> =
    interface
        abstract Active : bool with get, set
        end

/// A delegate for interfacing with correlated components.
type Statement<'c, 'w when
    'c : struct and 'c :> 'c Component> =
    delegate of 'c byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> =
    delegate of 'c byref * 'c2 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'c4, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref * 'w -> 'w