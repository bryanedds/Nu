// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Nu

/// Masks for Transform flags.
module TransformMasks =

    let [<Literal>] ActiveMask =                    0b0000000000000000000000001u
    let [<Literal>] DirtyMask =                     0b0000000000000000000000010u
    let [<Literal>] InvalidatedMask =               0b0000000000000000000000100u
    let [<Literal>] OmnipresentMask =               0b0000000000000000000001000u
    let [<Literal>] AbsoluteMask =                  0b0000000000000000000010000u
    let [<Literal>] ImperativeMask =                0b0000000000000000000100000u
    let [<Literal>] PublishChangeBindingsMask =     0b0000000000000000001000000u
    let [<Literal>] PublishChangeEventsMask =       0b0000000000000000010000000u
    let [<Literal>] EnabledMask =                   0b0000000000000000100000000u
    let [<Literal>] VisibleMask =                   0b0000000000000001000000000u
    let [<Literal>] AlwaysUpdateMask =              0b0000000000000010000000000u
    let [<Literal>] PublishUpdatesMask =            0b0000000000000100000000000u
    let [<Literal>] PublishPostUpdatesMask =        0b0000000000001000000000000u
    let [<Literal>] PersistentMask =                0b0000000000010000000000000u
    let [<Literal>] IgnorePropertyBindingsMask =    0b0000000000100000000000000u
    let [<Literal>] MountedMask =                   0b0000000001000000000000000u
    let [<Literal>] EnabledLocalMask =              0b0000000010000000000000000u
    let [<Literal>] VisibleLocalMask =              0b0000000100000000000000000u
    let [<Literal>] CenteredMask =                  0b0000001000000000000000000u
    let [<Literal>] StaticMask =                    0b0000010000000000000000000u
    let [<Literal>] EnclosedMask =                  0b0000100000000000000000000u
    let [<Literal>] LightMask =                     0b0001000000000000000000000u
    let [<Literal>] RotationMatrixDirtyMask =       0b0010000000000000000000000u
    let [<Literal>] PerimeterOrientedDirtyMask =    0b0100000000000000000000000u
    let [<Literal>] AnglesDirtyMask =               0b1000000000000000000000000u
    let [<Literal>] FlagsDefault =                  0b0110001110010001100100001u