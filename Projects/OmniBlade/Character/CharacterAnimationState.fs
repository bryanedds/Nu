// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open OmniBlade

type CharacterMaterialization =
    | Materializing
    | Dematerializing

type [<SymbolicExpansion>] CharacterAnimationState =
    { StartTime : int64
      AnimationSheet : Image AssetTag
      CharacterAnimationType : CharacterAnimationType
      MaterializationOpt : CharacterMaterialization option
      Direction : Direction }

    static member face direction (state : CharacterAnimationState) =
        { state with Direction = direction }

    static member setCharacterAnimationType time characterAnimationType state =
        if state.CharacterAnimationType <> characterAnimationType then
            { state with
                StartTime = time
                CharacterAnimationType = characterAnimationType
                MaterializationOpt = if time > state.StartTime then None else state.MaterializationOpt }
        else state

    static member directionToInt direction =
        match direction with
        | Upward -> 0
        | Rightward -> 1
        | Downward -> 2
        | Leftward -> 3

    static member localTime time state =
        time - state.StartTime

    static member indexCel delay time state =
        let localTime = CharacterAnimationState.localTime time state
        int (localTime / delay)

    static member indexLooped run delay time state =
        CharacterAnimationState.indexCel delay time state % run

    static member indexSaturated run delay time state =
        let cel = CharacterAnimationState.indexCel delay time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection run delay offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexLooped run delay time state + position, 0)
        let position = position + offset
        position

    static member indexLoopedWithoutDirection run delay offset time state =
        let position = CharacterAnimationState.indexLooped run delay time state
        let position = v2i position 0 + offset
        position

    static member indexSaturatedWithDirection run delay offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexSaturated run delay time state + position, 0)
        let position = position + offset
        position

    static member indexSaturatedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexSaturated run stutter time state
        let position = Vector2i (position, 0)
        let position = position + offset
        position

    static member index time state =
        match Map.tryFind state.CharacterAnimationType Data.Value.CharacterAnimations with
        | Some animationData ->
            match animationData.AnimationType with
            | LoopedWithDirection -> CharacterAnimationState.indexLoopedWithDirection animationData.Run animationData.Delay animationData.Offset time state
            | LoopedWithoutDirection -> CharacterAnimationState.indexLoopedWithoutDirection animationData.Run animationData.Delay animationData.Offset time state
            | SaturatedWithDirection -> CharacterAnimationState.indexSaturatedWithDirection animationData.Run animationData.Delay animationData.Offset time state
            | SaturatedWithoutDirection -> CharacterAnimationState.indexSaturatedWithoutDirection animationData.Run animationData.Delay animationData.Offset time state
        | None -> v2iZero

    static member inset time (celSize : Vector2) state =
        let index = CharacterAnimationState.index time state
        let offset = v2 (single index.X) (single index.Y) * celSize
        let inset = box2 offset celSize
        inset

    static member progressOpt time state =
        match Map.tryFind state.CharacterAnimationType Data.Value.CharacterAnimations with
        | Some animationData ->
            let localTime = CharacterAnimationState.localTime time state
            match animationData.LengthOpt with
            | Some length -> Some (min 1.0f (single localTime / single length))
            | None -> None
        | None -> None

    static member getFinished time state =
        match CharacterAnimationState.progressOpt time state with
        | Some progress -> progress = 1.0f
        | None -> true

    static member materialize time state =
        { state with StartTime = time; MaterializationOpt = Some Materializing }

    static member dematerialize time state =
        { state with StartTime = time; MaterializationOpt = Some Dematerializing }

    static member materialized time state =
        { state with StartTime = time; CharacterAnimationType = PoiseAnimation Poising; MaterializationOpt = None }

    static member empty =
        { StartTime = 0L
          AnimationSheet = Assets.Field.JinnAnimationSheet
          CharacterAnimationType = IdleAnimation
          MaterializationOpt = None
          Direction = Downward }

    static member initial =
        { CharacterAnimationState.empty with Direction = Upward }