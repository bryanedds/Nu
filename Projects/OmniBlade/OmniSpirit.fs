// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharpx.Collections
open Prime
open Nu

type [<NoEquality; NoComparison>] SpiritMovement =
    | Creep
    | Chase
    | Scatter
    | Wander

type [<NoEquality; NoComparison>] SpiritPattern =
    | Disinterested
    | Stoic
    | Confused
    | Stalking
    | Ambushing

    static member fromInt i =
        match i with
        | 0 -> Disinterested
        | 1 -> Confused
        | 2 -> Stoic
        | 3 -> Stalking
        | 4 -> Ambushing
        | _ -> failwithumf ()

    static member random () =
        Gen.random1 5 |> SpiritPattern.fromInt

    static member toSpiritMovement pattern =
        match pattern with
        | Disinterested ->  [|Creep;     Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander|]
        | Confused ->       [|Creep;     Creep; Creep; Wander;   Creep; Creep; Wander;   Creep; Creep; Wander;   Creep; Creep; Wander;   Creep; Creep; Wander|]
        | Stoic ->          [|Creep;     Creep; Creep; Creep;    Creep; Creep; Creep;    Creep; Creep; Creep;    Creep; Creep; Creep;    Creep; Creep; Creep|]
        | Stalking ->       [|Creep;     Creep; Chase; Chase;    Creep; Chase; Chase;    Creep; Chase; Chase;    Creep; Chase; Chase;    Creep; Chase; Chase|]
        | Ambushing ->      [|Creep;     Chase; Chase; Chase;    Chase; Chase; Chase;    Chase; Chase; Chase;    Chase; Chase; Chase;    Chase; Chase; Chase|]

type [<ReferenceEquality; NoComparison>] SpiritState =
    { SpiritMovements : SpiritMovement array
      SpiritMovementIndex : int
      SpiritMovementStart : int64
      SpiritMovementCachedOpt : Vector2 option }

    static member advance time (position : Vector2) (target : Vector2) spiritState =
        let localTime = time - spiritState.SpiritMovementStart
        let spiritState =
            if localTime >= Constants.Field.SpiritMovementDuration then
                let index = inc spiritState.SpiritMovementIndex
                { spiritState with
                    SpiritMovementIndex = if index >= Array.length spiritState.SpiritMovements then 0 else index
                    SpiritMovementStart = time
                    SpiritMovementCachedOpt = None }
            else spiritState
        match spiritState.SpiritMovements.[spiritState.SpiritMovementIndex] with
        | Creep ->
            let v = target - position
            let n = Vector2.Normalize v
            let m = n * Constants.Field.SpiritWalkSpeed
            (m, spiritState)
        | Chase ->
            let v = target - position
            let n = Vector2.Normalize v
            let m = n * Constants.Field.SpiritRunSpeed
            (m, spiritState)
        | Scatter ->
            match spiritState.SpiritMovementCachedOpt with
            | None ->
                let v = target - position
                let n = Vector2.Normalize v
                let m = n * Constants.Field.SpiritWalkSpeed
                let spiritState = { spiritState with SpiritMovementCachedOpt = Some m }
                (m, spiritState)
            | Some v -> (v, spiritState)
        | Wander ->
            match spiritState.SpiritMovementCachedOpt with
            | None ->
                let r = Gen.randomd * Math.PI * 2.0
                let n = v2 (single (cos r)) (single (sin r))
                let m = n * Constants.Field.SpiritWalkSpeed
                let spiritState = { spiritState with SpiritMovementCachedOpt = Some m }
                (m, spiritState)
            | Some v -> (v, spiritState)

[<RequireQualifiedAccess>]
module Spirit =

    type [<ReferenceEquality; NoComparison>] Spirit =
        private
            { Bounds_ : Vector4
              SpiritType_ : SpiritType
              SpiritState_ : SpiritState }

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.Size = this.Bounds_.Size

        (* Local Properties *)
        member this.SpiritType = this.SpiritType_
        member this.SpiritState = this.SpiritState_

        static member advance time target (spirit : Spirit) =
            let (movement, state) = SpiritState.advance time spirit.Center target spirit.SpiritState
            { spirit with
                Bounds_ = spirit.Bounds_.Translate movement
                SpiritState_ = state }

        static member spawn time center spiritType spiritPattern =
            let r = Gen.randomd * Math.PI * 2.0
            let n = v2 (single (cos r)) (single (sin r))
            let p = center + n * Constants.Field.SpiritRadius
            let spiritState = { SpiritMovements = spiritPattern; SpiritMovementIndex = 0; SpiritMovementStart = time; SpiritMovementCachedOpt = None }
            { Bounds_ = v4Bounds p Constants.Field.SpiritOrbBlipSize; SpiritType_ = spiritType; SpiritState_ = spiritState }

        static member make bounds spiritType spiritState =
            { Bounds_ = bounds
              SpiritType_ = spiritType
              SpiritState_ = spiritState }

type Spirit = Spirit.Spirit