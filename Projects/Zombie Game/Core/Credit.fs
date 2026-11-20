// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open Prime
open Nu

/// A line of the credits - a role, and a name credited with it. Lines of a role one after another show under one
/// heading.
type CreditData =
    { Role : string
      Name : string }

    /// The credits' roles in order, each with the names credited with it in a row.
    static member group (credits : CreditData list) =
        credits |>
        List.fold (fun groups credit ->
            match groups with
            | (role, names) :: rest when role = credit.Role -> (role, credit.Name :: names) :: rest
            | _ -> (credit.Role, [credit.Name]) :: groups)
            [] |>
        List.map (fun (role, names) -> (role, List.rev names)) |>
        List.rev