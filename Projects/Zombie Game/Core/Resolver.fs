// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Globalization
open System.Numerics
open System.Text
open Prime
open Nu

/// Resolves a scene file's statements - its palette, its layout, the props it places by points, walls and one another,
/// its creatures, arrivals and eyes - into the area it is.
[<RequireQualifiedAccess>]
module Resolver =

    (* Placement *)

    /// Fail on a prop or enemy of a scene, saying why.
    let private failPlaced sceneName name message =
        failwith ("'" + name + "' of scene '" + sceneName + "' " + message)

    /// A prop placed before another in a scene, where it stands and how it appears.
    let private findPlaced sceneName name (placed : Map<string, Framing * Appearance option>) other =
        match Map.tryFind other placed with
        | Some found -> found
        | None -> failPlaced sceneName name ("is placed by '" + other + "', which isn't a prop placed before it in the scene file.")

    /// How far an appearance reaches behind its origin and before it - nothing, for what isn't seen.
    let private getDepth appearanceOpt library =
        match appearanceOpt with
        | Some appearance -> Appearances.getDepth appearance library
        | None -> (0.0f, 0.0f)

    /// Where a prop or enemy stands and the way it faces, from its placement and the props placed before it.
    let private place sceneName (ground : GroundLaid) (placed : Map<string, Framing * Appearance option>) name appearanceOpt (placement : Placement) library =
        let (back, _) = getDepth appearanceOpt library
        let height = defaultArg placement.Height (match appearanceOpt with Some appearance -> Appearances.getHeight appearance library | None -> 0.0f)
        let frame =
            match (placement.At, placement.Against, placement.Before, placement.Above) with
            | (Some at, None, None, None) ->
                { Origin = v3 at.X 0.0f at.Y; Yaw = Math.DegreesToRadiansF placement.Facing; Scale = 1.0f; WallDistanceOpt = None }
            | (None, Some wall, None, None) ->
                let (face, yaw) = ground.WallFace wall placement.Along
                let distance = placement.Gap + back
                { Origin = face + Math.YawForward yaw * distance; Yaw = yaw; Scale = 1.0f; WallDistanceOpt = Some distance }
            | (None, None, Some other, None) ->
                let (otherFrame, otherAppearanceOpt) = findPlaced sceneName name placed other
                let (_, otherFront) = getDepth otherAppearanceOpt library
                let distance = otherFront + placement.Gap + back
                let origin = v3 otherFrame.Origin.X 0.0f otherFrame.Origin.Z + Math.YawForward otherFrame.Yaw * distance
                { Origin = origin; Yaw = otherFrame.Yaw; Scale = 1.0f; WallDistanceOpt = Option.map ((+) distance) otherFrame.WallDistanceOpt }
            | (None, None, None, Some other) ->
                let (otherFrame, _) = findPlaced sceneName name placed other
                { otherFrame with Origin = v3 otherFrame.Origin.X 0.0f otherFrame.Origin.Z; Scale = 1.0f }
            | (None, None, None, None) -> failPlaced sceneName name "isn't placed - give it one of At, Against, Before or Above."
            | _ -> failPlaced sceneName name "is placed more than one way - give it only one of At, Against, Before or Above."
        { frame with Origin = frame.Origin.WithY height }

    (* Resolution *)

    /// The name of a prop's appearance for the dump - a variant's name with that of the appearance it's of, or nothing,
    /// for what isn't seen.
    let private getAppearanceName appearanceOpt resolvedOpt =
        match (appearanceOpt, resolvedOpt) with
        | (Some (Variant name), Some resolved) -> name + " (" + Appearances.getName resolved + ")"
        | (_, Some resolved) -> Appearances.getName resolved
        | (_, None) -> "unseen"

    /// A prop as its builder built it, interacted with at a spot from its origin as it faces, and reached across its
    /// solid body from there.
    let private resolveProp name appearanceName (spot : Vector2) conditionOpt interactOpt (b : Builder) =
        let framing = b.Framing
        let rotation = Quaternion.CreateFromYawPitchRoll (framing.Yaw, 0.0f, 0.0f)
        let at = v3 framing.Origin.X 0.0f framing.Origin.Z + Vector3.Transform (v3 spot.X 0.0f spot.Y, rotation)
        { PropName = name; PropAppearanceName = appearanceName; PropFraming = framing
          PropSpot = at
          PropReach = b.Colliders |> Array.fold (fun reach collider -> max reach (Collider.getReach (Collider.toFloor at) collider)) 0.0f
          PropParts = b.Parts; PropLeaves = b.Leaves; PropColliders = b.Colliders; PropLights = b.Lights; PropConditionOpt = conditionOpt; PropInteractOpt = interactOpt }

    /// Fail on a scene, saying why.
    let private failScene (scene : SceneData) message =
        failwith ("Scene '" + scene.SceneName + "' " + message)

    /// A scene's statements with those of the scenes it includes spliced in where it includes them, and those there
    /// only while a condition holds each with its condition - all of its conditions, if they're nested.
    let rec private expand (scene : SceneData) seen conditionOpt (library : AppearancesLibrary) statements =
        statements |>
        List.collect (fun statement ->
            match statement with
            | Include name ->
                if List.contains name seen then failScene scene ("includes itself, by way of " + String.concat ", " (List.rev (name :: seen)) + ".")
                let included =
                    match Map.tryFind name library.Scenes with
                    | Some included -> included
                    | None -> failScene scene ("includes '" + name + "', which no scene file is named.")
                included.Statements |>
                List.filter (function Area _ -> false | Palette _ | Layout _ | Prop _ | Enemy _ | Include _ | Arrival _ | Camera _ | While _ -> true) |> // what the included scene is, the including one isn't
                expand scene (name :: seen) conditionOpt library
            | While (condition, statements) ->
                let condition = match conditionOpt with Some outer -> AllOf [outer; condition] | None -> condition
                expand scene seen (Some condition) library statements
            | Prop _ | Enemy _ -> [(conditionOpt, statement)]
            | Palette _ | Layout _ | Area _ | Arrival _ | Camera _ ->
                if conditionOpt.IsSome then failScene scene "has a statement other than a Prop or Enemy in a While, which holds only those."
                [(None, statement)])

    /// Resolve an area's scene into the area, its randomness keyed by a game seed.
    let resolve (seed : uint64) (scene : SceneData) library =
        // the statements of the scenes it includes, spliced in where it includes them, and those there only while a
        // condition holds, each with its condition
        let expanded = expand scene [scene.SceneName] None library scene.Statements
        let statements = List.map snd expanded

        // the palette - the colors the appearances files share, then every palette statement's, which take precedence - and
        // the one layout
        let palette =
            statements |>
            List.fold (fun palette statement ->
                match statement with
                | Palette colors -> Map.fold (fun palette name color -> Map.add name color palette) palette colors
                | Layout _ | Prop _ | Enemy _ | Include _ | Area _ | Arrival _ | Camera _ | While _ -> palette)
                library.SharedPalette
        let layout =
            match statements |> List.choose (function Layout layout -> Some layout | Palette _ | Prop _ | Enemy _ | Include _ | Area _ | Arrival _ | Camera _ | While _ -> None) with
            | [layout] -> layout
            | [] -> failScene scene "has no layout."
            | _ -> failScene scene "has more than one layout."

        // the doorways through its walls - every door set against one - where the layout's dado breaks
        let doorways =
            statements |>
            List.choose (function
                | Prop { Place = { Against = Some wall; Along = along }; Interact = Some { Cue = Enter enter } } -> Some (wall, along, enter.Style.Doorway.X)
                | Palette _ | Layout _ | Prop _ | Enemy _ | Include _ | Area _ | Arrival _ | Camera _ | While _ -> None)

        // the layout built
        let layoutBuilder = Builder (scene.SceneName, "Layout", { Origin = v3Zero; Yaw = 0.0f; Scale = 1.0f; WallDistanceOpt = None }, palette, seed)
        let (ground, layoutAppearanceName) =
            match layout with
            | Chamber chamber -> (Layouts.buildChamber chamber doorways layoutBuilder, nameof Chamber)
        let ceilingOpt =
            match layout with
            | Chamber chamber -> if chamber.Outdoors then None else Some Constants.Scenery.WallHeight

        // the props, each placed by the area or the props before it, and the enemies likewise
        let (props, enemies, _) =
            expanded |>
            List.fold (fun (props, enemies, placed) (conditionOpt, statement) ->
                match statement with
                | Prop prop ->
                    let name = prop.Name
                    if Map.containsKey name placed || name = "Layout" then failScene scene ("has more than one prop named '" + name + "'.")
                    let resolvedOpt = prop.Appearance |> Option.map (fun appearance -> Appearances.resolveVariant appearance library)
                    let frame = place scene.SceneName ground placed name resolvedOpt prop.Place library
                    let b = Builder (scene.SceneName, name, frame, palette, seed)
                    match resolvedOpt with Some resolved -> Appearances.build resolved b library | None -> ()
                    (resolveProp name (getAppearanceName prop.Appearance resolvedOpt) prop.Spot conditionOpt prop.Interact b :: props, enemies, Map.add name (frame, resolvedOpt) placed)
                | Enemy enemy ->
                    let frame = place scene.SceneName ground placed enemy.Name None enemy.Place library
                    let enemy =
                        { EnemyName = enemy.Name; EnemyCreatureType = enemy.Creature; EnemyPosition = frame.Origin; EnemyYaw = frame.Yaw
                          EnemyLying = enemy.Lying; EnemyConditionOpt = conditionOpt }
                    (props, enemy :: enemies, placed)
                | Palette _ | Layout _ | Include _ | Area _ | Arrival _ | Camera _ | While _ -> (props, enemies, placed))
                ([], [], Map.empty)
        let props = resolveProp "Layout" layoutAppearanceName v2Zero None None layoutBuilder :: List.rev props
        let enemies = List.rev enemies

        // what makes it an area, where one arrives in it other than through a door, and its eyes
        let area =
            match statements |> List.choose (function Area area -> Some area | Palette _ | Layout _ | Prop _ | Enemy _ | Include _ | Arrival _ | Camera _ | While _ -> None) with
            | [area] -> area
            | [] -> failScene scene "has no Area - only an area's scene is resolved."
            | _ -> failScene scene "has more than one Area."
        let arrivals = statements |> List.choose (function Arrival arrival -> Some arrival | Palette _ | Layout _ | Prop _ | Enemy _ | Include _ | Area _ | Camera _ | While _ -> None)
        let cameras =
            statements |>
            List.choose (function
                | Camera camera ->
                    match ceilingOpt with
                    | Some ceiling when camera.At.Y <> ceiling - Constants.Scenery.CameraCeilingDistance ->
                        failScene scene ("has a Camera at height " + string camera.At.Y + " - under its ceiling at " + string ceiling + ", every eye stands at " + string (ceiling - Constants.Scenery.CameraCeilingDistance) + ".")
                    | Some _ | None -> ()
                    Some
                        { CameraMin = camera.Zone.Min; CameraMax = camera.Zone.Min + camera.Zone.Size; CameraEye = camera.At
                          CameraTarget = camera.LookAt; CameraFieldOfView = Math.DegreesToRadiansF camera.FieldOfView }
                | Palette _ | Layout _ | Prop _ | Enemy _ | Include _ | Area _ | Arrival _ | While _ -> None)
        for (what, names) in [("arrival", arrivals |> List.map _.Name); ("enemy", enemies |> List.map _.EnemyName)] do
            match names |> List.countBy id |> List.tryFind (fun (_, count) -> count > 1) with
            | Some (name, _) -> failScene scene ("has more than one " + what + " named '" + name + "'.")
            | None -> ()

        Area.make scene.SceneName area ground props enemies arrivals cameras

    (* Checks *)

    /// The corners of the box bounding parts, as they're turned.
    let private getBounds (parts : Part seq) =
        let corners =
            [for part in parts do
                for x in [-0.5f; 0.5f] do
                    for y in [-0.5f; 0.5f] do
                        for z in [-0.5f; 0.5f] do
                            part.PartPosition + Vector3.Transform (v3 x y z * part.PartScale, part.PartRotation)]
        match corners with
        | [] -> (v3Zero, v3Zero)
        | _ -> (corners |> List.reduce (fun a b -> Vector3.Min (a, b)), corners |> List.reduce (fun a b -> Vector3.Max (a, b)))

    (* Dump *)

    /// A number for the dump, to three places.
    let private formatSingle (value : single) =
        let value = if abs value < 0.0005f then 0.0f else value // no negative zeroes
        value.ToString ("0.000", CultureInfo.InvariantCulture)

    /// A point on the ground for the dump.
    let private formatV2 (v : Vector2) =
        "[" + formatSingle v.X + " " + formatSingle v.Y + "]"

    /// A point for the dump.
    let private formatV3 (v : Vector3) =
        "[" + formatSingle v.X + " " + formatSingle v.Y + " " + formatSingle v.Z + "]"

    /// Add a line to a text.
    let private appendLine (text : StringBuilder) (line : string) =
        text.Append(line).Append('\n') |> ignore

    /// A text listing of where everything in an area landed - each prop's place, facing and bounds, and its colliders
    /// and lights - by the names its scene file gives them, to read and to compare before and after an edit.
    let dump (area : Area) =
        let text = StringBuilder ()
        appendLine text ("scene " + area.Scene)
        appendLine text ("  walk " + formatV2 area.WalkMin + " to " + formatV2 area.WalkMax)
        for arrival in area.Arrivals do
            appendLine text ("  arrival " + arrival.Name + " at " + formatV2 arrival.At + " facing " + formatSingle arrival.Facing)
        for camera in area.Cameras do
            appendLine text ("  camera watching " + formatV2 camera.CameraMin + " to " + formatV2 camera.CameraMax + " from " + formatV3 camera.CameraEye + " at " + formatV3 camera.CameraTarget)
        for enemy in area.Enemies do
            appendLine text ("  enemy " + enemy.EnemyName + " " + scstring enemy.EnemyCreatureType + " at " + formatV3 enemy.EnemyPosition + " facing " + formatSingle (Math.RadiansToDegreesF enemy.EnemyYaw) + (if enemy.EnemyLying then " lying" else ""))
            match enemy.EnemyConditionOpt with Some condition -> appendLine text ("    while " + scstring condition) | None -> ()
        for prop in area.Props do
            let frame = prop.PropFraming
            appendLine text ("prop " + prop.PropName + " " + prop.PropAppearanceName + " at " + formatV3 frame.Origin + " facing " + formatSingle (Math.RadiansToDegreesF frame.Yaw))
            match prop.PropConditionOpt with Some condition -> appendLine text ("  while " + scstring condition) | None -> ()
            match prop.PropInteractOpt with Some interact -> appendLine text ("  interacted with at " + formatV3 prop.PropSpot + " - " + scstring interact) | None -> ()
            let (min, max) = getBounds prop.PropParts
            appendLine text ("  parts " + string prop.PropParts.Length + " within " + formatV3 min + " to " + formatV3 max)
            for collider in prop.PropColliders do
                match collider with
                | CircleCollider (center, radius) -> appendLine text ("  circle " + formatV2 center + " radius " + formatSingle radius)
                | BoxCollider (center, halfExtents, angle) -> appendLine text ("  box " + formatV2 center + " half " + formatV2 halfExtents + " facing " + formatSingle (Math.RadiansToDegreesF angle))
            for light in prop.PropLights do
                appendLine text ("  light " + formatV3 light.LightPosition + " brightness " + formatSingle light.LightBrightness + " reach " + formatSingle light.LightReach + (if light.LightShadowed then " shadowed" else ""))
        string text
