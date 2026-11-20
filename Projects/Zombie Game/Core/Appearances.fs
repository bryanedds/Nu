// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// Where a composed appearance's pieces go - offset, turned, sized and stretched within the prop's frame by the copies
/// they're in, its parts' sizes varied along each axis too.
type Placing =
    { Offset : Vector3
      Rotation : Quaternion
      Scale : single
      Stretch : Vector3 // how much its pieces' positions are stretched along each axis
      Vary : Vector3
      Yaw : single }

/// The scenes and the appearances files' variants, composed appearances and shared palette - what building a prop of an
/// appearance and resolving a scene read.
type AppearancesLibrary =
    { Scenes : Map<string, SceneData>
      Variants : Map<string, Appearance>
      ComposedAppearances : Map<string, ComposedData>
      SharedPalette : Map<string, Color> }

/// The appearances a prop can have, each built in its own frame from its attributes.
[<RequireQualifiedAccess>]
module Appearances =

    (* Composed Appearances *)

    /// A slab of a surface, solid to walkers or not. Its origin is beneath its middle.
    let private slab (data : SlabData) (builder : Builder) =
        builder.Part data.Surface PartBox (v3 0.0f (data.Size.Y * 0.5f) 0.0f) quatIdentity data.Size (builder.Color data.Tint)
        if data.Solid then builder.Box v2Zero (v2 (data.Size.X * 0.5f) (data.Size.Z * 0.5f))

    /// A point of a piece placed, stretched, scaled and turned.
    let private applyPlacing placing (point : Vector3) =
        placing.Offset + Vector3.Transform (point * placing.Stretch * placing.Scale, placing.Rotation)

    /// The rotation of a turn in degrees - yaw, pitch and roll.
    let private turnOf (degrees : Vector3) =
        Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadiansF degrees.X, Math.DegreesToRadiansF degrees.Y, Math.DegreesToRadiansF degrees.Z)

    /// Where the copy of an index stands in an arrangement of a count of them.
    let private getArranged arrangeOpt count index =
        match arrangeOpt with
        | Some (Row spacing) -> v3 ((single index - single (dec count) * 0.5f) * spacing) 0.0f 0.0f
        | Some (Around (radius, start)) ->
            let angle = Math.DegreesToRadiansF start + single index * MathF.Tau / single count
            v3 (cos angle * radius) 0.0f (sin angle * radius)
        | Some (Tiled (columns, spacing)) ->
            let columns = max 1 columns
            let rows = (count + columns - 1) / columns
            v3 ((single (index % columns) - single (dec columns) * 0.5f) * spacing) 0.0f ((single (index / columns) - single (dec rows) * 0.5f) * spacing)
        | Some (Fill _) -> v3 (-0.5f + (single index + 0.5f) / single count) 0.0f 0.0f
        | None -> v3Zero

    /// Build a part of a composed appearance, placed, sized and reaching out as its definition says.
    let private composePart (colorOf : string -> Color) placing (part : PartData) (builder : Builder) =
        let color = if part.Jitter > 0.0f then builder.Jitter (colorOf part.Color) part.Jitter else colorOf part.Color
        let size = part.Size * placing.Vary
        let size = if part.Stretches then size * placing.Stretch else size
        let at = if part.Standing then part.At + v3 0.0f (size.Y * 0.5f) 0.0f else part.At
        let at = applyPlacing placing at
        let size = size * placing.Scale
        let frame = builder.Framing
        let floor = -frame.Origin.Y / frame.Scale // the floor's height in the prop's frame
        let at = if part.FromFloor then v3 at.X (floor + part.At.Y) at.Z else at
        let reached =
            match part.Reaches with
            | Some DownToFloor ->
                let top = at.Y + size.Y * 0.5f
                Some (v3 at.X ((top + floor) * 0.5f) at.Z, v3 size.X (top - floor) size.Z)
            | Some (UpToHeight height) ->
                let bottom = at.Y - size.Y * 0.5f
                let top = floor + height / frame.Scale
                Some (v3 at.X ((bottom + top) * 0.5f) at.Z, v3 size.X (top - bottom) size.Z)
            | Some BackToWall ->
                match frame.WallDistanceOpt with
                | Some distance ->
                    let front = at.Z + size.Z * 0.5f
                    let wall = -distance / frame.Scale
                    Some (v3 at.X at.Y ((front + wall) * 0.5f), v3 size.X size.Y (front - wall))
                | None -> None // no wall behind it to reach back to, so it's left out
            | None -> Some (at, size)
        match reached with
        | Some (at, size) ->
            let rotation = Quaternion.Concatenate (turnOf part.Turn, placing.Rotation)
            let glow = match part.Surface with Light -> defaultArg part.Glow Constants.Scenery.PartGlow | _ -> 0.0f
            builder.AddPart part.Swings (not part.Flat) glow part.Powered false part.Surface part.Shape at rotation size color
        | None -> ()

    /// Build a composed appearance's piece - a part, a collider, a light, copies of pieces, or pieces by chance.
    let rec private composePiece (colorOf : string -> Color) (settings : ComposedSettings) placing piece (builder : Builder) =
        match piece with
        | Part part -> composePart colorOf placing part builder
        | SolidBox (center, halfExtents) ->
            let at = applyPlacing placing (v3 center.X 0.0f center.Y)
            builder.BoxTurned (v2 at.X at.Z) (halfExtents * v2 placing.Stretch.X placing.Stretch.Z * placing.Scale) placing.Yaw
        | SolidCircle (center, radius) ->
            let at = applyPlacing placing (v3 center.X 0.0f center.Y)
            builder.Circle (v2 at.X at.Z) (radius * min placing.Stretch.X placing.Stretch.Z * placing.Scale)
        | Shine shine ->
            builder.Light
                (applyPlacing placing shine.At) (colorOf shine.Color)
                (defaultArg settings.Brightness shine.Brightness) (defaultArg settings.Reach shine.Reach)
                (defaultArg settings.Shadowed shine.Shadowed) shine.Powered
        | Copies (copies, pieces) ->
            let arrange =
                match (copies.Arrange, settings.Columns) with
                | (Some (Tiled (_, spacing)), Some columns) -> Some (Tiled (columns, spacing))
                | (arrange, _) -> arrange
            let count =
                match (arrange, settings.Count) with
                | (Some (Fill spacing), _) -> max 1 (int (placing.Stretch.X / spacing))
                | (Some _, Some count) -> count
                | (_, _) -> defaultArg copies.Count 1
            let count = if copies.Extra > 0 then count + builder.RandomIndex (inc copies.Extra) else count
            for index in 0 .. dec count do
                let arranged = getArranged arrange count index
                let offset = arranged + v3 (builder.Range -copies.Spread.X copies.Spread.X) (builder.Range -copies.Spread.Y copies.Spread.Y) (builder.Range -copies.Spread.Z copies.Spread.Z)
                let grow = match copies.Grow with Some grow -> builder.Range grow.X grow.Y | None -> 1.0f
                let vary = v3 (1.0f + builder.Range -copies.Vary.X copies.Vary.X) (1.0f + builder.Range -copies.Vary.Y copies.Vary.Y) (1.0f + builder.Range -copies.Vary.Z copies.Vary.Z)
                let spin = if copies.Spin then builder.Random () * MathF.Tau else 0.0f
                let tilt = v3 (builder.Range -copies.Tilt.X copies.Tilt.X) (builder.Range -copies.Tilt.Y copies.Tilt.Y) (builder.Range -copies.Tilt.Z copies.Tilt.Z)
                let turn = Quaternion.CreateFromYawPitchRoll (spin + Math.DegreesToRadiansF tilt.X, Math.DegreesToRadiansF tilt.Y, Math.DegreesToRadiansF tilt.Z)
                let placing =
                    { Offset = applyPlacing placing offset
                      Rotation = Quaternion.Concatenate (turn, placing.Rotation)
                      Scale = placing.Scale * grow
                      Stretch = placing.Stretch
                      Vary = placing.Vary * vary
                      Yaw = placing.Yaw + spin }
                for piece in pieces do composePiece colorOf settings placing piece builder
        | Chance (chance, pieces) ->
            if builder.Random () < chance then
                for piece in pieces do composePiece colorOf settings placing piece builder

    /// A composed appearance, by its name in the appearances files.
    let private getComposed name (library : AppearancesLibrary) =
        match Map.tryFind name library.ComposedAppearances with
        | Some composed -> composed
        | None -> failwith ("No appearances file composes the appearance '" + name + "'.")

    /// The color of a composed appearance's slot - as the scene sets it by its palette's name, or as the appearance has
    /// it, or the scene palette's by the slot's own name.
    let private getSlotColor (settings : ComposedSettings) (composed : ComposedData) (builder : Builder) slot =
        match settings.Colors |> Option.bind (Map.tryFind slot) with
        | Some paletteName -> builder.Color paletteName
        | None ->
            match Map.tryFind slot composed.Colors with
            | Some (OwnColor color) -> color
            | Some (PaletteColor paletteName) -> builder.Color paletteName
            | None -> builder.Color slot

    /// Build a composed appearance - its colors by slot, as the scene sets them by its palette's names, or as the
    /// appearance has them, or the scene palette's by the slot's own name.
    let private compose name (settings : ComposedSettings) (builder : Builder) library =
        let composed = getComposed name library
        let placing = { Offset = v3Zero; Rotation = quatIdentity; Scale = 1.0f; Stretch = defaultArg settings.Stretch v3One; Vary = v3One; Yaw = 0.0f }
        for piece in composed.Pieces do composePiece (getSlotColor settings composed builder) settings placing piece builder

    (* Appearances *)

    /// The appearance a variant is of, following variants of variants from those already seen.
    let rec private resolveVariantFrom (variants : Map<string, Appearance>) seen appearance =
        match appearance with
        | Variant name ->
            if List.contains name seen then failwith ("The variant '" + name + "' is a variant of itself, by way of " + String.concat ", " (List.rev seen) + ".")
            match Map.tryFind name variants with
            | Some appearance -> resolveVariantFrom variants (name :: seen) appearance
            | None -> failwith ("No appearances file defines the variant '" + name + "'.")
        | _ -> appearance

    /// The appearance a variant is of, its attributes as its appearances file sets them - following variants of
    /// variants - or an appearance that's no variant as it is.
    let resolveVariant appearance (library : AppearancesLibrary) =
        resolveVariantFrom library.Variants [] appearance

    /// An appearance's name, as the scene file spells it.
    let getName appearance =
        match appearance with
        | Slab _ -> nameof Slab
        | Variant name -> name
        | Composed (name, _) -> name

    /// How far a prop of an appearance reaches behind its origin and before it, the way it faces - what sets it out
    /// from a wall it's against, and where a prop placed before it stands.
    let getDepth appearance library =
        match appearance with
        | Slab appearance -> (appearance.Size.Z * 0.5f, appearance.Size.Z * 0.5f)
        | Composed (name, settings) -> let depth = (getComposed name library).Depth * (defaultArg settings.Stretch v3One).Z in (depth.X, depth.Y)
        | Variant name -> failwith ("The variant '" + name + "' must be resolved to its appearance first.")

    /// How high above the floor a prop of an appearance stands where its placement doesn't say.
    let getHeight appearance library =
        match appearance with
        | Slab _ -> 0.0f
        | Composed (name, _) -> (getComposed name library).Height
        | Variant name -> failwith ("The variant '" + name + "' must be resolved to its appearance first.")

    /// Build a prop of an appearance.
    let build appearance (builder : Builder) library =
        match appearance with
        | Slab appearance -> slab appearance builder
        | Composed (name, settings) -> compose name settings builder library
        | Variant name -> failwith ("The variant '" + name + "' must be resolved to its appearance first.")
