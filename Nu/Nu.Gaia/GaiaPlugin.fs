// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Numerics
open FSharp.Reflection
open Prime
open ImGuiNET
open Nu

/// The default plugin used by Gaia when a plugin from a game is not utilized.
type GaiaPlugin () =
    inherit NuPlugin ()
    override this.ImGuiProcess world =

        Globals.World <- world

        let world = ()

        ImGui.DockSpaceOverViewport (ImGui.GetMainViewport (), ImGuiDockNodeFlags.PassthruCentralNode) |> ignore<uint>

        ImGui.Begin "Gaia" |> ignore<bool>
        ImGui.Text "Group:"
        ImGui.SameLine ()
        ImGui.Button "New" |> ignore<bool>
        ImGui.SameLine ()
        ImGui.Button "Delete" |> ignore<bool>
        ImGui.SameLine ()
        ImGui.Button "Save" |> ignore<bool>
        ImGui.SameLine ()
        ImGui.Button "Load" |> ignore<bool>
        ImGui.SameLine ()
        ImGui.Text "Entity:"
        ImGui.SameLine ()
        ImGui.Button "Create" |> ignore<bool>
        ImGui.SameLine ()
        let mutable item = 0
        let dispatchers = World.getEntityDispatchers Globals.World |> Map.toKeyArray
        ImGui.SetNextItemWidth 150.0f
        ImGui.Combo ("", &item, dispatchers, dispatchers.Length) |> ignore<bool>
        ImGui.SameLine ()
        ImGui.Text "w/ Overlay"
        ImGui.SameLine ()
        let mutable item2 = 0
        let overlays = Array.append [|"(Default Overlay)"; "(Routed Overlay)"; "(No Overlay)"|] (World.getOverlays Globals.World |> Map.toKeyArray)
        ImGui.SetNextItemWidth 150.0f
        ImGui.Combo ("", &item2, overlays, overlays.Length) |> ignore<bool>
        ImGui.SameLine ()
        ImGui.Button "Quick Size" |> ignore<bool>
        ImGui.SameLine ()
        if World.getHalted Globals.World
        then ImGui.Button "Run" |> ignore<bool>
        else ImGui.Button "Pause" |> ignore<bool>
        ImGui.SameLine ()
        ImGui.End ()

        ImGui.Begin "Hierarchy" |> ignore<bool>
        let entities =
            World.getEntitiesSovereign Gaia.selectedGroup Globals.World |>
            Seq.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder Globals.World), entity)) |>
            Array.ofSeq |>
            Array.sortBy fst |>
            Array.map snd
        for entity in entities do
            ImGui.TreeNode entity.Name |> ignore<bool>
            for child in entity.GetChildren Globals.World do
                ImGui.Indent ()
                ImGui.TreeNode child.Name |> ignore<bool>
                ImGui.Unindent ()
        ImGui.End ()

        // TODO: implement in order of priority -
        //
        //  option & voption with custom checkbox header
        //  Enums
        //  AssetTag w/ picking
        //  RenderStyle
        //  Substance
        //  SymbolicCompression
        //  TmxMap
        //  LightType
        //  MaterialProperties
        //
        //  Layout
        //  CollisionMask
        //  CollisionCategories
        //  CollisionDetection
        //  BodyShape
        //  JointDevice
        //  DateTimeOffset?
        //  Flag Enums
        ImGui.Begin "Properties" |> ignore<bool>
        match Globals.Form.entityPropertyGrid.SelectedObject with
        | null -> ()
        | :? EntityTypeDescriptorSource as entityTds ->
            let entity = entityTds.DescribedEntity
            let makePropertyDescriptor = fun (epv, tcas) -> (EntityPropertyDescriptor (epv, Array.map (fun attr -> attr :> Attribute) tcas)) :> System.ComponentModel.PropertyDescriptor
            let properties = PropertyDescriptor.getPropertyDescriptors<EntityState> makePropertyDescriptor (Some (entity, Globals.World))
            for property in properties do
                let ty = property.PropertyType
                let converter = SymbolicConverter ty
                let value = property.GetValue entityTds
                let valueStr = converter.ConvertToString value
                match value with
                | :? bool as b -> let mutable b' = b in if ImGui.Checkbox (property.DisplayName, &b') then property.SetValue (entityTds, b')
                | :? int8 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.DisplayName, &i') then property.SetValue (entityTds, int8 i')
                | :? uint8 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.DisplayName, &i') then property.SetValue (entityTds, uint8 i')
                | :? int16 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.DisplayName, &i') then property.SetValue (entityTds, int16 i')
                | :? uint16 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.DisplayName, &i') then property.SetValue (entityTds, uint16 i')
                | :? int32 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.DisplayName, &i') then property.SetValue (entityTds, int32 i')
                | :? uint32 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.DisplayName, &i') then property.SetValue (entityTds, uint32 i')
                | :? int64 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.DisplayName, &i') then property.SetValue (entityTds, int64 i')
                | :? uint64 as i -> let mutable i' = int32 i in if ImGui.DragInt (property.DisplayName, &i') then property.SetValue (entityTds, uint64 i')
                | :? single as f -> let mutable f' = single f in if ImGui.DragFloat (property.DisplayName, &f') then property.SetValue (entityTds, single f')
                | :? double as f -> let mutable f' = single f in if ImGui.DragFloat (property.DisplayName, &f') then property.SetValue (entityTds, double f')
                | :? Vector2 as v -> let mutable v' = v in if ImGui.DragFloat2 (property.DisplayName, &v') then property.SetValue (entityTds, v')
                | :? Vector3 as v -> let mutable v' = v in if ImGui.DragFloat3 (property.DisplayName, &v') then property.SetValue (entityTds, v')
                | :? Vector4 as v -> let mutable v' = v in if ImGui.DragFloat4 (property.DisplayName, &v') then property.SetValue (entityTds, v')
                | :? Vector2i as v -> let mutable v' = v in if ImGui.DragInt2 (property.DisplayName, &v'.X) then property.SetValue (entityTds, v')
                | :? Vector3i as v -> let mutable v' = v in if ImGui.DragInt3 (property.DisplayName, &v'.X) then property.SetValue (entityTds, v')
                | :? Vector4i as v -> let mutable v' = v in if ImGui.DragInt4 (property.DisplayName, &v'.X) then property.SetValue (entityTds, v')
                | :? Box2 as b ->
                    ImGui.Text property.DisplayName
                    let mutable min = v2 b.Min.X b.Min.Y
                    let mutable size = v2 b.Size.X b.Size.Y
                    ImGui.Indent ()
                    if  ImGui.DragFloat2 ("Min", &min) ||
                        ImGui.DragFloat2 ("Size", &size) then
                        let b' = box2 min size
                        property.SetValue (entityTds, b')
                    ImGui.Unindent ()
                | :? Box3 as b ->
                    ImGui.Text property.DisplayName
                    let mutable min = v3 b.Min.X b.Min.Y b.Min.Z
                    let mutable size = v3 b.Size.X b.Size.Y b.Size.Z
                    ImGui.Indent ()
                    if  ImGui.DragFloat3 ("Min", &min) ||
                        ImGui.DragFloat3 ("Size", &size) then
                        let b' = box3 min size
                        property.SetValue (entityTds, b')
                    ImGui.Unindent ()
                | :? Box2i as b ->
                    ImGui.Text property.DisplayName
                    let mutable min = v2i b.Min.X b.Min.Y
                    let mutable size = v2i b.Size.X b.Size.Y
                    ImGui.Indent ()
                    if  ImGui.DragInt2 ("Min", &min.X) ||
                        ImGui.DragInt2 ("Size", &size.X) then
                        let b' = box2i min size
                        property.SetValue (entityTds, b')
                    ImGui.Unindent ()
                | :? Quaternion as q ->
                    let mutable v = v4 q.X q.Y q.Z q.W
                    if ImGui.DragFloat4 (property.DisplayName, &v) then
                        let q' = quat v.X v.Y v.Z v.W
                        property.SetValue (entityTds, q')
                | :? Color as c ->
                    let mutable v = v4 c.R c.G c.B c.A
                    if ImGui.ColorEdit4 (property.DisplayName, &v) then
                        let c' = color v.X v.Y v.Z v.W
                        property.SetValue (entityTds, c')
                | _ ->
                    let mutable combo = false
                    if FSharpType.IsUnion ty then
                        let cases = FSharpType.GetUnionCases ty
                        if Array.forall (fun (case : UnionCaseInfo) -> Array.isEmpty (case.GetFields ())) cases then
                            combo <- true
                            let caseNames = Array.map (fun (case : UnionCaseInfo) -> case.Name) cases
                            let (unionCaseInfo, _) = FSharpValue.GetUnionFields (value, ty)
                            let mutable tag = unionCaseInfo.Tag
                            if ImGui.Combo (property.DisplayName, &tag, caseNames, caseNames.Length) then
                                let value' = FSharpValue.MakeUnion (cases.[tag], [||])
                                property.SetValue (entityTds, value')
                    if not combo then
                        let mutable valueStr' = valueStr
                        if ImGui.InputText (property.Name, &valueStr', 131072u) then
                            try let value' = converter.ConvertFromString valueStr'
                                property.SetValue (entityTds, value')
                            with _ -> ()
        | _ -> ()
        ImGui.End ()

        ImGui.Begin "Property Editor" |> ignore<bool>
        ImGui.End ()

        Globals.World