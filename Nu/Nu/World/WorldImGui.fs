// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open System.Reflection
open FSharp.NativeInterop
open FSharp.Reflection
open DotRecast.Recast
open ImGuiNET
open Prime

[<AutoOpen>]
module WorldImGui =

    type World with

        static member internal getImGui world =
            world.Subsystems.ImGui

        /// Render circles via ImGui in the current eye 2d space, computing color as specified.
        static member imGuiCircles2dPlus absolute (positions : Vector2 seq) radius filled (computeColor : Vector2 -> Color) world =
            let drawList = ImGui.GetBackgroundDrawList ()
            let eyeSize = World.getEye2dSize world
            let eyeCenter = World.getEye2dCenter world
            for position in positions do
                let color = computeColor position
                let positionWindow = ImGui.Position2dToWindow (absolute, eyeSize, eyeCenter, position)
                if filled
                then drawList.AddCircleFilled (positionWindow, radius, color.Abgr)
                else drawList.AddCircle (positionWindow, radius, color.Abgr)

        /// Render circles via ImGui in the current eye 2d space.
        static member imGuiCircles2d absolute position radius filled color world =
            World.imGuiCircles2dPlus absolute position radius filled (constant color) world

        /// Render a circle via ImGui in the current eye 3d space.
        static member imGuiCircle2d absolute position radius filled color world =
            World.imGuiCircles2d absolute (SArray.singleton position) radius filled color world

        /// Render segments via ImGui in the current eye 2d space, computing color as specified.
        static member imGuiSegments2dPlus absolute (segments : struct (Vector2 * Vector2) seq) thickness (computeColor : struct (Vector2 * Vector2) -> Color) world =
            let drawList = ImGui.GetBackgroundDrawList ()
            let eyeSize = World.getEye2dSize world
            let eyeCenter = World.getEye2dCenter world
            for struct (start, stop) in segments do
                let color = computeColor struct (start, stop)
                let startWindow = ImGui.Position2dToWindow (absolute, eyeSize, eyeCenter, start)
                let stopWindow = ImGui.Position2dToWindow (absolute, eyeSize, eyeCenter, stop)
                drawList.AddLine (startWindow, stopWindow, color.Abgr, thickness)

        /// Render segments via ImGui in the current eye 2d space.
        static member imGuiSegments2d absolute segments thickness color world =
            World.imGuiSegments2dPlus absolute segments thickness (constant color) world

        /// Render a segment via ImGui in the current eye 2d space.
        static member imGuiSegment2d absolute segment thickness color world =
            World.imGuiSegments2d absolute (SArray.singleton segment) thickness color world

        /// Render circles via ImGui in the current eye 3d space, computing color as specified.
        static member imGuiCircles3dPlus absolute (positions : Vector3 seq) radius filled (computeColor : Vector3 -> Color) world =
            let drawList = ImGui.GetBackgroundDrawList ()
            let windowPosition = ImGui.GetWindowPos ()
            let windowSize = ImGui.GetWindowSize ()
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let eyeFrustum = World.getEye3dFrustumView world
            let viewport = Constants.Render.Viewport
            let view = viewport.View3d (absolute, eyeCenter, eyeRotation)
            let projection = viewport.Projection3d
            let viewProjection = view * projection
            for position in positions do
                if eyeFrustum.Contains position = ContainmentType.Contains then
                    let color = computeColor position
                    let positionWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, position)
                    if filled
                    then drawList.AddCircleFilled (positionWindow, radius, color.Abgr)
                    else drawList.AddCircle (positionWindow, radius, color.Abgr)

        /// Render circles via ImGui in the current eye 3d space.
        static member imGuiCircles3d absolute position radius filled color world =
            World.imGuiCircles3dPlus absolute position radius filled (constant color) world

        /// Render a circle via ImGui in the current eye 3d space.
        static member imGuiCircle3d absolute position radius filled color world =
            World.imGuiCircles3d absolute (SArray.singleton position) radius filled color world

        /// Render segments via ImGui in the current eye 3d space, computing color as specified.
        static member imGuiSegments3dPlus absolute (segments : Segment3 seq) thickness (computeColor : Segment3 -> Color) world =
            let drawList = ImGui.GetBackgroundDrawList ()
            let windowPosition = ImGui.GetWindowPos ()
            let windowSize = ImGui.GetWindowSize ()
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let eyeFrustum = World.getEye3dFrustumView world
            let viewport = Constants.Render.Viewport
            let view = viewport.View3d (absolute, eyeCenter, eyeRotation)
            let projection = viewport.Projection3d
            let viewProjection = view * projection
            for segment in segments do
                match Math.TryUnionSegmentAndFrustum segment.A segment.B eyeFrustum with
                | Some (start, stop) ->
                    let color = computeColor segment
                    let startWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, start)
                    let stopWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, stop)
                    drawList.AddLine (startWindow, stopWindow, color.Abgr, thickness)
                | None -> ()

        /// Render segments via ImGui in the current eye 3d space.
        static member imGuiSegments3d absolute segments thickness color world =
            World.imGuiSegments3dPlus absolute segments thickness (constant color) world

        /// Render a segment via ImGui in the current eye 3d space.
        static member imGuiSegment3d absolute segment thickness color world =
            World.imGuiSegments3d absolute (SArray.singleton segment) thickness color world

        /// Edit an array value via ImGui.
        static member imGuiEditPropertyArray<'a> (editItem : (unit -> unit) -> string -> 'a -> bool * 'a) (defaultItemValue : 'a) itemsName (items : 'a array) =
            let mutable focused = false
            let mutable changed = false
            let items =
                if ImGui.SmallButton "+" then
                    changed <- true
                    Array.add defaultItemValue items
                else items
            if ImGui.IsAnyItemFocused () then focused <- true
            let items =
                ImGui.Indent ()
                let itemOpts =
                    let mutable i = 0
                    [|for item in items do
                        let itemName = itemsName + ".[" + string i + "]"
                        ImGui.PushID itemName
                        let itemOpt =
                            if not (ImGui.SmallButton "x") then
                                if ImGui.IsAnyItemFocused () then focused <- true
                                ImGui.SameLine ()
                                try let (changed', item) = editItem (fun () -> focused <- true) itemName item
                                    changed <- changed || changed'
                                    if ImGui.IsItemFocused () then focused <- true
                                    Some item
                                with _ -> Some item
                            else
                                focused <- true
                                changed <- true
                                None
                        ImGui.PopID ()
                        i <- inc i
                        itemOpt|]
                let items = Array.definitize itemOpts
                ImGui.Unindent ()
                items
            ImGui.PopID ()
            (focused, changed, items)

        /// Edit a list value via ImGui.
        static member imGuiEditPropertyList<'a> (editItem : (unit -> unit) -> string -> 'a -> bool * 'a) (defaultItemValue : 'a) itemsName (items : 'a list) =
            let mutable focused = false
            let mutable changed = false
            let items =
                if ImGui.SmallButton "+" then
                    changed <- true
                    items @ [defaultItemValue]
                else items
            if ImGui.IsAnyItemFocused () then focused <- true
            let items =
                ImGui.Indent ()
                let itemOpts =
                    let mutable i = 0
                    [for item in items do
                        let itemName = itemsName + ".[" + string i + "]"
                        ImGui.PushID itemName
                        let itemOpt =
                            if not (ImGui.SmallButton "x") then
                                if ImGui.IsAnyItemFocused () then focused <- true
                                ImGui.SameLine ()
                                try let (changed', item) = editItem (fun () -> focused <- true) itemName item
                                    changed <- changed || changed'
                                    if ImGui.IsItemFocused () then focused <- true
                                    Some item
                                with _ -> Some item
                            else
                                focused <- true
                                changed <- true
                                None
                        ImGui.PopID ()
                        i <- inc i
                        itemOpt]
                let items = List.definitize itemOpts
                ImGui.Unindent ()
                items
            ImGui.PopID ()
            (focused, changed, items)

        /// Edit a record value via ImGui.
        static member imGuiEditPropertyRecord searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup headered (name : string) (ty : Type) (value : obj) =
            if headered then
                ImGui.Text name
                ImGui.Indent ()
            ImGui.PushID name
            let mutable focused = false
            let mutable changed = false
            let fields =
                FSharpType.GetRecordFields (ty, true) |>
                Array.zip (FSharpValue.GetRecordFields (value, true)) |>
                Array.map (fun (field, fieldInfo : PropertyInfo) ->
                    let fieldName =
                        if ty.IsDefined (typeof<SymbolicExpansionAttribute>, true) then
                            let expansionAttribute = ty.GetCustomAttribute<SymbolicExpansionAttribute> true
                            if expansionAttribute.PrettifyFieldNames then
                                if fieldInfo.Name.EndsWith "_"
                                then fieldInfo.Name.Substring (0, dec fieldInfo.Name.Length)
                                else String.capitalize fieldInfo.Name
                            else fieldInfo.Name
                        else fieldInfo.Name
                    let (focused', changed', field) =
                        if  fieldInfo.PropertyType.Name <> typedefof<_ AssetTag>.Name &&
                            (FSharpType.IsRecord fieldInfo.PropertyType || FSharpType.isRecordAbstract fieldInfo.PropertyType) then
                            World.imGuiEditPropertyRecord searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup true fieldName fieldInfo.PropertyType field
                        else World.imGuiEditProperty searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup fieldName fieldInfo.PropertyType field
                    if focused' then focused <- true
                    if changed' then changed <- true
                    field)
            let value = FSharpValue.MakeRecord (ty, fields, true)
            if headered then ImGui.Unindent ()
            ImGui.PopID ()
            (focused, changed, value)

        /// Edit a value via ImGui.
        /// TODO: split up this function.
        static member imGuiEditProperty
            (searchAssetViewer : unit -> unit)
            (snapDrag : single)
            (valueStrPreviousRef : string ref)
            (dragDropPayloadOpt : string option)
            (selectedScreen : Screen)
            (selectedGroup : Group)
            (name : string)
            (ty : Type)
            (value : obj) =
            let converter = SymbolicConverter (false, None, ty)
            let mutable focused = false
            let (changed, value) =
                match value with
                | :? bool as b -> let mutable b = b in (ImGui.Checkbox (name, &b), b :> obj)
                | :? int8 as i -> let mutable i = int32 i in (ImGui.DragInt (name, &i), int8 i :> obj)
                | :? uint8 as i -> let mutable i = int32 i in (ImGui.DragInt (name, &i), uint8 i :> obj)
                | :? int16 as i -> let mutable i = int32 i in (ImGui.DragInt (name, &i), int16 i :> obj)
                | :? uint16 as i -> let mutable i = int32 i in (ImGui.DragInt (name, &i), uint16 i :> obj)
                | :? int32 as i -> let mutable i = int32 i in (ImGui.DragInt (name, &i), int32 i :> obj)
                | :? uint32 as i -> let mutable i = int32 i in (ImGui.DragInt (name, &i), uint32 i :> obj)
                | :? int64 as i -> let mutable i = int32 i in (ImGui.DragInt (name, &i), int64 i :> obj)
                | :? uint64 as i -> let mutable i = int32 i in (ImGui.DragInt (name, &i), uint64 i :> obj)
                | :? single as f -> let mutable f = single f in (ImGui.DragFloat (name, &f, snapDrag), single f :> obj)
                | :? double as f -> let mutable f = single f in (ImGui.DragFloat (name, &f, snapDrag), double f :> obj)
                | :? Vector2 as v -> let mutable v = v in (ImGui.DragFloat2 (name, &v, snapDrag), v :> obj)
                | :? Vector3 as v -> let mutable v = v in (ImGui.DragFloat3 (name, &v, snapDrag), v :> obj)
                | :? Vector4 as v -> let mutable v = v in (ImGui.DragFloat4 (name, &v, snapDrag), v :> obj)
                | :? Vector2i as v -> let mutable v = v in (ImGui.DragInt2 (name, &v.X, snapDrag), v :> obj)
                | :? Vector3i as v -> let mutable v = v in (ImGui.DragInt3 (name, &v.X, snapDrag), v :> obj)
                | :? Vector4i as v -> let mutable v = v in (ImGui.DragInt4 (name, &v.X, snapDrag), v :> obj)
                | :? Box2 as b ->
                    ImGui.Text name
                    ImGui.PushID name
                    ImGui.Indent ()
                    let mutable min = v2 b.Min.X b.Min.Y
                    let mutable size = v2 b.Size.X b.Size.Y
                    let minChanged = ImGui.DragFloat2 (nameof b.Min, &min, snapDrag)
                    if ImGui.IsAnyItemFocused () then focused <- true
                    let sizeChanged = ImGui.DragFloat2 (nameof b.Size, &size, snapDrag)
                    ImGui.Unindent ()
                    (minChanged || sizeChanged, box2 min size :> obj)
                | :? Box3 as b ->
                    ImGui.Text name
                    ImGui.PushID name
                    ImGui.Indent ()
                    let mutable min = v3 b.Min.X b.Min.Y b.Min.Z
                    let mutable size = v3 b.Size.X b.Size.Y b.Size.Z
                    let minChanged = ImGui.DragFloat3 (nameof b.Min, &min, snapDrag)
                    if ImGui.IsAnyItemFocused () then focused <- true
                    let sizeChanged = ImGui.DragFloat3 (nameof b.Size, &size, snapDrag)
                    ImGui.Unindent ()
                    (minChanged || sizeChanged, box3 min size :> obj)
                | :? Box2i as b ->
                    ImGui.Text name
                    ImGui.PushID name
                    ImGui.Indent ()
                    let mutable min = v2i b.Min.X b.Min.Y
                    let mutable size = v2i b.Size.X b.Size.Y
                    let minChanged = ImGui.DragInt2 (nameof b.Min, &min.X, snapDrag)
                    if ImGui.IsAnyItemFocused () then focused <- true
                    let sizeChanged = ImGui.DragInt2 (nameof b.Size, &size.X, snapDrag)
                    ImGui.Unindent ()
                    (minChanged || sizeChanged, box2i min size :> obj)
                | :? Box3i as b ->
                    ImGui.Text name
                    ImGui.PushID name
                    ImGui.Indent ()
                    let mutable min = v3i b.Min.X b.Min.Y b.Min.Z
                    let mutable size = v3i b.Size.X b.Size.Y b.Size.Z
                    let minChanged = ImGui.DragInt3 (nameof b.Min, &min.X, snapDrag)
                    if ImGui.IsAnyItemFocused () then focused <- true
                    let sizeChanged = ImGui.DragInt3 (nameof b.Size, &size.X, snapDrag)
                    ImGui.Unindent ()
                    (minChanged || sizeChanged, box3i min size :> obj)
                | :? Quaternion as q ->
                    let mutable v = v4 q.X q.Y q.Z q.W
                    (ImGui.DragFloat4 (name, &v, snapDrag), quat v.X v.Y v.Z v.W :> obj)
                | :? Frustum as frustum ->
                    let mutable frustumStr = string frustum
                    (ImGui.InputText (name, &frustumStr, 4096u, ImGuiInputTextFlags.ReadOnly), frustum :> obj)
                | :? Color as c ->
                    let mutable v = v4 c.R c.G c.B c.A
                    (ImGui.ColorEdit4 (name, &v), color v.X v.Y v.Z v.W :> obj)
                | :? Transition as transition ->
                    let (focused', changed, transition) = World.imGuiEditPropertyRecord searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup true name (typeof<Transition>) transition
                    if focused' then focused <- true
                    (changed, transition)
                | :? Slide as slide ->
                    let (focused', changed, slide) = World.imGuiEditPropertyRecord searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup true name (typeof<Slide>) slide
                    if focused' then focused <- true
                    (changed, slide)
                | :? RenderStyle as style ->
                    let mutable index = match style with Deferred -> 0 | Forward _ -> 1
                    let (changed, style) =
                        if ImGui.Combo (name, &index, [|nameof Deferred; nameof Forward|], 2)
                        then (true, match index with 0 -> Deferred | 1 -> Forward (0.0f, 0.0f) | _ -> failwithumf ())
                        else (false, style)
                    if ImGui.IsAnyItemFocused () then focused <- true
                    let (changed, style) =
                        match index with
                        | 0 -> (changed, style)
                        | 1 ->
                            match style with
                            | Deferred -> failwithumf ()
                            | Forward (subsort, sort) ->
                                let mutable (subsort, sort) = (subsort, sort)
                                ImGui.Indent ()
                                let subsortChanged = ImGui.DragFloat ("Subsort via " + name, &subsort, snapDrag)
                                if ImGui.IsAnyItemFocused () then focused <- true
                                let sortChanged = ImGui.DragFloat ("Sort via " + name, &sort, snapDrag)
                                ImGui.Unindent ()
                                (changed || subsortChanged || sortChanged, Forward (subsort, sort))
                        | _ -> failwithumf ()
                    (changed, style :> obj)
                | :? LightType as light ->
                    let mutable index = match light with PointLight -> 0 | DirectionalLight -> 1 | SpotLight _ -> 2
                    let (changed, light) =
                        if ImGui.Combo (name, &index, [|nameof PointLight; nameof DirectionalLight; nameof SpotLight|], 3)
                        then (true, match index with 0 -> PointLight | 1 -> DirectionalLight | 2 -> SpotLight (0.9f, 1.0f) | _ -> failwithumf ())
                        else (false, light)
                    if ImGui.IsAnyItemFocused () then focused <- true
                    let (changed, light) =
                        match index with
                        | 0 -> (changed, light)
                        | 1 -> (changed, light)
                        | 2 ->
                            match light with
                            | PointLight -> failwithumf ()
                            | DirectionalLight -> failwithumf ()
                            | SpotLight (innerCone, outerCone) ->
                                let mutable (innerCone, outerCone) = (innerCone, outerCone)
                                ImGui.Indent ()
                                let innerConeChanged = ImGui.DragFloat ("InnerCone via " + name, &innerCone, snapDrag)
                                if ImGui.IsAnyItemFocused () then focused <- true
                                let outerConeChanged = ImGui.DragFloat ("OuterCone via " + name, &outerCone, snapDrag)
                                ImGui.Unindent ()
                                (changed || innerConeChanged || outerConeChanged, SpotLight (innerCone, outerCone))
                        | _ -> failwithumf ()
                    (changed, light :> obj)
                | :? Substance as substance ->
                    let mutable scalar = match substance with Mass m -> m | Density d -> d
                    let changed = ImGui.DragFloat ("##scalar via " + name, &scalar, snapDrag)
                    if ImGui.IsAnyItemFocused () then focused <- true
                    let mutable index = match substance with Mass _ -> 0 | Density _ -> 1
                    ImGui.SameLine ()
                    if ImGui.Combo (name, &index, [|nameof Mass; nameof Density|], 2) || changed then
                        let substance = match index with 0 -> Mass scalar | 1 -> Density scalar | _ -> failwithumf ()
                        (true, substance :> obj)
                    else (false, substance :> obj)
                | :? Animation as animation ->
                    let (focused', changed, animation) = World.imGuiEditPropertyRecord searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup true name (typeof<Animation>) animation
                    if focused' then focused <- true
                    (changed, animation)
                | :? TerrainMaterialProperties as tmps ->
                    let (focused', changed, tmps) = World.imGuiEditPropertyRecord searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup true name (typeof<TerrainMaterialProperties>) tmps
                    if focused' then focused <- true
                    (changed, tmps)
                | :? MaterialProperties as mps ->
                    let (focused', changed, mps) = World.imGuiEditPropertyRecord searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup false name (typeof<MaterialProperties>) mps
                    if focused' then focused <- true
                    (changed, mps)
                | :? Material as material ->
                    let (focused', changed, material) = World.imGuiEditPropertyRecord searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup false name (typeof<Material>) material
                    if focused' then focused <- true
                    (changed, material)
                | :? Lighting3dConfig as lighting3dConfig ->
                    let mutable lighting3dChanged = false
                    let mutable lightCutoffMargin = lighting3dConfig.LightCutoffMargin
                    let mutable lightShadowExponent = lighting3dConfig.LightShadowExponent
                    let mutable lightShadowDensity = lighting3dConfig.LightShadowDensity
                    let mutable ssaoIntensity = lighting3dConfig.SsaoIntensity
                    let mutable ssaoBias = lighting3dConfig.SsaoBias
                    let mutable ssaoRadius = lighting3dConfig.SsaoRadius
                    let mutable ssaoDistanceMax = lighting3dConfig.SsaoDistanceMax
                    let mutable ssvfEnabled = lighting3dConfig.SsvfEnabled
                    let mutable ssvfSteps = lighting3dConfig.SsvfSteps
                    let mutable ssvfAsymmetry = lighting3dConfig.SsvfAsymmetry
                    let mutable ssvfIntensity = lighting3dConfig.SsvfIntensity
                    let mutable ssrEnabled = lighting3dConfig.SsrEnabled
                    let mutable ssrDetail = lighting3dConfig.SsrDetail
                    let mutable ssrRefinementsMax = lighting3dConfig.SsrRefinementsMax
                    let mutable ssrRayThickness = lighting3dConfig.SsrRayThickness
                    let mutable ssrTowardEyeCutoff = lighting3dConfig.SsrTowardEyeCutoff
                    let mutable ssrDepthCutoff = lighting3dConfig.SsrDepthCutoff
                    let mutable ssrDepthCutoffMargin = lighting3dConfig.SsrDepthCutoffMargin
                    let mutable ssrDistanceCutoff = lighting3dConfig.SsrDistanceCutoff
                    let mutable ssrDistanceCutoffMargin = lighting3dConfig.SsrDistanceCutoffMargin
                    let mutable ssrRoughnessCutoff = lighting3dConfig.SsrRoughnessCutoff
                    let mutable ssrRoughnessCutoffMargin = lighting3dConfig.SsrRoughnessCutoffMargin
                    let mutable ssrSlopeCutoff = lighting3dConfig.SsrSlopeCutoff
                    let mutable ssrSlopeCutoffMargin = lighting3dConfig.SsrSlopeCutoffMargin
                    let mutable ssrEdgeHorizontalMargin = lighting3dConfig.SsrEdgeHorizontalMargin
                    let mutable ssrEdgeVerticalMargin = lighting3dConfig.SsrEdgeVerticalMargin
                    let mutable ssrLightColor = let color = lighting3dConfig.SsrLightColor in color.Vector4
                    let mutable ssrLightBrightness = lighting3dConfig.SsrLightBrightness
                    lighting3dChanged <- ImGui.SliderFloat ("Light Cutoff Margin", &lightCutoffMargin, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Light Shadow Exponent", &lightShadowExponent, 0.0f, 87.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Light Shadow Density", &lightShadowDensity, 0.0f, 32.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssao Intensity", &ssaoIntensity, 0.0f, 10.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssao Bias", &ssaoBias, 0.0f, 0.1f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssao Radius", &ssaoRadius, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssao Distance Max", &ssaoDistanceMax, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.Checkbox ("Ssvf Enabled", &ssvfEnabled) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderInt ("Ssvf Steps", &ssvfSteps, 0, 128) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssvf Asymmetry", &ssvfAsymmetry, -1.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssvf Intensity", &ssvfIntensity, 0.0f, 10.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.Checkbox ("Ssr Enabled", &ssrEnabled) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Detail", &ssrDetail, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderInt ("Ssr Refinements Max", &ssrRefinementsMax, 0, 32) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Ray Thickness", &ssrRayThickness, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Toward Eye Cutoff", &ssrTowardEyeCutoff, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Depth Cutoff", &ssrDepthCutoff, 0.0f, 128.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Depth Cutoff Margin", &ssrDepthCutoffMargin, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Distance Cutoff", &ssrDistanceCutoff, 0.0f, 128.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Distance Cutoff Margin", &ssrDistanceCutoffMargin, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Roughness Cutoff", &ssrRoughnessCutoff, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Roughness Cutoff Margin", &ssrRoughnessCutoffMargin, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Slope Cutoff", &ssrSlopeCutoff, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Slope Cutoff Margin", &ssrSlopeCutoffMargin, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Edge Horizontal Margin", &ssrEdgeHorizontalMargin, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Edge Vertical Margin", &ssrEdgeVerticalMargin, 0.0f, 1.0f) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.ColorEdit4 ("Ssr Light Color", &ssrLightColor) || lighting3dChanged; if ImGui.IsItemFocused () then focused <- true
                    lighting3dChanged <- ImGui.SliderFloat ("Ssr Light Brightness", &ssrLightBrightness, 0.0f, 32.0f) || lighting3dChanged
                    if lighting3dChanged then
                        let lighting3dConfig =
                            { LightCutoffMargin = lightCutoffMargin
                              LightShadowExponent = lightShadowExponent
                              LightShadowDensity = lightShadowDensity
                              SsaoIntensity = ssaoIntensity
                              SsaoBias = ssaoBias
                              SsaoRadius = ssaoRadius
                              SsaoDistanceMax = ssaoDistanceMax
                              SsvfEnabled = ssvfEnabled
                              SsvfSteps = ssvfSteps
                              SsvfAsymmetry = ssvfAsymmetry
                              SsvfIntensity = ssvfIntensity
                              SsrEnabled = ssrEnabled
                              SsrDetail = ssrDetail
                              SsrRefinementsMax = ssrRefinementsMax
                              SsrRayThickness = ssrRayThickness
                              SsrTowardEyeCutoff = ssrTowardEyeCutoff
                              SsrDepthCutoff = ssrDepthCutoff
                              SsrDepthCutoffMargin = ssrDepthCutoffMargin
                              SsrDistanceCutoff = ssrDistanceCutoff
                              SsrDistanceCutoffMargin = ssrDistanceCutoffMargin
                              SsrRoughnessCutoff = ssrRoughnessCutoff
                              SsrRoughnessCutoffMargin = ssrRoughnessCutoffMargin
                              SsrSlopeCutoff = ssrSlopeCutoff
                              SsrSlopeCutoffMargin = ssrSlopeCutoffMargin
                              SsrEdgeHorizontalMargin = ssrEdgeHorizontalMargin
                              SsrEdgeVerticalMargin = ssrEdgeVerticalMargin
                              SsrLightColor = Color ssrLightColor
                              SsrLightBrightness = ssrLightBrightness }
                        (true, lighting3dConfig)
                    else (false, lighting3dConfig)
                | :? Nav3dConfig as nav3dConfig ->
                    let mutable nav3dConfigChanged = false
                    let mutable cellSize = nav3dConfig.CellSize
                    let mutable cellHeight = nav3dConfig.CellHeight
                    let mutable agentHeight = nav3dConfig.AgentHeight
                    let mutable agentRadius = nav3dConfig.AgentRadius
                    let mutable agentClimbMax = nav3dConfig.AgentClimbMax
                    let mutable agentSlopeMax = nav3dConfig.AgentSlopeMax
                    let mutable regionSizeMin = nav3dConfig.RegionSizeMin
                    let mutable regionSizeMerge = nav3dConfig.RegionSizeMerge
                    let mutable edgeLengthMax = nav3dConfig.EdgeLengthMax
                    let mutable edgeErrorMax = nav3dConfig.EdgeErrorMax
                    let mutable vertsPerPolygon = nav3dConfig.VertsPerPolygon
                    let mutable detailSampleDistance = nav3dConfig.DetailSampleDistance
                    let mutable detailSampleErrorMax = nav3dConfig.DetailSampleErrorMax
                    let mutable filterLowHangingObstacles = nav3dConfig.FilterLowHangingObstacles
                    let mutable filterLedgeSpans = nav3dConfig.FilterLedgeSpans
                    let mutable filterWalkableLowHeightSpans = nav3dConfig.FilterWalkableLowHeightSpans
                    let mutable partitionTypeStr = scstring nav3dConfig.PartitionType
                    if ImGui.SliderFloat ("CellSize", &cellSize, 0.01f, 1.0f, "%.2f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("CellHeight", &cellHeight, 0.01f, 1.0f, "%.2f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("AgentHeight", &agentHeight, 0.1f, 5.0f, "%.2f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("AgentRadius", &agentRadius, 0.0f, 5.0f, "%.2f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("AgentClimbMax", &agentClimbMax, 0.1f, 5.0f, "%.2f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("AgentSlopeMax", &agentSlopeMax, 1.0f, 90.0f, "%.0f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderInt ("RegionSizeMin", &regionSizeMin, 1, 150) then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderInt ("RegionSizeMerge", &regionSizeMerge, 1, 150) then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("EdgeLengthMax", &edgeLengthMax, 0.0f, 50.0f, "%.1f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("EdgeErrorMax", &edgeErrorMax, 0.1f, 3f, "%.1f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderInt ("VertPerPoly", &vertsPerPolygon, 3, 12) then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("DetailSampleDistance", &detailSampleDistance, 0.0f, 16.0f, "%.1f") then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.SliderFloat ("DetailSampleErrorMax", &detailSampleErrorMax, 0.0f, 16.0f, "%.1f") then nav3dConfigChanged <- true        
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.Checkbox ("FilterLowHangingObstacles", &filterLowHangingObstacles) then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.Checkbox ("FilterLedgeSpans", &filterLedgeSpans) then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.Checkbox ("FilterWalkableLowHeightSpans", &filterWalkableLowHeightSpans) then nav3dConfigChanged <- true
                    if ImGui.IsItemFocused () then focused <- true
                    if ImGui.BeginCombo ("ParitionType", partitionTypeStr, ImGuiComboFlags.HeightLarge) then
                        let partitionTypeStrs = Array.map (fun (ptv : RcPartitionType) -> ptv.Name) RcPartitionType.Values
                        for partitionTypeStr' in partitionTypeStrs do
                            if ImGui.Selectable (partitionTypeStr', strEq partitionTypeStr' partitionTypeStr) then
                                if strNeq partitionTypeStr partitionTypeStr' then
                                    partitionTypeStr <- partitionTypeStr'
                                    nav3dConfigChanged <- true
                        ImGui.EndCombo ()
                    if ImGui.IsItemFocused () then focused <- true
                    if nav3dConfigChanged then
                        let nav3dConfig =
                            { CellSize = cellSize
                              CellHeight = cellHeight
                              AgentHeight = agentHeight
                              AgentRadius = agentRadius
                              AgentClimbMax = agentClimbMax
                              AgentSlopeMax = agentSlopeMax
                              RegionSizeMin = regionSizeMin
                              RegionSizeMerge = regionSizeMerge
                              EdgeLengthMax = edgeLengthMax
                              EdgeErrorMax = edgeErrorMax
                              VertsPerPolygon = vertsPerPolygon
                              DetailSampleDistance = detailSampleDistance
                              DetailSampleErrorMax = detailSampleErrorMax
                              FilterLowHangingObstacles = filterLowHangingObstacles
                              FilterLedgeSpans = filterLedgeSpans
                              FilterWalkableLowHeightSpans = filterWalkableLowHeightSpans
                              PartitionType = scvalue partitionTypeStr }
                        (true, nav3dConfig)
                    else (false, nav3dConfig)
                | :? (Animation array) as animations ->
                    ImGui.Text name
                    ImGui.SameLine ()
                    ImGui.PushID name
                    let (focused', changed, animations) =
                        World.imGuiEditPropertyArray
                            (fun focusProperty name animation ->
                                let (focused, changed, animation) = World.imGuiEditProperty searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup name (typeof<Animation>) animation
                                if focused then focusProperty ()
                                (changed, animation :?> Animation))
                            { StartTime = GameTime.zero; LifeTimeOpt = None; Name = "Armature"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None }
                            name
                            animations
                    if focused' then focused <- true
                    ImGui.PopID ()
                    (changed, animations)
                | _ ->
                    let mutable combo = false
                    let (changed, value) =
                        if FSharpType.IsUnion ty then
                            let cases = FSharpType.GetUnionCases ty
                            if Array.forall (fun (case : UnionCaseInfo) -> Array.isEmpty (case.GetFields ())) cases then
                                combo <- true
                                let caseNames = Array.map (fun (case : UnionCaseInfo) -> case.Name) cases
                                let (unionCaseInfo, _) = FSharpValue.GetUnionFields (value, ty)
                                let mutable tag = unionCaseInfo.Tag
                                if ImGui.Combo (name, &tag, caseNames, caseNames.Length) then
                                    (true, FSharpValue.MakeUnion (cases.[tag], [||]))
                                else (false, value)
                            else (false, value)
                        else (false, value)
                    if not combo then
                        if  ty.IsGenericType &&
                            ty.GetGenericTypeDefinition () = typedefof<_ option> &&
                            (not ty.GenericTypeArguments.[0].IsGenericType || ty.GenericTypeArguments.[0].GetGenericTypeDefinition () <> typedefof<_ option>) &&
                            (not ty.GenericTypeArguments.[0].IsGenericType || ty.GenericTypeArguments.[0].GetGenericTypeDefinition () <> typedefof<_ voption>) &&
                            ty.GenericTypeArguments.[0] <> typeof<MaterialProperties> &&
                            ty.GenericTypeArguments.[0] <> typeof<Material> &&
                            (ty.GenericTypeArguments.[0].IsValueType ||
                             ty.GenericTypeArguments.[0] = typeof<string> ||
                             ty.GenericTypeArguments.[0] = typeof<Slide> ||
                             ty.GenericTypeArguments.[0] = typeof<Image AssetTag> ||
                             ty.GenericTypeArguments.[0] = typeof<Font AssetTag> ||
                             ty.GenericTypeArguments.[0] = typeof<TileMap AssetTag> ||
                             ty.GenericTypeArguments.[0] = typeof<CubeMap AssetTag> ||
                             ty.GenericTypeArguments.[0] = typeof<Sound AssetTag> ||
                             ty.GenericTypeArguments.[0] = typeof<Song AssetTag> ||
                             ty.GenericTypeArguments.[0] = typeof<StaticModel AssetTag> ||
                             ty.GenericTypeArguments.[0] = typeof<AnimatedModel AssetTag> ||
                             ty.GenericTypeArguments.[0] = typeof<SoundDescriptor> ||
                             ty.GenericTypeArguments.[0] = typeof<SongDescriptor> ||
                             ty.GenericTypeArguments.[0] = typeof<Entity> ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ array>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ list>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FList>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FQueue>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FDeque>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Set>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FSet>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<Map<_, _>>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<FMap<_, _>>) ||
                             (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Relation>) ||
                             ty.GenericTypeArguments.[0] |> FSharpType.isNullTrueValue) then
                            let mutable isSome = ty.GetProperty("IsSome").GetValue(null, [|value|]) :?> bool
                            let (changed, value) =
                                if ImGui.Checkbox ("##" + name, &isSome) then
                                    if isSome then
                                        if ty.GenericTypeArguments.[0].IsValueType then
                                            if ty.GenericTypeArguments.[0] = typeof<Color> then
                                                (true, Activator.CreateInstance (ty, [|colorOne :> obj|]))
                                            elif ty.GenericTypeArguments.[0].Name = typedefof<_ AssetTag>.Name then
                                                (true, Activator.CreateInstance (ty, [|Activator.CreateInstance (ty.GenericTypeArguments.[0], [|""; ""|])|]))
                                            else (true, Activator.CreateInstance (ty, [|Activator.CreateInstance ty.GenericTypeArguments.[0]|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<string> then (true, Activator.CreateInstance (ty, [|"" :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Slide> then (true, Activator.CreateInstance (ty, [|{ IdlingTime = GameTime.zero; Destination = selectedScreen } :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Image AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.Image :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Font AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.Font :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<TileMap AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.TileMap :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<CubeMap AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.SkyBoxMap :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Sound AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.Sound :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Song AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.Song :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<StaticModel AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.StaticModel :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<AnimatedModel AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.AnimatedModel :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<SoundDescriptor> then (true, Activator.CreateInstance (ty, [|{ Volume = Constants.Audio.SongVolumeDefault; Sound = Assets.Default.Sound } :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<SongDescriptor> then (true, Activator.CreateInstance (ty, [|{ FadeInTime = GameTime.zero; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = GameTime.zero; RepeatLimitOpt = None; Volume = Constants.Audio.SongVolumeDefault; Song = Assets.Default.Song } :> obj|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ array> then (true, Activator.CreateInstance (ty, [|Reflection.objsToArray ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ list> then (true, Activator.CreateInstance (ty, [|Reflection.objsToList ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FList> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FList>.Name ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FQueue> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FQueue>.Name ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FDeque> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FDeque>.Name ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Set> then (true, Activator.CreateInstance (ty, [|Reflection.objsToSet ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FSet> then (true, Activator.CreateInstance (ty, [|Reflection.objsToFSet ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<Map<_, _>> then (true, Activator.CreateInstance (ty, [|Reflection.pairsToMap ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<FMap<_, _>> then (true, Activator.CreateInstance (ty, [|Reflection.pairsToFMap ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Relation> then
                                            let relationType = ty.GenericTypeArguments.[0]
                                            let makeFromStringFunction = relationType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
                                            let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((relationType.GetGenericArguments ()).[0])
                                            let relationValue = makeFromStringFunctionGeneric.Invoke (null, [|"???"|])
                                            (true, Activator.CreateInstance (ty, [|relationValue|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Entity> then
                                            (true, Activator.CreateInstance (ty, [|Nu.Entity (Array.add "???" selectedGroup.Names) :> obj|]))
                                        elif FSharpType.isNullTrueValue ty.GenericTypeArguments.[0] then
                                            (true, Activator.CreateInstance (ty, [|null|]))
                                        else (false, value)
                                    else (true, None)
                                else (false, value)
                            focused <- ImGui.IsItemFocused ()
                            if isSome then
                                ImGui.SameLine ()
                                ImGui.PushID name
                                let (focused', changed', value') = World.imGuiEditProperty searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup name ty.GenericTypeArguments.[0] (ty.GetProperty("Value").GetValue(value, [||]))
                                ImGui.PopID ()
                                let value = Activator.CreateInstance (ty, [|value'|])
                                if focused' then focused <- true
                                (changed || changed', value)
                            else
                                ImGui.SameLine ()
                                ImGui.Text name
                                (changed, value)
                        elif ty.IsGenericType &&
                             ty.GetGenericTypeDefinition () = typedefof<_ voption> &&
                             (not ty.GenericTypeArguments.[0].IsGenericType || ty.GenericTypeArguments.[0].GetGenericTypeDefinition () <> typedefof<_ option>) &&
                             (not ty.GenericTypeArguments.[0].IsGenericType || ty.GenericTypeArguments.[0].GetGenericTypeDefinition () <> typedefof<_ voption>) &&
                             ty.GenericTypeArguments.[0] <> typeof<MaterialProperties> &&
                             ty.GenericTypeArguments.[0] <> typeof<Material> &&
                             (ty.GenericTypeArguments.[0].IsValueType ||
                              ty.GenericTypeArguments.[0] = typeof<string> ||
                              ty.GenericTypeArguments.[0] = typeof<Slide> ||
                              ty.GenericTypeArguments.[0] = typeof<Image AssetTag> ||
                              ty.GenericTypeArguments.[0] = typeof<Font AssetTag> ||
                              ty.GenericTypeArguments.[0] = typeof<TileMap AssetTag> ||
                              ty.GenericTypeArguments.[0] = typeof<CubeMap AssetTag> ||
                              ty.GenericTypeArguments.[0] = typeof<Sound AssetTag> ||
                              ty.GenericTypeArguments.[0] = typeof<Song AssetTag> ||
                              ty.GenericTypeArguments.[0] = typeof<StaticModel AssetTag> ||
                              ty.GenericTypeArguments.[0] = typeof<AnimatedModel AssetTag> ||
                              ty.GenericTypeArguments.[0] = typeof<SoundDescriptor> ||
                              ty.GenericTypeArguments.[0] = typeof<SongDescriptor> ||
                              ty.GenericTypeArguments.[0] = typeof<Entity> ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ array>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ list>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FList>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FQueue>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FDeque>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Set>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FSet>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<Map<_, _>>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<FMap<_, _>>) ||
                              (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Relation>) ||
                              ty.GenericTypeArguments.[0] |> FSharpType.isNullTrueValue) then
                            let mutable isSome = ty.GetProperty("IsSome").GetValue(null, [|value|]) :?> bool
                            let (changed, value) =
                                if ImGui.Checkbox ("##" + name, &isSome) then
                                    if isSome then
                                        if ty.GenericTypeArguments.[0].IsValueType then
                                            if ty.GenericTypeArguments.[0] = typeof<Color> then
                                                (true, Activator.CreateInstance (ty, [|colorOne :> obj|]))
                                            elif ty.GenericTypeArguments.[0].Name = typedefof<_ AssetTag>.Name then
                                                (true, Activator.CreateInstance (ty, [|Activator.CreateInstance (ty.GenericTypeArguments.[0], [|""; ""|])|]))
                                            else (true, Activator.CreateInstance (ty, [|Activator.CreateInstance ty.GenericTypeArguments.[0]|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<string> then (true, Activator.CreateInstance (ty, [|"" :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Slide> then (true, Activator.CreateInstance (ty, [|{ IdlingTime = GameTime.zero; Destination = selectedScreen } :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Image AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.Image :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Font AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.Font :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<TileMap AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.TileMap :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<CubeMap AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.SkyBoxMap :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Sound AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.Sound :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Song AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.Song :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<StaticModel AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.StaticModel :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<AnimatedModel AssetTag> then (true, Activator.CreateInstance (ty, [|Assets.Default.AnimatedModel :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<SoundDescriptor> then (true, Activator.CreateInstance (ty, [|{ Volume = Constants.Audio.SongVolumeDefault; Sound = Assets.Default.Sound } :> obj|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<SongDescriptor> then (true, Activator.CreateInstance (ty, [|{ FadeInTime = GameTime.zero; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = GameTime.zero; RepeatLimitOpt = None; Volume = Constants.Audio.SongVolumeDefault; Song = Assets.Default.Song } :> obj|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ array> then (true, Activator.CreateInstance (ty, [|Reflection.objsToArray ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ list> then (true, Activator.CreateInstance (ty, [|Reflection.objsToList ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FList> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FList>.Name ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FQueue> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FQueue>.Name ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FDeque> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FDeque>.Name ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Set> then (true, Activator.CreateInstance (ty, [|Reflection.objsToSet ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FSet> then (true, Activator.CreateInstance (ty, [|Reflection.objsToFSet ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<Map<_, _>> then (true, Activator.CreateInstance (ty, [|Reflection.pairsToMap ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<FMap<_, _>> then (true, Activator.CreateInstance (ty, [|Reflection.pairsToFMap ty.GenericTypeArguments.[0] []|]))
                                        elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Relation> then
                                            let relationType = ty.GenericTypeArguments.[0]
                                            let makeFromStringFunction = relationType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
                                            let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((relationType.GetGenericArguments ()).[0])
                                            let relationValue = makeFromStringFunctionGeneric.Invoke (null, [|"^"|])
                                            (true, Activator.CreateInstance (ty, [|relationValue|]))
                                        elif ty.GenericTypeArguments.[0] = typeof<Entity> then
                                            (true, Activator.CreateInstance (ty, [|Nu.Entity (Array.add "???" selectedGroup.Names) :> obj|]))
                                        elif FSharpType.isNullTrueValue ty.GenericTypeArguments.[0] then
                                            (true, Activator.CreateInstance (ty, [|null|]))
                                        else failwithumf ()
                                    else (true, ValueNone)
                                else (false, value)
                            focused <- ImGui.IsItemFocused ()
                            if isSome then
                                ImGui.SameLine ()
                                ImGui.PushID name
                                let (focused', changed', value') = World.imGuiEditProperty searchAssetViewer snapDrag valueStrPreviousRef dragDropPayloadOpt selectedScreen selectedGroup name ty.GenericTypeArguments.[0] (ty.GetProperty("Value").GetValue(value, [||]))
                                ImGui.PopID ()
                                let value = Activator.CreateInstance (ty, [|value'|])
                                if focused' then focused <- true
                                (changed || changed', value)
                            else
                                ImGui.SameLine ()
                                ImGui.Text name
                                (changed, value)
                        elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ AssetTag> then
                            let mutable valueStr = converter.ConvertToString value
                            let (changed, value) =
                                if ImGui.InputText ("##text" + name, &valueStr, 4096u) then
                                    try let value = converter.ConvertFromString valueStr
                                        (true, value)
                                    with _ ->
                                        (false, value)
                                else (false, value)
                            if ImGui.IsItemFocused () then focused <- true
                            let (changed, value) =
                                if ImGui.BeginDragDropTarget () then
                                    let (changed, value) =
                                        if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                            match dragDropPayloadOpt with
                                            | Some payload ->
                                                try let valueStrEscaped = payload
                                                    let valueStrUnescaped = String.unescape valueStrEscaped
                                                    let value = converter.ConvertFromString valueStrUnescaped
                                                    (true, value)
                                                with _ ->
                                                    (changed, value)
                                            | None -> (changed, value)
                                        else (changed, value)
                                    ImGui.EndDragDropTarget ()
                                    (changed, value)
                                else (changed, value)
                            ImGui.SameLine ()
                            ImGui.PushID ("##pickAsset" + name)
                            if ImGui.Button ("V", v2Dup 19.0f) then searchAssetViewer ()
                            if ImGui.IsItemFocused () then focused <- true
                            ImGui.PopID ()
                            ImGui.SameLine ()
                            ImGui.Text name
                            (changed, value)
                        else
                            let mutable valueStr = converter.ConvertToString value
                            if ImGui.InputText (name, &valueStr, 131072u) && valueStr <> valueStrPreviousRef.Value then
                                let (changed, value) =
                                    try let value = converter.ConvertFromString valueStr
                                        (true, value)
                                    with _ ->
                                        (false, value)
                                valueStrPreviousRef.Value <- valueStr
                                (changed, value)
                            else (false, value)
                    else (changed, value)
            if ImGui.IsItemFocused () then focused <- true
            (focused, changed, value)