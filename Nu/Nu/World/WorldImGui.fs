// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open System.Reflection
open FSharp.NativeInterop
open FSharp.Reflection
open DotRecast.Recast
open ImGuiNET
open JoltPhysicsSharp
open Prime

/// ImGui functions for the world.
[<AutoOpen>]
module WorldImGui =

    type World with

        static member internal getImGui (world : World) =
            world.Subsystems.ImGui

        static member imGuiTryGetTextureId assetTag world =
            let rendererProcess = World.getRendererProcess world
            rendererProcess.TryGetImGuiTextureId assetTag

        /// Render circles via ImGui in the current eye 2d space, computing color as specified.
        static member imGuiCircles2dPlus absolute (positions : Vector2 seq) radius filled (computeColor : Vector2 -> Color) (world : World) =
            let drawList = ImGui.GetBackgroundDrawList ()
            let radiusScaled = radius * single Globals.Render.DisplayScalar
            for position in positions do
                let color = computeColor position
                let positionWindow = ImGui.Position2dToWindow (absolute, world.Eye2dCenter, world.Eye2dSize, world.RasterViewport, position)
                if filled
                then drawList.AddCircleFilled (positionWindow, radiusScaled, color.Abgr)
                else drawList.AddCircle (positionWindow, radiusScaled, color.Abgr)

        /// Render circles via ImGui in the current eye 2d space.
        static member imGuiCircles2d absolute position radius filled color world =
            World.imGuiCircles2dPlus absolute position radius filled (constant color) world

        /// Render a circle via ImGui in the current eye 3d space.
        static member imGuiCircle2d absolute position radius filled color world =
            World.imGuiCircles2d absolute (SArray.singleton position) radius filled color world

        /// Render segments via ImGui in the current eye 2d space, computing color as specified.
        static member imGuiSegments2dPlus absolute (segments : struct (Vector2 * Vector2) seq) thickness (computeColor : struct (Vector2 * Vector2) -> Color) (world : World) =
            let drawList = ImGui.GetBackgroundDrawList ()
            for struct (start, stop) in segments do
                let color = computeColor struct (start, stop)
                let startWindow = ImGui.Position2dToWindow (absolute, world.Eye2dCenter, world.Eye2dSize, world.RasterViewport, start)
                let stopWindow = ImGui.Position2dToWindow (absolute, world.Eye2dCenter, world.Eye2dSize, world.RasterViewport, stop)
                drawList.AddLine (startWindow, stopWindow, color.Abgr, thickness)

        /// Render segments via ImGui in the current eye 2d space.
        static member imGuiSegments2d absolute segments thickness color world =
            World.imGuiSegments2dPlus absolute segments thickness (constant color) world

        /// Render a segment via ImGui in the current eye 2d space.
        static member imGuiSegment2d absolute segment thickness color world =
            World.imGuiSegments2d absolute (SArray.singleton segment) thickness color world

        /// Render circles via ImGui in the current eye 3d space, computing color as specified.
        static member imGuiCircles3dPlus (positions : Vector3 seq) radius filled (computeColor : Vector3 -> Color) (world : World) =
            let drawList = ImGui.GetBackgroundDrawList ()
            let windowPosition = ImGui.GetWindowPos ()
            let windowSize = ImGui.GetWindowSize ()
            let view = Viewport.getView3d world.Eye3dCenter world.Eye3dRotation
            let projection = Viewport.getProjection3d world.Eye3dFieldOfView world.RasterViewport
            let viewProjection = view * projection
            let frustumView = world.Eye3dFrustumView
            for position in positions do
                if frustumView.Contains position = ContainmentType.Contains then
                    let color = computeColor position
                    let positionWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, position)
                    if filled
                    then drawList.AddCircleFilled (positionWindow, radius, color.Abgr)
                    else drawList.AddCircle (positionWindow, radius, color.Abgr)

        /// Render circles via ImGui in the current eye 3d space.
        static member imGuiCircles3d position radius filled color world =
            World.imGuiCircles3dPlus position radius filled (constant color) world

        /// Render a circle via ImGui in the current eye 3d space.
        static member imGuiCircle3d position radius filled color world =
            World.imGuiCircles3d (SArray.singleton position) radius filled color world

        /// Render segments via ImGui in the current eye 3d space, computing color as specified.
        static member imGuiSegments3dPlus (segments : Segment3 seq) thickness (computeColor : Segment3 -> Color) (world : World) =
            let drawList = ImGui.GetBackgroundDrawList ()
            let windowPosition = ImGui.GetWindowPos ()
            let windowSize = ImGui.GetWindowSize ()
            let view = Viewport.getView3d world.Eye3dCenter world.Eye3dRotation
            let projection = Viewport.getProjection3d world.Eye3dFieldOfView world.RasterViewport
            let viewProjection = view * projection
            let frustumView = world.Eye3dFrustumView
            for segment in segments do
                for segment' in Math.TryUnionSegmentAndFrustum' (segment, frustumView) do
                    let color = computeColor segment'
                    let startWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, segment'.A)
                    let stopWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, segment'.B)
                    drawList.AddLine (startWindow, stopWindow, color.Abgr, thickness)

        /// Render segments via ImGui in the current eye 3d space.
        static member imGuiSegments3d segments thickness color world =
            World.imGuiSegments3dPlus segments thickness (constant color) world

        /// Render a segment via ImGui in the current eye 3d space.
        static member imGuiSegment3d segment thickness color world =
            World.imGuiSegments3d (SArray.singleton segment) thickness color world

        /// Render a box via ImGui in the current eye 3d space.
        static member imGuiBox3d (box : Box3) (color : Color) (world : World) =
            let drawList = ImGui.GetBackgroundDrawList ()
            let windowPosition = ImGui.GetWindowPos ()
            let windowSize = ImGui.GetWindowSize ()
            let eyeCenter = world.Eye3dCenter
            let eyeRotation = world.Eye3dRotation
            let eyeFieldOfView = world.Eye3dFieldOfView
            let viewport = world.RasterViewport
            let frustum = Viewport.getFrustum eyeCenter eyeRotation eyeFieldOfView viewport
            let view = Viewport.getView3d eyeCenter eyeRotation
            let projection = Viewport.getProjection3d eyeFieldOfView viewport
            let viewProjection = view * projection
            let segments = box.Segments
            for segment in segments do
                for segment' in Math.TryUnionSegmentAndFrustum' (segment, frustum) do
                    let aWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, segment'.A)
                    let bWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, segment'.B)
                    drawList.AddLine (aWindow, bWindow, color.Abgr)

        /// Edit a Box3 via ImGui in the current eye 3d space.
        static member imGuiEditBox3d snap box (world : World) =
            let mutable box = box
            let manipulationResult =
                ImGuizmo.ManipulateBox3
                    (world.Eye3dCenter,
                     world.Eye3dRotation,
                     world.Eye3dFieldOfView,
                     world.RasterViewport,
                     snap,
                     &box)
            (manipulationResult, box)

        /// Edit an array value via ImGui.
        static member imGuiEditPropertyArray<'a> (editItem : string -> 'a -> bool * bool * 'a) (defaultItemValue : 'a) itemsName (items : 'a array) context =
            let mutable promoted = false
            let mutable edited = false
            let items =
                if ImGui.SmallButton "+" then
                    edited <- true
                    Array.add defaultItemValue items
                else items
            if ImGui.IsItemFocused () then context.FocusProperty ()
            ImGui.Indent ()
            let itemOpts =
                let mutable i = 0
                [|for item in items do
                    let itemName = itemsName + ".[" + string i + "]"
                    ImGui.PushID itemName
                    let itemOpt =
                        if not (ImGui.SmallButton "x") then
                            ImGui.SameLine ()
                            try let (promoted2, edited2, item) = editItem itemName item
                                if promoted2 then promoted <- true
                                if edited2 then edited <- true
                                if ImGui.IsItemFocused () then context.FocusProperty ()
                                Some item
                            with _ -> Some item
                        else edited <- true; None
                    if ImGui.IsItemFocused () then context.FocusProperty ()
                    ImGui.PopID ()
                    i <- inc i
                    itemOpt|]
            let items = Array.definitize itemOpts
            ImGui.Unindent ()
            (promoted, edited, items)

        /// Edit a list value via ImGui.
        static member imGuiEditPropertyList<'a> (editItem : string -> 'a -> bool * bool * 'a) (defaultItemValue : 'a) itemsName (items : 'a list) context =
            let mutable promoted = false
            let mutable edited = false
            let items =
                if ImGui.SmallButton "+" then
                    edited <- true
                    items @ [defaultItemValue]
                else items
            if ImGui.IsItemFocused () then context.FocusProperty ()
            ImGui.Indent ()
            let itemOpts =
                let mutable i = 0
                [for item in items do
                    let itemName = itemsName + ".[" + string i + "]"
                    ImGui.PushID itemName
                    let itemOpt =
                        if not (ImGui.SmallButton "x") then
                            ImGui.SameLine ()
                            try let (promoted2, edited2, item) = editItem itemName item
                                if promoted2 then promoted <- true
                                if edited2 then edited <- true
                                if ImGui.IsItemFocused () then context.FocusProperty ()
                                Some item
                            with _ -> Some item
                        else edited <- true; None
                    if ImGui.IsItemFocused () then context.FocusProperty ()
                    ImGui.PopID ()
                    i <- inc i
                    itemOpt]
            let items = List.definitize itemOpts
            ImGui.Unindent ()
            (edited, items)

        /// Edit a record value via ImGui, optionally replacing the instructed fields.
        /// This function can also automatically promote user-defined types for code-reloading.
        static member imGuiEditPropertyRecordPlus tryReplaceField headered (name : string) (ty : Type) (value : obj) context world : bool * bool * obj =
            if headered then
                ImGui.Text name
                ImGui.Indent ()
            ImGui.PushID name
            let mutable promoted = false
            let mutable edited = false
            let value =
                let value' = objToObj ty value
                if refNeq value' value then promoted <- true
                value'
            let fields =
                FSharpType.GetRecordFields (ty, true)
                |> Array.zip (FSharpValue.GetRecordFields (value, true))
                |> Array.map (fun (field, fieldInfo : PropertyInfo) ->
                    match tryReplaceField fieldInfo field with
                    | Some (edited2, field) ->
                        if edited2 then edited <- true
                        field
                    | None ->
                        let fieldName =
                            if ty.IsDefined (typeof<SymbolicExpansionAttribute>, true) then
                                let expansionAttribute = ty.GetCustomAttribute<SymbolicExpansionAttribute> true
                                if expansionAttribute.PrettifyFieldNames then
                                    if fieldInfo.Name.EndsWith "_"
                                    then fieldInfo.Name.Substring (0, dec fieldInfo.Name.Length)
                                    else String.capitalize fieldInfo.Name
                                else fieldInfo.Name
                            else fieldInfo.Name
                        let (promoted2, edited2, field) =
                            if  fieldInfo.PropertyType.Name <> typedefof<_ AssetTag>.Name &&
                                (FSharpType.IsRecord fieldInfo.PropertyType || FSharpType.isRecordAbstract fieldInfo.PropertyType) then
                                World.imGuiEditPropertyRecord true fieldName fieldInfo.PropertyType field context world
                            else World.imGuiEditProperty fieldName fieldInfo.PropertyType field context world
                        if promoted2 then promoted <- true
                        if edited2 then edited <- true
                        field)
            let value = FSharpValue.MakeRecord (ty, fields, true)
            if headered then ImGui.Unindent ()
            ImGui.PopID ()
            (promoted, edited, value)

        /// Edit a record value via ImGui, optionally replacing the instructed fields.
        static member imGuiEditPropertyRecord headered name ty value context world : bool * bool * obj =
            World.imGuiEditPropertyRecordPlus (fun _ _ -> None) headered name ty value context world

        /// Select a case name from an F# union via ImGui.
        static member imGuiSelectCase name ty (value : 'a) context =
            let (promoted, value) =
                let value' = objToObj ty value :?> 'a
                (refNeq value' value, value')
            let cases = FSharpType.GetUnionCases ty
            let tag = getCaseTag value
            let case = cases.[tag]
            let mutable caseNameEdited = false
            let mutable caseName = case.Name
            if ImGui.BeginCombo (name, caseName) then
                for case' in cases do
                    let caseName' = case'.Name
                    if ImGui.Selectable (caseName', strEq caseName' caseName) then
                        if strNeq caseName caseName' then
                            caseNameEdited <- true
                            caseName <- caseName'
                ImGui.EndCombo ()
            if ImGui.IsItemFocused () then context.FocusProperty ()
            (promoted, caseNameEdited, caseName)

        /// Edit a value via ImGui, also automatically promoting user-defined types for code-reloading.
        /// TODO: split up this function.
        static member imGuiEditProperty (name : string) (ty : Type) (value : obj) (context : EditContext) world : bool * bool * obj =
            let (promoted, value) =
                let value' = objToObj ty value
                (refNeq value' value, value')
            match value with
            | :? bool as b -> let mutable b = b in (promoted, ImGui.Checkbox (name, &b), b :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? int8 as i -> let mutable i = int32 i in (promoted, ImGui.DragInt (name, &i), int8 i :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? uint8 as i -> let mutable i = int32 i in (promoted, ImGui.DragInt (name, &i), uint8 i :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? int16 as i -> let mutable i = int32 i in (promoted, ImGui.DragInt (name, &i), int16 i :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? uint16 as i -> let mutable i = int32 i in (promoted, ImGui.DragInt (name, &i), uint16 i :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? int32 as i -> let mutable i = int32 i in (promoted, ImGui.DragInt (name, &i), int32 i :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? uint32 as i -> let mutable i = int32 i in (promoted, ImGui.DragInt (name, &i), uint32 i :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? int64 as i -> let mutable i = int32 i in (promoted, ImGui.DragInt (name, &i), int64 i :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? uint64 as i -> let mutable i = int32 i in (promoted, ImGui.DragInt (name, &i), uint64 i :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? single as f -> let mutable f = single f in (promoted, ImGui.DragFloat (name, &f, context.SnapDrag), single f :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? double as f -> let mutable f = single f in (promoted, ImGui.DragFloat (name, &f, context.SnapDrag), double f :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? Vector2 as v -> let mutable v = v in (promoted, ImGui.DragFloat2 (name, &v, context.SnapDrag), v :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? Vector3 as v -> let mutable v = v in (promoted, ImGui.DragFloat3 (name, &v, context.SnapDrag), v :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? Vector4 as v -> let mutable v = v in (promoted, ImGui.DragFloat4 (name, &v, context.SnapDrag), v :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? Vector2i as v -> let mutable v = v in (promoted, ImGui.DragInt2 (name, &v.X, context.SnapDrag), v :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? Vector3i as v -> let mutable v = v in (promoted, ImGui.DragInt3 (name, &v.X, context.SnapDrag), v :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? Vector4i as v -> let mutable v = v in (promoted, ImGui.DragInt4 (name, &v.X, context.SnapDrag), v :> obj) |> fun result -> (if ImGui.IsItemFocused () then context.FocusProperty ()); result
            | :? Box2 as b ->
                ImGui.Text name
                ImGui.PushID name
                ImGui.Indent ()
                let mutable min = v2 b.Min.X b.Min.Y
                let mutable size = v2 b.Size.X b.Size.Y
                let minEdited = ImGui.DragFloat2 (nameof b.Min, &min, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                let sizeEdited = ImGui.DragFloat2 (nameof b.Size, &size, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                ImGui.Unindent ()
                ImGui.PopID ()
                (promoted, minEdited || sizeEdited, box2 min size :> obj)
            | :? Box3 as b ->
                ImGui.Text name
                ImGui.PushID name
                ImGui.Indent ()
                let mutable min = v3 b.Min.X b.Min.Y b.Min.Z
                let mutable size = v3 b.Size.X b.Size.Y b.Size.Z
                let minEdited = ImGui.DragFloat3 (nameof b.Min, &min, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                let sizeEdited = ImGui.DragFloat3 (nameof b.Size, &size, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                ImGui.Unindent ()
                ImGui.PopID ()
                (promoted, minEdited || sizeEdited, box3 min size :> obj)
            | :? Box2i as b ->
                ImGui.Text name
                ImGui.PushID name
                ImGui.Indent ()
                let mutable min = v2i b.Min.X b.Min.Y
                let mutable size = v2i b.Size.X b.Size.Y
                let minEdited = ImGui.DragInt2 (nameof b.Min, &min.X, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                let sizeEdited = ImGui.DragInt2 (nameof b.Size, &size.X, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                ImGui.Unindent ()
                ImGui.PopID ()
                (promoted, minEdited || sizeEdited, box2i min size :> obj)
            | :? Box3i as b ->
                ImGui.Text name
                ImGui.PushID name
                ImGui.Indent ()
                let mutable min = v3i b.Min.X b.Min.Y b.Min.Z
                let mutable size = v3i b.Size.X b.Size.Y b.Size.Z
                let minEdited = ImGui.DragInt3 (nameof b.Min, &min.X, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                let sizeEdited = ImGui.DragInt3 (nameof b.Size, &size.X, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                ImGui.Unindent ()
                ImGui.PopID ()
                (promoted, minEdited || sizeEdited, box3i min size :> obj)
            | :? Quaternion as q ->
                let mutable v = v4 q.X q.Y q.Z q.W
                let result = (promoted, ImGui.DragFloat4 (name, &v, context.SnapDrag), quat v.X v.Y v.Z v.W :> obj)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                result
            | :? Frustum as frustum ->
                let mutable frustumStr = string frustum
                (promoted, ImGui.InputText (name, &frustumStr, 4096u, ImGuiInputTextFlags.ReadOnly), frustum :> obj)
            | :? Color as c ->
                let mutable v = v4 c.R c.G c.B c.A
                let result = (promoted, ImGui.ColorEdit4 (name, &v), color v.X v.Y v.Z v.W :> obj)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                result
            | :? Transition as transition ->
                World.imGuiEditPropertyRecord true name (typeof<Transition>) transition context world
            | :? Slide as slide ->
                World.imGuiEditPropertyRecord true name (typeof<Slide>) slide context world
            | :? RenderStyle as style ->
                let mutable index = match style with Deferred -> 0 | Forward _ -> 1
                let (edited, style) =
                    if ImGui.Combo (name, &index, [|nameof Deferred; nameof Forward|], 2)
                    then (true, match index with 0 -> Deferred | 1 -> Forward (0.0f, 0.0f) | _ -> failwithumf ())
                    else (promoted, style)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                let (edited, style) =
                    match index with
                    | 0 -> (edited, style)
                    | 1 ->
                        match style with
                        | Deferred -> failwithumf ()
                        | Forward (subsort, sort) ->
                            let mutable (subsort, sort) = (subsort, sort)
                            ImGui.Indent ()
                            let subsortEdited = ImGui.DragFloat ("Subsort via " + name, &subsort, context.SnapDrag)
                            if ImGui.IsItemFocused () then context.FocusProperty ()
                            let sortEdited = ImGui.DragFloat ("Sort via " + name, &sort, context.SnapDrag)
                            if ImGui.IsItemFocused () then context.FocusProperty ()
                            ImGui.Unindent ()
                            (edited || subsortEdited || sortEdited, Forward (subsort, sort))
                    | _ -> failwithumf ()
                (promoted, edited, style :> obj)
            | :? LightType as light ->
                let mutable index = light.Enumerate
                let (edited, light) =
                    let names = LightType.Names
                    if ImGui.Combo (name, &index, names, names.Length)
                    then (true, LightType.makeFromEnumeration index)
                    else (false, light)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                let (edited, light) =
                    match index with
                    | 0 -> (edited, light)
                    | 1 ->
                        match light with
                        | PointLight -> failwithumf ()
                        | SpotLight (innerCone, outerCone) ->
                            let mutable (innerCone, outerCone) = (innerCone, outerCone)
                            ImGui.Indent ()
                            let innerConeEdited = ImGui.DragFloat ("InnerCone via " + name, &innerCone, context.SnapDrag)
                            if ImGui.IsItemFocused () then context.FocusProperty ()
                            let outerConeEdited = ImGui.DragFloat ("OuterCone via " + name, &outerCone, context.SnapDrag)
                            if ImGui.IsItemFocused () then context.FocusProperty ()
                            ImGui.Unindent ()
                            (edited || innerConeEdited || outerConeEdited, SpotLight (innerCone, outerCone))
                        | DirectionalLight -> failwithumf ()
                        | CascadedLight -> failwithumf ()
                    | 2 -> (edited, light)
                    | 3 -> (edited, light)
                    | _ -> failwithumf ()
                (promoted, edited, light :> obj)
            | :? Substance as substance ->
                let mutable scalar = match substance with Mass m -> m | Density d -> d
                let edited = ImGui.DragFloat ("##scalar via " + name, &scalar, context.SnapDrag)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                let mutable index = match substance with Mass _ -> 0 | Density _ -> 1
                ImGui.SameLine ()
                let result =
                    if ImGui.Combo (name, &index, [|nameof Mass; nameof Density|], 2) || edited then
                        let substance = match index with 0 -> Mass scalar | 1 -> Density scalar | _ -> failwithumf ()
                        (promoted, true, substance :> obj)
                    else (promoted, false, substance :> obj)
                if ImGui.IsItemFocused () then context.FocusProperty ()
                result
            | :? Animation as animation ->
                let tryReplaceAnimationName (fieldInfo : PropertyInfo) (field : obj) =
                    match (fieldInfo.Name, context.SelectedEntityOpt) with
                    | ("Name", Some selectedEntity) ->
                        match selectedEntity.TryGetProperty "AnimatedModel" world with // HACK: can only get related asset tag from selected entity's top-level.
                        | Some property when (property.PropertyValue :? AnimatedModel AssetTag) ->
                            let animatedModel = property.PropertyValue :?> AnimatedModel AssetTag
                            match Metadata.tryGetAnimatedModelMetadata animatedModel with
                            | ValueSome metadata when metadata.SceneOpt.IsSome ->
                                let animationNames = metadata.SceneOpt.Value.Animations |> Seq.rev |> Seq.map _.Name // NOTE: for some reason, Assimp seems to store animations in reverse order.
                                let mutable animationName = field :?> string
                                let mutable animationNameEdited = false
                                if ImGui.BeginCombo (name, animationName) then
                                    for animationName' in animationNames do
                                        if String.notEmpty animationName' && ImGui.Selectable (animationName', strEq animationName' animationName) then
                                            if strNeq animationName animationName' then
                                                animationName <- animationName'
                                                animationNameEdited <- true
                                    ImGui.EndCombo ()
                                if ImGui.IsItemFocused () then context.FocusProperty ()
                                Some (animationNameEdited, animationName :> obj)
                            | ValueSome _ | ValueNone -> None
                        | Some _ | None -> None
                    | _ -> None
                World.imGuiEditPropertyRecordPlus tryReplaceAnimationName true name (typeof<Animation>) animation context world
            | :? TerrainMaterialProperties as tmps ->
                World.imGuiEditPropertyRecord true name (typeof<TerrainMaterialProperties>) tmps context world
            | :? MaterialProperties as mps ->
                World.imGuiEditPropertyRecord false name (typeof<MaterialProperties>) mps context world
            | :? Material as material ->
                World.imGuiEditPropertyRecord false name (typeof<Material>) material context world
            | :? FlowLimit as limit ->
                let (promoted, caseNameEdited, caseName) = World.imGuiSelectCase name ty limit context
                let limit =
                    if caseNameEdited then
                        match caseName with
                        | nameof FlowParent -> FlowParent
                        | nameof FlowUnlimited -> FlowUnlimited
                        | nameof FlowTo -> FlowTo 32.0f
                        | _ -> failwithumf ()
                    else limit
                match limit with
                | FlowParent -> (promoted, caseNameEdited, limit)
                | FlowUnlimited -> (promoted, caseNameEdited, limit)
                | FlowTo limit ->
                    let (promoted2, edited, limit) = World.imGuiEditProperty "Limit" (getType limit) limit context world
                    (promoted || promoted2, caseNameEdited || edited, FlowTo (limit :?> single))
            | :? Layout as layout ->
                let (promoted, caseNameEdited, caseName) = World.imGuiSelectCase name ty layout context
                let layout =
                    if caseNameEdited then
                        match caseName with
                        | nameof Flow -> Flow (FlowDownward, FlowParent)
                        | nameof Dock -> Dock (v4Dup 8.0f, false, true)
                        | nameof Grid -> Grid (v2iDup 2, None, true)
                        | nameof Manual -> Manual
                        | _ -> failwithumf ()
                    else layout
                ImGui.Indent ()
                let (edited, layout) =
                    match layout with // NOTE: we explicitly ignore any promotions that would take place here because we should be safe to presume there are none.
                    | Flow (direction, limit) ->
                        let (_, edited, direction) = World.imGuiEditProperty "FlowDirection" (getType direction) direction context world
                        if direction = FlowLeftward || direction = FlowUpward then ImGui.SameLine (); ImGui.Text "(not implemented)" // TODO: P1: remove this line when implemented.
                        let (_, edited2, limit) = World.imGuiEditProperty "FlowLimit" (getType limit) limit context world
                        (caseNameEdited || edited || edited2, Flow (direction :?> FlowDirection, limit :?> FlowLimit))
                    | Dock (margins, percentageBased, resizeChildren) ->
                        let (_, edited, margins) = World.imGuiEditProperty "Margins" (getType margins) margins context world
                        let (_, edited2, percentageBased) = World.imGuiEditProperty "PercentageBased" (getType percentageBased) percentageBased context world
                        ImGui.SameLine (); ImGui.Text "(not implemented)" // TODO: P1: remove this line when implemented.
                        let (_, edited3, resizeChildren) = World.imGuiEditProperty "ResizeChildren" (getType resizeChildren) resizeChildren context world
                        ImGui.SameLine (); ImGui.Text "(not implemented)" // TODO: P1: remove this line when implemented.
                        (caseNameEdited || edited || edited2 || edited3, Dock (margins :?> Vector4, percentageBased :?> bool, resizeChildren :?> bool))
                    | Grid (dims, flowDirectionOpt, resizeChildren) ->
                        let (_, edited, dims) = World.imGuiEditProperty "Dims" (getType dims) dims context world
                        let (_, edited2, flowDirectionOpt) = World.imGuiEditProperty "FlowDirectionOpt" (getType flowDirectionOpt) flowDirectionOpt context world
                        let (_, edited3, resizeChildren) = World.imGuiEditProperty "ResizeChildren" (getType resizeChildren) resizeChildren context world
                        (caseNameEdited || edited || edited2 || edited3, Grid (dims :?> Vector2i, flowDirectionOpt :?> FlowDirection option, resizeChildren :?> bool))
                    | Manual -> (caseNameEdited, layout)
                ImGui.Unindent ()
                (promoted, edited, layout)
            | :? Lighting3dConfig as lighting3dConfig ->
                let mutable lighting3dEdited = false
                let mutable lightCutoffMargin = lighting3dConfig.LightCutoffMargin
                let mutable lightAmbientBoostCutoff = lighting3dConfig.LightAmbientBoostCutoff
                let mutable lightAmbientBoostScalar = lighting3dConfig.LightAmbientBoostScalar
                let mutable lightShadowSamples = lighting3dConfig.LightShadowSamples
                let mutable lightShadowBias = lighting3dConfig.LightShadowBias
                let mutable lightShadowSampleScalar = lighting3dConfig.LightShadowSampleScalar
                let mutable lightShadowExponent = lighting3dConfig.LightShadowExponent
                let mutable lightShadowDensity = lighting3dConfig.LightShadowDensity
                let mutable fogEnabled = lighting3dConfig.FogEnabled
                let mutable fogType = lighting3dConfig.FogType.Enumerate
                let mutable fogStart = lighting3dConfig.FogStart
                let mutable fogFinish = lighting3dConfig.FogFinish
                let mutable fogDensity = lighting3dConfig.FogDensity
                let mutable fogColor = let color = lighting3dConfig.FogColor in color.V4
                let mutable sssEnabled = lighting3dConfig.SssEnabled
                let mutable ssaoEnabled = lighting3dConfig.SsaoEnabled
                let mutable ssaoIntensity = lighting3dConfig.SsaoIntensity
                let mutable ssaoBias = lighting3dConfig.SsaoBias
                let mutable ssaoRadius = lighting3dConfig.SsaoRadius
                let mutable ssaoDistanceMax = lighting3dConfig.SsaoDistanceMax
                let mutable ssvfEnabled = lighting3dConfig.SsvfEnabled
                let mutable ssvfSteps = lighting3dConfig.SsvfSteps
                let mutable ssvfAsymmetry = lighting3dConfig.SsvfAsymmetry
                let mutable ssvfIntensity = lighting3dConfig.SsvfIntensity
                let mutable ssrlEnabled = lighting3dConfig.SsrlEnabled
                let mutable ssrlIntensity = lighting3dConfig.SsrlIntensity
                let mutable ssrlDetail = lighting3dConfig.SsrlDetail
                let mutable ssrlRefinementsMax = lighting3dConfig.SsrlRefinementsMax
                let mutable ssrlRayThickness = lighting3dConfig.SsrlRayThickness
                let mutable ssrlTowardEyeCutoff = lighting3dConfig.SsrlTowardEyeCutoff
                let mutable ssrlDepthCutoff = lighting3dConfig.SsrlDepthCutoff
                let mutable ssrlDepthCutoffMargin = lighting3dConfig.SsrlDepthCutoffMargin
                let mutable ssrlDistanceCutoff = lighting3dConfig.SsrlDistanceCutoff
                let mutable ssrlDistanceCutoffMargin = lighting3dConfig.SsrlDistanceCutoffMargin
                let mutable ssrlRoughnessCutoff = lighting3dConfig.SsrlRoughnessCutoff
                let mutable ssrlRoughnessCutoffMargin = lighting3dConfig.SsrlRoughnessCutoffMargin
                let mutable ssrlSlopeCutoff = lighting3dConfig.SsrlSlopeCutoff
                let mutable ssrlSlopeCutoffMargin = lighting3dConfig.SsrlSlopeCutoffMargin
                let mutable ssrlEdgeHorizontalMargin = lighting3dConfig.SsrlEdgeHorizontalMargin
                let mutable ssrlEdgeVerticalMargin = lighting3dConfig.SsrlEdgeVerticalMargin
                let mutable ssrrEnabled = lighting3dConfig.SsrrEnabled
                let mutable ssrrIntensity = lighting3dConfig.SsrrIntensity
                let mutable ssrrDetail = lighting3dConfig.SsrrDetail
                let mutable ssrrRefinementsMax = lighting3dConfig.SsrrRefinementsMax
                let mutable ssrrRayThickness = lighting3dConfig.SsrrRayThickness
                let mutable ssrrDistanceCutoff = lighting3dConfig.SsrrDistanceCutoff
                let mutable ssrrDistanceCutoffMargin = lighting3dConfig.SsrrDistanceCutoffMargin
                let mutable ssrrEdgeHorizontalMargin = lighting3dConfig.SsrrEdgeHorizontalMargin
                let mutable ssrrEdgeVerticalMargin = lighting3dConfig.SsrrEdgeVerticalMargin
                let mutable bloomEnabled = lighting3dConfig.BloomEnabled
                let mutable bloomThreshold = lighting3dConfig.BloomThreshold
                let mutable bloomKarisAverageEnabled = lighting3dConfig.BloomKarisAverageEnabled
                let mutable bloomFilterRadius = lighting3dConfig.BloomFilterRadius
                let mutable bloomStrength = lighting3dConfig.BloomStrength
                lighting3dEdited <- ImGui.SliderFloat ("Light Cutoff Margin", &lightCutoffMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Light Ambient Boost Cutoff", &lightAmbientBoostCutoff, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Light Ambient Boost Scalar", &lightAmbientBoostScalar, 0.0f, 5.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderInt ("Light Shadow Samples", &lightShadowSamples, 0, 5) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Light Shadow Bias", &lightShadowBias, 0.0f, 0.05f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Light Shadow Sample Scalar", &lightShadowSampleScalar, 0.0f, 0.05f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Light Shadow Exponent", &lightShadowExponent, 0.0f, 90.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Light Shadow Density", &lightShadowDensity, 0.0f, 32.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Checkbox ("Fog Enabled", &fogEnabled) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Combo ("Fog Type", &fogType, FogType.Names, FogType.Names.Length) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.InputFloat ("Fog Start", &fogStart, 1.0f, 10.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.InputFloat ("Fog Finish", &fogFinish, 1.0f, 10.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Fog Density", &fogDensity, 0.0f, 0.1f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.ColorEdit4 ("Fog Color", &fogColor) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Checkbox ("Sss Enabled", &sssEnabled) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Checkbox ("Ssao Enabled", &ssaoEnabled) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssao Intensity", &ssaoIntensity, 0.0f, 10.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssao Bias", &ssaoBias, 0.0f, 0.1f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssao Radius", &ssaoRadius, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssao Distance Max", &ssaoDistanceMax, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Checkbox ("Ssvf Enabled", &ssvfEnabled) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderInt ("Ssvf Steps", &ssvfSteps, 0, 128) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssvf Asymmetry", &ssvfAsymmetry, -1.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssvf Intensity", &ssvfIntensity, 0.0f, 10.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Checkbox ("Ssrl Enabled", &ssrlEnabled) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Intensity", &ssrlIntensity, 0.0f, 10.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Detail", &ssrlDetail, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderInt ("Ssrl Refinements Max", &ssrlRefinementsMax, 0, 32) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Ray Thickness", &ssrlRayThickness, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Toward Eye Cutoff", &ssrlTowardEyeCutoff, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Depth Cutoff", &ssrlDepthCutoff, 0.0f, 128.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Depth Cutoff Margin", &ssrlDepthCutoffMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Distance Cutoff", &ssrlDistanceCutoff, 0.0f, 128.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Distance Cutoff Margin", &ssrlDistanceCutoffMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Roughness Cutoff", &ssrlRoughnessCutoff, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Roughness Cutoff Margin", &ssrlRoughnessCutoffMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Slope Cutoff", &ssrlSlopeCutoff, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Slope Cutoff Margin", &ssrlSlopeCutoffMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Edge Horizontal Margin", &ssrlEdgeHorizontalMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrl Edge Vertical Margin", &ssrlEdgeVerticalMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Checkbox ("Ssrr Enabled", &ssrrEnabled) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrr Intensity", &ssrrIntensity, 0.0f, 10.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrr Detail", &ssrrDetail, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderInt ("Ssrr Refinements Max", &ssrrRefinementsMax, 0, 32) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrr Ray Thickness", &ssrrRayThickness, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrr Distance Cutoff", &ssrrDistanceCutoff, 0.0f, 128.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrr Distance Cutoff Margin", &ssrrDistanceCutoffMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrr Edge Horizontal Margin", &ssrrEdgeHorizontalMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Ssrr Edge Vertical Margin", &ssrrEdgeVerticalMargin, 0.0f, 1.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Checkbox ("Bloom Enabled", &bloomEnabled) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Bloom Threshold", &bloomThreshold, 0.0f, 5.0f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.Checkbox ("Bloom Karis Average Enabled", &bloomKarisAverageEnabled) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Bloom Filter Radius", &bloomFilterRadius, 0.0f, 0.01f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                lighting3dEdited <- ImGui.SliderFloat ("Bloom Strength", &bloomStrength, 0.0f, 0.5f) || lighting3dEdited; if ImGui.IsItemFocused () then context.FocusProperty ()
                if lighting3dEdited then
                    let lighting3dConfig =
                        { LightCutoffMargin = lightCutoffMargin
                          LightAmbientBoostCutoff = lightAmbientBoostCutoff
                          LightAmbientBoostScalar = lightAmbientBoostScalar
                          LightShadowSamples = lightShadowSamples
                          LightShadowBias = lightShadowBias
                          LightShadowSampleScalar = lightShadowSampleScalar
                          LightShadowExponent = lightShadowExponent
                          LightShadowDensity = lightShadowDensity
                          FogEnabled = fogEnabled
                          FogType = FogType.makeFromEnumeration fogType
                          FogStart = fogStart
                          FogFinish = fogFinish
                          FogDensity = fogDensity
                          FogColor = Color fogColor
                          SssEnabled = sssEnabled
                          SsaoEnabled = ssaoEnabled
                          SsaoIntensity = ssaoIntensity
                          SsaoBias = ssaoBias
                          SsaoRadius = ssaoRadius
                          SsaoDistanceMax = ssaoDistanceMax
                          SsvfEnabled = ssvfEnabled
                          SsvfSteps = ssvfSteps
                          SsvfAsymmetry = ssvfAsymmetry
                          SsvfIntensity = ssvfIntensity
                          SsrlEnabled = ssrlEnabled
                          SsrlIntensity = ssrlIntensity
                          SsrlDetail = ssrlDetail
                          SsrlRefinementsMax = ssrlRefinementsMax
                          SsrlRayThickness = ssrlRayThickness
                          SsrlTowardEyeCutoff = ssrlTowardEyeCutoff
                          SsrlDepthCutoff = ssrlDepthCutoff
                          SsrlDepthCutoffMargin = ssrlDepthCutoffMargin
                          SsrlDistanceCutoff = ssrlDistanceCutoff
                          SsrlDistanceCutoffMargin = ssrlDistanceCutoffMargin
                          SsrlRoughnessCutoff = ssrlRoughnessCutoff
                          SsrlRoughnessCutoffMargin = ssrlRoughnessCutoffMargin
                          SsrlSlopeCutoff = ssrlSlopeCutoff
                          SsrlSlopeCutoffMargin = ssrlSlopeCutoffMargin
                          SsrlEdgeHorizontalMargin = ssrlEdgeHorizontalMargin
                          SsrlEdgeVerticalMargin = ssrlEdgeVerticalMargin
                          SsrrEnabled = ssrrEnabled
                          SsrrIntensity = ssrrIntensity
                          SsrrDetail = ssrrDetail
                          SsrrRefinementsMax = ssrrRefinementsMax
                          SsrrRayThickness = ssrrRayThickness
                          SsrrDistanceCutoff = ssrrDistanceCutoff
                          SsrrDistanceCutoffMargin = ssrrDistanceCutoffMargin
                          SsrrEdgeHorizontalMargin = ssrrEdgeHorizontalMargin
                          SsrrEdgeVerticalMargin = ssrrEdgeVerticalMargin
                          BloomEnabled = bloomEnabled
                          BloomThreshold = bloomThreshold
                          BloomKarisAverageEnabled = bloomKarisAverageEnabled
                          BloomFilterRadius = bloomFilterRadius
                          BloomStrength = bloomStrength }
                    (promoted, true, lighting3dConfig)
                else (promoted, false, lighting3dConfig)
            | :? Nav3dConfig as nav3dConfig ->
                let mutable nav3dConfigEdited = false
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
                if ImGui.SliderFloat ("CellSize", &cellSize, 0.01f, 1.0f, "%.2f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("CellHeight", &cellHeight, 0.01f, 1.0f, "%.2f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("AgentHeight", &agentHeight, 0.1f, 5.0f, "%.2f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("AgentRadius", &agentRadius, 0.0f, 5.0f, "%.2f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("AgentClimbMax", &agentClimbMax, 0.1f, 5.0f, "%.2f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("AgentSlopeMax", &agentSlopeMax, 1.0f, 90.0f, "%.0f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderInt ("RegionSizeMin", &regionSizeMin, 1, 150) then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderInt ("RegionSizeMerge", &regionSizeMerge, 1, 150) then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("EdgeLengthMax", &edgeLengthMax, 0.0f, 50.0f, "%.1f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("EdgeErrorMax", &edgeErrorMax, 0.1f, 3f, "%.1f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderInt ("VertPerPoly", &vertsPerPolygon, 3, 12) then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("DetailSampleDistance", &detailSampleDistance, 0.0f, 16.0f, "%.1f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.SliderFloat ("DetailSampleErrorMax", &detailSampleErrorMax, 0.0f, 16.0f, "%.1f") then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.Checkbox ("FilterLowHangingObstacles", &filterLowHangingObstacles) then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.Checkbox ("FilterLedgeSpans", &filterLedgeSpans) then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.Checkbox ("FilterWalkableLowHeightSpans", &filterWalkableLowHeightSpans) then nav3dConfigEdited <- true; if ImGui.IsItemFocused () then context.FocusProperty ()
                if ImGui.BeginCombo ("ParitionType", partitionTypeStr, ImGuiComboFlags.HeightLarge) then
                    let partitionTypeStrs = Array.map (fun (ptv : RcPartitionType) -> ptv.Name) RcPartitionType.Values
                    for partitionTypeStr' in partitionTypeStrs do
                        if ImGui.Selectable (partitionTypeStr', strEq partitionTypeStr' partitionTypeStr) then
                            if strNeq partitionTypeStr partitionTypeStr' then
                                partitionTypeStr <- partitionTypeStr'
                                nav3dConfigEdited <- true
                    ImGui.EndCombo ()
                if ImGui.IsItemFocused () then context.FocusProperty ()
                if nav3dConfigEdited then
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
                    (promoted, true, nav3dConfig)
                else (promoted, false, nav3dConfig)
            | :? (SpineAnimation array) as animations -> // TODO: P1: implement bepoke individual SpineAnimation editing.
                ImGui.Text name
                ImGui.SameLine ()
                ImGui.PushID name
                let (promoted, edited, animations) =
                    World.imGuiEditPropertyArray
                        (fun name animation ->
                            let (promoted, edited, animation) = World.imGuiEditProperty name (typeof<SpineAnimation>) animation context world
                            (promoted, edited, animation :?> SpineAnimation))
                        { SpineAnimationName = ""; SpineAnimationPlayback = Loop }
                        name animations context
                ImGui.PopID ()
                (promoted, edited, animations)
            | :? (Animation array) as animations ->
                ImGui.Text name
                ImGui.SameLine ()
                ImGui.PushID name
                let (promoted, edited, animations) =
                    World.imGuiEditPropertyArray
                        (fun name animation ->
                            let (promoted, edited, animation) = World.imGuiEditProperty name (typeof<Animation>) animation context world
                            (promoted, edited, animation :?> Animation))
                        { StartTime = GameTime.zero; LifeTimeOpt = None; Name = ""; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None }
                        name animations context
                ImGui.PopID ()
                (promoted, edited, animations)
            | :? CharacterProperties when
                (match context.SelectedEntityOpt with
                 | Some entity -> match entity.TryGet<Nu.BodyType> "BodyType" world with ValueSome bodyType -> not bodyType.IsCharacter | ValueNone -> false
                 | None -> false) ->
                (false, false, value) // hides character properties unless is character body type
            | _ ->
                let mutable combo = false
                let (edited, value) =
                    if FSharpType.IsUnion ty then
                        let cases = FSharpType.GetUnionCases ty
                        if Array.forall (fun (case : UnionCaseInfo) -> Array.isEmpty (case.GetFields ())) cases then
                            combo <- true
                            let caseNames = Array.map (fun (case : UnionCaseInfo) -> case.Name) cases
                            let (unionCaseInfo, _) = FSharpValue.GetUnionFields (value, ty)
                            let mutable tag = unionCaseInfo.Tag
                            let (edited2, value) =
                                if ImGui.Combo (name, &tag, caseNames, caseNames.Length)
                                then (true, FSharpValue.MakeUnion (cases.[tag], [||]))
                                else (false, value)
                            if ImGui.IsItemFocused () then context.FocusProperty ()
                            (edited2, value)
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
                         ty.GenericTypeArguments.[0] = typeof<ScatterType> ||
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
                         (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Address>) ||
                         ty.GenericTypeArguments.[0] |> FSharpType.isNullTrueValue) then
                        let mutable isSome = ty.GetProperty("IsSome").GetValue(null, [|value|]) :?> bool
                        let (edited2, value) =
                            if ImGui.Checkbox ("##" + name, &isSome) then
                                if isSome then
                                    if ty.GenericTypeArguments.[0].IsValueType then
                                        if ty.GenericTypeArguments.[0] = typeof<Color> then
                                            (true, Activator.CreateInstance (ty, [|colorOne :> obj|]))
                                        elif ty.GenericTypeArguments.[0].Name = typedefof<_ AssetTag>.Name then
                                            (true, Activator.CreateInstance (ty, [|Activator.CreateInstance (ty.GenericTypeArguments.[0], [|""; ""|])|]))
                                        else
                                            (true, Activator.CreateInstance (ty, [|Activator.CreateInstance ty.GenericTypeArguments.[0]|]))
                                    elif ty.GenericTypeArguments.[0] = typeof<string> then (true, Activator.CreateInstance (ty, [|"" :> obj|]))
                                    elif ty.GenericTypeArguments.[0] = typeof<Slide> then (true, Activator.CreateInstance (ty, [|{ IdlingTime = GameTime.zero; Destination = context.SelectedScreen } :> obj|]))
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
                                    elif ty.GenericTypeArguments.[0] = typeof<ScatterType> then (true, Activator.CreateInstance (ty, [|NoScatter :> obj|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ array> then (true, Activator.CreateInstance (ty, [|Reflection.objsToArray ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ list> then (true, Activator.CreateInstance (ty, [|Reflection.objsToList ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FList> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FList>.Name ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FQueue> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FQueue>.Name ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FDeque> then (true, Activator.CreateInstance (ty, [|Reflection.objsToCollection typedefof<_ FDeque>.Name ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Set> then (true, Activator.CreateInstance (ty, [|Reflection.objsToSet ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FSet> then (true, Activator.CreateInstance (ty, [|Reflection.objsToFSet ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<Map<_, _>> then (true, Activator.CreateInstance (ty, [|Reflection.pairsToMap ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<FMap<_, _>> then (true, Activator.CreateInstance (ty, [|Reflection.pairsToFMap ty.GenericTypeArguments.[0] []|]))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Address> then
                                        let addressType = ty.GenericTypeArguments.[0]
                                        let makeFromStringFunction = addressType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
                                        let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((addressType.GetGenericArguments ()).[0])
                                        let addressValue = makeFromStringFunctionGeneric.Invoke (null, [|"???"|])
                                        (true, Activator.CreateInstance (ty, [|addressValue|]))
                                    elif ty.GenericTypeArguments.[0] = typeof<Entity> then
                                        (true, Activator.CreateInstance (ty, [|Nu.Entity (Array.add "???" context.SelectedGroup.Names) :> obj|]))
                                    elif FSharpType.isNullTrueValue ty.GenericTypeArguments.[0] then
                                        (true, Activator.CreateInstance (ty, [|null|]))
                                    else (false, value)
                                else (true, None)
                            else (false, value)
                        if ImGui.IsItemFocused () then context.FocusProperty ()
                        if isSome then
                            ImGui.SameLine ()
                            ImGui.PushID name
                            let (promoted2, edited3, value2) = World.imGuiEditProperty name ty.GenericTypeArguments.[0] (ty.GetProperty("Value").GetValue(value, [||])) context world
                            ImGui.PopID ()
                            let value = Activator.CreateInstance (ty, [|value2|])
                            (promoted || promoted2, edited || edited2 || edited3, value)
                        else
                            ImGui.SameLine ()
                            ImGui.Text name
                            (promoted, edited || edited2, value)
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
                          ty.GenericTypeArguments.[0] = typeof<ScatterType> ||
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
                          (ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Address>) ||
                          ty.GenericTypeArguments.[0] |> FSharpType.isNullTrueValue) then
                        let mutable isSome = ty.GetProperty("IsSome").GetValue(value, [||]) :?> bool
                        let (edited2, value) =
                            if ImGui.Checkbox ("##" + name, &isSome) then
                                let createValueOption value =
                                    ty.GetMethod("Some", BindingFlags.Public ||| BindingFlags.Static).Invoke(null, [|value :> obj|])
                                if isSome then
                                    if ty.GenericTypeArguments.[0].IsValueType then
                                        if ty.GenericTypeArguments.[0] = typeof<Color> then
                                            (true, createValueOption colorOne)
                                        elif ty.GenericTypeArguments.[0].Name = typedefof<_ AssetTag>.Name then
                                            (true, createValueOption (Activator.CreateInstance (ty.GenericTypeArguments.[0], [|""; ""|])))
                                        else
                                            (true, createValueOption (Activator.CreateInstance ty.GenericTypeArguments.[0]))
                                    elif ty.GenericTypeArguments.[0] = typeof<string> then (true, createValueOption "")
                                    elif ty.GenericTypeArguments.[0] = typeof<Slide> then (true, createValueOption { IdlingTime = GameTime.zero; Destination = context.SelectedScreen })
                                    elif ty.GenericTypeArguments.[0] = typeof<Image AssetTag> then (true, createValueOption Assets.Default.Image)
                                    elif ty.GenericTypeArguments.[0] = typeof<Font AssetTag> then (true, createValueOption Assets.Default.Font)
                                    elif ty.GenericTypeArguments.[0] = typeof<TileMap AssetTag> then (true, createValueOption Assets.Default.TileMap)
                                    elif ty.GenericTypeArguments.[0] = typeof<CubeMap AssetTag> then (true, createValueOption Assets.Default.SkyBoxMap)
                                    elif ty.GenericTypeArguments.[0] = typeof<Sound AssetTag> then (true, createValueOption Assets.Default.Sound)
                                    elif ty.GenericTypeArguments.[0] = typeof<Song AssetTag> then (true, createValueOption Assets.Default.Song)
                                    elif ty.GenericTypeArguments.[0] = typeof<StaticModel AssetTag> then (true, createValueOption Assets.Default.StaticModel)
                                    elif ty.GenericTypeArguments.[0] = typeof<AnimatedModel AssetTag> then (true, createValueOption Assets.Default.AnimatedModel)
                                    elif ty.GenericTypeArguments.[0] = typeof<SoundDescriptor> then (true, createValueOption { Volume = Constants.Audio.SongVolumeDefault; Sound = Assets.Default.Sound })
                                    elif ty.GenericTypeArguments.[0] = typeof<SongDescriptor> then (true, createValueOption { FadeInTime = GameTime.zero; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = GameTime.zero; RepeatLimitOpt = None; Volume = Constants.Audio.SongVolumeDefault; Song = Assets.Default.Song })
                                    elif ty.GenericTypeArguments.[0] = typeof<ScatterType> then (true, createValueOption NoScatter)
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ array> then (true, createValueOption (Reflection.objsToArray ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ list> then (true, createValueOption (Reflection.objsToList ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FList> then (true, createValueOption (Reflection.objsToCollection typedefof<_ FList>.Name ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FQueue> then (true, createValueOption (Reflection.objsToCollection typedefof<_ FQueue>.Name ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FDeque> then (true, createValueOption (Reflection.objsToCollection typedefof<_ FDeque>.Name ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Set> then (true, createValueOption (Reflection.objsToSet ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ FSet> then (true, createValueOption (Reflection.objsToFSet ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<Map<_, _>> then (true, createValueOption (Reflection.pairsToMap ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<FMap<_, _>> then (true, createValueOption (Reflection.pairsToFMap ty.GenericTypeArguments.[0] []))
                                    elif ty.GenericTypeArguments.[0].IsGenericType && ty.GenericTypeArguments.[0].GetGenericTypeDefinition () = typedefof<_ Address> then
                                        let addressType = ty.GenericTypeArguments.[0]
                                        let makeFromStringFunction = addressType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
                                        let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((addressType.GetGenericArguments ()).[0])
                                        let addressValue = makeFromStringFunctionGeneric.Invoke (null, [|"^"|])
                                        (true, createValueOption addressValue)
                                    elif ty.GenericTypeArguments.[0] = typeof<Entity> then
                                        (true, createValueOption (Nu.Entity (Array.add "???" context.SelectedGroup.Names)))
                                    elif FSharpType.isNullTrueValue ty.GenericTypeArguments.[0] then
                                        (true, createValueOption null)
                                    else failwithumf ()
                                else (true, ty.GetProperty("None", BindingFlags.Public ||| BindingFlags.Static).GetValue(null))
                            else (false, value)
                        if ImGui.IsItemFocused () then context.FocusProperty ()
                        if isSome then
                            ImGui.SameLine ()
                            ImGui.PushID name
                            let (promoted2, edited3, value2) = World.imGuiEditProperty name ty.GenericTypeArguments.[0] (ty.GetProperty("Value").GetValue(value, [||])) context world
                            ImGui.PopID ()
                            let value = ty.GetMethod("Some", BindingFlags.Public ||| BindingFlags.Static).Invoke(null, [|value2|])
                            (promoted || promoted2, edited || edited2 || edited3, value)
                        else
                            ImGui.SameLine ()
                            ImGui.Text name
                            (promoted, edited || edited2, value)
                    elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ AssetTag> then
                        let converter = SymbolicConverter (false, None, ty, context.ToSymbolMemo, context.OfSymbolMemo)                        
                        let mutable valueStr = converter.ConvertToString value
                        let (edited2, value) =
                            if ImGui.InputText ("##text" + name, &valueStr, 4096u) then
                                try (true, converter.ConvertFromString valueStr)
                                with _ -> (false, value)
                            else (false, value)
                        if ImGui.IsItemFocused () then context.FocusProperty ()
                        let (edited3, value) =
                            if ImGui.BeginDragDropTarget () then
                                let (edited4, value) =
                                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                        match context.DragDropPayloadOpt with
                                        | Some payload ->
                                            try let valueStrEscaped = payload
                                                let valueStrUnescaped = String.unescape valueStrEscaped
                                                let value = converter.ConvertFromString valueStrUnescaped
                                                (true, value)
                                            with _ -> (false, value)
                                        | None -> (false, value)
                                    else (false, value)
                                ImGui.EndDragDropTarget ()
                                (edited4, value)
                            else (false, value)
                        ImGui.SameLine ()
                        ImGui.PushID ("##pickAsset" + name)
                        if ImGui.Button ("V", v2Dup 19.0f) then context.SearchAssetViewer ()
                        if ImGui.IsItemFocused () then context.FocusProperty ()
                        ImGui.PopID ()
                        ImGui.SameLine ()
                        ImGui.Text name
                        (promoted, edited || edited2 || edited3, value)
                    else
                        let converter = SymbolicConverter (false, None, ty, context.ToSymbolMemo, context.OfSymbolMemo)                        
                        let valueStr = converter.ConvertToString value
                        let prettyPrinter = (SyntaxAttribute.defaultValue ty).PrettyPrinter
                        let mutable valueStrPretty = PrettyPrinter.prettyPrint valueStr prettyPrinter
                        let lines = valueStrPretty |> Seq.filter ((=) '\n') |> Seq.length |> inc
                        let (edited2, value) =
                            if lines = 1 then
                                let mutable valueStr = valueStr
                                if ImGui.InputText (name, &valueStr, 131072u) then
                                    try (true, converter.ConvertFromString valueStr)
                                    with _ -> (false, value)
                                else (false, value)
                            else
                                ImGui.Text name
                                if ImGui.InputTextMultiline ("##" + name + "InputTextMultiline", &valueStrPretty, 131072u, v2 -1.0f (single (min 6 lines) * 13.0f + 7.0f)) then
                                    try (true, converter.ConvertFromString valueStrPretty)
                                    with _ -> (false, value)
                                else (false, value)
                        if ImGui.IsItemFocused () then context.FocusProperty ()
                        (promoted, edited || edited2, value)
                else (promoted, edited, value)

/// Override of Jolt DebugRenderer to render to ImGui.
type JoltDebugRendererImGui () =
    inherit DebugRenderer ()

    let segments = Dictionary<Color, Segment3 List> ()

    override this.DrawLine (start, stop, color) =
        let color = Color (color.ToVector4 ())
        let segment = Segment3 (start, stop)
        let magMaxSquared =
            Constants.Render.Body3dSegmentRenderMagnitudeMax *
            Constants.Render.Body3dSegmentRenderMagnitudeMax
        if segment.MagnitudeSquared < magMaxSquared then
            match segments.TryGetValue color with
            | (true, segmentList) -> segmentList.Add segment
            | (false, _) ->
                let segmentList = List ()
                segmentList.Add segment
                segments.Add (color, segmentList)

    override this.DrawText3D (_, _, _, _) =
        () // TODO: implement.

    /// Actually render all the stored drawing commands.
    member this.Flush (world : World) =
        let distanceMaxSquared =
            Constants.Render.Body3dSegmentRenderDistanceMax *
            Constants.Render.Body3dSegmentRenderDistanceMax
        for struct (color, segmentList) in segments.Pairs' do
            let segmentsNear = segmentList |> Seq.filter (fun segment -> ((segment.A + segment.Vector * 0.5f) - world.Eye3dCenter).MagnitudeSquared < distanceMaxSquared)
            World.imGuiSegments3d segmentsNear 1.0f color world
            segmentList.Clear ()
        this.NextFrame ()

/// More ImGui functions for the world.
[<AutoOpen>]
module WorldImGui2 =
    type World with

        /// Render the 2D physics via ImGui.
        static member imGuiRenderPhysics2d world =
            let segments = Dictionary<Color, struct (Vector2 * Vector2) List> ()
            let circles = Dictionary<struct (Color * single), Vector2 List> ()
            let physicsEngine2d = World.getPhysicsEngine2d world
            let renderContext =
                { new PhysicsEngine2dRenderContext with
                    override this.DrawLine (start : Vector2, stop : Vector2, color) =
                        match segments.TryGetValue color with
                        | (true, segmentList) -> segmentList.Add (start, stop)
                        | (false, _) -> segments.Add (color, List [struct (start, stop)])
                    override this.DrawCircle (center : Vector2, radius, color) =
                        match circles.TryGetValue struct (color, radius) with
                        | (true, circleList) -> circleList.Add center
                        | (false, _) -> circles.Add (struct (color, radius), List [center])
                    override _.EyeBounds = world.Eye2dBounds }
            physicsEngine2d.TryRender renderContext
            for struct (color, segmentList) in segments.Pairs' do
                World.imGuiSegments2d false segmentList 1.0f color world
                segmentList.Clear ()
            for struct (struct (color, radius), circleList) in circles.Pairs' do
                World.imGuiCircles2d false circleList radius false color world
                circleList.Clear ()

        /// Render the 3D physics via ImGui using the given settings.
        static member imGuiRenderPhysics3d (settings : DrawSettings) world =
            match World.getRendererPhysics3dOpt world with
            | Some debugRenderer ->
                let physicsEngine3d = World.getPhysicsEngine3d world
                let joltRenderer = debugRenderer :?> JoltDebugRendererImGui
                let renderContext =
                    { DebugRenderer = joltRenderer
                      DrawSettings = settings
                      EyeCenter = world.Eye3dCenter
                      EyeFrustum = world.Eye3dFrustumView }
                physicsEngine3d.TryRender renderContext
                joltRenderer.Flush world
            | None -> ()