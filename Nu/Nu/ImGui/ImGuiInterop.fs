// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace ImGuiNET
open System
open System.Numerics
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

/// ImGui Rect structure.
[<Struct; StructLayout (LayoutKind.Sequential)>]
type Rect =

    /// Minimum point of the rectangle.
    val mutable Min : Vector2

    /// Maximum point of the rectangle.
    val mutable Max : Vector2

/// ImGui internal operations.
[<RequireQualifiedAccess>]
module ImGuiInternal =

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igClearDragDrop")>]
    extern void ImGui_ClearDragDrop ()

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igClearActiveID")>]
    extern void ImGui_ClearActiveID ()

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igGetDragDropPayload")>]
    extern nativeint ImGui_GetDragDropPayload ()

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igSetKeyOwner")>]
    extern void ImGui_SetKeyOwner (ImGuiKey key, uint32 ownerId, ImGuiInputFlags flags)

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igDockBuilderGetCentralNode")>]
    extern nativeint DockBuilder_GetCentralNode (uint32 dockSpaceId)

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ImGuiDockNode_Rect")>]
    extern void DockNode_GetRect (nativeint rect, nativeint node)

    /// Claim a key for the given owner, keeping it from imgui's own uses of unowned keys - such as alt toggling the menu
    /// layer and arrows moving the nav cursor. A claim lasts while the key's held, so make it every frame.
    let setKeyOwner key ownerId =
        ImGui_SetKeyOwner (key, ownerId, ImGuiInputFlags.None)

    /// Clear the drag drop action.
    let tryCancelDragDrop () =
        if ImGui_GetDragDropPayload () <> 0n then
            ImGui_ClearActiveID ()
            ImGui_ClearDragDrop ()

    /// Try to get the bounds of the central dock node in the given dock space.
    let tryGetCentralDockNodeBounds dockSpaceId =
        let centralNode = DockBuilder_GetCentralNode dockSpaceId
        if centralNode <> nativeint 0 then
            let io = ImGui.GetIO ()
            let displaySize = io.DisplaySize
            let mutable rect = Unchecked.defaultof<Rect>
            DockNode_GetRect (NativePtr.toNativeInt &&rect, centralNode)
            let min = v2i (int rect.Min.X) (int rect.Min.Y)
            let max = v2i (int rect.Max.X) (int rect.Max.Y)
            let size = max - min
            let min' = v2i min.X (int displaySize.Y - max.Y)
            let bounds = box2i min' size
            Some bounds
        else None