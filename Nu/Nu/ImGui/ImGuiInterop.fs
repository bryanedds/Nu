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

[<Struct; StructLayout (LayoutKind.Sequential)>]
type Rect =
    val mutable Min : Vector2
    val mutable Max : Vector2

[<RequireQualifiedAccess>]
module ImGuiInternal =

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igClearDragDrop")>]
    extern void ImGui_ClearDragDrop ()

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igClearActiveID")>]
    extern void ImGui_ClearActiveID ()

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igGetDragDropPayload")>]
    extern nativeint ImGui_GetDragDropPayload ()

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igDockBuilderGetCentralNode")>]
    extern nativeint DockBuilder_GetCentralNode (uint32)

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ImGuiDockNode_Rect")>]
    extern void DockNode_GetRect (nativeint, nativeint)

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