// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace ImGuiNET
open System
open System.Numerics
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<Struct; StructLayout (LayoutKind.Sequential)>]
type ImVec2 =
    val mutable X : single
    val mutable Y : single

[<Struct; StructLayout (LayoutKind.Sequential)>]
type ImRect =
    val mutable Min : ImVec2
    val mutable Max : ImVec2

[<RequireQualifiedAccess>]
module ImGuiInternal =

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "igDockBuilderGetCentralNode")>]
    extern nativeint DockBuilder_GetCentralNode (uint32)

    [<DllImport("cimgui", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ImGuiDockNode_Rect")>]
    extern void DockNode_GetRect (nativeint, nativeint)

    /// Try to get the bounds of the central dock node in the given dock space.
    let tryGetCentralDockNodeBounds dockSpaceId =
        let centralNode = DockBuilder_GetCentralNode dockSpaceId
        if centralNode <> nativeint 0 then
            let io = ImGui.GetIO ()
            let displaySize = io.DisplaySize
            let mutable rect = Unchecked.defaultof<ImRect>
            DockNode_GetRect (NativePtr.toNativeInt &&rect, centralNode)
            let min = v2i (int rect.Min.X) (int rect.Min.Y)
            let max = v2i (int rect.Max.X) (int rect.Max.Y)
            let size = max - min
            let min' = v2i min.X (int displaySize.Y - max.Y)
            let bounds = box2i min' size
            Some bounds
        else None