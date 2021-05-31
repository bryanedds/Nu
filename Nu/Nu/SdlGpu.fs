// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace SDL2
open System
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module SDL =

    type GPU_Rect =
        val mutable x : single
        val mutable y : single
        val mutable w : single
        val mutable h : single

    type [<Flags>] GPU_BlendPresetEnum =
        | GPU_BLEND_NORMAL = 0
        | GPU_BLEND_PREMULTIPLIED_ALPHA = 1
        | GPU_BLEND_MULTIPLY = 2
        | GPU_BLEND_ADD = 3
        | GPU_BLEND_SUBTRACT = 4
        | GPU_BLEND_MOD_ALPHA = 5
        | GPU_BLEND_SET_ALPHA = 6
        | GPU_BLEND_SET = 7
        | GPU_BLEND_NORMAL_KEEP_ALPHA = 8
        | GPU_BLEND_NORMAL_ADD_ALPHA = 9
        | GPU_BLEND_NORMAL_FACTOR_ALPHA = 10

    type [<Flags>] GPU_InitFlagEnum =
        | GPU_INIT_ENABLE_VSYNC = 0x1u
        | GPU_INIT_DISABLE_VSYNC = 0x2u
        | GPU_INIT_DISABLE_DOUBLE_BUFFER = 0x4u
        | GPU_INIT_DISABLE_AUTO_VIRTUAL_RESOLUTION = 0x8u
        | GPU_INIT_REQUEST_COMPATIBILITY_PROFILE = 0x10u
        | GPU_INIT_USE_ROW_BY_ROW_TEXTURE_UPLOAD_FALLBACK = 0x20u
        | GPU_INIT_USE_COPY_TEXTURE_UPLOAD_FALLBACK = 0x40u

    type [<Flags>] GPU_FlipEnum =
        | GPU_FLIP_NONE = 0x0u
        | GPU_FLIP_HORIZONTAL = 0x1u
        | GPU_FLIP_VERTICAL = 0x2u

    type GPU_RendererEnum =
        | GPU_RENDERER_UNKNOWN = 0
        | GPU_RENDERER_OPENGL_1_BASE = 1
        | GPU_RENDERER_OPENGL_1 = 2
        | GPU_RENDERER_OPENGL_2 = 3
        | GPU_RENDERER_OPENGL_3 = 4
        | GPU_RENDERER_OPENGL_4 = 5
        | GPU_RENDERER_GLES_1 = 11
        | GPU_RENDERER_GLES_2 = 12
        | GPU_RENDERER_GLES_3 = 13
        | GPU_RENDERER_D3D9 = 21
        | GPU_RENDERER_D3D10 = 22
        | GPU_RENDERER_D3D11 = 23

[<RequireQualifiedAccess>]
module SDL_gpu =

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetPreInitFlags (uint flags)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr public GPU_Init (uint width, uint height, uint flags)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Quit ()

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetInitWindow (uint windowid)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr public GPU_CreateTargetFromWindow (uint windowid)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr public GPU_GetCurrentRenderer ()

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_ResetRendererState ()

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern bool public GPU_SetFullscreen (bool fullscreen, bool desktopres)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetAnchor (IntPtr image, single x, single y)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetDefaultAnchor (single x, single y)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr public GPU_CopyImageFromSurface (IntPtr surface)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_FreeImage (IntPtr image)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetRGBA (IntPtr image, uint r, uint g, uint b, uint a)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetBlendMode (IntPtr image, SDL.GPU_BlendPresetEnum mode)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetBlending (IntPtr image, bool enable)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Rectangle (IntPtr screen, single l, single t, single r, single b, SDL.SDL_Color c)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_RectangleFilled (IntPtr screen, single l, single t, single r, single b, SDL.SDL_Color c)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetLineThickness (single thickness)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Line (IntPtr screen, single x1, single y1, single x2, single y2, SDL.SDL_Color c)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Pixel (IntPtr screen, single x, single y, SDL.SDL_Color c)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_BlitRect (IntPtr image, SDL.GPU_Rect& source, IntPtr screen, SDL.GPU_Rect& dest)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_BlitRectX (IntPtr image, SDL.GPU_Rect& source, IntPtr screen, SDL.GPU_Rect& dest, single degrees, single pivotx, single pivoty, SDL.GPU_FlipEnum flip)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Clear (IntPtr target)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_ClearColor (IntPtr target, SDL.SDL_Color color)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_FlushBlitBuffer ()

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Flip (IntPtr target)

    let GPU_QueryImageSize (image : IntPtr, width : int byref, height : int byref) =
        let bytes = Array.zeroCreate<int> 9 // assumes pointers are 64-bit
        Marshal.Copy (image, bytes, 0, 0)
        let wh = bytes.[8]
        width <- int (wh >>> 16)
        height <- int (int16 wh)

    let GPU_QueryWindowFlags (renderer : IntPtr) =
        let bytes = Array.zeroCreate<int> 17 // assumes pointers are 64-bit
        Marshal.Copy (renderer, bytes, 0, 0)
        bytes.[16]