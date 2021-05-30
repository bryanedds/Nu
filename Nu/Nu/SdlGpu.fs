// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace SDL2
open System
open System.Runtime.InteropServices
open Prime
open Nu

type GPU_Rect =
    val X : float
    val Y : float
    val W : float
    val H : float

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
    extern IntPtr public GPU_Init (uint width, uint height, uint flags)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Quit ()

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr public GPU_CopyImageFromSurface (IntPtr surface)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_FreeImage (IntPtr image)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetBlendMode (IntPtr image, GPU_BlendPresetEnum mode)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern bool public GPU_SetFullscreen (bool fullscreen, bool desktop_res)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetLineThickness (float thickness)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern GPU_Rect public GPU_MakeRect (float x, float y, float w, float h)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_ClearColor (IntPtr target, SDL.SDL_Color color)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Flip (IntPtr target)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Blit (IntPtr image, GPU_Rect& rect, IntPtr screen, float x, float y)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_BlitRotate (IntPtr image, GPU_Rect& rect, IntPtr screen, float x, float y, float degrees)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_BlitScale (IntPtr image, GPU_Rect& rect, IntPtr screen, float x, float y, float sx, float sy)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_BlitTransform (IntPtr image, GPU_Rect& rect, IntPtr screen, float x, float y, float rotation, float sx, float sy)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_BlitTransformX (IntPtr image, GPU_Rect& rect, IntPtr screen, float x, float y, float pivot_x, float pivot_y, float rotation, float sx, float sy)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Rectangle (IntPtr screen, float l, float t, float r, float b, SDL.SDL_Color c)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_RectangleFilled (IntPtr screen, float l, float t, float r, float b, SDL.SDL_Color c)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Line (IntPtr screen, float x1, float y1, float x2, float y2, SDL.SDL_Color c)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_Pixel (IntPtr screen, float x, float y, SDL.SDL_Color c)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetBlending (IntPtr image, bool enable)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_FlushBlitBuffer ()

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetAnchor (IntPtr image, float x, float y)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetPreInitFlags (uint flags)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetDefaultAnchor (float x, float y)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr public GPU_CreateTargetFromWindow (uint windowid)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_SetInitWindow (uint winid)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr public GPU_InitRenderer (int rid, uint w, uint h, uint flags)

    [<DllImport("SDL2_gpu", CallingConvention = CallingConvention.Cdecl)>]
    extern void public GPU_ResetRendererState ()