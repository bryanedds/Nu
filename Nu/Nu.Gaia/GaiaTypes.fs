// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu
open Nu.Gaia.Design

type WorldChanger = World -> World
type WorldChangers = WorldChanger List

type DragEntityState =
    | DragEntityPosition of Vector2 * Vector2 * Entity
    | DragEntityRotation of Vector2 * Vector2 * Entity
    | DragEntityNone

type DragCameraState =
    | DragCameraPosition of Vector2 * Vector2
    | DragCameraNone

type EditorState =
    { TargetDir : string
      RightClickPosition : Vector2
      DragEntityState : DragEntityState
      DragCameraState : DragCameraState
      SelectedGroup : Group
      FilePaths : Map<Group Address, string>
      RefreshHierarchyViewRequested : bool } // HACK: make sure hierarchy view isn't updated more than once per frame.

type SavedState =
    { BinaryFilePath : string
      UseGameplayScreen : bool
      UseImperativeExecution : bool }

/// Globals needed to sync Nu with WinForms.
[<RequireQualifiedAccess>]
module Globals =

    let EditorGuid = Gen.id
    let mutable Form = Unchecked.defaultof<GaiaForm>
    let mutable World = Unchecked.defaultof<World>
    let mutable Screen = Simulants.DefaultScreen
    let mutable PastWorlds : World list = []
    let mutable FutureWorlds : World list = []
    let mutable WorldChangers = WorldChangers ()
    let mutable SelectEntity : Entity -> GaiaForm -> World -> unit = Unchecked.defaultof<_>

    let pushPastWorld pastWorld =
        let pastWorld = Nu.World.shelve pastWorld
        PastWorlds <- pastWorld :: PastWorlds
        FutureWorlds <- []

[<RequireQualifiedAccess>]
module Wgl =

    [<RequireQualifiedAccess>]
    module private Private =
    
        [<DllImport "user32.dll">]
        extern IntPtr GetDC (IntPtr hwnd);

        [<DllImport "user32.dll">]
        extern IntPtr GetWindowDC (IntPtr hwnd)

        [<DllImport "gdi32.dll">]
        extern int ChoosePixelFormat (IntPtr hdc, IntPtr ppfd)

        [<DllImport "gdi32.dll">]
        extern int SetPixelFormat (IntPtr hdc, int iPixelFormat, IntPtr ppfd)

        [<DllImport "opengl32.dll">] 
        extern IntPtr wglCreateContext (IntPtr hdc)

        [<DllImport "opengl32.dll">] 
        extern bool wglMakeCurrent (IntPtr hdc, IntPtr hglrc)

        [<DllImport "opengl32.dll">] 
        extern bool wglSwapBuffers (IntPtr hdc, uint layer)

    type PixelFormatDescriptor =
        struct
           val mutable nSize:           uint16
           val mutable nVersion:        uint16
           val mutable dwFlags:         uint
           val mutable iPixelType:      byte
           val mutable cColorBits:      byte
           val mutable cRedBits:        byte
           val mutable cRedShift:       byte
           val mutable cGreenBits:      byte
           val mutable cGreenShift:     byte
           val mutable cBlueBits:       byte
           val mutable cBlueShift:      byte
           val mutable cAlphaBits:      byte
           val mutable cAlphaShift:     byte
           val mutable cAccumBits:      byte
           val mutable cAccumRedBits:   byte
           val mutable cAccumGreenBits: byte
           val mutable cAccumBlueBits:  byte
           val mutable cAccumAlphaBits: byte
           val mutable cDepthBits:      byte
           val mutable cStencilBits:    byte
           val mutable cAuxBuffers:     byte
           val mutable iLayerType:      sbyte 
           val mutable bReserved:       byte
           val mutable dwLayerMask:     uint
           val mutable dwVisibleMask:   uint
           val mutable dwDamageMask:    uint
           end

    let PreferredPfd =
        let mutable pfd = PixelFormatDescriptor ()
        pfd.nSize <-          uint16 sizeof<PixelFormatDescriptor>
        pfd.nVersion <-       1us
        pfd.dwFlags <-        0x04u (*PFD_DRAW_TO_WINDOW*) ||| 0x20u (*PFD_SUPPORT_OPENGL*) ||| 0x01u (*PFD_DOUBLEBUFFER*)
        pfd.iPixelType <-     byte 0x01 (*PFD_TYPE_RGBA*)   // color type
        pfd.cColorBits <-     byte 24                       // 24-bit color depth 
        pfd.cRedBits <-       byte 0                        //
        pfd.cRedShift <-      byte 0                        //
        pfd.cGreenBits <-     byte 0                        //
        pfd.cGreenShift <-    byte 0                        //
        pfd.cBlueBits <-      byte 0                        //
        pfd.cBlueShift <-     byte 0                        //
        pfd.cAlphaBits <-     byte 0                        //
        pfd.cAlphaShift <-    byte 0                        //
        pfd.cAccumBits <-     byte 0                        //
        pfd.cAccumRedBits <-  byte 0                        //
        pfd.cAccumGreenBits <-byte 0                        //
        pfd.cAccumBlueBits <- byte 0                        //
        pfd.cAccumAlphaBits <-byte 0                        //
        pfd.cDepthBits <-     byte 32                       // 32-bit z-buffer
        pfd.cStencilBits <-   byte 0                        // no stencil buffer
        pfd.cAuxBuffers <-    byte 0                        // no auxiliary buffer
        pfd.iLayerType <-     sbyte 0                       //
        pfd.bReserved <-      byte 0                        //
        pfd.dwLayerMask <-    0u                            //
        pfd.dwVisibleMask <-  0u                            //
        pfd.dwDamageMask <-   0u                            //
        pfd

    let GetDC hwd = Private.GetDC hwd

    let GetWindowDC hwd = Private.GetWindowDC hwd

    let TryChoosePixelFormat hdc pfd =
        let pfdPtr = Marshal.AllocHGlobal (sizeof<PixelFormatDescriptor>)
        Marshal.StructureToPtr<PixelFormatDescriptor> (pfd, pfdPtr, false)
        let pfdOpt =
            let pixelFormat = Private.ChoosePixelFormat (hdc, pfdPtr)
            if pixelFormat <> 0 then
                let pfd = Marshal.PtrToStructure<PixelFormatDescriptor> pfdPtr
                Some (pixelFormat, pfd)
            else None
        Marshal.FreeHGlobal pfdPtr
        pfdOpt

    let SetPixelFormat hdc iPixelFormat pfd =
        let pfdPtr = Marshal.AllocHGlobal (sizeof<PixelFormatDescriptor>)
        Marshal.StructureToPtr<PixelFormatDescriptor> (pfd, pfdPtr, false)
        let result = Private.SetPixelFormat (hdc, iPixelFormat, pfdPtr)
        Marshal.FreeHGlobal pfdPtr
        result

    let CreateContext hdc = Private.wglCreateContext hdc

    let MakeCurrent hdc hglrc = Private.wglMakeCurrent (hdc, hglrc)

    let SwapBuffers hdc = Private.wglSwapBuffers (hdc, 0u)