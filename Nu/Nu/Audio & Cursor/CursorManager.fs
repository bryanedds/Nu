namespace Nu

open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open FSharp.NativeInterop
open SDL2
open Prime

/// A cursor.
type Cursor =
    | Cursor of AssetTag : Cursor AssetTag
    | CursorDefault
    | CursorArrow
    | CursorIBeam
    | CursorWait
    | CursorCrosshair
    | CursorWaitArrow
    | CursorSizeNWSE 
    | CursorSizeNESW 
    | CursorSizeWE
    | CursorSizeNS
    | CursorSizeAll
    | CursorNo
    | CursorHand

/// The cursor manager. Represents the cursor subsystem of Nu generally.
type CursorManager =
    
    /// The current cursor.
    abstract Cursor : Cursor with get, set

    /// Load a cursor package.
    abstract LoadCursorPackage : packageName : string -> unit
    
    /// Unload a cursor package.
    abstract UnloadCursorPackage : packageName : string -> unit

    /// Reload all cursor packages.
    abstract ReloadCursorPackages : unit -> unit
    
    /// Handle cursor clean up by freeing all loaded cursor assets.
    abstract CleanUp : unit -> unit

/// The stub implementation of CursorManager.
type [<ReferenceEquality>] StubCursorManager =
    private
        { StubCursorManager : unit }
    
    static member make () = { StubCursorManager = () }

    interface CursorManager with
        member _.Cursor with get () = CursorDefault and set _ = ()
        member _.LoadCursorPackage _ = ()
        member _.UnloadCursorPackage _ = ()
        member _.ReloadCursorPackages () = ()
        member _.CleanUp () = ()

/// The SDL implementation of CursorManager.
type [<ReferenceEquality>] SdlCursorManager =
    private
        { CursorPackages : Packages<nativeint, unit>
          SystemCursors : Dictionary<SDL.SDL_SystemCursor, nativeint>
          mutable CurrentCursor : Cursor }

    [<DllImport("SDL2", CallingConvention = CallingConvention.Cdecl)>]
    static extern nativeint SDL_GetDefaultCursor () // Missing in SDL2#? https://wiki.libsdl.org/SDL2/SDL_GetDefaultCursor

    /// Make an SdlCursorManager.
    static member make () =
        { CursorPackages = Packages StringComparer.Ordinal
          SystemCursors = Dictionary ()
          CurrentCursor = CursorDefault }

    static member private tryLoadCursorAsset (asset : Asset) =
        match PathF.GetExtensionLower asset.FilePath with
        | CursorExtension _ -> // In case you can't find a good editor for .cur files, try Greenfish Icon Editor.
            let surface = SDL_image.IMG_Load asset.FilePath
            if surface <> 0n then

                // NOTE: SDL3 reads cursor hotspots as SDLSurface properties: https://github.com/libsdl-org/SDL_image/pull/519/files
                // but SDL2 does not do that. Therefore, we read hotspot properties ourselves.
                use fileStream = new FileStream (asset.FilePath, FileMode.Open, FileAccess.Read, FileShare.None, 0) // 0 = Disable buffering
                use binaryReader = new BinaryReader (fileStream)
                // reference: https://www.daubnet.com/en/file-format-cur
                fileStream.Seek (10, SeekOrigin.Begin) |> ignore
                let hotspotX = binaryReader.ReadUInt16 () // HotspotX of first cursor is bytes 10 to 11 (little endian)
                let hotspotY = binaryReader.ReadUInt16 () // HotspotY of first cursor is bytes 12 to 13 (little endian)
                
                // create cursor and free the surface that the cursor copied from
                let cursor = SDL.SDL_CreateColorCursor (surface, int hotspotX, int hotspotY)
                SDL.SDL_FreeSurface surface
                if cursor <> 0n then
                    Some cursor
                else
                    Log.warn $"Could not create cursor for '{asset.FilePath}' due to '{SDL.SDL_GetError ()}'."
                    None
            
            else
                Log.warn $"Could not load cursor surface for '{asset.FilePath}' due to '{SDL.SDL_GetError ()}'."
                None
        | _ -> None

    static member private tryLoadPackage packageName cursorManager =
        // make new asset graph and load its assets
        let assetGraph = AssetGraph.makeFromFileOpt Assets.Global.AssetGraphFilePath
        match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Cursor) packageName assetGraph with
        | Right assetsCollected ->

            // find or create cursor package
            let cursorPackage =
                Dictionary.tryFind packageName cursorManager.CursorPackages
                |> Option.defaultWith (fun () ->
                    let cursorPackage = { Assets = Dictionary StringComparer.Ordinal; PackageState = () }
                    cursorManager.CursorPackages.[packageName] <- cursorPackage
                    cursorPackage)

            // keep existing cursors for which a newer one does not exist, free otherwise
            let assetsToKeep = HashSet ()
            for KeyValue (assetName, (lastWriteTime, asset, cursor)) in cursorPackage.Assets do
                let lastWriteTime' =
                    try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                    with exn -> Log.warn ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                if lastWriteTime = lastWriteTime'
                then assetsToKeep.Add assetName |> ignore
                else SDL.SDL_FreeCursor cursor
                
            // load newly found assets that are not kept
            for asset in assetsCollected do
                if not (assetsToKeep.Contains asset.AssetTag.AssetName) then
                    match SdlCursorManager.tryLoadCursorAsset asset with
                    | Some cursorAsset ->
                        let lastWriteTime =
                            try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                            with exn -> Log.warn $"Asset file write time read error due to: {scstring exn}"; DateTimeOffset.MinValue.DateTime
                        cursorPackage.Assets.[asset.AssetTag.AssetName] <- (lastWriteTime, asset, cursorAsset)
                    | None -> ()

        // handle error case
        | Left failedAssetNames ->
            Log.warn $"Cursor package load failed due to unloadable assets '{failedAssetNames}' for package '{packageName}'."

    static member private tryGetCursorAsset (assetTag : AssetTag) cursorManager =
        match Dictionary.tryFind assetTag.PackageName cursorManager.CursorPackages with
        | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
        | None ->
            Log.info $"Loading Cursor package '{assetTag.PackageName}' for asset '{assetTag.AssetName}' on the fly."
            SdlCursorManager.tryLoadPackage assetTag.PackageName cursorManager
            match Dictionary.tryFind assetTag.PackageName cursorManager.CursorPackages with
            | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
            | None -> None
            
    static member private setSystemCursor (systemCursor : SDL.SDL_SystemCursor) cursorManager =
        match Dictionary.tryFind systemCursor cursorManager.SystemCursors with
        | Some cursor -> SDL.SDL_SetCursor cursor
        | None ->
            let cursor = SDL.SDL_CreateSystemCursor systemCursor
            if cursor <> 0n then
                cursorManager.SystemCursors[systemCursor] <- cursor
                SDL.SDL_SetCursor cursor
            else Log.warn $"Failed to create system cursor '{systemCursor}': {SDL.SDL_GetError ()}"
        
    interface CursorManager with
        member cursorManager.Cursor
            with get () = cursorManager.CurrentCursor
            and set c =
                cursorManager.CurrentCursor <- c

                match c with
                | Cursor assetTag ->
                    match SdlCursorManager.tryGetCursorAsset assetTag cursorManager with
                    | Some cursor -> SDL.SDL_SetCursor cursor
                    | None -> Log.info $"Set cursor failed due to unloadable assets for '{scstring assetTag}'."
                | CursorDefault -> SDL.SDL_SetCursor <| SDL_GetDefaultCursor ()
                | CursorArrow -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_ARROW cursorManager
                | CursorIBeam -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_IBEAM cursorManager
                | CursorWait -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_WAIT cursorManager
                | CursorCrosshair -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_CROSSHAIR cursorManager
                | CursorWaitArrow -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_WAITARROW cursorManager
                | CursorSizeNWSE -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZENWSE cursorManager
                | CursorSizeNESW -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZENESW cursorManager
                | CursorSizeWE -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZEWE cursorManager
                | CursorSizeNS -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZENS cursorManager
                | CursorSizeAll -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZEALL cursorManager
                | CursorNo -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_NO cursorManager
                | CursorHand -> SdlCursorManager.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_HAND cursorManager

        member cursorManager.LoadCursorPackage packageName = SdlCursorManager.tryLoadPackage packageName cursorManager

        member cursorManager.UnloadCursorPackage packageName =
            match Dictionary.tryFind packageName cursorManager.CursorPackages with
            | Some package ->
                for KeyValue (_, (_, _, cursor)) in package.Assets do
                    SDL.SDL_FreeCursor cursor
                cursorManager.CursorPackages.Remove packageName |> ignore
            | None -> ()

        member cursorManager.ReloadCursorPackages () =
            for packageName in cursorManager.CursorPackages.Keys do
                SdlCursorManager.tryLoadPackage packageName cursorManager

        member cursorManager.CleanUp () =
            for package in cursorManager.CursorPackages.Values do
                for (_, _, cursor) in package.Assets.Values do
                    SDL.SDL_FreeCursor cursor
            for systemCursor in cursorManager.SystemCursors.Values do
                SDL.SDL_FreeCursor systemCursor