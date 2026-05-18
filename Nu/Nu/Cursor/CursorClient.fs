namespace Nu
open System
open System.Collections.Generic
open System.IO
open FSharp.NativeInterop
open SDL
open Prime
open Nu

/// The type of a system cursor.
type CursorType =
    
    /// Default cursor. Usually an arrow.
    | DefaultCursor
    
    /// Text selection. Usually an I-beam.
    | TextCursor
    
    /// Wait. Usually an hourglass or watch or spinning ball.
    | WaitCursor
    
    /// Crosshair.
    | CrosshairCursor
    
    /// Program is busy but still interactive. Usually it's WaitCursor with an arrow.
    | ProgressCursor
    
    /// Double arrow pointing northwest and southeast.
    | ResizeNwseCursor
    
    /// Double arrow pointing northeast and southwest.
    | ResizeNeswCursor
    
    /// Double arrow pointing west and east.
    | ResizeEastWestCursor
    
    /// Double arrow pointing north and south.
    | ResizeNorthSouthCursor
    
    /// Four pointed arrow pointing north, south, east, and west.
    | MoveCursor
    
    /// Not permitted. Usually a slashed circle or crossbones.
    | NotAllowedCursor
    
    /// Pointer that indicates a link. Usually a pointing hand.
    | PointerCursor
    
    /// Window resize top-left. This may be a single arrow or a double arrow like ResizeNwseCursor.
    | ResizeNorthWestCursor
    
    /// Window resize top. This may be a single arrow or a double arrow like ResizeNorthSouthCursor.
    | ResizeNorthCursor
    
    /// Window resize top-right. This may be a single arrow or a double arrow like ResizeNeswCursor.
    | ResizeNorthEastCursor
    
    /// Window resize right. This may be a single arrow or a double arrow like ResizeEastWestCursor.
    | ResizeEastCursor
    
    /// Window resize bottom-right. This may be a single arrow or a double arrow like ResizeNwseCursor.
    | ResizeSouthEastCursor
    
    /// Window resize bottom. This may be a single arrow or a double arrow like ResizeNorthSouthCursor.
    | ResizeSouthCursor
    
    /// Window resize bottom-left. This may be a single arrow or a double arrow like ResizeNeswCursor.
    | ResizeSouthWestCursor
    
    /// Window resize left. This may be a single arrow or a double arrow like ResizeEastWestCursor.
    | ResizeWestCursor
    
    /// User-defined cursor loaded from a cursor asset.
    | UserDefinedCursor of AssetTag : Cursor AssetTag

/// Instructs the system cursor display behavior.
type CursorClient =
    
    /// The type of the cursor.
    abstract CursorType : CursorType with get, set

    /// The cursor's visibility.
    abstract CursorVisible : bool with get, set

    /// Load a cursor package.
    abstract LoadCursorPackage : packageName : string -> unit
    
    /// Unload a cursor package.
    abstract UnloadCursorPackage : packageName : string -> unit

    /// Reload cursor assets.
    abstract ReloadCursorAssets : unit -> unit
    
    /// Handle cursor clean up by freeing all loaded cursor assets.
    abstract CleanUp : unit -> unit

/// The stub implementation of CursorClient.
type [<ReferenceEquality>] StubCursorClient =
    private
        { StubCursorClient : unit }
    
    /// Make a StubCursorClient.
    static member make () =
        { StubCursorClient = () }

    interface CursorClient with
        member cursorClient.CursorType with get () = DefaultCursor and set _ = ()
        member cursorClient.CursorVisible with get () = true and set _ = ()
        member cursorClient.LoadCursorPackage _ = ()
        member cursorClient.UnloadCursorPackage _ = ()
        member cursorClient.ReloadCursorAssets () = ()
        member cursorClient.CleanUp () = ()

/// The SDL implementation of CursorClient.
type [<ReferenceEquality>] SdlCursorClient =
    private
        { SystemCursors : Dictionary<SDL_SystemCursor, SDL_Cursor nativeptr>
          CursorPackages : Packages<SDL_Cursor nativeptr, unit>
          mutable CursorType : CursorType }

    static member private tryLoadCursorAsset (asset : Asset) =
        match PathF.GetExtensionLower asset.FilePath with
        | CursorExtension _ ->
            let filePathSdl = PathF.GetFullPath asset.FilePath
            let surface = SDL3_image.IMG_Load filePathSdl
            if not (NativePtr.isNullPtr surface) then

                // create cursor. hotspot parameters (0, 0) here are overridden by the surface properties
                // SDL_PROP_SURFACE_HOTSPOT_X_NUMBER and SDL_PROP_SURFACE_HOTSPOT_Y_NUMBER set by Image.Load
                let cursor = SDL3.SDL_CreateColorCursor (surface, 0, 0)
                SDL3.SDL_DestroySurface surface // the cursor stores a copy of the frame data, the surface can be destroyed here
                if not (NativePtr.isNullPtr cursor)
                then Some cursor
                else
                    Log.warn ("Could not create cursor for '" + filePathSdl + "' due to: '" + SDL3.SDL_GetError ())
                    None
            
            else
                Log.warn ("Could not load cursor for '" + filePathSdl + "' due to: '" + SDL3.SDL_GetError ())
                None
        | _ -> None

    static member private tryLoadCursorPackage packageName cursorClient =

        // make new asset graph and load its assets
        let assetGraph = AssetGraph.makeFromFileOpt Assets.Global.AssetGraphFilePath
        match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Cursor) packageName assetGraph with
        | Right assetsCollected ->

            // find or create cursor package
            let cursorPackage =
                Dictionary.tryFind packageName cursorClient.CursorPackages
                |> Option.defaultWith (fun () ->
                    let cursorPackage = { Assets = Dictionary StringComparer.Ordinal; PackageState = () }
                    cursorClient.CursorPackages[packageName] <- cursorPackage
                    cursorPackage)

            // keep existing cursors for which a newer one does not exist, free otherwise
            let assetsToKeep = HashSet ()
            for KeyValue (assetName, (lastWriteTime, asset, cursor)) in cursorPackage.Assets do
                let lastWriteTime' =
                    try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                    with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                if lastWriteTime = lastWriteTime'
                then assetsToKeep.Add assetName |> ignore
                else SDL3.SDL_DestroyCursor cursor
                
            // load newly found assets that are not kept
            for asset in assetsCollected do
                if not (assetsToKeep.Contains asset.AssetTag.AssetName) then
                    match SdlCursorClient.tryLoadCursorAsset asset with
                    | Some cursorAsset ->
                        let lastWriteTime =
                            try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                            with exn -> Log.warn ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                        cursorPackage.Assets[asset.AssetTag.AssetName] <- (lastWriteTime, asset, cursorAsset)
                    | None -> ()

        // handle error case
        | Left failedAssetNames ->
            Log.info ("Audio package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")

    static member private tryGetCursorAsset (assetTag : AssetTag) cursorClient =
        match Dictionary.tryFind assetTag.PackageName cursorClient.CursorPackages with
        | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
        | None ->
            Log.info ("Loading Cursor package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
            SdlCursorClient.tryLoadCursorPackage assetTag.PackageName cursorClient
            match Dictionary.tryFind assetTag.PackageName cursorClient.CursorPackages with
            | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
            | None -> None
            
    static member private setSystemCursor (systemCursor : SDL_SystemCursor) cursorClient =
        match Dictionary.tryFind systemCursor cursorClient.SystemCursors with
        | Some cursor -> SDL3.SDL_SetCursor cursor
        | None ->
            let cursor = SDL3.SDL_CreateSystemCursor systemCursor
            if not (NativePtr.isNullPtr cursor) then
                cursorClient.SystemCursors[systemCursor] <- cursor
                SDL3.SDL_SetCursor cursor
            else Log.warn ("Failed to create system cursor '" + scstring systemCursor + "' due to: " + SDL3.SDL_GetError ()); true

    static member private getCursorType cursorClient =
        cursorClient.CursorType

    static member private setCursorType cursorType (cursorClient : SdlCursorClient) =
        match cursorType with
        | DefaultCursor -> SDL3.SDL_SetCursor (SDL3.SDL_GetDefaultCursor ()) // is a shared system cursor instance of SDL_SystemCursor.SDL_SYSTEM_CURSOR_DEFAULT unless SDL is initialized with the SDL_HINT_MOUSE_DEFAULT_SYSTEM_CURSOR hint, which we don't use
        | TextCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_TEXT cursorClient
        | WaitCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_WAIT cursorClient
        | CrosshairCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_CROSSHAIR cursorClient
        | ProgressCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_PROGRESS cursorClient
        | ResizeNwseCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_NWSE_RESIZE cursorClient
        | ResizeNeswCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_NESW_RESIZE cursorClient
        | ResizeEastWestCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_EW_RESIZE cursorClient
        | ResizeNorthSouthCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_NS_RESIZE cursorClient
        | MoveCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_MOVE cursorClient
        | NotAllowedCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_NOT_ALLOWED cursorClient
        | PointerCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_POINTER cursorClient
        | ResizeNorthWestCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_NW_RESIZE cursorClient
        | ResizeNorthCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_N_RESIZE cursorClient
        | ResizeNorthEastCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_NE_RESIZE cursorClient
        | ResizeEastCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_E_RESIZE cursorClient
        | ResizeSouthEastCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_SE_RESIZE cursorClient
        | ResizeSouthCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_S_RESIZE cursorClient
        | ResizeSouthWestCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_SW_RESIZE cursorClient
        | ResizeWestCursor -> SdlCursorClient.setSystemCursor SDL_SystemCursor.SDL_SYSTEM_CURSOR_W_RESIZE cursorClient
        | UserDefinedCursor assetTag ->
            match SdlCursorClient.tryGetCursorAsset assetTag cursorClient with
            | Some cursor -> SDL3.SDL_SetCursor cursor
            | None -> Log.warn ("UserDefinedCursor message failed due to unloadable assets for '" + scstring assetTag + "'."); true
        |> fun result -> if not result then Log.warn ("Failed to set cursor type '" + scstring cursorType + "' due to: " + SDL3.SDL_GetError ())
        cursorClient.CursorType <- cursorType

    /// Make an SdlCursorClient.
    static member make () =
        { SystemCursors = Dictionary ()
          CursorPackages = Packages StringComparer.Ordinal
          CursorType = DefaultCursor }

    interface CursorClient with

        member cursorClient.CursorType
            with get () = SdlCursorClient.getCursorType cursorClient
            and set value = SdlCursorClient.setCursorType value cursorClient

        member cursorClient.CursorVisible
            with get () = SDL3.SDL_CursorVisible ()
            and set value =
                if not (if value then SDL3.SDL_ShowCursor () else SDL3.SDL_HideCursor ()) then
                    Log.warn ("Failed to set cursor visibility to '" + scstring value + "' due to: " + SDL3.SDL_GetError ())

        member cursorClient.LoadCursorPackage packageName =
            SdlCursorClient.tryLoadCursorPackage packageName cursorClient

        member cursorClient.UnloadCursorPackage packageName =
            match Dictionary.tryFind packageName cursorClient.CursorPackages with
            | Some package ->
                for asset in package.Assets do SDL3.SDL_DestroyCursor (__c asset.Value)
                cursorClient.CursorPackages.Remove packageName |> ignore
            | None -> ()

        member cursorClient.ReloadCursorAssets () =
            for systemCursor in cursorClient.SystemCursors.Values do
                SDL3.SDL_DestroyCursor systemCursor
            cursorClient.SystemCursors.Clear ()
            for packageName in cursorClient.CursorPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq do
                SdlCursorClient.tryLoadCursorPackage packageName cursorClient
            SdlCursorClient.setCursorType cursorClient.CursorType cursorClient

        member cursorClient.CleanUp () =
            for systemCursor in cursorClient.SystemCursors.Values do
                SDL3.SDL_DestroyCursor systemCursor
            for package in cursorClient.CursorPackages.Values do
                for (_, _, cursor) in package.Assets.Values do
                    SDL3.SDL_DestroyCursor cursor