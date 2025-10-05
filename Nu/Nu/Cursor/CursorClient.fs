namespace Nu
open System
open System.Collections.Generic
open System.IO
open SDL2
open Prime
open Nu

/// The type of a system cursor.
type CursorType =
    | DefaultCursor
    | ArrowCursor
    | IBeamCursor
    | WaitCursor
    | CrosshairCursor
    | WaitArrowCursor
    | SizeNwseCursor
    | SizeNeswCursor
    | SizeWestEastCursor
    | SizeNorthSouthCursor
    | SizeAllCursor
    | NoCursor
    | HandCursor
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
    
    static member make () = { StubCursorClient = () }

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
        { SystemCursors : Dictionary<SDL.SDL_SystemCursor, nativeint>
          CursorPackages : Packages<nativeint, unit>
          mutable CursorType : CursorType }

    /// Make an SdlCursorClient.
    static member make () =
        { SystemCursors = Dictionary ()
          CursorPackages = Packages StringComparer.Ordinal
          CursorType = DefaultCursor }

    static member private tryLoadCursorAsset (asset : Asset) =
        match PathF.GetExtensionLower asset.FilePath with
        | CursorExtension _ ->
            let surface = SDL_image.IMG_Load asset.FilePath
            if surface <> 0n then

                // load hotspot from .cur file. NOTE: SDL3 reads cursor hotspots as SDLSurface properties
                // (https://github.com/libsdl-org/SDL_image/pull/519/files), however SDL2 does not do that. Therefore,
                // we have to read the hotspot properties manually.
                // NOTE: reference for .cur file format here - https://www.daubnet.com/en/file-format-cur
                use fileStream = new FileStream (asset.FilePath, FileMode.Open, FileAccess.Read, FileShare.None, 0) // 0 = disable buffering
                use binaryReader = new BinaryReader (fileStream)
                fileStream.Seek (10, SeekOrigin.Begin) |> ignore
                let hotspotX = binaryReader.ReadUInt16 () // hotspotX of first cursor is bytes 10 to 11 (little endian)
                let hotspotY = binaryReader.ReadUInt16 () // hotspotY of first cursor is bytes 12 to 13 (little endian)
                
                // create cursor and free the surface that the cursor copied from
                let cursor = SDL.SDL_CreateColorCursor (surface, int hotspotX, int hotspotY)
                SDL.SDL_FreeSurface surface
                if cursor <> 0n
                then Some cursor
                else
                    Log.warn $"Could not create cursor for '{asset.FilePath}' due to '{SDL.SDL_GetError ()}'."
                    None
            
            else
                Log.warn $"Could not load cursor surface for '{asset.FilePath}' due to '{SDL.SDL_GetError ()}'."
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
                    cursorClient.CursorPackages.[packageName] <- cursorPackage
                    cursorPackage)

            // keep existing cursors for which a newer one does not exist, free otherwise
            let assetsToKeep = HashSet ()
            for KeyValue (assetName, (lastWriteTime, asset, cursor)) in cursorPackage.Assets do
                let lastWriteTime' =
                    try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                    with exn -> Log.warn $"Asset file write time read error due to: {scstring exn}"; DateTimeOffset.MinValue.DateTime
                if lastWriteTime = lastWriteTime'
                then assetsToKeep.Add assetName |> ignore
                else SDL.SDL_FreeCursor cursor
                
            // load newly found assets that are not kept
            for asset in assetsCollected do
                if not (assetsToKeep.Contains asset.AssetTag.AssetName) then
                    match SdlCursorClient.tryLoadCursorAsset asset with
                    | Some cursorAsset ->
                        let lastWriteTime =
                            try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                            with exn -> Log.warn $"Asset file write time read error due to: {scstring exn}"; DateTimeOffset.MinValue.DateTime
                        cursorPackage.Assets.[asset.AssetTag.AssetName] <- (lastWriteTime, asset, cursorAsset)
                    | None -> ()

        // handle error case
        | Left failedAssetNames ->
            Log.warn $"Cursor package load failed due to unloadable assets '{failedAssetNames}' for package '{packageName}'."

    static member private tryGetCursorAsset (assetTag : AssetTag) cursorClient =
        match Dictionary.tryFind assetTag.PackageName cursorClient.CursorPackages with
        | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
        | None ->
            Log.info $"Loading Cursor package '{assetTag.PackageName}' for asset '{assetTag.AssetName}' on the fly."
            SdlCursorClient.tryLoadCursorPackage assetTag.PackageName cursorClient
            match Dictionary.tryFind assetTag.PackageName cursorClient.CursorPackages with
            | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
            | None -> None
            
    static member private setSystemCursor (systemCursor : SDL.SDL_SystemCursor) cursorClient =
        match Dictionary.tryFind systemCursor cursorClient.SystemCursors with
        | Some cursor -> SDL.SDL_SetCursor cursor
        | None ->
            let cursor = SDL.SDL_CreateSystemCursor systemCursor
            if cursor <> nativeint 0 then
                cursorClient.SystemCursors[systemCursor] <- cursor
                SDL.SDL_SetCursor cursor
            else Log.warn $"Failed to create system cursor '{systemCursor}' due to: {SDL.SDL_GetError ()}"

    static member private getCursorType cursorClient =
        cursorClient.CursorType

    static member private setCursorType cursorType (cursorClient : SdlCursorClient) =
        match cursorType with
        | DefaultCursor -> SDL.SDL_SetCursor (SDL.SDL_GetDefaultCursor ())
        | ArrowCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_ARROW cursorClient
        | IBeamCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_IBEAM cursorClient
        | WaitCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_WAIT cursorClient
        | CrosshairCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_CROSSHAIR cursorClient
        | WaitArrowCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_WAITARROW cursorClient
        | SizeNwseCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZENWSE cursorClient
        | SizeNeswCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZENESW cursorClient
        | SizeWestEastCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZEWE cursorClient
        | SizeNorthSouthCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZENS cursorClient
        | SizeAllCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_SIZEALL cursorClient
        | NoCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_NO cursorClient
        | HandCursor -> SdlCursorClient.setSystemCursor SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_HAND cursorClient
        | UserDefinedCursor assetTag ->
            match SdlCursorClient.tryGetCursorAsset assetTag cursorClient with
            | Some cursor -> SDL.SDL_SetCursor cursor
            | None -> Log.info $"Set cursor failed due to unloadable assets for '{scstring assetTag}'."
        cursorClient.CursorType <- cursorType

    static member private getCursorVisible =
        SDL.SDL_ShowCursor SDL.SDL_QUERY = SDL.SDL_ENABLE
        
    static member private setCursorVisible visible =
        SDL.SDL_ShowCursor (if visible then SDL.SDL_ENABLE else SDL.SDL_DISABLE) |> ignore

    interface CursorClient with

        member cursorClient.CursorType
            with get () = SdlCursorClient.getCursorType cursorClient
            and set value = SdlCursorClient.setCursorType value cursorClient

        member cursorClient.CursorVisible
            with get () = SdlCursorClient.getCursorVisible
            and set value = SdlCursorClient.setCursorVisible value

        member cursorClient.LoadCursorPackage packageName =
            SdlCursorClient.tryLoadCursorPackage packageName cursorClient

        member cursorClient.UnloadCursorPackage packageName =
            match Dictionary.tryFind packageName cursorClient.CursorPackages with
            | Some package ->
                for asset in package.Assets do SDL.SDL_FreeCursor (__c asset.Value)
                cursorClient.CursorPackages.Remove packageName |> ignore
            | None -> ()

        member cursorClient.ReloadCursorAssets () =
            for systemCursor in cursorClient.SystemCursors.Values do
                SDL.SDL_FreeCursor systemCursor
            cursorClient.SystemCursors.Clear ()
            for packageName in cursorClient.CursorPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq do
                SdlCursorClient.tryLoadCursorPackage packageName cursorClient
            SdlCursorClient.setCursorType cursorClient.CursorType cursorClient

        member cursorClient.CleanUp () =
            for systemCursor in cursorClient.SystemCursors.Values do
                SDL.SDL_FreeCursor systemCursor
            for package in cursorClient.CursorPackages.Values do
                for (_, _, cursor) in package.Assets.Values do
                    SDL.SDL_FreeCursor cursor