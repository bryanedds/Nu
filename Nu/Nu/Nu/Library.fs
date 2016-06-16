// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen>]
module LibraryModule =

    /// Provides references to Symbols that are loaded from files.
    type [<ReferenceEquality>] Library =
        private
            { LibraryPackageMap : Symbol PackageMap }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Library =

        let private tryLoadLibraryAsset2 packageName (asset : Asset) =
            try let text = File.ReadAllText asset.FilePath
                try let symbol = Symbol.fromString text
                    Some (asset.AssetTag.AssetName, symbol)
                with exn ->
                    Log.info ^ "Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to:\r\n" + scstring exn
                    None
            with _ ->
                Log.info ^ "Failed to load symbol file '" + asset.FilePath + "' for package '" + packageName + "' due to:\r\n" + scstring exn
                None

        /// Try to load a library package with the given name.
        let tryLoadLibraryPackage packageName library =
            match AssetGraph.tryMakeFromFile Constants.Assets.AssetGraphFilePath with
            | Right assetGraph ->
                match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Library) packageName assetGraph with
                | Right assets ->
                    let optLibraryAssets = List.map (tryLoadLibraryAsset2 packageName) assets
                    let libraryAssets = List.definitize optLibraryAssets
                    let optLibraryAssetMap = Map.tryFind packageName library.LibraryPackageMap
                    match optLibraryAssetMap with
                    | Some libraryAssetMap ->
                        let libraryAssetMap = Map.addMany libraryAssets libraryAssetMap
                        let library = { library with LibraryPackageMap = Map.add packageName libraryAssetMap library.LibraryPackageMap }
                        library
                    | None ->
                        let libraryAssetMap = Map.ofSeq libraryAssets
                        let library = { library with LibraryPackageMap = Map.add packageName libraryAssetMap library.LibraryPackageMap }
                        library
                | Left error ->
                    Log.info ^ "Library package load failed due to unloadable assets '" + error + "' for package '" + packageName + "'."
                    library
            | Left error ->
                Log.info ^ "Library package load failed due to unloadable asset graph due to: '" + error
                library
    
        let private tryLoadLibraryAsset (assetTag : AssetTag) library =
            let (optAssetMap, library) =
                match Map.tryFind assetTag.PackageName library.LibraryPackageMap with
                | Some _ -> (Map.tryFind assetTag.PackageName library.LibraryPackageMap, library)
                | None ->
                    Log.info ^ "Loading library package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly."
                    let library = tryLoadLibraryPackage assetTag.PackageName library
                    let libraryAssetMap = Map.tryFind assetTag.PackageName library.LibraryPackageMap
                    (libraryAssetMap, library)
            (Option.bind (fun assetMap -> Map.tryFind assetTag.AssetName assetMap) optAssetMap, library)
    
        /// Unload a library package with the given name.
        let unloadLibraryPackage packageName library =
            { library with LibraryPackageMap = Map.remove packageName library.LibraryPackageMap }
    
        /// Try to find a symbol with the given asset tag.
        let tryFindLibraryAsset assetTag library =
            tryLoadLibraryAsset assetTag library
    
        /// Reload all the assets in the library.
        let reloadLibraryAssets library =
            let oldPackageMap = library.LibraryPackageMap
            let library = { library with LibraryPackageMap = Map.empty }
            List.fold
                (fun library packageName -> tryLoadLibraryPackage packageName library)
                library
                (Map.toKeyList oldPackageMap)
    
        /// The empty library.
        let empty =
            { LibraryPackageMap = Map.empty }