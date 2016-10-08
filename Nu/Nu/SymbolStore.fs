// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen>]
module SymbolStoreModule =

    /// Provides references to Symbols that are loaded from files.
    type [<ReferenceEquality>] SymbolStore =
        private
            { SymbolStorePackageMap : Symbol PackageMap }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module SymbolStore =

        let private tryLoadSymbol2 packageName (asset : Asset) =
            try let text = File.ReadAllText asset.FilePath
                try let symbol = Symbol.fromString text
                    Some (asset.AssetTag.AssetName, symbol)
                with exn ->
                    Log.info ^ "Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn
                    None
            with _ ->
                Log.info ^ "Failed to load symbol file '" + asset.FilePath + "' for package '" + packageName + "' due to: " + scstring exn
                None

        /// Try to load a symbol store package with the given name.
        let tryLoadSymbolStorePackage packageName symbolStore =
            match AssetGraph.tryMakeFromFile Constants.Assets.AssetGraphFilePath with
            | Right assetGraph ->
                match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Symbol) packageName assetGraph with
                | Right assets ->
                    let optSymbols = List.map (tryLoadSymbol2 packageName) assets
                    let symbols = List.definitize optSymbols
                    let optSymbolMap = Map.tryFind packageName symbolStore.SymbolStorePackageMap
                    match optSymbolMap with
                    | Some symbolMap ->
                        let symbolMap = Map.addMany symbols symbolMap
                        let symbolStore = { symbolStore with SymbolStorePackageMap = Map.add packageName symbolMap symbolStore.SymbolStorePackageMap }
                        symbolStore
                    | None ->
                        let symbolMap = Map.ofSeq symbols
                        let symbolStore = { symbolStore with SymbolStorePackageMap = Map.add packageName symbolMap symbolStore.SymbolStorePackageMap }
                        symbolStore
                | Left error ->
                    Log.info ^ "Symbol store package load failed due to unloadable assets '" + error + "' for package '" + packageName + "'."
                    symbolStore
            | Left error ->
                Log.info ^ "Symbol store package load failed due to unloadable asset graph due to: '" + error
                symbolStore
    
        let private tryLoadSymbol (assetTag : AssetTag) symbolStore =
            let (optAssetMap, symbolStore) =
                match Map.tryFind assetTag.PackageName symbolStore.SymbolStorePackageMap with
                | Some _ -> (Map.tryFind assetTag.PackageName symbolStore.SymbolStorePackageMap, symbolStore)
                | None ->
                    Log.info ^ "Loading symbol store package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly."
                    let symbolStore = tryLoadSymbolStorePackage assetTag.PackageName symbolStore
                    let symbolMap = Map.tryFind assetTag.PackageName symbolStore.SymbolStorePackageMap
                    (symbolMap, symbolStore)
            (Option.bind (fun assetMap -> Map.tryFind assetTag.AssetName assetMap) optAssetMap, symbolStore)
    
        /// Unload a symbolStore package with the given name.
        let unloadSymbolStorePackage packageName symbolStore =
            { symbolStore with SymbolStorePackageMap = Map.remove packageName symbolStore.SymbolStorePackageMap }
    
        /// Try to find a symbol with the given asset tag.
        let tryFindSymbol assetTag symbolStore =
            tryLoadSymbol assetTag symbolStore
            
        /// Try to find the symbols with the given asset tags.
        let tryFindSymbols assetTags world =
            List.foldBack
                (fun assetTag (optSymbols, world) ->
                    let (optSymbol, world) = tryFindSymbol assetTag world
                    match optSymbol with
                    | Some symbol -> (Some symbol :: optSymbols, world)
                    | None -> (None :: optSymbols, world))
                assetTags
                ([], world)
    
        /// Reload all the assets in the symbolStore.
        let reloadSymbols symbolStore =
            let oldPackageMap = symbolStore.SymbolStorePackageMap
            let symbolStore = { symbolStore with SymbolStorePackageMap = Map.empty }
            List.fold
                (fun symbolStore packageName -> tryLoadSymbolStorePackage packageName symbolStore)
                symbolStore
                (Map.toKeyList oldPackageMap)
    
        /// The empty symbolStore.
        let empty =
            { SymbolStorePackageMap = Map.empty }

/// Provides references to Symbols that are loaded from files.
type SymbolStore = SymbolStoreModule.SymbolStore