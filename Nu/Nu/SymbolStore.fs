// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

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
            { SymbolStorePackageMap : (bool * Symbol) PackageMap }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module SymbolStore =

        let private tryLoadSymbol3 implicitDelimiters packageName (asset : Asset) =
            try let text = File.ReadAllText asset.FilePath
                let text = if implicitDelimiters then Symbol.OpenSymbolsStr + text + Symbol.CloseSymbolsStr else text
                try let symbol = Symbol.fromString text
                    Some (asset.AssetTag.AssetName, (implicitDelimiters, symbol))
                with exn ->
                    Log.info ^ "Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn
                    None
            with _ ->
                Log.info ^ "Failed to load symbol file '" + asset.FilePath + "' for package '" + packageName + "' due to: " + scstring exn
                None

        /// Try to load a symbol store package with the given name.
        let tryLoadSymbolStorePackage implicitDelimiters packageName symbolStore =
            match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
            | Right assetGraph ->
                match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Symbol) packageName assetGraph with
                | Right assets ->
                    let symbolOpts = List.map (tryLoadSymbol3 implicitDelimiters packageName) assets
                    let symbols = List.definitize symbolOpts
                    let symbolMapOpt = Map.tryFind packageName symbolStore.SymbolStorePackageMap
                    match symbolMapOpt with
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
    
        let private tryLoadSymbol implicitDelimiters (assetTag : AssetTag) symbolStore =
            let (assetMapOpt, symbolStore) =
                match Map.tryFind assetTag.PackageName symbolStore.SymbolStorePackageMap with
                | Some _ -> (Map.tryFind assetTag.PackageName symbolStore.SymbolStorePackageMap, symbolStore)
                | None ->
                    Log.info ^ "Loading symbol store package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly."
                    let symbolStore = tryLoadSymbolStorePackage implicitDelimiters assetTag.PackageName symbolStore
                    let symbolMap = Map.tryFind assetTag.PackageName symbolStore.SymbolStorePackageMap
                    (symbolMap, symbolStore)
            match assetMapOpt with
            | Some assetMap ->
                match Map.tryFind assetTag.AssetName assetMap with
                | Some (_, asset) -> (Some asset, symbolStore)
                | None -> (None, symbolStore)
            | None -> (None, symbolStore)
    
        /// Unload a symbolStore package with the given name.
        let unloadSymbolStorePackage packageName symbolStore =
            { symbolStore with SymbolStorePackageMap = Map.remove packageName symbolStore.SymbolStorePackageMap }
    
        /// Try to find a symbol with the given asset tag.
        let tryFindSymbol implicitDelimiters assetTag symbolStore =
            tryLoadSymbol implicitDelimiters assetTag symbolStore
            
        /// Try to find the symbols with the given asset tags.
        let tryFindSymbols implicitDelimiters assetTags world =
            List.foldBack
                (fun assetTag (symbolOpts, world) ->
                    let (symbolOpt, world) = tryFindSymbol implicitDelimiters assetTag world
                    match symbolOpt with
                    | Some symbol -> (Some symbol :: symbolOpts, world)
                    | None -> (None :: symbolOpts, world))
                assetTags
                ([], world)
    
        /// Reload all the assets in the symbolStore.
        let reloadSymbols symbolStore =
            let oldPackageMap = symbolStore.SymbolStorePackageMap
            let symbolStore = { symbolStore with SymbolStorePackageMap = Map.empty }
            Map.fold (fun (symbolStore : SymbolStore) packageName package ->
                Map.fold (fun (symbolStore : SymbolStore) assetName (implicitDelimiters, _) ->
                    let assetTag = { PackageName = packageName; AssetName = assetName }
                    tryLoadSymbol implicitDelimiters assetTag symbolStore |> snd)
                    symbolStore
                    package)
                symbolStore
                oldPackageMap
    
        /// The empty symbolStore.
        let empty =
            { SymbolStorePackageMap = Map.empty }

/// Provides references to Symbols that are loaded from files.
type SymbolStore = SymbolStoreModule.SymbolStore