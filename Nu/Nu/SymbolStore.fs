// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

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
            // TODO: P1: consider making key a struct tuple.
            { SymbolStorePackageMap : (bool * Symbol) PackageMap }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module SymbolStore =

        let private tryLoadSymbol3 implicitDelimiters packageName (asset : Symbol Asset) =
            try let text = File.ReadAllText asset.FilePath
                match Path.GetExtension asset.FilePath with
                | ".csv" ->
                    try let symbol = Csv.readSymbolFromCsv text (Some asset.FilePath)
                        Some (asset.AssetTag.AssetName, (implicitDelimiters, symbol))
                    with exn ->
                        Log.info ("Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn)
                        None
                | _ ->
                    let text = if implicitDelimiters then Symbol.OpenSymbolsStr + text + Symbol.CloseSymbolsStr else text
                    try let symbol = Symbol.fromString text
                        Some (asset.AssetTag.AssetName, (implicitDelimiters, symbol))
                    with exn ->
                        Log.info ("Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn)
                        None
            with _ ->
                Log.info ("Failed to load symbol file '" + asset.FilePath + "' for package '" + packageName + "' due to: " + scstring exn)
                None

        /// Try to load a symbol store package with the given name.
        let tryLoadSymbolStorePackage implicitDelimiters packageName symbolStore =
            match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
            | Right assetGraph ->
                match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Symbol) packageName assetGraph with
                | Right assets ->
                    let assets = List.map Asset.specialize<Symbol> assets
                    let symbolOpts = List.map (tryLoadSymbol3 implicitDelimiters packageName) assets
                    let symbols = List.definitize symbolOpts
                    let symbolMapOpt = UMap.tryFindFast packageName symbolStore.SymbolStorePackageMap
                    if FOption.isSome symbolMapOpt then
                        let symbolMap = FOption.get symbolMapOpt
                        let symbolMap = UMap.addMany symbols symbolMap
                        let symbolStore = { symbolStore with SymbolStorePackageMap = UMap.add packageName symbolMap symbolStore.SymbolStorePackageMap }
                        symbolStore
                    else
                        let symbolMap = UMap.makeFromSeq Constants.SymbolStore.SymbolMapConfig symbols
                        let symbolStore = { symbolStore with SymbolStorePackageMap = UMap.add packageName symbolMap symbolStore.SymbolStorePackageMap }
                        symbolStore
                | Left error ->
                    Log.info ("Symbol store package load failed due to unloadable assets '" + error + "' for package '" + packageName + "'.")
                    symbolStore
            | Left error ->
                Log.info ("Symbol store package load failed due to unloadable asset graph due to: '" + error)
                symbolStore
    
        let private tryLoadSymbol implicitDelimiters (assetTag : Symbol AssetTag) symbolStore =
            let (assetMapOpt, symbolStore) =
                if UMap.containsKey assetTag.PackageName symbolStore.SymbolStorePackageMap
                then (UMap.tryFindFast assetTag.PackageName symbolStore.SymbolStorePackageMap, symbolStore)
                else
                    Log.info ("Loading symbol store package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                    let symbolStore = tryLoadSymbolStorePackage implicitDelimiters assetTag.PackageName symbolStore
                    let symbolMap = UMap.tryFindFast assetTag.PackageName symbolStore.SymbolStorePackageMap
                    (symbolMap, symbolStore)
            if FOption.isSome assetMapOpt then
                let assetOpt = UMap.tryFindFast assetTag.AssetName (FOption.get assetMapOpt)
                (FOption.map snd assetOpt, symbolStore)
            else (FOption.none (), symbolStore)
    
        /// Unload a symbolStore package with the given name.
        let unloadSymbolStorePackage packageName symbolStore =
            { symbolStore with SymbolStorePackageMap = UMap.remove packageName symbolStore.SymbolStorePackageMap }
    
        /// Try to find a symbol with the given asset tag.
        let tryFindSymbol implicitDelimiters assetTag symbolStore =
            // NOTE: converting canonical option here as I've not yet decided to allow FOption to
            // leak into the engine's public interface...
            let (symbolOpt, symbolStore) = tryLoadSymbol implicitDelimiters assetTag symbolStore
            (FOption.ToOpt symbolOpt, symbolStore)
            
        /// Try to find the symbols with the given asset tags.
        let tryFindSymbols implicitDelimiters assetTags symbolStore =
            List.foldBack
                (fun assetTag (symbolOpts, symbolStore) ->
                    let (symbolOpt, symbolStore) = tryFindSymbol implicitDelimiters assetTag symbolStore
                    match symbolOpt with
                    | Some symbol -> (Some symbol :: symbolOpts, symbolStore)
                    | None -> (None :: symbolOpts, symbolStore))
                assetTags
                ([], symbolStore)
    
        /// Reload all the assets in the symbolStore.
        let reloadSymbols symbolStore =
            let oldPackageMap = symbolStore.SymbolStorePackageMap
            let symbolStore = { symbolStore with SymbolStorePackageMap = UMap.makeEmpty (UMap.getConfig symbolStore.SymbolStorePackageMap) }
            UMap.fold (fun (symbolStore : SymbolStore) packageName package ->
                UMap.fold (fun (symbolStore : SymbolStore) assetName (implicitDelimiters, _) ->
                    let assetTag = AssetTag.make<Symbol> packageName assetName
                    tryLoadSymbol implicitDelimiters assetTag symbolStore |> snd)
                    symbolStore
                    package)
                symbolStore
                oldPackageMap
    
        /// The empty symbolStore.
        let empty = { SymbolStorePackageMap = UMap.makeEmpty Constants.SymbolStore.SymbolMapConfig }

/// Provides references to Symbols that are loaded from files.
type SymbolStore = SymbolStoreModule.SymbolStore