// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

[<AutoOpen>]
module SymbolStoreModule =

    /// Symbol loading metadata.
    type SymbolLoadMetadata =
        { ImplicitDelimiters : bool
          StripCsvHeader : bool }

    /// Provides references to Symbols that are loaded from files.
    type [<ReferenceEquality>] SymbolStore =
        private
            { SymbolPackages : struct (SymbolLoadMetadata * Symbol) Packages }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module SymbolStore =

        let private tryLoadSymbol3 metadata packageName (asset : Symbol Asset) =
            try let text = File.ReadAllText asset.FilePath
                match Path.GetExtension asset.FilePath with
                | ".csv" ->
                    try let symbol = Symbol.fromStringCsv metadata.StripCsvHeader text (Some asset.FilePath)
                        Some (asset.AssetTag.AssetName, struct (metadata, symbol))
                    with exn ->
                        Log.info ("Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn)
                        None
                | _ ->
                    let text =
                        if metadata.ImplicitDelimiters
                        then Symbol.OpenSymbolsStr + text + Symbol.CloseSymbolsStr
                        else text
                    try let symbol = Symbol.fromString text (Some asset.FilePath)
                        Some (asset.AssetTag.AssetName, struct (metadata, symbol))
                    with exn ->
                        Log.info ("Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn)
                        None
            with _ ->
                Log.info ("Failed to load symbol file '" + asset.FilePath + "' for package '" + packageName + "' due to: " + scstring exn)
                None

        /// Try to load a symbol store package with the given name.
        let tryLoadSymbolPackage packageName metadata symbolStore =
            match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
            | Right assetGraph ->
                match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Symbol) packageName assetGraph with
                | Right assets ->
                    let assets = List.map Asset.specialize<Symbol> assets
                    let symbolOpts = List.map (tryLoadSymbol3 metadata packageName) assets
                    let symbols = List.definitize symbolOpts
                    match Dictionary.tryFind packageName symbolStore.SymbolPackages with
                    | Some symbolDict ->
                        for (key, value) in symbols do symbolDict.ForceAdd (key, value)
                        symbolStore.SymbolPackages.ForceAdd (packageName, symbolDict)
                    | None ->
                        let symbolDict = dictPlus symbols
                        symbolStore.SymbolPackages.ForceAdd (packageName, symbolDict)
                | Left error ->
                    Log.info ("Symbol store package load failed due to unloadable assets '" + error + "' for package '" + packageName + "'.")
            | Left error ->
                Log.info ("Symbol store package load failed due to unloadable asset graph due to: '" + error)

        let tryLoadSymbol (assetTag : Symbol AssetTag) metadata symbolStore =
            let assetsOpt =
                if symbolStore.SymbolPackages.ContainsKey assetTag.PackageName then
                    Dictionary.tryFind assetTag.PackageName symbolStore.SymbolPackages
                else
                    Log.info ("Loading symbol package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                    tryLoadSymbolPackage assetTag.PackageName metadata symbolStore
                    Dictionary.tryFind assetTag.PackageName symbolStore.SymbolPackages
            Option.bind (fun assets -> Dictionary.tryFind assetTag.AssetName assets) assetsOpt
    
        /// Unload a symbolStore package with the given name.
        let unloadSymbolPackage packageName symbolStore =
            symbolStore.SymbolPackages.Remove packageName |> ignore
    
        /// Try to find a symbol with the given asset tag.
        let tryFindSymbol assetTag metadata symbolStore =
            tryLoadSymbol assetTag metadata symbolStore |> Option.map snd'
            
        /// Try to find the symbols with the given asset tags.
        let tryFindSymbols metadata assetTags symbolStore =
            List.foldBack
                (fun assetTag (symbolOpts, symbolStore) ->
                    match tryFindSymbol metadata assetTag symbolStore with
                    | Some symbol -> (Some symbol :: symbolOpts, symbolStore)
                    | None -> (None :: symbolOpts, symbolStore))
                assetTags
                ([], symbolStore)
    
        /// Reload all the assets in the symbol store.
        let reloadSymbols symbolStore =
            let symbolPackages = Dictionary<_, _> symbolStore.SymbolPackages
            symbolStore.SymbolPackages.Clear ()
            for packageEntry in symbolPackages do
                let packageName = packageEntry.Key
                let packageValue = packageEntry.Value
                for assetEntry in packageValue do
                    let metadata = fst' assetEntry.Value
                    tryLoadSymbolPackage packageName metadata symbolStore
    
        /// The empty symbol store.
        let makeEmpty () =
            { SymbolPackages = dictPlus [] }

/// Provides references to Symbols that are loaded from files.
type SymbolStore = SymbolStoreModule.SymbolStore