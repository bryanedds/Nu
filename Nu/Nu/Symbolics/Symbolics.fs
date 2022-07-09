// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

/// Symbol loading metadata.
/// OPTIMIZATION: made this a struct because, well, it's two booleans. If we add non-trivial fields to this, it will be
/// okay to make it a reference type.
type [<StructuralEquality; StructuralComparison; Struct>] SymbolLoadMetadata =
    { ImplicitDelimiters : bool
      StripCsvHeader : bool }

[<RequireQualifiedAccess>]
module Symbolics =

    /// Provides references to symbols that are loaded from files.
    type [<ReferenceEquality; NoComparison>] Symbolics =
        private
            { SymbolPackages : (SymbolLoadMetadata * Symbol) Packages }

    let private tryLoadSymbol3 metadata packageName (asset : Symbol Asset) =
        try let text = File.ReadAllText asset.FilePath
            match Path.GetExtension asset.FilePath with
            | ".csv" ->
                try let symbol = Symbol.ofStringCsv metadata.StripCsvHeader text (Some asset.FilePath)
                    Some (asset.AssetTag.AssetName, (metadata, symbol))
                with exn ->
                    Log.info ("Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn)
                    None
            | _ ->
                let text =
                    if metadata.ImplicitDelimiters
                    then Symbol.OpenSymbolsStr + text + Symbol.CloseSymbolsStr
                    else text
                try let symbol = Symbol.ofString text (Some asset.FilePath)
                    Some (asset.AssetTag.AssetName, (metadata, symbol))
                with exn ->
                    Log.info ("Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn)
                    None
        with _ ->
            Log.info ("Failed to load symbol file '" + asset.FilePath + "' for package '" + packageName + "' due to: " + scstring exn)
            None

    /// Try to load a symbol package with the given name.
    let tryLoadSymbolPackage packageName metadata symbolics =
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Symbol) packageName assetGraph with
            | Right assets ->
                let assets = List.map Asset.specialize<Symbol> assets
                let symbolOpts = List.map (tryLoadSymbol3 metadata packageName) assets
                let symbols = List.definitize symbolOpts
                match Dictionary.tryFind packageName symbolics.SymbolPackages with
                | Some symbolDict ->
                    for (key, value) in symbols do symbolDict.Assign (key, value)
                    symbolics.SymbolPackages.Assign (packageName, symbolDict)
                | None ->
                    let symbolDict = dictPlus StringComparer.Ordinal symbols
                    symbolics.SymbolPackages.Assign (packageName, symbolDict)
            | Left error ->
                Log.info ("Symbol package load failed due to unloadable assets '" + error + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Symbol package load failed due to unloadable asset graph due to: '" + error)

    let tryLoadSymbol (assetTag : Symbol AssetTag) metadata symbolics =
        match Dictionary.tryFind assetTag.PackageName symbolics.SymbolPackages with
        | Some assets -> Dictionary.tryFind assetTag.AssetName assets
        | None ->
            Log.info ("Loading Symbol package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
            tryLoadSymbolPackage assetTag.PackageName metadata symbolics
            match Dictionary.tryFind assetTag.PackageName symbolics.SymbolPackages with
            | Some assets -> Dictionary.tryFind assetTag.AssetName assets
            | None -> None

    /// Unload a symbol package with the given name.
    let unloadSymbolPackage packageName symbolics =
        symbolics.SymbolPackages.Remove packageName |> ignore

    /// Try to find a symbol with the given asset tag.
    let tryFindSymbol assetTag metadata symbolics =
        tryLoadSymbol assetTag metadata symbolics |> Option.map snd
        
    /// Try to find the symbols with the given asset tags.
    let tryFindSymbols metadata assetTags symbolics =
        List.foldBack
            (fun assetTag (symbolOpts, symbolics) ->
                match tryFindSymbol metadata assetTag symbolics with
                | Some symbol -> (Some symbol :: symbolOpts, symbolics)
                | None -> (None :: symbolOpts, symbolics))
            assetTags
            ([], symbolics)

    /// Reload all the symbols in symbolics.
    let reloadSymbols symbolics =
        let symbolPackages = Dictionary<_, _> symbolics.SymbolPackages
        symbolics.SymbolPackages.Clear ()
        for packageEntry in symbolPackages do
            let packageName = packageEntry.Key
            let packageValue = packageEntry.Value
            for assetEntry in packageValue do
                let metadata = fst assetEntry.Value
                tryLoadSymbolPackage packageName metadata symbolics

    /// Empty symbolics.
    let makeEmpty () =
        { SymbolPackages = dictPlus StringComparer.Ordinal [] }

/// Provides references to symbols that are loaded from files.
type Symbolics = Symbolics.Symbolics