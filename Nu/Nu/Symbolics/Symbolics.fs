// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open Prime

/// Symbol loading metadata.
/// OPTIMIZATION: made this a struct because, well, it's two booleans. If we add non-trivial fields to this, it will be
/// okay to make it a reference type.
type [<Struct>] SymbolLoadMetadata =
    { ImplicitDelimiters : bool
      StripCsvHeader : bool }

[<RequireQualifiedAccess>]
module Symbolics =

    /// Provides references to symbols that are loaded from files.
    type [<ReferenceEquality>] Symbolics =
        private
            { SymbolPackages : Packages<SymbolLoadMetadata * Symbol, unit> }

    let private tryLoadSymbol3 metadata packageName (asset : Asset) =
        try let text = File.ReadAllText asset.FilePath
            match PathF.GetExtensionLower asset.FilePath with
            | CsvExtension _ ->
                try let symbol = Symbol.ofStringCsv metadata.StripCsvHeader text (Some asset.FilePath)
                    Some (metadata, symbol)
                with exn ->
                    Log.info ("Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn)
                    None
            | _ ->
                let text =
                    if metadata.ImplicitDelimiters
                    then Symbol.OpenSymbolsStr + text + Symbol.CloseSymbolsStr
                    else text
                try let symbol = Symbol.ofString text (Some asset.FilePath)
                    Some (metadata, symbol)
                with exn ->
                    Log.info ("Failed to convert text in file '" + asset.FilePath + "' for package '" + packageName + "' to symbol due to: " + scstring exn)
                    None
        with exn ->
            Log.info ("Failed to load symbol file '" + asset.FilePath + "' for package '" + packageName + "' due to: " + scstring exn)
            None

    /// Attempt to load a symbol package with the given name.
    let tryLoadSymbolPackage packageName metadata symbolics =
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Symbol) packageName assetGraph with
            | Right assets ->
                let symbolPackage =
                    match Dictionary.tryFind packageName symbolics.SymbolPackages with
                    | Some symbolPackage -> symbolPackage
                    | None ->
                        let symbolPackage = { Assets = dictPlus StringComparer.Ordinal []; PackageState = () }
                        symbolics.SymbolPackages.[packageName] <- symbolPackage
                        symbolPackage
                for asset in assets do
                    match tryLoadSymbol3 metadata packageName asset with
                    | Some symbol ->
                        let lastWriteTime =
                            try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                            with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                        symbolPackage.Assets.[asset.AssetTag.AssetName] <- (lastWriteTime, asset.FilePath, symbol)
                    | None -> ()
            | Left error ->
                Log.info ("Symbol package load failed due to unloadable assets '" + error + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Symbol package load failed due to unloadable asset graph due to: '" + error)

    /// Attempt to load a symbol with the given asset tag.
    let tryLoadSymbol (assetTag : Symbol AssetTag) metadata symbolics =
        match Dictionary.tryFind assetTag.PackageName symbolics.SymbolPackages with
        | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
        | None ->
            Log.info ("Loading Symbol package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
            tryLoadSymbolPackage assetTag.PackageName metadata symbolics
            match Dictionary.tryFind assetTag.PackageName symbolics.SymbolPackages with
            | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
            | None -> None

    /// Unload a symbol package with the given name.
    let unloadSymbolPackage packageName symbolics =
        symbolics.SymbolPackages.Remove packageName |> ignore<bool>

    /// Try to find a symbol with the given asset tag.
    let tryGetSymbol assetTag metadata symbolics =
        tryLoadSymbol assetTag metadata symbolics |> Option.map snd

    /// Try to find the symbols with the given asset tags.
    let tryGetSymbols metadata assetTags symbolics =
        List.foldBack
            (fun assetTag symbols ->
                match tryGetSymbol metadata assetTag symbolics with
                | Some symbol -> symbol :: symbols
                | None -> symbols)
            assetTags
            []

    /// Reload all the symbols in symbolics.
    let reloadSymbols symbolics =
        let symbolPackages = Dictionary<_, _> symbolics.SymbolPackages
        for packageEntry in symbolPackages do
            let packageName = packageEntry.Key
            let packageValue = packageEntry.Value
            for entry in packageValue.Assets do
                let metadata = fst (__c entry.Value)
                tryLoadSymbolPackage packageName metadata symbolics

    /// Empty symbolics.
    let makeEmpty () =
        { SymbolPackages = dictPlus StringComparer.Ordinal [] }

/// Provides references to symbols that are loaded from files.
type Symbolics = Symbolics.Symbolics