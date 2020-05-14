// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open SDL2
open Prime
open Nu

/// Audio. Currently just used as a phantom type.
type Audio = private { __ : unit }

/// A message to the audio system to play a song.
type [<StructuralEquality; NoComparison>] PlaySongMessage =
    { FadeOutMs : int
      Volume : single
      Song : Audio AssetTag }

/// A message to the audio system to play a sound.
type [<StructuralEquality; NoComparison>] PlaySoundMessage =
    { Volume : single
      Sound : Audio AssetTag }

/// A message to the audio system.
type [<StructuralEquality; NoComparison>] AudioMessage =
    | HintAudioPackageUseMessage of string
    | HintAudioPackageDisuseMessage of string
    | PlaySoundMessage of PlaySoundMessage
    | PlaySongMessage of PlaySongMessage
    | FadeOutSongMessage of int
    | StopSongMessage
    | ReloadAudioAssetsMessage

/// An audio asset used by the audio system.
type [<ReferenceEquality>] AudioAsset =
    | WavAsset of nativeint
    | OggAsset of nativeint

/// The audio player. Represents the audio subsystem of Nu generally.
type AudioPlayer =
    /// Pop all of the audio messages that have been enqueued.
    abstract PopMessages : unit -> AudioMessage List
    /// Clear all of the audio messages that have been enqueued.
    abstract ClearMessages : unit -> unit
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : AudioMessage -> unit
    /// 'Play' the audio system. Must be called once per frame.
    abstract Play : AudioMessage List -> unit

/// The mock implementation of AudioPlayer.
type [<ReferenceEquality>] MockAudioPlayer =
    private
        { MockAudioPlayer : unit }
    
    interface AudioPlayer with
        member audioPlayer.PopMessages () = List ()
        member audioPlayer.ClearMessages () = ()
        member audioPlayer.EnqueueMessage _ = ()
        member audioPlayer.Play _ = ()

    static member make () =
        { MockAudioPlayer = () }

/// The SDL implementation of AudioPlayer.
type [<ReferenceEquality>] SdlAudioPlayer =
    private
        { AudioContext : unit // audio context, interestingly, is global. Good luck encapsulating that!
          AudioPackages : AudioAsset Packages
          mutable AudioMessages : AudioMessage List
          mutable CurrentSongOpt : PlaySongMessage option }

    static member private haltSound () =
        SDL_mixer.Mix_HaltMusic () |> ignore
        let (_, _, _, channelCount) =  SDL_mixer.Mix_QuerySpec ()
        for i in [0 .. channelCount - 1] do
            SDL_mixer.Mix_HaltChannel i |> ignore
    
    static member private tryLoadAudioAsset2 (asset : Audio Asset) =
        match Path.GetExtension asset.FilePath with
        | ".wav" ->
            let wavOpt = SDL_mixer.Mix_LoadWAV asset.FilePath
            if wavOpt <> IntPtr.Zero then Some (asset.AssetTag.AssetName, WavAsset wavOpt)
            else
                let errorMsg = SDL.SDL_GetError ()
                Log.debug ("Could not load wav '" + asset.FilePath + "' due to '" + errorMsg + "'.")
                None
        | ".ogg" ->
            let oggOpt = SDL_mixer.Mix_LoadMUS asset.FilePath
            if oggOpt <> IntPtr.Zero then Some (asset.AssetTag.AssetName, OggAsset oggOpt)
            else
                let errorMsg = SDL.SDL_GetError ()
                Log.debug ("Could not load ogg '" + asset.FilePath + "' due to '" + errorMsg + "'.")
                None
        | extension -> Log.debug ("Could not load audio asset '" + scstring asset + "' due to unknown extension '" + extension + "'."); None

    static member private tryLoadAudioPackage packageName audioPlayer =
        match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryLoadAssetsFromPackage true (Some Constants.Associations.Audio) packageName assetGraph with
            | Right assets ->
                let assets = List.map Asset.specialize<Audio> assets
                let audioAssetOpts = List.map SdlAudioPlayer.tryLoadAudioAsset2 assets
                let audioAssets = List.definitize audioAssetOpts
                match Dictionary.tryFind packageName audioPlayer.AudioPackages with
                | Some audioAssetDict ->
                    for (key, value) in audioAssets do audioAssetDict.ForceAdd (key, value)
                    audioPlayer.AudioPackages.ForceAdd (packageName, audioAssetDict)
                | None ->
                    let audioAssetDict = dictPlus audioAssets
                    audioPlayer.AudioPackages.ForceAdd (packageName, audioAssetDict)
            | Left error ->
                Log.info ("Audio package load failed due to unloadable assets '" + error + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Audio package load failed due to unloadable asset graph due to: '" + error)

    static member private tryLoadAudioAsset (assetTag : Audio AssetTag) audioPlayer =
        let assetsOpt =
            if audioPlayer.AudioPackages.ContainsKey assetTag.PackageName then
                Dictionary.tryFind assetTag.PackageName audioPlayer.AudioPackages
            else
                Log.info ("Loading audio package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                SdlAudioPlayer.tryLoadAudioPackage assetTag.PackageName audioPlayer
                Dictionary.tryFind assetTag.PackageName audioPlayer.AudioPackages
        Option.bind (fun assets -> Dictionary.tryFind assetTag.AssetName assets) assetsOpt

    static member private playSong playSongMessage audioPlayer =
        let song = playSongMessage.Song
        match SdlAudioPlayer.tryLoadAudioAsset song audioPlayer with
        | Some audioAsset ->
            match audioAsset with
            | WavAsset _ ->
                Log.info ("Cannot play wav file as song '" + scstring song + "'.")
            | OggAsset oggAsset ->
                SDL_mixer.Mix_HaltMusic () |> ignore // NOTE: have to stop current song in case it is still fading out, causing the next song not to play
                SDL_mixer.Mix_VolumeMusic (int (playSongMessage.Volume * single SDL_mixer.MIX_MAX_VOLUME)) |> ignore
                SDL_mixer.Mix_FadeInMusic (oggAsset, -1, 256) |> ignore // Mix_PlayMusic seems to somtimes cause audio 'popping' when starting a song, so a fade is used instead... |> ignore
            audioPlayer.CurrentSongOpt <- Some playSongMessage
        | None ->
            Log.info ("PlaySongMessage failed due to unloadable assets for '" + scstring song + "'.")
    
    static member private handleHintAudioPackageUse hintPackageName audioPlayer =
        SdlAudioPlayer.tryLoadAudioPackage hintPackageName audioPlayer
    
    static member private handleHintAudioPackageDisuse hintPackageName audioPlayer =
        match Dictionary.tryFind hintPackageName  audioPlayer.AudioPackages with
        | Some assets ->
            // all sounds / music must be halted because one of them might be playing during unload
            // (which is very bad according to the API docs).
            SdlAudioPlayer.haltSound ()
            for asset in assets do
                match asset.Value with
                | WavAsset wavAsset -> SDL_mixer.Mix_FreeChunk wavAsset
                | OggAsset oggAsset -> SDL_mixer.Mix_FreeMusic oggAsset
            audioPlayer.AudioPackages.Remove hintPackageName |> ignore
        | None -> ()

    static member private handlePlaySound playSoundMessage audioPlayer =
        let sound = playSoundMessage.Sound
        match SdlAudioPlayer.tryLoadAudioAsset sound audioPlayer with
        | Some audioAsset ->
            match audioAsset with
            | WavAsset wavAsset ->
                SDL_mixer.Mix_VolumeChunk (wavAsset, int (playSoundMessage.Volume * single SDL_mixer.MIX_MAX_VOLUME)) |> ignore
                SDL_mixer.Mix_PlayChannel (-1, wavAsset, 0) |> ignore
            | OggAsset _ -> Log.info ("Cannot play ogg file as sound '" + scstring sound + "'.")
        | None ->
            Log.info ("PlaySoundMessage failed due to unloadable assets for '" + scstring sound + "'.")
    
    static member private handlePlaySong playSongMessage audioPlayer =
        SdlAudioPlayer.playSong playSongMessage audioPlayer
    
    static member private handleFadeOutSong timeToFadeOutSongMs =
        if SDL_mixer.Mix_PlayingMusic () = 1 then
            if  timeToFadeOutSongMs <> 0 &&
                SDL_mixer.Mix_FadingMusic () <> SDL_mixer.Mix_Fading.MIX_FADING_OUT then
                SDL_mixer.Mix_FadeOutMusic timeToFadeOutSongMs |> ignore
            else
                SDL_mixer.Mix_HaltMusic () |> ignore
    
    static member private handleStopSong =
        if SDL_mixer.Mix_PlayingMusic () = 1 then
            SDL_mixer.Mix_HaltMusic () |> ignore

    static member private handleReloadAudioAssets audioPlayer =
        let packageNames = audioPlayer.AudioPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        audioPlayer.AudioPackages.Clear ()
        for packageName in packageNames do
            SdlAudioPlayer.tryLoadAudioPackage packageName audioPlayer

    static member private handleAudioMessage audioMessage audioPlayer =
        match audioMessage with
        | HintAudioPackageUseMessage hintPackageUse -> SdlAudioPlayer.handleHintAudioPackageUse hintPackageUse audioPlayer
        | HintAudioPackageDisuseMessage hintPackageDisuse -> SdlAudioPlayer.handleHintAudioPackageDisuse hintPackageDisuse audioPlayer
        | PlaySoundMessage playSoundMessage -> SdlAudioPlayer.handlePlaySound playSoundMessage audioPlayer
        | PlaySongMessage playSongMessage -> SdlAudioPlayer.handlePlaySong playSongMessage audioPlayer
        | FadeOutSongMessage timeToFadeSongMs -> SdlAudioPlayer.handleFadeOutSong timeToFadeSongMs
        | StopSongMessage -> SdlAudioPlayer.handleStopSong
        | ReloadAudioAssetsMessage -> SdlAudioPlayer.handleReloadAudioAssets audioPlayer
    
    static member private handleAudioMessages audioMessages audioPlayer =
        for audioMessage in audioMessages do
            SdlAudioPlayer.handleAudioMessage audioMessage audioPlayer
    
    static member private tryUpdateCurrentSong audioPlayer =
        if SDL_mixer.Mix_PlayingMusic () = 1 then
            audioPlayer.CurrentSongOpt <- None
        
    
    /// Make a NuAudioPlayer.
    static member make () =
        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO = 0u then
            failwith "Cannot create an AudioPlayer without SDL audio initialized."
        let audioPlayer =
            { AudioContext = ()
              AudioPackages = dictPlus []
              AudioMessages = List ()
              CurrentSongOpt = None }
        audioPlayer
    
    interface AudioPlayer with

        member audioPlayer.PopMessages () =
            let messages = audioPlayer.AudioMessages
            audioPlayer.AudioMessages <- List ()
            messages

        member audioPlayer.ClearMessages () =
            audioPlayer.AudioMessages <- List ()

        member audioPlayer.EnqueueMessage audioMessage =
            audioPlayer.AudioMessages.Add audioMessage 

        member audioPlayer.Play audioMessages =
            SdlAudioPlayer.handleAudioMessages audioMessages audioPlayer
            SdlAudioPlayer.tryUpdateCurrentSong audioPlayer

[<RequireQualifiedAccess>]
module AudioPlayer =

    /// Clear all of the audio messages that have been enqueued.
    let clearMessages (audioPlayer : AudioPlayer) =
        audioPlayer.ClearMessages ()

    /// Enqueue a message from an external source.
    let enqueueMessage message (audioPlayer : AudioPlayer) =
        audioPlayer.EnqueueMessage message

    /// 'Play' the audio system. Must be called once per frame.
    let play audioMessages (audioPlayer : AudioPlayer) =
        audioPlayer.Play audioMessages