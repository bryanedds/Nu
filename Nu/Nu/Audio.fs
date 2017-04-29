// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.IO
open SDL2
open Prime
open Nu

/// A message to the audio system to play a song.
type [<StructuralEquality; NoComparison>] PlaySongMessage =
    { TimeToFadeOutSongMs : int
      Volume : single
      Song : AssetTag }

/// A message to the audio system to play a sound.
type [<StructuralEquality; NoComparison>] PlaySoundMessage =
    { Volume : single
      Sound : AssetTag }

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
type IAudioPlayer =
    /// Clear all of the audio messages that have been enqueued.
    abstract ClearMessages : unit -> IAudioPlayer
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : AudioMessage -> IAudioPlayer
    /// 'Play' the audio system. Must be called once per frame.
    abstract Play : unit -> IAudioPlayer

[<AutoOpen>]
module AudioPlayerModule =

    /// The primary implementation of IAudioPlayer.
    type [<ReferenceEquality>] AudioPlayer =
        private
            { AudioContext : unit // audio context, interestingly, is global. Good luck encapsulating that!
              AudioPackageMap : AudioAsset PackageMap
              AudioMessages : AudioMessage UList
              CurrentSongOpt : PlaySongMessage option
              NextPlaySongOpt : PlaySongMessage option }
    
        static member private haltSound () =
            SDL_mixer.Mix_HaltMusic () |> ignore
            let (_, _, _, channelCount) =  SDL_mixer.Mix_QuerySpec ()
            for i in [0 .. channelCount - 1] do
                SDL_mixer.Mix_HaltChannel i |> ignore
    
        static member private tryLoadAudioAsset2 (asset : Asset) =
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
                    let audioAssetOpts = List.map AudioPlayer.tryLoadAudioAsset2 assets
                    let audioAssets = List.definitize audioAssetOpts
                    let audioAssetMapOpt = UMap.tryFindFast packageName audioPlayer.AudioPackageMap
                    if FOption.isSome audioAssetMapOpt then
                        let audioAssetMap = FOption.get audioAssetMapOpt
                        let audioAssetMap = UMap.addMany audioAssets audioAssetMap
                        { audioPlayer with AudioPackageMap = UMap.add packageName audioAssetMap audioPlayer.AudioPackageMap }
                    else
                        let audioAssetMap = UMap.makeFromSeq None audioAssets
                        { audioPlayer with AudioPackageMap = UMap.add packageName audioAssetMap audioPlayer.AudioPackageMap }
                | Left error ->
                    Log.info ("Audio package load failed due to unloadable assets '" + error + "' for package '" + packageName + "'.")
                    audioPlayer
            | Left error ->
                Log.info ("Audio package load failed due to unloadable asset graph due to: '" + error)
                audioPlayer
    
        static member private tryLoadAudioAsset (assetTag : AssetTag) audioPlayer =
            let (assetMapOpt, audioPlayer) =
                if UMap.containsKey assetTag.PackageName audioPlayer.AudioPackageMap
                then (UMap.tryFindFast assetTag.PackageName audioPlayer.AudioPackageMap, audioPlayer)
                else
                    Log.info ("Loading audio package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                    let audioPlayer = AudioPlayer.tryLoadAudioPackage assetTag.PackageName audioPlayer
                    (UMap.tryFindFast assetTag.PackageName audioPlayer.AudioPackageMap, audioPlayer)
            (FOption.bind (fun assetMap -> UMap.tryFindFast assetTag.AssetName assetMap) assetMapOpt, audioPlayer)
    
        static member private playSong playSongMessage audioPlayer =
            let song = playSongMessage.Song
            let (audioAssetOpt, audioPlayer) = AudioPlayer.tryLoadAudioAsset song audioPlayer
            if FOption.isSome audioAssetOpt then
                match FOption.get audioAssetOpt with
                | WavAsset _ ->
                    Log.info ("Cannot play wav file as song '" + scstring song + "'.")
                | OggAsset oggAsset ->
                    SDL_mixer.Mix_VolumeMusic (int ^ playSongMessage.Volume * single SDL_mixer.MIX_MAX_VOLUME) |> ignore
                    SDL_mixer.Mix_FadeInMusic (oggAsset, -1, 256) |> ignore // Mix_PlayMusic seems to somtimes cause audio 'popping' when starting a song, so a fade is used instead... |> ignore
                { audioPlayer with CurrentSongOpt = Some playSongMessage }
            else
                Log.info ("PlaySongMessage failed due to unloadable assets for '" + scstring song + "'.")
                audioPlayer
    
        static member private handleHintAudioPackageUse hintPackageName audioPlayer =
            AudioPlayer.tryLoadAudioPackage hintPackageName audioPlayer
    
        static member private handleHintAudioPackageDisuse hintPackageName audioPlayer =
            let assetsOpt = UMap.tryFindFast hintPackageName audioPlayer.AudioPackageMap
            if FOption.isSome assetsOpt then
                let assets = FOption.get assetsOpt
                // all sounds / music must be halted because one of them might be playing during unload
                // (which is very bad according to the API docs).
                AudioPlayer.haltSound ()
                for (_, asset) in assets do
                    match asset with
                    | WavAsset wavAsset -> SDL_mixer.Mix_FreeChunk wavAsset
                    | OggAsset oggAsset -> SDL_mixer.Mix_FreeMusic oggAsset
                { audioPlayer with AudioPackageMap = UMap.remove hintPackageName audioPlayer.AudioPackageMap }
            else audioPlayer
    
        static member private handlePlaySound playSoundMessage audioPlayer =
            let sound = playSoundMessage.Sound
            let (audioAssetOpt, audioPlayer) = AudioPlayer.tryLoadAudioAsset sound audioPlayer
            if FOption.isSome audioAssetOpt then
                match FOption.get audioAssetOpt with
                | WavAsset wavAsset ->
                    SDL_mixer.Mix_VolumeChunk (wavAsset, int ^ playSoundMessage.Volume * single SDL_mixer.MIX_MAX_VOLUME) |> ignore
                    SDL_mixer.Mix_PlayChannel (-1, wavAsset, 0) |> ignore
                | OggAsset _ -> Log.info ("Cannot play ogg file as sound '" + scstring sound + "'.")
                audioPlayer
            else
                Log.info ("PlaySoundMessage failed due to unloadable assets for '" + scstring sound + "'.")
                audioPlayer
    
        static member private handlePlaySong playSongMessage audioPlayer =
            if SDL_mixer.Mix_PlayingMusic () = 1 then
                if audioPlayer.CurrentSongOpt <> Some playSongMessage then
                    if  playSongMessage.TimeToFadeOutSongMs <> 0 &&
                        not (SDL_mixer.Mix_FadingMusic () = SDL_mixer.Mix_Fading.MIX_FADING_OUT) then
                        SDL_mixer.Mix_FadeOutMusic playSongMessage.TimeToFadeOutSongMs |> ignore
                    else
                        SDL_mixer.Mix_HaltMusic () |> ignore
                    { audioPlayer with NextPlaySongOpt = Some playSongMessage }
                else audioPlayer
            else AudioPlayer.playSong playSongMessage audioPlayer
    
        static member private handleFadeOutSong timeToFadeOutSongMs audioPlayer =
            if SDL_mixer.Mix_PlayingMusic () = 1 then
                if  timeToFadeOutSongMs <> 0 &&
                    SDL_mixer.Mix_FadingMusic () <> SDL_mixer.Mix_Fading.MIX_FADING_OUT then
                    SDL_mixer.Mix_FadeOutMusic timeToFadeOutSongMs |> ignore
                else
                    SDL_mixer.Mix_HaltMusic () |> ignore
            audioPlayer
    
        static member private handleStopSong audioPlayer =
            if SDL_mixer.Mix_PlayingMusic () = 1 then SDL_mixer.Mix_HaltMusic () |> ignore
            audioPlayer
    
        static member private handleReloadAudioAssets audioPlayer =
            let oldPackageMap = audioPlayer.AudioPackageMap
            let oldPackageNames = oldPackageMap |> UMap.toSeq |> Seq.map fst
            let audioPlayer = { audioPlayer with AudioPackageMap = UMap.makeEmpty None }
            Seq.fold
                (fun audioPlayer packageName -> AudioPlayer.tryLoadAudioPackage packageName audioPlayer)
                audioPlayer
                oldPackageNames
    
        static member private handleAudioMessage audioPlayer audioMessage =
            match audioMessage with
            | HintAudioPackageUseMessage hintPackageUse -> AudioPlayer.handleHintAudioPackageUse hintPackageUse audioPlayer
            | HintAudioPackageDisuseMessage hintPackageDisuse -> AudioPlayer.handleHintAudioPackageDisuse hintPackageDisuse audioPlayer
            | PlaySoundMessage playSoundMessage -> AudioPlayer.handlePlaySound playSoundMessage audioPlayer
            | PlaySongMessage playSongMessage -> AudioPlayer.handlePlaySong playSongMessage audioPlayer
            | FadeOutSongMessage timeToFadeSongMs -> AudioPlayer.handleFadeOutSong timeToFadeSongMs audioPlayer
            | StopSongMessage -> AudioPlayer.handleStopSong audioPlayer
            | ReloadAudioAssetsMessage -> AudioPlayer.handleReloadAudioAssets audioPlayer
    
        static member private handleAudioMessages audioMessages audioPlayer =
            UList.fold AudioPlayer.handleAudioMessage audioPlayer audioMessages
    
        static member private tryUpdateCurrentSong audioPlayer =
            if SDL_mixer.Mix_PlayingMusic () = 1 then audioPlayer
            else { audioPlayer with CurrentSongOpt = None }
    
        static member private tryUpdateNextSong audioPlayer =
            match audioPlayer.NextPlaySongOpt with
            | Some nextPlaySong ->
                if SDL_mixer.Mix_PlayingMusic () = 0 then
                    let audioPlayer = AudioPlayer.handlePlaySong nextPlaySong audioPlayer
                    { audioPlayer with NextPlaySongOpt = None }
                else audioPlayer
            | None -> audioPlayer
    
        static member private updateAudioPlayer audioPlayer =
            audioPlayer |>
                AudioPlayer.tryUpdateCurrentSong |>
                AudioPlayer.tryUpdateNextSong
    
        /// Make an AudioPlayer.
        static member make () =
            if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO = 0u then
                failwith "Cannot create an AudioPlayer without SDL audio initialized."
            let audioPlayer =
                { AudioContext = ()
                  AudioPackageMap = UMap.makeEmpty None
                  AudioMessages = UList.makeEmpty None
                  CurrentSongOpt = None
                  NextPlaySongOpt = None }
            audioPlayer
    
        interface IAudioPlayer with
    
            member audioPlayer.ClearMessages () =
                let audioPlayer = { audioPlayer with AudioMessages = UList.makeEmpty None }
                audioPlayer :> IAudioPlayer
    
            member audioPlayer.EnqueueMessage audioMessage =
                let audioMessages = UList.add audioMessage audioPlayer.AudioMessages
                let audioPlayer = { audioPlayer with AudioMessages = audioMessages }
                audioPlayer :> IAudioPlayer
    
            member audioPlayer.Play () =
                let audioMessages = audioPlayer.AudioMessages
                let audioPlayer = { audioPlayer with AudioMessages = UList.makeEmpty None }
                let audioPlayer = AudioPlayer.handleAudioMessages audioMessages audioPlayer
                let audioPlayer = AudioPlayer.updateAudioPlayer audioPlayer
                audioPlayer :> IAudioPlayer

/// The primary implementation of IAudioPlayer.
type AudioPlayer = AudioPlayerModule.AudioPlayer
    
/// The mock implementation of IAudioPlayer.
type [<ReferenceEquality>] MockAudioPlayer =
    private
        { MockAudioPlayer : unit }
    
    interface IAudioPlayer with
        member audioPlayer.ClearMessages () = audioPlayer :> IAudioPlayer
        member audioPlayer.EnqueueMessage _ = audioPlayer :> IAudioPlayer
        member audioPlayer.Play () = audioPlayer :> IAudioPlayer
    
    static member make () =
        { MockAudioPlayer = () }