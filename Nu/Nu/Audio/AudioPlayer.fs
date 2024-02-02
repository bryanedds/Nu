// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open SDL2
open Prime

/// Descrides a song.
type SongDescriptor =
    { FadeInTime: GameTime
      FadeOutTime : GameTime
      StartTime : GameTime
      Volume : single
      Song : Song AssetTag }

/// Describes a sound.
type SoundDescriptor =
    { Volume : single
      Sound : Sound AssetTag }

/// A message to the audio system.
type AudioMessage =
    | LoadAudioPackageMessage of string
    | UnloadAudioPackageMessage of string
    | PlaySoundMessage of SoundDescriptor
    | PlaySongMessage of SongDescriptor
    | FadeOutSongMessage of GameTime
    | StopSongMessage
    | ReloadAudioAssetsMessage

/// An audio asset used by the audio system.
type AudioAsset =
    | WavAsset of nativeint
    | OggAsset of nativeint

/// The audio player. Represents the audio subsystem of Nu generally.
type AudioPlayer =
    /// The master audio volume.
    abstract MasterAudioVolume : single with get, set
    /// The master sound volume.
    abstract MasterSoundVolume : single with get, set
    /// The master song volume.
    abstract MasterSongVolume : single with get, set
    /// Pop all of the audio messages that have been enqueued.
    abstract PopMessages : unit -> AudioMessage List
    /// Clear all of the audio messages that have been enqueued.
    abstract ClearMessages : unit -> unit
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : AudioMessage -> unit
    /// Get the current optionally-playing song.
    abstract CurrentSongOpt : SongDescriptor option
    /// Get the current song's position or 0 if one isn't playing.
    abstract CurrentSongPosition : double
    /// 'Play' the audio system. Must be called once per frame.
    abstract Play : AudioMessage List -> unit

/// The stub implementation of AudioPlayer.
type [<ReferenceEquality>] StubAudioPlayer =
    private
        { StubAudioPlayer : unit }
    
    interface AudioPlayer with
        member audioPlayer.PopMessages () = List ()
        member audioPlayer.ClearMessages () = ()
        member audioPlayer.EnqueueMessage _ = ()
        member audioPlayer.CurrentSongOpt = None
        member audioPlayer.CurrentSongPosition = 0.0
        member audioPlayer.Play _ = ()
        member audioPlayer.MasterAudioVolume with get () = 1.0f and set _ = ()
        member audioPlayer.MasterSoundVolume with get () = 1.0f and set _ = ()
        member audioPlayer.MasterSongVolume with get () = 1.0f and set _ = ()

    static member make () =
        { StubAudioPlayer = () }

/// The SDL implementation of AudioPlayer.
type [<ReferenceEquality>] SdlAudioPlayer =
    private
        { AudioContext : unit // audio context, interestingly, is global. Good luck encapsulating that!
          AudioPackages : Packages<AudioAsset, unit>
          mutable AudioMessages : AudioMessage List
          mutable MasterAudioVolume : single
          mutable MasterSoundVolume : single
          mutable MasterSongVolume : single
          mutable CurrentSongOpt : (SongDescriptor * nativeint) option }

    static member private tryFreeAudioAsset (audioAsset : AudioAsset) (audioPlayer : SdlAudioPlayer) =
        match audioAsset with
        | WavAsset wav ->
            match audioPlayer.CurrentSongOpt with
            | Some (_, wavPlaying) ->
                let freeing = wav <> wavPlaying
                if freeing then SDL_mixer.Mix_FreeChunk wav
                freeing
            | None ->
                SDL_mixer.Mix_FreeChunk wav
                true
        | OggAsset ogg ->
            match audioPlayer.CurrentSongOpt with
            | Some (_, oggPlaying) ->
                let freeing = ogg <> oggPlaying
                if freeing then SDL_mixer.Mix_FreeMusic ogg
                freeing
            | None ->
                SDL_mixer.Mix_FreeMusic ogg
                true

    static member private haltSound () =
        SDL_mixer.Mix_HaltMusic () |> ignore
        let (_, _, _, channelCount) =  SDL_mixer.Mix_QuerySpec ()
        for i in [0 .. channelCount - 1] do
            SDL_mixer.Mix_HaltChannel i |> ignore

    static member private tryLoadAudioAsset (asset : Asset) =
        match PathF.GetExtensionLower asset.FilePath with
        | ".wav" ->
            let wavOpt = SDL_mixer.Mix_LoadWAV asset.FilePath
            if wavOpt <> IntPtr.Zero then Some (WavAsset wavOpt)
            else
                let errorMsg = SDL.SDL_GetError ()
                Log.info ("Could not load wav '" + asset.FilePath + "' due to '" + errorMsg + "'.")
                None
        | ".ogg" ->
            let oggOpt = SDL_mixer.Mix_LoadMUS asset.FilePath
            if oggOpt <> IntPtr.Zero then Some (OggAsset oggOpt)
            else
                let errorMsg = SDL.SDL_GetError ()
                Log.info ("Could not load ogg '" + asset.FilePath + "' due to '" + errorMsg + "'.")
                None
        | _ -> None

    static member private tryLoadAudioPackage packageName audioPlayer =

        // attempt to make new asset graph and load its assets
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            match AssetGraph.tryCollectAssetsFromPackage (Some Constants.Associations.Audio) packageName assetGraph with
            | Right assetsCollected ->

                // find or create audio package
                let audioPackage =
                    match Dictionary.tryFind packageName audioPlayer.AudioPackages with
                    | Some audioPackage -> audioPackage
                    | None ->
                        let audioPackage = { Assets = dictPlus StringComparer.Ordinal []; PackageState = () }
                        audioPlayer.AudioPackages.[packageName] <- audioPackage
                        audioPackage

                // categorize existing assets based on the required action
                let assetsExisting = audioPackage.Assets
                let assetsToFree = Dictionary ()
                let assetsToKeep = Dictionary ()
                for assetEntry in assetsExisting do
                    let assetName = assetEntry.Key
                    let (lastWriteTime, filePath, audioAsset) = assetEntry.Value
                    let lastWriteTime' =
                        try DateTimeOffset (File.GetLastWriteTime filePath)
                        with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                    if lastWriteTime < lastWriteTime'
                    then assetsToFree.Add (filePath, audioAsset)
                    else assetsToKeep.Add (assetName, (lastWriteTime, filePath, audioAsset))

                // attempt to free assets
                for assetEntry in assetsToFree do
                    let filePath = assetEntry.Key
                    let audioAsset = assetEntry.Value
                    if not (SdlAudioPlayer.tryFreeAudioAsset audioAsset audioPlayer) then
                        assetsToKeep.Add (filePath, (DateTimeOffset.MinValue.DateTime, filePath, audioAsset))

                // categorize assets to load
                let assetsToLoad = HashSet ()
                for asset in assetsCollected do
                    if not (assetsToKeep.ContainsKey asset.AssetTag.AssetName) then
                        assetsToLoad.Add asset |> ignore<bool>

                // load assets
                let assetsLoaded = Dictionary ()
                for asset in assetsToLoad do
                    match SdlAudioPlayer.tryLoadAudioAsset asset with
                    | Some audioAsset ->
                        let lastWriteTime =
                            try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                            with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                        assetsLoaded.[asset.AssetTag.AssetName] <- (lastWriteTime, asset.FilePath, audioAsset)
                    | None -> ()

                // insert assets into package
                for assetEntry in Seq.append assetsToKeep assetsLoaded do
                    let assetName = assetEntry.Key
                    let (lastWriteTime, filePath, audioAsset) = assetEntry.Value
                    audioPackage.Assets.[assetName] <- (lastWriteTime, filePath, audioAsset)

            // handle error cases
            | Left failedAssetNames ->
                Log.info ("Audio package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")
        | Left error ->
            Log.info ("Audio package load failed due to unloadable asset graph due to: '" + error)

    static member private tryGetAudioAsset (assetTag : AssetTag) audioPlayer =
        match Dictionary.tryFind assetTag.PackageName audioPlayer.AudioPackages with
        | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
        | None ->
            Log.info ("Loading Audio package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
            SdlAudioPlayer.tryLoadAudioPackage assetTag.PackageName audioPlayer
            match Dictionary.tryFind assetTag.PackageName audioPlayer.AudioPackages with
            | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
            | None -> None

    static member private playSong playSongMessage audioPlayer =
        let song = playSongMessage.Song
        match SdlAudioPlayer.tryGetAudioAsset song audioPlayer with
        | Some audioAsset ->
            match audioAsset with
            | WavAsset _ ->
                Log.info ("Cannot play wav file as song '" + scstring song + "'.")
            | OggAsset oggAsset ->
                SDL_mixer.Mix_HaltMusic () |> ignore // NOTE: have to stop current song in case it is still fading out, causing the next song not to play.
                SDL_mixer.Mix_VolumeMusic (int (playSongMessage.Volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSongVolume * single SDL_mixer.MIX_MAX_VOLUME)) |> ignore
                match SDL_mixer.Mix_FadeInMusicPos (oggAsset, -1, int (max Constants.Audio.FadeInSecondsMin playSongMessage.FadeInTime.Seconds * 1000.0f), double playSongMessage.StartTime.Seconds) with
                | -1 ->
                    // HACK: start time exceeded length of track, so starting over.
                    SDL_mixer.Mix_FadeInMusicPos (oggAsset, -1, int (max Constants.Audio.FadeInSecondsMin playSongMessage.FadeInTime.Seconds * 1000.0f), 0.0) |> ignore
                | _ -> ()
                audioPlayer.CurrentSongOpt <- Some (playSongMessage, oggAsset)
        | None ->
            Log.info ("PlaySongMessage failed due to unloadable assets for '" + scstring song + "'.")

    static member private handleLoadAudioPackage packageName audioPlayer =
        SdlAudioPlayer.tryLoadAudioPackage packageName audioPlayer

    static member private handleUnloadAudioPackage packageName audioPlayer =
        match Dictionary.tryFind packageName  audioPlayer.AudioPackages with
        | Some package ->
            // all sounds / music must be halted because one of them might be playing during unload
            // (which is very bad according to the API docs).
            SdlAudioPlayer.haltSound ()
            for asset in package.Assets do
                match __c asset.Value with
                | WavAsset wavAsset -> SDL_mixer.Mix_FreeChunk wavAsset
                | OggAsset oggAsset -> SDL_mixer.Mix_FreeMusic oggAsset
            audioPlayer.AudioPackages.Remove packageName |> ignore
        | None -> ()

    static member private handlePlaySound playSoundMessage audioPlayer =
        let sound = playSoundMessage.Sound
        match SdlAudioPlayer.tryGetAudioAsset sound audioPlayer with
        | Some audioAsset ->
            match audioAsset with
            | WavAsset wavAsset ->
                SDL_mixer.Mix_VolumeChunk (wavAsset, int (playSoundMessage.Volume * audioPlayer.MasterSoundVolume * single SDL_mixer.MIX_MAX_VOLUME)) |> ignore
                SDL_mixer.Mix_PlayChannel (-1, wavAsset, 0) |> ignore
            | OggAsset _ -> Log.info ("Cannot play ogg file as sound '" + scstring sound + "'.")
        | None ->
            Log.info ("PlaySoundMessage failed due to unloadable assets for '" + scstring sound + "'.")
    
    static member private handlePlaySong playSongMessage audioPlayer =
        SdlAudioPlayer.playSong playSongMessage audioPlayer
    
    static member private handleFadeOutSong (fadeOutTime : GameTime) =
        if SDL_mixer.Mix_PlayingMusic () = 1 then
            if  fadeOutTime <> GameTime.zero &&
                SDL_mixer.Mix_FadingMusic () <> SDL_mixer.Mix_Fading.MIX_FADING_OUT then
                SDL_mixer.Mix_FadeOutMusic (int (fadeOutTime.Seconds * 1000.0f)) |> ignore
            else
                SDL_mixer.Mix_HaltMusic () |> ignore
    
    static member private handleStopSong =
        if SDL_mixer.Mix_PlayingMusic () = 1 then
            SDL_mixer.Mix_HaltMusic () |> ignore

    static member private handleReloadAudioAssets audioPlayer =
        let packageNames = audioPlayer.AudioPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq
        for packageName in packageNames do
            SdlAudioPlayer.tryLoadAudioPackage packageName audioPlayer

    static member private handleAudioMessage audioMessage audioPlayer =
        match audioMessage with
        | LoadAudioPackageMessage packageName -> SdlAudioPlayer.handleLoadAudioPackage packageName audioPlayer
        | UnloadAudioPackageMessage packageName -> SdlAudioPlayer.handleUnloadAudioPackage packageName audioPlayer
        | PlaySoundMessage playSoundMessage -> SdlAudioPlayer.handlePlaySound playSoundMessage audioPlayer
        | PlaySongMessage playSongMessage -> SdlAudioPlayer.handlePlaySong playSongMessage audioPlayer
        | FadeOutSongMessage fadeOutTime -> SdlAudioPlayer.handleFadeOutSong fadeOutTime
        | StopSongMessage -> SdlAudioPlayer.handleStopSong
        | ReloadAudioAssetsMessage -> SdlAudioPlayer.handleReloadAudioAssets audioPlayer
    
    static member private handleAudioMessages audioMessages audioPlayer =
        for audioMessage in audioMessages do
            SdlAudioPlayer.handleAudioMessage audioMessage audioPlayer

    static member private tryUpdateCurrentSongVolume audioPlayer =
        match audioPlayer.CurrentSongOpt with
        | Some (currentSong, _) -> SDL_mixer.Mix_VolumeMusic (int (currentSong.Volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSongVolume * single SDL_mixer.MIX_MAX_VOLUME)) |> ignore
        | None -> ()
    
    static member private tryUpdateCurrentSong audioPlayer =
        if SDL_mixer.Mix_PlayingMusic () = 0 then
            audioPlayer.CurrentSongOpt <- None
    
    /// Make a NuAudioPlayer.
    static member make () =
        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO = 0u then
            failwith "Cannot create an AudioPlayer without SDL audio initialized."
        let audioPlayer =
            { AudioContext = ()
              AudioPackages = dictPlus StringComparer.Ordinal []
              AudioMessages = List ()
              MasterAudioVolume = 1.0f
              MasterSoundVolume = 1.0f
              MasterSongVolume = 1.0f
              CurrentSongOpt = None }
        audioPlayer
    
    interface AudioPlayer with
    
        member audioPlayer.MasterAudioVolume
            with get () = audioPlayer.MasterAudioVolume
            and  set volume =
                audioPlayer.MasterAudioVolume <- volume
                SdlAudioPlayer.tryUpdateCurrentSongVolume audioPlayer

        member audioPlayer.MasterSoundVolume
            with get () = audioPlayer.MasterSoundVolume
            and  set volume = audioPlayer.MasterSoundVolume <- volume

        member audioPlayer.MasterSongVolume
            with get () = audioPlayer.MasterSongVolume
            and  set volume =
                audioPlayer.MasterSongVolume <- volume
                SdlAudioPlayer.tryUpdateCurrentSongVolume audioPlayer

        member audioPlayer.PopMessages () =
            let messages = audioPlayer.AudioMessages
            audioPlayer.AudioMessages <- List ()
            messages

        member audioPlayer.ClearMessages () =
            audioPlayer.AudioMessages <- List ()

        member audioPlayer.EnqueueMessage audioMessage =
            audioPlayer.AudioMessages.Add audioMessage 

        member audioPlayer.CurrentSongOpt =
            Option.map fst audioPlayer.CurrentSongOpt

        member audioPlayer.CurrentSongPosition =
            match audioPlayer.CurrentSongOpt with
            | Some (_, oggAsset) -> ignore oggAsset; failwithnie () // SDL_mixer.Mix_GetMusicPosition oggAsset
            | None -> failwithnie () // 0.0

        member audioPlayer.Play audioMessages =
            SdlAudioPlayer.handleAudioMessages audioMessages audioPlayer
            SdlAudioPlayer.tryUpdateCurrentSong audioPlayer