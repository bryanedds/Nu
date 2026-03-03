// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open FSharp.NativeInterop
open SDL
open Prime

/// Describes a sound.
type SoundDescriptor =
    { Distance : single
      Panning : single
      Volume : single
      Sound : Sound AssetTag }

    /// The default sound descriptor.
    static member val defaultDescriptor =
        { Distance = 0.0f
          Panning = 0.0f
          Volume = Constants.Audio.SoundVolumeDefault
          Sound = asset Assets.Default.PackageName Assets.Default.SoundName }

/// Describes a song.
type SongDescriptor =
    { FadeInTime : GameTime
      FadeOutTime : GameTime
      StartTime : GameTime
      RepeatLimitOpt : uint option
      Volume : single
      Song : Song AssetTag }

    /// The default song descriptor.
    static member val defaultDescriptor =
        { FadeInTime = GameTime.zero
          FadeOutTime = Constants.Audio.FadeOutTimeDefault
          StartTime = GameTime.zero
          RepeatLimitOpt = None
          Volume = Constants.Audio.SongVolumeDefault
          Song = asset Assets.Default.PackageName Assets.Default.SongName }

/// A message to the audio system.
type AudioMessage =
    | LoadAudioPackageMessage of string
    | UnloadAudioPackageMessage of string
    | PlaySoundMessage of SoundDescriptor
    | PlaySongMessage of SongDescriptor
    | SetSongVolumeMessage of single
    | FadeOutSongMessage of GameTime
    | StopSongMessage
    | ReloadAudioAssetsMessage

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
    abstract EnqueueMessage : message : AudioMessage -> unit
    
    /// Get the current optionally-playing song.
    abstract SongOpt : SongDescriptor option // TODO: We can support multiple tracks for multiple songs.
    
    /// Get the current song's position or 0.0 if one isn't playing.
    abstract SongPosition : GameTime
    
    /// Get the current song's volume or 0.0f if one isn't playing.
    abstract SongVolume : single
    
    /// Whether a song is currently playing and fading in.
    abstract SongFadingIn : bool
    
    /// Whether a song is currently playing and fading out.
    abstract SongFadingOut : bool
    
    /// 'Play' the audio system. Must be called once per frame.
    abstract Play : messages : AudioMessage List -> unit
    
    /// Handle audio clean up by stopping all playback and freeing all loaded audio assets.
    abstract CleanUp : unit -> unit

/// The stub implementation of AudioPlayer.
type [<ReferenceEquality>] StubAudioPlayer =
    private
        { StubAudioPlayer : unit }
    
    interface AudioPlayer with
        member audioPlayer.MasterAudioVolume with get () = 1.0f and set _ = ()
        member audioPlayer.MasterSoundVolume with get () = 1.0f and set _ = ()
        member audioPlayer.MasterSongVolume with get () = 1.0f and set _ = ()
        member audioPlayer.PopMessages () = List ()
        member audioPlayer.ClearMessages () = ()
        member audioPlayer.EnqueueMessage _ = ()
        member audioPlayer.SongOpt = None
        member audioPlayer.SongPosition = GameTime.zero
        member audioPlayer.SongVolume = 0.0f
        member audioPlayer.SongFadingIn = false
        member audioPlayer.SongFadingOut = false
        member audioPlayer.Play _ = ()
        member audioPlayer.CleanUp () = ()

    static member make () =
        { StubAudioPlayer = () }

/// The SDL implementation of AudioPlayer.
type [<ReferenceEquality>] SdlAudioPlayer =
    private
        { AudioMixer : MIX_Mixer nativeptr
          SoundTrack : MIX_Track nativeptr // One track for all sounds for now. We can add more tracks later.
          SongTrack : MIX_Track nativeptr // One track for all songs for now. We can add more tracks later.
          SongTrackPropertiesId : SDL_PropertiesID // reused instance across PlayTrack calls.
          AudioPackages : Packages<MIX_Audio nativeptr, unit>
          mutable AudioMessages : AudioMessage List
          mutable MasterAudioVolume : single
          mutable MasterSoundVolume : single
          mutable MasterSongVolume : single
          mutable SongOpt : (SongDescriptor * MIX_Audio nativeptr) option }
    
    static member private haltAudio audioPlayer =
        if not (SDL3_mixer.MIX_StopAllTracks (audioPlayer.AudioMixer, 0L)) then
            Log.error ("Could not halt audio due to '" + SDL3.SDL_GetError () + "'.")

    static member private tryLoadAudioAsset (asset : Asset) audioPlayer =
        // Set predecode to true: https://github.com/libsdl-org/SDL_mixer/issues/662#issuecomment-2626072254
        // "There's also the need to decode the data in advance because some formats are expensive to decode
        // and can't be done just in time to feed the audio device. I'm operating under the assumption that for
        // the most part games want the minimum possible latency so will be feeding the output small chunks at a high rate."
        match PathF.GetExtensionLower asset.FilePath with
        | SoundExtension _ | SongExtension _ ->
            use filePath = new StringWrap (asset.FilePath)
            let musOpt = SDL3_mixer.MIX_LoadAudio (audioPlayer.AudioMixer, filePath.Pointer, true)
            if NativePtr.isNullPtr musOpt then 
                let errorMsg = SDL3.SDL_GetError ()
                Log.info ("Could not load sound or song asset '" + asset.FilePath + "' due to '" + errorMsg + "'.")
                None
            else Some musOpt
        | _ -> None

    static member private tryLoadAudioPackage packageName audioPlayer =

        // make new asset graph and load its assets
        let assetGraph = AssetGraph.makeFromFileOpt Assets.Global.AssetGraphFilePath
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
                let (lastWriteTime, asset, audioAsset) = assetEntry.Value
                let lastWriteTime' =
                    try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                    with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                if lastWriteTime < lastWriteTime'
                then assetsToFree.Add (asset, audioAsset)
                else assetsToKeep.Add (assetName, (lastWriteTime, asset, audioAsset))

            // free assets
            for asset in assetsToFree do
                SDL3_mixer.MIX_DestroyAudio asset.Value // Audio assets are reference counted. If a track is still using it, the actual deallocation will happen when the track stops using it.

            // categorize assets to load
            let assetsToLoad = HashSet ()
            for asset in assetsCollected do
                if not (assetsToKeep.ContainsKey asset.AssetTag.AssetName) then
                    assetsToLoad.Add asset |> ignore<bool>

            // load assets
            let assetsLoaded = Dictionary ()
            for asset in assetsToLoad do
                match SdlAudioPlayer.tryLoadAudioAsset asset audioPlayer with
                | Some audioAsset ->
                    let lastWriteTime =
                        try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                        with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                    assetsLoaded.[asset.AssetTag.AssetName] <- (lastWriteTime, asset, audioAsset)
                | None -> ()

            // insert assets into package
            for assetEntry in assetsLoaded do
                let assetName = assetEntry.Key
                let (lastWriteTime, asset, audioAsset) = assetEntry.Value
                audioPackage.Assets.[assetName] <- (lastWriteTime, asset, audioAsset)

        // handle error case
        | Left failedAssetNames ->
            Log.info ("Audio package load failed due to unloadable assets '" + failedAssetNames + "' for package '" + packageName + "'.")

    static member private tryGetAudioAsset (assetTag : AssetTag) audioPlayer =
        match Dictionary.tryFind assetTag.PackageName audioPlayer.AudioPackages with
        | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
        | None ->
            Log.info ("Loading Audio package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
            SdlAudioPlayer.tryLoadAudioPackage assetTag.PackageName audioPlayer
            match Dictionary.tryFind assetTag.PackageName audioPlayer.AudioPackages with
            | Some package -> package.Assets |> Dictionary.tryFind assetTag.AssetName |> Option.map __c
            | None -> None

    static member private playSong songDescriptor audioPlayer =
        if songDescriptor.Volume > 0.0f then
            let song = songDescriptor.Song
            match SdlAudioPlayer.tryGetAudioAsset song audioPlayer with
            | Some audioAsset ->
                let loops = match songDescriptor.RepeatLimitOpt with Some repeatLimit -> max 0L (int64 repeatLimit) | None -> -1L
                SDL3_mixer.MIX_SetTrackAudio (audioPlayer.SongTrack, audioAsset) |> ignore<SDLBool>
                SDL3_mixer.MIX_SetTrackGain (audioPlayer.SongTrack, songDescriptor.Volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSongVolume) |> ignore
                SDL3.SDL_SetNumberProperty (audioPlayer.SongTrackPropertiesId, SDL3_mixer.MIX_PROP_PLAY_LOOPS_NUMBER, loops) |> ignore<SDLBool>
                SDL3.SDL_SetNumberProperty (audioPlayer.SongTrackPropertiesId, SDL3_mixer.MIX_PROP_PLAY_FADE_IN_MILLISECONDS_NUMBER, int64 (max Constants.Audio.FadeInSecondsMin songDescriptor.FadeInTime.Seconds * 1000.0)) |> ignore<SDLBool>
                SDL3.SDL_SetNumberProperty (audioPlayer.SongTrackPropertiesId, SDL3_mixer.MIX_PROP_PLAY_START_MILLISECOND_NUMBER, int64 (songDescriptor.StartTime.Seconds * 1000.0)) |> ignore<SDLBool>
                if not (SDL3_mixer.MIX_PlayTrack (audioPlayer.SongTrack, audioPlayer.SongTrackPropertiesId)) then
                    Log.info ("Could not play song asset '" + scstring song + "' due to '" + SDL3.SDL_GetError () + "'.")
                audioPlayer.SongOpt <- Some (songDescriptor, audioAsset)
            | None ->
                Log.info ("PlaySongMessage failed due to unloadable assets for '" + scstring song + "'.")

    static member private handleLoadAudioPackage packageName audioPlayer =
        SdlAudioPlayer.tryLoadAudioPackage packageName audioPlayer

    static member private handleUnloadAudioPackage packageName audioPlayer =
        match Dictionary.tryFind packageName audioPlayer.AudioPackages with
        | Some package ->
            for asset in package.Assets do
                SDL3_mixer.MIX_DestroyAudio (__c asset.Value)
            audioPlayer.AudioPackages.Remove packageName |> ignore
        | None -> ()

    static member private handlePlaySound (soundDescriptor : SoundDescriptor) audioPlayer =
        if soundDescriptor.Volume > 0.0f then
            match SdlAudioPlayer.tryGetAudioAsset soundDescriptor.Sound audioPlayer with
            | Some audioAsset ->
                SDL3_mixer.MIX_SetTrackAudio (audioPlayer.SoundTrack, audioAsset) |> ignore<SDLBool>
                SDL3_mixer.MIX_SetTrackGain (audioPlayer.SoundTrack, soundDescriptor.Volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSoundVolume) |> ignore<SDLBool>
                let pan = soundDescriptor.Panning |> max -1.0f |> min 1.0f // TODO: support 3D sound via SetTrack3DPosition. It is not optimal to force stereo for surround sound setups.
                let mutable stereoGains = MIX_StereoGains (left = 1.0f - max 0.0f pan, right = 1.0f + min 0.0f pan)
                SDL3_mixer.MIX_SetTrackStereo (audioPlayer.SoundTrack, &&stereoGains) |> ignore<SDLBool>
                // TODO: Distance is not supported with stereo gains! We need to use 3D sound.
                SDL3_mixer.MIX_PlayTrack (audioPlayer.SoundTrack, Unchecked.defaultof<SDL_PropertiesID>) |> ignore<SDLBool>
            | None ->
                Log.info ("PlaySoundMessage failed due to unloadable assets for '" + scstring soundDescriptor.Sound + "'.")

    static member private handlePlaySong songDescriptor audioPlayer =
        SdlAudioPlayer.playSong songDescriptor audioPlayer

    static member private handleFadeOutSong (fadeOutTime : GameTime) audioPlayer =
        if not (SDL3_mixer.MIX_StopTrack (audioPlayer.SongTrack, SDL3_mixer.MIX_TrackFramesToMS (audioPlayer.SongTrack, int64 (fadeOutTime.Seconds * 1000.0)))) then
            Log.info ("Could not fade out song due to '" + SDL3.SDL_GetError () + "'.")

    static member private handleStopSong audioPlayer =
        if not (SDL3_mixer.MIX_StopTrack (audioPlayer.SongTrack, 0)) then
            Log.info ("Could not stop song due to '" + SDL3.SDL_GetError () + "'.")

    static member private handleReloadAudioAssets audioPlayer =
        for packageName in audioPlayer.AudioPackages |> Seq.map (fun entry -> entry.Key) |> Array.ofSeq do
            SdlAudioPlayer.tryLoadAudioPackage packageName audioPlayer

    static member private handleAudioMessage audioMessage audioPlayer =
        match audioMessage with
        | LoadAudioPackageMessage packageName -> SdlAudioPlayer.handleLoadAudioPackage packageName audioPlayer
        | UnloadAudioPackageMessage packageName -> SdlAudioPlayer.handleUnloadAudioPackage packageName audioPlayer
        | PlaySoundMessage soundDescriptor -> SdlAudioPlayer.handlePlaySound soundDescriptor audioPlayer
        | PlaySongMessage songDescriptor -> SdlAudioPlayer.handlePlaySong songDescriptor audioPlayer
        | SetSongVolumeMessage volume -> SdlAudioPlayer.setSongVolume volume audioPlayer
        | FadeOutSongMessage fadeOutTime -> SdlAudioPlayer.handleFadeOutSong fadeOutTime audioPlayer
        | StopSongMessage -> SdlAudioPlayer.handleStopSong audioPlayer
        | ReloadAudioAssetsMessage -> SdlAudioPlayer.handleReloadAudioAssets audioPlayer

    static member private handleAudioMessages audioMessages audioPlayer =
        for audioMessage in audioMessages do
            SdlAudioPlayer.handleAudioMessage audioMessage audioPlayer

    static member private updateSongVolume audioPlayer =
        match audioPlayer.SongOpt with
        | Some (currentSong, _) -> SDL3_mixer.MIX_SetTrackGain (audioPlayer.SongTrack, currentSong.Volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSongVolume) |> ignore<SDLBool>
        | None -> ()

    static member private setSongVolume volume audioPlayer =
        match audioPlayer.SongOpt with
        | Some (currentSong, musPlaying) ->
            if currentSong.Volume <> volume then
                SDL3_mixer.MIX_SetTrackGain (audioPlayer.SongTrack, volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSongVolume) |> ignore<SDLBool>
                audioPlayer.SongOpt <- Some ({ currentSong with Volume = volume }, musPlaying)
        | None -> ()

    static member private updateSong audioPlayer =
        if not (SDL3_mixer.MIX_TrackPlaying audioPlayer.SongTrack) then
            audioPlayer.SongOpt <- None

    static member private getSongFadingIn audioPlayer =
        SDL3_mixer.MIX_GetTrackFadeFrames audioPlayer.SongTrack > 0L

    static member private getSongFadingOut audioPlayer =
        SDL3_mixer.MIX_GetTrackFadeFrames audioPlayer.SongTrack < 0L

    /// Make an SdlAudioPlayer.
    static member make () =
        if SDL3.SDL_WasInit SDL_InitFlags.SDL_INIT_AUDIO = LanguagePrimitives.EnumOfValue 0u then
            failwith "Cannot create an AudioPlayer without SDL audio initialized."
        let mixer = SDL3_mixer.MIX_CreateMixerDevice (SDL3.SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, NativePtr.nullPtr)
        if NativePtr.isNullPtr mixer then
            Log.info ("Mixer could not initialize audio due to '" + SDL3.SDL_GetError () + "'.")
        let soundTrack = SDL3_mixer.MIX_CreateTrack mixer
        if NativePtr.isNullPtr soundTrack then
            Log.info ("Sound track could not be created due to '" + SDL3.SDL_GetError () + "'.")
        let songTrack = SDL3_mixer.MIX_CreateTrack mixer
        if NativePtr.isNullPtr songTrack then
            Log.info ("Song track could not be created due to '" + SDL3.SDL_GetError () + "'.")
        let props = SDL3.SDL_CreateProperties ()
        if props = Unchecked.defaultof<_> then
            Log.info ("SDL properties could not be created due to '" + SDL3.SDL_GetError () + "'.")
        let audioPlayer =
            { AudioMixer = mixer
              SoundTrack = soundTrack
              SongTrack = songTrack
              SongTrackPropertiesId = props
              AudioPackages = dictPlus StringComparer.Ordinal []
              AudioMessages = List ()
              MasterAudioVolume = Constants.Audio.MasterAudioVolumeDefault
              MasterSoundVolume = Constants.Audio.MasterSoundVolumeDefault
              MasterSongVolume = Constants.Audio.MasterSongVolumeDefault
              SongOpt = None }
        audioPlayer
    
    interface AudioPlayer with
    
        member audioPlayer.MasterAudioVolume
            with get () = audioPlayer.MasterAudioVolume
            and set volume =
                audioPlayer.MasterAudioVolume <- volume
                SdlAudioPlayer.updateSongVolume audioPlayer

        member audioPlayer.MasterSoundVolume
            with get () = audioPlayer.MasterSoundVolume
            and set volume = audioPlayer.MasterSoundVolume <- volume

        member audioPlayer.MasterSongVolume
            with get () = audioPlayer.MasterSongVolume
            and set volume =
                audioPlayer.MasterSongVolume <- volume
                SdlAudioPlayer.updateSongVolume audioPlayer

        member audioPlayer.PopMessages () =
            let messages = audioPlayer.AudioMessages
            audioPlayer.AudioMessages <- List ()
            messages

        member audioPlayer.ClearMessages () =
            audioPlayer.AudioMessages <- List ()

        member audioPlayer.EnqueueMessage audioMessage =
            audioPlayer.AudioMessages.Add audioMessage

        member audioPlayer.SongOpt =
            Option.map fst audioPlayer.SongOpt

        member audioPlayer.SongPosition =
            match audioPlayer.SongOpt with
            | Some (_, musAsset) -> SDL3_mixer.MIX_AudioFramesToMS (musAsset, SDL3_mixer.MIX_GetTrackPlaybackPosition audioPlayer.SongTrack) |> double |> (*) 0.001 |> GameTime.ofSeconds
            | None -> GameTime.zero

        member audioPlayer.SongVolume =
            match audioPlayer.SongOpt with
            | Some (song, _) -> song.Volume
            | None -> 0.0f

        member audioPlayer.SongFadingIn =
            SdlAudioPlayer.getSongFadingIn audioPlayer

        member audioPlayer.SongFadingOut =
            SdlAudioPlayer.getSongFadingOut audioPlayer

        member audioPlayer.Play audioMessages =
            SdlAudioPlayer.handleAudioMessages audioMessages audioPlayer
            SdlAudioPlayer.updateSong audioPlayer

        member audioPlayer.CleanUp () =
            SDL3_mixer.MIX_DestroyMixer audioPlayer.AudioMixer // also destroys tracks
            let audioPackages = audioPlayer.AudioPackages |> Seq.map (fun entry -> entry.Value)
            let audioAssets = audioPackages |> Seq.map (fun package -> package.Assets.Values) |> Seq.concat
            for (_, _, audioAsset) in audioAssets do SDL3_mixer.MIX_DestroyAudio audioAsset
            audioPlayer.AudioPackages.Clear ()
            SDL3.SDL_DestroyProperties audioPlayer.SongTrackPropertiesId