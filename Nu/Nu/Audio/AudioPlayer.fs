// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Runtime.InteropServices
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

    /// Make a StubAudioPlayer.
    static member make () =
        { StubAudioPlayer = () }
    
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

/// Callback for when audio has been stopped in an SDL context.
type private SdlAudioStoppedCallback =
    delegate of userdata : voidptr * track : MIX_Track nativeptr -> unit

/// The SDL implementation of AudioPlayer.
type [<ReferenceEquality>] SdlAudioPlayer =
    private
        { MixerOpt : MIX_Mixer nativeptr option
          mutable FreeTracks : MIX_Track nativeptr ConcurrentStack // needs to be concurrent because tracks are returned from audio callbacks on SDL threads.
          mutable ReturnTrack : SdlAudioStoppedCallback // we need to keep a reference to the audio callback delegate to prevent it from being garbage collected.
          AudioPackages : Packages<MIX_Audio nativeptr, unit>
          mutable AudioMessages : AudioMessage List
          mutable MasterAudioVolume : single
          mutable MasterSoundVolume : single
          mutable MasterSongVolume : single
          SongTrackPropertiesId : SDL_PropertiesID // reused instance across PlayTrack calls.
          mutable SongOpt : (SongDescriptor * MIX_Track nativeptr) option }

    static member private tryLoadAudioAsset (asset : Asset) audioPlayer =
        match audioPlayer.MixerOpt with
        | Some mixer ->
            let predecode =
                match PathF.GetExtensionLower asset.FilePath with
                | SongExtension _ -> true
                | _ -> false
            let filePathSdl = PathF.GetFullPath asset.FilePath
            let audioOpt = SDL3_mixer.MIX_LoadAudio (mixer, filePathSdl, predecode)
            if NativePtr.isNullPtr audioOpt then 
                let errorMsg = SDL3.SDL_GetError ()
                Log.info ("Could not load sound or song asset '" + filePathSdl + "' due to '" + errorMsg + "'.")
                None
            else Some audioOpt
        | None -> None

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
                    audioPlayer.AudioPackages[packageName] <- audioPackage
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
                    with exn ->
                        Log.info ("Asset file write time read error due to: " + scstring exn)
                        DateTimeOffset.MinValue.DateTime
                if lastWriteTime < lastWriteTime'
                then assetsToFree.Add (asset, audioAsset)
                else assetsToKeep.Add (assetName, (lastWriteTime, asset, audioAsset))

            // free assets
            // NOTE: audio assets are reference counted. If a track is still using it, the actual deallocation will
            // happen when the track stops using it.
            for asset in assetsToFree do
                SDL3_mixer.MIX_DestroyAudio asset.Value

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
                        with exn ->
                            Log.info ("Asset file write time read error due to: " + scstring exn)
                            DateTimeOffset.MinValue.DateTime
                    assetsLoaded[asset.AssetTag.AssetName] <- (lastWriteTime, asset, audioAsset)
                | None -> ()

            // insert assets into package
            for assetEntry in assetsLoaded do
                let assetName = assetEntry.Key
                let (lastWriteTime, asset, audioAsset) = assetEntry.Value
                audioPackage.Assets[assetName] <- (lastWriteTime, asset, audioAsset)

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
                match audioPlayer.SongOpt with
                | Some (_, songTrack) ->
                    if not (SDL3_mixer.MIX_StopTrack (songTrack, 0)) then
                        Log.info ("Could not stop previous song due to '" + SDL3.SDL_GetError () + "'.")
                | None -> ()
                match audioPlayer.FreeTracks.TryPop () with
                | (true, songTrack) ->
                    let loops = match songDescriptor.RepeatLimitOpt with Some repeatLimit -> max 0L (int64 repeatLimit) | None -> -1L
                    let durationFrames = SDL3_mixer.MIX_GetAudioDuration audioAsset
                    let durationSeconds = double (SDL3_mixer.MIX_AudioFramesToMS (audioAsset, durationFrames)) * 0.001
                    let startTimeSeconds = songDescriptor.StartTime.Seconds % durationSeconds
                    SDL3_mixer.MIX_SetTrackAudio (songTrack, audioAsset) |> ignore<SDLBool>
                    SDL3_mixer.MIX_SetTrackGain (songTrack, songDescriptor.Volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSongVolume) |> ignore
                    let mutable mixPoint = MIX_Point3D () in SDL3_mixer.MIX_SetTrack3DPosition (songTrack, &&mixPoint) |> ignore<SDLBool>
                    SDL3.SDL_SetNumberProperty (audioPlayer.SongTrackPropertiesId, SDL3_mixer.MIX_PROP_PLAY_LOOPS_NUMBER, loops) |> ignore<SDLBool>
                    SDL3.SDL_SetNumberProperty (audioPlayer.SongTrackPropertiesId, SDL3_mixer.MIX_PROP_PLAY_FADE_IN_MILLISECONDS_NUMBER, int64 (max Constants.Audio.FadeInSecondsMin songDescriptor.FadeInTime.Seconds * 1000.0)) |> ignore<SDLBool>
                    SDL3.SDL_SetNumberProperty (audioPlayer.SongTrackPropertiesId, SDL3_mixer.MIX_PROP_PLAY_START_MILLISECOND_NUMBER, int64 (startTimeSeconds * 1000.0)) |> ignore<SDLBool>
                    if not (SDL3_mixer.MIX_PlayTrack (songTrack, audioPlayer.SongTrackPropertiesId)) then
                        Log.info ("Could not play song asset '" + scstring song + "' due to '" + SDL3.SDL_GetError () + "'.")
                    audioPlayer.SongOpt <- Some (songDescriptor, songTrack)
                | (false, _) -> Log.info ("PlaySongMessage failed due to no free tracks for '" + scstring song + "'.")
            | None -> Log.info ("PlaySongMessage failed due to unloadable assets for '" + scstring song + "'.")

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
                match audioPlayer.FreeTracks.TryPop () with
                | (true, soundTrack) ->
                    SDL3_mixer.MIX_SetTrackAudio (soundTrack, audioAsset) |> ignore<SDLBool>
                    SDL3_mixer.MIX_SetTrackGain (soundTrack, soundDescriptor.Volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSoundVolume) |> ignore<SDLBool>
                    let mutable mixPoint = MIX_Point3D ()
                    mixPoint.x <- soundDescriptor.Panning
                    mixPoint.y <- 0.0f
                    mixPoint.z <- soundDescriptor.Distance // NOTE: forward is positive here.
                    SDL3_mixer.MIX_SetTrack3DPosition (soundTrack, &&mixPoint) |> ignore<SDLBool>
                    SDL3_mixer.MIX_PlayTrack (soundTrack, Unchecked.defaultof<SDL_PropertiesID>) |> ignore<SDLBool>
                | (false, _) -> Log.info ("PlaySoundMessage failed due to no free tracks for '" + scstring soundDescriptor.Sound + "'.")
            | None -> Log.info ("PlaySoundMessage failed due to unloadable assets for '" + scstring soundDescriptor.Sound + "'.")

    static member private handlePlaySong songDescriptor audioPlayer =
        SdlAudioPlayer.playSong songDescriptor audioPlayer

    static member private handleFadeOutSong (fadeOutTime : GameTime) audioPlayer =
        match audioPlayer.SongOpt with
        | Some (_, songTrack) ->
            let frames = SDL3_mixer.MIX_TrackMSToFrames (songTrack, int64 (fadeOutTime.Seconds * 1000.0))
            if not (SDL3_mixer.MIX_StopTrack (songTrack, frames)) then
                Log.info ("Could not fade out song due to '" + SDL3.SDL_GetError () + "'.")
        | None -> ()

    static member private handleStopSong audioPlayer =
        match audioPlayer.SongOpt with
        | Some (_, songTrack) ->
            if not (SDL3_mixer.MIX_StopTrack (songTrack, 0)) then
                Log.info ("Could not stop song due to '" + SDL3.SDL_GetError () + "'.")
        | None -> ()

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
        | Some (currentSong, songTrack) ->
            SDL3_mixer.MIX_SetTrackGain (songTrack, currentSong.Volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSongVolume) |> ignore<SDLBool>
        | None -> ()

    static member private setSongVolume volume audioPlayer =
        match audioPlayer.SongOpt with
        | Some (currentSong, songTrack) ->
            if currentSong.Volume <> volume then
                SDL3_mixer.MIX_SetTrackGain (songTrack, volume * audioPlayer.MasterAudioVolume * audioPlayer.MasterSongVolume) |> ignore<SDLBool>
                audioPlayer.SongOpt <- Some ({ currentSong with Volume = volume }, songTrack)
        | None -> ()

    static member private getSongFadingIn audioPlayer =
        match audioPlayer.SongOpt with
        | Some (_, songTrack) ->
            SDL3_mixer.MIX_GetTrackFadeFrames songTrack > 0L
        | None -> false

    static member private getSongFadingOut audioPlayer =
        match audioPlayer.SongOpt with
        | Some (_, songTrack) ->
            SDL3_mixer.MIX_GetTrackFadeFrames songTrack < 0L
        | None -> false

    /// Make an SdlAudioPlayer.
    static member make () =

        // ensure audio init was specified
        if SDL3.SDL_WasInit SDL_InitFlags.SDL_INIT_AUDIO = LanguagePrimitives.EnumOfValue 0u then
            failwith "Cannot create an AudioPlayer without SDL audio initialized."

        // attempt to create mixer, failing gracefully otherwise
        let mixerOpt =
            match SDL3_mixer.MIX_CreateMixerDevice (SDL3.SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, NativePtr.nullPtr) with
            | mixer when NativePtr.isNullPtr mixer ->
                Log.info ("Mixer could not initialize audio due to '" + SDL3.SDL_GetError () + "'.")
                None
            | mixer -> Some mixer

        // attempt to create properties
        let properties = SDL3.SDL_CreateProperties ()
        if properties = Unchecked.defaultof<_> then
            Log.warn ("SDL properties could not be created due to '" + SDL3.SDL_GetError () + "'.")

        // create audio player
        let audioPlayer =
            { MixerOpt = mixerOpt
              FreeTracks = Unchecked.defaultof<_>  
              ReturnTrack = Unchecked.defaultof<_>
              AudioPackages = dictPlus StringComparer.Ordinal []
              AudioMessages = List ()
              MasterAudioVolume = Constants.Audio.MasterAudioVolumeDefault
              MasterSoundVolume = Constants.Audio.MasterSoundVolumeDefault
              MasterSongVolume = Constants.Audio.MasterSongVolumeDefault
              SongTrackPropertiesId = properties
              SongOpt = None }

        // initialize return track handler
        audioPlayer.ReturnTrack <- SdlAudioStoppedCallback (fun (_ : voidptr) track ->
            match audioPlayer.SongOpt with
            | Some (_, songTrack) when songTrack = track -> audioPlayer.SongOpt <- None
            | _ -> ()
            audioPlayer.FreeTracks.Push track)

        // initialize free tracks
        audioPlayer.FreeTracks <-
            Seq.init Constants.Audio.TrackPoolSize (fun i ->
                match mixerOpt with
                | Some mixer ->
                    let track = SDL3_mixer.MIX_CreateTrack mixer
                    if NativePtr.isNullPtr track then
                        Log.info ("Track " + scstring i + " could not be created due to '" + SDL3.SDL_GetError () + "'.")
                    elif not (SDL3_mixer.MIX_SetTrackStoppedCallback (track, Marshal.GetFunctionPointerForDelegate<SdlAudioStoppedCallback> audioPlayer.ReturnTrack, 0n)) then
                        Log.info ("Track " + scstring i + " could not have its stopped callback set due to '" + SDL3.SDL_GetError () + "'.")
                    track
                | None -> NativePtr.nullPtr)
            |> Seq.takeWhile (not << NativePtr.isNullPtr)
            |> ConcurrentStack // seq ctor is more efficient than adding each track with Push, which needs to be thread-safe

        // fin
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
            | Some (_, songTrack) ->
                match SDL3_mixer.MIX_GetTrackPlaybackPosition songTrack with
                | -1L -> Log.info ("Could not get song position due to '" + SDL3.SDL_GetError () + "'."); GameTime.zero
                | positionFrames ->
                    match SDL3_mixer.MIX_TrackFramesToMS (songTrack, positionFrames) with
                    | -1L -> Log.info ("Could not convert song position in frames to ms due to '" + SDL3.SDL_GetError () + "'."); GameTime.zero
                    | positionMs -> GameTime.ofSeconds (double positionMs * 0.001)
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

        member audioPlayer.CleanUp () =
            match audioPlayer.MixerOpt with
            | Some mixer -> SDL3_mixer.MIX_DestroyMixer mixer // also destroys tracks
            | None -> ()
            let audioPackages = audioPlayer.AudioPackages |> Seq.map (fun entry -> entry.Value)
            let audioAssets = audioPackages |> Seq.map (fun package -> package.Assets.Values) |> Seq.concat
            for (_, _, audioAsset) in audioAssets do SDL3_mixer.MIX_DestroyAudio audioAsset
            audioPlayer.AudioPackages.Clear ()
            SDL3.SDL_DestroyProperties audioPlayer.SongTrackPropertiesId