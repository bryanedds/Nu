// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldAudioModule =

    /// The subsystem for the world's audio player.
    type [<ReferenceEquality>] AudioPlayerSubsystem =
        private
            { SubsystemOrder : single
              AudioPlayer : IAudioPlayer }
    
        interface World Subsystem with
            member this.SubsystemType = AudioType
            member this.SubsystemOrder = this.SubsystemOrder
            member this.ClearMessages () = { this with AudioPlayer = this.AudioPlayer.ClearMessages () } :> World Subsystem
            member this.EnqueueMessage message = { this with AudioPlayer = this.AudioPlayer.EnqueueMessage (message :?> AudioMessage) } :> World Subsystem
            member this.ProcessMessages world = (() :> obj, { this with AudioPlayer = this.AudioPlayer.Play () } :> World Subsystem, world)
            member this.ApplyResult (_, world) = world
            member this.CleanUp world = (this :> World Subsystem, world)

        static member make subsystemOrder audioPlayer =
            { SubsystemOrder = subsystemOrder
              AudioPlayer = audioPlayer }

    type World with

        /// Add an audio message to the world.
        static member addAudioMessage (message : AudioMessage) world =
            World.updateSubsystem (fun aps _ -> aps.EnqueueMessage message) Constants.Engine.AudioPlayerSubsystemName world

        /// Send a message to the audio system to play a song.
        static member playSong timeToFadeOutSongMs volume song world =
            let playSongMessage = PlaySongMessage { TimeToFadeOutSongMs = timeToFadeOutSongMs; Volume = volume; Song = song }
            World.addAudioMessage playSongMessage world

        /// Send a message to the audio system to play a song.
        static member playSong6 timeToFadeOutSongMs volume songPackageName songAssetName world =
            let song = { PackageName = songPackageName; AssetName = songAssetName }
            World.playSong timeToFadeOutSongMs volume song world

        /// Send a message to the audio system to play a sound.
        static member playSound volume sound world =
            let playSoundMessage = PlaySoundMessage { Sound = sound; Volume = volume }
            World.addAudioMessage playSoundMessage world

        /// Send a message to the audio system to play a sound.
        static member playSound5 volume soundPackageName soundAssetName world =
            let sound = { PackageName = soundPackageName; AssetName = soundAssetName }
            World.playSound volume sound world

        /// Send a message to the audio system to fade out a song.
        static member fadeOutSong timeToFadeOutSongMs world =
            let fadeOutSongMessage = FadeOutSongMessage timeToFadeOutSongMs
            World.addAudioMessage fadeOutSongMessage world

        /// Send a message to the audio system to stop a song.
        static member stopSong world =
            World.addAudioMessage StopSongMessage world
            
        /// Hint that an audio asset package with the given name should be loaded. Should be used
        /// to avoid loading assets at inconvenient times (such as in the middle of game play!)
        static member hintAudioPackageUse packageName world =
            let hintAudioPackageUseMessage = HintAudioPackageUseMessage { PackageName = packageName }
            World.addAudioMessage hintAudioPackageUseMessage world
            
        /// Hint that an audio package should be unloaded since its assets will not be used again
        /// (or until specified via a HintAudioPackageUseMessage).
        static member hintAudioPackageDisuse packageName world =
            let hintAudioPackageDisuseMessage = HintAudioPackageDisuseMessage { PackageName = packageName }
            World.addAudioMessage hintAudioPackageDisuseMessage world

        /// Send a message to the audio player to reload its audio assets.
        static member reloadAudioAssets world =
            let reloadAudioAssetsMessage = ReloadAudioAssetsMessage
            World.addAudioMessage reloadAudioAssetsMessage world

/// The subsystem for the world's audio player.
type AudioPlayerSubsystem = WorldAudioModule.AudioPlayerSubsystem