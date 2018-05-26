// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldAudioModule =

    /// The subsystem for the world's audio player.
    type [<ReferenceEquality>] AudioPlayerSubsystem =
        private
            { SubsystemOrder : single
              AudioPlayer : IAudioPlayer }
    
        interface World Subsystem with
            member this.SubsystemType = AudioType
            member this.SubsystemOrder = this.SubsystemOrder
            member this.ClearMessages () = { this with AudioPlayer = AudioPlayer.clearMessages this.AudioPlayer } :> World Subsystem
            member this.EnqueueMessage message = { this with AudioPlayer = AudioPlayer.enqueueMessage (message :?> AudioMessage) this.AudioPlayer } :> World Subsystem
            member this.ProcessMessages world = (() :> obj, { this with AudioPlayer = AudioPlayer.play this.AudioPlayer } :> World Subsystem, world)
            member this.ApplyResult (_, world) = world
            member this.CleanUp world = (this :> World Subsystem, world)

        static member make subsystemOrder audioPlayer =
            { SubsystemOrder = subsystemOrder
              AudioPlayer = audioPlayer }

    type World with

        /// Enqueue an audio message in the world.
        static member enqueueAudioMessage (message : AudioMessage) world =
            World.updateSubsystem (fun aps _ -> Subsystem.enqueueMessage message aps) Constants.Engine.AudioPlayerSubsystemName world

        /// Send a message to the audio system to play a song.
        [<FunctionBinding>]
        static member playSong timeToFadeOutSongMs volume song world =
            let playSongMessage = PlaySongMessage { TimeToFadeOutSongMs = timeToFadeOutSongMs; Volume = volume; Song = song }
            World.enqueueAudioMessage playSongMessage world

        /// Send a message to the audio system to play a song.
        [<FunctionBinding ("playSong4")>]
        static member playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world =
            let song = AssetTag.make<Audio> songPackageName songAssetName
            World.playSong timeToFadeOutSongMs volume song world

        /// Send a message to the audio system to play a sound.
        [<FunctionBinding>]
        static member playSound volume sound world =
            let playSoundMessage = PlaySoundMessage { Sound = sound; Volume = volume }
            World.enqueueAudioMessage playSoundMessage world

        /// Send a message to the audio system to play a sound.
        [<FunctionBinding ("playSound3")>]
        static member playSound4 volume soundPackageName soundAssetName world =
            let sound = AssetTag.make<Audio> soundPackageName soundAssetName
            World.playSound volume sound world

        /// Send a message to the audio system to fade out any current song.
        [<FunctionBinding>]
        static member fadeOutSong timeToFadeOutSongMs world =
            let fadeOutSongMessage = FadeOutSongMessage timeToFadeOutSongMs
            World.enqueueAudioMessage fadeOutSongMessage world

        /// Send a message to the audio system to stop a song.
        [<FunctionBinding>]
        static member stopSong world =
            World.enqueueAudioMessage StopSongMessage world
            
        /// Hint that an audio asset package with the given name should be loaded. Should be used
        /// to avoid loading assets at inconvenient times (such as in the middle of game play!)
        [<FunctionBinding>]
        static member hintAudioPackageUse packageName world =
            let hintAudioPackageUseMessage = HintAudioPackageUseMessage packageName
            World.enqueueAudioMessage hintAudioPackageUseMessage world
            
        /// Hint that an audio package should be unloaded since its assets will not be used again
        /// (or until specified via a HintAudioPackageUseMessage).
        [<FunctionBinding>]
        static member hintAudioPackageDisuse packageName world =
            let hintAudioPackageDisuseMessage = HintAudioPackageDisuseMessage packageName
            World.enqueueAudioMessage hintAudioPackageDisuseMessage world

        /// Send a message to the audio player to reload its audio assets.
        [<FunctionBinding>]
        static member reloadAudioAssets world =
            let reloadAudioAssetsMessage = ReloadAudioAssetsMessage
            World.enqueueAudioMessage reloadAudioAssetsMessage world

/// The subsystem for the world's audio player.
type AudioPlayerSubsystem = WorldAudioModule.AudioPlayerSubsystem