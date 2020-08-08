// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldAudio =

    type World with

        static member internal getAudioPlayer world =
            world.Subsystems.AudioPlayer

        static member internal setAudioPlayer audioPlayer world =
            World.updateSubsystems (fun subsystems -> { subsystems with AudioPlayer = audioPlayer }) world

        static member internal updateAudioPlayer updater world =
            World.setAudioPlayer (updater (World.getAudioPlayer world)) world

        /// Enqueue an audio message to the world.
        static member enqueueAudioMessage (message : AudioMessage) world =
            World.updateAudioPlayer (fun audioPlayer -> AudioPlayer.enqueueMessage message audioPlayer; audioPlayer) world

        /// Enqueue multiple audio messages to the world.
        static member enqueueAudioMessages (messages : AudioMessage seq) world =
            Seq.fold (fun world message -> World.enqueueAudioMessage message world) world messages

        /// Send a message to the audio system to play a song.
        [<FunctionBinding>]
        static member playSong timeToFadeOutSongMs volume song world =
            let playSongMessage = PlaySongMessage { FadeOutMs = timeToFadeOutSongMs; Volume = volume; Song = song }
            World.enqueueAudioMessage playSongMessage world

        /// Send a message to the audio system to play a song.
        [<FunctionBinding "playSong4">]
        static member playSong5 timeToFadeOutSongMs volume songPackageName songAssetName world =
            let song = AssetTag.make<Song> songPackageName songAssetName
            World.playSong timeToFadeOutSongMs volume song world

        /// Send a message to the audio system to play a sound.
        [<FunctionBinding>]
        static member playSound volume sound world =
            let playSoundMessage = PlaySoundMessage { Sound = sound; Volume = volume }
            World.enqueueAudioMessage playSoundMessage world

        /// Send a message to the audio system to play a sound.
        [<FunctionBinding "playSound3">]
        static member playSound4 volume soundPackageName soundAssetName world =
            let sound = AssetTag.make<Sound> soundPackageName soundAssetName
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