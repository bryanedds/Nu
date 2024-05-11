// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

[<AutoOpen>]
module WorldAudio =

    type World with

        static member internal getAudioPlayer world =
            world.Subsystems.AudioPlayer

        static member internal withAudioPlayer fn world =
            fn (World.getAudioPlayer world)

        /// Enqueue an audio message to the world.
        static member enqueueAudioMessage (message : AudioMessage) world =
            world.Subsystems.AudioPlayer.EnqueueMessage message

        /// Enqueue multiple audio messages to the world.
        static member enqueueAudioMessages (messages : AudioMessage seq) world =
            let audioPlayer = World.getAudioPlayer world
            for message in messages do audioPlayer.EnqueueMessage message

        /// Get the currently playing song, if any.
        static member getCurrentSongOpt world =
            let audioPlayer = World.getAudioPlayer world
            audioPlayer.CurrentSongOpt
            
        /// Get the currently playing song's position or 0.0.
        static member getCurrentSongPosition world =
            let audioPlayer = World.getAudioPlayer world
            audioPlayer.CurrentSongPosition

        /// Get the master volume.
        static member getMasterAudioVolume world =
            let audioPlayer = World.getAudioPlayer world
            audioPlayer.MasterAudioVolume

        /// Get the master sound volume.
        static member getMasterSoundVolume world =
            let audioPlayer = World.getAudioPlayer world
            audioPlayer.MasterSoundVolume

        /// Get the master song volume.
        static member getMasterSongVolume world =
            let audioPlayer = World.getAudioPlayer world
            audioPlayer.MasterSongVolume

        /// Set the master volume.
        static member setMasterAudioVolume volume world =
            let audioPlayer = World.getAudioPlayer world
            audioPlayer.MasterAudioVolume <- volume

        /// Set the master sound volume.
        static member setMasterSoundVolume volume world =
            let audioPlayer = World.getAudioPlayer world
            audioPlayer.MasterSoundVolume <- volume

        /// Set the master song volume.
        static member setMasterSongVolume volume world =
            let audioPlayer = World.getAudioPlayer world
            audioPlayer.MasterSongVolume <- volume

        /// Send a message to the audio system to play a song.
        static member playSong fadeOutTime fadeInTime startTime volume song world =
            let playSongMessage = PlaySongMessage { FadeOutTime = fadeOutTime; FadeInTime = fadeInTime; StartTime = startTime; Volume = volume; Song = song }
            World.enqueueAudioMessage playSongMessage world

        /// Send a message to the audio system to play a sound.
        static member playSound volume sound world =
            let playSoundMessage = PlaySoundMessage { Sound = sound; Volume = volume }
            World.enqueueAudioMessage playSoundMessage world

        /// Send a message to the audio system to fade out any current song.
        static member fadeOutSong fadeOutTime world =
            let fadeOutSongMessage = FadeOutSongMessage fadeOutTime
            World.enqueueAudioMessage fadeOutSongMessage world

        /// Send a message to the audio system to stop a song.
        static member stopSong world =
            World.enqueueAudioMessage StopSongMessage world
            
        /// Load an audio asset package. Should be used to avoid loading assets at inconvenient times (such as in the
        /// middle of game play!)
        static member loadAudioPackage packageName world =
            let loadAudioPackageMessage = LoadAudioPackageMessage packageName
            World.enqueueAudioMessage loadAudioPackageMessage world
            world
            
        /// Unload an audio package should be unloaded since its assets will not be used again soon.
        static member unloadAudioPackage packageName world =
            let unloadAudioPackageMessage = UnloadAudioPackageMessage packageName
            World.enqueueAudioMessage unloadAudioPackageMessage world
            world

        /// Send a message to the audio player to reload its audio assets.
        static member reloadAudioAssets world =
            let reloadAudioAssetsMessage = ReloadAudioAssetsMessage
            World.enqueueAudioMessage reloadAudioAssetsMessage world
            world