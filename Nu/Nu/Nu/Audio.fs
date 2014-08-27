namespace Nu
open System
open System.IO
open System.ComponentModel
open SDL2
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module AudioModule =

    type [<StructuralEquality; NoComparison; XDefaultValue (DefaultSongValue)>] Song =
        { SongAssetName : string
          PackageName : string
          PackageFileName : string }

    type [<StructuralEquality; NoComparison; XDefaultValue (DefaultSoundValue)>] Sound =
        { SoundAssetName : string
          PackageName : string
          PackageFileName : string }

    type [<StructuralEquality; NoComparison>] PlaySongMessage =
        { Song : Song
          Volume : single
          TimeToFadeOutSongMs : int }

    type [<StructuralEquality; NoComparison>] PlaySoundMessage =
        { Volume : single
          Sound : Sound }

    type [<StructuralEquality; NoComparison>] HintAudioPackageUseMessage =
        { FileName : string
          PackageName : string }

    type [<StructuralEquality; NoComparison>] HintAudioPackageDisuseMessage =
        { FileName : string
          PackageName : string }

    type [<StructuralEquality; NoComparison>] ReloadAudioAssetsMessage =
        { FileName : string }

    type [<StructuralEquality; NoComparison>] AudioMessage =
        | HintAudioPackageUseMessage of HintAudioPackageUseMessage
        | HintAudioPackageDisuseMessage of HintAudioPackageDisuseMessage
        | PlaySoundMessage of PlaySoundMessage
        | PlaySongMessage of PlaySongMessage
        | FadeOutSongMessage of int
        | StopSongMessage
        | ReloadAudioAssetsMessage of ReloadAudioAssetsMessage

    type [<ReferenceEquality>] AudioAsset =
        | WavAsset of nativeint
        | OggAsset of nativeint

    type [<ReferenceEquality>] AudioPlayer =
        { AudioContext : unit // audio context, interestingly, is global
          AudioAssetMap : AudioAsset AssetMap
          OptCurrentSong : PlaySongMessage option
          OptNextPlaySong : PlaySongMessage option }

    type SoundTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj, _) =
            let s = obj :?> Sound
            String.Format (culture, "{0};{1};{2}", s.SoundAssetName, s.PackageName, s.PackageFileName) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Sound> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Sound> then obj
            else
                let args = (obj :?> string).Split ';'
                { SoundAssetName = args.[0]; PackageName = args.[1]; PackageFileName = args.[2] } :> obj

    type SongTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj, _) =
            let s = obj :?> Song
            String.Format (culture, "{0};{1};{2}", s.SongAssetName, s.PackageName, s.PackageFileName) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Song> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Song> then obj
            else
                let args = (obj :?> string).Split ';'
                { SongAssetName = args.[0]; PackageName = args.[1]; PackageFileName = args.[2] } :> obj

[<RequireQualifiedAccess>]
module Audio = 

    let initTypeConverters () =
        assignTypeConverter<Sound, SoundTypeConverter> ()
        assignTypeConverter<Song, SongTypeConverter> ()

    let private haltSound () =
        ignore <| SDL_mixer.Mix_HaltMusic ()
        let channelCount = ref 0
        ignore <| SDL_mixer.Mix_QuerySpec (ref 0, ref 0us, channelCount)
        for i in [0 .. !channelCount - 1] do
            ignore <| SDL_mixer.Mix_HaltChannel i

    let private tryLoadAudioAsset2 (asset : Asset) =
        let extension = Path.GetExtension asset.FileName
        match extension with
        | ".wav" ->
            let optWav = SDL_mixer.Mix_LoadWAV asset.FileName
            if optWav <> IntPtr.Zero then Some (asset.Name, WavAsset optWav)
            else
                let errorMsg = SDL.SDL_GetError ()
                trace <| "Could not load wav '" + asset.FileName + "' due to '" + errorMsg + "'."
                None
        | ".ogg" ->
            let optOgg = SDL_mixer.Mix_LoadMUS asset.FileName
            if optOgg <> IntPtr.Zero then Some (asset.Name, OggAsset optOgg)
            else
                let errorMsg = SDL.SDL_GetError ()
                trace <| "Could not load ogg '" + asset.FileName + "' due to '" + errorMsg + "'."
                None
        | _ -> trace <| "Could not load audio asset '" + string asset + "' due to unknown extension '" + extension + "'."; None

    let private tryLoadAudioPackage packageName fileName audioPlayer =
        let optAssets = Assets.tryLoadAssetsFromPackage (Some AudioAssociation) packageName fileName
        match optAssets with
        | Left error ->
            trace <| "HintAudioPackageUseMessage failed due unloadable assets '" + error + "' for '" + string (packageName, fileName) + "'."
            audioPlayer
        | Right assets ->
            let optAudioAssets = List.map tryLoadAudioAsset2 assets
            let audioAssets = List.definitize optAudioAssets
            let optAudioAssetMap = Map.tryFind packageName audioPlayer.AudioAssetMap
            match optAudioAssetMap with
            | None ->
                let audioAssetMap = Map.ofSeq audioAssets
                { audioPlayer with AudioAssetMap = Map.add packageName audioAssetMap audioPlayer.AudioAssetMap }
            | Some audioAssetMap ->
                let audioAssetMap = Map.addMany audioAssets audioAssetMap
                { audioPlayer with AudioAssetMap = Map.add packageName audioAssetMap audioPlayer.AudioAssetMap }

    let private tryLoadAudioAsset packageName packageFileName assetName audioPlayer =
        let optAssetMap = Map.tryFind packageName audioPlayer.AudioAssetMap
        let (audioPlayer, optAssetMap) =
            match optAssetMap with
            | None ->
                note <| "Loading audio package '" + packageName + "' for asset '" + assetName + "' on the fly."
                let audioPlayer = tryLoadAudioPackage packageName packageFileName audioPlayer
                (audioPlayer, Map.tryFind packageName audioPlayer.AudioAssetMap)
            | Some _ -> (audioPlayer, Map.tryFind packageName audioPlayer.AudioAssetMap)
        (audioPlayer, Option.bind (fun assetMap -> Map.tryFind assetName assetMap) optAssetMap)

    let private playSong playSongMessage audioPlayer =
        let song = playSongMessage.Song
        let (audioPlayer', optAudioAsset) = tryLoadAudioAsset song.PackageName song.PackageFileName song.SongAssetName audioPlayer
        match optAudioAsset with
        | None -> debug <| "PlaySongMessage failed due to unloadable assets for '" + string song + "'."
        | Some (WavAsset _) -> debug <| "Cannot play wav file as song '" + string song + "'."
        | Some (OggAsset oggAsset) ->
            ignore <| SDL_mixer.Mix_VolumeMusic (int <| playSongMessage.Volume * single SDL_mixer.MIX_MAX_VOLUME)
            ignore <| SDL_mixer.Mix_PlayMusic (oggAsset, -1)
        { audioPlayer' with OptCurrentSong = Some playSongMessage }

    let private handleHintAudioPackageUse (hintPackageUse : HintAudioPackageUseMessage) audioPlayer =
        tryLoadAudioPackage hintPackageUse.PackageName hintPackageUse.FileName audioPlayer

    let private handleHintAudioPackageDisuse (hintPackageDisuse : HintAudioPackageDisuseMessage) audioPlayer =
        let packageName = hintPackageDisuse.PackageName
        let optAssets = Map.tryFind packageName audioPlayer.AudioAssetMap
        match optAssets with
        | None -> audioPlayer
        | Some assets ->
            // all sounds / music must be halted because one of them might be playing during unload
            // (which is very bad according to the API docs).
            haltSound ()
            for asset in Map.toValueList assets do
                match asset with
                | WavAsset wavAsset -> SDL_mixer.Mix_FreeChunk wavAsset
                | OggAsset oggAsset -> SDL_mixer.Mix_FreeMusic oggAsset
            { audioPlayer with AudioAssetMap = Map.remove packageName audioPlayer.AudioAssetMap }

    let private handlePlaySound playSoundMessage audioPlayer =
        let sound = playSoundMessage.Sound
        let (audioPlayer, optAudioAsset) = tryLoadAudioAsset sound.PackageName sound.PackageFileName sound.SoundAssetName audioPlayer
        match optAudioAsset with
        | None -> debug <| "PlaySoundMessage failed due to unloadable assets for '" + string sound + "'."
        | Some (WavAsset wavAsset) ->
            ignore <| SDL_mixer.Mix_VolumeChunk (wavAsset, int <| playSoundMessage.Volume * single SDL_mixer.MIX_MAX_VOLUME)
            ignore <| SDL_mixer.Mix_PlayChannel (-1, wavAsset, 0)
        | Some (OggAsset _) -> debug <| "Cannot play ogg file as sound '" + string sound + "'."
        audioPlayer

    let private handlePlaySong playSongMessage audioPlayer =
        if SDL_mixer.Mix_PlayingMusic () = 1 then
            if audioPlayer.OptCurrentSong = Some playSongMessage then audioPlayer
            else
                if  playSongMessage.TimeToFadeOutSongMs <> 0 &&
                    not (SDL_mixer.Mix_FadingMusic () = SDL_mixer.Mix_Fading.MIX_FADING_OUT) then
                    ignore <| SDL_mixer.Mix_FadeOutMusic playSongMessage.TimeToFadeOutSongMs
                else
                    ignore <| SDL_mixer.Mix_HaltMusic ()
                { audioPlayer with OptNextPlaySong = Some playSongMessage }
        else playSong playSongMessage audioPlayer

    let private handleFadeOutSong timeToFadeOutSongMs audioPlayer =
        if SDL_mixer.Mix_PlayingMusic () = 1 then
            if  timeToFadeOutSongMs <> 0 &&
                not (SDL_mixer.Mix_FadingMusic () = SDL_mixer.Mix_Fading.MIX_FADING_OUT) then
                ignore <| SDL_mixer.Mix_FadeOutMusic timeToFadeOutSongMs
            else
                ignore <| SDL_mixer.Mix_HaltMusic ()
        audioPlayer

    let private handleStopSong audioPlayer =
        if SDL_mixer.Mix_PlayingMusic () = 1 then ignore <| SDL_mixer.Mix_HaltMusic ()
        audioPlayer

    let private handleReloadAudioAssets reloadAudioAssetsMessage audioPlayer =
        let oldAssetMap = audioPlayer.AudioAssetMap
        let audioPlayer = { audioPlayer with AudioAssetMap = Map.empty }
        List.fold
            (fun audioPlayer packageName -> tryLoadAudioPackage packageName reloadAudioAssetsMessage.FileName audioPlayer)
            audioPlayer
            (Map.toKeyList oldAssetMap)

    let private handleAudioMessage audioPlayer audioMessage =
        match audioMessage with
        | HintAudioPackageUseMessage hintPackageUse -> handleHintAudioPackageUse hintPackageUse audioPlayer
        | HintAudioPackageDisuseMessage hintPackageDisuse -> handleHintAudioPackageDisuse hintPackageDisuse audioPlayer
        | PlaySoundMessage playSoundMessage -> handlePlaySound playSoundMessage audioPlayer
        | PlaySongMessage playSongMessage -> handlePlaySong playSongMessage audioPlayer
        | FadeOutSongMessage timeToFadeSongMs -> handleFadeOutSong timeToFadeSongMs audioPlayer
        | StopSongMessage -> handleStopSong audioPlayer
        | ReloadAudioAssetsMessage reloadAudioAssetsMessage -> handleReloadAudioAssets reloadAudioAssetsMessage audioPlayer

    let private handleAudioMessages (audioMessages : AudioMessage rQueue) audioPlayer =
        List.fold handleAudioMessage audioPlayer (List.rev audioMessages)

    let private tryUpdateCurrentSong audioPlayer =
        if SDL_mixer.Mix_PlayingMusic () = 1 then audioPlayer
        else { audioPlayer with OptCurrentSong = None }

    let private tryUpdateNextSong audioPlayer =
        match audioPlayer.OptNextPlaySong with
        | None -> audioPlayer
        | Some nextPlaySong ->
            if SDL_mixer.Mix_PlayingMusic () = 1 then audioPlayer
            else
                let audioPlayer = handlePlaySong nextPlaySong audioPlayer
                { audioPlayer with OptNextPlaySong = None }

    let private updateAudioPlayer audioPlayer =
        audioPlayer |>
            tryUpdateCurrentSong |>
            tryUpdateNextSong

    let play audioMessages audioPlayer =
        let audioPlayer = handleAudioMessages audioMessages audioPlayer
        updateAudioPlayer audioPlayer

    let makeAudioPlayer () =
        { AudioContext = ()
          AudioAssetMap = Map.empty
          OptCurrentSong = None
          OptNextPlaySong = None }