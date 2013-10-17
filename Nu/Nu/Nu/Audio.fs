module Nu.Audio
open System
open System.IO
open SDL2
open Nu.Assets

let [<Literal>] TimeToFadeOutMs = 5000 // TODO: move to Constants

type [<StructuralEquality; NoComparison>] Sound =
    { SoundAssetName : Lun
      PackageName : Lun }

type [<StructuralEquality; NoComparison>] Song =
    { SongAssetName : Lun
      PackageName : Lun }

type [<StructuralEquality; NoComparison>] PlaySong =
    { Song : Song
      FadeOutCurrentSong : bool }

/// Describes an audio asset.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] AudioDescriptor =
    | TODO

type [<StructuralEquality; NoComparison>] PlaySound =
    { Volume : single
      Sound : Sound }

type [<StructuralEquality; NoComparison>] HintAudioPackageUse =
    { FileName : string
      PackageName : string
      HAPU : unit }

type [<StructuralEquality; NoComparison>] HintAudioPackageDisuse =
    { FileName : string
      PackageName : string
      HAPD : unit }

type [<StructuralEquality; NoComparison>] AudioMessage =
    | HintAudioPackageUse of HintAudioPackageUse
    | HintAudioPackageDisuse of HintAudioPackageDisuse
    | PlaySound of PlaySound
    | PlaySong of PlaySong
    | FadeOutSong
    | StopSong

type [<ReferenceEquality>] AudioAsset =
    | WavAsset of nativeint
    | OggAsset of nativeint

type [<ReferenceEquality>] AudioPlayer =
    private
        { AudioContext : unit // audio context, interestingly, is global
          AudioAssetMap : AudioAsset AssetMap
          OptCurrentSong : Song option
          OptNextSong : Song option }

let tryLoadAudioAsset audioContext (asset : Asset) =
    let extension = Path.GetExtension asset.FileName
    match extension with
    | ".wav" ->
        let optWav = SDL_mixer.Mix_LoadWAV asset.FileName
        if optWav = IntPtr.Zero then
            trace ("Could not load wav '" + asset.FileName + "'.")
            None
        else
            Some (Lun.make asset.Name, WavAsset optWav)
    | ".ogg" ->
        let optOgg = SDL_mixer.Mix_LoadMUS asset.FileName
        if optOgg = IntPtr.Zero then
            trace ("Could not load ogg '" + asset.FileName + "'.")
            None
        else
            Some (Lun.make asset.Name, OggAsset optOgg)
    | _ ->
        trace ("Could not load audio asset '" + str asset + "' due to unknown extension '" + extension + "'.")
        None

let playSong song audioPlayer =
    let optAudioAsset =
        Option.reduce
            (fun assetMap -> Map.tryFind song.SongAssetName assetMap)
            (Map.tryFind song.PackageName audioPlayer.AudioAssetMap)
    match optAudioAsset with
    | None -> () // TODO: load and play asset
    | Some (WavAsset wavAsset) -> ignore (SDL_mixer.Mix_PlayMusic (wavAsset, -1))
    | Some (OggAsset oggAsset) -> ignore (SDL_mixer.Mix_PlayMusic (oggAsset, -1))
    { audioPlayer with OptCurrentSong = Some song }

let tryUpdateCurrentSong audioPlayer =
    if SDL_mixer.Mix_PlayingMusic () = 1 then audioPlayer
    else { audioPlayer with OptCurrentSong = None }

let tryUpdateNextSong audioPlayer =
    match audioPlayer.OptNextSong with
    | None -> audioPlayer
    | Some nextSong ->
        if SDL_mixer.Mix_PlayingMusic () = 1 then audioPlayer
        else
            let audioPlayer2 = playSong nextSong audioPlayer
            { audioPlayer2 with OptNextSong = None }

let updateAudioPlayer audioPlayer =
    audioPlayer |>
        tryUpdateCurrentSong |>
        tryUpdateNextSong

let handleAudioMessage audioPlayer audioMessage =
    match audioMessage with
    | HintAudioPackageUse hintPackageUse ->
        let optAssets = Assets.tryLoadAssets "Audio" hintPackageUse.PackageName hintPackageUse.FileName
        match optAssets with
        | Left error ->
            trace ("HintAudioPackageUse failed due unloadable assets '" + error + "' for '" + str hintPackageUse + "'.")
            audioPlayer
        | Right assets ->
            let optAudioAssets = List.map (tryLoadAudioAsset audioPlayer.AudioContext) assets
            let audioAssets = List.definitize optAudioAssets
            let packageNameLun = Lun.make hintPackageUse.PackageName
            let optAudioAssetMap = Map.tryFind packageNameLun audioPlayer.AudioAssetMap
            match optAudioAssetMap with
            | None ->
                let audioAssetMap = Map.ofSeq audioAssets
                { audioPlayer with AudioAssetMap = Map.add packageNameLun audioAssetMap audioPlayer.AudioAssetMap }
            | Some audioAssetMap ->
                let audioAssetMap2 = Map.addMany audioAssets audioAssetMap
                { audioPlayer with AudioAssetMap = Map.add packageNameLun audioAssetMap2 audioPlayer.AudioAssetMap }
    | HintAudioPackageDisuse hintPackageDisuse ->
        let optAssets = Assets.tryLoadAssets "Audio" hintPackageDisuse.PackageName hintPackageDisuse.FileName
        match optAssets with
        | Left error ->
            trace ("HintAudioPackageDisuse failed due unloadable assets '" + error + "' for '" + str hintPackageDisuse + "'.")
            audioPlayer
        | Right assets -> audioPlayer // TODO: unload assets
    | PlaySound playSound ->
        let optAudioAsset =
            Option.reduce
                (fun assetMap -> Map.tryFind playSound.Sound.SoundAssetName assetMap)
                (Map.tryFind playSound.Sound.PackageName audioPlayer.AudioAssetMap)
        match optAudioAsset with
        | None -> () // TODO: load and play asset
        | Some (WavAsset wavAsset) -> ignore (SDL_mixer.Mix_PlayChannel (-1, wavAsset, 0))
        | Some (OggAsset oggAsset) -> ignore (SDL_mixer.Mix_PlayChannel (-1, oggAsset, 0))
        audioPlayer
    | PlaySong playSongValue ->
        if SDL_mixer.Mix_PlayingMusic () = 1 && playSongValue.FadeOutCurrentSong then
            ignore (SDL_mixer.Mix_FadeOutMusic TimeToFadeOutMs)
            { audioPlayer with OptNextSong = Some playSongValue.Song }
        else playSong playSongValue.Song audioPlayer
    | FadeOutSong ->
        if SDL_mixer.Mix_PlayingMusic () = 1 then ignore (SDL_mixer.Mix_FadeOutMusic TimeToFadeOutMs)
        audioPlayer
    | StopSong ->
        if SDL_mixer.Mix_PlayingMusic () = 1 then ignore (SDL_mixer.Mix_HaltMusic ())
        audioPlayer

let handleAudioMessages (audioMessages : AudioMessage rQueue) audioPlayer =
    List.fold handleAudioMessage audioPlayer (List.rev audioMessages)

let play audioMessages audioDescriptors audioPlayer =
    let audioPlayer2 = handleAudioMessages audioMessages audioPlayer
    () // TODO: do stuff with descriptors when we have some
    updateAudioPlayer audioPlayer2

let makeAudioPlayer () =
    { AudioContext = ()
      AudioAssetMap = Map.empty
      OptCurrentSong = None
      OptNextSong = None }