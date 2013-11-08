module Nu.Audio
open System
open System.IO
open System.ComponentModel
open SDL2
open Nu.Core
open Nu.Constants
open Nu.Assets

type [<StructuralEquality; NoComparison>] Sound =
    { SoundAssetName : Lun
      PackageName : Lun
      PackageFileName : string }

type [<StructuralEquality; NoComparison>] Song =
    { SongAssetName : Lun
      PackageName : Lun
      PackageFileName : string }

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

type SoundTypeConverter () =
    inherit TypeConverter ()
    override this.CanConvertTo (_, destType) =
        destType = typeof<string>
    override this.ConvertTo (_, culture, obj : obj, _) =
        let s = obj :?> Sound
        String.Format (culture, "{0};{1};{2}", s.SoundAssetName, s.PackageName, s.PackageFileName) :> obj
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Sound> || sourceType = typeof<string>
    override this.ConvertFrom (_, culture, obj : obj) =
        let sourceType = obj.GetType ()
        if sourceType = typeof<Sound> then obj
        else
            let args = (obj :?> string).Split ';'
            { SoundAssetName = Lun.make args.[0]; PackageName = Lun.make args.[1]; PackageFileName = args.[2] } :> obj

type SongTypeConverter () =
    inherit TypeConverter ()
    override this.CanConvertTo (_, destType) =
        destType = typeof<string>
    override this.ConvertTo (_, culture, obj : obj, _) =
        let s = obj :?> Song
        String.Format (culture, "{0};{1};{2}", s.SongAssetName, s.PackageName, s.PackageFileName) :> obj
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Song> || sourceType = typeof<string>
    override this.ConvertFrom (_, culture, obj : obj) =
        let sourceType = obj.GetType ()
        if sourceType = typeof<Song> then obj
        else
            let args = (obj :?> string).Split ';'
            { SongAssetName = Lun.make args.[0]; PackageName = Lun.make args.[1]; PackageFileName = args.[2] } :> obj

let initAudioConverters () =
    AssignTypeConverter<Sound, SoundTypeConverter> ()
    AssignTypeConverter<Song, SongTypeConverter> ()

let haltSound () =
    ignore (SDL_mixer.Mix_HaltMusic ())
    let channelCount = ref 0
    ignore (SDL_mixer.Mix_QuerySpec (ref 0, ref 0us, channelCount))
    for i in [0 .. !channelCount - 1] do
        ignore (SDL_mixer.Mix_HaltChannel i)

let tryLoadAudioAsset2 audioContext (asset : Asset) =
    let extension = Path.GetExtension asset.FileName
    match extension with
    | ".wav" ->
        let optWav = SDL_mixer.Mix_LoadWAV asset.FileName
        if optWav <> IntPtr.Zero then Some (Lun.make asset.Name, WavAsset optWav)
        else trace ("Could not load wav '" + asset.FileName + "'."); None
    | ".ogg" ->
        let optOgg = SDL_mixer.Mix_LoadMUS asset.FileName
        if optOgg <> IntPtr.Zero then Some (Lun.make asset.Name, OggAsset optOgg)
        else trace ("Could not load ogg '" + asset.FileName + "'."); None
    | _ -> trace ("Could not load audio asset '" + str asset + "' due to unknown extension '" + extension + "'."); None

let tryLoadAudioPackage packageName fileName audioPlayer =
    let optAssets = tryLoadAssets "Audio" packageName.LunStr fileName
    match optAssets with
    | Left error ->
        trace ("HintAudioPackageUse failed due unloadable assets '" + error + "' for '" + str (packageName, fileName) + "'.")
        audioPlayer
    | Right assets ->
        let optAudioAssets = List.map (tryLoadAudioAsset2 audioPlayer.AudioContext) assets
        let audioAssets = List.definitize optAudioAssets
        let optAudioAssetMap = Map.tryFind packageName audioPlayer.AudioAssetMap
        match optAudioAssetMap with
        | None ->
            let audioAssetMap = Map.ofSeq audioAssets
            { audioPlayer with AudioAssetMap = Map.add packageName audioAssetMap audioPlayer.AudioAssetMap }
        | Some audioAssetMap ->
            let audioAssetMap2 = Map.addMany audioAssets audioAssetMap
            { audioPlayer with AudioAssetMap = Map.add packageName audioAssetMap2 audioPlayer.AudioAssetMap }

let tryLoadAudioAsset packageName packageFileName assetName audioPlayer_ =
    let optAssetMap = Map.tryFind packageName audioPlayer_.AudioAssetMap
    let (audioPlayer_, optAssetMap_) =
        match optAssetMap with
        | None ->
            log ("Loading audio package '" + packageName.LunStr + "' for asset '" + assetName.LunStr + "' on the fly.")
            let audioPlayer_ = tryLoadAudioPackage packageName packageFileName audioPlayer_
            (audioPlayer_, Map.tryFind packageName audioPlayer_.AudioAssetMap)
        | Some assetMap -> (audioPlayer_, Map.tryFind packageName audioPlayer_.AudioAssetMap)
    (audioPlayer_, Option.bind (fun assetMap -> Map.tryFind assetName assetMap) optAssetMap_)

let playSong (song : Song) audioPlayer =
    let (audioPlayer2, optAudioAsset) = tryLoadAudioAsset song.PackageName song.PackageFileName song.SongAssetName audioPlayer
    match optAudioAsset with
    | None -> debug ("PlaySong failed due to unloadable assets for '" + str song + "'.")
    | Some (WavAsset wavAsset) -> debug ("Cannot play wav file as song '" + str song + "'.")
    | Some (OggAsset oggAsset) -> ignore (SDL_mixer.Mix_PlayMusic (oggAsset, -1))
    { audioPlayer2 with OptCurrentSong = Some song }

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

let handleHintAudioPackageUse (hintPackageUse : HintAudioPackageUse) audioPlayer =
    tryLoadAudioPackage (Lun.make hintPackageUse.PackageName) hintPackageUse.FileName audioPlayer

let handleHintAudioPackageDisuse (hintPackageDisuse : HintAudioPackageDisuse) audioPlayer =
    let packageNameLun = Lun.make hintPackageDisuse.PackageName
    let optAssets = Map.tryFind packageNameLun audioPlayer.AudioAssetMap
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
        { audioPlayer with AudioAssetMap = Map.remove packageNameLun audioPlayer.AudioAssetMap }

let handlePlaySound playSound audioPlayer =
    let sound = playSound.Sound
    let (audioPlayer2, optAudioAsset) = tryLoadAudioAsset sound.PackageName sound.PackageFileName sound.SoundAssetName audioPlayer
    match optAudioAsset with
    | None -> debug ("PlaySound failed due to unloadable assets for '" + str sound + "'.")
    | Some (WavAsset wavAsset) -> ignore (SDL_mixer.Mix_PlayChannel (-1, wavAsset, 0))
    | Some (OggAsset oggAsset) -> debug ("Cannot play ogg file as sound '" + str sound + "'.")
    audioPlayer2

let handlePlaySong playSongValue audioPlayer =
    if SDL_mixer.Mix_PlayingMusic () = 1 then
        if  playSongValue.FadeOutCurrentSong &&
            not (SDL_mixer.Mix_FadingMusic () = SDL_mixer.Mix_Fading.MIX_FADING_OUT) then
            ignore (SDL_mixer.Mix_FadeOutMusic TimeToFadeOutSongMs)
        { audioPlayer with OptNextSong = Some playSongValue.Song }
    else playSong playSongValue.Song audioPlayer

let handleFadeOutSong audioPlayer =
    if  SDL_mixer.Mix_PlayingMusic () = 1 &&
        not (SDL_mixer.Mix_FadingMusic () = SDL_mixer.Mix_Fading.MIX_FADING_OUT) then
        ignore (SDL_mixer.Mix_FadeOutMusic TimeToFadeOutSongMs)
    audioPlayer

let handleStopSong audioPlayer =
    if SDL_mixer.Mix_PlayingMusic () = 1 then ignore (SDL_mixer.Mix_HaltMusic ())
    audioPlayer

let handleAudioMessage audioPlayer audioMessage =
    match audioMessage with
    | HintAudioPackageUse hintPackageUse -> handleHintAudioPackageUse hintPackageUse audioPlayer
    | HintAudioPackageDisuse hintPackageDisuse -> handleHintAudioPackageDisuse hintPackageDisuse audioPlayer
    | PlaySound playSound -> handlePlaySound playSound audioPlayer
    | PlaySong playSongValue -> handlePlaySong playSongValue audioPlayer
    | FadeOutSong -> handleFadeOutSong audioPlayer
    | StopSong -> handleStopSong audioPlayer

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