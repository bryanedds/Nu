module Nu.Audio

type [<StructuralEquality; NoComparison>] Sound =
    { AssetName : Lun
      PackageName : Lun }

type [<StructuralEquality; NoComparison>] Song =
    { AssetName : string
      PackageName : string }

type [<StructuralEquality; NoComparison>] SongDescriptor =
    { Volume : float
      Repeat : bool
      Song : Song }

/// Describes an audio asset.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] AudioDescriptor =
    | SongDescriptor of SongDescriptor

type [<StructuralEquality; NoComparison>] SoundMessage =
    { Volume : single
      Sound : Sound }

type [<StructuralEquality; NoComparison>] HintAudioPackageUse =
    { FileName : string
      AudioPackageName : string
      HAPU : unit }

type [<StructuralEquality; NoComparison>] HintAudioPackageDisuse =
    { FileName : string
      AudioPackageName : string
      HAPD : unit }

type [<StructuralEquality; NoComparison>] AudioMessage =
    | HintAudioPackageUse of HintAudioPackageUse
    | HintAudioPackageDisuse of HintAudioPackageDisuse
    | PlaySound of SoundMessage

type [<ReferenceEquality>] AudioPlayer =
    private
        { AudioContext : unit }

let play audioMessages audioDescriptors audioPlayer : unit =
    () // TODO: play

let makeAudioPlayer () =
    { AudioContext = () }