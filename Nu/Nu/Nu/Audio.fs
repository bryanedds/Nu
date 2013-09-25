module Nu.Audio

type [<StructuralEquality; NoComparison>] SongDescriptor =
    { Volume : float
      Repeat : bool
      AssetName : string
      PackageName : string }

/// Describes an audio asset.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] AudioDescriptor =
    | SongDescriptor of SongDescriptor

type [<StructuralEquality; NoComparison>] SoundMessage =
    { Volume : single
      AssetName : Lun
      PackageName : Lun }

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
    { AudioContext : unit }

let play audioMessages audioDescriptors audioPlayer : unit =
    () // TODO: play