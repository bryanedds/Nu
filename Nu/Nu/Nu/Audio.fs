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
      AssetName : string
      PackageName : string }

type [<StructuralEquality; NoComparison>] AudioMessage =
    | HintPackageUse of (*FileName*) string * (*PackageName*) string
    | HintPackageDisuse of (*PackageName*) string
    | PlaySound of SoundMessage

type [<ReferenceEquality>] AudioPlayer =
    { AudioContext : unit }

let play audioMessages audioDescriptors audioPlayer : unit =
    () // TODO: play