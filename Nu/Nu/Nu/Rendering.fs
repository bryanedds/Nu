module Nu.Rendering

/// Describes a game asset, such as a texture, sound, or model in the abstract.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] SpriteDescriptor =
    { AssetName : string
      PackageName : string }

type [<StructuralEquality; NoComparison>] RenderDescriptor =
    | SpriteDescriptor of SpriteDescriptor

type [<StructuralEquality; NoComparison>] RenderMessage =
    | HintPackageUse of (*FileName*) string * (*PackageName*) string
    | HintPackageDisuse of (*PackageName*) string
    | ScreenFlash // of ...

type [<ReferenceEquality>] Renderer =
    { RenderContext : unit }

let render renderMessages renderDescriptors renderer : unit =
    ()  // TODO: render