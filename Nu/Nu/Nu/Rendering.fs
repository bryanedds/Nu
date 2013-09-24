module Nu.Rendering

type [<StructuralEquality; NoComparison>] SpriteDescriptor =
    { AssetName : string
      PackageName : string }

/// Describes a rendering asset.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] RenderDescriptor =
    | SpriteDescriptor of SpriteDescriptor

type [<StructuralEquality; NoComparison>] RenderMessage =
    | HintPackageUse of (*FileName*) string * (*PackageName*) string
    | HintPackageDisuse of (*PackageName*) string
    | ScreenFlash // of ...

type [<ReferenceEquality>] Renderer =
    { RenderContext : unit }

let render renderMessages renderDescriptors renderer : unit =
    () // TODO: render