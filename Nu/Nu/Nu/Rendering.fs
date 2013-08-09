module Nu.Rendering

type [<StructuralEquality; StructuralComparison>] RenderDescriptor =
    | SpriteDescriptor // of ...

type [<StructuralEquality; StructuralComparison>] RenderMessage =
    | ScreenFlashMessage // of ...

type [<ReferenceEquality>] Renderer =
    { RenderContext : unit }

let render renderMessages renderDescriptors renderer : unit =
    ()  // TODO: render