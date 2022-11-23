namespace Elmario
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    // here we create an entity reference for Elmario. This is useful for simulants that you want
    // to refer to from multiple places
    let Elmario = Simulants.Default.Group / "Elmario"