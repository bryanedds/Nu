namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu
open Nu.Core
open Nu.DomainModel
module GameModule =

    // WISDOM:
    //
    // A simulation that would put physics on another thread should likely do so in a different app
    // domain with communication via .NET remoting to make 100% sure that no sharing is happening.
    //
    // NOTE: for simulation types, value semantics are preferred over open semantics as it eases
    // serialization and other forms of automation. However, perhaps there is a way to get both...

    let gameLens =
        { Get = fun world -> world.Game
          Set = fun game world -> { world with Game = game }}

    let gameIdLens =
        { Get = fun world -> (get world gameLens).Id
          Set = fun value world -> set { get world gameLens with Id = value } world gameLens }
      
    let cameraLens =
        { Get = fun world -> world.Camera
          Set = fun camera world -> { world with Camera = camera }}

    let mouseStateLens =
        { Get = fun world -> world.MouseState
          Set = fun mouseState world -> { world with MouseState = mouseState }}

    let worldOptSelectedScreenAddressLens =
        { Get = fun world -> (get world gameLens).OptSelectedScreenAddress
          Set = fun value world -> set { (get world gameLens) with OptSelectedScreenAddress = value } world gameLens}

    let worldOptSelectedScreenLens =
        { Get = fun world ->
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None -> None
            | Some selectedScreenAddress -> get world <| Screens.worldOptScreenLens selectedScreenAddress
          Set = fun screen world ->
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None -> failwith "Cannot set a non-existent screen."
            | Some selectedScreenAddress -> set screen.Value world <| Screens.worldScreenLens selectedScreenAddress }