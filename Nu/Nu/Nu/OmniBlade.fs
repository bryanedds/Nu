module Nu.OmniBlade
open System
open SDL2
open OpenTK
open TiledSharp
open Nu.Core
open Nu.Constants
open Nu.Sdl
open Nu.Audio
open Nu.Rendering
open Nu.Physics
open Nu.AssetMetadata
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.Simulation

let tryCreateOmniBladeWorld (sdlDeps : SdlDeps) =
    let game =
        { Id = getNuId ()
          Screens = Map.empty
          OptSelectedScreenAddress = None
          GameSemantic = () }
    match tryGenerateAssetMetadataMap "AssetGraph.xml" with
    | Left errorMsg -> Left errorMsg
    | Right assetMetadataMap ->
        let world =
            { Game = game
              Camera = { EyePosition = Vector2.Zero; EyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH) }
              Subscriptions = Map.empty
              MouseState = { MousePosition = Vector2.Zero; MouseLeftDown = false; MouseRightDown = false; MouseCenterDown = false }
              AudioPlayer = makeAudioPlayer ()
              Renderer = makeRenderer sdlDeps.RenderContext
              Integrator = makeIntegrator Gravity
              AssetMetadataMap = assetMetadataMap
              AudioMessages = []
              RenderMessages = []
              PhysicsMessages = []
              Components = [] }
        Right world