namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module InfinityRpgModule =

    type Game with
    
        member game.Seed = game?Seed : uint64
        static member setSeed (value : uint64) (game : Game) = game?Seed <- value

    type InfinityRpgDispatcher () =
        inherit GameDispatcher ()

        static let makeField world =
            let rand = Rand.make world.Game.Seed
            let pathEdgesM = [(Vector2i (1, 10), Vector2i (20, 10))]
            let fieldMap = FieldMap.make FieldTileSheetImage (Vector2i 22) pathEdgesM rand
            let field = World.makeEntity typeof<FieldDispatcher>.Name (Some FieldName) world
            let field = Entity.setFieldMapNp fieldMap field
            let field = Entity.setSize (Entity.getQuickSize field world) field
            Entity.setPersistent false field

        static let handleNewGame _ world =

            // change game seed
            let systemRandomSeed = uint64 <| (Random ()).Next ()
            let game = Game.setSeed systemRandomSeed world.Game
            let world = World.setGame game world

            // make field
            let field = makeField world

            // make character
            let playerCharacter = World.makeEntity typeof<PlayerCharacterDispatcher>.Name (Some PlayerCharacterName) world
            let playerCharacter = Entity.setDepth 1.0f playerCharacter

            // make scene hierarchy
            let entities = Map.ofList [(field.Name, field); (playerCharacter.Name, playerCharacter)]
            let scene = World.makeGroup typeof<GroupDispatcher>.Name (Some SceneName) world
            let sceneHierarchy = (scene, entities)

            // add scene hierarchy to world
            let world = snd <| World.addGroup SceneAddress sceneHierarchy world
            (Cascade, world)

        static let handleLoadGame _ world =

            // replace game value
            let gameHierarchy = World.readGameFromFile SaveFilePath world
            let (game, screenHierarchy) = gameHierarchy
            let world = World.setGame game world

            // make field
            let field = makeField world

            // find scene hierarchy and add field to it
            let sceneHierarchy =
                Map.find GameplayName screenHierarchy |> snd |>
                Map.find SceneName
            let (scene, entities) = sceneHierarchy
            let entities = Map.add field.Name field entities
            let sceneHierarchy = (scene, entities)

            // add scene hierarchy to world
            let world = snd <| World.addGroup SceneAddress sceneHierarchy world
            (Cascade, world)

        static let handleClickSaveGame _ world =
            let gameHierarchy = World.getGame' world
            World.writeGameToFile SaveFilePath gameHierarchy world
            (Cascade, world)

        static let handleDeselectGameplay _ world =
            let scene = World.getGroup SceneAddress world
            let world = snd <| World.removeGroup SceneAddress scene world
            (Cascade, world)

        static let addTitle world =
            let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleAddress TitleGroupFilePath world
            let world = World.subscribe4 GameAddress ClickTitleCreditsEventAddress (World.handleAsScreenTransition CreditsAddress) world
            let world = World.subscribe4 GameAddress ClickTitleNewGameEventAddress (World.handleAsScreenTransition GameplayAddress) world
            let world = World.subscribe4 GameAddress ClickTitleLoadGameEventAddress (World.handleAsScreenTransition GameplayAddress) world
            let world = observe GameAddress ClickTitleNewGameEventAddress |> product (SelectEventAddress ->>- GameplayAddress) |> subscribe handleNewGame world |> snd
            let world = observe GameAddress ClickTitleLoadGameEventAddress |> product (SelectEventAddress ->>- GameplayAddress) |> subscribe handleLoadGame world |> snd
            World.subscribe4 GameAddress ClickTitleExitEventAddress World.handleAsExit world

        static let addCredits world =
            let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsAddress CreditsGroupFilePath world
            World.subscribe4 GameAddress ClickCreditsBackEventAddress (World.handleAsScreenTransition TitleAddress) world

        static let addGameplay world =
            let world = snd <| World.addDissolveScreenFromGroupFile true DissolveData typeof<ScreenDispatcher>.Name GameplayAddress HudFilePath world
            let world = World.setGroup HudAddress (Group.setPersistent false <| World.getGroup HudAddress world) world
            let world = World.subscribe4 GameAddress ClickHudBackEventAddress (World.handleAsScreenTransition TitleAddress) world
            let world = World.subscribe4 GameAddress ClickHudSaveGameEventAddress handleClickSaveGame world
            World.subscribe4 GameAddress (DeselectEventAddress ->>- GameplayAddress) handleDeselectGameplay world

        static member FieldDefinitions =
            [define? Seed Rand.DefaultSeed]

        override dispatcher.Register (game, world) =
            let world = World.hintRenderPackageUse GuiPackageName world
            let world = World.hintRenderPackageUse GameplayPackageName world
            let world = addTitle world
            let world = addCredits world
            let world = addGameplay world
            let (splashScreen, world) = World.addSplashScreen false NuSplashData typeof<ScreenDispatcher>.Name NuSplashAddress TitleAddress world
            let world = snd <| World.selectScreen NuSplashAddress splashScreen world
            (game, world)