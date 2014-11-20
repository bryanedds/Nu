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

[<RequireQualifiedAccess>]
module Progression =

    // TODO: consider moving nearly all of this into InfinityRpgDispatcher.

    let private makeField world =
        let rand = Rand.make world.Game.Seed
        let pathEdges = [(Vector2i (1, 10), Vector2i (20, 10))]
        let fieldMap = FieldMap.make FieldTileSheetImage (Vector2i 22) pathEdges rand
        let field = World.makeEntity typeof<FieldDispatcher>.Name (Some FieldName) world
        let field = Entity.setFieldMapNp fieldMap field
        let field = Entity.setSize (Entity.getQuickSize field world) field
        Entity.setPersistent false field

    let private handleNewGame _ world =

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

    let private handleLoadGame _ world =

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

    let private handleClickSaveGame _ world =
        let gameHierarchy = World.getGame' world
        World.writeGameToFile SaveFilePath gameHierarchy world
        (Cascade, world)

    let private handleStopGameplay _ world =
        let scene = World.getGroup SceneAddress world
        let world = snd <| World.removeGroup SceneAddress scene world
        (Cascade, world)

    let private addTitleScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name TitleAddress TitleGroupFilePath world
        let world = World.subscribe4 GameAddress ClickTitleCreditsEventAddress (World.handleAsScreenTransition CreditsAddress) world
        let world = World.subscribe4 GameAddress ClickTitleNewGameEventAddress (World.handleAsScreenTransition GameplayAddress) world
        let world = World.subscribe4 GameAddress ClickTitleLoadGameEventAddress (World.handleAsScreenTransition GameplayAddress) world
        let world = observe GameAddress ClickTitleNewGameEventAddress |> product (SelectEventAddress ->>- GameplayAddress) |> subscribe handleNewGame world |> snd
        let world = observe GameAddress ClickTitleLoadGameEventAddress |> product (SelectEventAddress ->>- GameplayAddress) |> subscribe handleLoadGame world |> snd
        World.subscribe4 GameAddress ClickTitleExitEventAddress World.handleAsExit world

    let private addCreditsScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile false DissolveData typeof<ScreenDispatcher>.Name CreditsAddress CreditsGroupFilePath world
        World.subscribe4 GameAddress ClickCreditsBackEventAddress (World.handleAsScreenTransition TitleAddress) world

    let private addGameplayScreen world =
        let world = snd <| World.addDissolveScreenFromGroupFile true DissolveData typeof<ScreenDispatcher>.Name GameplayAddress HudFilePath world
        let world = World.setGroup HudAddress (Group.setPersistent false <| World.getGroup HudAddress world) world
        let world = World.subscribe4 GameAddress ClickHudBackEventAddress (World.handleAsScreenTransition TitleAddress) world
        let world = World.subscribe4 GameAddress ClickHudSaveGameEventAddress handleClickSaveGame world
        World.subscribe4 GameAddress (DeselectEventAddress ->>- GameplayAddress) handleStopGameplay world

    let tryMakeInfinityRpgWorld sdlDeps userState =
        let componentFactory = InfinityRpgComponentFactory ()
        let optWorld = World.tryMake sdlDeps componentFactory GuiAndPhysicsAndGamePlay false userState
        match optWorld with
        | Right world ->
            let world = World.hintRenderPackageUse GuiPackageName world
            let world = addTitleScreen world
            let world = addCreditsScreen world
            let world = addGameplayScreen world
            let (splashScreen, world) = World.addSplashScreen false NuSplashData typeof<ScreenDispatcher>.Name NuSplashAddress TitleAddress world
            let world = snd <| World.selectScreen NuSplashAddress splashScreen world
            Right world
        | Left _ as left -> left