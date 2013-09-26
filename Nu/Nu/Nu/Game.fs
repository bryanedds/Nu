// NaiveSimulation - a technical prototype for Nu, the Flipped prototype engine.
//
// A naive, closed simulation implementation in F# that uses semantics and identity.
//
// A simulation that would put physics on another thread should likely do so in a different app
// domain with communication via .NET remoting to make 100% sure that no sharing is happening.
//
// NOTE: for simulation types, value semantics are preferred over open semantics as it eases
// serialization and other forms of automation. However, perhaps there is a way to get both...

// TODO: go through Prime and label / attribute all its types as was done here.

module Nu.Game
open System
open OpenTK
open Nu.Core
open Nu.Audio
open Nu.Rendering

let getNuId = createGetNextId ()

/// Specifies the address of an element in a game.
/// Note that subscribing to a partial address results in listening to all messages whose
/// beginning address nodes match the partial address (sort of a wild-card).
/// A value type.
type Address = Lun list

type [<StructuralEquality; NoComparison>] Button =
    { IsDown : bool
      UpSprite : Sprite
      DownSprite : Sprite
      ClickSound : SoundMessage }
      
type [<StructuralEquality; NoComparison>] Label =
    { Sprite : Sprite }

/// An algabraically-closed semantics for game gui elements.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] GuiSemantic =
    | Button of Button
    | Label of Label
 // | ...additional controls
 // | UserDefinedGui of IUserDefinedGui (* this would give us open gui semantics, but perhaps at the cost of its value semantics...  *)

/// A game gui element.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Gui =
    { Position : Vector2
      Size : Vector2
      GuiSemantic : GuiSemantic }

/// An algabraically-closed semantics for game actors.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] ActorSemantic =
    | Avatar // of Avatar
    | Item // of Item
 // | ...additional actors
 // | UserDefinedActor of IUserDefinedActor (* this would be one way to get open actor semantics, but perhaps at the cost of its value semantics... *)

/// A game actor.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Actor =
    { Position : Vector2
      Size : Vector2
      ActorSemantic : ActorSemantic }

/// An algabraically-closed semantics for game entities.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] EntitySemantic =
    | Gui of Gui
    | Actor of Actor
 // | Actor2D of Actor2D

/// A game entity.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Entity =
    { ID : ID
      Enabled : bool
      Visible : bool
      EntitySemantic : EntitySemantic }

/// A game entity group.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Group =
    { ID : ID
      Enabled : bool
      Visible : bool
      Entities : Entity LunTrie }

type [<StructuralEquality; NoComparison>] TestScreen =
    { Unused : unit }

/// An algabraically-closed semantics for game screens.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] ScreenSemantic =
    | TestScreen of TestScreen
    | Title // of Title
    | Intro // of Intro
 // | ...additional screens
 // | UserDefinedScreen of IUserDefinedScreen (* this would give us open screen semantics, but perhaps at the cost of its value semantics...  *)
    
/// A game screen.
/// A serializable value type
type [<StructuralEquality; NoComparison>] Screen =
    { ID : ID
      Enabled : bool
      Visible : bool
      Groups : Group LunTrie
      ScreenSemantic : ScreenSemantic }

/// A game.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Game =
    { ID : ID
      Enabled : bool
      Screens : Screen LunTrie
      OptActiveScreen : Address option }

type [<StructuralEquality; NoComparison>] FindResult =
    { OptEntity : Entity option
      OptGroup : Group option
      OptScreen : Screen option
      Game : Game }

let requireScreen errorFn address findResult =
    match findResult.OptScreen with
    | None -> errorFn  ("No screen at address '" + str address + "'.")
    | Some _ -> findResult

let requireGroup errorFn address findResult =
    match findResult.OptGroup with
    | None -> errorFn ("No group at address '" + str address + "'.")
    | Some _ -> requireScreen errorFn address findResult

let requireEntity errorFn address findResult =
    match findResult.OptEntity with
    | None -> errorFn ("No entity at address '" + str address + "'.")
    | Some _ -> requireGroup errorFn address findResult

let findInGame4 errorFn requirement address game =
    match address with
    | [] -> errorFn ("Invalid address '" + str address + "'.")
    | head :: tail ->
        let optScreen = LunTrie.tryFind head game.Screens
        match optScreen with
        | None -> errorFn ("No screen at address '" + str address + "'.")
        | Some screen ->
            match tail with
            | [] -> requirement errorFn address { OptEntity = None; OptGroup = None; OptScreen = Some screen; Game = game }
            | head :: tail ->
                let optGroup = LunTrie.tryFind head screen.Groups
                match optGroup with
                | None -> errorFn ("No group at address '" + str address + "'.")
                | Some group ->
                    match tail with
                    | [] -> requirement errorFn address { OptEntity = None; OptGroup = Some group; OptScreen = Some screen; Game = game }
                    | head :: tail ->
                        let optEntity = LunTrie.tryFind head group.Entities
                        match optEntity with
                        | None -> errorFn ("No entity at address '" + str address + "'.")
                        | Some entity -> requirement errorFn address { OptEntity = Some entity; OptGroup = Some group; OptScreen = Some screen; Game = game }
                
/// Find an element at the given address, failing with exception otherwise.
let inline findInGame requirement address game =
    findInGame4 failwith requirement address game

/// Try to find an element at the given address.
let inline tryFindInGame address game =
    findInGame4
        (fun _ -> { OptEntity = None; OptGroup = None; OptScreen = None; Game = game })
        (fun _ _ findResult -> findResult) address game

let setInGame address (element : obj) game : Game =
    match address with
    | [] -> failwith ("Invalid address '" + str address + "'.")
    | [head] ->
        match element with
        | :? Screen as screen -> { game with Screens = LunTrie.add head screen game.Screens }
        | _ -> failwith ("Invalid address for screen '" + str address + "'.")
    | head :: tail ->
        let optScreen = LunTrie.tryFind head game.Screens
        match optScreen with
        | None -> failwith ("No screen at address '" + str address + "'.")
        | Some screen ->
            match tail with
            | [] -> failwith ("Invalid address '" + str address + "'.")
            | [head'] ->
                match element with
                | :? Group as group ->
                    let newScreen = { screen with Groups = LunTrie.add head' group screen.Groups }
                    { game with Screens = LunTrie.add head newScreen game.Screens }
                | _ -> failwith ("Invalid address for group '" + str address + "'.")
            | head' :: tail' ->
                let optGroup = LunTrie.tryFind head' screen.Groups
                match optGroup with
                | None -> failwith ("No group at address '" + str address + "'.")
                | Some group ->
                    match tail' with
                    | [] -> failwith ("Invalid address '" + str address + "'.")
                    | [head''] ->
                        match element with
                        | :? Entity as entity ->
                            let newGroup = { group with Entities = LunTrie.add head'' entity group.Entities }
                            let newScreen = { screen with Groups = LunTrie.add head' newGroup screen.Groups }
                            { game with Screens = LunTrie.add head newScreen game.Screens }
                        | _ -> failwith ("Invalid address for group '" + str address + "'.")
                    | head'' :: tail'' -> failwith ("Invalid address '" + str address + "'.")

let removeInGame address game : Game =
    match address with
    | [] -> failwith ("Invalid address '" + str address + "'.")
    | [head] -> { game with Screens = LunTrie.remove head game.Screens }
    | head :: tail ->
        let optScreen = LunTrie.tryFind head game.Screens
        match optScreen with
        | None -> failwith ("No screen at address '" + str address + "'.")
        | Some screen ->
            match tail with
            | [] -> failwith ("Invalid address '" + str address + "'.")
            | [head'] ->
                let newScreen = { screen with Groups = LunTrie.remove head' screen.Groups }
                { game with Screens = LunTrie.add head newScreen game.Screens }
            | head' :: tail' ->
                let optGroup = LunTrie.tryFind head' screen.Groups
                match optGroup with
                | None -> failwith ("No group at address '" + str address + "'.")
                | Some group ->
                    match tail' with
                    | [] -> failwith ("Invalid address '" + str address + "'.")
                    | [head''] ->
                        let newGroup = { group with Entities = LunTrie.remove head'' group.Entities }
                        let newScreen = { screen with Groups = LunTrie.add head' newGroup screen.Groups }
                        { game with Screens = LunTrie.add head newScreen game.Screens }
                    | head'' :: tail'' -> failwith ("Invalid address '" + str address + "'.")