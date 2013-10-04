// WISDOM:
//
// A simulation that would put physics on another thread should likely do so in a different app
// domain with communication via .NET remoting to make 100% sure that no sharing is happening.
//
// The only reason I am using objects here is for processing speed.
//
// To avoid the vortex of suck that is much of OOP, I use only 'dumb' objects, where 'dumb' is
// defined as not knowing about the outside world. This includes no back references to parents, no
// references of external dependencies, and so on. So, if a method dispatch requires a dependency,
// the dependency must come in through the method's parameters.

// TODO: go through Prime and label / attribute all its types as was done here.

module Nu.Simulants
open System
open System.Collections.Generic
open OpenTK
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.Physics
open Nu.Audio
open Nu.Rendering

let getNuId = createGetNextId ()

type Entity (id) =
    let mutable _isEnabled = true
    let mutable _isVisible = true
    member this.Id = id : Id
    member this.IsEnabled with get () = _isEnabled and set value = _isEnabled <- value
    member this.IsVisible with get () = _isVisible and set value = _isVisible <- value

type Gui (id, position, size) =
    inherit Entity (id)
    let mutable _position = position : Vector2
    member this.Position with get () = _position and set value = _position <- value
    member this.Size = size : Vector2

type Button (id, position, size, upSprite, downSprite, clickSound) =
    inherit Gui (id, position, size)
    let mutable _isDown = false
    member this.IsDown with get () = _isDown and set value = _isDown <- value
    member this.UpSprite = upSprite : Sprite
    member this.DownSprite = downSprite : Sprite
    member this.clickSound = clickSound : Sound

type Label (id, position, size, sprite) =
    inherit Gui (id, position, size)
    member this.Sprite = sprite : Sprite

type Actor (id, position, size, rotation) =
    inherit Entity (id)
    let mutable _position = position : Vector2
    let mutable _rotation = rotation : single
    member this.Position with get () = _position and set value = _position <- value
    member this.Size = size : Vector2
    member this.Rotation with get () = _rotation and set value = _rotation <- value

type Block (id, position, size, rotation, physicsId, density, bodyType, sprite, contactSound) =
    inherit Actor (id, position, size, rotation)
    member this.PhysicsId = physicsId : Id
    member this.Density = density : single
    member this.BodyType = bodyType : BodyType
    member this.Sprite = sprite : Sprite
    member this.ContactSound = contactSound : Sound

type Group (id) =
    let mutable _isEnabled = true
    let mutable _isVisible = true
    let _entities = Dictionary<Lun, Entity> ()
    member this.Id = id : Id
    member this.IsEnabled with get () = _isEnabled and set value = _isEnabled <- value
    member this.IsVisible with get () = _isVisible and set value = _isVisible <- value
    member this.Entities = _entities
    
let getEntityFromGroupC address (group : Group) =
    match address with
    | [lun] -> group.Entities.[lun]
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getEntityFromGroup<'g when 'g :> Entity> address group =
    match address with
    | [lun] -> getEntityFromGroupC address group :?> 'g
    | _ -> failwith ("Invalid address '" + str address + "'.")

let tryGetEntityFromGroupC address (group : Group) =
    match address with
    | [lun] -> Dictionary.tryFind lun group.Entities
    | _ -> failwith ("Invalid address '" + str address + "'.")

let tryGetEntityFromGroup<'e when 'e :> Entity> address group =
    match address with
    | [lun] ->
        let optEntity = tryGetEntityFromGroupC address group
        match optEntity with
        | None -> None
        | Some entity -> match entity with :? 'e as entityAsE -> Some entityAsE | _ -> None
    | _ -> failwith ("Invalid address '" + str address + "'.")

let addEntityToGroup entity address (group : Group) =
    match address with
    | [lun] -> group.Entities.Add (lun, entity)
    | _ -> failwith ("Invalid address '" + str address + "'.")

let removeEntityFromGroup address (group : Group) =
    match address with
    | [lun] -> ignore (group.Entities.Remove lun)
    | _ -> failwith ("Invalid address '" + str address + "'.")

type Screen (id) =
    let mutable _isEnabled = true
    let mutable _isVisible = true
    let _groups = Dictionary<Lun, Group> ()
    member this.Id = id : Id
    member this.IsEnabled with get () = _isEnabled and set value = _isEnabled <- value
    member this.IsVisible with get () = _isVisible and set value = _isVisible <- value
    member this.Groups = _groups

let getGroupFromScreenC address (screen : Screen) =
    match address with
    | [lun] -> screen.Groups.[lun]
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getGroupFromScreen<'g when 'g :> Group> address screen =
    match address with
    | [lun] -> getGroupFromScreenC address screen :?> 'g
    | _ -> failwith ("Invalid address '" + str address + "'.")

let tryGetGroupFromScreenC address (screen : Screen) =
    match address with
    | [lun] -> Dictionary.tryFind lun screen.Groups
    | _ -> failwith ("Invalid address '" + str address + "'.")

let tryGetGroupFromScreen<'g when 'g :> Group> address screen =
    match address with
    | [lun] ->
        let optGroup = tryGetGroupFromScreenC address screen
        match optGroup with
        | None -> None
        | Some group -> match group with :? 'g as groupAsG -> Some groupAsG | _ -> None
    | _ -> failwith ("Invalid address '" + str address + "'.")

let addGroupToScreen group address (screen : Screen) =
    match address with
    | [lun] -> screen.Groups.Add (lun, group)
    | _ -> failwith ("Invalid address '" + str address + "'.")

let removeGroupFromScreen address (screen : Screen) =
    match address with
    | [lun] -> ignore (screen.Groups.Remove lun)
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getEntityFromScreenC address screen =
    let group = getGroupFromScreen [List.head address] screen
    getEntityFromGroup (List.tail address) group

let getEntityFromScreen<'e when 'e :> Entity> address screen =
    let group = getGroupFromScreen [List.head address] screen
    getEntityFromGroup<'e> (List.tail address) group

let tryGetEntityFromScreenC address screen =
    let optGroup = tryGetGroupFromScreen [List.head address] screen
    match optGroup with
    | None -> None
    | Some group -> tryGetEntityFromGroup (List.tail address) group

let tryGetEntityFromScreen<'e when 'e :> Entity> address screen =
    let optGroup = tryGetGroupFromScreen [List.head address] screen
    match optGroup with
    | None -> None
    | Some group -> tryGetEntityFromGroup<'e> (List.tail address) group

let addEntityToScreen entity address screen =
    let group = getGroupFromScreen [List.head address] screen
    addEntityToGroup entity (List.tail address) group

let removeEntityFromScreen address screen =
    let group = getGroupFromScreen [List.head address] screen
    removeEntityFromGroup (List.tail address) group

type Game (id) =
    let mutable _isEnabled = true
    let mutable _optActiveScreenAddress = None : Address option
    let _screens = Dictionary<Lun, Screen> ()
    member this.Id = id : Id
    member this.Screens = _screens
    member this.IsEnabled with get () = _isEnabled and set value = _isEnabled <- value
    member this.OptActiveScreenAddress with get () = _optActiveScreenAddress and set value = _optActiveScreenAddress <- value

let getScreenFromGameC address (game : Game) =
    match address with
    | [lun] -> game.Screens.[lun]
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getScreenFromGame<'s when 's :> Screen> address game =
    match address with
    | [lun] -> getScreenFromGameC address game :?> 's
    | _ -> failwith ("Invalid address '" + str address + "'.")

let tryGetScreenFromGameC address (game : Game) =
    match address with
    | [lun] -> Dictionary.tryFind lun game.Screens
    | _ -> failwith ("Invalid address '" + str address + "'.")

let tryGetScreenFromGame<'s when 's :> Screen> address game =
    match address with
    | [lun] ->
        let optScreen = tryGetScreenFromGameC address game
        match optScreen with
        | None -> None
        | Some screen -> match screen with :? 's as screenAsS -> Some screenAsS | _ -> None
    | _ -> failwith ("Invalid address '" + str address + "'.")

let addScreenToGame screen address (game : Game) =
    match address with
    | [lun] -> game.Screens.Add (lun, screen)
    | _ -> failwith ("Invalid address '" + str address + "'.")

let removeScreenFromGame address (game : Game) =
    match address with
    | [lun] -> ignore (game.Screens.Remove lun)
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getGroupFromGameC address game =
    let group = getScreenFromGame [List.head address] game
    getGroupFromScreen (List.tail address) group

let getGroupFromGame<'g when 'g :> Group> address game =
    let group = getScreenFromGameC [List.head address] game
    getGroupFromScreen<'g> (List.tail address) group

let tryGetGroupFromGameC address game =
    let optScreen = tryGetScreenFromGame [List.head address] game
    match optScreen with
    | None -> None
    | Some group -> tryGetGroupFromScreen (List.tail address) group

let tryGetGroupFromGame<'g when 'g :> Group> address game =
    let optScreen = tryGetScreenFromGameC [List.head address] game
    match optScreen with
    | None -> None
    | Some group -> tryGetGroupFromScreen<'g> (List.tail address) group

let addGroupToGame group address game =
    let screen = getScreenFromGame [List.head address] game
    addGroupToScreen group (List.tail address) screen

let removeGroupFromGame address game =
    let screen = getScreenFromGame [List.head address] game
    removeGroupFromScreen (List.tail address) screen

let getEntityFromGameC address game =
    let group = getScreenFromGame [List.head address] game
    getEntityFromScreen (List.tail address) group

let getEntityFromGame<'e when 'e :> Entity> address game =
    let group = getScreenFromGameC [List.head address] game
    getEntityFromScreen<'e> (List.tail address) group

let tryGetEntityFromGameC address game =
    let optScreen = tryGetScreenFromGame [List.head address] game
    match optScreen with
    | None -> None
    | Some group -> tryGetEntityFromScreen (List.tail address) group

let tryGetEntityFromGame<'e when 'e :> Entity> address game =
    let optScreen = tryGetScreenFromGameC [List.head address] game
    match optScreen with
    | None -> None
    | Some group -> tryGetEntityFromScreen<'e> (List.tail address) group

let addEntityToGame entity address game =
    let screen = getScreenFromGame [List.head address] game
    addEntityToScreen entity (List.tail address) screen

let removeEntityFromGame address game =
    let screen = getScreenFromGame [List.head address] game
    removeEntityFromScreen (List.tail address) screen