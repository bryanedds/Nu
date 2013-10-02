// WISDOM:
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
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.Audio
open Nu.Rendering

let getNuId = createGetNextId ()
                        
let getChild childFinder parent (address : Address) =
    match address with
    | [head] -> childFinder head parent
    | _ -> failwith ("Invalid address '" + str address + "'.")

let setChild childAdder parent (address : Address) child =
    match address with
    | [head] -> childAdder head parent child
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getChildPlus childFinder childToPlus address parent =
    let child = getChild childFinder parent address
    let plus = childToPlus child
    (child, plus)

let setChildPlus childAdder childPlusSetter address parent child plus =
    let child2 = childPlusSetter child plus
    setChild childAdder parent address child2

let getChildPlusPlus childFinder childToPlusPlus address parent =
    let child = getChild childFinder parent address
    let (plus, plus2) = childToPlusPlus child
    (child, plus, plus2)

let setChildPlusPlus childAdder childPlusPlusSetter address parent child plus plus2 =
    let child2 = childPlusPlusSetter child plus plus2
    setChild childAdder parent address child2

let getOptChild optChildFinder parent (address : Address) =
    match address with
    | [] -> None
    | [head] ->
        let optChild = optChildFinder head parent
        match optChild with
        | None -> None
        | Some child -> Some child
    | _ :: _ -> None

let setOptChild addChild removeChild parent (address : Address) optChild =
    match address with
    | [head] ->
        match optChild with
        | None -> removeChild head parent
        | Some child -> addChild head parent child
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getOptChildPlus optChildFinder childToOptPlus parent address =
    let optChild = getOptChild optChildFinder parent address
    match optChild with
    | None -> None
    | Some child ->
        let optPlus = childToOptPlus child
        match optPlus with
        | None -> None
        | Some plus -> Some (child, plus)

let setOptChildPlus childAdder childRemover childPlusSetter optChildPlus parent address =
    match optChildPlus with
    | None -> setOptChild childAdder childRemover parent address None
    | Some (child, plus) -> setChildPlus childAdder childPlusSetter address parent child plus
    
let getOptChildPlusPlus optChildFinder childToOptPlusPlus parent address =
    let optChild = getOptChild optChildFinder parent address
    match optChild with
    | None -> None
    | Some child ->
        let optPlusPlus = childToOptPlusPlus child
        match optPlusPlus with
        | None -> None
        | Some (plus, plus2) -> Some (child, plus, plus2)

let setOptChildPlusPlus childAdder childRemover childPlusPlusSetter optChildPlusPlus parent address =
    match optChildPlusPlus with
    | None -> setOptChild childAdder childRemover parent address None
    | Some (child, plus, plus2) -> setChildPlusPlus childAdder childPlusPlusSetter address parent child plus plus2

type [<StructuralEquality; NoComparison>] Button =
    { IsDown : bool
      UpSprite : Sprite
      DownSprite : Sprite
      ClickSound : Sound }

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
    with
        static member button =
            { Get = fun this ->
                match this.GuiSemantic with
                | Button button -> button
                | _ -> failwith "Gui is not a button."
              Set = fun button this ->
                { this with GuiSemantic = Button button }}

        static member optButton =
            { Get = fun this ->
                match this.GuiSemantic with
                | Button button -> Some button
                | _ -> None
              Set = fun optButton this ->
                match optButton with
                | None -> failwith "Cannot set gui to None."
                | Some button -> { this with GuiSemantic = Button button }}
        
        static member label =
            { Get = fun this ->
                match this.GuiSemantic with
                | Label label -> label
                | _ -> failwith "Gui is not a label."
              Set = fun label this ->
                { this with GuiSemantic = Label label }}

        static member optLabel =
            { Get = fun this ->
                match this.GuiSemantic with
                | Label label -> Some label
                | _ -> None
              Set = fun optButton this ->
                match optButton with
                | None -> failwith "Cannot set semantic to None."
                | Some label -> { this with GuiSemantic = Label label }}

type [<StructuralEquality; NoComparison>] Crate =
    { Sprite : Sprite
      ContactSound : Sound }

/// An algabraically-closed semantics for game actors.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] ActorSemantic =
    | Crate of Crate
    | Avatar of unit
 // | ...additional actors
 // | UserDefinedActor of IUserDefinedActor (* this would be one way to get open actor semantics, but perhaps at the cost of its value semantics... *)

/// A game actor.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Actor =
    { Position : Vector2
      Size : Vector2
      Rotation : single
      ActorSemantic : ActorSemantic }
    with
        static member crate =
            { Get = fun this ->
                match this.ActorSemantic with
                | Crate crate -> crate
                | _ -> failwith "Actor is not a crate."
              Set = fun crate this ->
                { this with ActorSemantic = Crate crate }}

        static member optCrate =
            { Get = fun this ->
                match this.ActorSemantic with
                | Crate crate -> Some crate
                | _ -> None
              Set = fun optCrate this ->
                match optCrate with
                | None -> failwith "Cannot set semantic to None."
                | Some crate -> { this with ActorSemantic = Crate crate }}

/// An algabraically-closed semantics for game entities.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] EntitySemantic =
    | Gui of Gui
    | Actor of Actor
 // | Actor3d of Actor3d

/// A game entity.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Entity =
    { Id : Id // TODO: consider if these IDs are actually necessary
      IsEnabled : bool
      IsVisible : bool
      EntitySemantic : EntitySemantic }
    with
        static member gui =
            { Get = fun this ->
                match this.EntitySemantic with
                | Gui gui -> gui
                | _ -> failwith "Entity is not a gui."
              Set = fun gui this ->
                { this with EntitySemantic = Gui gui }}
        
        static member optGui =
            { Get = fun this ->
                match this.EntitySemantic with
                | Gui gui -> Some gui
                | _ -> None
              Set = fun optGui this ->
                match optGui with
                | None -> failwith "Cannot set Entity.optGui to None."
                | Some gui -> set gui this Entity.gui }
        
        static member guiButton =
            { Get = fun this ->
                let gui = get this Entity.gui
                (gui, get gui Gui.button)
              Set = fun (gui, button) this ->
                let newGui = set button Gui.button
                set gui this Entity.gui }
        
        static member optGuiButton =
            { Get = fun this ->
                let optGui = get this Entity.optGui
                match optGui with
                | None -> None
                | Some gui ->
                    let optButton = get gui Gui.optButton
                    match optButton with
                    | None -> None
                    | Some button -> Some (gui, button)
              Set = fun optGuiButton this ->
                match optGuiButton with
                | None -> failwith "Cannot set Entity.optGui to None."
                | Some guiButton -> set guiButton this Entity.guiButton }
        
        static member guiLabel =
            { Get = fun this ->
                let gui = get this Entity.gui
                (gui, get gui Gui.label)
              Set = fun (gui, label) this ->
                let newGui = set label Gui.label
                set gui this Entity.gui }
        
        static member optGuiLabel =
            { Get = fun this ->
                let optGui = get this Entity.optGui
                match optGui with
                | None -> None
                | Some gui ->
                    let optLabel = get gui Gui.optLabel
                    match optLabel with
                    | None -> None
                    | Some label -> Some (gui, label)
              Set = fun optGuiLabel this ->
                match optGuiLabel with
                | None -> failwith "Cannot set Entity.optGui to None."
                | Some guiLabel -> set guiLabel this Entity.guiLabel }

        static member actor =
            { Get = fun this ->
                match this.EntitySemantic with
                | Actor actor -> actor
                | _ -> failwith "Entity is not an actor."
              Set = fun actor this ->
                { this with EntitySemantic = Actor actor }}
        
        static member optActor =
            { Get = fun this ->
                match this.EntitySemantic with
                | Actor actor -> Some actor
                | _ -> None
              Set = fun optActor this ->
                match optActor with
                | None -> failwith "Cannot set Entity.optActor to None."
                | Some actor -> set actor this Entity.actor }
        
        static member actorCrate =
            { Get = fun this ->
                let actor = get this Entity.actor
                (actor, get actor Actor.crate)
              Set = fun (actor, crate) this ->
                let newActor = set crate Actor.crate
                set actor this Entity.actor }
        
        static member optActorCrate =
            { Get = fun this ->
                let optActor = get this Entity.optActor
                match optActor with
                | None -> None
                | Some actor ->
                    let optCrate = get actor Actor.optCrate
                    match optCrate with
                    | None -> None
                    | Some crate -> Some (actor, crate)
              Set = fun optActorCrate this ->
                match optActorCrate with
                | None -> failwith "Cannot set Entity.optActor to None."
                | Some actorCrate -> set actorCrate this Entity.actorCrate }

/// A game entity group.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Group =
    { Id : Id // TODO: consider if these IDs are actually necessary
      IsEnabled : bool
      IsVisible : bool
      Entities : Entity LunTrie }
    with
        static member private optChildFinder addressHead parent =
            LunTrie.tryFind addressHead parent.Entities
        
        static member private childFinder addressHead parent =
            let optChild = Group.optChildFinder addressHead parent
            match optChild with
            | None -> failwith ("Could not find child at address '" + str addressHead + "'.")
            | Some child -> child
        
        static member private childAdder addressHead parent child =
            { parent with Entities = LunTrie.add addressHead child parent.Entities }
        
        static member private childRemover addressHead parent =
            { parent with Entities = LunTrie.remove addressHead parent.Entities }
        
        static member private childGuiSetter child gui =
            { child with EntitySemantic = Gui gui }
        
        static member private childToGui child =
            match child.EntitySemantic with
            | Gui gui -> gui
            | _ -> failwith "Semantic of wrong type."
        
        static member private childToOptGui child =
            match child.EntitySemantic with
            | Gui gui -> Some gui
            | _ -> None
        
        static member private childGuiButtonSetter child gui button =
            let gui2 = { gui with GuiSemantic = Button button }
            { child with EntitySemantic = Gui gui2 }
        
        static member private childToGuiButton child =
            match child.EntitySemantic with
            | Gui gui ->
                match gui.GuiSemantic with
                | Button button -> (gui, button)
                | _ -> failwith "Semantic of wrong type."
            | _ -> failwith "Semantic of wrong type."
        
        static member private childToOptGuiButton child =
            match child.EntitySemantic with
            | Gui gui ->
                match gui.GuiSemantic with
                | Button button -> Some (gui, button)
                | _ -> None
            | _ -> None
        
        static member private childGuiLabelSetter child gui label =
            let gui2 = { gui with GuiSemantic = Label label }
            { child with EntitySemantic = Gui gui2 }
        
        static member private childToGuiLabel child =
            match child.EntitySemantic with
            | Gui gui ->
                match gui.GuiSemantic with
                | Label label -> (gui, label)
                | _ -> failwith "Semantic of wrong type."
            | _ -> failwith "Semantic of wrong type."
        
        static member private childToOptGuiLabel child =
            match child.EntitySemantic with
            | Gui gui ->
                match gui.GuiSemantic with
                | Label label -> Some (gui, label)
                | _ -> None
            | _ -> None
        
        static member private childActorSetter child actor =
            { child with EntitySemantic = Actor actor }
        
        static member private childToActor child =
            match child.EntitySemantic with
            | Actor actor -> actor
            | _ -> failwith "Semantic of wrong type."
        
        static member private childToOptActor child =
            match child.EntitySemantic with
            | Actor actor -> Some actor
            | _ -> None
        
        static member private childActorCrateSetter child actor crate =
            let actor2 = { actor with ActorSemantic = Crate crate }
            { child with EntitySemantic = Actor actor2 }
        
        static member private childToActorCrate child =
            match child.EntitySemantic with
            | Actor actor ->
                match actor.ActorSemantic with
                | Crate crate -> (actor, crate)
                | _ -> failwith "Semantic of wrong type."
            | _ -> failwith "Semantic of wrong type."
        
        static member private childToOptActorCrate child =
            match child.EntitySemantic with
            | Actor actor ->
                match actor.ActorSemantic with
                | Crate crate -> Some (actor, crate)
                | _ -> None
            | _ -> None
        
        static member entity address =
            { Get = fun this -> getChild Group.childFinder this address
              Set = fun entity this -> setChild Group.childAdder this address entity }
        
        static member optEntity address =
            { Get = fun this -> getOptChild Group.optChildFinder this address
              Set = fun optEntity this -> setOptChild Group.childAdder Group.childRemover this address optEntity }
        
        static member entityGui address =
            { Get = fun this -> getChildPlus Group.childFinder Group.childToGui address this
              Set = fun (entity, gui) this -> setChildPlus Group.childAdder Group.childGuiSetter address this entity gui }
        
        static member optEntityGui address =
            { Get = fun this -> getOptChildPlus Group.optChildFinder Group.childToOptGui this address
              Set = fun optEntityGui this -> setOptChildPlus Group.childAdder Group.childRemover Group.childGuiSetter optEntityGui this address }
        
        static member entityGuiButton address =
            { Get = fun this -> getChildPlusPlus Group.childFinder Group.childToGuiButton address this
              Set = fun (entity, gui, button) this -> setChildPlusPlus Group.childAdder Group.childGuiButtonSetter address this entity gui button }
        
        static member optEntityGuiButton address =
            { Get = fun this -> getOptChildPlusPlus Group.optChildFinder Group.childToOptGuiButton this address
              Set = fun optEntityGuiButton this -> setOptChildPlusPlus Group.childAdder Group.childRemover Group.childGuiButtonSetter optEntityGuiButton this address }
        
        static member entityGuiLabel address =
            { Get = fun this -> getChildPlusPlus Group.childFinder Group.childToGuiLabel address this
              Set = fun (entity, gui, label) this -> setChildPlusPlus Group.childAdder Group.childGuiLabelSetter address this entity gui label }
        
        static member optEntityGuiLabel address =
            { Get = fun this -> getOptChildPlusPlus Group.optChildFinder Group.childToOptGuiLabel this address
              Set = fun optEntityGuiLabel this -> setOptChildPlusPlus Group.childAdder Group.childRemover Group.childGuiLabelSetter optEntityGuiLabel this address }

        static member entityActor address =
            { Get = fun this -> getChildPlus Group.childFinder Group.childToActor address this
              Set = fun (entity, actor) this -> setChildPlus Group.childAdder Group.childActorSetter address this entity actor }
        
        static member optEntityActor address =
            { Get = fun this -> getOptChildPlus Group.optChildFinder Group.childToOptActor this address
              Set = fun optEntityActor this -> setOptChildPlus Group.childAdder Group.childRemover Group.childActorSetter optEntityActor this address }
        
        static member entityActorCrate address =
            { Get = fun this -> getChildPlusPlus Group.childFinder Group.childToActorCrate address this
              Set = fun (entity, actor, crate) this -> setChildPlusPlus Group.childAdder Group.childActorCrateSetter address this entity actor crate }
        
        static member optEntityActorCrate address =
            { Get = fun this -> getOptChildPlusPlus Group.optChildFinder Group.childToOptActorCrate this address
              Set = fun optEntityActorCrate this -> setOptChildPlusPlus Group.childAdder Group.childRemover Group.childActorCrateSetter optEntityActorCrate this address }
        
        static member entities =
            { Get = fun this -> this.Entities
              Set = fun entities this -> { this with Entities = entities }}

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
    { Id : Id // TODO: consider if these IDs are actually necessary
      IsEnabled : bool
      IsVisible : bool
      Groups : Group LunTrie
      ScreenSemantic : ScreenSemantic }
    with
        static member private optChildFinder addressHead parent =
            LunTrie.tryFind addressHead parent.Groups
        
        static member private childFinder addressHead parent =
            let optChild = Screen.optChildFinder addressHead parent
            match optChild with
            | None -> failwith ("Could not find child at address '" + str addressHead + "'.")
            | Some child -> child
        
        static member private childAdder addressHead parent child =
            { parent with Screen.Groups = LunTrie.add addressHead child parent.Groups }
        
        static member private childRemover addressHead parent =
            { parent with Screen.Groups = LunTrie.remove addressHead parent.Groups }
        
        static member group address =
            { Get = fun this -> getChild Screen.childFinder this address
              Set = fun group this -> setChild Screen.childAdder this address group }
        
        static member optGroup address =
            { Get = fun this -> getOptChild Screen.optChildFinder this address
              Set = fun optGroup this -> setOptChild Screen.childAdder Screen.childRemover this address optGroup }
        
        static member groups =
            { Get = fun this -> this.Groups
              Set = fun groups this -> { this with Groups = groups }}
        
        static member entity address =
            Screen.group [List.head address] >>| Group.entity (List.tail address)
        
        static member optEntity address =
            Screen.group [List.head address] >>| Group.optEntity (List.tail address)
        
        static member entityGui address =
            Screen.group [List.head address] >>| Group.entityGui (List.tail address)
        
        static member optEntityGui address =
            Screen.group [List.head address] >>| Group.optEntityGui (List.tail address)
        
        static member entityGuiButton address =
            Screen.group [List.head address] >>| Group.entityGuiButton (List.tail address)
        
        static member optEntityGuiButton address =
            Screen.group [List.head address] >>| Group.optEntityGuiButton (List.tail address)
        
        static member entityGuiLabel address =
            Screen.group [List.head address] >>| Group.entityGuiLabel (List.tail address)
        
        static member optEntityGuiLabel address =
            Screen.group [List.head address] >>| Group.optEntityGuiLabel (List.tail address)
        
        static member entityActor address =
            Screen.group [List.head address] >>| Group.entityActor (List.tail address)
        
        static member optEntityActor address =
            Screen.group [List.head address] >>| Group.optEntityActor (List.tail address)
        
        static member entityActorCrate address =
            Screen.group [List.head address] >>| Group.entityActorCrate (List.tail address)
        
        static member optEntityActorCrate address =
            Screen.group [List.head address] >>| Group.optEntityActorCrate (List.tail address)

/// A game.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Game =
    { Id : Id // TODO: consider if these IDs are actually necessary
      IsEnabled : bool
      Screens : Screen LunTrie
      OptActiveScreenAddress : Address option }
    with
        static member private optChildFinder addressHead parent =
            LunTrie.tryFind addressHead parent.Screens
        
        static member private childFinder addressHead parent =
            let optChild = Game.optChildFinder addressHead parent
            match optChild with
            | None -> failwith ("Could not find child at address '" + str addressHead + "'.")
            | Some child -> child
        
        static member private childAdder addressHead parent child =
            { parent with Game.Screens = LunTrie.add addressHead child parent.Screens }
        
        static member private childRemover addressHead parent =
            { parent with Game.Screens = LunTrie.remove addressHead parent.Screens }
        
        static member screen address =
            { Get = fun this -> getChild Game.childFinder this address
              Set = fun screen this -> setChild Game.childAdder this address screen }
        
        static member optScreen address =
            { Get = fun this -> getOptChild Game.optChildFinder this address
              Set = fun optScreen this -> setOptChild Game.childAdder Game.childRemover this address optScreen }
        
        static member screens =
            { Get = fun this -> this.Screens
              Set = fun screens this -> { this with Screens = screens }}
        
        static member optActiveScreenAddress =
            { Get = fun this -> this.OptActiveScreenAddress
              Set = fun optActiveScreenAddress this -> { this with OptActiveScreenAddress = optActiveScreenAddress }}
        
        static member group (address : Address) =
            Game.screen [List.head address] >>| Screen.group (List.tail address)
        
        static member entity (address : Address) =
            Game.screen [List.head address] >>| Screen.entity (List.tail address)
        
        static member optEntity (address : Address) =
            Game.screen [List.head address] >>| Screen.optEntity (List.tail address)
        
        static member entityGui (address : Address) =
            Game.screen [List.head address] >>| Screen.entityGui (List.tail address)
        
        static member optEntityGui (address : Address) =
            Game.screen [List.head address] >>| Screen.optEntityGui (List.tail address)
        
        static member entityGuiButton (address : Address) =
            Game.screen [List.head address] >>| Screen.entityGuiButton (List.tail address)
        
        static member optEntityGuiButton (address : Address) =
            Game.screen [List.head address] >>| Screen.optEntityGuiButton (List.tail address)
        
        static member entityGuiLabel (address : Address) =
            Game.screen [List.head address] >>| Screen.entityGuiLabel (List.tail address)
        
        static member optEntityGuiLabel (address : Address) =
            Game.screen [List.head address] >>| Screen.optEntityGuiLabel (List.tail address)
        
        static member entityActor (address : Address) =
            Game.screen [List.head address] >>| Screen.entityActor (List.tail address)
        
        static member optEntityActor (address : Address) =
            Game.screen [List.head address] >>| Screen.optEntityActor (List.tail address)
        
        static member entityActorCrate (address : Address) =
            Game.screen [List.head address] >>| Screen.entityActorCrate (List.tail address)
        
        static member optEntityActorCrate (address : Address) =
            Game.screen [List.head address] >>| Screen.optEntityActorCrate (List.tail address)