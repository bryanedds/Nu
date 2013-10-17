// WISDOM:
//
// A simulation that would put physics on another thread should likely do so in a different app
// domain with communication via .NET remoting to make 100% sure that no sharing is happening.
//
// NOTE: for simulation types, value semantics are preferred over open semantics as it eases
// serialization and other forms of automation. However, perhaps there is a way to get both...

// TODO: go through Prime and label / attribute all its types as was done here.

module Nu.Simulants
open System
open OpenTK
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.Physics
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

let getChildSem childFinder childToSem address parent =
    let child = getChild childFinder parent address
    let semantic = childToSem child
    (child, semantic)

let setChildSem childAdder childSemSetter address parent child semantic =
    let child2 = childSemSetter child semantic
    setChild childAdder parent address child2

let getChildSemSem childFinder childToSemSem address parent =
    let child = getChild childFinder parent address
    let (semantic, semantic2) = childToSemSem child
    (child, semantic, semantic2)

let setChildSemSem childAdder childSemSemSetter address parent child semantic semantic2 =
    let child2 = childSemSemSetter child semantic semantic2
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

let getOptChildSem optChildFinder childToOptSem parent address =
    let optChild = getOptChild optChildFinder parent address
    match optChild with
    | None -> None
    | Some child ->
        let optSem = childToOptSem child
        match optSem with
        | None -> None
        | Some semantic -> Some (child, semantic)

let setOptChildSem childAdder childRemover childSemSetter optChildSem parent address =
    match optChildSem with
    | None -> setOptChild childAdder childRemover parent address None
    | Some (child, semantic) -> setChildSem childAdder childSemSetter address parent child semantic
    
let getOptChildSemSem optChildFinder childToOptSemSem parent address =
    let optChild = getOptChild optChildFinder parent address
    match optChild with
    | None -> None
    | Some child ->
        let optSemSem = childToOptSemSem child
        match optSemSem with
        | None -> None
        | Some (semantic, semantic2) -> Some (child, semantic, semantic2)

let setOptChildSemSem childAdder childRemover childSemSemSetter optChildSemSem parent address =
    match optChildSemSem with
    | None -> setOptChild childAdder childRemover parent address None
    | Some (child, semantic, semantic2) -> setChildSemSem childAdder childSemSemSetter address parent child semantic semantic2

type [<StructuralEquality; NoComparison>] Button =
    { IsDown : bool
      UpSprite : Sprite
      DownSprite : Sprite
      ClickSound : Sound }

type [<StructuralEquality; NoComparison>] Label =
    { LabelSprite : Sprite }

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
      Depth : single
      Size : Vector2
      GuiSemantic : GuiSemantic }
    with
        static member button =
            { Get = fun this -> match this.GuiSemantic with Button button -> button | _ -> failwith "Gui is not a button."
              Set = fun button this -> { this with GuiSemantic = Button button }}

        static member optButton =
            { Get = fun this -> match this.GuiSemantic with Button button -> Some button | _ -> None
              Set = fun optButton this -> match optButton with None -> failwith "Cannot set semantic to None." | Some button -> { this with GuiSemantic = Button button }}
        
        static member label =
            { Get = fun this -> match this.GuiSemantic with Label label -> label | _ -> failwith "Gui is not a label."
              Set = fun label this -> { this with GuiSemantic = Label label }}

        static member optLabel =
            { Get = fun this -> match this.GuiSemantic with Label label -> Some label | _ -> None
              Set = fun optButton this -> match optButton with None -> failwith "Cannot set semantic to None." | Some label -> { this with GuiSemantic = Label label }}

type [<StructuralEquality; NoComparison>] Block =
    { PhysicsId : Id
      Density : single
      BodyType : BodyType
      Sprite : Sprite
      ContactSound : Sound }

/// An algabraically-closed semantics for game actors.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] ActorSemantic =
    | Block of Block
    | Avatar of unit
 // | ...additional actors
 // | UserDefinedActor of IUserDefinedActor (* this would be one way to get open actor semantics, but perhaps at the cost of its value semantics... *)

/// A game actor.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Actor =
    { Position : Vector2
      Depth : single
      Size : Vector2
      Rotation : single
      ActorSemantic : ActorSemantic }
    with
        static member block =
            { Get = fun this -> match this.ActorSemantic with Block block -> block | _ -> failwith "Actor is not a block."
              Set = fun block this -> { this with ActorSemantic = Block block }}

        static member optBlock =
            { Get = fun this -> match this.ActorSemantic with Block block -> Some block | _ -> None
              Set = fun optBlock this -> match optBlock with None -> failwith "Cannot set semantic to None." | Some block -> { this with ActorSemantic = Block block }}

/// An algabraically-closed semantics for game entities.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] EntitySemantic =
    | Gui of Gui
    | Actor of Actor
 // | Actor3d of Actor3d

/// A game entity.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Entity =
    { Id : Id
      IsEnabled : bool
      IsVisible : bool
      EntitySemantic : EntitySemantic }
    with
        static member gui =
            { Get = fun this -> match this.EntitySemantic with Gui gui -> gui | _ -> failwith "Entity is not a gui."
              Set = fun gui this -> { this with EntitySemantic = Gui gui }}
        
        static member optGui =
            { Get = fun this -> match this.EntitySemantic with Gui gui -> Some gui | _ -> None
              Set = fun optGui this -> match optGui with None -> failwith "Cannot set Entity.optGui to None." | Some gui -> set gui this Entity.gui }
        
        static member guiButton =
            { Get = fun this -> let gui = get this Entity.gui in (gui, get gui Gui.button)
              Set = fun (gui, button) this -> let newGui = set button Gui.button in set gui this Entity.gui }
        
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
            { Get = fun this -> let gui = get this Entity.gui in (gui, get gui Gui.label)
              Set = fun (gui, label) this -> let newGui = set label Gui.label in set gui this Entity.gui }
        
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
            { Get = fun this -> match this.EntitySemantic with Actor actor -> actor | _ -> failwith "Entity is not an actor."
              Set = fun actor this -> { this with EntitySemantic = Actor actor }}
        
        static member optActor =
            { Get = fun this -> match this.EntitySemantic with Actor actor -> Some actor | _ -> None
              Set = fun optActor this -> match optActor with None -> failwith "Cannot set Entity.optActor to None." | Some actor -> set actor this Entity.actor }
        
        static member actorBlock =
            { Get = fun this -> let actor = get this Entity.actor in (actor, get actor Actor.block)
              Set = fun (actor, block) this -> let newActor = set block Actor.block in set actor this Entity.actor }
        
        static member optActorBlock =
            { Get = fun this ->
                let optActor = get this Entity.optActor
                match optActor with
                | None -> None
                | Some actor ->
                    let optBlock = get actor Actor.optBlock
                    match optBlock with
                    | None -> None
                    | Some block -> Some (actor, block)
              Set = fun optActorBlock this ->
                match optActorBlock with
                | None -> failwith "Cannot set Entity.optActor to None."
                | Some actorBlock -> set actorBlock this Entity.actorBlock }

/// A game entity group.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Group =
    { Id : Id
      IsEnabled : bool
      IsVisible : bool
      Entities : Map<Lun, Entity> }
    with
        static member private optChildFinder addressHead parent =
            Map.tryFind addressHead parent.Entities
        
        static member private childFinder addressHead parent =
            let optChild = Group.optChildFinder addressHead parent
            match optChild with
            | None -> failwith ("Could not find child at address '" + str addressHead + "'.")
            | Some child -> child
        
        static member private childAdder addressHead parent child =
            { parent with Entities = Map.add addressHead child parent.Entities }
        
        static member private childRemover addressHead parent =
            { parent with Entities = Map.remove addressHead parent.Entities }
        
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
        
        static member private childActorBlockSetter child actor block =
            let actor2 = { actor with ActorSemantic = Block block }
            { child with EntitySemantic = Actor actor2 }
        
        static member private childToActorBlock child =
            match child.EntitySemantic with
            | Actor actor ->
                match actor.ActorSemantic with
                | Block block -> (actor, block)
                | _ -> failwith "Semantic of wrong type."
            | _ -> failwith "Semantic of wrong type."
        
        static member private childToOptActorBlock child =
            match child.EntitySemantic with
            | Actor actor ->
                match actor.ActorSemantic with
                | Block block -> Some (actor, block)
                | _ -> None
            | _ -> None
        
        static member entity address =
            { Get = fun this -> getChild Group.childFinder this address
              Set = fun entity this -> setChild Group.childAdder this address entity }
        
        static member optEntity address =
            { Get = fun this -> getOptChild Group.optChildFinder this address
              Set = fun optEntity this -> setOptChild Group.childAdder Group.childRemover this address optEntity }
        
        static member entityGui address =
            { Get = fun this -> getChildSem Group.childFinder Group.childToGui address this
              Set = fun (entity, gui) this -> setChildSem Group.childAdder Group.childGuiSetter address this entity gui }
        
        static member optEntityGui address =
            { Get = fun this -> getOptChildSem Group.optChildFinder Group.childToOptGui this address
              Set = fun optEntityGui this -> setOptChildSem Group.childAdder Group.childRemover Group.childGuiSetter optEntityGui this address }
        
        static member entityGuiButton address =
            { Get = fun this -> getChildSemSem Group.childFinder Group.childToGuiButton address this
              Set = fun (entity, gui, button) this -> setChildSemSem Group.childAdder Group.childGuiButtonSetter address this entity gui button }
        
        static member optEntityGuiButton address =
            { Get = fun this -> getOptChildSemSem Group.optChildFinder Group.childToOptGuiButton this address
              Set = fun optEntityGuiButton this -> setOptChildSemSem Group.childAdder Group.childRemover Group.childGuiButtonSetter optEntityGuiButton this address }
        
        static member entityGuiLabel address =
            { Get = fun this -> getChildSemSem Group.childFinder Group.childToGuiLabel address this
              Set = fun (entity, gui, label) this -> setChildSemSem Group.childAdder Group.childGuiLabelSetter address this entity gui label }
        
        static member optEntityGuiLabel address =
            { Get = fun this -> getOptChildSemSem Group.optChildFinder Group.childToOptGuiLabel this address
              Set = fun optEntityGuiLabel this -> setOptChildSemSem Group.childAdder Group.childRemover Group.childGuiLabelSetter optEntityGuiLabel this address }

        static member entityActor address =
            { Get = fun this -> getChildSem Group.childFinder Group.childToActor address this
              Set = fun (entity, actor) this -> setChildSem Group.childAdder Group.childActorSetter address this entity actor }
        
        static member optEntityActor address =
            { Get = fun this -> getOptChildSem Group.optChildFinder Group.childToOptActor this address
              Set = fun optEntityActor this -> setOptChildSem Group.childAdder Group.childRemover Group.childActorSetter optEntityActor this address }
        
        static member entityActorBlock address =
            { Get = fun this -> getChildSemSem Group.childFinder Group.childToActorBlock address this
              Set = fun (entity, actor, block) this -> setChildSemSem Group.childAdder Group.childActorBlockSetter address this entity actor block }
        
        static member optEntityActorBlock address =
            { Get = fun this -> getOptChildSemSem Group.optChildFinder Group.childToOptActorBlock this address
              Set = fun optEntityActorBlock this -> setOptChildSemSem Group.childAdder Group.childRemover Group.childActorBlockSetter optEntityActorBlock this address }
        
        static member entities =
            { Get = fun this -> this.Entities
              Set = fun entities this -> { this with Entities = entities }}

/// An algabraically-closed semantics for game screens.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] ScreenSemantic =
    | Title // of Title
    | Intro // of Intro
 // | ...additional screens
 // | UserDefinedScreen of IUserDefinedScreen (* this would give us open screen semantics, but perhaps at the cost of its value semantics...  *)
    
/// A game screen.
/// A serializable value type
type [<StructuralEquality; NoComparison>] Screen =
    { Id : Id
      IsEnabled : bool
      IsVisible : bool
      Groups : Map<Lun, Group>
      ScreenSemantic : ScreenSemantic }
    with
        static member private optChildFinder addressHead parent =
            Map.tryFind addressHead parent.Groups
        
        static member private childFinder addressHead parent =
            let optChild = Screen.optChildFinder addressHead parent
            match optChild with
            | None -> failwith ("Could not find child at address '" + str addressHead + "'.")
            | Some child -> child
        
        static member private childAdder addressHead parent child =
            { parent with Screen.Groups = Map.add addressHead child parent.Groups }
        
        static member private childRemover addressHead parent =
            { parent with Screen.Groups = Map.remove addressHead parent.Groups }
        
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
        
        static member entityActorBlock address =
            Screen.group [List.head address] >>| Group.entityActorBlock (List.tail address)
        
        static member optEntityActorBlock address =
            Screen.group [List.head address] >>| Group.optEntityActorBlock (List.tail address)
        
        static member group address =
            { Get = fun this -> getChild Screen.childFinder this address
              Set = fun group this -> setChild Screen.childAdder this address group }
        
        static member optGroup address =
            { Get = fun this -> getOptChild Screen.optChildFinder this address
              Set = fun optGroup this -> setOptChild Screen.childAdder Screen.childRemover this address optGroup }
        
        static member groups =
            { Get = fun this -> this.Groups
              Set = fun groups this -> { this with Groups = groups }}

/// A game.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Game =
    { Id : Id
      IsEnabled : bool
      Screens : Map<Lun, Screen>
      OptActiveScreenAddress : Address option }
    with
        static member private optChildFinder addressHead parent =
            Map.tryFind addressHead parent.Screens
        
        static member private childFinder addressHead parent =
            let optChild = Game.optChildFinder addressHead parent
            match optChild with
            | None -> failwith ("Could not find child at address '" + str addressHead + "'.")
            | Some child -> child
        
        static member private childAdder addressHead parent child =
            { parent with Game.Screens = Map.add addressHead child parent.Screens }
        
        static member private childRemover addressHead parent =
            { parent with Game.Screens = Map.remove addressHead parent.Screens }
        
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
        
        static member entityActorBlock (address : Address) =
            Game.screen [List.head address] >>| Screen.entityActorBlock (List.tail address)
        
        static member optEntityActorBlock (address : Address) =
            Game.screen [List.head address] >>| Screen.optEntityActorBlock (List.tail address)
        
        static member group (address : Address) =
            Game.screen [List.head address] >>| Screen.group (List.tail address)
        
        static member optGroup (address : Address) =
            Game.screen [List.head address] >>| Screen.optGroup (List.tail address)
        
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