module Nu.Entity
open System
open FSharpx
open FSharpx.Lens.Operators
open OpenTK
open Nu.Core
open Nu.Physics
open Nu.Audio
open Nu.Rendering
                        
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
 // | UserDefinedGui of IUserDefinedGui (* this would give us more open gui semantics, but perhaps at the cost of its value semantics...  *)

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