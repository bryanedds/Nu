module Nu.Group
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DataModel
open Nu.Entity

/// A game entity group.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Group =
    { Id : Id
      Entities : Map<Lun, Entity>
      Subtype : unit }
       
    static member private optChildFinder addressHead parent =
        Map.tryFind addressHead parent.Entities
    
    static member private childFinder addressHead parent =
        let optChild = Group.optChildFinder addressHead parent
        match optChild with
        | None -> failwith ("Could not find child at address '" + str addressHead + "'.")
        | Some child -> child
    
    static member private childAdder addressHead parent (child : Entity) =
        { parent with Entities = Map.add addressHead child parent.Entities }
    
    static member private childRemover addressHead parent =
        { parent with Entities = Map.remove addressHead parent.Entities }
    
    static member private childGuiSetter (child : Entity) gui =
        { child with Subtype = Gui gui }
    
    static member private childToGui (child : Entity) =
        match child.Subtype with
        | Gui gui -> gui
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptGui (child : Entity) =
        match child.Subtype with
        | Gui gui -> Some gui
        | _ -> None
    
    static member private childGuiButtonSetter (child : Entity) gui button =
        let gui2 = { gui with Gui.SubSubtype = Button button }
        { child with Subtype = Gui gui2 }
    
    static member private childToGuiButton (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | Button button -> (gui, button)
            | _ -> failwith "Sub type of wrong type."
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptGuiButton (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | Button button -> Some (gui, button)
            | _ -> None
        | _ -> None
    
    static member private childGuiLabelSetter (child : Entity) gui label =
        let gui2 = { gui with Gui.SubSubtype = Label label }
        { child with Subtype = Gui gui2 }
    
    static member private childToGuiLabel (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | Label label -> (gui, label)
            | _ -> failwith "Sub type of wrong type."
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptGuiLabel (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | Label label -> Some (gui, label)
            | _ -> None
        | _ -> None
    
    static member private childGuiTextBoxSetter (child : Entity) gui textBox =
        let gui2 = { gui with Gui.SubSubtype = TextBox textBox }
        { child with Subtype = Gui gui2 }
    
    static member private childToGuiTextBox (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | TextBox textBox -> (gui, textBox)
            | _ -> failwith "Sub type of wrong type."
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptGuiTextBox (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | TextBox textBox -> Some (gui, textBox)
            | _ -> None
        | _ -> None
    
    static member private childGuiToggleSetter (child : Entity) gui toggle =
        let gui2 = { gui with Gui.SubSubtype = Toggle toggle }
        { child with Subtype = Gui gui2 }
        
    static member private childToGuiToggle (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | Toggle toggle -> (gui, toggle)
            | _ -> failwith "Sub type of wrong type."
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptGuiToggle (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | Toggle toggle -> Some (gui, toggle)
            | _ -> None
        | _ -> None
    
    static member private childGuiFeelerSetter (child : Entity) gui feeler =
        let gui2 = { gui with Gui.SubSubtype = Feeler feeler }
        { child with Subtype = Gui gui2 }
    
    static member private childToGuiFeeler (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | Feeler feeler -> (gui, feeler)
            | _ -> failwith "Sub type of wrong type."
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptGuiFeeler (child : Entity) =
        match child.Subtype with
        | Gui gui ->
            match gui.SubSubtype with
            | Feeler feeler -> Some (gui, feeler)
            | _ -> None
        | _ -> None
    
    static member private childActorSetter (child : Entity) actor =
        { child with Subtype = Actor actor }
    
    static member private childToActor (child : Entity) =
        match child.Subtype with
        | Actor actor -> actor
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptActor (child : Entity) =
        match child.Subtype with
        | Actor actor -> Some actor
        | _ -> None
    
    static member private childActorBlockSetter (child : Entity) actor block =
        let actor2 = { actor with Actor.SubSubtype = Block block }
        { child with Subtype = Actor actor2 }
    
    static member private childToActorBlock (child : Entity) =
        match child.Subtype with
        | Actor actor ->
            match actor.SubSubtype with
            | Block block -> (actor, block)
            | _ -> failwith "Sub type of wrong type."
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptActorBlock (child : Entity) =
        match child.Subtype with
        | Actor actor ->
            match actor.SubSubtype with
            | Block block -> Some (actor, block)
            | _ -> None
        | _ -> None
    
    static member private childActorAvatarSetter (child : Entity) actor avatar =
        let actor2 = { actor with Actor.SubSubtype = Avatar avatar }
        { child with Subtype = Actor actor2 }
    
    static member private childToActorAvatar (child : Entity) =
        match child.Subtype with
        | Actor actor ->
            match actor.SubSubtype with
            | Avatar avatar -> (actor, avatar)
            | _ -> failwith "Sub type of wrong type."
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptActorAvatar (child : Entity) =
        match child.Subtype with
        | Actor actor ->
            match actor.SubSubtype with
            | Avatar avatar -> Some (actor, avatar)
            | _ -> None
        | _ -> None
    
    static member private childActorTileMapSetter (child : Entity) actor tileMap =
        let actor2 = { actor with Actor.SubSubtype = TileMap tileMap }
        { child with Subtype = Actor actor2 }
    
    static member private childToActorTileMap (child : Entity) =
        match child.Subtype with
        | Actor actor ->
            match actor.SubSubtype with
            | TileMap tileMap -> (actor, tileMap)
            | _ -> failwith "Sub type of wrong type."
        | _ -> failwith "Sub type of wrong type."
    
    static member private childToOptActorTileMap (child : Entity) =
        match child.Subtype with
        | Actor actor ->
            match actor.SubSubtype with
            | TileMap tileMap -> Some (actor, tileMap)
            | _ -> None
        | _ -> None
    
    static member entity address =
        { Get = fun this -> getChild Group.childFinder this address
          Set = fun entity this -> setChild Group.childAdder this address entity }
    
    static member optEntity address =
        { Get = fun this -> getOptChild Group.optChildFinder this address
          Set = fun optEntity this -> setOptChild Group.childAdder Group.childRemover this address optEntity }
    
    static member entityGui address =
        { Get = fun this -> getChildSubtype Group.childFinder Group.childToGui address this
          Set = fun (entity, gui) this -> setChildSubtype Group.childAdder Group.childGuiSetter address this entity gui }
    
    static member optEntityGui address =
        { Get = fun this -> getOptChildSubtype Group.optChildFinder Group.childToOptGui this address
          Set = fun optEntityGui this -> setOptChildSubtype Group.childAdder Group.childRemover Group.childGuiSetter optEntityGui this address }
    
    static member entityGuiButton address =
        { Get = fun this -> getChildSubSubtype Group.childFinder Group.childToGuiButton address this
          Set = fun (entity, gui, button) this -> setChildSubSubtype Group.childAdder Group.childGuiButtonSetter address this entity gui button }
    
    static member optEntityGuiButton address =
        { Get = fun this -> getOptChildSubSubtype Group.optChildFinder Group.childToOptGuiButton this address
          Set = fun optEntityGuiButton this -> setOptChildSubSubtype Group.childAdder Group.childRemover Group.childGuiButtonSetter optEntityGuiButton this address }
    
    static member entityGuiLabel address =
        { Get = fun this -> getChildSubSubtype Group.childFinder Group.childToGuiLabel address this
          Set = fun (entity, gui, label) this -> setChildSubSubtype Group.childAdder Group.childGuiLabelSetter address this entity gui label }
    
    static member optEntityGuiLabel address =
        { Get = fun this -> getOptChildSubSubtype Group.optChildFinder Group.childToOptGuiLabel this address
          Set = fun optEntityGuiLabel this -> setOptChildSubSubtype Group.childAdder Group.childRemover Group.childGuiLabelSetter optEntityGuiLabel this address }
    
    static member entityGuiTextBox address =
        { Get = fun this -> getChildSubSubtype Group.childFinder Group.childToGuiTextBox address this
          Set = fun (entity, gui, textBox) this -> setChildSubSubtype Group.childAdder Group.childGuiTextBoxSetter address this entity gui textBox }
    
    static member optEntityGuiTextBox address =
        { Get = fun this -> getOptChildSubSubtype Group.optChildFinder Group.childToOptGuiTextBox this address
          Set = fun optEntityGuiTextBox this -> setOptChildSubSubtype Group.childAdder Group.childRemover Group.childGuiTextBoxSetter optEntityGuiTextBox this address }
    
    static member entityGuiToggle address =
        { Get = fun this -> getChildSubSubtype Group.childFinder Group.childToGuiToggle address this
          Set = fun (entity, gui, toggle) this -> setChildSubSubtype Group.childAdder Group.childGuiToggleSetter address this entity gui toggle }
    
    static member optEntityGuiToggle address =
        { Get = fun this -> getOptChildSubSubtype Group.optChildFinder Group.childToOptGuiToggle this address
          Set = fun optEntityGuiToggle this -> setOptChildSubSubtype Group.childAdder Group.childRemover Group.childGuiToggleSetter optEntityGuiToggle this address }
    
    static member entityGuiFeeler address =
        { Get = fun this -> getChildSubSubtype Group.childFinder Group.childToGuiFeeler address this
          Set = fun (entity, gui, feeler) this -> setChildSubSubtype Group.childAdder Group.childGuiFeelerSetter address this entity gui feeler }
    
    static member optEntityGuiFeeler address =
        { Get = fun this -> getOptChildSubSubtype Group.optChildFinder Group.childToOptGuiFeeler this address
          Set = fun optEntityGuiFeeler this -> setOptChildSubSubtype Group.childAdder Group.childRemover Group.childGuiFeelerSetter optEntityGuiFeeler this address }

    static member entityActor address =
        { Get = fun this -> getChildSubtype Group.childFinder Group.childToActor address this
          Set = fun (entity, actor) this -> setChildSubtype Group.childAdder Group.childActorSetter address this entity actor }
    
    static member optEntityActor address =
        { Get = fun this -> getOptChildSubtype Group.optChildFinder Group.childToOptActor this address
          Set = fun optEntityActor this -> setOptChildSubtype Group.childAdder Group.childRemover Group.childActorSetter optEntityActor this address }
    
    static member entityActorBlock address =
        { Get = fun this -> getChildSubSubtype Group.childFinder Group.childToActorBlock address this
          Set = fun (entity, actor, block) this -> setChildSubSubtype Group.childAdder Group.childActorBlockSetter address this entity actor block }
    
    static member optEntityActorBlock address =
        { Get = fun this -> getOptChildSubSubtype Group.optChildFinder Group.childToOptActorBlock this address
          Set = fun optEntityActorBlock this -> setOptChildSubSubtype Group.childAdder Group.childRemover Group.childActorBlockSetter optEntityActorBlock this address }
    
    static member entityActorAvatar address =
        { Get = fun this -> getChildSubSubtype Group.childFinder Group.childToActorAvatar address this
          Set = fun (entity, actor, avatar) this -> setChildSubSubtype Group.childAdder Group.childActorAvatarSetter address this entity actor avatar }
    
    static member optEntityActorAvatar address =
        { Get = fun this -> getOptChildSubSubtype Group.optChildFinder Group.childToOptActorAvatar this address
          Set = fun optEntityActorAvatar this -> setOptChildSubSubtype Group.childAdder Group.childRemover Group.childActorAvatarSetter optEntityActorAvatar this address }
    
    static member entityActorTileMap address =
        { Get = fun this -> getChildSubSubtype Group.childFinder Group.childToActorTileMap address this
          Set = fun (entity, actor, tileMap) this -> setChildSubSubtype Group.childAdder Group.childActorTileMapSetter address this entity actor tileMap }
    
    static member optEntityActorTileMap address =
        { Get = fun this -> getOptChildSubSubtype Group.optChildFinder Group.childToOptActorTileMap this address
          Set = fun optEntityActorTileMap this -> setOptChildSubSubtype Group.childAdder Group.childRemover Group.childActorTileMapSetter optEntityActorTileMap this address }
    
    static member entities =
        { Get = fun this -> this.Entities
          Set = fun entities this -> { this with Entities = entities }}