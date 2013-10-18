module Nu.Screen
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.Entity
open Nu.Group

/// An algabraically-closed semantics for game screens.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] ScreenSemantic =
    | Title // of Title
    | Intro // of Intro
 // | ...additional screens
 // | UserDefinedScreen of IUserDefinedScreen (* this would give us more open screen semantics, but perhaps at the cost of its value semantics...  *)
    
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