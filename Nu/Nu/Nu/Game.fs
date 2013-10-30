module Nu.Game
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.LensHelper
open Nu.Entity
open Nu.Group
open Nu.Screen

// WISDOM:
//
// A simulation that would put physics on another thread should likely do so in a different app
// domain with communication via .NET remoting to make 100% sure that no sharing is happening.
//
// NOTE: for simulation types, value semantics are preferred over open semantics as it eases
// serialization and other forms of automation. However, perhaps there is a way to get both...

// TODO: go through Prime and label / attribute all its types as was done here.

/// A game.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Game =
    { Id : Id
      Screens : Map<Lun, Screen>
      OptSelectedScreenAddress : Address option
      GameSemantic : unit }
        
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
    
    static member entity address =
        Game.screen [List.head address] >>| Screen.entity (List.tail address)
    
    static member optEntity address =
        Game.screen [List.head address] >>| Screen.optEntity (List.tail address)
    
    static member entityGui address =
        Game.screen [List.head address] >>| Screen.entityGui (List.tail address)
    
    static member optEntityGui address =
        Game.screen [List.head address] >>| Screen.optEntityGui (List.tail address)
    
    static member entityGuiButton address =
        Game.screen [List.head address] >>| Screen.entityGuiButton (List.tail address)
    
    static member optEntityGuiButton address =
        Game.screen [List.head address] >>| Screen.optEntityGuiButton (List.tail address)
    
    static member entityGuiLabel address =
        Game.screen [List.head address] >>| Screen.entityGuiLabel (List.tail address)
    
    static member optEntityGuiLabel address =
        Game.screen [List.head address] >>| Screen.optEntityGuiLabel (List.tail address)
    
    static member entityGuiTextBox address =
        Game.screen [List.head address] >>| Screen.entityGuiTextBox (List.tail address)
    
    static member optEntityGuiTextBox address =
        Game.screen [List.head address] >>| Screen.optEntityGuiTextBox (List.tail address)
    
    static member entityGuiToggle address =
        Game.screen [List.head address] >>| Screen.entityGuiToggle (List.tail address)
    
    static member optEntityGuiToggle address =
        Game.screen [List.head address] >>| Screen.optEntityGuiToggle (List.tail address)
    
    static member entityGuiFeeler address =
        Game.screen [List.head address] >>| Screen.entityGuiFeeler (List.tail address)
    
    static member optEntityGuiFeeler address =
        Game.screen [List.head address] >>| Screen.optEntityGuiFeeler (List.tail address)
    
    static member entityActor address =
        Game.screen [List.head address] >>| Screen.entityActor (List.tail address)
    
    static member optEntityActor address =
        Game.screen [List.head address] >>| Screen.optEntityActor (List.tail address)
    
    static member entityActorBlock address =
        Game.screen [List.head address] >>| Screen.entityActorBlock (List.tail address)
    
    static member optEntityActorBlock address =
        Game.screen [List.head address] >>| Screen.optEntityActorBlock (List.tail address)
    
    static member entityActorAvatar address =
        Game.screen [List.head address] >>| Screen.entityActorAvatar (List.tail address)
    
    static member optEntityActorAvatar address =
        Game.screen [List.head address] >>| Screen.optEntityActorAvatar (List.tail address)
    
    static member entityActorTileMap address =
        Game.screen [List.head address] >>| Screen.entityActorTileMap (List.tail address)
    
    static member optEntityActorTileMap address =
        Game.screen [List.head address] >>| Screen.optEntityActorTileMap (List.tail address)
    
    static member group address =
        Game.screen [List.head address] >>| Screen.group (List.tail address)
    
    static member optGroup address =
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
        { Get = fun this -> this.OptSelectedScreenAddress
          Set = fun optActiveScreenAddress this -> { this with OptSelectedScreenAddress = optActiveScreenAddress }}