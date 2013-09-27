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
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.Audio
open Nu.Rendering

let inline emptyFailure _ = None

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
    with
        static member isDown =
            { Get = fun this -> this.IsDown
              Set = fun isDown this -> { this with IsDown = isDown }}

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
        static member position =
            { Get = fun this -> this.Position
              Set = fun position this -> { this with Position = position }}
        static member size =
            { Get = fun this -> this.Size
              Set = fun size this -> { this with Size = size }}
        static member button =
            { Get = fun this -> match this.GuiSemantic with Button button -> button | _ -> failwith "Gui is not a button."
              Set = fun button this -> match this.GuiSemantic with Button _ -> { this with GuiSemantic = Button button } | _ -> failwith "Gui is not a button."}
        static member label =
            { Get = fun this -> match this.GuiSemantic with Label label -> label | _ -> failwith "Gui is not a label."
              Set = fun label this -> match this.GuiSemantic with Label _ -> { this with GuiSemantic = Label label } | _ -> failwith "Gui is not a label."}
        static member buttonIsDown =
            Gui.button >>| Button.isDown

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
      IsEnabled : bool
      IsVisible : bool
      EntitySemantic : EntitySemantic }
    with
        static member isEnabled =
            { Get = fun this -> this.IsEnabled
              Set = fun isEnabled this -> { this with IsEnabled = isEnabled }}
        static member isVisible =
            { Get = fun this -> this.IsVisible
              Set = fun isVisible this -> { this with IsVisible = isVisible }}
        static member gui =
            { Get = fun this -> match this.EntitySemantic with Gui gui -> gui | _ -> failwith "Entity is not a gui."
              Set = fun gui this -> match this.EntitySemantic with Gui _ -> { this with EntitySemantic = Gui gui } | _ -> failwith "Entity is not a gui."}
        static member guiPosition =
            Entity.gui >>| Gui.position
        static member guiSize =
            Entity.gui >>| Gui.size
        static member button =
            Entity.gui >>| Gui.button
        static member buttonIsDown =
            Entity.gui >>| Gui.buttonIsDown
        static member label =
            Entity.gui >>| Gui.label

/// A game entity group.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Group =
    { ID : ID
      IsEnabled : bool
      IsVisible : bool
      Entities : Entity LunTrie }
    with
        static member isEnabled =
            { Get = fun this -> this.IsEnabled
              Set = fun isEnabled this -> { this with IsEnabled = isEnabled }}
        static member isVisible =
            { Get = fun this -> this.IsVisible
              Set = fun isVisible this -> { this with IsVisible = isVisible }}
        static member getEntity3 this successFn errorFn address =
            match address with
            | [] -> errorFn ("Invalid address '" + str address + "'.")
            | [head] ->
                let optEntity = LunTrie.tryFind head this.Entities
                match optEntity with
                | None -> errorFn ("No entity at address '" + str address + "'.")
                | Some entity -> successFn entity
            | _ :: _ -> errorFn ("Inavlid address for entity '" + str address + "'.")
        static member setEntity3 this errorFn address entity =
            match address with
            | [] -> errorFn ("Invalid address '" + str address + "'.")
            | [head] -> { this with Entities = LunTrie.add head entity this.Entities }
            | _ :: _ -> errorFn ("Inavlid address for entity '" + str address + "'.")
        static member entity address =
            { Get = fun this -> Group.getEntity3 this id failwith address
              Set = fun entity this -> Group.setEntity3 this failwith address entity }
        static member optEntity address =
            { Get = fun this -> Group.getEntity3 this (Some) emptyFailure address
              Set = fun optEntity this ->
                match optEntity with
                | None -> { this with Entities = LunTrie.remove address this.Entities }
                | Some entity -> Group.setEntity3 this (fun _ -> this) address entity }
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
    { ID : ID
      IsEnabled : bool
      IsVisible : bool
      Groups : Group LunTrie
      ScreenSemantic : ScreenSemantic }
    with
        static member isEnabled =
            { Get = fun this -> this.IsEnabled
              Set = fun isEnabled this -> { this with IsEnabled = isEnabled }}
        static member isVisible =
            { Get = fun this -> this.IsVisible
              Set = fun isVisible this -> { this with IsVisible = isVisible }}
        static member getGroup3 this successFn errorFn address =
            match address with
            | [] -> errorFn ("Invalid address '" + str address + "'.")
            | [head] ->
                let optGroup = LunTrie.tryFind head this.Groups
                match optGroup with
                | None -> errorFn ("No group at address '" + str address + "'.")
                | Some group -> successFn group
            | _ :: _ -> errorFn ("Inavlid address for group '" + str address + "'.")
        static member setGroup3 (this : Screen) errorFn address group =
            match address with
            | [] -> errorFn ("Invalid address '" + str address + "'.")
            | [head] -> { this with Groups = LunTrie.add head group this.Groups }
            | _ :: _ -> errorFn ("Inavlid address for group '" + str address + "'.")
        static member group address =
            { Get = fun this -> Screen.getGroup3 this id failwith address
              Set = fun group this -> Screen.setGroup3 this failwith address group }
        static member optGroup address =
            { Get = fun this -> Screen.getGroup3 this (Some) emptyFailure address
              Set = fun optGroup this ->
                match optGroup with
                | None -> { this with Groups = LunTrie.remove address this.Groups }
                | Some group -> Screen.setGroup3 this (fun _ -> this) address group }
        static member groups =
            { Get = fun this -> this.Groups
              Set = fun groups this -> { this with Groups = groups }}
        static member entity address =
            Screen.group ([List.head address]) >>| Group.entity (List.tail address)

/// A game.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Game =
    { ID : ID
      IsEnabled : bool
      Screens : Screen LunTrie
      OptActiveScreenAddress : Address option }
    with
        static member isEnabled =
            { Get = fun this -> this.IsEnabled
              Set = fun isEnabled this -> { this with IsEnabled = isEnabled }}
        static member isVisible =
            { Get = fun this -> this.IsVisible
              Set = fun isVisible this -> { this with IsVisible = isVisible }}
        static member getScreen3 this successFn errorFn address =
            match address with
            | [] -> errorFn ("Invalid address '" + str address + "'.")
            | [head] ->
                let optScreen = LunTrie.tryFind head this.Screens
                match optScreen with
                | None -> errorFn ("No screen at address '" + str address + "'.")
                | Some screen -> successFn screen
            | _ :: _ -> errorFn ("Inavlid address for screen '" + str address + "'.")
        static member setScreen3 (this : Game) errorFn address screen =
            match address with
            | [] -> errorFn ("Invalid address '" + str address + "'.")
            | [head] -> { this with Screens = LunTrie.add head screen this.Screens }
            | _ :: _ -> errorFn ("Inavlid address for screen '" + str address + "'.")
        static member screen address =
            { Get = fun this -> Game.getScreen3 this id failwith address
              Set = fun screen this -> Game.setScreen3 this failwith address screen }
        static member optScreen address =
            { Get = fun this -> Game.getScreen3 this (Some) emptyFailure address
              Set = fun optScreen this ->
                match optScreen with
                | None -> { this with Screens = LunTrie.remove address this.Screens }
                | Some screen -> Game.setScreen3 this (fun _ -> this) address screen }
        static member screens =
            { Get = fun this -> this.Screens
              Set = fun screens this -> { this with Screens = screens }}
        static member optActiveScreenAddress =
            { Get = fun this -> this.OptActiveScreenAddress
              Set = fun optActiveScreenAddress this -> { this with OptActiveScreenAddress = optActiveScreenAddress }}
        static member group (address : Address) =
            Game.screen ([List.head address]) >>| Screen.group (List.tail address)
        static member entity (address : Address) =
            Game.screen ([List.head address]) >>| Screen.entity (List.tail address)