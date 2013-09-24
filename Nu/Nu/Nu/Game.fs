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

type [<StructuralEquality; NoComparison>] Button =
    { IsUp : bool
      UpSprite : SpriteDescriptor
      DownSprite : SpriteDescriptor
      ClickSound : SoundMessage }
      
type [<StructuralEquality; NoComparison>] Label =
    { Sprite : SpriteDescriptor }

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
/// A serializable value type, albeit with quick equality, hashing, and comparison for use in Maps.
type [<CustomEquality; CustomComparison>] Entity =
    { ID : ID
      Enabled : bool
      Visible : bool
      EntitySemantic : EntitySemantic }
    interface Entity IComparable with member this.CompareTo that = this.ID.CompareTo that.ID
    override this.GetHashCode () = int this.ID ^^^ typeof<Entity>.GetHashCode ()
    override this.Equals that = match that with :? Entity as thatEntity -> this.ID = thatEntity.ID | _ -> false

/// A game entity group.
/// A serializable value type, albeit with quick equality, hashing, and comparison for use in Maps.
type [<CustomEquality; CustomComparison>] Group =
    { ID : ID
      Enabled : bool
      Visible : bool
      Entities : Entity LunTrie }
    interface Group IComparable with member this.CompareTo that = this.ID.CompareTo that.ID
    override this.GetHashCode () = int this.ID ^^^ typeof<Group>.GetHashCode ()
    override this.Equals that = match that with :? Group as thatGroup -> this.ID = thatGroup.ID | _ -> false
    
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
/// A serializable value type, albeit with quick equality, hashing, and comparison for use in Maps.
type [<CustomEquality; CustomComparison>] Screen =
    { ID : ID
      Enabled : bool
      Visible : bool
      Groups : Group LunTrie
      ScreenSemantic : ScreenSemantic }
    interface Screen IComparable with member this.CompareTo that = this.ID.CompareTo that.ID
    override this.GetHashCode () = int this.ID ^^^ typeof<Screen>.GetHashCode ()
    override this.Equals that = match that with :? Screen as thatScreen -> this.ID = thatScreen.ID | _ -> false

/// A game.
/// A serializable value type, albeit with quick equality, hashing, and comparison for use in Maps.
type [<CustomEquality; CustomComparison>] Game =
    { ID : ID
      Enabled : bool
      Screens : Screen LunTrie
      ActiveScreen : Screen option }
    interface Game IComparable with member this.CompareTo that = this.ID.CompareTo that.ID
    override this.GetHashCode () = int this.ID ^^^ typeof<Game>.GetHashCode ()
    override this.Equals that = match that with :? Game as thatGame -> this.ID = thatGame.ID | _ -> false