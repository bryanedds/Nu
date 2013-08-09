// NaiveSimulation - a technical prototype for Nu, the Flipped! prototype engine.
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
open Nu.Rendering

/// A generic identification code type.
/// OPTIMIZATION: A 32-bit type rather than 64 as a way to save performance on 32-bit platforms.
/// Note that this optimization is speculative.
/// A serializable value type.
type ID = uint32

/// A 2D vector that stands-in for the one that will come from a .NET library / wrapper.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] Vector2 =
    { X : single
      Y : single }

/// An algabraically-closed semantics for game gui elements.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] GuiSemantic =
    | Button // of Button
    | CheckBox // of CheckBox
    | Label // of Label
 // | ...additional controls
 // | UserDefinedGui of IUserDefinedGui (* this would give us open gui semantics, but perhaps at the cost of its value semantics...  *)

/// A game gui element.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] Gui =
    { Position : Vector2
      Size : Vector2
      Semantic : GuiSemantic }

/// An algabraically-closed semantics for game actors.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] ActorSemantic =
    | Avatar // of Avatar
    | Item // of Item
    | Geostage // of Geostage
 // | ...additional actors
 // | UserDefinedActor of IUserDefinedActor (* this would be one way to get open actor semantics, but perhaps at the cost of its value semantics... *)

/// A game actor.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] Actor =
    { Position : Vector2
      Scale : Vector2
      Semantic : ActorSemantic }

/// An algabraically-closed semantics for game entities.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] EntitySemantic =
    | Gui of Gui
    | Actor of Actor
 // | Actor2D of Actor2D

/// A game entity.
/// A serializable value type, albeit with quick equality, hashing, and comparison for use in Maps.
type [<CustomEquality; CustomComparison>] Entity =
    { ID : ID
      Visible : bool
      Enabled : bool
      Semantic : EntitySemantic }
    interface Entity IComparable with member this.CompareTo that = this.ID.CompareTo that.ID
    override this.GetHashCode () = int this.ID ^^^ typeof<Entity>.GetHashCode ()
    override this.Equals that = match that with :? Entity as thatEntity -> this.ID = thatEntity.ID | _ -> false

/// A game entity group.
/// A serializable value type, albeit with quick equality, hashing, and comparison for use in Maps.
type [<CustomEquality; CustomComparison>] Group =
    { ID : ID
      Visible : bool
      Enabled : bool
      Entities : Entity LunTrie }
    interface Group IComparable with member this.CompareTo that = this.ID.CompareTo that.ID
    override this.GetHashCode () = int this.ID ^^^ typeof<Group>.GetHashCode ()
    override this.Equals that = match that with :? Group as thatGroup -> this.ID = thatGroup.ID | _ -> false
    
/// An algabraically-closed semantics for game screens.
/// A serializable value type.
type [<StructuralEquality; StructuralComparison>] ScreenSemantic =
    | Title // of Title
    | Intro // of Intro
    | Playground // of Playground
 // | ...additional screens
 // | UserDefinedScreen of IUserDefinedScreen (* this would give us open screen semantics, but perhaps at the cost of its value semantics...  *)
    
/// A game screen.
/// A serializable value type, albeit with quick equality, hashing, and comparison for use in Maps.
type [<CustomEquality; CustomComparison>] Screen =
    { ID : ID
      Visible : bool
      Enabled : bool
      Groups : Group LunTrie
      Semantic : ScreenSemantic }
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