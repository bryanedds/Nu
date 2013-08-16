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
open Nu.Rendering

/// An algabraically-closed semantics for game gui elements.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] GuiSemantic =
    | Button // of Button
    | CheckBox // of CheckBox
    | Label // of Label
 // | ...additional controls
 // | UserDefinedGui of IUserDefinedGui (* this would give us open gui semantics, but perhaps at the cost of its value semantics...  *)

/// A game gui element.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Gui =
    { Position : Vector2
      Size : Vector2
      Semantic : GuiSemantic }

/// A Lustre stone.
type [<StructuralEquality; NoComparison>] LustreStone =
    { LustreStoneTag : unit
      Sprite : SpriteDescriptor }

/// A Lustre gem.
type [<StructuralEquality; NoComparison>] LustreGem =
    { LustreGemTag : unit
      Sprite : SpriteDescriptor }

/// A Lustre chest.
type [<StructuralEquality; NoComparison>] LustreChest =
    { LustreChestTag : unit
      Sprite : SpriteDescriptor }

/// An algabraically-closed semantics for game actors.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] ActorSemantic =
    | LustreStone of LustreStone
    | LustreChest of LustreChest
    | LustreGem of LustreGem
    | Avatar // of Avatar
    | Item // of Item
 // | ...additional actors
 // | UserDefinedActor of IUserDefinedActor (* this would be one way to get open actor semantics, but perhaps at the cost of its value semantics... *)

/// A game actor.
/// A serializable value type.
type [<StructuralEquality; NoComparison>] Actor =
    { Position : Vector2
      Size : Vector2
      Semantic : ActorSemantic }

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
type [<StructuralEquality; NoComparison>] ScreenSemantic =
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