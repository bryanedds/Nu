namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DomainModel
open Nu.Entities
open Nu.Groups
open Nu.Screens

// WISDOM:
//
// A simulation that would put physics on another thread should likely do so in a different app
// domain with communication via .NET remoting to make 100% sure that no sharing is happening.
//
// NOTE: for simulation types, value semantics are preferred over open semantics as it eases
// serialization and other forms of automation. However, perhaps there is a way to get both...

type [<StructuralEquality; NoComparison; CLIMutable>] Game =
    { Id : Id
      OptSelectedScreenModelAddress : Address option }

type [<StructuralEquality; NoComparison; CLIMutable>] OmniGame =
    { Game : Game
      OptPlayer : OmniPlayer option }
        
type [<StructuralEquality; NoComparison>] GameModel =
    | Game of Game
    | OmniGame of OmniGame

module Games =

    let _ = 0 // NOTE: just here to make the Games module