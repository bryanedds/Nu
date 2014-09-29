namespace NuTests
open System
open System.IO
open System.Xml
open Xunit
open Prime
open Nu
open Nu.Constants
module WorldTests =

    let [<Fact>] something () =
        ignore <| World.makeEmpty ()
