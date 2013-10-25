module NuEditLogic.Program
open NuEditDesign
open System
open System.Windows.Forms
open System.Threading

[<EntryPoint>]
let main _ = 
    use form = new NuEditForm ()
    Application.EnableVisualStyles ()
    Application.Run form
    0