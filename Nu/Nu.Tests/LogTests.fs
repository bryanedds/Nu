// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open System.Diagnostics
open NUnit.Framework
open Prime
open Nu
open Nu.Tests

module LogTests =

    [<SetUp>]
    let Setup () =
        // Initialize logging for tests
        Log.init None

    [<Test>]
    let ``Console.WriteLine redirects to logging system`` () =
        // Capture trace output
        let mutable capturedOutput = ""
        let listener = 
            { new TraceListener () with
                member _.Write (message : string) = capturedOutput <- capturedOutput + message
                member _.WriteLine (message : string) = capturedOutput <- capturedOutput + message + Environment.NewLine }
        
        Trace.Listeners.Add listener |> ignore
        
        try
            // Write to Console which should be redirected
            Console.WriteLine "Test native message"
            
            // Flush to ensure output is captured
            Log.flush ()
            System.Threading.Thread.Sleep 100 // Give a moment for async operations
            
            // Verify the message was captured with Native tag
            Assert.IsTrue (capturedOutput.Contains "Native")
            Assert.IsTrue (capturedOutput.Contains "Test native message")
        finally
            Trace.Listeners.Remove listener

    [<Test>]
    let ``Console.Error.WriteLine redirects to logging system`` () =
        // Capture trace output
        let mutable capturedOutput = ""
        let listener = 
            { new TraceListener () with
                member _.Write (message : string) = capturedOutput <- capturedOutput + message
                member _.WriteLine (message : string) = capturedOutput <- capturedOutput + message + Environment.NewLine }
        
        Trace.Listeners.Add listener |> ignore
        
        try
            // Write to Console.Error which should be redirected
            Console.Error.WriteLine "Test native error"
            
            // Flush to ensure output is captured
            Log.flush ()
            System.Threading.Thread.Sleep 100 // Give a moment for async operations
            
            // Verify the message was captured with Native tag and Error level
            Assert.IsTrue (capturedOutput.Contains "Native")
            Assert.IsTrue (capturedOutput.Contains "Error")
            Assert.IsTrue (capturedOutput.Contains "Test native error")
        finally
            Trace.Listeners.Remove listener
