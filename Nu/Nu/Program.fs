// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Diagnostics
open System.IO
open Prime
module Program =

    /// Program entry point.
    let [<EntryPoint; STAThread>] main _ =

        // ensure template directory exists
        let programDir = Reflection.Assembly.GetEntryAssembly().Location |> Path.GetDirectoryName 
        let slnDir = programDir + "/../../../.." |> Path.Simplify
        let templateDir = programDir + "/../../../Nu.Template" |> Path.Simplify
        if Directory.Exists templateDir then

            // query user to create new project
            Console.Write "Create a new game with the Nu Game Engine? [y/n]: "
            let result = Console.ReadLine ()
            match result.ToUpper () with
            | "Y" ->

                // execute name entry
                Console.Write "Please enter your project's name (no spaces, tabs or dots, PascalCase is preferred): "
                let name = Console.ReadLine ()
                let name = name.Replace(" ", "").Replace("\t", "").Replace(".", "")
                if Array.notExists (fun char -> name.Contains (string char)) (Path.GetInvalidPathChars ()) then

                    // compute directories
                    let templateIdentifier = templateDir.Replace("/", "\\") // this is what dotnet knows the template as for uninstall...
                    let templateFileName = "Nu.Template.fsproj"
                    let projectsDir = programDir + "/../../../../Projects" |> Path.Simplify
                    let newProjDir = projectsDir + "/" + name |> Path.Simplify
                    let newFileName = name + ".fsproj"
                    let newProj = newProjDir + "/" + newFileName |> Path.Simplify

                    // ensure project doesn't already exist
                    if not (File.Exists newProj) then

                        // echo creation begun
                        Console.Write ("Creating project '" + name + "' in '" + projectsDir + "'...")

                        // install nu template
                        Directory.SetCurrentDirectory templateDir
                        Process.Start("dotnet", "new -u \"" + templateIdentifier + "\"").WaitForExit()
                        Process.Start("dotnet", "new -i ./").WaitForExit()

                        // instantiate nu template
                        Directory.SetCurrentDirectory projectsDir
                        Directory.CreateDirectory name |> ignore<DirectoryInfo>
                        Directory.SetCurrentDirectory newProjDir
                        Process.Start("dotnet", "new nu-game --force").WaitForExit()

                        // rename project file
                        File.Copy (templateFileName, newFileName, true)
                        File.Delete templateFileName

                        // substitute $safeprojectname$ in project file
                        let newProjStr = File.ReadAllText newProj
                        let newProjStr = newProjStr.Replace("$safeprojectname$", name)
                        File.WriteAllText (newProj, newProjStr)

                        // add project to sln file
                        Directory.SetCurrentDirectory slnDir
                        let projectGuid = Gen.id
                        let projectGuidStr = projectGuid.ToString().ToUpper()
                        let slnLines = "Nu.sln" |> File.ReadAllLines |> Array.toList
                        let insertionIndex = List.findIndex ((=) "Global") slnLines
                        let slnLines =
                            List.take insertionIndex slnLines @
                            ["Project(\"{F2A71F9B-5D33-465A-A702-920D77279786}\") = \"" + name + "\", \"Projects\\" + name + "\\" + name + ".fsproj\", \"{" + string projectGuid + "}\""
                             "EndProject"] @
                            List.skip insertionIndex slnLines
                        let insertionIndex = List.findIndex ((=) "\tGlobalSection(SolutionProperties) = preSolution") slnLines - 1
                        let slnLines =
                            List.take insertionIndex slnLines @
                            ["\t\t{" + projectGuidStr + "}.Debug|x64.ActiveCfg = Debug|x64"
                             "\t\t{" + projectGuidStr + "}.Debug|x64.Build.0 = Debug|x64"
                             "\t\t{" + projectGuidStr + "}.Release|x64.ActiveCfg = Release|x64"
                             "\t\t{" + projectGuidStr + "}.Release|x64.Build.0 = Release|x64"] @
                            List.skip insertionIndex slnLines
                        let insertionIndex = List.findIndex ((=) "\tGlobalSection(ExtensibilityGlobals) = postSolution") slnLines - 1
                        let slnLines =
                            List.take insertionIndex slnLines @
                            ["\t\t{" + projectGuidStr + "} = {E3C4D6E1-0572-4D80-84A9-8001C21372D3}"] @
                            List.skip insertionIndex slnLines
                        File.WriteAllLines ("Nu.sln", List.toArray slnLines)

                        // success
                        Constants.Engine.ExitCodeSuccess

                    // project already exists
                    else
                        Console.Write ("Project name '" + name + "' already exists.")
                        Constants.Engine.ExitCodeFailure

                // invalid name; failure
                else
                    Console.Write ("Project name '" + name + "' contains invalid characters.")
                    Constants.Engine.ExitCodeFailure

            // rejected
            | _ -> Constants.Engine.ExitCodeSuccess

        // nothing to do
        else Constants.Engine.ExitCodeSuccess