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
        let slnDir = programDir + "/../../../../.." |> Path.Simplify
        let templateDir = programDir + "/../../../../Nu.Template" |> Path.Simplify
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
                    let projectsDir = programDir + "/../../../../../Projects" |> Path.Simplify
                    let newProjDir = projectsDir + "/" + name |> Path.Simplify
                    let newFileName = name + ".fsproj"
                    let newProj = newProjDir + "/" + newFileName |> Path.Simplify

                    // ensure project doesn't already exist
                    if not (File.Exists newProj) then

                        // echo creation begun
                        Console.Write ("Creating project '" + name + "' in '" + projectsDir + "'...")

                        // install nu template
                        Directory.SetCurrentDirectory templateDir
                        Process.Start("dotnet", "new uninstall \"" + templateIdentifier + "\"").WaitForExit()
                        Process.Start("dotnet", "new install ./").WaitForExit()

                        // instantiate nu template
                        Directory.SetCurrentDirectory projectsDir
                        Directory.CreateDirectory name |> ignore<DirectoryInfo>
                        Directory.SetCurrentDirectory newProjDir
                        Process.Start("dotnet", "new nu-game --force").WaitForExit()

                        // rename project file
                        File.Copy (templateFileName, newFileName, true)
                        File.Delete templateFileName

                        // substitute project guid in project file
                        let projectGuid = Gen.id
                        let projectGuidStr = projectGuid.ToString().ToUpper()
                        let newProjStr = File.ReadAllText newProj
                        let newProjStr = newProjStr.Replace("4DBBAA23-56BA-43CB-AB63-C45D5FC1016F", projectGuidStr)
                        File.WriteAllText (newProj, newProjStr)

                        // add project to sln file
                        Directory.SetCurrentDirectory slnDir
                        let slnLines = "Nu.sln" |> File.ReadAllLines |> Array.toList
                        let insertionIndex = List.findIndex ((=) "Global") slnLines
                        let slnLines =
                            List.take insertionIndex slnLines @
                            ["Project(\"{6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705}\") = \"" + name + "\", \"Projects\\" + name + "\\" + name + ".fsproj\", \"{" + projectGuidStr + "}\""
                             "EndProject"] @
                            List.skip insertionIndex slnLines
                        let insertionIndex = List.findIndex ((=) "\tGlobalSection(SolutionProperties) = preSolution") slnLines - 1
                        let slnLines =
                            List.take insertionIndex slnLines @
                            ["\t\t{" + projectGuidStr + "}.Debug|Any CPU.ActiveCfg = Debug|Any CPU"
                             "\t\t{" + projectGuidStr + "}.Debug|Any CPU.Build.0 = Debug|Any CPU"
                             "\t\t{" + projectGuidStr + "}.Release|Any CPU.ActiveCfg = Release|Any CPU"
                             "\t\t{" + projectGuidStr + "}.Release|Any CPU.Build.0 = Release|x64"] @
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

                // invalid name
                else
                    Console.Write ("Project name '" + name + "' contains invalid characters.")
                    Constants.Engine.ExitCodeFailure

            // cancelled
            | _ ->
                Constants.Engine.ExitCodeSuccess

        // template project missing
        else
            Console.Write ("Template project is missing; new project cannot be generated.")
            Constants.Engine.ExitCodeFailure