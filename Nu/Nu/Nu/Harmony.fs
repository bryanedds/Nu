// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.IO
open System.Xml
open OpenTK
open Prime
open Nu

type HarmonyDwg =
    { DwgName : string }

type HarmonyElement =
    { ElementId : int64
      ElementName : string
      ElementFolder : string
      RootFolder : string
      Drawings : HarmonyDwg list }

type HarmonyProjectOptions =
    { Resolution : Vector2i
      FrameRate : int }

type HarmonyGroup =
    { GroupName : string }

type HarmonyElementSeq =
    { Exposures : int
      ElementSeqVal : int
      ElementSeqId : int64 }

type HarmonyColumn =
    { ColumnType : int
      ColumnName : string
      ColumnId : int64 }

type HarmonySceneOptions =
    { DefaultDisplay : string }

type HarmonyScene =
    { SceneName : string
      SceneId : int64
      FrameCount : int
      SceneColumns : unit list
      SceneOptions : HarmonySceneOptions
      RootGroup : HarmonyGroup }

type HarmonyProject =
    { Source : string
      Version : int
      Build : int
      Creator : string
      Elements : HarmonyElement list
      ProjectOptions : HarmonyProjectOptions
      TimeLineMarkers : unit list
      Scenes : HarmonyScene list }

/// A mostly broken implementation of Harmony for the Nu Game Engine.
[<RequireQualifiedAccess>]
module Harmony =

    let readProjectOptions (optionsNode : XmlNode) =
        { Resolution =
            (optionsNode.SelectSingleNode "resolution").Attributes.["size"].InnerText.Split ',' |>
            (fun size -> Array.map Int32.Parse size) |>
            (function [|x; y|] -> Vector2i (x, y) | _ -> failwithumf ())
          FrameRate = (optionsNode.SelectSingleNode "framerate").Attributes.["val"].InnerText |> Int32.Parse }

    let readElement (elementNode : XmlNode) =
        { ElementId = elementNode.Attributes.["id"].InnerText |> Int64.Parse
          ElementName = elementNode.Attributes.["elementName"].InnerText
          ElementFolder = elementNode.Attributes.["elementFolder"].InnerText
          RootFolder = elementNode.Attributes.["rootFolder"].InnerText
          Drawings = [] } // TODO

    let readElements (elementsNode : XmlNode) =
        let elementNodes = elementsNode.SelectNodes "element" |> enumerable |> List.ofSeq
        List.map readElement elementNodes

    let readProject (projectNode : XmlNode) =
        { Source = projectNode.Attributes.["source"].InnerText
          Version = projectNode.Attributes.["version"].InnerText |> Int32.Parse
          Build = projectNode.Attributes.["build"].InnerText |> Int32.Parse
          Creator = projectNode.Attributes.["creator"].InnerText
          Elements = readElements ^ projectNode.SelectSingleNode "elements"
          ProjectOptions = readProjectOptions ^ projectNode.SelectSingleNode "options"
          TimeLineMarkers = [] // TODO
          Scenes = [] } // TODO

    let readProjectFromFile (filePath : string) =
        let xmlScene = XmlDocument ()
        xmlScene.Load filePath
        let projectNode = xmlScene.SelectSingleNode "project"
        readProject projectNode