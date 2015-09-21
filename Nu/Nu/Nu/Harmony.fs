// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Globalization
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

type HarmonyTimelineMarker =
    { MarkerStart : int
      MarkerLength : int
      MarkerNote : string }

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

type HarmonyScene =
    { SceneName : string
      SceneId : int64
      NbFrames : int
      SceneColumns : unit list
      RootGroup : HarmonyGroup }

type HarmonyProject =
    { Source : string
      Version : int
      Build : int
      Creator : string
      Elements : HarmonyElement list
      ProjectOptions : HarmonyProjectOptions
      TimelineMarkers : HarmonyTimelineMarker list
      Scenes : HarmonyScene list }

/// A mostly broken implementation of Harmony for the Nu Game Engine.
[<RequireQualifiedAccess>]
module Harmony =

    let readGroup (groupNode : XmlNode) =
        { GroupName = groupNode.Attributes.["name"].InnerText }

    let readElement (elementNode : XmlNode) =
        { ElementId = elementNode.Attributes.["id"].InnerText |> Int64.Parse
          ElementName = elementNode.Attributes.["elementName"].InnerText
          ElementFolder = elementNode.Attributes.["elementFolder"].InnerText
          RootFolder = elementNode.Attributes.["rootFolder"].InnerText
          Drawings = [] } // TODO

    let readElements (elementsNode : XmlNode) =
        let elementNodes = elementsNode.SelectNodes "element" |> enumerable |> List.ofSeq
        List.map readElement elementNodes

    let readProjectOptions (optionsNode : XmlNode) =
        { Resolution =
            (optionsNode.SelectSingleNode "resolution").Attributes.["size"].InnerText.Split ',' |>
            (fun size -> Array.map Int32.Parse size) |>
            (function [|x; y|] -> Vector2i (x, y) | _ -> failwithumf ())
          FrameRate = (optionsNode.SelectSingleNode "framerate").Attributes.["val"].InnerText |> Int32.Parse }

    let readTimelineMarker (markerNode : XmlNode) =
        { MarkerStart = markerNode.Attributes.["markerStart"].InnerText |> Int32.Parse
          MarkerLength = markerNode.Attributes.["markerLength"].InnerText |> Int32.Parse
          MarkerNote = markerNode.Attributes.["note"].InnerText }

    let readTimelineMarkers (markersNode : XmlNode) =
        let markerNodes = markersNode.SelectNodes "marker" |> enumerable |> List.ofSeq
        List.map readTimelineMarker markerNodes

    let readScene (sceneNode : XmlNode) =
        { SceneName = sceneNode.Attributes.["name"].InnerText
          SceneId = sceneNode.Attributes.["id"].InnerText |> fun str -> Int64.Parse (str, NumberStyles.AllowHexSpecifier)
          NbFrames = sceneNode.Attributes.["nbframes"].InnerText |> Int32.Parse
          SceneColumns = [] // TODO
          RootGroup = readGroup ^ sceneNode.SelectSingleNode "rootgroup" }

    let readScenes (scenesNode : XmlNode) =
        let sceneNodes = scenesNode.SelectNodes "scene" |> enumerable |> List.ofSeq
        List.map readScene sceneNodes

    let readProject (projectNode : XmlNode) =
        { Source = projectNode.Attributes.["source"].InnerText
          Version = projectNode.Attributes.["version"].InnerText |> Int32.Parse
          Build = projectNode.Attributes.["build"].InnerText |> Int32.Parse
          Creator = projectNode.Attributes.["creator"].InnerText
          Elements = readElements ^ projectNode.SelectSingleNode "elements"
          ProjectOptions = readProjectOptions ^ projectNode.SelectSingleNode "options"
          TimelineMarkers = readTimelineMarkers ^ projectNode.SelectSingleNode "timelineMarkers"
          Scenes = readScenes ^ projectNode.SelectSingleNode "scenes" }

    let readProjectFromFile (filePath : string) =
        let xmlScene = XmlDocument ()
        xmlScene.Load filePath
        let projectNode = xmlScene.SelectSingleNode "project"
        readProject projectNode