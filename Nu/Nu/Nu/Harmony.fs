// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.IO
open System.Xml
open OpenTK
open Nu

type HarmonyDwg =
    { DwgName : string }

type HarmonyElement =
    { Drawings : HarmonyDwg list }

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
module Harmony =

    let readHarmonyProject (projectNode : XmlNode) =
        let source = projectNode.Attributes.["source"].InnerText
        let version = projectNode.Attributes.["version"].InnerText |> Int32.Parse
        let build = projectNode.Attributes.["build"].InnerText |> Int32.Parse
        let creator = projectNode.Attributes.["creator"].InnerText
        { Source = source
          Version = version
          Build = build
          Creator = creator
          Elements = [] // TODO
          ProjectOptions = { Resolution = Vector2i.Zero; FrameRate = 0 } // TODO
          TimeLineMarkers = [] // TODO
          Scenes = [] } // TODO

    let readHarmonyProjectFromFile (filePath : string) =
        let xmlScene = XmlDocument ()
        xmlScene.Load filePath
        let projectNode = xmlScene.SelectSingleNode "project"
        readHarmonyProject projectNode