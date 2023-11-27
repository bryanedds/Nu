// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Numerics
open Prime
open Nu

type DragEntityState =
    | DragEntityPosition2d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : Vector2 * Entity : Entity
    | DragEntityRotation2d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : single * Entity : Entity
    | DragEntityInactive

type DragEyeState =
    | DragEyeCenter2d of Vector2 * Vector2
    | DragEyeInactive

type [<SymbolicExpansion>] GaiaState =
    { ProjectDllPath : string
      ProjectEditModeOpt : string option
      ProjectImperativeExecution : bool
      DesiredEyeCenter2d : Vector2
      DesiredEyeCenter3d : Vector3
      DesiredEyeRotation3d : Quaternion
      MasterSoundVolume : single
      MasterSongVolume : single }
    static member make dllPath editModeOpt imperativeExecution desiredEyeCenter2d desiredEyeCenter3d desiredEyeRotation3d masterSoundVolume masterSongVolume =
        { ProjectDllPath = dllPath
          ProjectEditModeOpt = editModeOpt
          ProjectImperativeExecution = imperativeExecution
          DesiredEyeCenter2d = desiredEyeCenter2d
          DesiredEyeCenter3d = desiredEyeCenter3d
          DesiredEyeRotation3d = desiredEyeRotation3d
          MasterSoundVolume = masterSoundVolume
          MasterSongVolume = masterSongVolume }
    static member defaultState =
        GaiaState.make "" None false v2Zero Constants.Engine.EyeCenter3dDefault quatIdentity Constants.Audio.SoundVolumeDefault Constants.Audio.SongVolumeDefault