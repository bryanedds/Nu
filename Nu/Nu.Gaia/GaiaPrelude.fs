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
      ProjectFreshlyLoaded : bool
      DesiredEyeCenter2d : Vector2
      DesiredEyeCenter3d : Vector3
      DesiredEyeRotation3d : Quaternion
      MasterSoundVolume : single
      MasterSongVolume : single
      Snaps2dSelected : bool
      Snaps2d : single * single * single
      Snaps3d : single * single * single
      CreationElevation : single
      CreationDistance : single
      AlternativeEyeTravelInput : bool }
    static member make
        dllPath editModeOpt imperativeExecution freshlyLoaded
        desiredEyeCenter2d desiredEyeCenter3d desiredEyeRotation3d masterSoundVolume masterSongVolume
        snaps2dSelected snaps2d snaps3d creationElevation creationDistance alternativeEyeTravelInput =
        { ProjectDllPath = dllPath
          ProjectEditModeOpt = editModeOpt
          ProjectImperativeExecution = imperativeExecution
          ProjectFreshlyLoaded = freshlyLoaded
          DesiredEyeCenter2d = desiredEyeCenter2d
          DesiredEyeCenter3d = desiredEyeCenter3d
          DesiredEyeRotation3d = desiredEyeRotation3d
          MasterSoundVolume = masterSoundVolume
          MasterSongVolume = masterSongVolume
          Snaps2dSelected = snaps2dSelected
          Snaps2d = snaps2d
          Snaps3d = snaps3d
          CreationElevation = creationElevation
          CreationDistance = creationDistance
          AlternativeEyeTravelInput = alternativeEyeTravelInput }
    static member defaultState =
        GaiaState.make
            "" None false false
            v2Zero Constants.Engine.EyeCenter3dDefault quatIdentity Constants.Audio.SoundVolumeDefault Constants.Audio.SongVolumeDefault
            true Constants.Gaia.Snaps2dDefault Constants.Gaia.Snaps3dDefault 0.0f 2.0f false