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
    | DragEye2dCenter of Vector2 * Vector2
    | DragEyeInactive

type [<SymbolicExpansion>] GaiaState =
    { ProjectDllPath : string
      ProjectEditModeOpt : string option
      ProjectFreshlyLoaded : bool
      ProjectImperativeExecution : bool
      EditWhileAdvancing : bool
      DesiredEye2dCenter : Vector2
      DesiredEye3dCenter : Vector3
      DesiredEye3dRotation : Quaternion
      MasterSoundVolume : single
      MasterSongVolume : single
      Snaps2dSelected : bool
      Snaps2d : single * single * single
      Snaps3d : single * single * single
      CreationElevation : single
      CreationDistance : single
      AlternativeEyeTravelInput : bool }
    static member make
        dllPath editModeOpt freshlyLoaded imperativeExecution editWhileAdvancing
        desiredEye2dCenter desiredEye3dCenter desiredEye3dRotation masterSoundVolume masterSongVolume
        snaps2dSelected snaps2d snaps3d creationElevation creationDistance alternativeEyeTravelInput =
        { ProjectDllPath = dllPath
          ProjectEditModeOpt = editModeOpt
          ProjectFreshlyLoaded = freshlyLoaded
          ProjectImperativeExecution = imperativeExecution
          EditWhileAdvancing = editWhileAdvancing
          DesiredEye2dCenter = desiredEye2dCenter
          DesiredEye3dCenter = desiredEye3dCenter
          DesiredEye3dRotation = desiredEye3dRotation
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
            "" None false false false
            v2Zero Constants.Engine.Eye3dCenterDefault quatIdentity Constants.Audio.SoundVolumeDefault Constants.Audio.SongVolumeDefault
            true Constants.Gaia.Snaps2dDefault Constants.Gaia.Snaps3dDefault 0.0f 2.0f false