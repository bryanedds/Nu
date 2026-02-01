// Gaia - The Nu Game Engine editor.
// Required Notice:
// Copyright (C) Bryan Edds.
// Gaia - The Nu Game Engine editor is licensed under the Nu Game Engine Noncommercial License.
// See: https://github.com/bryanedds/Nu/master/License.md

namespace Nu.Gaia
open System
open System.Numerics
open Prime
open Nu
open Nu.Gaia

type DragEntityState =
    | DragEntityPosition2d of DateTime : DateTimeOffset * Snapshotted : bool ref * MousePositionWorldOrig : Vector2 * EntityDragOffset : Vector2 * Entity : Entity
    | DragEntityRotation2d of DateTime : DateTimeOffset * Snapshotted : bool ref * MousePositionWorldOrig : Vector2 * EntityDragOffset : single * Entity : Entity
    | DragEntityInactive

type DragEyeState =
    | DragEye2dCenter of Offset : Vector2 * Origin : Vector2
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
      AlternativeEyeTravelInput : bool
      OverlayMode : bool }

    static member make
        dllPath editModeOpt freshlyLoaded imperativeExecution editWhileAdvancing
        desiredEye2dCenter desiredEye3dCenter desiredEye3dRotation masterSoundVolume masterSongVolume
        snaps2dSelected snaps2d snaps3d creationElevation creationDistance alternativeEyeTravelInput
        overlayMode =
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
          AlternativeEyeTravelInput = alternativeEyeTravelInput
          OverlayMode = overlayMode }

    static member defaultState =
        GaiaState.make
            "" None false false false
            v2Zero Constants.Engine.Eye3dCenterDefault quatIdentity Constants.Audio.SoundVolumeDefault Constants.Audio.SongVolumeDefault
            true Constants.Gaia.Snaps2dDefault Constants.Gaia.Snaps3dDefault 0.0f 2.0f false false
            
type TypeName =
    unit // some way to identify a type at runtime (string?)

type BlockStyle =
    { BlockDescription : string
      BlockProperties : Map<string, TypeName * Symbol> }

type BlockPalette =
    { BlockStyles : Map<Color, BlockStyle> } // palette of 32 color tones, each with 4 additional shades (2 brighter, 2 darker).
    static member add : BlockStyle -> BlockPalette -> BlockPalette = Unchecked.defaultof<_>

type Block =
    { BlockColor : Color // also used to look up BlockStyle
      BlockColorShift : int // -3 .. +3. Each shift can signify an additional level of adornment, such as stacking a pen on a book on a desk.
      BlockProperties : Map<string, TypeName * Symbol> }

type BlockGranulator =
    { BlockGranulation : Vector3i
      BlockGranulator : BlockChunk -> BlockChunk }

and BlockCombiner =
    { BlockCombination : Vector3i
      BlockCombiner : BlockChunk -> BlockChunk }

and BlockChunk =
    { BlockBounds : Box3i
      Blocks : Map<Vector3i, Block> }
    static member granulate : BlockGranulator -> BlockChunk -> BlockChunk = Unchecked.defaultof<_>
    static member combine : BlockCombiner -> BlockChunk -> BlockChunk = Unchecked.defaultof<_>

type BlockMap =
    { BlockScale : Vector3
      BlockChunk : BlockChunk }
    member this.Size = this.BlockChunk.BlockBounds.Size.V3 * this.BlockScale

type BlockCursor =
    { BlockPosition : Vector3i }

type BlockSelection =
    | BlockSelectionVolume of Vector3i * Vector3i
    | BlockSelectionAdHoc of Vector3i Set

type BlockOutputRigidModel =
    { BlockOutputAffine : Affine
      BlockOutputStaticModel : StaticModel AssetTag
      BlockOutputBodyType : BodyType }

type BlockOutputRigidModelSurface =
    { BlockOutputAffine : Affine
      BlockOutputStaticModel : StaticModel AssetTag
      BlockOutputSurfaceIndex : int
      BlockOutputBodyType : BodyType }

type BlockOutput =
    | BlockOutputRigidModel of BlockOutputRigidModel
    | BlockOutputRigidModelSurface of BlockOutputRigidModelSurface
    | BlockOutputUserDefined of string // NuPlugin provides interpretation?
    | BlockOutputs of BlockOutput list

type BlockProcessor =
    { BlockProcessingVolume : Vector3i
      BlockMatch : BlockChunk -> bool 
      BlockEval : BlockChunk -> BlockOutput * (*leftovers/replacements*) BlockChunk } // replacements allow for additional passes, such as wall decoration and object stacking

type BlockPass =
    { BlockProcessors : Map<string, BlockProcessor> }

type BlockPlane =
    | XNeg | XPos | YNeg | YPos | ZNeg | ZPos

type BlockEditor =
    { BlockMap : BlockMap
      BlockPalette : BlockPalette
      BlockPlane : BlockPlane // plane currently containing cursor
      BlockCursor : BlockCursor
      BlockSelection : BlockSelection
      BlockPasses : Map<string, BlockPass> }