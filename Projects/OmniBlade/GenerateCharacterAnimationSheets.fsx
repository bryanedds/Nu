// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "../../packages/Magick.NET-Q8-x64.7.5.0.1/lib/net40/Magick.NET-Q8-x64.dll"
#r "../../packages/Csv.1.0.58/lib/net40/Csv.dll"
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/Prime.8.1.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.8.0.0/lib/net472/Prime.Scripting.exe"
#r "../../packages/Aether.Physics2D.1.5.0/lib/net40/Aether.Physics2D.dll"
#r "../../packages/Nito.Collections.Deque.1.1.0/lib/netstandard2.0/Nito.Collections.Deque.dll"
#r "../../Nu/Nu.Dependencies/SDL2-CS.dll/lib/net20/SDL2-CS.dll"
#r "../../Nu/Nu.Dependencies/TiledSharp.1.0.2/lib/netstandard2.0/TiledSharp.dll"
#r "../../Nu/Nu.Math/bin/x64/Debug/Nu.Math.dll"
#r "../../Nu/Nu/bin/Debug/Nu.exe"

let workingDirPath = __SOURCE_DIRECTORY__ + "/bin/Debug"
if not (System.IO.Directory.Exists workingDirPath) then failwith "You must build the project in Debug mode at least once before running in interactive."
System.IO.Directory.SetCurrentDirectory workingDirPath

open System
open System.Drawing
open System.Numerics
open System.IO
open ImageMagick
open Prime
open Nu

type Metrics =
    { WidthSource : int
      HeightSource : int
      WidthDest : int
      HeightDest : int
      XOffset : int
      YOffset : int }

let copy (w : int) (h : int) (x : int) (y : int) (w2 : int) (h2 : int) (x2 : int) (y2 : int) (sourceImage : MagickImage) (targetImage : MagickImage) =
    if w <= w2 && h <= h2 then
        let xOffset = w2 - w
        let yOffset = h2 - h
        targetImage.CopyPixels (sourceImage, MagickGeometry (new Rectangle (x, y, w, h)), x2 + xOffset / 2, y2 + yOffset)
    elif w >= w2 && h <= h2 then
        let xOffset = w - w2
        let yOffset = h2 - h
        targetImage.CopyPixels (sourceImage, MagickGeometry (new Rectangle (x + xOffset / 2, y, w - xOffset, h)), x2, y2 + yOffset)
    elif w <= w2 && h >= h2 then
        let xOffset = w2 - w
        let yOffset = h - h2
        targetImage.CopyPixels (sourceImage, MagickGeometry (new Rectangle (x, y + yOffset, w, h - yOffset)), x2 + xOffset / 2, y2)
    else
        let xOffset = w - w2
        let yOffset = h - h2
        targetImage.CopyPixels (sourceImage, MagickGeometry (new Rectangle (x + xOffset / 2, y + yOffset, w - xOffset, h - yOffset)), x2, y2)

let flip (w : int) (h : int) (x : int) (y : int) (w2 : int) (h2 : int) (x2 : int) (y2 : int) (sourceImage : MagickImage) (targetImage : MagickImage) =
    use tempImage = new MagickImage (MagickColor.FromRgba (byte 0, byte 0, byte 0, byte 0), w, h)
    tempImage.CopyPixels (sourceImage, MagickGeometry (new Rectangle (x, y, w, h)), 0, 0)
    tempImage.Flop ()
    copy w h 0 0 w2 h2 x2 y2 tempImage targetImage

let copyWalkCels (sourceImage : MagickImage) (targetImage : MagickImage) metrics =
    let width = metrics.WidthSource
    let height = metrics.HeightSource
    let widthDest = metrics.WidthDest
    let heightDest = metrics.HeightDest
    let xOffset = metrics.XOffset
    let yOffset = metrics.YOffset
    copy width height (width * 0) (height * 3) widthDest heightDest (xOffset + widthDest * 0) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 1) (height * 3) widthDest heightDest (xOffset + widthDest * 1) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 2) (height * 3) widthDest heightDest (xOffset + widthDest * 2) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 1) (height * 3) widthDest heightDest (xOffset + widthDest * 3) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 0) (height * 2) widthDest heightDest (xOffset + widthDest * 4) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 1) (height * 2) widthDest heightDest (xOffset + widthDest * 5) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 2) (height * 2) widthDest heightDest (xOffset + widthDest * 6) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 1) (height * 2) widthDest heightDest (xOffset + widthDest * 7) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 0) (height * 0) widthDest heightDest (xOffset + widthDest * 8) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 1) (height * 0) widthDest heightDest (xOffset + widthDest * 9) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 2) (height * 0) widthDest heightDest (xOffset + widthDest * 10) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 1) (height * 0) widthDest heightDest (xOffset + widthDest * 11) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 0) (height * 1) widthDest heightDest (xOffset + widthDest * 12) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 1) (height * 1) widthDest heightDest (xOffset + widthDest * 13) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 2) (height * 1) widthDest heightDest (xOffset + widthDest * 14) (yOffset + heightDest * 0) sourceImage targetImage
    copy width height (width * 1) (height * 1) widthDest heightDest (xOffset + widthDest * 15) (yOffset + heightDest * 0) sourceImage targetImage

let copyIdleCels rowDest (sourceImage : MagickImage) (targetImage : MagickImage) metrics =
    let width = metrics.WidthSource
    let height = metrics.HeightSource
    let widthDest = metrics.WidthDest
    let heightDest = metrics.HeightDest
    let xOffset = metrics.XOffset
    let yOffset = metrics.YOffset
    copy width height (width * 1) (height * 3) widthDest heightDest (xOffset + widthDest * 0) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * 1) (height * 2) widthDest heightDest (xOffset + widthDest * 1) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * 1) (height * 0) widthDest heightDest (xOffset + widthDest * 2) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * 1) (height * 1) widthDest heightDest (xOffset + widthDest * 3) (yOffset + heightDest * rowDest) sourceImage targetImage

let copyTriCelsToQuadCels x y rowDest (sourceImage : MagickImage) (targetImage : MagickImage) metrics =
    let width = metrics.WidthSource
    let height = metrics.HeightSource
    let widthDest = metrics.WidthDest
    let heightDest = metrics.HeightDest
    let yOffset = metrics.YOffset
    flip width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 00) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 01) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 2)) (height * y) widthDest heightDest (widthDest * 02) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 03) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 04) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 05) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 2)) (height * y) widthDest heightDest (widthDest * 06) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 07) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 08) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 09) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 2)) (height * y) widthDest heightDest (widthDest * 10) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 11) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 12) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 13) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 2)) (height * y) widthDest heightDest (widthDest * 14) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 15) (yOffset + heightDest * rowDest) sourceImage targetImage

let copyTriCels x y rowDest (sourceImage : MagickImage) (targetImage : MagickImage) metrics =
    let width = metrics.WidthSource
    let height = metrics.HeightSource
    let widthDest = metrics.WidthDest
    let heightDest = metrics.HeightDest
    let yOffset = metrics.YOffset
    flip width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 00) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 01) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 2)) (height * y) widthDest heightDest (widthDest * 02) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 03) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 04) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 2)) (height * y) widthDest heightDest (widthDest * 05) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 06) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 07) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 2)) (height * y) widthDest heightDest (widthDest * 08) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 09) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 10) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 2)) (height * y) widthDest heightDest (widthDest * 11) (yOffset + heightDest * rowDest) sourceImage targetImage

let copyBiCels x y rowDest (sourceImage : MagickImage) (targetImage : MagickImage) metrics =
    let width = metrics.WidthSource
    let height = metrics.HeightSource
    let widthDest = metrics.WidthDest
    let heightDest = metrics.HeightDest
    let yOffset = metrics.YOffset
    flip width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 0) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 1) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 2) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 3) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 4) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 5) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) widthDest heightDest (widthDest * 6) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) widthDest heightDest (widthDest * 7) (yOffset + heightDest * rowDest) sourceImage targetImage

let copyMonoCels x y rowDest (sourceImage : MagickImage) (targetImage : MagickImage) metrics =
    let width = metrics.WidthSource
    let height = metrics.HeightSource
    let widthDest = metrics.WidthDest
    let heightDest = metrics.HeightDest
    let yOffset = metrics.YOffset
    flip width height (width * x) (height * y) widthDest heightDest (widthDest * 0) (yOffset + heightDest * rowDest) sourceImage targetImage
    flip width height (width * x) (height * y) widthDest heightDest (widthDest * 1) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * x) (height * y) widthDest heightDest (widthDest * 2) (yOffset + heightDest * rowDest) sourceImage targetImage
    copy width height (width * x) (height * y) widthDest heightDest (widthDest * 3) (yOffset + heightDest * rowDest) sourceImage targetImage

let copyCel x y rowDest (sourceImage : MagickImage) (targetImage : MagickImage) metrics =
    let width = metrics.WidthSource
    let height = metrics.HeightSource
    let widthDest = metrics.WidthDest
    let heightDest = metrics.HeightDest
    copy width height (width * x) (height * y) widthDest heightDest (widthDest * 0) (heightDest * rowDest) sourceImage targetImage

let flipCel x y rowDest (sourceImage : MagickImage) (targetImage : MagickImage) metrics =
    let width = metrics.WidthSource
    let height = metrics.HeightSource
    let widthDest = metrics.WidthDest
    let heightDest = metrics.HeightDest
    flip width height (width * x) (height * y) widthDest heightDest (widthDest * 0) (heightDest * rowDest) sourceImage targetImage

let generateBattlerAnimationSheet (sourceFilePath : string) (sourceFilePath2Opt : string option) (targetFilePath : string) =
    use stream = File.OpenWrite targetFilePath
    use sourceImage = new MagickImage (sourceFilePath)
    use targetImage = new MagickImage (MagickColor.FromRgba (byte 0, byte 0, byte 0, byte 0), 1024, 1024)
    match sourceFilePath2Opt with
    | Some sourceFilePath2 ->
        let metrics = { WidthSource = 26; HeightSource = 36; WidthDest = 48; HeightDest = 48; XOffset = 0; YOffset = 0 }
        use sourceImage2 = new MagickImage (sourceFilePath2)
        copyWalkCels sourceImage2 targetImage metrics // walk
        copyIdleCels 10 sourceImage2 targetImage metrics // idle
    | None -> ()
    let metrics = { WidthSource = 48; HeightSource = 48; WidthDest = 48; HeightDest = 48; XOffset = 0; YOffset = 3 }
    copyTriCelsToQuadCels 6 1 1 sourceImage targetImage metrics // celebrate
    copyTriCelsToQuadCels 0 2 2 sourceImage targetImage metrics // charging
    copyTriCelsToQuadCels 0 1 3 sourceImage targetImage metrics // poised
    copyTriCelsToQuadCels 6 3 4 sourceImage targetImage metrics // tired
    copyTriCels 1 0 5 sourceImage targetImage metrics // ready
    copyTriCels 3 0 6 sourceImage targetImage metrics // attack
    copyBiCels 0 5 7 sourceImage targetImage metrics // casting
    copyMonoCels 0 4 8 sourceImage targetImage metrics // damage
    copyMonoCels 3 5 9 sourceImage targetImage metrics // defend
    copyMonoCels 6 5 11 sourceImage targetImage { metrics with YOffset = 0 } // dead
    targetImage.Write (stream, MagickFormat.Png32)

let generateWalkingOnlyAnimationSheet widthSource heightSource widthDest heightDest sheetWidth sheetHeight (sourceFilePath : string) (targetFilePath : string) =
    use stream = File.OpenWrite targetFilePath
    use sourceImage = new MagickImage (sourceFilePath)
    use targetImage = new MagickImage (MagickColor.FromRgba (byte 0, byte 0, byte 0, byte 0), sheetWidth, sheetHeight)
    let metrics = { WidthSource = widthSource; HeightSource = heightSource; WidthDest = widthDest; HeightDest = heightDest; XOffset = 0; YOffset = 0 }
    copyWalkCels sourceImage targetImage metrics // walk
    copyTriCelsToQuadCels 0 0 1 sourceImage targetImage metrics // celebrate
    copyTriCelsToQuadCels 0 0 2 sourceImage targetImage metrics // charging
    copyTriCelsToQuadCels 0 1 3 sourceImage targetImage metrics // poised
    copyTriCelsToQuadCels 0 1 4 sourceImage targetImage metrics // tired
    copyTriCels 0 0 5 sourceImage targetImage metrics // ready
    copyTriCels 0 0 6 sourceImage targetImage metrics // attack
    copyBiCels 0 0 7 sourceImage targetImage metrics // casting
    copyMonoCels 0 3 8 sourceImage targetImage metrics // damage
    copyMonoCels 0 3 9 sourceImage targetImage metrics // defend
    copyIdleCels 10 sourceImage targetImage metrics // idle
    copyMonoCels 0 0 11 sourceImage targetImage metrics // dead
    targetImage.Write (stream, MagickFormat.Png32)

// generate ally characters
Directory.CreateDirectory "../../Art/Allies/Out"
generateBattlerAnimationSheet "../../Art/Allies/Assassin.png" (Some "../../Art/Allies/AssassinWalk.png") "../../Art/Allies/Out/Assassin.png"
generateBattlerAnimationSheet "../../Art/Allies/Bishop.png" (Some "../../Art/Allies/BishopWalk.png") "../../Art/Allies/Out/Bishop.png"
generateBattlerAnimationSheet "../../Art/Allies/Cleric.png" (Some "../../Art/Allies/ClericWalk.png") "../../Art/Allies/Out/Cleric.png"
generateBattlerAnimationSheet "../../Art/Allies/DarkPriestess.png" (Some "../../Art/Allies/DarkPriestessWalk.png") "../../Art/Allies/Out/DarkPriestess.png"
generateBattlerAnimationSheet "../../Art/Allies/Dwarf.png" (Some "../../Art/Allies/DwarfWalk.png") "../../Art/Allies/Out/Dwarf.png"
generateBattlerAnimationSheet "../../Art/Allies/Elf.png" (Some "../../Art/Allies/ElfWalk.png") "../../Art/Allies/Out/Elf.png"
generateBattlerAnimationSheet "../../Art/Allies/HeroAngel.png" (Some "../../Art/Allies/HeroAngelWalk.png") "../../Art/Allies/Out/HeroAngel.png"
generateBattlerAnimationSheet "../../Art/Allies/HeroBlue.png" (Some "../../Art/Allies/HeroBlueWalk.png") "../../Art/Allies/Out/HeroBlue.png"
generateBattlerAnimationSheet "../../Art/Allies/HeroGold.png" (Some "../../Art/Allies/HeroGoldWalk.png") "../../Art/Allies/Out/HeroGold.png"
generateBattlerAnimationSheet "../../Art/Allies/HeroGray.png" (Some "../../Art/Allies/HeroGrayWalk.png") "../../Art/Allies/Out/HeroGray.png"
generateBattlerAnimationSheet "../../Art/Allies/HeroIce.png" (Some "../../Art/Allies/HeroIceWalk.png") "../../Art/Allies/Out/HeroIce.png"
generateBattlerAnimationSheet "../../Art/Allies/HeroRed.png" (Some "../../Art/Allies/HeroRedWalk.png") "../../Art/Allies/Out/HeroRed.png"
generateBattlerAnimationSheet "../../Art/Allies/HeroSilver.png" (Some "../../Art/Allies/HeroSilverWalk.png") "../../Art/Allies/Out/HeroSilver.png"
generateBattlerAnimationSheet "../../Art/Allies/Mercenary.png" (Some "../../Art/Allies/MercenaryWalk.png") "../../Art/Allies/Out/Mercenary.png"
generateBattlerAnimationSheet "../../Art/Allies/Ninja.png" (Some "../../Art/Allies/NinjaWalk.png") "../../Art/Allies/Out/Ninja.png"
generateBattlerAnimationSheet "../../Art/Allies/Prince.png" (Some "../../Art/Allies/PrinceWalk.png") "../../Art/Allies/Out/Prince.png"

// generate walking enemy characters
Directory.CreateDirectory "../../Art/Enemies/Walking/Out"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Bedeviler.png" (Some "../../Art/Enemies/Walking/BedevilerWalk.png") "../../Art/Enemies/Walking/Out/Bedeviler.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Cloak.png" (Some "../../Art/Enemies/Walking/CloakWalk.png") "../../Art/Enemies/Walking/Out/Cloak.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Dwarf.png" (Some "../../Art/Enemies/Walking/DwarfWalk.png") "../../Art/Enemies/Walking/Out/Dwarf.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Birdman.png" (Some "../../Art/Enemies/Walking/BirdmanWalk.png") "../../Art/Enemies/Walking/Out/Birdman.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Foxess.png" (Some "../../Art/Enemies/Walking/FoxessWalk.png") "../../Art/Enemies/Walking/Out/Foxess.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/General.png" (Some "../../Art/Enemies/Walking/GeneralWalk.png") "../../Art/Enemies/Walking/Out/General.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Goblin.png" (Some "../../Art/Enemies/Walking/GoblinWalk.png") "../../Art/Enemies/Walking/Out/Goblin.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Knight.png" (Some "../../Art/Enemies/Walking/KnightWalk.png") "../../Art/Enemies/Walking/Out/Knight.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Legionnaire.png" (Some "../../Art/Enemies/Walking/LegionnaireWalk.png") "../../Art/Enemies/Walking/Out/Legionnaire.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Mage.png" (Some "../../Art/Enemies/Walking/MageWalk.png") "../../Art/Enemies/Walking/Out/Mage.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Magician.png" (Some "../../Art/Enemies/Walking/MagicianWalk.png") "../../Art/Enemies/Walking/Out/Magician.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Merman.png" (Some "../../Art/Enemies/Walking/MermanWalk.png") "../../Art/Enemies/Walking/Out/Merman.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Monk.png" (Some "../../Art/Enemies/Walking/MonkWalk.png") "../../Art/Enemies/Walking/Out/Monk.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Poisoner.png" (Some "../../Art/Enemies/Walking/PoisonerWalk.png") "../../Art/Enemies/Walking/Out/Poisoner.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Samurai.png" (Some "../../Art/Enemies/Walking/SamuraiWalk.png") "../../Art/Enemies/Walking/Out/Samurai.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Samurai2.png" (Some "../../Art/Enemies/Walking/Samurai2Walk.png") "../../Art/Enemies/Walking/Out/Samurai2.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Soldier.png" (Some "../../Art/Enemies/Walking/SoldierWalk.png") "../../Art/Enemies/Walking/Out/Soldier.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Thief.png" (Some "../../Art/Enemies/Walking/ThiefWalk.png") "../../Art/Enemies/Walking/Out/Thief.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Traveler.png" (Some "../../Art/Enemies/Walking/TravelerWalk.png") "../../Art/Enemies/Walking/Out/Traveler.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Trixter.png" (Some "../../Art/Enemies/Walking/TrixterWalk.png") "../../Art/Enemies/Walking/Out/Trixter.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Vampire.png" (Some "../../Art/Enemies/Walking/VampireWalk.png") "../../Art/Enemies/Walking/Out/Vampire.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Witch.png" (Some "../../Art/Enemies/Walking/WitchWalk.png") "../../Art/Enemies/Walking/Out/Witch.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Wolfman.png" (Some "../../Art/Enemies/Walking/WolfmanWalk.png") "../../Art/Enemies/Walking/Out/Wolfman.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/BlackKnight.png" (Some "../../Art/Enemies/Walking/BlackKnightWalk.png") "../../Art/Enemies/Walking/Out/BlackKnight.png"
generateBattlerAnimationSheet "../../Art/Enemies/Walking/Kyla.png" (Some "../../Art/Enemies/Walking/KylaWalk.png") "../../Art/Enemies/Walking/Out/Kyla.png"

// generate non-walking enemy characters
Directory.CreateDirectory "../../Art/Enemies/NonWalking/Out"
generateBattlerAnimationSheet "../../Art/Enemies/NonWalking/Feral.png" None "../../Art/Enemies/NonWalking/Out/Feral.png"
generateBattlerAnimationSheet "../../Art/Enemies/NonWalking/DuneBrigand.png" None "../../Art/Enemies/NonWalking/Out/DuneBrigand.png"
generateBattlerAnimationSheet "../../Art/Enemies/NonWalking/Sage.png" None "../../Art/Enemies/NonWalking/Out/Sage.png"
generateBattlerAnimationSheet "../../Art/Enemies/NonWalking/Splittah.png" None "../../Art/Enemies/NonWalking/Out/Splittah.png"

// generate walking-only enemy characters
Directory.CreateDirectory "../../Art/Enemies/WalkingOnly/Out"
generateWalkingOnlyAnimationSheet 48 48 48 48 1024 1024 "../../Art/Enemies/WalkingOnly/144x192/Hawk.png" "../../Art/Enemies/WalkingOnly/Out/Hawk.png"
generateWalkingOnlyAnimationSheet 52 53 48 48 1024 1024 "../../Art/Enemies/WalkingOnly/156x212/Gorgon.png" "../../Art/Enemies/WalkingOnly/Out/Gorgon.png"
generateWalkingOnlyAnimationSheet 60 64 48 48 1024 1024 "../../Art/Enemies/WalkingOnly/180x256/Apparition.png" "../../Art/Enemies/WalkingOnly/Out/Apparition.png"
generateWalkingOnlyAnimationSheet 60 64 48 48 1024 1024 "../../Art/Enemies/WalkingOnly/180x256/Bat.png" "../../Art/Enemies/WalkingOnly/Out/Bat.png"
generateWalkingOnlyAnimationSheet 60 64 48 48 1024 1024 "../../Art/Enemies/WalkingOnly/180x256/Snake.png" "../../Art/Enemies/WalkingOnly/Out/Snake.png"
generateWalkingOnlyAnimationSheet 47 50 48 48 1024 1024 "../../Art/Enemies/WalkingOnly/141x200/Armoros.png" "../../Art/Enemies/WalkingOnly/Out/Armoros.png"
generateWalkingOnlyAnimationSheet 47 50 48 48 1024 1024 "../../Art/Enemies/WalkingOnly/141x200/HeavyArmoros.png" "../../Art/Enemies/WalkingOnly/Out/HeavyArmoros.png"
generateWalkingOnlyAnimationSheet 47 50 48 48 1024 1024 "../../Art/Enemies/WalkingOnly/141x200/Minotaur.png" "../../Art/Enemies/WalkingOnly/Out/Minotaur.png"
generateWalkingOnlyAnimationSheet 108 92 108 92 2048 2048 "../../Art/Enemies/WalkingOnly/324x368/Arachnos.png" "../../Art/Enemies/WalkingOnly/Out/Arachnos.png"
generateWalkingOnlyAnimationSheet 108 92 108 92 2048 2048 "../../Art/Enemies/WalkingOnly/324x368/Chimera.png" "../../Art/Enemies/WalkingOnly/Out/Chimera.png"
generateWalkingOnlyAnimationSheet 108 92 108 92 2048 2048 "../../Art/Enemies/WalkingOnly/324x368/Pharoah.png" "../../Art/Enemies/WalkingOnly/Out/Pharoah.png"
generateWalkingOnlyAnimationSheet 108 92 108 92 2048 2048 "../../Art/Enemies/WalkingOnly/324x368/Tiamat.png" "../../Art/Enemies/WalkingOnly/Out/Tiamat.png"