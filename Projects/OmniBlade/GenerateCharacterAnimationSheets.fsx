// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

#I __SOURCE_DIRECTORY__
#r "../../packages/Magick.NET-Q8-x64.7.5.0.1/lib/net40/Magick.NET-Q8-x64.dll"
#r "../../packages/TiledSharp.1.0.1/lib/netstandard2.0/TiledSharp.dll"
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../packages/FParsec.1.0.3/lib/net40-client/FParsec.dll"
#r "../../packages/FsCheck.2.11.0/lib/net452/FsCheck.dll"
#r "../../packages/FsCheck.Xunit.2.11.0/lib/net452/FsCheck.Xunit.dll"
#r "../../packages/Prime.6.9.0/lib/net472/Prime.dll"
#r "../../packages/Prime.Scripting.6.1.0/lib/net472/Prime.Scripting.exe"
#r "../../packages/FSharpx.Core.1.8.32/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharpx.Collections.2.1.3/lib/net45/FSharpx.Collections.dll"
#r "../../packages/Aether.Physics2D.1.5.0/lib/net40/Aether.Physics2D.dll"
#r "../../packages/Nito.Collections.Deque.1.1.0/lib/netstandard2.0/Nito.Collections.Deque.dll"
#r "../../packages/SDL2-CS.dll.2.0.0.0/lib/net20/SDL2-CS.dll"
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

let copy (w : int) (h : int) (x : int) (y : int) (x2 : int) (y2 : int) (sourceImage : MagickImage) (targetImage : MagickImage) =
    targetImage.CopyPixels (sourceImage, MagickGeometry (new Rectangle (x, y, w, h)), x2, y2)

let flip (w : int) (h : int) (x : int) (y : int) (x2 : int) (y2 : int) (sourceImage : MagickImage) (targetImage : MagickImage) =
    use tempImage = new MagickImage (MagickColor.FromRgba (byte 0, byte 0, byte 0, byte 0), w, h)
    tempImage.CopyPixels (sourceImage, MagickGeometry (new Rectangle (x, y, w, h)), 0, 0)
    tempImage.Flop ()
    targetImage.CopyPixels (tempImage, MagickGeometry (new Rectangle (0, 0, w, h)), x2, y2)

let copyWalkCels (sourceImage : MagickImage) (targetImage : MagickImage) =
    let width = 26
    let height = 36
    let width' = 48
    let height' = 48
    let xOffset = 11
    let yOffset = 12
    copy width height (width * 0) (height * 3) (xOffset + width' * 0) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 1) (height * 3) (xOffset + width' * 1) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 2) (height * 3) (xOffset + width' * 2) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 1) (height * 3) (xOffset + width' * 3) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 0) (height * 2) (xOffset + width' * 4) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 1) (height * 2) (xOffset + width' * 5) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 2) (height * 2) (xOffset + width' * 6) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 1) (height * 2) (xOffset + width' * 7) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 0) (height * 0) (xOffset + width' * 8) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 1) (height * 0) (xOffset + width' * 9) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 2) (height * 0) (xOffset + width' * 10) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 1) (height * 0) (xOffset + width' * 11) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 0) (height * 1) (xOffset + width' * 12) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 1) (height * 1) (xOffset + width' * 13) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 2) (height * 1) (xOffset + width' * 14) (yOffset + height' * 0) sourceImage targetImage
    copy width height (width * 1) (height * 1) (xOffset + width' * 15) (yOffset + height' * 0) sourceImage targetImage

let copyIdleCels r (sourceImage : MagickImage) (targetImage : MagickImage) =
    let width = 26
    let height = 36
    let width' = 48
    let height' = 48
    let xOffset = 11
    let yOffset = 12
    copy width height (width * 1) (height * 3) (xOffset + width' * 0) (yOffset + height' * r) sourceImage targetImage
    copy width height (width * 1) (height * 2) (xOffset + width' * 1) (yOffset + height' * r) sourceImage targetImage
    copy width height (width * 1) (height * 0) (xOffset + width' * 2) (yOffset + height' * r) sourceImage targetImage
    copy width height (width * 1) (height * 1) (xOffset + width' * 3) (yOffset + height' * r) sourceImage targetImage

let copyTriCelsToQuadCels x y r (sourceImage : MagickImage) (targetImage : MagickImage) =
    let width = 48
    let height = 48
    let yOffset = 3
    flip width height (width * (x + 0)) (height * y) (width * 00) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) (width * 01) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 2)) (height * y) (width * 02) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) (width * 03) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 0)) (height * y) (width * 04) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) (width * 05) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 2)) (height * y) (width * 06) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) (width * 07) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) (width * 08) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) (width * 09) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 2)) (height * y) (width * 10) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) (width * 11) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) (width * 12) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) (width * 13) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 2)) (height * y) (width * 14) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) (width * 15) (yOffset + height * r) sourceImage targetImage

let copyTriCels x y r (sourceImage : MagickImage) (targetImage : MagickImage) =
    let width = 48
    let height = 48
    let yOffset = 3
    flip width height (width * (x + 0)) (height * y) (width * 00) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) (width * 01) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 2)) (height * y) (width * 02) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 0)) (height * y) (width * 03) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) (width * 04) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 2)) (height * y) (width * 05) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) (width * 06) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) (width * 07) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 2)) (height * y) (width * 08) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) (width * 09) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) (width * 10) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 2)) (height * y) (width * 11) (yOffset + height * r) sourceImage targetImage

let copyBiCels x y r (sourceImage : MagickImage) (targetImage : MagickImage) =
    let width = 48
    let height = 48
    let yOffset = 3
    flip width height (width * (x + 0)) (height * y) (width * 0) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) (width * 1) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 0)) (height * y) (width * 2) (yOffset + height * r) sourceImage targetImage
    flip width height (width * (x + 1)) (height * y) (width * 3) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) (width * 4) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) (width * 5) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 0)) (height * y) (width * 6) (yOffset + height * r) sourceImage targetImage
    copy width height (width * (x + 1)) (height * y) (width * 7) (yOffset + height * r) sourceImage targetImage

let copyCels x y r (sourceImage : MagickImage) (targetImage : MagickImage) =
    let width = 48
    let height = 48
    let yOffset = 3
    flip width height (width * x) (height * y) (width * 0) (yOffset + height * r) sourceImage targetImage
    flip width height (width * x) (height * y) (width * 1) (yOffset + height * r) sourceImage targetImage
    copy width height (width * x) (height * y) (width * 2) (yOffset + height * r) sourceImage targetImage
    copy width height (width * x) (height * y) (width * 3) (yOffset + height * r) sourceImage targetImage

let copyCel x y r (sourceImage : MagickImage) (targetImage : MagickImage) =
    let width = 48
    let height = 48
    copy width height (width * x) (height * y) (width * 0) (height * r) sourceImage targetImage

let generateCharacterAnimationSheet (sourceFilePath : string) (sourceFilePath2Opt : string option) (targetFilePath : string) =
    use stream = File.OpenWrite targetFilePath
    use sourceImage = new MagickImage (sourceFilePath)
    use targetImage = new MagickImage (MagickColor.FromRgba (byte 0, byte 0, byte 0, byte 0), 1024, 1024)
    match sourceFilePath2Opt with
    | Some sourceFilePath2 ->
        use sourceImage2 = new MagickImage (sourceFilePath2)
        copyWalkCels sourceImage2 targetImage // walk
        copyIdleCels 10 sourceImage2 targetImage // idle
    | None -> ()
    copyTriCelsToQuadCels 6 1 1 sourceImage targetImage // celebrate
    copyTriCelsToQuadCels 0 2 2 sourceImage targetImage // charging
    copyTriCelsToQuadCels 0 1 3 sourceImage targetImage // poised
    copyTriCelsToQuadCels 6 3 4 sourceImage targetImage // tired
    copyTriCels 1 0 5 sourceImage targetImage // ready
    copyTriCels 3 0 6 sourceImage targetImage // attack
    copyBiCels 0 5 7 sourceImage targetImage // casting
    copyCels 0 4 8 sourceImage targetImage // damage
    copyCels 3 5 9 sourceImage targetImage // defend
    copyCel 6 5 11 sourceImage targetImage // dead
    targetImage.Write (stream, MagickFormat.Png32)

// generate ally characters
Directory.CreateDirectory "../../Art/Allies/Out"
generateCharacterAnimationSheet "../../Art/Allies/Assassin.png" (Some "../../Art/Allies/AssassinWalk.png") "../../Art/Allies/Out/Assassin.png"
generateCharacterAnimationSheet "../../Art/Allies/Bishop.png" (Some "../../Art/Allies/BishopWalk.png") "../../Art/Allies/Out/Bishop.png"
generateCharacterAnimationSheet "../../Art/Allies/Cleric.png" (Some "../../Art/Allies/ClericWalk.png") "../../Art/Allies/Out/Cleric.png"
generateCharacterAnimationSheet "../../Art/Allies/DarkPriestess.png" (Some "../../Art/Allies/DarkPriestessWalk.png") "../../Art/Allies/Out/DarkPriestess.png"
generateCharacterAnimationSheet "../../Art/Allies/Dwarf.png" (Some "../../Art/Allies/DwarfWalk.png") "../../Art/Allies/Out/Dwarf.png"
generateCharacterAnimationSheet "../../Art/Allies/Elf.png" (Some "../../Art/Allies/ElfWalk.png") "../../Art/Allies/Out/Elf.png"
generateCharacterAnimationSheet "../../Art/Allies/HeroAngel.png" (Some "../../Art/Allies/HeroAngelWalk.png") "../../Art/Allies/Out/HeroAngel.png"
generateCharacterAnimationSheet "../../Art/Allies/HeroBlue.png" (Some "../../Art/Allies/HeroBlueWalk.png") "../../Art/Allies/Out/HeroBlue.png"
generateCharacterAnimationSheet "../../Art/Allies/HeroGold.png" (Some "../../Art/Allies/HeroGoldWalk.png") "../../Art/Allies/Out/HeroGold.png"
generateCharacterAnimationSheet "../../Art/Allies/HeroGray.png" (Some "../../Art/Allies/HeroGrayWalk.png") "../../Art/Allies/Out/HeroGray.png"
generateCharacterAnimationSheet "../../Art/Allies/HeroIce.png" (Some "../../Art/Allies/HeroIceWalk.png") "../../Art/Allies/Out/HeroIce.png"
generateCharacterAnimationSheet "../../Art/Allies/HeroRed.png" (Some "../../Art/Allies/HeroRedWalk.png") "../../Art/Allies/Out/HeroRed.png"
generateCharacterAnimationSheet "../../Art/Allies/HeroSilver.png" (Some "../../Art/Allies/HeroSilverWalk.png") "../../Art/Allies/Out/HeroSilver.png"
generateCharacterAnimationSheet "../../Art/Allies/Mercenary.png" (Some "../../Art/Allies/MercenaryWalk.png") "../../Art/Allies/Out/Mercenary.png"
generateCharacterAnimationSheet "../../Art/Allies/Prince.png" (Some "../../Art/Allies/PrinceWalk.png") "../../Art/Allies/Out/Prince.png"
generateCharacterAnimationSheet "../../Art/Allies/Thief.png" (Some "../../Art/Allies/ThiefWalk.png") "../../Art/Allies/Out/Thief.png"

// generate walking enemy characters
Directory.CreateDirectory "../../Art/Enemies/Walking/Out"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Bedeviler.png" (Some "../../Art/Enemies/Walking/BedevilerWalk.png") "../../Art/Enemies/Walking/Out/Bedeviler.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Cloak.png" (Some "../../Art/Enemies/Walking/CloakWalk.png") "../../Art/Enemies/Walking/Out/Cloak.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Dwarf.png" (Some "../../Art/Enemies/Walking/DwarfWalk.png") "../../Art/Enemies/Walking/Out/Dwarf.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Elkman.png" (Some "../../Art/Enemies/Walking/ElkmanWalk.png") "../../Art/Enemies/Walking/Out/Elkman.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Foxess.png" (Some "../../Art/Enemies/Walking/FoxessWalk.png") "../../Art/Enemies/Walking/Out/Foxess.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/General.png" (Some "../../Art/Enemies/Walking/GeneralWalk.png") "../../Art/Enemies/Walking/Out/General.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Goblin.png" (Some "../../Art/Enemies/Walking/GoblinWalk.png") "../../Art/Enemies/Walking/Out/Goblin.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Knight.png" (Some "../../Art/Enemies/Walking/KnightWalk.png") "../../Art/Enemies/Walking/Out/Knight.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Legionnaire.png" (Some "../../Art/Enemies/Walking/LegionnaireWalk.png") "../../Art/Enemies/Walking/Out/Legionnaire.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Mage.png" (Some "../../Art/Enemies/Walking/MageWalk.png") "../../Art/Enemies/Walking/Out/Mage.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Magician.png" (Some "../../Art/Enemies/Walking/MagicianWalk.png") "../../Art/Enemies/Walking/Out/Magician.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Merman.png" (Some "../../Art/Enemies/Walking/MermanWalk.png") "../../Art/Enemies/Walking/Out/Merman.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Monk.png" (Some "../../Art/Enemies/Walking/MonkWalk.png") "../../Art/Enemies/Walking/Out/Monk.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Ninja.png" (Some "../../Art/Enemies/Walking/NinjaWalk.png") "../../Art/Enemies/Walking/Out/Ninja.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Poisoner.png" (Some "../../Art/Enemies/Walking/PoisonerWalk.png") "../../Art/Enemies/Walking/Out/Poisoner.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Samurai.png" (Some "../../Art/Enemies/Walking/SamuraiWalk.png") "../../Art/Enemies/Walking/Out/Samurai.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Samurai2.png" (Some "../../Art/Enemies/Walking/Samurai2Walk.png") "../../Art/Enemies/Walking/Out/Samurai2.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Soldier.png" (Some "../../Art/Enemies/Walking/SoldierWalk.png") "../../Art/Enemies/Walking/Out/Soldier.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Traveler.png" (Some "../../Art/Enemies/Walking/TravelerWalk.png") "../../Art/Enemies/Walking/Out/Traveler.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Trixter.png" (Some "../../Art/Enemies/Walking/TrixterWalk.png") "../../Art/Enemies/Walking/Out/Trixter.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Vampire.png" (Some "../../Art/Enemies/Walking/VampireWalk.png") "../../Art/Enemies/Walking/Out/Vampire.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Witch.png" (Some "../../Art/Enemies/Walking/WitchWalk.png") "../../Art/Enemies/Walking/Out/Witch.png"
generateCharacterAnimationSheet "../../Art/Enemies/Walking/Wolfman.png" (Some "../../Art/Enemies/Walking/WolfmanWalk.png") "../../Art/Enemies/Walking/Out/Wolfman.png"

// generate non-walking enemy characters
Directory.CreateDirectory "../../Art/Enemies/NonWalking/Out"
generateCharacterAnimationSheet "../../Art/Enemies/NonWalking/BeastMaster.png" None "../../Art/Enemies/NonWalking/Out/BeastMaster.png"
generateCharacterAnimationSheet "../../Art/Enemies/NonWalking/DuneBrigand.png" None "../../Art/Enemies/NonWalking/Out/DuneBrigand.png"
generateCharacterAnimationSheet "../../Art/Enemies/NonWalking/Sage.png" None "../../Art/Enemies/NonWalking/Out/Sage.png"
