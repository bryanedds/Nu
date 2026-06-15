#!/usr/bin/env -S dotnet fsi
// Generate platform-specific desktop app icons from MAUI adaptive icon SVG sources.
// These SVGs are the SINGLE SOURCE OF TRUTH across all platforms.
//
// Usage:
//   dotnet fsi GenerateIcons.fsx macos   <bg.svg> <fg.svg> <output_dir>
//   dotnet fsi GenerateIcons.fsx windows <bg.svg> <fg.svg> <output.ico>
//   dotnet fsi GenerateIcons.fsx linux   <bg.svg> <fg.svg> <output_dir>
//
// Dependencies (resolved via NuGet automatically):
//   Magick.NET-Q8-AnyCPU (SVG rendering + image resizing)

#r "nuget: Magick.NET-Q8-AnyCPU, 14.5.0"

open System
open System.IO
open System.Diagnostics
open ImageMagick

// ---- CLI ----

let usage () =
    eprintfn """Usage: dotnet fsi GenerateIcons.fsx <platform> <bg.svg> <fg.svg> <output_dir>
  platform: macos | windows | linux
  bg.svg  : path to the background SVG (MAUI adaptive icon background layer)
  fg.svg  : path to the foreground SVG (MAUI adaptive icon foreground layer)
  output  : output directory (macOS/linux) or .ico file path (Windows)
The SVGs in App/ are the single source of truth for the app icon design."""

// ---- ICO binary writer (PNG-encoded entries, Vista+) ----

let writeIco (pngDataList : (int * byte[]) list) (path : string) =
    use fs = new FileStream (path, FileMode.Create, FileAccess.Write)
    use bw = new BinaryWriter (fs)
    // ICO header: reserved=0, type=1 (ICO), count
    bw.Write (uint16 0)
    bw.Write (uint16 1)
    bw.Write (uint16 pngDataList.Length)
    // compute offsets: header (6) + directory (16 * count) then each image
    let mutable offset = 6 + 16 * pngDataList.Length
    for size, data in pngDataList do
        let w = if size >= 256 then 0uy else byte size
        let h = if size >= 256 then 0uy else byte size
        bw.Write w       // width
        bw.Write h       // height
        bw.Write (0uy)   // palette
        bw.Write (0uy)   // reserved
        bw.Write (uint16 1)  // color planes
        bw.Write (uint16 32) // bpp
        bw.Write (uint32 data.Length)  // image size
        bw.Write (uint32 offset)       // image offset
        offset <- offset + data.Length
    for _, data in pngDataList do
        bw.Write data

// ---- macOS .icns via iconutil ----

let writeIcns (png1024Path : string) (outputDir : string) =
    let iconsetDir = Path.Combine (outputDir, "AppIcon.iconset")
    if Directory.Exists iconsetDir then Directory.Delete (iconsetDir, true)
    Directory.CreateDirectory iconsetDir |> ignore

    // size list: (width, height, basenameSuffix)
    let sizes = [
        16, 16, "16x16"
        32, 32, "16x16@2x"
        32, 32, "32x32"
        64, 64, "32x32@2x"
        128, 128, "128x128"
        256, 256, "128x128@2x"
        256, 256, "256x256"
        512, 512, "256x256@2x"
        512, 512, "512x512"
        1024, 1024, "512x512@2x"
    ]
    use src = new MagickImage (png1024Path)
    src.Format <- MagickFormat.Png32
    for w, h, suffix in sizes do
        use resized = src.Clone ()
        resized.Resize (uint32 w, uint32 h)
        resized.Write (Path.Combine (iconsetDir, $"icon_{suffix}.png"))

    let icnsPath = Path.Combine (outputDir, "AppIcon.icns")
    let psi = ProcessStartInfo ("iconutil", $"-c icns \"{iconsetDir}\" -o \"{icnsPath}\"")
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    use p = Process.Start psi
    p.WaitForExit ()
    if p.ExitCode <> 0 then
        failwithf "iconutil failed: %s" (p.StandardError.ReadToEnd())
    Directory.Delete (iconsetDir, true)
    icnsPath

// ---- Linux icon (.png at 512px) ----

let writeLinuxPng (png1024Path : string) (outputDir : string) =
    Directory.CreateDirectory outputDir |> ignore
    use src = new MagickImage (png1024Path)
    src.Resize (512u, 512u)
    src.Format <- MagickFormat.Png32
    let pngPath = Path.Combine (outputDir, "app.png")
    src.Write pngPath
    pngPath

// ---- Windows .ico ----

let writeWindowsIco (png1024Path : string) (outputIco : string) =
    Directory.CreateDirectory (Path.GetDirectoryName outputIco) |> ignore
    use src = new MagickImage (png1024Path)
    src.Format <- MagickFormat.Png32
    let sizes = [16; 24; 32; 48; 64; 128; 256]
    let pngEntries =
        [ for s in sizes do
            use resized = src.Clone ()
            resized.Resize (uint32 s, uint32 s)
            use ms = new MemoryStream ()
            resized.Write (ms)
            s, ms.ToArray () ]
    writeIco pngEntries outputIco
    outputIco

// ---- Main compositing ----

let compositeIcon (bgSvgPath : string) (fgSvgPath : string) (outputPngPath : string) =
    let readSettings =
        MagickReadSettings (BackgroundColor = MagickColors.Transparent, Width = 1024u, Height = 1024u) // read as 1024 x 1024 for high DPI output.
    use bg = new MagickImage (bgSvgPath, readSettings)
    use fg = new MagickImage (fgSvgPath, readSettings)
    bg.Format <- MagickFormat.Png32
    fg.Format <- MagickFormat.Png32

    // Composite foreground onto background (like MAUI adaptive icon).
    // Both are pre-composited at full size; foreground artwork is already
    // designed to sit within the MAUI safe zone (66.67% of canvas).
    bg.Composite (fg, CompositeOperator.Over)

    Directory.CreateDirectory (Path.GetDirectoryName outputPngPath) |> ignore
    bg.Write outputPngPath

// ---- Entry point ----

match fsi.CommandLineArgs |> Array.toList with
| _scriptPath :: "macos" :: bgSvg :: fgSvg :: outputDir :: _ ->
    printfn "Compositing macOS icon..."
    let compositePath = Path.Combine (outputDir, "composited_1024.png")
    compositeIcon bgSvg fgSvg compositePath
    let icnsPath = writeIcns compositePath outputDir
    printfn "macOS .icns -> %s" icnsPath

| _scriptPath :: "windows" :: bgSvg :: fgSvg :: outputIco :: _ ->
    printfn "Compositing Windows icon..."
    let compositePath = Path.GetTempFileName () + ".png"
    compositeIcon bgSvg fgSvg compositePath
    let icoPath = writeWindowsIco compositePath outputIco
    printfn "Windows .ico -> %s" icoPath

| _scriptPath :: "linux" :: bgSvg :: fgSvg :: outputDir :: _ ->
    printfn "Compositing Linux icon..."
    let compositePath = Path.Combine (outputDir, "composited_1024.png")
    compositeIcon bgSvg fgSvg compositePath
    let pngPath = writeLinuxPng compositePath outputDir
    printfn "Linux  .png -> %s" pngPath

| _ ->
    usage ()
    Environment.Exit 1
