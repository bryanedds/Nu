// Zombie Game.
// Copyright (C) Bryan Edds.
//
// Synthesizes every sound and song of the game from nothing but numbers - no samples - into Assets/Gameplay, the songs
// of the screens around play into Assets/Gui.
// The sounds are mono 16-bit wavs and the songs stereo oggs (through ffmpeg), each loop's tail wrapped onto its start
// so it repeats seamlessly. Seeded, so it makes the same files each time. Run it from anywhere:
//
//   dotnet fsi Tools/SynthesizeAudio.fsx
//
// It needs only ffmpeg beside .NET. Its random numbers are numpy's own - its PCG64 generator, seeded the same way, and
// its ziggurat for normal draws - since the sounds were first made by a numpy script, and so stay as they were made.

open System
open System.Diagnostics
open System.IO
open System.Numerics

let SR = 44100.0
let Assets = Path.Combine (__SOURCE_DIRECTORY__, "..", "Assets")
let GuiSongs = set ["TitleSong"; "EndingSong"; "GameOverSong"] // the songs of the screens around play, in the Gui package

// ---------------------------------------------------------------- waves

/// A run of samples, added, multiplied and so on sample by sample, or by a number.
type Wave (samples : float array) =
    member this.Samples = samples
    member this.Length = samples.Length
    member this.Item with get (i : int) = samples[i]
    static member (+) (a : Wave, b : Wave) = Wave (Array.map2 (+) a.Samples b.Samples)
    static member (+) (a : Wave, b : float) = Wave (Array.map (fun x -> x + b) a.Samples)
    static member (+) (a : float, b : Wave) = Wave (Array.map (fun x -> a + x) b.Samples)
    static member (-) (a : Wave, b : Wave) = Wave (Array.map2 (-) a.Samples b.Samples)
    static member (-) (a : Wave, b : float) = Wave (Array.map (fun x -> x - b) a.Samples)
    static member (-) (a : float, b : Wave) = Wave (Array.map (fun x -> a - x) b.Samples)
    static member (*) (a : Wave, b : Wave) = Wave (Array.map2 (*) a.Samples b.Samples)
    static member (*) (a : Wave, b : float) = Wave (Array.map (fun x -> x * b) a.Samples)
    static member (*) (a : float, b : Wave) = Wave (Array.map (fun x -> a * x) b.Samples)
    static member (/) (a : Wave, b : Wave) = Wave (Array.map2 (/) a.Samples b.Samples)
    static member (/) (a : Wave, b : float) = Wave (Array.map (fun x -> x / b) a.Samples)
    static member (~-) (a : Wave) = Wave (Array.map (fun x -> -x) a.Samples)

let wave (samples : float seq) = Wave (Array.ofSeq samples)
let zeros n = Wave (Array.zeroCreate n)
let full n (value : float) = Wave (Array.create n value)
let arange n = Wave (Array.init n float)
let linspace (start : float) (stop : float) n =
    if n = 1 then Wave [|start|]
    else
        let step = (stop - start) / float (n - 1)
        Wave (Array.init n (fun i -> if i = n - 1 then stop else float i * step + start))
let apply f (x : Wave) = Wave (Array.map f x.Samples)
let sinw x = apply Math.Sin x
let cosw x = apply Math.Cos x
let expw x = apply Math.Exp x
let tanhw x = apply Math.Tanh x
let absw x = apply Math.Abs x
let signw x = apply (fun (v : float) -> float (sign v)) x
let floorw x = apply Math.Floor x
let squarew x = apply (fun v -> v * v) x
let poww (x : Wave) (p : float) = apply (fun v -> Math.Pow (v, p)) x
let powBase (b : float) (x : Wave) = apply (fun v -> Math.Pow (b, v)) x
let maximumw (x : Wave) (floor : float) = apply (fun v -> max v floor) x
let clipBelow (x : Wave) (floor : float) = apply (fun v -> if v < floor then floor else v) x
let peak (x : Wave) = Array.max x.Samples
let cumsum (x : Wave) =
    let out = Array.zeroCreate x.Length
    let mutable acc = 0.0
    for i in 0 .. x.Length - 1 do
        acc <- acc + x[i]
        out[i] <- acc
    Wave out
let concat (xs : Wave list) = Wave (xs |> Seq.collect (fun x -> x.Samples) |> Array.ofSeq)
let slice (x : Wave) start stop =
    let start = max 0 (min start x.Length)
    let stop = max start (min stop x.Length)
    Wave x.Samples[start .. stop - 1]
let sumw (xs : Wave seq) = Seq.reduce (+) xs

/// The pairwise sum numpy takes of an array, so a sum of many comes out as its does.
let rec pairwiseSum (x : float array) start count =
    if count < 8 then
        let mutable s = 0.0
        for i in start .. start + count - 1 do s <- s + x[i]
        s
    elif count <= 128 then
        let r = Array.zeroCreate 8
        for j in 0 .. 7 do r[j] <- x[start + j]
        let mutable i = 8
        while i < count - count % 8 do
            for j in 0 .. 7 do r[j] <- r[j] + x[start + i + j]
            i <- i + 8
        let mutable s = ((r[0] + r[1]) + (r[2] + r[3])) + ((r[4] + r[5]) + (r[6] + r[7]))
        while i < count do
            s <- s + x[start + i]
            i <- i + 1
        s
    else
        let half = (count / 2) - (count / 2) % 8
        pairwiseSum x start half + pairwiseSum x (start + half) (count - half)

// ---------------------------------------------------------------- numpy's random numbers

#load "Ziggurat.fsx"
open Ziggurat

let private crc32Table =
    Array.init 256 (fun n ->
        let mutable c = uint32 n
        for _ in 0 .. 7 do c <- if c &&& 1u <> 0u then 0xEDB88320u ^^^ (c >>> 1) else c >>> 1
        c)

/// The CRC-32 a name's bytes come to, as zlib's - the seed of a stage's sounds.
let crc32 (text : string) =
    let mutable c = 0xFFFFFFFFu
    for b in Text.Encoding.UTF8.GetBytes text do c <- crc32Table[int ((c ^^^ uint32 b) &&& 0xFFu)] ^^^ (c >>> 8)
    c ^^^ 0xFFFFFFFFu

/// numpy's default generator - PCG64, seeded through a SeedSequence - drawing as numpy's Generator does.
type Rng (seed : uint32) =
    static let multiplier = UInt128 (2549297995355413924UL, 4865540595714422341UL)
    let mutable state = UInt128.Zero
    let mutable increment = UInt128.Zero
    let mutable hasUint32 = false
    let mutable uinteger = 0u
    let step () = state <- state * multiplier + increment
    let next64 () =
        step ()
        let high = uint64 (state >>> 64)
        let x = high ^^^ uint64 state
        let rotation = int (high >>> 58)
        (x >>> rotation) ||| (x <<< ((64 - rotation) &&& 63))
    do
        // the seed sequence's pool, its entropy hashed and mixed in
        let mutable hashConst = 0x43b0d7e5u
        let hashMix (value : uint32) =
            let mutable value = value ^^^ hashConst
            hashConst <- hashConst * 0x931e8875u
            value <- value * hashConst
            value ^^^ (value >>> 16)
        let mix (x : uint32) (y : uint32) =
            let result = 0xca01f9ddu * x - 0x4973f715u * y
            result ^^^ (result >>> 16)
        let pool = Array.init 4 (fun i -> hashMix (if i = 0 then seed else 0u))
        for source in 0 .. 3 do
            for destination in 0 .. 3 do
                if source <> destination then pool[destination] <- mix pool[destination] (hashMix pool[source])

        // the generator's state drawn from the pool
        let mutable hashConstB = 0x8b51f9ddu
        let words =
            Array.init 8 (fun i ->
                let mutable value = pool[i % 4] ^^^ hashConstB
                hashConstB <- hashConstB * 0x58f38dedu
                value <- value * hashConstB
                value ^^^ (value >>> 16))
        let words64 = Array.init 4 (fun i -> uint64 words[2 * i] ||| (uint64 words[2 * i + 1] <<< 32))
        increment <- (UInt128 (words64[2], words64[3]) <<< 1) ||| UInt128.One
        step ()
        state <- state + UInt128 (words64[0], words64[1])
        step ()
    member this.Next64 () = next64 ()
    member this.Next32 () =
        if hasUint32 then
            hasUint32 <- false
            uinteger
        else
            let next = next64 ()
            hasUint32 <- true
            uinteger <- uint32 (next >>> 32)
            uint32 next
    member this.Random () = float (next64 () >>> 11) * (1.0 / 9007199254740992.0)
    member this.Random n = Wave (Array.init n (fun _ -> this.Random ()))
    member this.Uniform (low : float, high : float) = low + (high - low) * this.Random ()
    member this.Uniform (low : float, high : float, n) = Wave (Array.init n (fun _ -> this.Uniform (low, high)))
    /// An integer from low up to but not including high, by Lemire's method on 32 bits.
    member this.Integers (low : int, high : int) =
        let range = uint32 (high - low - 1)
        if range = 0u then low
        else
            let exclusive = range + 1u
            let mutable m = uint64 (this.Next32 ()) * uint64 exclusive
            let mutable leftover = uint32 m
            if leftover < exclusive then
                let threshold = (UInt32.MaxValue - range) % exclusive
                while leftover < threshold do
                    m <- uint64 (this.Next32 ()) * uint64 exclusive
                    leftover <- uint32 m
            low + int (m >>> 32)
    member this.StandardNormal () =
        let mutable result = nan
        while Double.IsNaN result do
            let r = next64 ()
            let index = int (r &&& 0xFFUL)
            let r = r >>> 8
            let negative = r &&& 1UL <> 0UL
            let rabs = (r >>> 1) &&& 0x000fffffffffffffUL
            let x = float rabs * ZigguratW[index]
            let x = if negative then -x else x
            if rabs < ZigguratK[index] then result <- x
            elif index = 0 then
                let mutable tail = nan
                while Double.IsNaN tail do
                    let xx = -ZigguratInverseR * Math.Log (1.0 - this.Random ())
                    let yy = -Math.Log (1.0 - this.Random ())
                    if yy + yy > xx * xx then tail <- if (rabs >>> 8) &&& 1UL <> 0UL then -(ZigguratR + xx) else ZigguratR + xx
                result <- tail
            elif (ZigguratF[index - 1] - ZigguratF[index]) * this.Random () + ZigguratF[index] < Math.Exp (-0.5 * x * x) then
                result <- x
        result
    member this.StandardNormal n = Wave (Array.init n (fun _ -> this.StandardNormal ()))

let mutable rng = Rng 1996u

// ---------------------------------------------------------------- filters

type FilterType =
    | Low of float
    | High of float
    | Band of float * float

/// A Butterworth filter of an order as scipy designs it, as second order sections - each its b0, b1, b2, a1 and a2.
let butter order filterType =
    let fs = 2.0
    let warp (frequency : float) = 2.0 * fs * Math.Tan (Math.PI * (2.0 * frequency / SR) / fs)
    let prototype = [|for m in -order + 1 .. 2 .. order - 1 -> -Complex.Exp (Complex (0.0, Math.PI * float m / (2.0 * float order)))|]
    let product (xs : Complex seq) = Seq.fold (*) Complex.One xs
    let (poles, zeros, gain) =
        match filterType with
        | Low frequency ->
            let wo = warp frequency
            (prototype |> Array.map (fun p -> wo * p), [||], Math.Pow (wo, float order))
        | High frequency ->
            let wo = warp frequency
            (prototype |> Array.map (fun p -> Complex (wo, 0.0) / p), Array.create order Complex.Zero, (Complex.One / product (prototype |> Array.map (fun p -> -p))).Real)
        | Band (low, high) ->
            let (lowWarped, highWarped) = (warp low, warp high)
            let bw = highWarped - lowWarped
            let wo = Math.Sqrt (lowWarped * highWarped)
            let lowpass = prototype |> Array.map (fun p -> p * bw / 2.0)
            let root (p : Complex) = Complex.Sqrt (p * p - Complex (wo * wo, 0.0))
            (Array.append (lowpass |> Array.map (fun p -> p + root p)) (lowpass |> Array.map (fun p -> p - root p)), Array.create order Complex.Zero, Math.Pow (bw, float order))
    let fs2 = 2.0 * fs
    let digitalZeros = Array.append (zeros |> Array.map (fun z -> (fs2 + z) / (fs2 - z))) (Array.create (poles.Length - zeros.Length) (Complex (-1.0, 0.0)))
    let digitalPoles = poles |> Array.map (fun p -> (fs2 + p) / (fs2 - p))
    let digitalGain = gain * (product (zeros |> Array.map (fun z -> fs2 - z)) / product (poles |> Array.map (fun p -> fs2 - p))).Real
    let mutable zeroPool = digitalZeros |> Array.map (fun z -> z.Real) |> Array.sort |> List.ofArray // the minus ones first
    let takeZero (preferPositive : bool) =
        let pick = if preferPositive then List.last zeroPool else List.head zeroPool
        zeroPool <- (if preferPositive then List.take (zeroPool.Length - 1) zeroPool else List.tail zeroPool)
        pick
    let sections =
        [|for p in digitalPoles do
            if p.Imaginary > 1e-12 then
                let (z1, z2) = (takeZero false, takeZero true)
                [|1.0; -(z1 + z2); z1 * z2; -2.0 * p.Real; p.Real * p.Real + p.Imaginary * p.Imaginary|]
            elif abs p.Imaginary <= 1e-12 then
                let z = takeZero false
                [|1.0; -z; 0.0; -p.Real; 0.0|]|]
    for i in 0 .. 2 do sections[0][i] <- sections[0][i] * digitalGain
    sections

/// Run a signal through second order sections, each a direct form II transposed, from rest.
let sosfilt (sections : float array array) (x : Wave) =
    let out = Array.copy x.Samples
    for s in sections do
        let (b0, b1, b2, a1, a2) = (s[0], s[1], s[2], s[3], s[4])
        let mutable z0 = 0.0
        let mutable z1 = 0.0
        for n in 0 .. out.Length - 1 do
            let input = out[n]
            let output = b0 * input + z0
            z0 <- b1 * input - a1 * output + z1
            z1 <- b2 * input - a2 * output
            out[n] <- output
    Wave out

let private fftCache = Collections.Generic.Dictionary<int, float array * float array> ()

/// A complex fast Fourier transform in place, of a power of two's length - inverse unscaled.
let fft (re : float array) (im : float array) inverse =
    let n = re.Length
    let mutable j = 0
    for i in 1 .. n - 1 do
        let mutable bit = n >>> 1
        while j &&& bit <> 0 do
            j <- j ^^^ bit
            bit <- bit >>> 1
        j <- j ^^^ bit
        if i < j then
            let t = re[i] in re[i] <- re[j]; re[j] <- t
            let t = im[i] in im[i] <- im[j]; im[j] <- t
    let (cosines, sines) =
        match fftCache.TryGetValue n with
        | (true, table) -> table
        | (false, _) ->
            let table = (Array.init (n / 2) (fun k -> Math.Cos (2.0 * Math.PI * float k / float n)), Array.init (n / 2) (fun k -> Math.Sin (2.0 * Math.PI * float k / float n)))
            fftCache[n] <- table
            table
    let sign = if inverse then 1.0 else -1.0
    let mutable length = 2
    while length <= n do
        let half = length / 2
        let stride = n / length
        let mutable start = 0
        while start < n do
            for k in 0 .. half - 1 do
                let wr = cosines[k * stride]
                let wi = sign * sines[k * stride]
                let a = start + k
                let b = a + half
                let tr = re[b] * wr - im[b] * wi
                let ti = re[b] * wi + im[b] * wr
                re[b] <- re[a] - tr
                im[b] <- im[a] - ti
                re[a] <- re[a] + tr
                im[a] <- im[a] + ti
            start <- start + length
        length <- length * 2

/// The full convolution of a signal with an impulse response, added block by block through the transform.
let fftconvolve (x : Wave) (h : Wave) =
    let m = h.Length
    let mutable size = 4096
    while size < 2 * m do size <- size * 2
    let block = size - m + 1
    let (hr, hi) = (Array.zeroCreate size, Array.zeroCreate size)
    Array.blit h.Samples 0 hr 0 m
    fft hr hi false
    let out = Array.zeroCreate (x.Length + m - 1)
    let mutable start = 0
    while start < x.Length do
        let count = min block (x.Length - start)
        let (re, im) = (Array.zeroCreate size, Array.zeroCreate size)
        Array.blit x.Samples start re 0 count
        fft re im false
        for k in 0 .. size - 1 do
            let r = re[k] * hr[k] - im[k] * hi[k]
            im[k] <- re[k] * hi[k] + im[k] * hr[k]
            re[k] <- r
        fft re im true
        for k in 0 .. count + m - 2 do out[start + k] <- out[start + k] + re[k] / float size
        start <- start + block
    Wave out

// ---------------------------------------------------------------- building blocks

let tOf seconds = arange (int (seconds * SR)) / SR

let noise seconds = rng.StandardNormal (int (seconds * SR))

let filtOrder order (x : Wave) filterType = sosfilt (butter order filterType) x

let filt x filterType = filtOrder 2 x filterType

let brown seconds =
    let n = cumsum (noise seconds)
    let n = filtOrder 1 n (High 20.0)
    n / (peak (absw n) + 1e-9)

let bandpass x (center : float) (q : float) =
    let low = center / (1.0 + 1.0 / (2.0 * q))
    let high = center * (1.0 + 1.0 / (2.0 * q))
    filt x (Band (max 20.0 low, min (SR / 2.0 - 100.0) high))

/// A two-pole resonator ringing at a frequency, its ring dying in about 'decay' seconds - its gain at resonance one
/// whatever its decay, so a long ring is no quieter than a short one.
let resonate (x : Wave) (freq : float) (decay : float) =
    let r = Math.Exp (-1.0 / (decay * SR))
    let w = 2.0 * Math.PI * freq / SR
    let gain = (1.0 - r) * Math.Sqrt (1.0 - 2.0 * r * Math.Cos (2.0 * w) + r * r)
    let (a1, a2) = (-2.0 * r * Math.Cos w, r * r)
    let out = Array.zeroCreate x.Length
    let mutable z0 = 0.0
    let mutable z1 = 0.0
    for n in 0 .. x.Length - 1 do
        let y = gain * x[n] + z0
        z0 <- z1 - a1 * y
        z1 <- -a2 * y
        out[n] <- y
    Wave out

let envExp n decay = -(arange n) / (decay * SR) |> expw

let envAdsr n attack release =
    let e = Array.create n 1.0
    let a = int (attack * SR)
    let r = int (release * SR)
    if a > 0 then Array.blit (squarew (linspace 0.0 1.0 a)).Samples 0 e 0 a
    if r > 0 then
        let fall = squarew (linspace 1.0 0.0 r)
        for i in 0 .. r - 1 do e[n - r + i] <- e[n - r + i] * fall[i]
    Wave e

type Shape = Sine | Saw | Square | Tri

/// An oscillator following a frequency per sample, so sweeps and vibrato come free.
let osc freqs shape =
    let phase = 2.0 * Math.PI * cumsum freqs / SR
    let frac () = apply (fun v -> v % 1.0) (phase / (2.0 * Math.PI))
    match shape with
    | Sine -> sinw phase
    | Saw -> 2.0 * frac () - 1.0
    | Square -> signw (sinw phase)
    | Tri -> 2.0 * absw (2.0 * frac () - 1.0) - 1.0

let sweep (f0 : float) (f1 : float) seconds curve =
    let k = poww (linspace 0.0 1.0 (int (seconds * SR))) curve
    f0 * powBase (f1 / f0) k

let normalizeTo x (level : float) = x * (level / (peak (absw x) + 1e-9))

let formants x vowel =
    let table =
        match vowel with
        | "a" -> [(800.0, 1.0); (1150.0, 0.5); (2900.0, 0.2)]
        | "o" -> [(450.0, 1.0); (800.0, 0.5); (2830.0, 0.1)]
        | "u" -> [(325.0, 1.0); (700.0, 0.3); (2530.0, 0.08)]
        | "e" -> [(400.0, 1.0); (1700.0, 0.4); (2600.0, 0.2)]
        | "aw" -> [(570.0, 1.0); (840.0, 0.6); (2410.0, 0.15)]
        | _ -> failwith ("No vowel " + vowel + ".")
    sumw [for (f, gain) in table -> gain * bandpass x f 6.0]

let fit (x : Wave) n = if x.Length >= n then slice x 0 n else concat [x; zeros (n - x.Length)]

let mixFor (seconds : float option) (parts : (float * Wave) list) =
    let n =
        match seconds with
        | Some seconds -> int (seconds * SR)
        | None -> parts |> List.map (fun (o, p) -> p.Length + int (o * SR)) |> List.max
    let out = Array.zeroCreate n
    for (offset, p) in parts do
        let i = int (offset * SR)
        if i < n then
            let k = min p.Length (n - i)
            for j in 0 .. k - 1 do out[i + j] <- out[i + j] + p[j]
    Wave out

let mix parts = mixFor None parts
let mixIn seconds parts = mixFor (Some seconds) parts

let reverbIrOne seconds damp =
    let n = int (seconds * SR)
    let ir = rng.StandardNormal n * envExp n (seconds / 6.0)
    let ir = (filtOrder 1 ir (Low damp)).Samples
    ir[0] <- 0.0
    let ir = Wave ir
    ir / Math.Sqrt (pairwiseSum (squarew ir).Samples 0 ir.Length)

/// X wet with a room of a size, its tail kept.
let reverb (x : Wave) seconds (wet : float) damp =
    let ir = reverbIrOne seconds damp
    concat [x; zeros (ir.Length - 1)] + wet * fftconvolve x ir

let ping (freq : float) (decay : float) (strike : float) =
    // something struck that rings - a burst of noise rung through a resonator, at full strength
    let seconds = max 0.1 (decay * 5.0)
    normalizeTo (resonate (noise seconds * envExp (int (seconds * SR)) strike) freq decay) 1.0

let fadeOut (x : Wave) seconds =
    let n = min x.Length (int (seconds * SR))
    let out = Array.copy x.Samples
    let fall = squarew (linspace 1.0 0.0 n)
    for i in 0 .. n - 1 do out[out.Length - n + i] <- out[out.Length - n + i] * fall[i]
    Wave out

let trimSilence (x : Wave) =
    let floor = 0.0005 * peak (absw x)
    match Array.tryFindIndexBack (fun v -> abs v > floor) x.Samples with
    | Some last -> slice x 0 (last + 1)
    | None -> x

let writeWav (path : string) channels (samples : float array) =
    use stream = new BinaryWriter (File.Create path)
    let dataBytes = samples.Length * 2
    stream.Write "RIFF"B
    stream.Write (36 + dataBytes)
    stream.Write "WAVEfmt "B
    stream.Write 16
    stream.Write 1s
    stream.Write (int16 channels)
    stream.Write (int SR)
    stream.Write (int SR * channels * 2)
    stream.Write (int16 (channels * 2))
    stream.Write 16s
    stream.Write "data"B
    stream.Write dataBytes
    for s in samples do stream.Write (int16 (s * 32767.0))

let writeSound name x (level : float) =
    let x = normalizeTo (trimSilence x) level
    let x = fadeOut x 0.01
    writeWav (Path.Combine (Assets, "Gameplay", name + ".wav")) 1 x.Samples

let writeSong name (left : Wave) (right : Wave) =
    let scale = 0.89 / (max (peak (absw left)) (peak (absw right)) + 1e-9)
    let stereo = Array.init (2 * left.Length) (fun i -> (if i % 2 = 0 then left[i / 2] else right[i / 2]) * scale)
    let wav = Path.Combine (Path.GetTempPath (), "ZombieGameSong.wav")
    writeWav wav 2 stereo
    let ogg = Path.Combine (Assets, (if GuiSongs.Contains name then "Gui" else "Gameplay"), name + ".ogg")
    let info = ProcessStartInfo ("ffmpeg", UseShellExecute = false)
    for arg in ["-y"; "-loglevel"; "error"; "-i"; wav; "-c:a"; "libvorbis"; "-q:a"; "5"; ogg] do info.ArgumentList.Add arg
    use ffmpeg = Process.Start info
    ffmpeg.WaitForExit ()
    if ffmpeg.ExitCode <> 0 then failwith ("ffmpeg couldn't encode " + name + ".")
    File.Delete wav

// ---------------------------------------------------------------- instruments

let midi note = 440.0 * Math.Pow (2.0, (float note - 69.0) / 12.0)

let private noteOffsets =
    Map.ofList
        [("C", 0); ("C#", 1); ("Db", 1); ("D", 2); ("D#", 3); ("Eb", 3); ("E", 4); ("F", 5); ("F#", 6); ("Gb", 6); ("G", 7)
         ("G#", 8); ("Ab", 8); ("A", 9); ("A#", 10); ("Bb", 10); ("B", 11)]

/// A note's name, as 'A4', to its midi number.
let n (name : string) = 12 * (int name[name.Length - 1 ..] + 1) + noteOffsets[name[.. name.Length - 2]]

/// Additive partials, slightly stretched as a real string's are, the high ones dying first, a felt hammer's thump.
let piano (freq : float) seconds (velocity : float) =
    let t = tOf seconds
    let mutable out = zeros t.Length
    let b = 0.0003
    let mutable k = 1
    while k < 14 && freq * float k * Math.Sqrt (1.0 + b * float k * float k) <= SR / 2.2 do
        let f = freq * float k * Math.Sqrt (1.0 + b * float k * float k)
        let amp = (1.0 / Math.Pow (float k, 1.1)) * Math.Pow (velocity, 0.5 + 0.15 * float k)
        let decay = 3.2 / (1.0 + 0.35 * float k) * Math.Pow (220.0 / max freq 60.0, 0.35)
        out <- out + amp * sinw (2.0 * Math.PI * f * t + rng.Uniform (0.0, 6.28)) * expw (-t / decay)
        k <- k + 1
    let hammer = filt (noise 0.02) (Low 2500.0) * envExp (int (0.02 * SR)) 0.004 * 0.15 * velocity
    let out = Array.copy out.Samples
    for i in 0 .. hammer.Length - 1 do out[i] <- out[i] + hammer[i]
    Wave out * envAdsr out.Length 0.002 0.08

/// A soft electric piano - a sine bent by another at its own frequency, the bending fading as it rings.
let bell (freq : float) seconds (velocity : float) =
    let t = tOf seconds
    let index = 1.6 * velocity * expw (-t / 0.4)
    let out = sinw (2.0 * Math.PI * freq * t + index * sinw (2.0 * Math.PI * freq * t))
    let out = out + 0.25 * sinw (2.0 * Math.PI * freq * 4.0 * t) * expw (-t / 0.15) * velocity
    out * expw (-t / 2.2) * envAdsr t.Length 0.004 0.2 * velocity

/// A string pad - detuned saws, softened, swelling in and out.
let pad (freq : float) seconds brightness attack release =
    let (detune, voices) = (0.25, 5)
    let t = tOf seconds
    let mutable out = zeros t.Length
    for v in 0 .. voices - 1 do
        let cents = (float v - float (voices - 1) / 2.0) * detune * 10.0
        let f = freq * Math.Pow (2.0, cents / 1200.0) * (1.0 + 0.002 * sinw (2.0 * Math.PI * (0.2 + 0.07 * float v) * t))
        out <- out + osc f Saw
    let out = filt out (Low brightness)
    out / float voices * envAdsr t.Length attack release

let choir (freq : float) seconds vowel =
    let (attack, release) = (2.0, 2.5)
    let t = tOf seconds
    let mutable out = zeros t.Length
    for v in 0 .. 3 do
        let f = freq * (1.0 + 0.004 * (float v - 1.5)) * (1.0 + 0.006 * sinw (2.0 * Math.PI * (5.0 + 0.3 * float v) * t))
        out <- out + osc f Saw
    let out = formants (out + 0.05 * noise seconds) vowel
    out / 4.0 * envAdsr t.Length attack release

let staccato (freq : float) seconds brightness =
    let t = tOf seconds
    let out = osc (full t.Length freq) Saw + osc (full t.Length (freq * 1.005)) Saw
    let out = filt out (Low brightness)
    out * expw (-t / 0.06) * envAdsr t.Length 0.003 0.02

let brass (freq : float) seconds (brightness : float) =
    let t = tOf seconds
    let out = sumw [for d in [-0.006; 0.0; 0.007] -> osc (full t.Length (freq * (1.0 + d))) Saw]
    let sweepEnv = brightness * (0.3 + 0.7 * expw (-t / 0.25))
    let out = concat [for i in 0 .. 2048 .. out.Length - 1 -> filt (slice out i (i + 2048)) (Low sweepEnv[i])]
    tanhw (1.5 * out / 3.0) * envAdsr t.Length 0.03 0.15

let taiko seconds (pitch : float) (velocity : float) =
    let t = tOf seconds
    let body = sinw (2.0 * Math.PI * cumsum (pitch * (1.0 + 1.5 * expw (-t / 0.03))) / SR) * expw (-t / 0.35)
    let skin = filt (noise seconds) (Low 900.0) * expw (-t / 0.05) * 0.4
    (body + skin) * velocity

let boom seconds (pitch : float) =
    let t = tOf seconds
    let body = sinw (2.0 * Math.PI * cumsum (pitch * (1.0 + 2.0 * expw (-t / 0.05))) / SR) * expw (-t / 1.2)
    let rumble = filt (noise seconds) (Low 160.0) * expw (-t / 1.0) * 0.6
    body + rumble

// ---------------------------------------------------------------- sounds

/// A rubber-soled heel on waxed linoleum - a tight knock, a click, and now and then the sole's squeak; retired, but still
/// drawn first, so the hospital's other sounds keep their draws.
let squeakyFootstep variant =
    let s = 0.25
    let thud = filt (noise s) (Low (420.0 + 60.0 * float variant)) * envExp (int (s * SR)) 0.012
    let knock = fit (ping (260.0 + 40.0 * float variant) 0.03 0.002) (int (s * SR)) * 0.4
    let click = filt (noise s) (High 3200.0) * envExp (int (s * SR)) 0.003 * 0.3
    let parts =
        [(0.0, thud + knock + click)
         if variant = 2 then
            let t = tOf 0.06
            let squeak = sinw (2.0 * Math.PI * cumsum (sweep 2200.0 2900.0 0.06 1.0) / SR) * sinw (linspace 0.0 Math.PI t.Length) * 0.05
            (0.02, squeak)]
    reverb (mixIn s parts) 0.6 0.18 3000.0

/// A crack, the blast's body, a low thump, and the room ringing after.
let gunshot (size : float) =
    let s = 1.6
    let crack = filt (noise s) (High 1500.0) * envExp (int (s * SR)) (0.004 * size)
    let body = filt (noise s) (Low (2200.0 / size)) * envExp (int (s * SR)) (0.05 * size)
    let t = tOf s
    let thump = sinw (2.0 * Math.PI * cumsum (55.0 / size * (1.0 + 3.0 * expw (-t / 0.02))) / SR) * expw (-t / (0.09 * size))
    let dry = 1.0 * crack + 0.9 * body + 1.2 * thump
    let dry = tanhw (2.5 * dry / peak (absw dry)) // the clip of something louder than anything can be
    reverb dry (1.1 * size) 0.3 3500.0

let shotgun () =
    let blast = gunshot 1.7
    let clack f = fit (ping f 0.03 0.002) (int (0.08 * SR)) + filt (noise 0.08) (High 3000.0) * envExp (int (0.08 * SR)) 0.003 * 0.3
    let pump = mix [(0.0, clack 1900.0 * 0.25); (0.13, clack 1500.0 * 0.3)]
    mix [(0.0, blast); (0.42, reverb pump 0.4 0.2 4000.0)]

let click () =
    let s = 0.15
    fit (ping 3400.0 0.02 0.0008) (int (s * SR)) + filt (noise s) (High 4000.0) * envExp (int (s * SR)) 0.0015 * 0.3

/// Air parted by something swung - noise through a band swept along.
let whoosh seconds (f0 : float) (f1 : float) (q : float) =
    let x = noise seconds
    let out = Array.zeroCreate x.Length
    let centers = sweep f0 f1 seconds 1.0
    let block = 1024
    for i in 0 .. block .. x.Length - 1 do
        let c = centers[min i (centers.Length - 1)]
        let count = (slice x i (i + block)).Length
        let filtered = slice (bandpass (slice x i (i + block + 512)) c q) 0 count
        Array.blit filtered.Samples 0 out i count
    Wave out * squarew (sinw (linspace 0.0 Math.PI x.Length))

let fleshHit () =
    let s = 0.35
    let t = tOf s
    let thwack = filt (noise s) (Low 1400.0) * envExp (int (s * SR)) 0.025
    let thump = sinw (2.0 * Math.PI * 110.0 * t) * expw (-t / 0.06)
    let squelch = bandpass (noise s) 700.0 3.0 * envExp (int (s * SR)) 0.06 * 0.5
    reverb (thwack + thump + squelch) 0.4 0.1 4000.0

let bite () =
    let s = 0.6
    let crunches =
        mixIn s
            [for (o, a) in [(0.0, 1.0); (0.05, 0.7); (0.12, 0.8); (0.2, 0.5); (0.27, 0.4)] ->
                (o, filt (noise 0.05) (Band (600.0, 3500.0)) * envExp (int (0.05 * SR)) 0.008 * a)]
    let wet = bandpass (noise s) 450.0 2.0 * sinw (linspace 0.0 Math.PI (int (s * SR))) * 0.4
    reverb (crunches + wet + 0.6 * fit (fleshHit ()) (int (s * SR))) 0.4 0.1 4000.0

/// A throat - a buzzing glottis through a vowel's resonances, breathy, rough with jitter, rattling if it growls.
let voice seconds (f0Curve : Wave) vowel (breath : float) (rough : float) (growlRate : float) =
    let f = f0Curve * (1.0 + rough * filt (noise seconds) (Low 30.0) * 0.5)
    let src = osc f Saw + breath * noise seconds
    let src =
        if growlRate > 0.0 then src * (0.55 + 0.45 * absw (sinw (Math.PI * cumsum (full src.Length growlRate) / SR)))
        else src
    formants src vowel

let zombieGroan variant =
    let (s, start, stop, vowel) = [|(1.9, 95.0, 70.0, "aw"); (1.6, 85.0, 78.0, "u"); (2.2, 105.0, 62.0, "o")|][variant]
    let f0 = sweep start stop s 0.7 * (1.0 + 0.04 * sinw (2.0 * Math.PI * 3.3 * tOf s))
    let g = voice s f0 vowel 0.35 0.6 31.0
    let g = tanhw (2.0 * g / peak (absw g)) * envAdsr g.Length 0.25 0.6
    reverb g 0.8 0.2 2500.0

let zombieDeath () =
    let s = 2.3
    let f0 = sweep 110.0 45.0 s 0.6
    let g = voice s f0 "aw" 0.4 0.8 24.0
    let g = tanhw (2.2 * g / peak (absw g)) * envAdsr g.Length 0.05 1.2
    mix [(0.0, reverb g 0.8 0.2 2500.0); (1.0, 0.8 * reverb (boom 1.2 60.0 * 0.6) 0.6 0.15 4000.0)]

let playerHurt () =
    let s = 0.42
    let f0 = sweep 210.0 150.0 s 0.5
    let g = voice s f0 "u" 0.3 0.3 0.0
    reverb (g * envAdsr g.Length 0.01 0.2) 0.5 0.12 4000.0

let playerDeath () =
    let s = 1.6
    let f0 = sweep 260.0 120.0 s 0.6 * (1.0 + 0.02 * sinw (2.0 * Math.PI * 6.0 * tOf s))
    let g = voice s f0 "a" 0.35 0.35 0.0
    let g = g * envAdsr g.Length 0.02 0.9
    mix [(0.0, reverb g 0.9 0.2 4000.0); (0.9, 0.7 * boom 1.5 55.0)]

let dogGrowl () =
    let s = 1.3
    let f0 = 72.0 * (1.0 + 0.05 * sinw (2.0 * Math.PI * 2.0 * tOf s))
    let g = voice s f0 "u" 0.5 0.8 26.0
    reverb (tanhw (2.0 * g / peak (absw g)) * envAdsr g.Length 0.15 0.3) 0.6 0.15 4000.0

let dogBark () =
    let one f =
        let s = 0.2
        let g = voice s (sweep f (f * 0.6) s 0.5) "a" 0.6 0.5 0.0
        tanhw (3.0 * g / peak (absw g)) * envAdsr g.Length 0.005 0.12
    reverb (mix [(0.0, one 380.0); (0.27, 0.85 * one 340.0)]) 0.7 0.2 4000.0

let dogDeath () =
    let s = 0.9
    let g = voice s (sweep 620.0 300.0 s 0.8) "e" 0.5 0.3 0.0
    reverb (g * envAdsr g.Length 0.01 0.6) 0.6 0.15 4000.0

let bruteRoar () =
    let s = 2.4
    let t = tOf s
    let f0 = sweep 48.0 68.0 s 0.4 * (1.0 + 0.05 * sinw (2.0 * Math.PI * 4.0 * t))
    let g = voice s f0 "aw" 0.6 1.0 19.0 + 0.6 * voice s (f0 * 0.5) "o" 0.3 1.0 0.0
    let g = tanhw (3.0 * g / peak (absw g)) * envAdsr g.Length 0.15 0.9
    reverb (filt g (Low 3500.0)) 1.4 0.3 2500.0

let bruteSwipe () =
    let grunt = voice 0.5 (sweep 70.0 55.0 0.5 1.0) "u" 0.4 0.8 20.0
    let grunt = tanhw (2.0 * grunt / peak (absw grunt)) * envAdsr (int (0.5 * SR)) 0.02 0.25
    reverb (mix [(0.0, 0.7 * grunt); (0.15, whoosh 0.45 300.0 1100.0 2.0)]) 0.8 0.2 4000.0

let bruteDeath () =
    let s = 3.2
    let f0 = sweep 70.0 30.0 s 0.5
    let g = voice s f0 "aw" 0.6 1.0 16.0
    let g = tanhw (3.0 * g / peak (absw g)) * envAdsr g.Length 0.05 2.0
    mix [(0.0, reverb g 1.4 0.3 2200.0); (2.2, reverb (boom 2.5 42.0) 1.2 0.3 4000.0)]

/// Stick-slip friction - a train of clicks speeding and slowing, rung through the wood's own resonances.
let creak seconds (baseRate : float) resonances (roughness : float) (pitchWander : float) =
    let t = tOf seconds
    let rate = baseRate * (1.0 + pitchWander * sinw (2.0 * Math.PI * cumsum (0.6 + 0.6 * filt (noise seconds) (Low 2.0)) / SR))
    let rate = rate * (1.0 + roughness * filt (noise seconds) (Low 8.0))
    let phase = cumsum (maximumw rate 5.0) / SR
    let floors = floorw phase
    let steps = Wave (Array.init floors.Length (fun i -> if i = 0 then floors[0] else floors[i] - floors[i - 1]))
    let pulses = steps * (0.6 + 0.4 * rng.Random t.Length)
    normalizeTo (sumw [for (f, g, d) in resonances -> g * resonate pulses f d]) 1.0

/// The latch, the long complaint of the hinges as the door swings into the dark, and it settling against the wall.
let doorWood double =
    let latch = fit (ping 1300.0 0.05 0.001) (int (0.3 * SR)) * 0.4 + fit (ping 240.0 0.06 0.003) (int (0.3 * SR)) * 0.6
    let hinge = creak 1.8 60.0 [(420.0, 1.0, 0.04); (950.0, 0.6, 0.03); (1900.0, 0.3, 0.02)] 0.5 0.4 * envAdsr (int (1.8 * SR)) 0.2 0.5
    let parts =
        [(0.15, latch * 0.8); (0.45, hinge * 0.5)
         if double then (0.65, creak 1.6 48.0 [(360.0, 1.0, 0.04); (820.0, 0.6, 0.03)] 0.5 0.4 * envAdsr (int (1.6 * SR)) 0.2 0.5 * 0.4)]
    let parts = parts @ [(2.25, filt (noise 0.3) (Low 300.0) * envExp (int (0.3 * SR)) 0.04 * 0.7)]
    reverb (mixIn 2.6 parts) 1.2 0.3 3000.0

let doorLocked () =
    let rattle = mix [for (i, o) in List.indexed [0.0; 0.09; 0.17; 0.27; 0.36] -> (o, ping (900.0 + 200.0 * float (i % 2)) 0.05 0.002 * (1.0 - 0.15 * float i))]
    reverb (rattle + 0.5 * fit (filt (noise 0.5) (Low 250.0) * envExp (int (0.5 * SR)) 0.03) rattle.Length) 0.7 0.2 4000.0

let keyUnlock () =
    let c f = fit (ping f 0.04 0.001) (int (0.25 * SR))
    let scrape = filt (noise 0.25) (Band (2000.0, 6000.0)) * sinw (linspace 0.0 Math.PI (int (0.25 * SR))) * 0.06
    reverb (mix [(0.0, scrape); (0.25, c 2200.0); (0.45, c 1600.0 + 0.6 * c 800.0)]) 0.6 0.2 4000.0

let itemPickup () =
    let rustle = filt (noise 0.25) (Band (1500.0, 7000.0)) * envAdsr (int (0.25 * SR)) 0.02 0.15 * 0.4
    let tone = mix [(0.05, bell (midi (n "A5")) 0.8 0.5); (0.13, bell (midi (n "E6")) 1.0 0.45)]
    reverb (mix [(0.0, rustle); (0.0, tone * 0.5)]) 1.0 0.25 4000.0

let typewriter () =
    let mutable parts = []
    let mutable o = 0.0
    for _ in 0 .. 10 do
        let key = fit (ping (rng.Uniform (1700.0, 2300.0)) 0.02 0.001) (int (0.1 * SR)) * 0.6 + fit (filt (noise 0.06) (Low 400.0) * envExp (int (0.06 * SR)) 0.01 * 0.6) (int (0.1 * SR))
        parts <- parts @ [(o, key * rng.Uniform (0.6, 1.0))]
        o <- o + rng.Uniform (0.09, 0.2)
    parts <- parts @ [(o + 0.1, bell (midi (n "A6")) 1.4 0.9 * 0.35)]
    parts <- parts @ [(o + 0.3, filt (noise 0.3) (Band (600.0, 3000.0)) * envAdsr (int (0.3 * SR)) 0.05 0.2 * 0.3)]
    reverb (mix parts) 0.6 0.15 4000.0

let boxOpen () =
    let lid = creak 0.6 70.0 [(380.0, 1.0, 0.04); (880.0, 0.5, 0.03)] 0.5 0.4 * envAdsr (int (0.6 * SR)) 0.05 0.2 * 0.5
    let thunk = filt (noise 0.3) (Low 350.0) * envExp (int (0.3 * SR)) 0.03
    reverb (mix [(0.0, lid); (0.55, thunk * 0.7)]) 0.6 0.15 4000.0

let menuMove () =
    let t = tOf 0.07
    sinw (2.0 * Math.PI * 1400.0 * t) * expw (-t / 0.015) * 0.5

let menuConfirm () =
    let t = tOf 0.25
    let a = sinw (2.0 * Math.PI * 880.0 * t) * expw (-t / 0.05)
    let b = sinw (2.0 * Math.PI * 1320.0 * t) * expw (-t / 0.08)
    mix [(0.0, a * 0.5); (0.06, b * 0.5)]

let menuBack () =
    let t = tOf 0.2
    mix [(0.0, sinw (2.0 * Math.PI * 990.0 * t) * expw (-t / 0.05) * 0.5); (0.06, sinw (2.0 * Math.PI * 660.0 * t) * expw (-t / 0.06) * 0.5)]

let heal () =
    let hiss = filt (noise 0.9) (High 3000.0) * envAdsr (int (0.9 * SR)) 0.03 0.4 * 0.35
    let chord = sumw [for x in ["C5"; "E5"; "G5"] -> bell (midi (n x)) 1.8 0.4]
    reverb (mix [(0.0, hiss); (0.2, chord * 0.3)]) 1.4 0.3 4000.0

let gunReady () =
    let c f d = fit (ping f d 0.0015) (int (0.25 * SR))
    reverb (mix [(0.0, c 1800.0 0.03 * 0.6); (0.1, c 1200.0 0.04 + 0.5 * c 2600.0 0.02)]) 0.5 0.15 4000.0

let page () =
    filt (noise 0.18) (Band (800.0, 5000.0)) * envAdsr (int (0.18 * SR)) 0.03 0.12 * 0.5

/// A push bar's clack, then two leaves swinging on their hinges.
let doorDouble () =
    let bar = fit (ping 1800.0 0.05 0.001) (int (0.3 * SR)) * 0.5 + fit (ping 700.0 0.08 0.002) (int (0.3 * SR)) * 0.4
    mix [(0.0, reverb bar 0.6 0.2 4000.0); (0.05, doorWood true * 0.9)]

/// A roller shutter going up - its motor whining, its slats rattling over the drum, a clunk as it stops.
let shutter () =
    let s = 2.4
    let t = tOf s
    let motor = filt (osc (full t.Length 118.0) Saw + 0.5 * osc (full t.Length 236.5) Saw) (Low 900.0) * envAdsr t.Length 0.2 0.3 * 0.3
    let slats = creak s 26.0 [(900.0, 1.0, 0.03); (2100.0, 0.6, 0.02)] 0.15 0.05 * envAdsr t.Length 0.15 0.3 * 0.5
    let clunk = fit (ping 180.0 0.3 0.003) (int (1.0 * SR))
    reverb (mixIn 3.4 [(0.0, motor + slats); (2.3, clunk * 0.7)]) 1.2 0.3 4000.0

let keypad () =
    let t = tOf 0.09
    let tone = filt (signw (sinw (2.0 * Math.PI * 1250.0 * t))) (Low 4000.0) * envAdsr t.Length 0.002 0.02
    tone * 0.5

let keypadWrong () =
    let t = tOf 0.45
    let buzz = filt (signw (sinw (2.0 * Math.PI * 310.0 * t)) + 0.5 * signw (sinw (2.0 * Math.PI * 293.0 * t))) (Low 2500.0) * envAdsr t.Length 0.005 0.05
    buzz * 0.4

/// The lock's chirp, a click, and a metal drawer running out on its rails.
let drawerOpen () =
    let chirp = mix [(0.0, keypad ()); (0.1, keypad ())]
    let latch = fit (ping 1600.0 0.04 0.001) (int (0.2 * SR)) * 0.5
    let slide = whoosh 0.5 600.0 1500.0 1.2 * 0.5 + creak 0.5 70.0 [(1300.0, 1.0, 0.02)] 0.6 0.1 * 0.2
    let stop = fit (ping 420.0 0.15 0.002) (int (0.5 * SR)) * 0.6
    reverb (mix [(0.0, chirp); (0.25, latch); (0.35, slide); (0.85, stop)]) 0.6 0.2 4000.0

/// A relay slamming home, the generator winding up to speed, and the lamps ticking into life down the halls.
let powerOn () =
    let s = 4.0
    let t = tOf s
    let relay = fit (ping 150.0 0.2 0.002) (int (0.6 * SR)) + fit (ping 900.0 0.08 0.001) (int (0.6 * SR)) * 0.5
    let f = sweep 18.0 52.0 2.5 0.6
    let f = concat [f; full (t.Length - f.Length) 52.0]
    let hum = filt (osc f Saw + 0.6 * osc (f * 2.0) Saw) (Low 700.0) * envAdsr t.Length 0.4 0.8 * 0.4
    let ticks = mixIn s [for i in 0 .. 5 -> (1.4 + 0.35 * float i + 0.1 * rng.Random (), fit (ping (rng.Uniform (2500.0, 4000.0)) 0.02 0.001) (int (0.1 * SR)) * 0.3)]
    let buzz = filt (osc (full t.Length 120.0) Square) (Band (200.0, 2000.0)) * envAdsr t.Length 2.0 0.6 * 0.06
    reverb (mix [(0.0, relay * 0.8); (0.15, hum + ticks + buzz)]) 1.5 0.3 4000.0

let fuse () =
    let clink = fit (ping 3100.0 0.05 0.001) (int (0.2 * SR)) * 0.5
    let seat = fit (ping 950.0 0.06 0.002) (int (0.3 * SR)) + fit (ping 2400.0 0.03 0.001) (int (0.3 * SR)) * 0.4
    reverb (mix [(0.0, clink); (0.3, seat)]) 0.5 0.15 4000.0

/// A morgue drawer flung open from within - a steel bang ringing long, and the drawer shuddering on its rails.
let drawerSlam () =
    let bang = fit (ping 190.0 0.9 0.004) (int (3.0 * SR)) + fit (ping 530.0 0.6 0.003) (int (3.0 * SR)) * 0.6 + fit (ping 1270.0 0.4 0.002) (int (3.0 * SR)) * 0.3
    let thump = filt (noise 0.4) (Low 220.0) * envExp (int (0.4 * SR)) 0.05
    let rattle = creak 0.6 40.0 [(800.0, 1.0, 0.05)] 0.8 0.2 * envAdsr (int (0.6 * SR)) 0.01 0.3 * 0.3
    reverb (mix [(0.0, bang * 0.8 + fit thump (int (3.0 * SR))); (0.1, rattle)]) 1.6 0.35 4000.0

/// The starter grinding, the engine catching, and its idle settling.
let engine () =
    let s = 3.6
    let starter = filt (osc (sweep 180.0 260.0 0.9 1.0) Saw) (Low 1500.0) * creak 0.9 18.0 [(300.0, 1.0, 0.02)] 0.2 0.0 * 0.4
    let pulse = 0.5 + 0.5 * sinw (2.0 * Math.PI * cumsum (sweep 40.0 22.0 (s - 0.9) 0.5) / SR)
    let idle = filt (noise (s - 0.9)) (Low 260.0) * pulse * envAdsr (int ((s - 0.9) * SR)) 0.05 0.8
    let roar = filt (noise 0.6) (Low 600.0) * envAdsr (int (0.6 * SR)) 0.02 0.4 * 0.8
    reverb (mix [(0.0, starter); (0.85, roar); (0.9, idle * 1.4)]) 1.4 0.25 4000.0

let RetiredSounds = [(fun () -> squeakyFootstep 0); (fun () -> squeakyFootstep 1); (fun () -> squeakyFootstep 2)]

let Sounds =
    ["HandgunShot", (fun () -> gunshot 1.0); "ShotgunShot", shotgun; "EmptyClick", click
     "KnifeSwing", (fun () -> whoosh 0.24 1100.0 5200.0 3.0); "FleshHit", fleshHit; "Bite", bite
     "PlayerHurt", playerHurt; "PlayerDeath", playerDeath
     "ZombieGroan0", (fun () -> zombieGroan 0); "ZombieGroan1", (fun () -> zombieGroan 1); "ZombieGroan2", (fun () -> zombieGroan 2)
     "ZombieDeath", zombieDeath; "DogGrowl", dogGrowl; "DogBark", dogBark; "DogDeath", dogDeath
     "SurgeonRoar", bruteRoar; "SurgeonSwipe", bruteSwipe; "SurgeonDeath", bruteDeath
     "Door", (fun () -> doorWood false); "DoubleDoor", doorDouble; "Shutter", shutter; "DoorLocked", doorLocked
     "KeyUnlock", keyUnlock; "ItemPickup", itemPickup; "Typewriter", typewriter; "BoxOpen", boxOpen
     "MenuMove", menuMove; "MenuConfirm", menuConfirm; "MenuBack", menuBack; "Heal", heal; "GunReady", gunReady
     "Page", page; "Keypad", keypad; "KeypadWrong", keypadWrong; "DrawerOpen", drawerOpen; "PowerOn", powerOn
     "Fuse", fuse; "DrawerSlam", drawerSlam; "Engine", engine]

// ---------------------------------------------------------------- songs

/// A stereo buffer to lay notes into at times, panned, then wet with a hall and wrapped into a loop.
type Track (seconds : float) =
    let left = Array.zeroCreate (int ((seconds + 12.0) * SR))
    let right = Array.zeroCreate left.Length
    member this.Add (at : float, x : Wave, ?gain : float, ?pan : float) =
        let (gain, pan) = (defaultArg gain 1.0, defaultArg pan 0.0)
        let i = int (at * SR)
        let k = min x.Length (left.Length - i)
        if k > 0 then
            let lg = Math.Cos ((pan + 1.0) * Math.PI / 4.0)
            let rg = Math.Sin ((pan + 1.0) * Math.PI / 4.0)
            for j in 0 .. k - 1 do
                left[i + j] <- left[i + j] + x[j] * gain * lg * 1.414
                right[i + j] <- right[i + j] + x[j] * gain * rg * 1.414
    member this.Finish (hall, wet : float, ?damp, ?loop) =
        let (damp, loop) = (defaultArg damp 3500.0, defaultArg loop true)
        let irl = reverbIrOne hall damp
        let irr = reverbIrOne hall damp
        let left = Wave left + wet * slice (fftconvolve (Wave left) irl) 0 left.Length
        let right = Wave right + wet * slice (fftconvolve (Wave right) irr) 0 right.Length
        let n = int (seconds * SR)
        if loop then
            // wrap everything ringing past the loop's end back onto its start, so it repeats seamlessly
            let l = Array.copy left.Samples[.. n - 1]
            let r = Array.copy right.Samples[.. n - 1]
            let tail = left.Length - n
            for i in 0 .. min n tail - 1 do
                l[i] <- l[i] + left[n + i]
                r[i] <- r[i] + right[n + i]
            (Wave l, Wave r)
        else
            let ending = (trimSilence (absw left + absw right)).Length
            (fadeOut (slice left 0 ending) 2.0, fadeOut (slice right 0 ending) 2.0)

let chordNotes root kind =
    let intervals = match kind with "m" -> [0; 3; 7] | "M" -> [0; 4; 7] | "dim" -> [0; 3; 6] | "sus" -> [0; 5; 7] | _ -> failwith ("No chord " + kind + ".")
    [|for i in intervals -> root + i|]

/// A music box's lullaby in E minor, too slow, over a low swell, and a monitor's beep keeping time far off.
let titleSong () =
    let bar = 4.0
    let tr = Track (bar * 16.0)
    let prog = [|("E2", "m"); ("E2", "m"); ("C2", "M"); ("C2", "M"); ("A1", "m"); ("A1", "m"); ("B1", "M"); ("B1", "M")|]
    let tune =
        [|Some "B5"; Some "G5"; Some "E5"; Some "G5"; Some "F#5"; Some "D5"; Some "B4"; Some "D5"; Some "E5"; Some "C5"; Some "A4"; Some "C5"; Some "D#5"; Some "B4"; Some "F#5"; Some "B4"
          Some "B5"; Some "G5"; Some "E5"; Some "B5"; Some "A5"; Some "F#5"; Some "D5"; Some "A5"; Some "G5"; Some "E5"; Some "C5"; Some "E5"; Some "F#5"; Some "D#5"; Some "B4"; None|]
    for b in 0 .. 15 do
        let (root, kind) = prog[b % 8]
        let t0 = float b * bar
        let notes = chordNotes (n root) kind
        if b % 2 = 0 then
            tr.Add (t0, pad (midi notes[0]) (bar * 2.0 + 1.5) 500.0 2.0 2.5, 0.22, -0.3)
            tr.Add (t0, pad (midi (notes[0] + 12)) (bar * 2.0 + 1.5) 700.0 2.0 2.5, 0.12, 0.3)
            if b >= 8 then tr.Add (t0, choir (midi (notes[1] + 12)) (bar * 2.0 + 1.0) "u", 0.08, 0.4)
        for i in 0 .. 1 do
            match tune[(b * 2 + i) % tune.Length] with
            | Some m -> tr.Add (t0 + float i * 2.0, bell (midi (n m + 12)) 2.5 0.5, (if b >= 8 then 0.3 else 0.22), 0.2)
            | None -> ()
        let beep = sinw (2.0 * Math.PI * 980.0 * tOf 0.12) * envAdsr (int (0.12 * SR)) 0.005 0.04
        for k in 0 .. 3 do tr.Add (t0 + float k * 1.0 + 0.5, beep, 0.025, -0.7)
        if b % 8 = 0 then tr.Add (t0, boom 5.0 34.0, 0.45)
    tr.Finish (4.5, 0.55)

/// Not a tune - the building at night: the ventilation's breath, drips somewhere in the pipes, a far-off clatter, and
/// something moaning a long way off - with the power back, the lamps' hum and a monitor beeping in an empty room.
let hospitalSong powered =
    let seconds = 96.0
    let tr = Track seconds
    let t = tOf (seconds + 12.0)
    let vent = filt (brown (seconds + 12.0)) (Low (if not powered then 220.0 else 320.0))
    tr.Add (0.0, vent, (if not powered then 0.3 else 0.4))
    if powered then
        let hum = filt (osc (full t.Length 120.0) Square + 0.5 * osc (full t.Length 240.3) Saw) (Band (150.0, 2500.0))
        let hum = hum * (0.7 + 0.3 * squarew (sinw (2.0 * Math.PI * t / 7.0)))
        tr.Add (0.0, hum, 0.018, -0.2)
        tr.Add (0.0, hum, 0.018, 0.2)
        let beep = sinw (2.0 * Math.PI * 880.0 * tOf 0.1) * envAdsr (int (0.1 * SR)) 0.005 0.03
        for k in 0 .. int (seconds / 1.1) - 1 do tr.Add (3.0 + float k * 1.1, filt beep (Low 2500.0), 0.015, 0.65)
    else
        // wind finding its way down the dead vents
        let w = noise (seconds + 12.0)
        let centers = 300.0 + 200.0 * squarew (sinw (2.0 * Math.PI * t / 19.0))
        let wind = Array.zeroCreate w.Length
        for i in 0 .. 4096 .. w.Length - 1 do
            let count = (slice w i (i + 4096)).Length
            Array.blit (slice (bandpass (slice w i (i + 4096 + 1024)) centers[i] 5.0) 0 count).Samples 0 wind i count
        let gust = poww (clipBelow (filt (noise (seconds + 12.0)) (Low 0.08) * 6.0) 0.0) 1.5
        tr.Add (0.0, Wave wind * (0.1 + gust), 0.08, -0.4)
    let mutable at = 2.0
    while at < seconds - 4.0 do
        let kind = rng.Random ()
        if kind < 0.45 then
            let drip = fit (ping (rng.Uniform (1400.0, 2600.0)) 0.04 0.0005) (int (0.3 * SR))
            tr.Add (at, drip, 0.2, rng.Uniform (-0.9, 0.9))
        elif kind < 0.65 then
            let clatter = mix [for k in 0 .. 3 -> (0.06 * float k, fit (ping (rng.Uniform (600.0, 1800.0)) 0.12 0.002) (int (0.5 * SR)) * (1.0 - 0.2 * float k))]
            tr.Add (at, filt clatter (Low 2500.0), 0.12, rng.Uniform (-0.9, 0.9))
        elif kind < 0.85 then
            let c = creak (rng.Uniform (0.5, 1.0)) (rng.Uniform (80.0, 140.0)) [(rng.Uniform (900.0, 1600.0), 1.0, 0.02)] 0.6 0.3
            tr.Add (at, filt (c * envAdsr c.Length 0.1 0.3) (Low 3000.0), 0.06, rng.Uniform (-0.9, 0.9)) // a gurney's wheel, somewhere
        else
            let far = zombieGroan (rng.Integers (0, 3))
            tr.Add (at, filt far (Low 900.0), 0.07, rng.Uniform (-0.8, 0.8))
        at <- at + rng.Uniform (3.0, 8.0)
    tr.Finish (3.5, 0.45, 2500.0)

/// A quiet room where nothing comes - soft piano over held strings, C minor resolving warm, unhurried.
let safeRoomSong () =
    let beat = 60.0 / 62.0
    let bar = beat * 4.0
    let prog = [|("C3", "m"); ("Ab2", "M"); ("Eb3", "M"); ("G2", "M"); ("C3", "m"); ("Ab2", "M"); ("F2", "m"); ("G2", "M")|]
    let tr = Track (bar * 16.0)
    let top = [|"G5"; "Eb5"; "C5"; "D5"; "Eb5"; "G5"; "F5"; "D5"; "G5"; "C6"; "Bb5"; "Ab5"; "Ab5"; "G5"; "F5"; "D5"|]
    for b in 0 .. 15 do
        let (root, kind) = prog[b % 8]
        let t0 = float b * bar
        let notes = chordNotes (n root) kind
        tr.Add (t0, pad (midi notes[0]) (bar + 1.5) 650.0 1.2 1.5, 0.2, -0.4)
        tr.Add (t0, pad (midi (notes[2] + 12)) (bar + 1.5) 850.0 1.2 1.5, 0.1, 0.4)
        for (i, note) in List.indexed [notes[0] + 12; notes[2] + 12; notes[1] + 24; notes[2] + 12] do
            tr.Add (t0 + float i * beat, piano (midi note) 3.0 0.35, 0.28, -0.2 + 0.1 * float i)
        if b >= 8 then
            tr.Add (t0, piano (midi (n top[(b - 8) * 2])) 3.5 0.6, 0.4, 0.2)
            tr.Add (t0 + 2.0 * beat, piano (midi (n top[(b - 8) * 2 + 1])) 3.5 0.55, 0.36, 0.2)
    tr.Finish (3.5, 0.5)

/// The surgeon - pounding metal, a grinding ostinato in D minor, a siren's wail climbing over it.
let bossSong () =
    let beat = 60.0 / 144.0
    let bar = beat * 4.0
    let bars = 32
    let tr = Track (bar * float bars)
    let roots = [|"D2"; "D2"; "Bb1"; "A1"; "D2"; "D2"; "Eb2"; "A1"|]
    let figure = [0; 0; 12; 0; 0; 1; 0; 7]
    for b in 0 .. bars - 1 do
        let t0 = float b * bar
        let r = n roots[b % 8]
        for (i, step) in List.indexed figure do
            tr.Add (t0 + float i * beat / 2.0, staccato (midi (r + step)) 0.18 (1000.0 + 500.0 * float (b / 8)), 0.32, -0.2)
            tr.Add (t0 + float i * beat / 2.0, staccato (midi (r + step + 12)) 0.12 2200.0, 0.1, 0.25)
        for (k, hit) in List.indexed (if b % 2 = 1 then [0.0; 1.5; 2.0; 3.0] else [0.0; 1.0; 2.5; 3.0]) do
            tr.Add (t0 + hit * beat, taiko 1.0 (if k % 2 = 0 then 62.0 else 90.0) (if k = 0 then 1.0 else 0.7), 0.5, -0.1 + 0.2 * float (k % 2))
        if b % 2 = 1 then
            let clang = fit (ping (rng.Uniform (400.0, 700.0)) 0.4 0.002) (int (1.0 * SR))
            tr.Add (t0 + 3.5 * beat, clang, 0.25, 0.4)
        if b >= 8 && b % 4 = 0 then
            let siren = osc (sweep (midi (r + 31)) (midi (r + 36)) (bar * 4.0) 0.7) Tri
            tr.Add (t0, filt siren (Low 3000.0) * envAdsr siren.Length 0.6 1.0, 0.05, rng.Uniform (-0.5, 0.5))
        if b >= 16 then
            for note in (if b % 4 = 3 then [r + 12; r + 15; r + 20] else []) do
                tr.Add (t0 + 3.0 * beat, brass (midi note) (beat * 1.5) 2200.0, 0.1, 0.1)
    tr.Finish (2.0, 0.3)

/// The road out at dawn - the music box's tune again, in G major now, the strings opening under it.
let endingSong () =
    let bar = 60.0 / 68.0 * 4.0
    let prog = [("G2", "M"); ("D3", "M"); ("E2", "m"); ("C3", "M"); ("G2", "M"); ("D3", "M"); ("C3", "M"); ("G2", "M")]
    let tr = Track (bar * 9.0)
    let melody = [|"B5"; "A5"; "G5"; "E5"; "D5"; "F#5"; "E5"; "D5"|]
    for (b, (root, kind)) in List.indexed prog do
        let t0 = float b * bar
        let notes = chordNotes (n root) kind
        for (k, note) in List.indexed [notes[0]; notes[1] + 12; notes[2] + 12] do
            tr.Add (t0, pad (midi note) (bar + 2.0) 1200.0 1.5 2.0, 0.14, -0.4 + 0.4 * float k)
        for (i, note) in List.indexed [notes[0] + 12; notes[2] + 12; notes[1] + 24; notes[2] + 12] do
            tr.Add (t0 + float i * bar / 4.0, piano (midi note) 3.0 0.38, 0.3, -0.1)
        tr.Add (t0, bell (midi (n melody[b])) 3.0 0.6, 0.35, 0.15)
    for note in ["G3"; "D4"; "B4"] do
        tr.Add (8.0 * bar, piano (midi (n note)) 6.0 0.55, 0.4)
    tr.Finish (4.0, 0.5, loop = false)

/// The end - a heavy blow, the low brass falling through a cluster, the strings sinking after it, and a flatline.
let gameOverSong () =
    let tr = Track 9.0
    tr.Add (0.0, boom 6.0 32.0, 0.9)
    for (k, note) in List.indexed ["E2"; "F2"; "B2"; "E3"] do
        tr.Add (0.05, brass (midi (n note)) 3.5 1400.0, 0.22, -0.4 + 0.25 * float k)
    for (k, (a, b)) in List.indexed [("E5", "B4"); ("G5", "D5"); ("Bb5", "F5")] do
        let f = sweep (midi (n a)) (midi (n b)) 5.5 1.0
        let strings = sumw [for d in [-0.004; 0.0; 0.005] -> osc (f * (1.0 + d)) Saw]
        tr.Add (0.6, filt strings (Low 2500.0) * envAdsr strings.Length 0.8 2.5, 0.06, -0.5 + 0.5 * float k)
    let flat = sinw (2.0 * Math.PI * 980.0 * tOf 4.0) * envAdsr (int (4.0 * SR)) 0.01 1.5
    tr.Add (3.0, flat, 0.04, 0.0)
    tr.Finish (5.0, 0.5, loop = false)

let Songs =
    ["TitleSong", titleSong; "HospitalSong", (fun () -> hospitalSong false); "HospitalPoweredSong", (fun () -> hospitalSong true)
     "SafeRoomSong", safeRoomSong; "BossSong", bossSong; "EndingSong", endingSong; "GameOverSong", gameOverSong]

// ---------------------------------------------------------------- the dorm

/// A heavier crack than the handgun's, a long roll of thunder after it, and the cylinder clicking round.
let magnum () =
    let blast = gunshot 1.45
    let tick = fit (ping 2600.0 0.02 0.001) (int (0.06 * SR)) * 0.2
    mix [(0.0, blast); (0.55, reverb tick 0.3 0.2 4000.0)]

/// A runner's ragged breath dragged through a throat that's stopped working - fast, wet, hissing.
let sprinterHiss () =
    let s = 1.1
    let t = tOf s
    let pant = poww (absw (sinw (2.0 * Math.PI * 3.5 * t))) 0.6
    let breath = bandpass (noise s) 2400.0 1.5 * pant * 0.8 + voice s (sweep 150.0 120.0 s 1.0) "e" 0.9 0.7 0.0 * pant * 0.3
    reverb (breath * envAdsr breath.Length 0.05 0.3) 0.6 0.15 4000.0

/// The shriek as it pounces - high and torn.
let sprinterShriek () =
    let s = 0.7
    let g = voice s (sweep 420.0 640.0 s 0.4 * (1.0 + 0.04 * sinw (2.0 * Math.PI * 11.0 * tOf s))) "e" 0.5 0.9 40.0
    let g = tanhw (3.0 * g / peak (absw g)) * envAdsr g.Length 0.01 0.3
    reverb g 0.7 0.2 4000.0

let sprinterDeath () =
    let s = 1.4
    let g = voice s (sweep 380.0 110.0 s 0.6) "a" 0.5 0.8 30.0
    let g = tanhw (2.5 * g / peak (absw g)) * envAdsr g.Length 0.01 0.9
    mix [(0.0, reverb g 0.7 0.2 4000.0); (0.7, 0.6 * reverb (boom 1.0 70.0 * 0.6) 0.5 0.15 4000.0)]

/// A roar like the surgeon's, but hoarse from a season of shouting, the helmet ringing with it.
let linebackerRoar () =
    let s = 2.2
    let t = tOf s
    let f0 = sweep 58.0 80.0 s 0.4 * (1.0 + 0.06 * sinw (2.0 * Math.PI * 5.0 * t))
    let g = voice s f0 "a" 0.7 1.0 22.0 + 0.5 * voice s (f0 * 0.5) "aw" 0.3 1.0 0.0
    let ring = fit (ping 880.0 0.6 0.002) g.Length * 0.05
    let g = tanhw (3.0 * g / peak (absw g)) * envAdsr g.Length 0.1 0.8 + ring
    reverb (filt g (Low 3800.0)) 1.3 0.3 2600.0

/// A grunt and a great padded shoulder driving through the air.
let linebackerSmash () =
    let grunt = voice 0.45 (sweep 85.0 62.0 0.45 1.0) "u" 0.4 0.8 22.0
    let grunt = tanhw (2.0 * grunt / peak (absw grunt)) * envAdsr (int (0.45 * SR)) 0.02 0.2
    let thud = filt (noise 0.3) (Low 300.0) * envExp (int (0.3 * SR)) 0.05
    reverb (mix [(0.0, 0.7 * grunt); (0.12, whoosh 0.4 250.0 900.0 2.0); (0.42, thud)]) 0.8 0.2 4000.0

let linebackerDeath () =
    let s = 3.0
    let g = voice s (sweep 80.0 32.0 s 0.5) "aw" 0.6 1.0 18.0
    let g = tanhw (3.0 * g / peak (absw g)) * envAdsr g.Length 0.05 1.8
    mix [(0.0, reverb g 1.3 0.3 2300.0); (2.0, reverb (boom 2.4 40.0) 1.2 0.3 4000.0)]

let DormSounds =
    ["MagnumShot", magnum; "SprinterHiss", sprinterHiss; "SprinterShriek", sprinterShriek; "SprinterDeath", sprinterDeath
     "LinebackerRoar", linebackerRoar; "LinebackerSmash", linebackerSmash; "LinebackerDeath", linebackerDeath]

/// Not a tune - the dorm at night: a vending machine's hum, a shower dripping down the hall, a smoke detector chirping
/// for a battery no one will change, a television's static through a wall, and something thumping on a door.
let dormSong () =
    let seconds = 96.0
    let tr = Track seconds
    let t = tOf (seconds + 12.0)
    let air = filt (brown (seconds + 12.0)) (Low 260.0)
    tr.Add (0.0, air, 0.3)
    let hum = filt (osc (full t.Length 60.0) Saw + 0.4 * osc (full t.Length 120.4) Saw) (Band (80.0, 900.0))
    tr.Add (0.0, hum * (0.8 + 0.2 * sinw (2.0 * Math.PI * t / 11.0)), 0.025, -0.5)
    let static' = filt (noise (seconds + 12.0)) (Band (1500.0, 6000.0)) * (0.5 + 0.5 * squarew (sinw (2.0 * Math.PI * t / 23.0)))
    tr.Add (0.0, filt static' (Low 2200.0), 0.02, 0.6)
    let chirp = sinw (2.0 * Math.PI * 3200.0 * tOf 0.05) * envAdsr (int (0.05 * SR)) 0.002 0.02
    for k in 0 .. int (seconds / 24.0) - 1 do tr.Add (7.0 + float k * 24.0, chirp, 0.05, 0.3)
    let mutable at = 1.5
    while at < seconds - 4.0 do
        let kind = rng.Random ()
        if kind < 0.45 then
            let drip = fit (ping (rng.Uniform (1700.0, 2900.0)) 0.05 0.0005) (int (0.3 * SR))
            tr.Add (at, filt drip (Low 4000.0), 0.16, rng.Uniform (-0.9, 0.9))
        elif kind < 0.7 then
            let count = rng.Integers (3, 6)
            let thumps = mix [for k in 0 .. count - 1 -> (0.32 * float k, filt (noise 0.12) (Low 180.0) * envExp (int (0.12 * SR)) 0.03)]
            tr.Add (at, thumps, 0.25, rng.Uniform (-0.6, 0.6)) // something throwing itself at a door, rooms away
        elif kind < 0.85 then
            let c = creak (rng.Uniform (0.6, 1.1)) (rng.Uniform (60.0, 110.0)) [(rng.Uniform (700.0, 1300.0), 1.0, 0.02)] 0.6 0.3
            tr.Add (at, filt (c * envAdsr c.Length 0.1 0.3) (Low 2600.0), 0.06, rng.Uniform (-0.9, 0.9)) // a bunk's springs
        else
            let far = zombieGroan (rng.Integers (0, 3))
            tr.Add (at, filt far (Low 800.0), 0.07, rng.Uniform (-0.8, 0.8))
        at <- at + rng.Uniform (3.0, 8.0)
    tr.Finish (3.0, 0.4, 2400.0)

/// The linebacker - a drumline gone wrong: snares rolling and stuttering, a bass drum stamping, the fight song's brass
/// stabbing in a minor key over it, and the crowd that isn't there any more.
let dormBossSong () =
    let beat = 60.0 / 132.0
    let bar = beat * 4.0
    let bars = 32
    let tr = Track (bar * float bars)
    let roots = [|"E2"; "E2"; "C2"; "B1"; "E2"; "E2"; "F2"; "B1"|]
    let snare v = (filt (noise 0.14) (Band (900.0, 7000.0)) * envExp (int (0.14 * SR)) 0.03 + fit (ping 190.0 0.06 0.001) (int (0.14 * SR)) * 0.5) * v
    for b in 0 .. bars - 1 do
        let t0 = float b * bar
        let r = n roots[b % 8]
        for k in 0 .. 15 do
            if k % 4 = 0 || (b % 2 = 1 && List.contains k [6; 7; 14]) then tr.Add (t0 + float k * beat / 4.0, snare (if k % 4 = 0 then 1.0 else 0.6), 0.28, 0.25)
            elif rng.Random () < 0.35 then tr.Add (t0 + float k * beat / 4.0, snare 0.35, 0.2, 0.3)
        for hit in (if b % 2 = 0 then [0.0; 2.0] else [0.0; 1.5; 2.5]) do
            tr.Add (t0 + hit * beat, taiko 0.9 58.0 1.0, 0.45, -0.15)
        for (i, step) in List.indexed [0; 7; 12; 7] do
            tr.Add (t0 + float i * beat, staccato (midi (r + step)) 0.2 (900.0 + 300.0 * float (b / 8)), 0.26, -0.3)
        if b % 2 = 1 then
            for note in [r + 12; r + 15; r + 19] do
                tr.Add (t0 + 2.0 * beat, brass (midi note) (beat * 1.2) 1800.0, 0.12, 0.15)
        if b >= 16 && b % 4 = 0 then
            let crowd = filt (noise (bar * 2.0)) (Band (300.0, 2500.0)) * envAdsr (int (bar * 2.0 * SR)) 1.0 1.5
            tr.Add (t0, crowd, 0.04, rng.Uniform (-0.6, 0.6))
    tr.Finish (2.2, 0.3)

let DormSongs = ["DormSong", dormSong; "DormBossSong", dormBossSong]

// ---------------------------------------------------------------- the plant

/// The launcher's hollow thoonk, the grenade's quiet arc, then the blast where it lands, rolling round the walls.
let grenade () =
    let thoonk = mix [(0.0, tanhw (3.0 * boom 0.35 95.0) * 0.7); (0.0, filt (noise 0.12) (Band (180.0, 900.0)) * envExp (int (0.12 * SR)) 0.02 * 0.5)]
    let clack = fit (ping 1800.0 0.05 0.001) (int (0.2 * SR)) * 0.2 // the breech snapping
    let blast = tanhw (2.0 * boom 3.0 40.0) + filt (noise 3.0) (Low 900.0) * envExp (int (3.0 * SR)) 0.25
    mix [(0.0, reverb thoonk 0.5 0.2 4000.0); (0.02, clack); (0.55, reverb blast 1.8 0.35 2600.0)]

/// A valve heaved round, then steam hissing off through the pipes and the pipes knocking as it goes.
let steam () =
    let s = 3.2
    let squeal = creak 0.5 30.0 [(700.0, 1.0, 0.03)] 0.6 0.2 * envAdsr (int (0.5 * SR)) 0.05 0.2 * 0.4
    let hiss = filt (noise s) (High 2500.0) * envAdsr (int (s * SR)) 0.15 1.6 * 0.5
    let knocks = mix [for k in 0 .. 6 -> (0.4 + float k * 0.33 + rng.Uniform (-0.05, 0.05), fit (ping (rng.Uniform (140.0, 260.0)) 0.15 0.002) (int (0.8 * SR)) * (1.0 - float k * 0.1))]
    reverb (mix [(0.0, squeal); (0.3, hiss); (0.0, knocks * 0.6)]) 1.4 0.3 4000.0

/// A heavy man's wet grunt, catching in a throat that's stopped working.
let butcherGrunt () =
    let s = 1.2
    let g = voice s (sweep 92.0 74.0 s 0.6 * (1.0 + 0.04 * sinw (2.0 * Math.PI * 3.0 * tOf s))) "u" 0.4 0.8 24.0
    let g = tanhw (2.2 * g / peak (absw g)) * envAdsr g.Length 0.1 0.5
    reverb g 0.8 0.2 2600.0

/// The cleaver swung down - air, then the chop's wet bite into the block of whatever's there.
let butcherChop () =
    let chop = filt (noise 0.3) (Low 1800.0) * envExp (int (0.3 * SR)) 0.02 + fit (ping 320.0 0.05 0.002) (int (0.3 * SR)) * 0.4
    reverb (mix [(0.0, whoosh 0.32 500.0 2400.0 2.5); (0.28, chop); (0.28, 0.6 * fleshHit ())]) 0.7 0.2 4000.0

let butcherDeath () =
    let s = 2.2
    let g = voice s (sweep 96.0 40.0 s 0.6) "aw" 0.4 0.8 20.0
    let g = tanhw (2.4 * g / peak (absw g)) * envAdsr g.Length 0.05 1.2
    let clang = fit (ping 1300.0 0.4 0.002) (int (1.2 * SR)) * 0.25 // the cleaver dropping
    mix [(0.0, reverb g 0.9 0.2 2400.0); (1.1, reverb clang 0.8 0.2 4000.0); (1.3, 0.8 * reverb (boom 1.4 55.0 * 0.6) 0.6 0.15 4000.0)]

/// A pig's squeal, too big a pig's, ragged and wet.
let sowSqueal () =
    let s = 1.4
    let t = tOf s
    let f0 = (sweep 420.0 760.0 s 0.4 + 60.0 * sinw (2.0 * Math.PI * 5.0 * t)) * (1.0 + 0.03 * sinw (2.0 * Math.PI * 23.0 * t))
    let g = voice s f0 "e" 0.5 0.9 38.0 + 0.5 * voice s (f0 * 0.5) "a" 0.3 0.9 0.0
    let g = tanhw (3.0 * g / peak (absw g)) * envAdsr g.Length 0.04 0.5
    reverb (filt g (Low 5000.0)) 1.0 0.25 3000.0

/// A snorting grunt, her weight thrown up through her tusks.
let sowGore () =
    let grunt = voice 0.4 (sweep 130.0 90.0 0.4 1.0) "o" 0.7 1.0 30.0
    let grunt = tanhw (2.5 * grunt / peak (absw grunt)) * envAdsr (int (0.4 * SR)) 0.01 0.2
    let snort = filt (noise 0.25) (Band (300.0, 2500.0)) * envExp (int (0.25 * SR)) 0.06
    reverb (mix [(0.0, 0.6 * snort); (0.08, grunt); (0.2, whoosh 0.35 250.0 1000.0 2.0); (0.45, 0.5 * fleshHit ())]) 0.8 0.2 4000.0

let sowDeath () =
    let s = 2.6
    let g = voice s (sweep 700.0 160.0 s 0.5) "e" 0.5 0.9 30.0
    let g = tanhw (2.6 * g / peak (absw g)) * envAdsr g.Length 0.02 1.4
    mix [(0.0, reverb g 1.0 0.25 2800.0); (1.6, reverb (boom 2.6 38.0) 1.2 0.3 4000.0)]

/// A roar from somewhere deep in a heap of meat - three throats at once, wet and gurgling, shaking the pit.
let meatKingRoar () =
    let s = 3.2
    let t = tOf s
    let f0 = sweep 40.0 58.0 s 0.35 * (1.0 + 0.06 * sinw (2.0 * Math.PI * 3.5 * t))
    let g = voice s f0 "aw" 0.7 1.0 14.0 + 0.7 * voice s (f0 * 1.5) "o" 0.5 1.0 21.0 + 0.5 * voice s (f0 * 0.5) "u" 0.3 1.0 0.0
    let gurgle = bandpass (noise s) 380.0 2.0 * (0.5 + 0.5 * absw (sinw (2.0 * Math.PI * 9.0 * t))) * 0.5
    let g = tanhw (3.5 * (g / peak (absw g) + gurgle)) * envAdsr g.Length 0.25 1.1
    mix [(0.0, reverb (filt g (Low 3000.0)) 2.0 0.4 2200.0); (0.1, 0.4 * reverb (boom 2.5 32.0) 1.6 0.3 4000.0)]

/// Both cleavers swung down at once - their air, a scrape of steel on steel, and the wet chop landing.
let meatKingChop () =
    let ring = fit (ping 900.0 0.5 0.002) (int (1.0 * SR)) * 0.25 + fit (ping 1430.0 0.4 0.002) (int (1.0 * SR)) * 0.15
    let chop = filt (noise 0.4) (Low 1200.0) * envExp (int (0.4 * SR)) 0.03
    reverb (mix [(0.0, whoosh 0.4 250.0 1500.0 2.0); (0.06, whoosh 0.4 300.0 1800.0 2.0 * 0.8); (0.34, chop); (0.34, ring); (0.36, 0.7 * fleshHit ())]) 1.0 0.25 4000.0

/// The roar sinking into a gurgle, and then the whole heap of him coming down into the muck.
let meatKingDeath () =
    let s = 4.0
    let t = tOf s
    let g = voice s (sweep 58.0 22.0 s 0.5) "aw" 0.7 1.0 12.0 + 0.5 * voice s (sweep 87.0 33.0 s 0.5) "o" 0.5 1.0 0.0
    let gurgle = bandpass (noise s) 300.0 2.0 * (0.5 + 0.5 * absw (sinw (2.0 * Math.PI * 6.0 * t))) * 0.6
    let g = tanhw (3.0 * (g / peak (absw g) + gurgle)) * envAdsr g.Length 0.05 2.4
    let slump = filt (noise 1.5) (Low 400.0) * envExp (int (1.5 * SR)) 0.3
    mix [(0.0, reverb g 1.8 0.35 2000.0); (2.6, reverb (fit slump (int (3.0 * SR)) + boom 3.0 30.0) 1.8 0.35 4000.0)]

let PlantSounds =
    ["GrenadeShot", grenade; "Steam", steam; "ButcherGrunt", butcherGrunt; "ButcherChop", butcherChop; "ButcherDeath", butcherDeath
     "SowSqueal", sowSqueal; "SowGore", sowGore; "SowDeath", sowDeath
     "MeatKingRoar", meatKingRoar; "MeatKingChop", meatKingChop; "MeatKingDeath", meatKingDeath]

/// Not a tune - the plant still running for no one: the refrigeration's drone, the line's machinery thudding round,
/// hooks rattling along a rail, chains swinging, drips, and something groaning down in the pit.
let plantSong () =
    let seconds = 96.0
    let tr = Track seconds
    let t = tOf (seconds + 12.0)
    let drone = filt (osc (full t.Length 50.0) Saw + 0.5 * osc (full t.Length 100.6) Saw) (Band (60.0, 700.0))
    tr.Add (0.0, drone * (0.8 + 0.2 * sinw (2.0 * Math.PI * t / 13.0)), 0.03, -0.3)
    tr.Add (0.0, filt (brown (seconds + 12.0)) (Low 200.0), 0.28)
    let thud = filt (noise 0.25) (Low 150.0) * envExp (int (0.25 * SR)) 0.05 + fit (ping 95.0 0.12 0.003) (int (0.25 * SR)) * 0.4
    for k in 0 .. int (seconds / 1.6) - 1 do tr.Add (0.8 + float k * 1.6, thud, 0.12, 0.4) // the line, thudding round
    let mutable at = 2.0
    while at < seconds - 4.0 do
        let kind = rng.Random ()
        if kind < 0.3 then
            let count = rng.Integers (5, 9)
            let rattle = mix [for k in 0 .. count - 1 -> (0.09 * float k, fit (ping (rng.Uniform (1200.0, 2400.0)) 0.05 0.001) (int (0.3 * SR)) * (1.0 - 0.1 * float k))]
            tr.Add (at, filt rattle (Low 3500.0), 0.07, rng.Uniform (-0.9, 0.9)) // hooks along a rail
        elif kind < 0.55 then
            let drip = fit (ping (rng.Uniform (1200.0, 2200.0)) 0.05 0.0005) (int (0.3 * SR))
            tr.Add (at, drip, 0.18, rng.Uniform (-0.9, 0.9))
        elif kind < 0.75 then
            let c = creak (rng.Uniform (0.8, 1.4)) (rng.Uniform (30.0, 60.0)) [(rng.Uniform (500.0, 900.0), 1.0, 0.03)] 0.7 0.3
            tr.Add (at, filt (c * envAdsr c.Length 0.2 0.4) (Low 2400.0), 0.07, rng.Uniform (-0.9, 0.9)) // a chain swinging
        else
            let far = zombieGroan (rng.Integers (0, 3))
            tr.Add (at, filt far (Low 700.0), 0.08, rng.Uniform (-0.8, 0.8))
        at <- at + rng.Uniform (3.0, 7.0)
    tr.Finish (3.5, 0.45, 2200.0)

/// The sow - the line gone mad: a stamping machine beat, chains lashing, a grinding figure in B flat minor, and a
/// squeal of strings over it.
let plantBossSong () =
    let beat = 60.0 / 128.0
    let bar = beat * 4.0
    let bars = 32
    let tr = Track (bar * float bars)
    let roots = [|"Bb1"; "Bb1"; "Gb1"; "F1"; "Bb1"; "Bb1"; "B1"; "F1"|]
    let figure = [0; 12; 0; 1; 0; 12; 0; 6]
    for b in 0 .. bars - 1 do
        let t0 = float b * bar
        let r = n roots[b % 8]
        for (i, step) in List.indexed figure do
            tr.Add (t0 + float i * beat / 2.0, staccato (midi (r + step)) 0.2 (900.0 + 300.0 * float (b / 8)), 0.3, -0.2)
        for (k, hit) in List.indexed [0.0; 1.0; 2.0; 3.0] do
            tr.Add (t0 + hit * beat, taiko 0.8 (if k % 2 = 0 then 55.0 else 75.0) (if k % 2 = 0 then 1.0 else 0.6), 0.45, -0.1)
        let lash = filt (noise 0.2) (Band (1500.0, 7000.0)) * envExp (int (0.2 * SR)) 0.04 + fit (ping (rng.Uniform (1800.0, 2600.0)) 0.08 0.001) (int (0.2 * SR)) * 0.3
        for hit in (if b % 2 = 1 then [1.5; 3.5] else [3.5]) do
            tr.Add (t0 + hit * beat, lash, 0.18, 0.4)
        if b >= 8 && b % 4 = 0 then
            let f = sweep (midi (r + 34)) (midi (r + 39)) (bar * 2.0) 0.6
            let strings = sumw [for d in [-0.005; 0.0; 0.006] -> osc (f * (1.0 + d)) Saw]
            tr.Add (t0, filt strings (Low 3500.0) * envAdsr strings.Length 0.4 1.2, 0.035, rng.Uniform (-0.5, 0.5))
    tr.Finish (2.0, 0.3)

/// The meat king - a slow, crushing doom in C minor, a choir of the dead moaning over it, the drums falling like
/// cleavers; raging, the same figure driven twice as hard, the choir screaming and a siren climbing.
let finalBossSong raging =
    let beat = 60.0 / (if raging then 150.0 else 92.0)
    let bar = beat * 4.0
    let bars = if raging then 32 else 16
    let tr = Track (bar * float bars)
    let roots = [|"C2"; "C2"; "Ab1"; "G1"; "C2"; "Db2"; "Ab1"; "G1"|]
    for b in 0 .. bars - 1 do
        let t0 = float b * bar
        let r = n roots[b % 8]
        let notes = chordNotes r (if b % 8 <> 2 && b % 8 <> 5 then "m" else "M")
        if raging then
            for (i, step) in List.indexed [0; 0; 12; 0; 1; 0; 7; 6] do
                tr.Add (t0 + float i * beat / 2.0, staccato (midi (r + step)) 0.18 1400.0, 0.32, -0.2)
            for (k, hit) in List.indexed [0.0; 0.5; 1.0; 2.0; 2.5; 3.0] do
                tr.Add (t0 + hit * beat, taiko 0.8 (if k % 3 = 0 then 58.0 else 85.0) (if k % 3 = 0 then 1.0 else 0.7), 0.48, -0.1 + 0.2 * float (k % 2))
        else
            tr.Add (t0, brass (midi r) (bar * 0.9) 1100.0, 0.3, -0.2)
            tr.Add (t0, brass (midi (r + 7)) (bar * 0.9) 1100.0, 0.18, 0.2)
            for hit in [0.0; 2.5] do
                tr.Add (t0 + hit * beat, boom 1.6 34.0, 0.5)
            tr.Add (t0 + 3.0 * beat, taiko 1.0 60.0 0.8, 0.35, 0.1)
        if b % 2 = 0 then
            tr.Add (t0, choir (midi (notes[1] + 12)) (bar * 2.0 + 1.0) (if raging then "a" else "o"), (if raging then 0.16 else 0.12), -0.4)
            tr.Add (t0, choir (midi (notes[2] + 12)) (bar * 2.0 + 1.0) (if raging then "a" else "u"), (if raging then 0.12 else 0.09), 0.4)
        let clang = fit (ping (rng.Uniform (300.0, 520.0)) 0.5 0.002) (int (1.2 * SR))
        tr.Add (t0 + 3.5 * beat, clang, 0.22, rng.Uniform (-0.5, 0.5))
        if raging && b % 4 = 0 then
            let siren = osc (sweep (midi (r + 31)) (midi (r + 38)) (bar * 4.0) 0.7) Tri
            tr.Add (t0, filt siren (Low 3200.0) * envAdsr siren.Length 0.5 1.0, 0.06, rng.Uniform (-0.5, 0.5))
    tr.Finish (2.4, 0.35)

let PlantSongs =
    ["PlantSong", plantSong; "PlantBossSong", plantBossSong
     "FinalBossSong", (fun () -> finalBossSong false); "FinalBossRageSong", (fun () -> finalBossSong true)]

// ---------------------------------------------------------------- gore - its own draws, so adding to it changes nothing else

/// A head shot off - a wet crack, the burst of it, and its bits pattering down round about.
let headBurst () =
    let s = 0.9
    let t = tOf 0.3
    let crack = filt (noise 0.03) (Band (1500.0, 7000.0)) * envExp (int (0.03 * SR)) 0.004
    let burst = filt (noise 0.3) (Low 900.0) * envExp (int (0.3 * SR)) 0.05 + sinw (2.0 * Math.PI * 80.0 * t) * expw (-t / 0.08)
    let squelch = bandpass (noise 0.3) 500.0 2.5 * envExp (int (0.3 * SR)) 0.08 * 0.6
    let times = Array.sort (rng.Uniform (0.18, 0.75, 9)).Samples
    let splats = [for (o, a) in Array.zip times (linspace 0.5 0.15 9).Samples -> (o, filt (noise 0.04) (Band (500.0, 2500.0)) * envExp (int (0.04 * SR)) 0.012 * a)]
    reverb (mixIn s ([(0.0, crack); (0.004, crack * 0.6); (0.0, burst + squelch)] @ splats)) 0.5 0.15 4000.0

let GoreSounds = ["HeadBurst", headBurst]

// ---------------------------------------------------------------- the step - its own draws, quiet, as it's heard all the time

/// A soft sole set down on linoleum - a muffled thud, a dull knock under it, and the faintest scuff, in a small room.
let footstep () =
    let s = 0.2
    let count = int (s * SR)
    let thud = filt (noise s) (Low 300.0) * envExp count 0.016
    let knock = fit (ping 170.0 0.025 0.003) count * 0.3
    let scuff = filt (noise s) (Band (900.0, 2200.0)) * envExp count 0.006 * 0.06
    reverb (thud + knock + scuff) 0.4 0.1 2200.0

let StepSounds = ["Footstep", footstep]
let StepLevel = 0.3 // well under the others' - it's underfoot the whole game

// ---------------------------------------------------------------- making it all

// the hospital's draw on the generator in turn, as they always have; each later stage's reseeded by name, so adding to
// one stage changes nothing of another
for make in RetiredSounds do make () |> ignore
for (name, make) in Sounds do
    writeSound name (make ()) 0.89
    printfn "sound %s" name
for (name, make) in Songs do
    let (left, right) = make ()
    writeSong name left right
    printfn "song %s %.1f s" name (float left.Length / SR)
for (sounds, songs, level) in [(DormSounds, DormSongs, 0.89); (PlantSounds, PlantSongs, 0.89); (GoreSounds, [], 0.89); (StepSounds, [], StepLevel)] do
    for (name, make) in sounds do
        rng <- Rng (crc32 name)
        writeSound name (make ()) level
        printfn "sound %s" name
    for (name, make) in songs do
        rng <- Rng (crc32 name)
        let (left, right) = make ()
        writeSong name left right
        printfn "song %s %.1f s" name (float left.Length / SR)
