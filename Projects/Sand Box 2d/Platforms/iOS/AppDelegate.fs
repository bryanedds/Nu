open SDL
open FSharp.NativeInterop
open System.Runtime.InteropServices

// SDL usage taken from https://github.com/ppy/SDL3-CS/blob/master/SDL3-CS.Tests.iOS/Main.cs
type SdlMain = delegate of argc : int * argv : byte nativeptr nativeptr -> int

let private sdlMain = SdlMain (fun _ _ -> SandBox2d.Program.main ())
   
let [<EntryPoint>] main args =
    NativeLibrary.SetDllImportResolver (typeof<SDL3>.Assembly, fun _ assembly path -> NativeLibrary.Load ("Frameworks/SDL3.framework/SDL3", assembly, path))
    NativeLibrary.SetDllImportResolver (typeof<SDL3_image>.Assembly, fun _ assembly path -> NativeLibrary.Load ("Frameworks/SDL3_image.framework/SDL3_image", assembly, path))
    NativeLibrary.SetDllImportResolver (typeof<SDL3_ttf>.Assembly, fun _ assembly path -> NativeLibrary.Load ("Frameworks/SDL3_ttf.framework/SDL3_ttf", assembly, path))
    NativeLibrary.SetDllImportResolver (typeof<SDL3_mixer>.Assembly, fun _ assembly path -> NativeLibrary.Load ("Frameworks/SDL3_mixer.framework/SDL3_mixer", assembly, path))
    if ObjCRuntime.Runtime.Arch = ObjCRuntime.Arch.SIMULATOR then
        let result = Vortice.Vulkan.Vulkan.vkInitialize "Frameworks/MoltenVK.framework/MoltenVK"
        assert (result = Vortice.Vulkan.VkResult.Success)

    SDL3.SDL_RunApp (0, NativePtr.nullPtr, Marshal.GetFunctionPointerForDelegate<_> sdlMain, 0n)