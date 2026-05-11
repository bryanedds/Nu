#if ANDROID || IOS
namespace SandBox2d
#else
let [<EntryPoint>] main _ = SandBox2d.Program.main ()
#endif