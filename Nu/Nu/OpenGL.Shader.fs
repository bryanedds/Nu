// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.IO
open Nu

[<RequireQualifiedAccess>]
module Shader =

    /// Create a shader from vertex and fragment code strings.
    let CreateShaderFromStrs (vertexShaderStr, fragmentShaderStr) =

        // construct gl shader
        let shader = OpenGL.Gl.CreateProgram ()
        OpenGL.Hl.Assert ()

        // add vertex shader
        let vertexShader = OpenGL.Gl.CreateShader OpenGL.ShaderType.VertexShader
        OpenGL.Gl.ShaderSource (vertexShader, [|vertexShaderStr|], null)
        OpenGL.Gl.CompileShader vertexShader
        OpenGL.Gl.AttachShader (shader, vertexShader)
        OpenGL.Hl.Assert ()

        // add fragement shader to program
        let fragmentShader = OpenGL.Gl.CreateShader OpenGL.ShaderType.FragmentShader
        OpenGL.Gl.ShaderSource (fragmentShader, [|fragmentShaderStr|], null)
        OpenGL.Gl.CompileShader fragmentShader
        OpenGL.Gl.AttachShader (shader, fragmentShader)
        OpenGL.Hl.Assert ()

        // link shader
        OpenGL.Gl.LinkProgram shader
        shader

    /// Create a shader from a vertex stream and a fragment stream.
    let CreateShaderFromStreams (vertexStream : StreamReader, fragmentStream : StreamReader) =
        let vertexStr = vertexStream.ReadToEnd ()
        let fragmentStr = fragmentStream.ReadToEnd ()
        CreateShaderFromStrs (vertexStr, fragmentStr)

    /// Create a shader from a vertex file and a fragment file.
    let CreateShaderFromFilePaths (vertexFilePath : string, fragmentFilePath : string) =
        use vertexStream = new StreamReader (File.OpenRead vertexFilePath)
        use fragmentStream = new StreamReader (File.OpenRead fragmentFilePath)
        CreateShaderFromStreams (vertexStream, fragmentStream)

    /// Create a shader from a single file with both a '#shader vertex' section and a '#shader fragment' section.
    let CreateShaderFromFilePath (shaderFilePath : string) =
        use shaderStream = new StreamReader (File.OpenRead shaderFilePath)
        let shaderStr = shaderStream.ReadToEnd ()
        let vertexStrIndex = shaderStr.IndexOf "#shader vertex"
        let fragmentStrIndex = shaderStr.IndexOf "#shader fragment"
        if vertexStrIndex > -1 && fragmentStrIndex > -1 then
            let (vertexStr, fragmentStr) =
                if vertexStrIndex < fragmentStrIndex then
                    (shaderStr.Substring (vertexStrIndex, fragmentStrIndex - vertexStrIndex),
                     shaderStr.Substring (fragmentStrIndex, shaderStr.Length - fragmentStrIndex))
                else
                    (shaderStr.Substring (fragmentStrIndex, vertexStrIndex - fragmentStrIndex),
                     shaderStr.Substring (vertexStrIndex, shaderStr.Length - vertexStrIndex))
            CreateShaderFromStrs (vertexStr.Replace ("#shader vertex", ""), fragmentStr.Replace ("#shader fragment", ""))
        else failwith ("Invalid shader file '" + shaderFilePath + "'. Both vertex and fragment shader sections required.")