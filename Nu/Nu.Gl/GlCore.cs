using System;
using System.Runtime.InteropServices;

namespace Nu
{
    partial class Gl
    {
        internal static partial class NativeMethods
        {
            [DllImport(Gl.Library, EntryPoint = "glActiveShaderProgram", ExactSpelling = true)]
            internal extern static void ActiveShaderProgram(UInt32 pipeline, UInt32 program);
            [DllImport(Gl.Library, EntryPoint = "glActiveTexture", ExactSpelling = true)]
            internal extern static void ActiveTexture(Int32 texture);
            [DllImport(Gl.Library, EntryPoint = "glAttachShader", ExactSpelling = true)]
            internal extern static void AttachShader(UInt32 program, UInt32 shader);
            [DllImport(Gl.Library, EntryPoint = "glBeginConditionalRender", ExactSpelling = true)]
            internal extern static void BeginConditionalRender(UInt32 id, ConditionalRenderType mode);
            [DllImport(Gl.Library, EntryPoint = "glEndConditionalRender", ExactSpelling = true)]
            internal extern static void EndConditionalRender();
            [DllImport(Gl.Library, EntryPoint = "glBeginQuery", ExactSpelling = true)]
            internal extern static void BeginQuery(QueryTarget target, UInt32 id);
            [DllImport(Gl.Library, EntryPoint = "glEndQuery", ExactSpelling = true)]
            internal extern static void EndQuery(QueryTarget target);
            [DllImport(Gl.Library, EntryPoint = "glBeginQueryIndexed", ExactSpelling = true)]
            internal extern static void BeginQueryIndexed(QueryTarget target, UInt32 index, UInt32 id);
            [DllImport(Gl.Library, EntryPoint = "glEndQueryIndexed", ExactSpelling = true)]
            internal extern static void EndQueryIndexed(QueryTarget target, UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glBeginTransformFeedback", ExactSpelling = true)]
            internal extern static void BeginTransformFeedback(BeginFeedbackMode primitiveMode);
            [DllImport(Gl.Library, EntryPoint = "glEndTransformFeedback", ExactSpelling = true)]
            internal extern static void EndTransformFeedback();
            [DllImport(Gl.Library, EntryPoint = "glBindAttribLocation", ExactSpelling = true)]
            internal extern static void BindAttribLocation(UInt32 program, UInt32 index, String name);
            [DllImport(Gl.Library, EntryPoint = "glBindBuffer", ExactSpelling = true)]
            internal extern static void BindBuffer(BufferTarget target, UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glBindBufferBase", ExactSpelling = true)]
            internal extern static void BindBufferBase(BufferTarget target, UInt32 index, UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glBindBufferRange", ExactSpelling = true)]
            internal extern static void BindBufferRange(BufferTarget target, UInt32 index, UInt32 buffer, IntPtr offset, IntPtr size);
            [DllImport(Gl.Library, EntryPoint = "glBindBuffersBase", ExactSpelling = true)]
            internal extern static void BindBuffersBase(BufferTarget target, UInt32 first, Int32 count, UInt32[] buffers);
            [DllImport(Gl.Library, EntryPoint = "glBindBuffersRange", ExactSpelling = true)]
            internal extern static void BindBuffersRange(BufferTarget target, UInt32 first, Int32 count, UInt32[] buffers, IntPtr[] offsets, IntPtr[] sizes);
            [DllImport(Gl.Library, EntryPoint = "glBindFragDataLocation", ExactSpelling = true)]
            internal extern static void BindFragDataLocation(UInt32 program, UInt32 colorNumber, String name);
            [DllImport(Gl.Library, EntryPoint = "glBindFragDataLocationIndexed", ExactSpelling = true)]
            internal extern static void BindFragDataLocationIndexed(UInt32 program, UInt32 colorNumber, UInt32 index, String name);
            [DllImport(Gl.Library, EntryPoint = "glBindFramebuffer", ExactSpelling = true)]
            internal extern static void BindFramebuffer(FramebufferTarget target, UInt32 framebuffer);
            [DllImport(Gl.Library, EntryPoint = "glBindImageTexture", ExactSpelling = true)]
            internal extern static void BindImageTexture(UInt32 unit, UInt32 texture, Int32 level, Boolean layered, Int32 layer, BufferAccess access, PixelInternalFormat format);
            [DllImport(Gl.Library, EntryPoint = "glBindImageTextures", ExactSpelling = true)]
            internal extern static void BindImageTextures(UInt32 first, Int32 count, UInt32[] textures);
            [DllImport(Gl.Library, EntryPoint = "glBindProgramPipeline", ExactSpelling = true)]
            internal extern static void BindProgramPipeline(UInt32 pipeline);
            [DllImport(Gl.Library, EntryPoint = "glBindRenderbuffer", ExactSpelling = true)]
            internal extern static void BindRenderbuffer(RenderbufferTarget target, UInt32 renderbuffer);
            [DllImport(Gl.Library, EntryPoint = "glBindSampler", ExactSpelling = true)]
            internal extern static void BindSampler(UInt32 unit, UInt32 sampler);
            [DllImport(Gl.Library, EntryPoint = "glBindSamplers", ExactSpelling = true)]
            internal extern static void BindSamplers(UInt32 first, Int32 count, UInt32[] samplers);
            [DllImport(Gl.Library, EntryPoint = "glBindTexture", ExactSpelling = true)]
            internal extern static void BindTexture(TextureTarget target, UInt32 texture);
            [DllImport(Gl.Library, EntryPoint = "glBindTextures", ExactSpelling = true)]
            internal extern static void BindTextures(UInt32 first, Int32 count, UInt32[] textures);
            [DllImport(Gl.Library, EntryPoint = "glBindTextureUnit", ExactSpelling = true)]
            internal extern static void BindTextureUnit(UInt32 unit, UInt32 texture);
            [DllImport(Gl.Library, EntryPoint = "glBindTransformFeedback", ExactSpelling = true)]
            internal extern static void BindTransformFeedback(NvTransformFeedback2 target, UInt32 id);
            [DllImport(Gl.Library, EntryPoint = "glBindVertexArray", ExactSpelling = true)]
            internal extern static void BindVertexArray(UInt32 array);
            [DllImport(Gl.Library, EntryPoint = "glBindVertexBuffer", ExactSpelling = true)]
            internal extern static void BindVertexBuffer(UInt32 bindingindex, UInt32 buffer, IntPtr offset, IntPtr stride);
            [DllImport(Gl.Library, EntryPoint = "glVertexArrayVertexBuffer", ExactSpelling = true)]
            internal extern static void VertexArrayVertexBuffer(UInt32 vaobj, UInt32 bindingindex, UInt32 buffer, IntPtr offset, Int32 stride);
            [DllImport(Gl.Library, EntryPoint = "glBindVertexBuffers", ExactSpelling = true)]
            internal extern static void BindVertexBuffers(UInt32 first, Int32 count, UInt32[] buffers, IntPtr[] offsets, Int32[] strides);
            [DllImport(Gl.Library, EntryPoint = "glVertexArrayVertexBuffers", ExactSpelling = true)]
            internal extern static void VertexArrayVertexBuffers(UInt32 vaobj, UInt32 first, Int32 count, UInt32[] buffers, IntPtr[] offsets, Int32[] strides);
            [DllImport(Gl.Library, EntryPoint = "glBlendColor", ExactSpelling = true)]
            internal extern static void BlendColor(Single red, Single green, Single blue, Single alpha);
            [DllImport(Gl.Library, EntryPoint = "glBlendEquation", ExactSpelling = true)]
            internal extern static void BlendEquation(BlendEquationMode mode);
            [DllImport(Gl.Library, EntryPoint = "glBlendEquationi", ExactSpelling = true)]
            internal extern static void BlendEquationi(UInt32 buf, BlendEquationMode mode);
            [DllImport(Gl.Library, EntryPoint = "glBlendEquationSeparate", ExactSpelling = true)]
            internal extern static void BlendEquationSeparate(BlendEquationMode modeRGB, BlendEquationMode modeAlpha);
            [DllImport(Gl.Library, EntryPoint = "glBlendEquationSeparatei", ExactSpelling = true)]
            internal extern static void BlendEquationSeparatei(UInt32 buf, BlendEquationMode modeRGB, BlendEquationMode modeAlpha);
            [DllImport(Gl.Library, EntryPoint = "glBlendFunc", ExactSpelling = true)]
            internal extern static void BlendFunc(BlendingFactorSrc sfactor, BlendingFactorDest dfactor);
            [DllImport(Gl.Library, EntryPoint = "glBlendFunci", ExactSpelling = true)]
            internal extern static void BlendFunci(UInt32 buf, BlendingFactorSrc sfactor, BlendingFactorDest dfactor);
            [DllImport(Gl.Library, EntryPoint = "glBlendFuncSeparate", ExactSpelling = true)]
            internal extern static void BlendFuncSeparate(BlendingFactorSrc srcRGB, BlendingFactorDest dstRGB, BlendingFactorSrc srcAlpha, BlendingFactorDest dstAlpha);
            [DllImport(Gl.Library, EntryPoint = "glBlendFuncSeparatei", ExactSpelling = true)]
            internal extern static void BlendFuncSeparatei(UInt32 buf, BlendingFactorSrc srcRGB, BlendingFactorDest dstRGB, BlendingFactorSrc srcAlpha, BlendingFactorDest dstAlpha);
            [DllImport(Gl.Library, EntryPoint = "glBlitFramebuffer", ExactSpelling = true)]
            internal extern static void BlitFramebuffer(Int32 srcX0, Int32 srcY0, Int32 srcX1, Int32 srcY1, Int32 dstX0, Int32 dstY0, Int32 dstX1, Int32 dstY1, ClearBufferMask mask, BlitFramebufferFilter filter);
            [DllImport(Gl.Library, EntryPoint = "glBlitNamedFramebuffer", ExactSpelling = true)]
            internal extern static void BlitNamedFramebuffer(UInt32 readFramebuffer, UInt32 drawFramebuffer, Int32 srcX0, Int32 srcY0, Int32 srcX1, Int32 srcY1, Int32 dstX0, Int32 dstY0, Int32 dstX1, Int32 dstY1, ClearBufferMask mask, BlitFramebufferFilter filter);
            [DllImport(Gl.Library, EntryPoint = "glBufferData", ExactSpelling = true)]
            internal extern static void BufferData(BufferTarget target, IntPtr size, IntPtr data, BufferUsageHint usage);
            [DllImport(Gl.Library, EntryPoint = "glNamedBufferData", ExactSpelling = true)]
            internal extern static void NamedBufferData(UInt32 buffer, Int32 size, IntPtr data, BufferUsageHint usage);
            [DllImport(Gl.Library, EntryPoint = "glBufferStorage", ExactSpelling = true)]
            internal extern static void BufferStorage(BufferTarget target, IntPtr size, IntPtr data, UInt32 flags);
            [DllImport(Gl.Library, EntryPoint = "glNamedBufferStorage", ExactSpelling = true)]
            internal extern static void NamedBufferStorage(UInt32 buffer, Int32 size, IntPtr data, UInt32 flags);
            [DllImport(Gl.Library, EntryPoint = "glBufferSubData", ExactSpelling = true)]
            internal extern static void BufferSubData(BufferTarget target, IntPtr offset, IntPtr size, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glNamedBufferSubData", ExactSpelling = true)]
            internal extern static void NamedBufferSubData(UInt32 buffer, IntPtr offset, Int32 size, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCheckFramebufferStatus", ExactSpelling = true)]
            internal extern static FramebufferErrorCode CheckFramebufferStatus(FramebufferTarget target);
            [DllImport(Gl.Library, EntryPoint = "glCheckNamedFramebufferStatus", ExactSpelling = true)]
            internal extern static FramebufferErrorCode CheckNamedFramebufferStatus(UInt32 framebuffer, FramebufferTarget target);
            [DllImport(Gl.Library, EntryPoint = "glClampColor", ExactSpelling = true)]
            internal extern static void ClampColor(ClampColorTarget target, ClampColorMode clamp);
            [DllImport(Gl.Library, EntryPoint = "glClear", ExactSpelling = true)]
            internal extern static void Clear(ClearBufferMask mask);
            [DllImport(Gl.Library, EntryPoint = "glClearBufferiv", ExactSpelling = true)]
            internal extern static void ClearBufferiv(ClearBuffer buffer, Int32 drawbuffer, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glClearBufferuiv", ExactSpelling = true)]
            internal extern static void ClearBufferuiv(ClearBuffer buffer, Int32 drawbuffer, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glClearBufferfv", ExactSpelling = true)]
            internal extern static void ClearBufferfv(ClearBuffer buffer, Int32 drawbuffer, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glClearBufferfi", ExactSpelling = true)]
            internal extern static void ClearBufferfi(ClearBuffer buffer, Int32 drawbuffer, Single depth, Int32 stencil);
            [DllImport(Gl.Library, EntryPoint = "glClearNamedFramebufferiv", ExactSpelling = true)]
            internal extern static void ClearNamedFramebufferiv(UInt32 framebuffer, ClearBuffer buffer, Int32 drawbuffer, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glClearNamedFramebufferuiv", ExactSpelling = true)]
            internal extern static void ClearNamedFramebufferuiv(UInt32 framebuffer, ClearBuffer buffer, Int32 drawbuffer, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glClearNamedFramebufferfv", ExactSpelling = true)]
            internal extern static void ClearNamedFramebufferfv(UInt32 framebuffer, ClearBuffer buffer, Int32 drawbuffer, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glClearNamedFramebufferfi", ExactSpelling = true)]
            internal extern static void ClearNamedFramebufferfi(UInt32 framebuffer, ClearBuffer buffer, Int32 drawbuffer, Single depth, Int32 stencil);
            [DllImport(Gl.Library, EntryPoint = "glClearBufferData", ExactSpelling = true)]
            internal extern static void ClearBufferData(BufferTarget target, SizedInternalFormat internalFormat, PixelInternalFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glClearNamedBufferData", ExactSpelling = true)]
            internal extern static void ClearNamedBufferData(UInt32 buffer, SizedInternalFormat internalFormat, PixelInternalFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glClearBufferSubData", ExactSpelling = true)]
            internal extern static void ClearBufferSubData(BufferTarget target, SizedInternalFormat internalFormat, IntPtr offset, IntPtr size, PixelInternalFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glClearNamedBufferSubData", ExactSpelling = true)]
            internal extern static void ClearNamedBufferSubData(UInt32 buffer, SizedInternalFormat internalFormat, IntPtr offset, Int32 size, PixelInternalFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glClearColor", ExactSpelling = true)]
            internal extern static void ClearColor(Single red, Single green, Single blue, Single alpha);
            [DllImport(Gl.Library, EntryPoint = "glClearDepth", ExactSpelling = true)]
            internal extern static void ClearDepth(Double depth);
            [DllImport(Gl.Library, EntryPoint = "glClearDepthf", ExactSpelling = true)]
            internal extern static void ClearDepthf(Single depth);
            [DllImport(Gl.Library, EntryPoint = "glClearStencil", ExactSpelling = true)]
            internal extern static void ClearStencil(Int32 s);
            [DllImport(Gl.Library, EntryPoint = "glClearTexImage", ExactSpelling = true)]
            internal extern static void ClearTexImage(UInt32 texture, Int32 level, PixelInternalFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glClearTexSubImage", ExactSpelling = true)]
            internal extern static void ClearTexSubImage(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelInternalFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glClientWaitSync", ExactSpelling = true)]
            internal extern static ArbSync ClientWaitSync(IntPtr sync, UInt32 flags, UInt64 timeout);
            [DllImport(Gl.Library, EntryPoint = "glClipControl", ExactSpelling = true)]
            internal extern static void ClipControl(ClipControlOrigin origin, ClipControlDepth depth);
            [DllImport(Gl.Library, EntryPoint = "glColorMask", ExactSpelling = true)]
            internal extern static void ColorMask(Boolean red, Boolean green, Boolean blue, Boolean alpha);
            [DllImport(Gl.Library, EntryPoint = "glColorMaski", ExactSpelling = true)]
            internal extern static void ColorMaski(UInt32 buf, Boolean red, Boolean green, Boolean blue, Boolean alpha);
            [DllImport(Gl.Library, EntryPoint = "glCompileShader", ExactSpelling = true)]
            internal extern static void CompileShader(UInt32 shader);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTexImage1D", ExactSpelling = true)]
            internal extern static void CompressedTexImage1D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 border, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTexImage2D", ExactSpelling = true)]
            internal extern static void CompressedTexImage2D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 border, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTexImage3D", ExactSpelling = true)]
            internal extern static void CompressedTexImage3D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Int32 border, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTexSubImage1D", ExactSpelling = true)]
            internal extern static void CompressedTexSubImage1D(TextureTarget target, Int32 level, Int32 xoffset, Int32 width, PixelFormat format, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTextureSubImage1D", ExactSpelling = true)]
            internal extern static void CompressedTextureSubImage1D(UInt32 texture, Int32 level, Int32 xoffset, Int32 width, PixelInternalFormat format, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTexSubImage2D", ExactSpelling = true)]
            internal extern static void CompressedTexSubImage2D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelFormat format, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTextureSubImage2D", ExactSpelling = true)]
            internal extern static void CompressedTextureSubImage2D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelInternalFormat format, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTexSubImage3D", ExactSpelling = true)]
            internal extern static void CompressedTexSubImage3D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelFormat format, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCompressedTextureSubImage3D", ExactSpelling = true)]
            internal extern static void CompressedTextureSubImage3D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelInternalFormat format, Int32 imageSize, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glCopyBufferSubData", ExactSpelling = true)]
            internal extern static void CopyBufferSubData(BufferTarget readTarget, BufferTarget writeTarget, IntPtr readOffset, IntPtr writeOffset, IntPtr size);
            [DllImport(Gl.Library, EntryPoint = "glCopyNamedBufferSubData", ExactSpelling = true)]
            internal extern static void CopyNamedBufferSubData(UInt32 readBuffer, UInt32 writeBuffer, IntPtr readOffset, IntPtr writeOffset, Int32 size);
            [DllImport(Gl.Library, EntryPoint = "glCopyImageSubData", ExactSpelling = true)]
            internal extern static void CopyImageSubData(UInt32 srcName, BufferTarget srcTarget, Int32 srcLevel, Int32 srcX, Int32 srcY, Int32 srcZ, UInt32 dstName, BufferTarget dstTarget, Int32 dstLevel, Int32 dstX, Int32 dstY, Int32 dstZ, Int32 srcWidth, Int32 srcHeight, Int32 srcDepth);
            [DllImport(Gl.Library, EntryPoint = "glCopyTexImage1D", ExactSpelling = true)]
            internal extern static void CopyTexImage1D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 x, Int32 y, Int32 width, Int32 border);
            [DllImport(Gl.Library, EntryPoint = "glCopyTexImage2D", ExactSpelling = true)]
            internal extern static void CopyTexImage2D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 x, Int32 y, Int32 width, Int32 height, Int32 border);
            [DllImport(Gl.Library, EntryPoint = "glCopyTexSubImage1D", ExactSpelling = true)]
            internal extern static void CopyTexSubImage1D(TextureTarget target, Int32 level, Int32 xoffset, Int32 x, Int32 y, Int32 width);
            [DllImport(Gl.Library, EntryPoint = "glCopyTextureSubImage1D", ExactSpelling = true)]
            internal extern static void CopyTextureSubImage1D(UInt32 texture, Int32 level, Int32 xoffset, Int32 x, Int32 y, Int32 width);
            [DllImport(Gl.Library, EntryPoint = "glCopyTexSubImage2D", ExactSpelling = true)]
            internal extern static void CopyTexSubImage2D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 x, Int32 y, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glCopyTextureSubImage2D", ExactSpelling = true)]
            internal extern static void CopyTextureSubImage2D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 x, Int32 y, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glCopyTexSubImage3D", ExactSpelling = true)]
            internal extern static void CopyTexSubImage3D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 x, Int32 y, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glCopyTextureSubImage3D", ExactSpelling = true)]
            internal extern static void CopyTextureSubImage3D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 x, Int32 y, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glCreateBuffers", ExactSpelling = true)]
            internal extern static void CreateBuffers(Int32 n, UInt32[] buffers);
            [DllImport(Gl.Library, EntryPoint = "glCreateFramebuffers", ExactSpelling = true)]
            internal extern static void CreateFramebuffers(Int32 n, UInt32[] ids);
            [DllImport(Gl.Library, EntryPoint = "glCreateProgram", ExactSpelling = true)]
            internal extern static UInt32 CreateProgram();
            [DllImport(Gl.Library, EntryPoint = "glCreateProgramPipelines", ExactSpelling = true)]
            internal extern static void CreateProgramPipelines(Int32 n, UInt32[] pipelines);
            [DllImport(Gl.Library, EntryPoint = "glCreateQueries", ExactSpelling = true)]
            internal extern static void CreateQueries(QueryTarget target, Int32 n, UInt32[] ids);
            [DllImport(Gl.Library, EntryPoint = "glCreateRenderbuffers", ExactSpelling = true)]
            internal extern static void CreateRenderbuffers(Int32 n, UInt32[] renderbuffers);
            [DllImport(Gl.Library, EntryPoint = "glCreateSamplers", ExactSpelling = true)]
            internal extern static void CreateSamplers(Int32 n, UInt32[] samplers);
            [DllImport(Gl.Library, EntryPoint = "glCreateShader", ExactSpelling = true)]
            internal extern static UInt32 CreateShader(ShaderType shaderType);
            [DllImport(Gl.Library, EntryPoint = "glCreateShaderProgramv", ExactSpelling = true)]
            internal extern static UInt32 CreateShaderProgramv(ShaderType type, Int32 count, String strings);
            [DllImport(Gl.Library, EntryPoint = "glCreateTextures", ExactSpelling = true)]
            internal extern static void CreateTextures(TextureTarget target, Int32 n, UInt32[] textures);
            [DllImport(Gl.Library, EntryPoint = "glCreateTransformFeedbacks", ExactSpelling = true)]
            internal extern static void CreateTransformFeedbacks(Int32 n, UInt32[] ids);
            [DllImport(Gl.Library, EntryPoint = "glCreateVertexArrays", ExactSpelling = true)]
            internal extern static void CreateVertexArrays(Int32 n, UInt32[] arrays);
            [DllImport(Gl.Library, EntryPoint = "glCullFace", ExactSpelling = true)]
            internal extern static void CullFace(CullFaceMode mode);
            [DllImport(Gl.Library, EntryPoint = "glDeleteBuffers", ExactSpelling = true)]
            internal extern static void DeleteBuffers(Int32 n, UInt32[] buffers);
            [DllImport(Gl.Library, EntryPoint = "glDeleteFramebuffers", ExactSpelling = true)]
            internal extern static void DeleteFramebuffers(Int32 n, UInt32[] framebuffers);
            [DllImport(Gl.Library, EntryPoint = "glDeleteProgram", ExactSpelling = true)]
            internal extern static void DeleteProgram(UInt32 program);
            [DllImport(Gl.Library, EntryPoint = "glDeleteProgramPipelines", ExactSpelling = true)]
            internal extern static void DeleteProgramPipelines(Int32 n, UInt32[] pipelines);
            [DllImport(Gl.Library, EntryPoint = "glDeleteQueries", ExactSpelling = true)]
            internal extern static void DeleteQueries(Int32 n, UInt32[] ids);
            [DllImport(Gl.Library, EntryPoint = "glDeleteRenderbuffers", ExactSpelling = true)]
            internal extern static void DeleteRenderbuffers(Int32 n, UInt32[] renderbuffers);
            [DllImport(Gl.Library, EntryPoint = "glDeleteSamplers", ExactSpelling = true)]
            internal extern static void DeleteSamplers(Int32 n, UInt32[] samplers);
            [DllImport(Gl.Library, EntryPoint = "glDeleteShader", ExactSpelling = true)]
            internal extern static void DeleteShader(UInt32 shader);
            [DllImport(Gl.Library, EntryPoint = "glDeleteSync", ExactSpelling = true)]
            internal extern static void DeleteSync(IntPtr sync);
            [DllImport(Gl.Library, EntryPoint = "glDeleteTextures", ExactSpelling = true)]
            internal extern static void DeleteTextures(Int32 n, UInt32[] textures);
            [DllImport(Gl.Library, EntryPoint = "glDeleteTransformFeedbacks", ExactSpelling = true)]
            internal extern static void DeleteTransformFeedbacks(Int32 n, UInt32[] ids);
            [DllImport(Gl.Library, EntryPoint = "glDeleteVertexArrays", ExactSpelling = true)]
            internal extern static void DeleteVertexArrays(Int32 n, UInt32[] arrays);
            [DllImport(Gl.Library, EntryPoint = "glDepthFunc", ExactSpelling = true)]
            internal extern static void DepthFunc(DepthFunction func);
            [DllImport(Gl.Library, EntryPoint = "glDepthMask", ExactSpelling = true)]
            internal extern static void DepthMask(Boolean flag);
            [DllImport(Gl.Library, EntryPoint = "glDepthRange", ExactSpelling = true)]
            internal extern static void DepthRange(Double nearVal, Double farVal);
            [DllImport(Gl.Library, EntryPoint = "glDepthRangef", ExactSpelling = true)]
            internal extern static void DepthRangef(Single nearVal, Single farVal);
            [DllImport(Gl.Library, EntryPoint = "glDepthRangeArrayv", ExactSpelling = true)]
            internal extern static void DepthRangeArrayv(UInt32 first, Int32 count, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glDepthRangeIndexed", ExactSpelling = true)]
            internal extern static void DepthRangeIndexed(UInt32 index, Double nearVal, Double farVal);
            [DllImport(Gl.Library, EntryPoint = "glDetachShader", ExactSpelling = true)]
            internal extern static void DetachShader(UInt32 program, UInt32 shader);
            [DllImport(Gl.Library, EntryPoint = "glDispatchCompute", ExactSpelling = true)]
            internal extern static void DispatchCompute(UInt32 num_groups_x, UInt32 num_groups_y, UInt32 num_groups_z);
            [DllImport(Gl.Library, EntryPoint = "glDispatchComputeIndirect", ExactSpelling = true)]
            internal extern static void DispatchComputeIndirect(IntPtr indirect);
            [DllImport(Gl.Library, EntryPoint = "glDrawArrays", ExactSpelling = true)]
            internal extern static void DrawArrays(BeginMode mode, Int32 first, Int32 count);
            [DllImport(Gl.Library, EntryPoint = "glDrawArraysIndirect", ExactSpelling = true)]
            internal extern static void DrawArraysIndirect(BeginMode mode, IntPtr indirect);
            [DllImport(Gl.Library, EntryPoint = "glDrawArraysInstanced", ExactSpelling = true)]
            internal extern static void DrawArraysInstanced(BeginMode mode, Int32 first, Int32 count, Int32 primcount);
            [DllImport(Gl.Library, EntryPoint = "glDrawArraysInstancedBaseInstance", ExactSpelling = true)]
            internal extern static void DrawArraysInstancedBaseInstance(BeginMode mode, Int32 first, Int32 count, Int32 primcount, UInt32 baseinstance);
            [DllImport(Gl.Library, EntryPoint = "glDrawBuffer", ExactSpelling = true)]
            internal extern static void DrawBuffer(DrawBufferMode buf);
            [DllImport(Gl.Library, EntryPoint = "glNamedFramebufferDrawBuffer", ExactSpelling = true)]
            internal extern static void NamedFramebufferDrawBuffer(UInt32 framebuffer, DrawBufferMode buf);
            [DllImport(Gl.Library, EntryPoint = "glDrawBuffers", ExactSpelling = true)]
            internal extern static void DrawBuffers(Int32 n, DrawBuffersEnum[] bufs);
            [DllImport(Gl.Library, EntryPoint = "glNamedFramebufferDrawBuffers", ExactSpelling = true)]
            internal extern static void NamedFramebufferDrawBuffers(UInt32 framebuffer, Int32 n, DrawBufferMode[] bufs);
            [DllImport(Gl.Library, EntryPoint = "glDrawElements", ExactSpelling = true)]
            internal extern static void DrawElements(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices);
            [DllImport(Gl.Library, EntryPoint = "glDrawElementsBaseVertex", ExactSpelling = true)]
            internal extern static void DrawElementsBaseVertex(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 basevertex);
            [DllImport(Gl.Library, EntryPoint = "glDrawElementsIndirect", ExactSpelling = true)]
            internal extern static void DrawElementsIndirect(BeginMode mode, DrawElementsType type, IntPtr indirect);
            [DllImport(Gl.Library, EntryPoint = "glDrawElementsInstanced", ExactSpelling = true)]
            internal extern static void DrawElementsInstanced(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 primcount);
            [DllImport(Gl.Library, EntryPoint = "glDrawElementsInstancedBaseInstance", ExactSpelling = true)]
            internal extern static void DrawElementsInstancedBaseInstance(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 primcount, UInt32 baseinstance);
            [DllImport(Gl.Library, EntryPoint = "glDrawElementsInstancedBaseVertex", ExactSpelling = true)]
            internal extern static void DrawElementsInstancedBaseVertex(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 primcount, Int32 basevertex);
            [DllImport(Gl.Library, EntryPoint = "glDrawElementsInstancedBaseVertexBaseInstance", ExactSpelling = true)]
            internal extern static void DrawElementsInstancedBaseVertexBaseInstance(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 primcount, Int32 basevertex, UInt32 baseinstance);
            [DllImport(Gl.Library, EntryPoint = "glDrawRangeElements", ExactSpelling = true)]
            internal extern static void DrawRangeElements(BeginMode mode, UInt32 start, UInt32 end, Int32 count, DrawElementsType type, IntPtr indices);
            [DllImport(Gl.Library, EntryPoint = "glDrawRangeElementsBaseVertex", ExactSpelling = true)]
            internal extern static void DrawRangeElementsBaseVertex(BeginMode mode, UInt32 start, UInt32 end, Int32 count, DrawElementsType type, IntPtr indices, Int32 basevertex);
            [DllImport(Gl.Library, EntryPoint = "glDrawTransformFeedback", ExactSpelling = true)]
            internal extern static void DrawTransformFeedback(NvTransformFeedback2 mode, UInt32 id);
            [DllImport(Gl.Library, EntryPoint = "glDrawTransformFeedbackInstanced", ExactSpelling = true)]
            internal extern static void DrawTransformFeedbackInstanced(BeginMode mode, UInt32 id, Int32 primcount);
            [DllImport(Gl.Library, EntryPoint = "glDrawTransformFeedbackStream", ExactSpelling = true)]
            internal extern static void DrawTransformFeedbackStream(NvTransformFeedback2 mode, UInt32 id, UInt32 stream);
            [DllImport(Gl.Library, EntryPoint = "glDrawTransformFeedbackStreamInstanced", ExactSpelling = true)]
            internal extern static void DrawTransformFeedbackStreamInstanced(BeginMode mode, UInt32 id, UInt32 stream, Int32 primcount);
            [DllImport(Gl.Library, EntryPoint = "glEnable", ExactSpelling = true)]
            internal extern static void Enable(EnableCap cap);
            [DllImport(Gl.Library, EntryPoint = "glDisable", ExactSpelling = true)]
            internal extern static void Disable(EnableCap cap);
            [DllImport(Gl.Library, EntryPoint = "glEnablei", ExactSpelling = true)]
            internal extern static void Enablei(EnableCap cap, UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glDisablei", ExactSpelling = true)]
            internal extern static void Disablei(EnableCap cap, UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glEnableVertexAttribArray", ExactSpelling = true)]
            internal extern static void EnableVertexAttribArray(UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glDisableVertexAttribArray", ExactSpelling = true)]
            internal extern static void DisableVertexAttribArray(UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glEnableVertexArrayAttrib", ExactSpelling = true)]
            internal extern static void EnableVertexArrayAttrib(UInt32 vaobj, UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glDisableVertexArrayAttrib", ExactSpelling = true)]
            internal extern static void DisableVertexArrayAttrib(UInt32 vaobj, UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glFenceSync", ExactSpelling = true)]
            internal extern static IntPtr FenceSync(ArbSync condition, UInt32 flags);
            [DllImport(Gl.Library, EntryPoint = "glFinish", ExactSpelling = true)]
            internal extern static void Finish();
            [DllImport(Gl.Library, EntryPoint = "glFlush", ExactSpelling = true)]
            internal extern static void Flush();
            [DllImport(Gl.Library, EntryPoint = "glFlushMappedBufferRange", ExactSpelling = true)]
            internal extern static void FlushMappedBufferRange(BufferTarget target, IntPtr offset, IntPtr length);
            [DllImport(Gl.Library, EntryPoint = "glFlushMappedNamedBufferRange", ExactSpelling = true)]
            internal extern static void FlushMappedNamedBufferRange(UInt32 buffer, IntPtr offset, Int32 length);
            [DllImport(Gl.Library, EntryPoint = "glFramebufferParameteri", ExactSpelling = true)]
            internal extern static void FramebufferParameteri(FramebufferTarget target, FramebufferPName pname, Int32 param);
            [DllImport(Gl.Library, EntryPoint = "glNamedFramebufferParameteri", ExactSpelling = true)]
            internal extern static void NamedFramebufferParameteri(UInt32 framebuffer, FramebufferPName pname, Int32 param);
            [DllImport(Gl.Library, EntryPoint = "glFramebufferRenderbuffer", ExactSpelling = true)]
            internal extern static void FramebufferRenderbuffer(FramebufferTarget target, FramebufferAttachment attachment, RenderbufferTarget renderbuffertarget, UInt32 renderbuffer);
            [DllImport(Gl.Library, EntryPoint = "glNamedFramebufferRenderbuffer", ExactSpelling = true)]
            internal extern static void NamedFramebufferRenderbuffer(UInt32 framebuffer, FramebufferAttachment attachment, RenderbufferTarget renderbuffertarget, UInt32 renderbuffer);
            [DllImport(Gl.Library, EntryPoint = "glFramebufferTexture", ExactSpelling = true)]
            internal extern static void FramebufferTexture(FramebufferTarget target, FramebufferAttachment attachment, UInt32 texture, Int32 level);
            [DllImport(Gl.Library, EntryPoint = "glFramebufferTexture1D", ExactSpelling = true)]
            internal extern static void FramebufferTexture1D(FramebufferTarget target, FramebufferAttachment attachment, TextureTarget textarget, UInt32 texture, Int32 level);
            [DllImport(Gl.Library, EntryPoint = "glFramebufferTexture2D", ExactSpelling = true)]
            internal extern static void FramebufferTexture2D(FramebufferTarget target, FramebufferAttachment attachment, TextureTarget textarget, UInt32 texture, Int32 level);
            [DllImport(Gl.Library, EntryPoint = "glFramebufferTexture3D", ExactSpelling = true)]
            internal extern static void FramebufferTexture3D(FramebufferTarget target, FramebufferAttachment attachment, TextureTarget textarget, UInt32 texture, Int32 level, Int32 layer);
            [DllImport(Gl.Library, EntryPoint = "glNamedFramebufferTexture", ExactSpelling = true)]
            internal extern static void NamedFramebufferTexture(UInt32 framebuffer, FramebufferAttachment attachment, UInt32 texture, Int32 level);
            [DllImport(Gl.Library, EntryPoint = "glFramebufferTextureLayer", ExactSpelling = true)]
            internal extern static void FramebufferTextureLayer(FramebufferTarget target, FramebufferAttachment attachment, UInt32 texture, Int32 level, Int32 layer);
            [DllImport(Gl.Library, EntryPoint = "glNamedFramebufferTextureLayer", ExactSpelling = true)]
            internal extern static void NamedFramebufferTextureLayer(UInt32 framebuffer, FramebufferAttachment attachment, UInt32 texture, Int32 level, Int32 layer);
            [DllImport(Gl.Library, EntryPoint = "glFrontFace", ExactSpelling = true)]
            internal extern static void FrontFace(FrontFaceDirection mode);
            [DllImport(Gl.Library, EntryPoint = "glGenBuffers", ExactSpelling = true)]
            internal extern static void GenBuffers(Int32 n, [OutAttribute] UInt32[] buffers);
            [DllImport(Gl.Library, EntryPoint = "glGenerateMipmap", ExactSpelling = true)]
            internal extern static void GenerateMipmap(GenerateMipmapTarget target);
            [DllImport(Gl.Library, EntryPoint = "glGenerateTextureMipmap", ExactSpelling = true)]
            internal extern static void GenerateTextureMipmap(UInt32 texture);
            [DllImport(Gl.Library, EntryPoint = "glGenFramebuffers", ExactSpelling = true)]
            internal extern static void GenFramebuffers(Int32 n, [OutAttribute] UInt32[] ids);
            [DllImport(Gl.Library, EntryPoint = "glGenProgramPipelines", ExactSpelling = true)]
            internal extern static void GenProgramPipelines(Int32 n, [OutAttribute] UInt32[] pipelines);
            [DllImport(Gl.Library, EntryPoint = "glGenQueries", ExactSpelling = true)]
            internal extern static void GenQueries(Int32 n, [OutAttribute] UInt32[] ids);
            [DllImport(Gl.Library, EntryPoint = "glGenRenderbuffers", ExactSpelling = true)]
            internal extern static void GenRenderbuffers(Int32 n, [OutAttribute] UInt32[] renderbuffers);
            [DllImport(Gl.Library, EntryPoint = "glGenSamplers", ExactSpelling = true)]
            internal extern static void GenSamplers(Int32 n, [OutAttribute] UInt32[] samplers);
            [DllImport(Gl.Library, EntryPoint = "glGenTextures", ExactSpelling = true)]
            internal extern static void GenTextures(Int32 n, [OutAttribute] UInt32[] textures);
            [DllImport(Gl.Library, EntryPoint = "glGenTransformFeedbacks", ExactSpelling = true)]
            internal extern static void GenTransformFeedbacks(Int32 n, [OutAttribute] UInt32[] ids);
            [DllImport(Gl.Library, EntryPoint = "glGenVertexArrays", ExactSpelling = true)]
            internal extern static void GenVertexArrays(Int32 n, [OutAttribute] UInt32[] arrays);
            [DllImport(Gl.Library, EntryPoint = "glGetBooleanv", ExactSpelling = true)]
            internal extern static void GetBooleanv(GetPName pname, [OutAttribute] Boolean[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetDoublev", ExactSpelling = true)]
            internal extern static void GetDoublev(GetPName pname, [OutAttribute] Double[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetFloatv", ExactSpelling = true)]
            internal extern static void GetFloatv(GetPName pname, [OutAttribute] Single[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetIntegerv", ExactSpelling = true)]
            internal extern static void GetIntegerv(GetPName pname, [OutAttribute] Int32[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetInteger64v", ExactSpelling = true)]
            internal extern static void GetInteger64v(ArbSync pname, [OutAttribute] Int64[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetBooleani_v", ExactSpelling = true)]
            internal extern static void GetBooleani_v(GetPName target, UInt32 index, [OutAttribute] Boolean[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetIntegeri_v", ExactSpelling = true)]
            internal extern static void GetIntegeri_v(GetPName target, UInt32 index, [OutAttribute] Int32[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetFloati_v", ExactSpelling = true)]
            internal extern static void GetFloati_v(GetPName target, UInt32 index, [OutAttribute] Single[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetDoublei_v", ExactSpelling = true)]
            internal extern static void GetDoublei_v(GetPName target, UInt32 index, [OutAttribute] Double[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetInteger64i_v", ExactSpelling = true)]
            internal extern static void GetInteger64i_v(GetPName target, UInt32 index, [OutAttribute] Int64[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveAtomicCounterBufferiv", ExactSpelling = true)]
            internal extern static void GetActiveAtomicCounterBufferiv(UInt32 program, UInt32 bufferIndex, AtomicCounterParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveAttrib", ExactSpelling = true)]
            internal extern static void GetActiveAttrib(UInt32 program, UInt32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] size, [OutAttribute] ActiveAttribType[] type, [OutAttribute] System.Text.StringBuilder name);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveSubroutineName", ExactSpelling = true)]
            internal extern static void GetActiveSubroutineName(UInt32 program, ShaderType shadertype, UInt32 index, Int32 bufsize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder name);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveSubroutineUniformiv", ExactSpelling = true)]
            internal extern static void GetActiveSubroutineUniformiv(UInt32 program, ShaderType shadertype, UInt32 index, SubroutineParameterName pname, [OutAttribute] Int32[] values);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveSubroutineUniformName", ExactSpelling = true)]
            internal extern static void GetActiveSubroutineUniformName(UInt32 program, ShaderType shadertype, UInt32 index, Int32 bufsize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder name);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveUniform", ExactSpelling = true)]
            internal extern static void GetActiveUniform(UInt32 program, UInt32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] size, [OutAttribute] ActiveUniformType[] type, [OutAttribute] System.Text.StringBuilder name);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveUniformBlockiv", ExactSpelling = true)]
            internal extern static void GetActiveUniformBlockiv(UInt32 program, UInt32 uniformBlockIndex, ActiveUniformBlockParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveUniformBlockName", ExactSpelling = true)]
            internal extern static void GetActiveUniformBlockName(UInt32 program, UInt32 uniformBlockIndex, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder uniformBlockName);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveUniformName", ExactSpelling = true)]
            internal extern static void GetActiveUniformName(UInt32 program, UInt32 uniformIndex, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder uniformName);
            [DllImport(Gl.Library, EntryPoint = "glGetActiveUniformsiv", ExactSpelling = true)]
            internal extern static void GetActiveUniformsiv(UInt32 program, Int32 uniformCount, [OutAttribute] UInt32[] uniformIndices, ActiveUniformType pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetAttachedShaders", ExactSpelling = true)]
            internal extern static void GetAttachedShaders(UInt32 program, Int32 maxCount, [OutAttribute] Int32[] count, [OutAttribute] UInt32[] shaders);
            [DllImport(Gl.Library, EntryPoint = "glGetAttribLocation", ExactSpelling = true)]
            internal extern static Int32 GetAttribLocation(UInt32 program, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetBufferParameteriv", ExactSpelling = true)]
            internal extern static void GetBufferParameteriv(BufferTarget target, BufferParameterName value, [OutAttribute] Int32[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetBufferParameteri64v", ExactSpelling = true)]
            internal extern static void GetBufferParameteri64v(BufferTarget target, BufferParameterName value, [OutAttribute] Int64[] data);
            [DllImport(Gl.Library, EntryPoint = "glGetNamedBufferParameteriv", ExactSpelling = true)]
            internal extern static void GetNamedBufferParameteriv(UInt32 buffer, BufferParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetNamedBufferParameteri64v", ExactSpelling = true)]
            internal extern static void GetNamedBufferParameteri64v(UInt32 buffer, BufferParameterName pname, [OutAttribute] Int64[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetBufferPointerv", ExactSpelling = true)]
            internal extern static void GetBufferPointerv(BufferTarget target, BufferPointer pname, [OutAttribute] IntPtr @params);
            [DllImport(Gl.Library, EntryPoint = "glGetNamedBufferPointerv", ExactSpelling = true)]
            internal extern static void GetNamedBufferPointerv(UInt32 buffer, BufferPointer pname, [OutAttribute] IntPtr @params);
            [DllImport(Gl.Library, EntryPoint = "glGetBufferSubData", ExactSpelling = true)]
            internal extern static void GetBufferSubData(BufferTarget target, IntPtr offset, IntPtr size, [OutAttribute] IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glGetNamedBufferSubData", ExactSpelling = true)]
            internal extern static void GetNamedBufferSubData(UInt32 buffer, IntPtr offset, Int32 size, [OutAttribute] IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glGetCompressedTexImage", ExactSpelling = true)]
            internal extern static void GetCompressedTexImage(TextureTarget target, Int32 level, [OutAttribute] IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glGetnCompressedTexImage", ExactSpelling = true)]
            internal extern static void GetnCompressedTexImage(TextureTarget target, Int32 level, Int32 bufSize, [OutAttribute] IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glGetCompressedTextureImage", ExactSpelling = true)]
            internal extern static void GetCompressedTextureImage(UInt32 texture, Int32 level, Int32 bufSize, [OutAttribute] IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glGetCompressedTextureSubImage", ExactSpelling = true)]
            internal extern static void GetCompressedTextureSubImage(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, Int32 bufSize, [OutAttribute] IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glGetError", ExactSpelling = true)]
            internal extern static ErrorCode GetError();
            [DllImport(Gl.Library, EntryPoint = "glGetFragDataIndex", ExactSpelling = true)]
            internal extern static Int32 GetFragDataIndex(UInt32 program, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetFragDataLocation", ExactSpelling = true)]
            internal extern static Int32 GetFragDataLocation(UInt32 program, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetFramebufferAttachmentParameteriv", ExactSpelling = true)]
            internal extern static void GetFramebufferAttachmentParameteriv(FramebufferTarget target, FramebufferAttachment attachment, FramebufferParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetNamedFramebufferAttachmentParameteriv", ExactSpelling = true)]
            internal extern static void GetNamedFramebufferAttachmentParameteriv(UInt32 framebuffer, FramebufferAttachment attachment, FramebufferParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetFramebufferParameteriv", ExactSpelling = true)]
            internal extern static void GetFramebufferParameteriv(FramebufferTarget target, FramebufferPName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetNamedFramebufferParameteriv", ExactSpelling = true)]
            internal extern static void GetNamedFramebufferParameteriv(UInt32 framebuffer, FramebufferPName pname, [OutAttribute] Int32[] param);
            [DllImport(Gl.Library, EntryPoint = "glGetGraphicsResetStatus", ExactSpelling = true)]
            internal extern static GraphicResetStatus GetGraphicsResetStatus();
            [DllImport(Gl.Library, EntryPoint = "glGetInternalformativ", ExactSpelling = true)]
            internal extern static void GetInternalformativ(TextureTarget target, PixelInternalFormat internalFormat, GetPName pname, Int32 bufSize, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetInternalformati64v", ExactSpelling = true)]
            internal extern static void GetInternalformati64v(TextureTarget target, PixelInternalFormat internalFormat, GetPName pname, Int32 bufSize, [OutAttribute] Int64[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetMultisamplefv", ExactSpelling = true)]
            internal extern static void GetMultisamplefv(GetMultisamplePName pname, UInt32 index, [OutAttribute] Single[] val);
            [DllImport(Gl.Library, EntryPoint = "glGetObjectLabel", ExactSpelling = true)]
            internal extern static void GetObjectLabel(ObjectLabelEnum identifier, UInt32 name, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder label);
            [DllImport(Gl.Library, EntryPoint = "glGetObjectPtrLabel", ExactSpelling = true)]
            internal extern static void GetObjectPtrLabel([OutAttribute] IntPtr ptr, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder label);
            [DllImport(Gl.Library, EntryPoint = "glGetPointerv", ExactSpelling = true)]
            internal extern static void GetPointerv(GetPointerParameter pname, [OutAttribute] IntPtr @params);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramiv", ExactSpelling = true)]
            internal extern static void GetProgramiv(UInt32 program, ProgramParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramBinary", ExactSpelling = true)]
            internal extern static void GetProgramBinary(UInt32 program, Int32 bufsize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] binaryFormat, [OutAttribute] IntPtr binary);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramInfoLog", ExactSpelling = true)]
            internal extern static void GetProgramInfoLog(UInt32 program, Int32 maxLength, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder infoLog);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramInterfaceiv", ExactSpelling = true)]
            internal extern static void GetProgramInterfaceiv(UInt32 program, ProgramInterface programInterface, ProgramInterfaceParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramPipelineiv", ExactSpelling = true)]
            internal extern static void GetProgramPipelineiv(UInt32 pipeline, Int32 pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramPipelineInfoLog", ExactSpelling = true)]
            internal extern static void GetProgramPipelineInfoLog(UInt32 pipeline, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder infoLog);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramResourceiv", ExactSpelling = true)]
            internal extern static void GetProgramResourceiv(UInt32 program, ProgramInterface programInterface, UInt32 index, Int32 propCount, [OutAttribute] ProgramResourceParameterName[] props, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramResourceIndex", ExactSpelling = true)]
            internal extern static UInt32 GetProgramResourceIndex(UInt32 program, ProgramInterface programInterface, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramResourceLocation", ExactSpelling = true)]
            internal extern static Int32 GetProgramResourceLocation(UInt32 program, ProgramInterface programInterface, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramResourceLocationIndex", ExactSpelling = true)]
            internal extern static Int32 GetProgramResourceLocationIndex(UInt32 program, ProgramInterface programInterface, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramResourceName", ExactSpelling = true)]
            internal extern static void GetProgramResourceName(UInt32 program, ProgramInterface programInterface, UInt32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder name);
            [DllImport(Gl.Library, EntryPoint = "glGetProgramStageiv", ExactSpelling = true)]
            internal extern static void GetProgramStageiv(UInt32 program, ShaderType shadertype, ProgramStageParameterName pname, [OutAttribute] Int32[] values);
            [DllImport(Gl.Library, EntryPoint = "glGetQueryIndexediv", ExactSpelling = true)]
            internal extern static void GetQueryIndexediv(QueryTarget target, UInt32 index, GetQueryParam pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetQueryiv", ExactSpelling = true)]
            internal extern static void GetQueryiv(QueryTarget target, GetQueryParam pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetQueryObjectiv", ExactSpelling = true)]
            internal extern static void GetQueryObjectiv(UInt32 id, GetQueryObjectParam pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetQueryObjectuiv", ExactSpelling = true)]
            internal extern static void GetQueryObjectuiv(UInt32 id, GetQueryObjectParam pname, [OutAttribute] UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetQueryObjecti64v", ExactSpelling = true)]
            internal extern static void GetQueryObjecti64v(UInt32 id, GetQueryObjectParam pname, [OutAttribute] Int64[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetQueryObjectui64v", ExactSpelling = true)]
            internal extern static void GetQueryObjectui64v(UInt32 id, GetQueryObjectParam pname, [OutAttribute] UInt64[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetRenderbufferParameteriv", ExactSpelling = true)]
            internal extern static void GetRenderbufferParameteriv(RenderbufferTarget target, RenderbufferParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetNamedRenderbufferParameteriv", ExactSpelling = true)]
            internal extern static void GetNamedRenderbufferParameteriv(UInt32 renderbuffer, RenderbufferParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetSamplerParameterfv", ExactSpelling = true)]
            internal extern static void GetSamplerParameterfv(UInt32 sampler, TextureParameterName pname, [OutAttribute] Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetSamplerParameteriv", ExactSpelling = true)]
            internal extern static void GetSamplerParameteriv(UInt32 sampler, TextureParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetSamplerParameterIiv", ExactSpelling = true)]
            internal extern static void GetSamplerParameterIiv(UInt32 sampler, TextureParameterName pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetSamplerParameterIuiv", ExactSpelling = true)]
            internal extern static void GetSamplerParameterIuiv(UInt32 sampler, TextureParameterName pname, [OutAttribute] UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetShaderiv", ExactSpelling = true)]
            internal extern static void GetShaderiv(UInt32 shader, ShaderParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetShaderInfoLog", ExactSpelling = true)]
            internal extern static void GetShaderInfoLog(UInt32 shader, Int32 maxLength, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder infoLog);
            [DllImport(Gl.Library, EntryPoint = "glGetShaderPrecisionFormat", ExactSpelling = true)]
            internal extern static void GetShaderPrecisionFormat(ShaderType shaderType, Int32 precisionType, [OutAttribute] Int32[] range, [OutAttribute] Int32[] precision);
            [DllImport(Gl.Library, EntryPoint = "glGetShaderSource", ExactSpelling = true)]
            internal extern static void GetShaderSource(UInt32 shader, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder source);
            [DllImport(Gl.Library, EntryPoint = "glGetString", ExactSpelling = true)]
            internal extern static IntPtr GetString(StringName name);
            [DllImport(Gl.Library, EntryPoint = "glGetStringi", ExactSpelling = true)]
            internal extern static IntPtr GetStringi(StringName name, UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glGetSubroutineIndex", ExactSpelling = true)]
            internal extern static UInt32 GetSubroutineIndex(UInt32 program, ShaderType shadertype, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetSubroutineUniformLocation", ExactSpelling = true)]
            internal extern static Int32 GetSubroutineUniformLocation(UInt32 program, ShaderType shadertype, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetSynciv", ExactSpelling = true)]
            internal extern static void GetSynciv(IntPtr sync, ArbSync pname, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] values);
            [DllImport(Gl.Library, EntryPoint = "glGetTexImage", ExactSpelling = true)]
            internal extern static void GetTexImage(TextureTarget target, Int32 level, PixelFormat format, PixelType type, [OutAttribute] IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glGetnTexImage", ExactSpelling = true)]
            internal extern static void GetnTexImage(TextureTarget target, Int32 level, PixelFormat format, PixelType type, Int32 bufSize, [OutAttribute] IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glGetTextureImage", ExactSpelling = true)]
            internal extern static void GetTextureImage(UInt32 texture, Int32 level, PixelFormat format, PixelType type, Int32 bufSize, [OutAttribute] IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glGetTexLevelParameterfv", ExactSpelling = true)]
            internal extern static void GetTexLevelParameterfv(GetPName target, Int32 level, GetTextureLevelParameter pname, [OutAttribute] Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTexLevelParameteriv", ExactSpelling = true)]
            internal extern static void GetTexLevelParameteriv(GetPName target, Int32 level, GetTextureLevelParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTextureLevelParameterfv", ExactSpelling = true)]
            internal extern static void GetTextureLevelParameterfv(UInt32 texture, Int32 level, GetTextureLevelParameter pname, [OutAttribute] Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTextureLevelParameteriv", ExactSpelling = true)]
            internal extern static void GetTextureLevelParameteriv(UInt32 texture, Int32 level, GetTextureLevelParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTexParameterfv", ExactSpelling = true)]
            internal extern static void GetTexParameterfv(TextureTarget target, GetTextureParameter pname, [OutAttribute] Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTexParameteriv", ExactSpelling = true)]
            internal extern static void GetTexParameteriv(TextureTarget target, GetTextureParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTexParameterIiv", ExactSpelling = true)]
            internal extern static void GetTexParameterIiv(TextureTarget target, GetTextureParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTexParameterIuiv", ExactSpelling = true)]
            internal extern static void GetTexParameterIuiv(TextureTarget target, GetTextureParameter pname, [OutAttribute] UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTextureParameterfv", ExactSpelling = true)]
            internal extern static void GetTextureParameterfv(UInt32 texture, GetTextureParameter pname, [OutAttribute] Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTextureParameteriv", ExactSpelling = true)]
            internal extern static void GetTextureParameteriv(UInt32 texture, GetTextureParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTextureParameterIiv", ExactSpelling = true)]
            internal extern static void GetTextureParameterIiv(UInt32 texture, GetTextureParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTextureParameterIuiv", ExactSpelling = true)]
            internal extern static void GetTextureParameterIuiv(UInt32 texture, GetTextureParameter pname, [OutAttribute] UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetTextureSubImage", ExactSpelling = true)]
            internal extern static void GetTextureSubImage(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelFormat format, PixelType type, Int32 bufSize, [OutAttribute] IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glGetTransformFeedbackiv", ExactSpelling = true)]
            internal extern static void GetTransformFeedbackiv(UInt32 xfb, TransformFeedbackParameterName pname, [OutAttribute] Int32[] param);
            [DllImport(Gl.Library, EntryPoint = "glGetTransformFeedbacki_v", ExactSpelling = true)]
            internal extern static void GetTransformFeedbacki_v(UInt32 xfb, TransformFeedbackParameterName pname, UInt32 index, [OutAttribute] Int32[] param);
            [DllImport(Gl.Library, EntryPoint = "glGetTransformFeedbacki64_v", ExactSpelling = true)]
            internal extern static void GetTransformFeedbacki64_v(UInt32 xfb, TransformFeedbackParameterName pname, UInt32 index, [OutAttribute] Int64[] param);
            [DllImport(Gl.Library, EntryPoint = "glGetTransformFeedbackVarying", ExactSpelling = true)]
            internal extern static void GetTransformFeedbackVarying(UInt32 program, UInt32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] size, [OutAttribute] ActiveAttribType[] type, [OutAttribute] System.Text.StringBuilder name);
            [DllImport(Gl.Library, EntryPoint = "glGetUniformfv", ExactSpelling = true)]
            internal extern static void GetUniformfv(UInt32 program, Int32 location, [OutAttribute] Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetUniformiv", ExactSpelling = true)]
            internal extern static void GetUniformiv(UInt32 program, Int32 location, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetUniformuiv", ExactSpelling = true)]
            internal extern static void GetUniformuiv(UInt32 program, Int32 location, [OutAttribute] UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetUniformdv", ExactSpelling = true)]
            internal extern static void GetUniformdv(UInt32 program, Int32 location, [OutAttribute] Double[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetnUniformfv", ExactSpelling = true)]
            internal extern static void GetnUniformfv(UInt32 program, Int32 location, Int32 bufSize, [OutAttribute] Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetnUniformiv", ExactSpelling = true)]
            internal extern static void GetnUniformiv(UInt32 program, Int32 location, Int32 bufSize, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetnUniformuiv", ExactSpelling = true)]
            internal extern static void GetnUniformuiv(UInt32 program, Int32 location, Int32 bufSize, [OutAttribute] UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetnUniformdv", ExactSpelling = true)]
            internal extern static void GetnUniformdv(UInt32 program, Int32 location, Int32 bufSize, [OutAttribute] Double[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetUniformBlockIndex", ExactSpelling = true)]
            internal extern static UInt32 GetUniformBlockIndex(UInt32 program, String uniformBlockName);
            [DllImport(Gl.Library, EntryPoint = "glGetUniformIndices", ExactSpelling = true)]
            internal extern static void GetUniformIndices(UInt32 program, Int32 uniformCount, String uniformNames, [OutAttribute] UInt32[] uniformIndices);
            [DllImport(Gl.Library, EntryPoint = "glGetUniformLocation", ExactSpelling = true)]
            internal extern static Int32 GetUniformLocation(UInt32 program, String name);
            [DllImport(Gl.Library, EntryPoint = "glGetUniformSubroutineuiv", ExactSpelling = true)]
            internal extern static void GetUniformSubroutineuiv(ShaderType shadertype, Int32 location, [OutAttribute] UInt32[] values);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexArrayIndexed64iv", ExactSpelling = true)]
            internal extern static void GetVertexArrayIndexed64iv(UInt32 vaobj, UInt32 index, VertexAttribParameter pname, [OutAttribute] Int64[] param);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexArrayIndexediv", ExactSpelling = true)]
            internal extern static void GetVertexArrayIndexediv(UInt32 vaobj, UInt32 index, VertexAttribParameter pname, [OutAttribute] Int32[] param);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexArrayiv", ExactSpelling = true)]
            internal extern static void GetVertexArrayiv(UInt32 vaobj, VertexAttribParameter pname, [OutAttribute] Int32[] param);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexAttribdv", ExactSpelling = true)]
            internal extern static void GetVertexAttribdv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Double[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexAttribfv", ExactSpelling = true)]
            internal extern static void GetVertexAttribfv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexAttribiv", ExactSpelling = true)]
            internal extern static void GetVertexAttribiv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexAttribIiv", ExactSpelling = true)]
            internal extern static void GetVertexAttribIiv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexAttribIuiv", ExactSpelling = true)]
            internal extern static void GetVertexAttribIuiv(UInt32 index, VertexAttribParameter pname, [OutAttribute] UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexAttribLdv", ExactSpelling = true)]
            internal extern static void GetVertexAttribLdv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Double[] @params);
            [DllImport(Gl.Library, EntryPoint = "glGetVertexAttribPointerv", ExactSpelling = true)]
            internal extern static void GetVertexAttribPointerv(UInt32 index, VertexAttribPointerParameter pname, [OutAttribute] IntPtr pointer);
            [DllImport(Gl.Library, EntryPoint = "glHint", ExactSpelling = true)]
            internal extern static void Hint(HintTarget target, HintMode mode);
            [DllImport(Gl.Library, EntryPoint = "glInvalidateBufferData", ExactSpelling = true)]
            internal extern static void InvalidateBufferData(UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glInvalidateBufferSubData", ExactSpelling = true)]
            internal extern static void InvalidateBufferSubData(UInt32 buffer, IntPtr offset, IntPtr length);
            [DllImport(Gl.Library, EntryPoint = "glInvalidateFramebuffer", ExactSpelling = true)]
            internal extern static void InvalidateFramebuffer(FramebufferTarget target, Int32 numAttachments, FramebufferAttachment[] attachments);
            [DllImport(Gl.Library, EntryPoint = "glInvalidateNamedFramebufferData", ExactSpelling = true)]
            internal extern static void InvalidateNamedFramebufferData(UInt32 framebuffer, Int32 numAttachments, FramebufferAttachment[] attachments);
            [DllImport(Gl.Library, EntryPoint = "glInvalidateSubFramebuffer", ExactSpelling = true)]
            internal extern static void InvalidateSubFramebuffer(FramebufferTarget target, Int32 numAttachments, FramebufferAttachment[] attachments, Int32 x, Int32 y, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glInvalidateNamedFramebufferSubData", ExactSpelling = true)]
            internal extern static void InvalidateNamedFramebufferSubData(UInt32 framebuffer, Int32 numAttachments, FramebufferAttachment[] attachments, Int32 x, Int32 y, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glInvalidateTexImage", ExactSpelling = true)]
            internal extern static void InvalidateTexImage(UInt32 texture, Int32 level);
            [DllImport(Gl.Library, EntryPoint = "glInvalidateTexSubImage", ExactSpelling = true)]
            internal extern static void InvalidateTexSubImage(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth);
            [DllImport(Gl.Library, EntryPoint = "glIsBuffer", ExactSpelling = true)]
            internal extern static Boolean IsBuffer(UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glIsEnabled", ExactSpelling = true)]
            internal extern static Boolean IsEnabled(EnableCap cap);
            [DllImport(Gl.Library, EntryPoint = "glIsEnabledi", ExactSpelling = true)]
            internal extern static Boolean IsEnabledi(EnableCap cap, UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glIsFramebuffer", ExactSpelling = true)]
            internal extern static Boolean IsFramebuffer(UInt32 framebuffer);
            [DllImport(Gl.Library, EntryPoint = "glIsProgram", ExactSpelling = true)]
            internal extern static Boolean IsProgram(UInt32 program);
            [DllImport(Gl.Library, EntryPoint = "glIsProgramPipeline", ExactSpelling = true)]
            internal extern static Boolean IsProgramPipeline(UInt32 pipeline);
            [DllImport(Gl.Library, EntryPoint = "glIsQuery", ExactSpelling = true)]
            internal extern static Boolean IsQuery(UInt32 id);
            [DllImport(Gl.Library, EntryPoint = "glIsRenderbuffer", ExactSpelling = true)]
            internal extern static Boolean IsRenderbuffer(UInt32 renderbuffer);
            [DllImport(Gl.Library, EntryPoint = "glIsSampler", ExactSpelling = true)]
            internal extern static Boolean IsSampler(UInt32 id);
            [DllImport(Gl.Library, EntryPoint = "glIsShader", ExactSpelling = true)]
            internal extern static Boolean IsShader(UInt32 shader);
            [DllImport(Gl.Library, EntryPoint = "glIsSync", ExactSpelling = true)]
            internal extern static Boolean IsSync(IntPtr sync);
            [DllImport(Gl.Library, EntryPoint = "glIsTexture", ExactSpelling = true)]
            internal extern static Boolean IsTexture(UInt32 texture);
            [DllImport(Gl.Library, EntryPoint = "glIsTransformFeedback", ExactSpelling = true)]
            internal extern static Boolean IsTransformFeedback(UInt32 id);
            [DllImport(Gl.Library, EntryPoint = "glIsVertexArray", ExactSpelling = true)]
            internal extern static Boolean IsVertexArray(UInt32 array);
            [DllImport(Gl.Library, EntryPoint = "glLineWidth", ExactSpelling = true)]
            internal extern static void LineWidth(Single width);
            [DllImport(Gl.Library, EntryPoint = "glLinkProgram", ExactSpelling = true)]
            internal extern static void LinkProgram(UInt32 program);
            [DllImport(Gl.Library, EntryPoint = "glLogicOp", ExactSpelling = true)]
            internal extern static void LogicOp(LogicOpEnum opcode);
            [DllImport(Gl.Library, EntryPoint = "glMapBuffer", ExactSpelling = true)]
            internal extern static IntPtr MapBuffer(BufferTarget target, BufferAccess access);
            [DllImport(Gl.Library, EntryPoint = "glMapNamedBuffer", ExactSpelling = true)]
            internal extern static IntPtr MapNamedBuffer(UInt32 buffer, BufferAccess access);
            [DllImport(Gl.Library, EntryPoint = "glMapBufferRange", ExactSpelling = true)]
            internal extern static IntPtr MapBufferRange(BufferTarget target, IntPtr offset, IntPtr length, BufferAccessMask access);
            [DllImport(Gl.Library, EntryPoint = "glMapNamedBufferRange", ExactSpelling = true)]
            internal extern static IntPtr MapNamedBufferRange(UInt32 buffer, IntPtr offset, Int32 length, UInt32 access);
            [DllImport(Gl.Library, EntryPoint = "glMemoryBarrier", ExactSpelling = true)]
            internal extern static void MemoryBarrier(UInt32 barriers);
            [DllImport(Gl.Library, EntryPoint = "glMemoryBarrierByRegion", ExactSpelling = true)]
            internal extern static void MemoryBarrierByRegion(UInt32 barriers);
            [DllImport(Gl.Library, EntryPoint = "glMinSampleShading", ExactSpelling = true)]
            internal extern static void MinSampleShading(Single value);
            [DllImport(Gl.Library, EntryPoint = "glMultiDrawArrays", ExactSpelling = true)]
            internal extern static void MultiDrawArrays(BeginMode mode, Int32[] first, Int32[] count, Int32 drawcount);
            [DllImport(Gl.Library, EntryPoint = "glMultiDrawArraysIndirect", ExactSpelling = true)]
            internal extern static void MultiDrawArraysIndirect(BeginMode mode, IntPtr indirect, Int32 drawcount, Int32 stride);
            [DllImport(Gl.Library, EntryPoint = "glMultiDrawElements", ExactSpelling = true)]
            internal extern static void MultiDrawElements(BeginMode mode, Int32[] count, DrawElementsType type, IntPtr indices, Int32 drawcount);
            [DllImport(Gl.Library, EntryPoint = "glMultiDrawElementsBaseVertex", ExactSpelling = true)]
            internal extern static void MultiDrawElementsBaseVertex(BeginMode mode, Int32[] count, DrawElementsType type, IntPtr indices, Int32 drawcount, Int32[] basevertex);
            [DllImport(Gl.Library, EntryPoint = "glMultiDrawElementsIndirect", ExactSpelling = true)]
            internal extern static void MultiDrawElementsIndirect(BeginMode mode, DrawElementsType type, IntPtr indirect, Int32 drawcount, Int32 stride);
            [DllImport(Gl.Library, EntryPoint = "glObjectLabel", ExactSpelling = true)]
            internal extern static void ObjectLabel(ObjectLabelEnum identifier, UInt32 name, Int32 length, String label);
            [DllImport(Gl.Library, EntryPoint = "glObjectPtrLabel", ExactSpelling = true)]
            internal extern static void ObjectPtrLabel(IntPtr ptr, Int32 length, String label);
            [DllImport(Gl.Library, EntryPoint = "glPatchParameteri", ExactSpelling = true)]
            internal extern static void PatchParameteri(Int32 pname, Int32 value);
            [DllImport(Gl.Library, EntryPoint = "glPatchParameterfv", ExactSpelling = true)]
            internal extern static void PatchParameterfv(Int32 pname, Single[] values);
            [DllImport(Gl.Library, EntryPoint = "glPixelStoref", ExactSpelling = true)]
            internal extern static void PixelStoref(PixelStoreParameter pname, Single param);
            [DllImport(Gl.Library, EntryPoint = "glPixelStorei", ExactSpelling = true)]
            internal extern static void PixelStorei(PixelStoreParameter pname, Int32 param);
            [DllImport(Gl.Library, EntryPoint = "glPointParameterf", ExactSpelling = true)]
            internal extern static void PointParameterf(PointParameterName pname, Single param);
            [DllImport(Gl.Library, EntryPoint = "glPointParameteri", ExactSpelling = true)]
            internal extern static void PointParameteri(PointParameterName pname, Int32 param);
            [DllImport(Gl.Library, EntryPoint = "glPointParameterfv", ExactSpelling = true)]
            internal extern static void PointParameterfv(PointParameterName pname, Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glPointParameteriv", ExactSpelling = true)]
            internal extern static void PointParameteriv(PointParameterName pname, Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glPointSize", ExactSpelling = true)]
            internal extern static void PointSize(Single size);
            [DllImport(Gl.Library, EntryPoint = "glPolygonMode", ExactSpelling = true)]
            internal extern static void PolygonMode(MaterialFace face, PolygonModeEnum mode);
            [DllImport(Gl.Library, EntryPoint = "glPolygonOffset", ExactSpelling = true)]
            internal extern static void PolygonOffset(Single factor, Single units);
            [DllImport(Gl.Library, EntryPoint = "glPrimitiveRestartIndex", ExactSpelling = true)]
            internal extern static void PrimitiveRestartIndex(UInt32 index);
            [DllImport(Gl.Library, EntryPoint = "glProgramBinary", ExactSpelling = true)]
            internal extern static void ProgramBinary(UInt32 program, Int32 binaryFormat, IntPtr binary, Int32 length);
            [DllImport(Gl.Library, EntryPoint = "glProgramParameteri", ExactSpelling = true)]
            internal extern static void ProgramParameteri(UInt32 program, Version32 pname, Int32 value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform1f", ExactSpelling = true)]
            internal extern static void ProgramUniform1f(UInt32 program, Int32 location, Single v0);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform2f", ExactSpelling = true)]
            internal extern static void ProgramUniform2f(UInt32 program, Int32 location, Single v0, Single v1);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform3f", ExactSpelling = true)]
            internal extern static void ProgramUniform3f(UInt32 program, Int32 location, Single v0, Single v1, Single v2);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform4f", ExactSpelling = true)]
            internal extern static void ProgramUniform4f(UInt32 program, Int32 location, Single v0, Single v1, Single v2, Single v3);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform1i", ExactSpelling = true)]
            internal extern static void ProgramUniform1i(UInt32 program, Int32 location, Int32 v0);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform2i", ExactSpelling = true)]
            internal extern static void ProgramUniform2i(UInt32 program, Int32 location, Int32 v0, Int32 v1);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform3i", ExactSpelling = true)]
            internal extern static void ProgramUniform3i(UInt32 program, Int32 location, Int32 v0, Int32 v1, Int32 v2);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform4i", ExactSpelling = true)]
            internal extern static void ProgramUniform4i(UInt32 program, Int32 location, Int32 v0, Int32 v1, Int32 v2, Int32 v3);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform1ui", ExactSpelling = true)]
            internal extern static void ProgramUniform1ui(UInt32 program, Int32 location, UInt32 v0);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform2ui", ExactSpelling = true)]
            internal extern static void ProgramUniform2ui(UInt32 program, Int32 location, Int32 v0, UInt32 v1);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform3ui", ExactSpelling = true)]
            internal extern static void ProgramUniform3ui(UInt32 program, Int32 location, Int32 v0, Int32 v1, UInt32 v2);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform4ui", ExactSpelling = true)]
            internal extern static void ProgramUniform4ui(UInt32 program, Int32 location, Int32 v0, Int32 v1, Int32 v2, UInt32 v3);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform1fv", ExactSpelling = true)]
            internal extern static void ProgramUniform1fv(UInt32 program, Int32 location, Int32 count, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform2fv", ExactSpelling = true)]
            internal extern static void ProgramUniform2fv(UInt32 program, Int32 location, Int32 count, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform3fv", ExactSpelling = true)]
            internal extern static void ProgramUniform3fv(UInt32 program, Int32 location, Int32 count, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform4fv", ExactSpelling = true)]
            internal extern static void ProgramUniform4fv(UInt32 program, Int32 location, Int32 count, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform1iv", ExactSpelling = true)]
            internal extern static void ProgramUniform1iv(UInt32 program, Int32 location, Int32 count, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform2iv", ExactSpelling = true)]
            internal extern static void ProgramUniform2iv(UInt32 program, Int32 location, Int32 count, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform3iv", ExactSpelling = true)]
            internal extern static void ProgramUniform3iv(UInt32 program, Int32 location, Int32 count, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform4iv", ExactSpelling = true)]
            internal extern static void ProgramUniform4iv(UInt32 program, Int32 location, Int32 count, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform1uiv", ExactSpelling = true)]
            internal extern static void ProgramUniform1uiv(UInt32 program, Int32 location, Int32 count, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform2uiv", ExactSpelling = true)]
            internal extern static void ProgramUniform2uiv(UInt32 program, Int32 location, Int32 count, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform3uiv", ExactSpelling = true)]
            internal extern static void ProgramUniform3uiv(UInt32 program, Int32 location, Int32 count, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniform4uiv", ExactSpelling = true)]
            internal extern static void ProgramUniform4uiv(UInt32 program, Int32 location, Int32 count, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix2fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix2fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix3fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix3fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix4fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix4fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix2x3fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix2x3fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix3x2fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix3x2fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix2x4fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix2x4fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix4x2fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix4x2fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix3x4fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix3x4fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProgramUniformMatrix4x3fv", ExactSpelling = true)]
            internal extern static void ProgramUniformMatrix4x3fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glProvokingVertex", ExactSpelling = true)]
            internal extern static void ProvokingVertex(ProvokingVertexMode provokeMode);
            [DllImport(Gl.Library, EntryPoint = "glQueryCounter", ExactSpelling = true)]
            internal extern static void QueryCounter(UInt32 id, QueryTarget target);
            [DllImport(Gl.Library, EntryPoint = "glReadBuffer", ExactSpelling = true)]
            internal extern static void ReadBuffer(ReadBufferMode mode);
            [DllImport(Gl.Library, EntryPoint = "glNamedFramebufferReadBuffer", ExactSpelling = true)]
            internal extern static void NamedFramebufferReadBuffer(ReadBufferMode framebuffer, BeginMode mode);
            [DllImport(Gl.Library, EntryPoint = "glReadPixels", ExactSpelling = true)]
            internal extern static void ReadPixels(Int32 x, Int32 y, Int32 width, Int32 height, PixelFormat format, PixelType type, Int32[] data);
            [DllImport(Gl.Library, EntryPoint = "glReadnPixels", ExactSpelling = true)]
            internal extern static void ReadnPixels(Int32 x, Int32 y, Int32 width, Int32 height, PixelFormat format, PixelType type, Int32 bufSize, Int32[] data);
            [DllImport(Gl.Library, EntryPoint = "glRenderbufferStorage", ExactSpelling = true)]
            internal extern static void RenderbufferStorage(RenderbufferTarget target, RenderbufferStorageEnum internalFormat, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glNamedRenderbufferStorage", ExactSpelling = true)]
            internal extern static void NamedRenderbufferStorage(UInt32 renderbuffer, RenderbufferStorageEnum internalFormat, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glRenderbufferStorageMultisample", ExactSpelling = true)]
            internal extern static void RenderbufferStorageMultisample(RenderbufferTarget target, Int32 samples, RenderbufferStorageEnum internalFormat, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glNamedRenderbufferStorageMultisample", ExactSpelling = true)]
            internal extern static void NamedRenderbufferStorageMultisample(UInt32 renderbuffer, Int32 samples, RenderbufferStorageEnum internalFormat, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glSampleCoverage", ExactSpelling = true)]
            internal extern static void SampleCoverage(Single value, Boolean invert);
            [DllImport(Gl.Library, EntryPoint = "glSampleMaski", ExactSpelling = true)]
            internal extern static void SampleMaski(UInt32 maskNumber, UInt32 mask);
            [DllImport(Gl.Library, EntryPoint = "glSamplerParameterf", ExactSpelling = true)]
            internal extern static void SamplerParameterf(UInt32 sampler, TextureParameterName pname, Single param);
            [DllImport(Gl.Library, EntryPoint = "glSamplerParameteri", ExactSpelling = true)]
            internal extern static void SamplerParameteri(UInt32 sampler, TextureParameterName pname, Int32 param);
            [DllImport(Gl.Library, EntryPoint = "glSamplerParameterfv", ExactSpelling = true)]
            internal extern static void SamplerParameterfv(UInt32 sampler, TextureParameterName pname, Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glSamplerParameteriv", ExactSpelling = true)]
            internal extern static void SamplerParameteriv(UInt32 sampler, TextureParameterName pname, Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glSamplerParameterIiv", ExactSpelling = true)]
            internal extern static void SamplerParameterIiv(UInt32 sampler, TextureParameterName pname, Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glSamplerParameterIuiv", ExactSpelling = true)]
            internal extern static void SamplerParameterIuiv(UInt32 sampler, TextureParameterName pname, UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glScissor", ExactSpelling = true)]
            internal extern static void Scissor(Int32 x, Int32 y, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glScissorArrayv", ExactSpelling = true)]
            internal extern static void ScissorArrayv(UInt32 first, Int32 count, Int32[] v);
            [DllImport(Gl.Library, EntryPoint = "glScissorIndexed", ExactSpelling = true)]
            internal extern static void ScissorIndexed(UInt32 index, Int32 left, Int32 bottom, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glScissorIndexedv", ExactSpelling = true)]
            internal extern static void ScissorIndexedv(UInt32 index, Int32[] v);
            [DllImport(Gl.Library, EntryPoint = "glShaderBinary", ExactSpelling = true)]
            internal extern static void ShaderBinary(Int32 count, UInt32[] shaders, Int32 binaryFormat, IntPtr binary, Int32 length);
            [DllImport(Gl.Library, EntryPoint = "glShaderSource", ExactSpelling = true)]
            internal extern static void ShaderSource(UInt32 shader, Int32 count, String[] @string, Int32[] length);
            [DllImport(Gl.Library, EntryPoint = "glShaderStorageBlockBinding", ExactSpelling = true)]
            internal extern static void ShaderStorageBlockBinding(UInt32 program, UInt32 storageBlockIndex, UInt32 storageBlockBinding);
            [DllImport(Gl.Library, EntryPoint = "glStencilFunc", ExactSpelling = true)]
            internal extern static void StencilFunc(StencilFunction func, Int32 @ref, UInt32 mask);
            [DllImport(Gl.Library, EntryPoint = "glStencilFuncSeparate", ExactSpelling = true)]
            internal extern static void StencilFuncSeparate(StencilFace face, StencilFunction func, Int32 @ref, UInt32 mask);
            [DllImport(Gl.Library, EntryPoint = "glStencilMask", ExactSpelling = true)]
            internal extern static void StencilMask(UInt32 mask);
            [DllImport(Gl.Library, EntryPoint = "glStencilMaskSeparate", ExactSpelling = true)]
            internal extern static void StencilMaskSeparate(StencilFace face, UInt32 mask);
            [DllImport(Gl.Library, EntryPoint = "glStencilOp", ExactSpelling = true)]
            internal extern static void StencilOp(StencilOpEnum sfail, StencilOpEnum dpfail, StencilOpEnum dppass);
            [DllImport(Gl.Library, EntryPoint = "glStencilOpSeparate", ExactSpelling = true)]
            internal extern static void StencilOpSeparate(StencilFace face, StencilOpEnum sfail, StencilOpEnum dpfail, StencilOpEnum dppass);
            [DllImport(Gl.Library, EntryPoint = "glTexBuffer", ExactSpelling = true)]
            internal extern static void TexBuffer(TextureBufferTarget target, SizedInternalFormat internalFormat, UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glTextureBuffer", ExactSpelling = true)]
            internal extern static void TextureBuffer(UInt32 texture, SizedInternalFormat internalFormat, UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glTexBufferRange", ExactSpelling = true)]
            internal extern static void TexBufferRange(BufferTarget target, SizedInternalFormat internalFormat, UInt32 buffer, IntPtr offset, IntPtr size);
            [DllImport(Gl.Library, EntryPoint = "glTextureBufferRange", ExactSpelling = true)]
            internal extern static void TextureBufferRange(UInt32 texture, SizedInternalFormat internalFormat, UInt32 buffer, IntPtr offset, Int32 size);
            [DllImport(Gl.Library, EntryPoint = "glTexImage1D", ExactSpelling = true)]
            internal extern static void TexImage1D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 border, PixelFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glTexImage2D", ExactSpelling = true)]
            internal extern static void TexImage2D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 border, PixelFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glTexImage2DMultisample", ExactSpelling = true)]
            internal extern static void TexImage2DMultisample(TextureTargetMultisample target, Int32 samples, PixelInternalFormat internalFormat, Int32 width, Int32 height, Boolean fixedsamplelocations);
            [DllImport(Gl.Library, EntryPoint = "glTexImage3D", ExactSpelling = true)]
            internal extern static void TexImage3D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Int32 border, PixelFormat format, PixelType type, IntPtr data);
            [DllImport(Gl.Library, EntryPoint = "glTexImage3DMultisample", ExactSpelling = true)]
            internal extern static void TexImage3DMultisample(TextureTargetMultisample target, Int32 samples, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Boolean fixedsamplelocations);
            [DllImport(Gl.Library, EntryPoint = "glTexParameterf", ExactSpelling = true)]
            internal extern static void TexParameterf(TextureTarget target, TextureParameterName pname, Single param);
            [DllImport(Gl.Library, EntryPoint = "glTexParameteri", ExactSpelling = true)]
            internal extern static void TexParameteri(TextureTarget target, TextureParameterName pname, Int32 param);
            [DllImport(Gl.Library, EntryPoint = "glTextureParameterf", ExactSpelling = true)]
            internal extern static void TextureParameterf(UInt32 texture, TextureParameter pname, Single param);
            [DllImport(Gl.Library, EntryPoint = "glTextureParameteri", ExactSpelling = true)]
            internal extern static void TextureParameteri(UInt32 texture, TextureParameter pname, Int32 param);
            [DllImport(Gl.Library, EntryPoint = "glTexParameterfv", ExactSpelling = true)]
            internal extern static void TexParameterfv(TextureTarget target, TextureParameterName pname, Single[] @params);
            [DllImport(Gl.Library, EntryPoint = "glTexParameteriv", ExactSpelling = true)]
            internal extern static void TexParameteriv(TextureTarget target, TextureParameterName pname, Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glTexParameterIiv", ExactSpelling = true)]
            internal extern static void TexParameterIiv(TextureTarget target, TextureParameterName pname, Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glTexParameterIuiv", ExactSpelling = true)]
            internal extern static void TexParameterIuiv(TextureTarget target, TextureParameterName pname, UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glTextureParameterfv", ExactSpelling = true)]
            internal extern static void TextureParameterfv(UInt32 texture, TextureParameter pname, Single[] paramtexture);
            [DllImport(Gl.Library, EntryPoint = "glTextureParameteriv", ExactSpelling = true)]
            internal extern static void TextureParameteriv(UInt32 texture, TextureParameter pname, Int32[] param);
            [DllImport(Gl.Library, EntryPoint = "glTextureParameterIiv", ExactSpelling = true)]
            internal extern static void TextureParameterIiv(UInt32 texture, TextureParameter pname, Int32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glTextureParameterIuiv", ExactSpelling = true)]
            internal extern static void TextureParameterIuiv(UInt32 texture, TextureParameter pname, UInt32[] @params);
            [DllImport(Gl.Library, EntryPoint = "glTexStorage1D", ExactSpelling = true)]
            internal extern static void TexStorage1D(TextureTarget target, Int32 levels, SizedInternalFormat internalFormat, Int32 width);
            [DllImport(Gl.Library, EntryPoint = "glTextureStorage1D", ExactSpelling = true)]
            internal extern static void TextureStorage1D(UInt32 texture, Int32 levels, SizedInternalFormat internalFormat, Int32 width);
            [DllImport(Gl.Library, EntryPoint = "glTexStorage2D", ExactSpelling = true)]
            internal extern static void TexStorage2D(TextureTarget target, Int32 levels, SizedInternalFormat internalFormat, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glTextureStorage2D", ExactSpelling = true)]
            internal extern static void TextureStorage2D(UInt32 texture, Int32 levels, SizedInternalFormat internalFormat, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glTexStorage2DMultisample", ExactSpelling = true)]
            internal extern static void TexStorage2DMultisample(TextureTarget target, Int32 samples, SizedInternalFormat internalFormat, Int32 width, Int32 height, Boolean fixedsamplelocations);
            [DllImport(Gl.Library, EntryPoint = "glTextureStorage2DMultisample", ExactSpelling = true)]
            internal extern static void TextureStorage2DMultisample(UInt32 texture, Int32 samples, SizedInternalFormat internalFormat, Int32 width, Int32 height, Boolean fixedsamplelocations);
            [DllImport(Gl.Library, EntryPoint = "glTexStorage3D", ExactSpelling = true)]
            internal extern static void TexStorage3D(TextureTarget target, Int32 levels, SizedInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth);
            [DllImport(Gl.Library, EntryPoint = "glTextureStorage3D", ExactSpelling = true)]
            internal extern static void TextureStorage3D(UInt32 texture, Int32 levels, SizedInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth);
            [DllImport(Gl.Library, EntryPoint = "glTexStorage3DMultisample", ExactSpelling = true)]
            internal extern static void TexStorage3DMultisample(TextureTarget target, Int32 samples, SizedInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Boolean fixedsamplelocations);
            [DllImport(Gl.Library, EntryPoint = "glTextureStorage3DMultisample", ExactSpelling = true)]
            internal extern static void TextureStorage3DMultisample(UInt32 texture, Int32 samples, SizedInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Boolean fixedsamplelocations);
            [DllImport(Gl.Library, EntryPoint = "glTexSubImage1D", ExactSpelling = true)]
            internal extern static void TexSubImage1D(TextureTarget target, Int32 level, Int32 xoffset, Int32 width, PixelFormat format, PixelType type, IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glTextureSubImage1D", ExactSpelling = true)]
            internal extern static void TextureSubImage1D(UInt32 texture, Int32 level, Int32 xoffset, Int32 width, PixelFormat format, PixelType type, IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glTexSubImage2D", ExactSpelling = true)]
            internal extern static void TexSubImage2D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelFormat format, PixelType type, IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glTextureSubImage2D", ExactSpelling = true)]
            internal extern static void TextureSubImage2D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelFormat format, PixelType type, IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glTexSubImage3D", ExactSpelling = true)]
            internal extern static void TexSubImage3D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelFormat format, PixelType type, IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glTextureSubImage3D", ExactSpelling = true)]
            internal extern static void TextureSubImage3D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelFormat format, PixelType type, IntPtr pixels);
            [DllImport(Gl.Library, EntryPoint = "glTextureBarrier", ExactSpelling = true)]
            internal extern static void TextureBarrier();
            [DllImport(Gl.Library, EntryPoint = "glTextureView", ExactSpelling = true)]
            internal extern static void TextureView(UInt32 texture, TextureTarget target, UInt32 origtexture, PixelInternalFormat internalFormat, UInt32 minlevel, UInt32 numlevels, UInt32 minlayer, UInt32 numlayers);
            [DllImport(Gl.Library, EntryPoint = "glTransformFeedbackBufferBase", ExactSpelling = true)]
            internal extern static void TransformFeedbackBufferBase(UInt32 xfb, UInt32 index, UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glTransformFeedbackBufferRange", ExactSpelling = true)]
            internal extern static void TransformFeedbackBufferRange(UInt32 xfb, UInt32 index, UInt32 buffer, IntPtr offset, Int32 size);
            [DllImport(Gl.Library, EntryPoint = "glTransformFeedbackVaryings", ExactSpelling = true)]
            internal extern static void TransformFeedbackVaryings(UInt32 program, Int32 count, String[] varyings, TransformFeedbackMode bufferMode);
            [DllImport(Gl.Library, EntryPoint = "glUniform1f", ExactSpelling = true)]
            internal extern static void Uniform1f(Int32 location, Single v0);
            [DllImport(Gl.Library, EntryPoint = "glUniform2f", ExactSpelling = true)]
            internal extern static void Uniform2f(Int32 location, Single v0, Single v1);
            [DllImport(Gl.Library, EntryPoint = "glUniform3f", ExactSpelling = true)]
            internal extern static void Uniform3f(Int32 location, Single v0, Single v1, Single v2);
            [DllImport(Gl.Library, EntryPoint = "glUniform4f", ExactSpelling = true)]
            internal extern static void Uniform4f(Int32 location, Single v0, Single v1, Single v2, Single v3);
            [DllImport(Gl.Library, EntryPoint = "glUniform1i", ExactSpelling = true)]
            internal extern static void Uniform1i(Int32 location, Int32 v0);
            [DllImport(Gl.Library, EntryPoint = "glUniform2i", ExactSpelling = true)]
            internal extern static void Uniform2i(Int32 location, Int32 v0, Int32 v1);
            [DllImport(Gl.Library, EntryPoint = "glUniform3i", ExactSpelling = true)]
            internal extern static void Uniform3i(Int32 location, Int32 v0, Int32 v1, Int32 v2);
            [DllImport(Gl.Library, EntryPoint = "glUniform4i", ExactSpelling = true)]
            internal extern static void Uniform4i(Int32 location, Int32 v0, Int32 v1, Int32 v2, Int32 v3);
            [DllImport(Gl.Library, EntryPoint = "glUniform1ui", ExactSpelling = true)]
            internal extern static void Uniform1ui(Int32 location, UInt32 v0);
            [DllImport(Gl.Library, EntryPoint = "glUniform2ui", ExactSpelling = true)]
            internal extern static void Uniform2ui(Int32 location, UInt32 v0, UInt32 v1);
            [DllImport(Gl.Library, EntryPoint = "glUniform3ui", ExactSpelling = true)]
            internal extern static void Uniform3ui(Int32 location, UInt32 v0, UInt32 v1, UInt32 v2);
            [DllImport(Gl.Library, EntryPoint = "glUniform4ui", ExactSpelling = true)]
            internal extern static void Uniform4ui(Int32 location, UInt32 v0, UInt32 v1, UInt32 v2, UInt32 v3);
            [DllImport(Gl.Library, EntryPoint = "glUniform1fv", ExactSpelling = true)]
            internal extern static void Uniform1fv(Int32 location, Int32 count, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform2fv", ExactSpelling = true)]
            internal extern static void Uniform2fv(Int32 location, Int32 count, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform3fv", ExactSpelling = true)]
            internal extern static void Uniform3fv(Int32 location, Int32 count, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform4fv", ExactSpelling = true)]
            internal extern static void Uniform4fv(Int32 location, Int32 count, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform1iv", ExactSpelling = true)]
            internal extern static void Uniform1iv(Int32 location, Int32 count, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform2iv", ExactSpelling = true)]
            internal extern static void Uniform2iv(Int32 location, Int32 count, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform3iv", ExactSpelling = true)]
            internal extern static void Uniform3iv(Int32 location, Int32 count, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform4iv", ExactSpelling = true)]
            internal extern static void Uniform4iv(Int32 location, Int32 count, Int32[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform1uiv", ExactSpelling = true)]
            internal extern static void Uniform1uiv(Int32 location, Int32 count, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform2uiv", ExactSpelling = true)]
            internal extern static void Uniform2uiv(Int32 location, Int32 count, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform3uiv", ExactSpelling = true)]
            internal extern static void Uniform3uiv(Int32 location, Int32 count, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniform4uiv", ExactSpelling = true)]
            internal extern static void Uniform4uiv(Int32 location, Int32 count, UInt32[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix2fv", ExactSpelling = true)]
            internal extern static void UniformMatrix2fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix3fv", ExactSpelling = true)]
            internal extern static void UniformMatrix3fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix4fv", ExactSpelling = true)]
            internal extern static void UniformMatrix4fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix2x3fv", ExactSpelling = true)]
            internal extern static void UniformMatrix2x3fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix3x2fv", ExactSpelling = true)]
            internal extern static void UniformMatrix3x2fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix2x4fv", ExactSpelling = true)]
            internal extern static void UniformMatrix2x4fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix4x2fv", ExactSpelling = true)]
            internal extern static void UniformMatrix4x2fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix3x4fv", ExactSpelling = true)]
            internal extern static void UniformMatrix3x4fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformMatrix4x3fv", ExactSpelling = true)]
            internal extern static void UniformMatrix4x3fv(Int32 location, Int32 count, Boolean transpose, Single[] value);
            [DllImport(Gl.Library, EntryPoint = "glUniformBlockBinding", ExactSpelling = true)]
            internal extern static void UniformBlockBinding(UInt32 program, UInt32 uniformBlockIndex, UInt32 uniformBlockBinding);
            [DllImport(Gl.Library, EntryPoint = "glUniformSubroutinesuiv", ExactSpelling = true)]
            internal extern static void UniformSubroutinesuiv(ShaderType shadertype, Int32 count, UInt32[] indices);
            [DllImport(Gl.Library, EntryPoint = "glUnmapBuffer", ExactSpelling = true)]
            internal extern static Boolean UnmapBuffer(BufferTarget target);
            [DllImport(Gl.Library, EntryPoint = "glUnmapNamedBuffer", ExactSpelling = true)]
            internal extern static Boolean UnmapNamedBuffer(UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glUseProgram", ExactSpelling = true)]
            internal extern static void UseProgram(UInt32 program);
            [DllImport(Gl.Library, EntryPoint = "glUseProgramStages", ExactSpelling = true)]
            internal extern static void UseProgramStages(UInt32 pipeline, UInt32 stages, UInt32 program);
            [DllImport(Gl.Library, EntryPoint = "glValidateProgram", ExactSpelling = true)]
            internal extern static void ValidateProgram(UInt32 program);
            [DllImport(Gl.Library, EntryPoint = "glValidateProgramPipeline", ExactSpelling = true)]
            internal extern static void ValidateProgramPipeline(UInt32 pipeline);
            [DllImport(Gl.Library, EntryPoint = "glVertexArrayElementBuffer", ExactSpelling = true)]
            internal extern static void VertexArrayElementBuffer(UInt32 vaobj, UInt32 buffer);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib1f", ExactSpelling = true)]
            internal extern static void VertexAttrib1f(UInt32 index, Single v0);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib1s", ExactSpelling = true)]
            internal extern static void VertexAttrib1s(UInt32 index, Int16 v0);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib1d", ExactSpelling = true)]
            internal extern static void VertexAttrib1d(UInt32 index, Double v0);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI1i", ExactSpelling = true)]
            internal extern static void VertexAttribI1i(UInt32 index, Int32 v0);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI1ui", ExactSpelling = true)]
            internal extern static void VertexAttribI1ui(UInt32 index, UInt32 v0);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib2f", ExactSpelling = true)]
            internal extern static void VertexAttrib2f(UInt32 index, Single v0, Single v1);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib2s", ExactSpelling = true)]
            internal extern static void VertexAttrib2s(UInt32 index, Int16 v0, Int16 v1);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib2d", ExactSpelling = true)]
            internal extern static void VertexAttrib2d(UInt32 index, Double v0, Double v1);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI2i", ExactSpelling = true)]
            internal extern static void VertexAttribI2i(UInt32 index, Int32 v0, Int32 v1);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI2ui", ExactSpelling = true)]
            internal extern static void VertexAttribI2ui(UInt32 index, UInt32 v0, UInt32 v1);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib3f", ExactSpelling = true)]
            internal extern static void VertexAttrib3f(UInt32 index, Single v0, Single v1, Single v2);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib3s", ExactSpelling = true)]
            internal extern static void VertexAttrib3s(UInt32 index, Int16 v0, Int16 v1, Int16 v2);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib3d", ExactSpelling = true)]
            internal extern static void VertexAttrib3d(UInt32 index, Double v0, Double v1, Double v2);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI3i", ExactSpelling = true)]
            internal extern static void VertexAttribI3i(UInt32 index, Int32 v0, Int32 v1, Int32 v2);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI3ui", ExactSpelling = true)]
            internal extern static void VertexAttribI3ui(UInt32 index, UInt32 v0, UInt32 v1, UInt32 v2);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4f", ExactSpelling = true)]
            internal extern static void VertexAttrib4f(UInt32 index, Single v0, Single v1, Single v2, Single v3);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4s", ExactSpelling = true)]
            internal extern static void VertexAttrib4s(UInt32 index, Int16 v0, Int16 v1, Int16 v2, Int16 v3);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4d", ExactSpelling = true)]
            internal extern static void VertexAttrib4d(UInt32 index, Double v0, Double v1, Double v2, Double v3);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4Nub", ExactSpelling = true)]
            internal extern static void VertexAttrib4Nub(UInt32 index, Byte v0, Byte v1, Byte v2, Byte v3);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI4i", ExactSpelling = true)]
            internal extern static void VertexAttribI4i(UInt32 index, Int32 v0, Int32 v1, Int32 v2, Int32 v3);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI4ui", ExactSpelling = true)]
            internal extern static void VertexAttribI4ui(UInt32 index, UInt32 v0, UInt32 v1, UInt32 v2, UInt32 v3);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribL1d", ExactSpelling = true)]
            internal extern static void VertexAttribL1d(UInt32 index, Double v0);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribL2d", ExactSpelling = true)]
            internal extern static void VertexAttribL2d(UInt32 index, Double v0, Double v1);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribL3d", ExactSpelling = true)]
            internal extern static void VertexAttribL3d(UInt32 index, Double v0, Double v1, Double v2);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribL4d", ExactSpelling = true)]
            internal extern static void VertexAttribL4d(UInt32 index, Double v0, Double v1, Double v2, Double v3);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib1fv", ExactSpelling = true)]
            internal extern static void VertexAttrib1fv(UInt32 index, Single[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib1sv", ExactSpelling = true)]
            internal extern static void VertexAttrib1sv(UInt32 index, Int16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib1dv", ExactSpelling = true)]
            internal extern static void VertexAttrib1dv(UInt32 index, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI1iv", ExactSpelling = true)]
            internal extern static void VertexAttribI1iv(UInt32 index, Int32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI1uiv", ExactSpelling = true)]
            internal extern static void VertexAttribI1uiv(UInt32 index, UInt32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib2fv", ExactSpelling = true)]
            internal extern static void VertexAttrib2fv(UInt32 index, Single[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib2sv", ExactSpelling = true)]
            internal extern static void VertexAttrib2sv(UInt32 index, Int16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib2dv", ExactSpelling = true)]
            internal extern static void VertexAttrib2dv(UInt32 index, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI2iv", ExactSpelling = true)]
            internal extern static void VertexAttribI2iv(UInt32 index, Int32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI2uiv", ExactSpelling = true)]
            internal extern static void VertexAttribI2uiv(UInt32 index, UInt32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib3fv", ExactSpelling = true)]
            internal extern static void VertexAttrib3fv(UInt32 index, Single[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib3sv", ExactSpelling = true)]
            internal extern static void VertexAttrib3sv(UInt32 index, Int16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib3dv", ExactSpelling = true)]
            internal extern static void VertexAttrib3dv(UInt32 index, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI3iv", ExactSpelling = true)]
            internal extern static void VertexAttribI3iv(UInt32 index, Int32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI3uiv", ExactSpelling = true)]
            internal extern static void VertexAttribI3uiv(UInt32 index, UInt32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4fv", ExactSpelling = true)]
            internal extern static void VertexAttrib4fv(UInt32 index, Single[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4sv", ExactSpelling = true)]
            internal extern static void VertexAttrib4sv(UInt32 index, Int16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4dv", ExactSpelling = true)]
            internal extern static void VertexAttrib4dv(UInt32 index, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4iv", ExactSpelling = true)]
            internal extern static void VertexAttrib4iv(UInt32 index, Int32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4bv", ExactSpelling = true)]
            internal extern static void VertexAttrib4bv(UInt32 index, SByte[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4ubv", ExactSpelling = true)]
            internal extern static void VertexAttrib4ubv(UInt32 index, Byte[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4usv", ExactSpelling = true)]
            internal extern static void VertexAttrib4usv(UInt32 index, UInt16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4uiv", ExactSpelling = true)]
            internal extern static void VertexAttrib4uiv(UInt32 index, UInt32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4Nbv", ExactSpelling = true)]
            internal extern static void VertexAttrib4Nbv(UInt32 index, SByte[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4Nsv", ExactSpelling = true)]
            internal extern static void VertexAttrib4Nsv(UInt32 index, Int16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4Niv", ExactSpelling = true)]
            internal extern static void VertexAttrib4Niv(UInt32 index, Int32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4Nubv", ExactSpelling = true)]
            internal extern static void VertexAttrib4Nubv(UInt32 index, Byte[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4Nusv", ExactSpelling = true)]
            internal extern static void VertexAttrib4Nusv(UInt32 index, UInt16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttrib4Nuiv", ExactSpelling = true)]
            internal extern static void VertexAttrib4Nuiv(UInt32 index, UInt32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI4bv", ExactSpelling = true)]
            internal extern static void VertexAttribI4bv(UInt32 index, SByte[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI4ubv", ExactSpelling = true)]
            internal extern static void VertexAttribI4ubv(UInt32 index, Byte[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI4sv", ExactSpelling = true)]
            internal extern static void VertexAttribI4sv(UInt32 index, Int16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI4usv", ExactSpelling = true)]
            internal extern static void VertexAttribI4usv(UInt32 index, UInt16[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI4iv", ExactSpelling = true)]
            internal extern static void VertexAttribI4iv(UInt32 index, Int32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribI4uiv", ExactSpelling = true)]
            internal extern static void VertexAttribI4uiv(UInt32 index, UInt32[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribL1dv", ExactSpelling = true)]
            internal extern static void VertexAttribL1dv(UInt32 index, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribL2dv", ExactSpelling = true)]
            internal extern static void VertexAttribL2dv(UInt32 index, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribL3dv", ExactSpelling = true)]
            internal extern static void VertexAttribL3dv(UInt32 index, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribL4dv", ExactSpelling = true)]
            internal extern static void VertexAttribL4dv(UInt32 index, Double[] v);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribP1ui", ExactSpelling = true)]
            internal extern static void VertexAttribP1ui(UInt32 index, VertexAttribPType type, Boolean normalized, UInt32 value);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribP2ui", ExactSpelling = true)]
            internal extern static void VertexAttribP2ui(UInt32 index, VertexAttribPType type, Boolean normalized, UInt32 value);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribP3ui", ExactSpelling = true)]
            internal extern static void VertexAttribP3ui(UInt32 index, VertexAttribPType type, Boolean normalized, UInt32 value);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribP4ui", ExactSpelling = true)]
            internal extern static void VertexAttribP4ui(UInt32 index, VertexAttribPType type, Boolean normalized, UInt32 value);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribBinding", ExactSpelling = true)]
            internal extern static void VertexAttribBinding(UInt32 attribindex, UInt32 bindingindex);
            [DllImport(Gl.Library, EntryPoint = "glVertexArrayAttribBinding", ExactSpelling = true)]
            internal extern static void VertexArrayAttribBinding(UInt32 vaobj, UInt32 attribindex, UInt32 bindingindex);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribDivisor", ExactSpelling = true)]
            internal extern static void VertexAttribDivisor(UInt32 index, UInt32 divisor);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribFormat", ExactSpelling = true)]
            internal extern static void VertexAttribFormat(UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, Boolean normalized, UInt32 relativeoffset);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribIFormat", ExactSpelling = true)]
            internal extern static void VertexAttribIFormat(UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, UInt32 relativeoffset);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribLFormat", ExactSpelling = true)]
            internal extern static void VertexAttribLFormat(UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, UInt32 relativeoffset);
            [DllImport(Gl.Library, EntryPoint = "glVertexArrayAttribFormat", ExactSpelling = true)]
            internal extern static void VertexArrayAttribFormat(UInt32 vaobj, UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, Boolean normalized, UInt32 relativeoffset);
            [DllImport(Gl.Library, EntryPoint = "glVertexArrayAttribIFormat", ExactSpelling = true)]
            internal extern static void VertexArrayAttribIFormat(UInt32 vaobj, UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, UInt32 relativeoffset);
            [DllImport(Gl.Library, EntryPoint = "glVertexArrayAttribLFormat", ExactSpelling = true)]
            internal extern static void VertexArrayAttribLFormat(UInt32 vaobj, UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, UInt32 relativeoffset);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribPointer", ExactSpelling = true)]
            internal extern static void VertexAttribPointer(UInt32 index, Int32 size, VertexAttribPointerType type, Boolean normalized, Int32 stride, IntPtr pointer);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribIPointer", ExactSpelling = true)]
            internal extern static void VertexAttribIPointer(UInt32 index, Int32 size, VertexAttribPointerType type, Int32 stride, IntPtr pointer);
            [DllImport(Gl.Library, EntryPoint = "glVertexAttribLPointer", ExactSpelling = true)]
            internal extern static void VertexAttribLPointer(UInt32 index, Int32 size, VertexAttribPointerType type, Int32 stride, IntPtr pointer);
            [DllImport(Gl.Library, EntryPoint = "glVertexBindingDivisor", ExactSpelling = true)]
            internal extern static void VertexBindingDivisor(UInt32 bindingindex, UInt32 divisor);
            [DllImport(Gl.Library, EntryPoint = "glVertexArrayBindingDivisor", ExactSpelling = true)]
            internal extern static void VertexArrayBindingDivisor(UInt32 vaobj, UInt32 bindingindex, UInt32 divisor);
            [DllImport(Gl.Library, EntryPoint = "glViewport", ExactSpelling = true)]
            internal extern static void Viewport(Int32 x, Int32 y, Int32 width, Int32 height);
            [DllImport(Gl.Library, EntryPoint = "glViewportArrayv", ExactSpelling = true)]
            internal extern static void ViewportArrayv(UInt32 first, Int32 count, Single[] v);
            [DllImport(Gl.Library, EntryPoint = "glViewportIndexedf", ExactSpelling = true)]
            internal extern static void ViewportIndexedf(UInt32 index, Single x, Single y, Single w, Single h);
            [DllImport(Gl.Library, EntryPoint = "glViewportIndexedfv", ExactSpelling = true)]
            internal extern static void ViewportIndexedfv(UInt32 index, Single[] v);
            [DllImport(Gl.Library, EntryPoint = "glWaitSync", ExactSpelling = true)]
            internal extern static void WaitSync(IntPtr sync, UInt32 flags, UInt64 timeout);
        }
    }
}
