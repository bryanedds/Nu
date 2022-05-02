using System;
using System.Collections.Generic;

namespace Nu
{
    partial class Gl
    {
        private static bool[] availableExtensions;

        /// <summary>
        /// Reload the list of extensions that are present in the current OpenGL context.
        /// </summary>
        public static void ReloadExtensions()
        {
            HashSet<string> stringExtensions = new HashSet<string>();

            if (Version() >= 3)
            {
                // OpenGL version >= 3 supports indexed extensions, and actually deprecated the legacy GetString method
                int[] int1 = new int[] { 0 };
                GetIntegerv(GetPName.NumExtensions, int1);
                int extensionCount = int1[0];

                for (int i = 0; i < extensionCount; i++)
                {
                    string extension = GetStringi(StringName.Extensions, (uint)i);
                    if (!stringExtensions.Contains(extension)) stringExtensions.Add(extension.ToLower());
                }
            }
            else
            {
                // Older versions of OpenGL have to use the normal GetString method, which returns a space delimited list of supported extensions
                var extensions = GetString(StringName.Extensions).Split(new char[] { '\0', ' ' });

                for (int i = 0; i < extensions.Length; i++)
                {
                    if (extensions[i].Length == 0) continue;
                    else if (!stringExtensions.Contains(extensions[i])) stringExtensions.Add(extensions[i].ToLower());
                }
            }

            // now convert the extension names back into Extension, which is easier on lookups
            var extensionNames = Enum.GetNames(typeof(Extension));
            availableExtensions = new bool[extensionNames.Length];

            for (int i = 0; i < extensionNames.Length; i++)
            {
                availableExtensions[i] = stringExtensions.Contains(extensionNames[i].ToLower());
            }
        }

        /// <summary>
        /// Check to see if an extension is supported in the current OpenGL context.
        /// </summary>
        /// <param name="name">The name of the extension to check.</param>
        /// <returns>True if the extension is supported, false if the extension is unavailable.</returns>
        public static bool IsExtensionSupported(Extension name)
        {
            if (availableExtensions == null) ReloadExtensions();

            // check if the extension is supported
            return availableExtensions[(int)name];
        }

        /// <summary>
        /// Exposes each extension as a boolean that can be queried at runtime.  The boolean will be true if the OpenGL extension is supported.
        /// Note:  The extensions are loaded lazily, so the first call to IsExtensionSupported may take some time.
        /// </summary>
        public static class Extensions
        {
#pragma warning disable CS1591
            // automatically generated
            public static bool Multitexture { get { return Gl.IsExtensionSupported(Extension.GL_ARB_multitexture); } }
            public static bool LXARBGetProcAddress_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_get_proc_address); } }
            public static bool TransposeMatrix { get { return Gl.IsExtensionSupported(Extension.GL_ARB_transpose_matrix); } }
            public static bool ARBBufferRegion_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_buffer_region); } }
            public static bool Multisample { get { return Gl.IsExtensionSupported(Extension.GL_ARB_multisample); } }
            public static bool LXARBMultisample_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_multisample); } }
            public static bool ARBMultisample_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_multisample); } }
            public static bool TextureEnvAdd { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_env_add); } }
            public static bool TextureCubeMap { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_cube_map); } }
            public static bool ARBExtensionsString_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_extensions_string); } }
            public static bool ARBPixelFormat_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_pixel_format); } }
            public static bool ARBMakeCurrentRead_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_make_current_read); } }
            public static bool ARBPbuffer_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_pbuffer); } }
            public static bool TextureCompression { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_compression); } }
            public static bool TextureBorderClamp { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_border_clamp); } }
            public static bool PointParameters { get { return Gl.IsExtensionSupported(Extension.GL_ARB_point_parameters); } }
            public static bool VertexBlend { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_blend); } }
            public static bool MatrixPalette { get { return Gl.IsExtensionSupported(Extension.GL_ARB_matrix_palette); } }
            public static bool TextureEnvCombine { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_env_combine); } }
            public static bool TextureEnvCrossbar { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_env_crossbar); } }
            public static bool TextureEnvDot3 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_env_dot3); } }
            public static bool ARBRenderTexture_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_render_texture); } }
            public static bool TextureMirroredRepeat { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_mirrored_repeat); } }
            public static bool DepthTexture { get { return Gl.IsExtensionSupported(Extension.GL_ARB_depth_texture); } }
            public static bool Shadow { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shadow); } }
            public static bool ShadowAmbient { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shadow_ambient); } }
            public static bool WindowPos { get { return Gl.IsExtensionSupported(Extension.GL_ARB_window_pos); } }
            public static bool VertexProgram { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_program); } }
            public static bool FragmentProgram { get { return Gl.IsExtensionSupported(Extension.GL_ARB_fragment_program); } }
            public static bool VertexBufferObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_buffer_object); } }
            public static bool LXARBVertexBufferObject_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_vertex_buffer_object); } }
            public static bool OcclusionQuery { get { return Gl.IsExtensionSupported(Extension.GL_ARB_occlusion_query); } }
            public static bool ShaderObjects { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_objects); } }
            public static bool VertexShader { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_shader); } }
            public static bool FragmentShader { get { return Gl.IsExtensionSupported(Extension.GL_ARB_fragment_shader); } }
            public static bool ShadingLanguage100 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shading_language_100); } }
            public static bool TextureNonPowerOfTwo { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_non_power_of_two); } }
            public static bool PointSprite { get { return Gl.IsExtensionSupported(Extension.GL_ARB_point_sprite); } }
            public static bool FragmentProgramShadow { get { return Gl.IsExtensionSupported(Extension.GL_ARB_fragment_program_shadow); } }
            public static bool DrawBuffers { get { return Gl.IsExtensionSupported(Extension.GL_ARB_draw_buffers); } }
            public static bool TextureRectangle { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_rectangle); } }
            public static bool ColorBufferFloat { get { return Gl.IsExtensionSupported(Extension.GL_ARB_color_buffer_float); } }
            public static bool ARBPixelFormatFloat_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_pixel_format_float); } }
            public static bool LXARBFbconfigFloat_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_fbconfig_float); } }
            public static bool HalfFloatPixel { get { return Gl.IsExtensionSupported(Extension.GL_ARB_half_float_pixel); } }
            public static bool TextureFloat { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_float); } }
            public static bool PixelBufferObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_pixel_buffer_object); } }
            public static bool DepthBufferFloat { get { return Gl.IsExtensionSupported(Extension.GL_ARB_depth_buffer_float); } }
            public static bool DrawInstanced { get { return Gl.IsExtensionSupported(Extension.GL_ARB_draw_instanced); } }
            public static bool FramebufferObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_framebuffer_object); } }
            public static bool FramebufferSRGB { get { return Gl.IsExtensionSupported(Extension.GL_ARB_framebuffer_sRGB); } }
            public static bool LXARBFramebufferSRGB_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_framebuffer_sRGB); } }
            public static bool ARBFramebufferSRGB_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_framebuffer_sRGB); } }
            public static bool GeometryShader4 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_geometry_shader4); } }
            public static bool HalfFloatVertex { get { return Gl.IsExtensionSupported(Extension.GL_ARB_half_float_vertex); } }
            public static bool InstancedArrays { get { return Gl.IsExtensionSupported(Extension.GL_ARB_instanced_arrays); } }
            public static bool MapBufferRange { get { return Gl.IsExtensionSupported(Extension.GL_ARB_map_buffer_range); } }
            public static bool TextureBufferObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_buffer_object); } }
            public static bool TextureCompressionRgtc { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_compression_rgtc); } }
            public static bool TextureRg { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_rg); } }
            public static bool VertexArrayObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_array_object); } }
            public static bool ARBCreateContext_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_create_context); } }
            public static bool LXARBCreateContext_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_create_context); } }
            public static bool UniformBufferObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_uniform_buffer_object); } }
            public static bool Compatibility { get { return Gl.IsExtensionSupported(Extension.GL_ARB_compatibility); } }
            public static bool CopyBuffer { get { return Gl.IsExtensionSupported(Extension.GL_ARB_copy_buffer); } }
            public static bool ShaderTextureLod { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_texture_lod); } }
            public static bool DepthClamp { get { return Gl.IsExtensionSupported(Extension.GL_ARB_depth_clamp); } }
            public static bool DrawElementsBaseVertex { get { return Gl.IsExtensionSupported(Extension.GL_ARB_draw_elements_base_vertex); } }
            public static bool FragmentCoordConventions { get { return Gl.IsExtensionSupported(Extension.GL_ARB_fragment_coord_conventions); } }
            public static bool ProvokingVertex { get { return Gl.IsExtensionSupported(Extension.GL_ARB_provoking_vertex); } }
            public static bool SeamlessCubeMap { get { return Gl.IsExtensionSupported(Extension.GL_ARB_seamless_cube_map); } }
            public static bool Sync { get { return Gl.IsExtensionSupported(Extension.GL_ARB_sync); } }
            public static bool TextureMultisample { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_multisample); } }
            public static bool VertexArrayBgra { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_array_bgra); } }
            public static bool DrawBuffersBlend { get { return Gl.IsExtensionSupported(Extension.GL_ARB_draw_buffers_blend); } }
            public static bool SampleShading { get { return Gl.IsExtensionSupported(Extension.GL_ARB_sample_shading); } }
            public static bool TextureCubeMapArray { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_cube_map_array); } }
            public static bool TextureGather { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_gather); } }
            public static bool TextureQueryLod { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_query_lod); } }
            public static bool ARBCreateContextProfile_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_create_context_profile); } }
            public static bool LXARBCreateContextProfile_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_create_context_profile); } }
            public static bool ShadingLanguageInclude { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shading_language_include); } }
            public static bool TextureCompressionBptc { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_compression_bptc); } }
            public static bool BlendFuncExtended { get { return Gl.IsExtensionSupported(Extension.GL_ARB_blend_func_extended); } }
            public static bool ExplicitAttribLocation { get { return Gl.IsExtensionSupported(Extension.GL_ARB_explicit_attrib_location); } }
            public static bool OcclusionQuery2 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_occlusion_query2); } }
            public static bool SamplerObjects { get { return Gl.IsExtensionSupported(Extension.GL_ARB_sampler_objects); } }
            public static bool ShaderBitEncoding { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_bit_encoding); } }
            public static bool TextureRgb10A2ui { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_rgb10_a2ui); } }
            public static bool TextureSwizzle { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_swizzle); } }
            public static bool TimerQuery { get { return Gl.IsExtensionSupported(Extension.GL_ARB_timer_query); } }
            public static bool VertexType2101010Rev { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_type_2_10_10_10_rev); } }
            public static bool DrawIndirect { get { return Gl.IsExtensionSupported(Extension.GL_ARB_draw_indirect); } }
            public static bool GpuShader5 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_gpu_shader5); } }
            public static bool GpuShaderFp64 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_gpu_shader_fp64); } }
            public static bool ShaderSubroutine { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_subroutine); } }
            public static bool TessellationShader { get { return Gl.IsExtensionSupported(Extension.GL_ARB_tessellation_shader); } }
            public static bool TextureBufferObjectRgb32 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_buffer_object_rgb32); } }
            public static bool TransformFeedback2 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_transform_feedback2); } }
            public static bool TransformFeedback3 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_transform_feedback3); } }
            public static bool ES2Compatibility { get { return Gl.IsExtensionSupported(Extension.GL_ARB_ES2_compatibility); } }
            public static bool GetProgramBinary { get { return Gl.IsExtensionSupported(Extension.GL_ARB_get_program_binary); } }
            public static bool SeparateShaderObjects { get { return Gl.IsExtensionSupported(Extension.GL_ARB_separate_shader_objects); } }
            public static bool ShaderPrecision { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_precision); } }
            public static bool VertexAttrib64bit { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_attrib_64bit); } }
            public static bool ViewportArray { get { return Gl.IsExtensionSupported(Extension.GL_ARB_viewport_array); } }
            public static bool LXARBCreateContextRobustness_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_create_context_robustness); } }
            public static bool ARBCreateContextRobustness_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_create_context_robustness); } }
            public static bool ClEvent { get { return Gl.IsExtensionSupported(Extension.GL_ARB_cl_event); } }
            public static bool DebugOutput { get { return Gl.IsExtensionSupported(Extension.GL_ARB_debug_output); } }
            public static bool Robustness { get { return Gl.IsExtensionSupported(Extension.GL_ARB_robustness); } }
            public static bool ShaderStencilExport { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_stencil_export); } }
            public static bool BaseInstance { get { return Gl.IsExtensionSupported(Extension.GL_ARB_base_instance); } }
            public static bool ShadingLanguage420pack { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shading_language_420pack); } }
            public static bool TransformFeedbackInstanced { get { return Gl.IsExtensionSupported(Extension.GL_ARB_transform_feedback_instanced); } }
            public static bool CompressedTexturePixelStorage { get { return Gl.IsExtensionSupported(Extension.GL_ARB_compressed_texture_pixel_storage); } }
            public static bool ConservativeDepth { get { return Gl.IsExtensionSupported(Extension.GL_ARB_conservative_depth); } }
            public static bool InternalformatQuery { get { return Gl.IsExtensionSupported(Extension.GL_ARB_internalformat_query); } }
            public static bool MapBufferAlignment { get { return Gl.IsExtensionSupported(Extension.GL_ARB_map_buffer_alignment); } }
            public static bool ShaderAtomicCounters { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_atomic_counters); } }
            public static bool ShaderImageLoadStore { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_image_load_store); } }
            public static bool ShadingLanguagePacking { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shading_language_packing); } }
            public static bool TextureStorage { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_storage); } }
            public static bool TextureCompressionAstcHdr_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_texture_compression_astc_hdr); } }
            public static bool TextureCompressionAstcLdr_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_texture_compression_astc_ldr); } }
            public static bool Debug_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_debug); } }
            public static bool ArraysOfArrays { get { return Gl.IsExtensionSupported(Extension.GL_ARB_arrays_of_arrays); } }
            public static bool ClearBufferObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_clear_buffer_object); } }
            public static bool ComputeShader { get { return Gl.IsExtensionSupported(Extension.GL_ARB_compute_shader); } }
            public static bool CopyImage { get { return Gl.IsExtensionSupported(Extension.GL_ARB_copy_image); } }
            public static bool TextureView { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_view); } }
            public static bool VertexAttribBinding { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_attrib_binding); } }
            public static bool RobustnessIsolation { get { return Gl.IsExtensionSupported(Extension.GL_ARB_robustness_isolation); } }
            public static bool ES3Compatibility { get { return Gl.IsExtensionSupported(Extension.GL_ARB_ES3_compatibility); } }
            public static bool ExplicitUniformLocation { get { return Gl.IsExtensionSupported(Extension.GL_ARB_explicit_uniform_location); } }
            public static bool FragmentLayerViewport { get { return Gl.IsExtensionSupported(Extension.GL_ARB_fragment_layer_viewport); } }
            public static bool FramebufferNoAttachments { get { return Gl.IsExtensionSupported(Extension.GL_ARB_framebuffer_no_attachments); } }
            public static bool InternalformatQuery2 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_internalformat_query2); } }
            public static bool InvalidateSubdata { get { return Gl.IsExtensionSupported(Extension.GL_ARB_invalidate_subdata); } }
            public static bool MultiDrawIndirect { get { return Gl.IsExtensionSupported(Extension.GL_ARB_multi_draw_indirect); } }
            public static bool ProgramInterfaceQuery { get { return Gl.IsExtensionSupported(Extension.GL_ARB_program_interface_query); } }
            public static bool RobustBufferAccessBehavior { get { return Gl.IsExtensionSupported(Extension.GL_ARB_robust_buffer_access_behavior); } }
            public static bool ShaderImageSize { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_image_size); } }
            public static bool ShaderStorageBufferObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_storage_buffer_object); } }
            public static bool StencilTexturing { get { return Gl.IsExtensionSupported(Extension.GL_ARB_stencil_texturing); } }
            public static bool TextureBufferRange { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_buffer_range); } }
            public static bool TextureQueryLevels { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_query_levels); } }
            public static bool TextureStorageMultisample { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_storage_multisample); } }
            public static bool LXARBRobustnessApplicationIsolation_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_robustness_application_isolation); } }
            public static bool LXARBRobustnessShareGroupIsolation_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_robustness_share_group_isolation); } }
            public static bool ARBRobustnessApplicationIsolation_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_robustness_application_isolation); } }
            public static bool ARBRobustnessShareGroupIsolation_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_robustness_share_group_isolation); } }
            public static bool BufferStorage { get { return Gl.IsExtensionSupported(Extension.GL_ARB_buffer_storage); } }
            public static bool ClearTexture { get { return Gl.IsExtensionSupported(Extension.GL_ARB_clear_texture); } }
            public static bool EnhancedLayouts { get { return Gl.IsExtensionSupported(Extension.GL_ARB_enhanced_layouts); } }
            public static bool MultiBind { get { return Gl.IsExtensionSupported(Extension.GL_ARB_multi_bind); } }
            public static bool QueryBufferObject { get { return Gl.IsExtensionSupported(Extension.GL_ARB_query_buffer_object); } }
            public static bool TextureMirrorClampToEdge { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_mirror_clamp_to_edge); } }
            public static bool TextureStencil8 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_stencil8); } }
            public static bool VertexType10f11f11fRev { get { return Gl.IsExtensionSupported(Extension.GL_ARB_vertex_type_10f_11f_11f_rev); } }
            public static bool BindlessTexture { get { return Gl.IsExtensionSupported(Extension.GL_ARB_bindless_texture); } }
            public static bool ComputeVariableGroupSize { get { return Gl.IsExtensionSupported(Extension.GL_ARB_compute_variable_group_size); } }
            public static bool IndirectParameters { get { return Gl.IsExtensionSupported(Extension.GL_ARB_indirect_parameters); } }
            public static bool SeamlessCubemapPerTexture { get { return Gl.IsExtensionSupported(Extension.GL_ARB_seamless_cubemap_per_texture); } }
            public static bool ShaderDrawParameters { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_draw_parameters); } }
            public static bool ShaderGroupVote { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_group_vote); } }
            public static bool SparseTexture { get { return Gl.IsExtensionSupported(Extension.GL_ARB_sparse_texture); } }
            public static bool ES31Compatibility { get { return Gl.IsExtensionSupported(Extension.GL_ARB_ES3_1_compatibility); } }
            public static bool ClipControl { get { return Gl.IsExtensionSupported(Extension.GL_ARB_clip_control); } }
            public static bool ConditionalRenderInverted { get { return Gl.IsExtensionSupported(Extension.GL_ARB_conditional_render_inverted); } }
            public static bool CullDistance { get { return Gl.IsExtensionSupported(Extension.GL_ARB_cull_distance); } }
            public static bool DerivativeControl { get { return Gl.IsExtensionSupported(Extension.GL_ARB_derivative_control); } }
            public static bool DirectStateAccess { get { return Gl.IsExtensionSupported(Extension.GL_ARB_direct_state_access); } }
            public static bool GetTextureSubImage { get { return Gl.IsExtensionSupported(Extension.GL_ARB_get_texture_sub_image); } }
            public static bool ShaderTextureImageSamples { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_texture_image_samples); } }
            public static bool TextureBarrier { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_barrier); } }
            public static bool ContextFlushControl_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_context_flush_control); } }
            public static bool LXARBContextFlushControl_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_ARB_context_flush_control); } }
            public static bool ARBContextFlushControl_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ARB_context_flush_control); } }
            public static bool RobustBufferAccessBehavior_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_robust_buffer_access_behavior); } }
            public static bool Robustness_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_robustness); } }
            public static bool PipelineStatisticsQuery { get { return Gl.IsExtensionSupported(Extension.GL_ARB_pipeline_statistics_query); } }
            public static bool SparseBuffer { get { return Gl.IsExtensionSupported(Extension.GL_ARB_sparse_buffer); } }
            public static bool TransformFeedbackOverflowQuery { get { return Gl.IsExtensionSupported(Extension.GL_ARB_transform_feedback_overflow_query); } }
            public static bool BlendEquationAdvanced_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_blend_equation_advanced); } }
            public static bool BlendEquationAdvancedCoherent_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_blend_equation_advanced_coherent); } }
            public static bool NoError_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_no_error); } }
            public static bool ES32Compatibility { get { return Gl.IsExtensionSupported(Extension.GL_ARB_ES3_2_compatibility); } }
            public static bool FragmentShaderInterlock { get { return Gl.IsExtensionSupported(Extension.GL_ARB_fragment_shader_interlock); } }
            public static bool GpuShaderInt64 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_gpu_shader_int64); } }
            public static bool ParallelShaderCompile { get { return Gl.IsExtensionSupported(Extension.GL_ARB_parallel_shader_compile); } }
            public static bool PostDepthCoverage { get { return Gl.IsExtensionSupported(Extension.GL_ARB_post_depth_coverage); } }
            public static bool SampleLocations { get { return Gl.IsExtensionSupported(Extension.GL_ARB_sample_locations); } }
            public static bool ShaderAtomicCounterOps { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_atomic_counter_ops); } }
            public static bool ShaderBallot { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_ballot); } }
            public static bool ShaderClock { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_clock); } }
            public static bool ShaderViewportLayerArray { get { return Gl.IsExtensionSupported(Extension.GL_ARB_shader_viewport_layer_array); } }
            public static bool SparseTexture2 { get { return Gl.IsExtensionSupported(Extension.GL_ARB_sparse_texture2); } }
            public static bool SparseTextureClamp { get { return Gl.IsExtensionSupported(Extension.GL_ARB_sparse_texture_clamp); } }
            public static bool TextureFilterMinmax { get { return Gl.IsExtensionSupported(Extension.GL_ARB_texture_filter_minmax); } }
            public static bool TextureCompressionAstcSliced3d_KHR { get { return Gl.IsExtensionSupported(Extension.GL_KHR_texture_compression_astc_sliced_3d); } }
            public static bool GlSpirv { get { return Gl.IsExtensionSupported(Extension.GL_ARB_gl_spirv); } }
            public static bool Abgr_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_abgr); } }
            public static bool BlendColor_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_blend_color); } }
            public static bool PolygonOffset_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_polygon_offset); } }
            public static bool Texture_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture); } }
            public static bool Texture3D_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture3D); } }
            public static bool TextureFilter4_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_texture_filter4); } }
            public static bool Subtexture_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_subtexture); } }
            public static bool CopyTexture_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_copy_texture); } }
            public static bool Histogram_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_histogram); } }
            public static bool Convolution_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_convolution); } }
            public static bool ColorMatrix_SGI { get { return Gl.IsExtensionSupported(Extension.GL_SGI_color_matrix); } }
            public static bool ColorTable_SGI { get { return Gl.IsExtensionSupported(Extension.GL_SGI_color_table); } }
            public static bool PixelTexture_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_pixel_texture); } }
            public static bool PixelTexture_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_pixel_texture); } }
            public static bool Texture4D_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_texture4D); } }
            public static bool TextureColorTable_SGI { get { return Gl.IsExtensionSupported(Extension.GL_SGI_texture_color_table); } }
            public static bool Cmyka_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_cmyka); } }
            public static bool TextureObject_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_object); } }
            public static bool DetailTexture_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_detail_texture); } }
            public static bool SharpenTexture_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_sharpen_texture); } }
            public static bool PackedPixels_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_packed_pixels); } }
            public static bool TextureLod_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_texture_lod); } }
            public static bool Multisample_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_multisample); } }
            public static bool LXSGISMultisample_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIS_multisample); } }
            public static bool RescaleNormal_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_rescale_normal); } }
            public static bool LXEXTVisualInfo_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_visual_info); } }
            public static bool VertexArray_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_vertex_array); } }
            public static bool MiscAttribute_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_misc_attribute); } }
            public static bool GenerateMipmap_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_generate_mipmap); } }
            public static bool Clipmap_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_clipmap); } }
            public static bool Shadow_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_shadow); } }
            public static bool TextureEdgeClamp_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_texture_edge_clamp); } }
            public static bool TextureBorderClamp_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_texture_border_clamp); } }
            public static bool BlendMinmax_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_blend_minmax); } }
            public static bool BlendSubtract_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_blend_subtract); } }
            public static bool BlendLogicOp_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_blend_logic_op); } }
            public static bool LXSGISwapControl_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGI_swap_control); } }
            public static bool LXSGIVideoSync_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGI_video_sync); } }
            public static bool LXSGIMakeCurrentRead_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGI_make_current_read); } }
            public static bool LXSGIXVideoSource_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_video_source); } }
            public static bool LXEXTVisualRating_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_visual_rating); } }
            public static bool Interlace_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_interlace); } }
            public static bool LXEXTImportContext_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_import_context); } }
            public static bool LXSGIXFbconfig_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_fbconfig); } }
            public static bool LXSGIXPbuffer_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_pbuffer); } }
            public static bool TextureSelect_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_texture_select); } }
            public static bool Sprite_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_sprite); } }
            public static bool TextureMultiBuffer_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_texture_multi_buffer); } }
            public static bool PointParameters_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_point_parameters); } }
            public static bool Instruments_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_instruments); } }
            public static bool TextureScaleBias_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_texture_scale_bias); } }
            public static bool Framezoom_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_framezoom); } }
            public static bool TagSampleBuffer_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_tag_sample_buffer); } }
            public static bool ReferencePlane_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_reference_plane); } }
            public static bool FlushRaster_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_flush_raster); } }
            public static bool LXSGICushion_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGI_cushion); } }
            public static bool DepthTexture_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_depth_texture); } }
            public static bool FogFunction_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_fog_function); } }
            public static bool FogOffset_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_fog_offset); } }
            public static bool ImageTransform_HP { get { return Gl.IsExtensionSupported(Extension.GL_HP_image_transform); } }
            public static bool ConvolutionBorderModes_HP { get { return Gl.IsExtensionSupported(Extension.GL_HP_convolution_border_modes); } }
            public static bool TextureAddEnv_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_texture_add_env); } }
            public static bool ColorSubtable_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_color_subtable); } }
            public static bool EXTObjectSpaceTess_GLU { get { return Gl.IsExtensionSupported(Extension.GLU_EXT_object_space_tess); } }
            public static bool VertexHints_PGI { get { return Gl.IsExtensionSupported(Extension.GL_PGI_vertex_hints); } }
            public static bool MiscHints_PGI { get { return Gl.IsExtensionSupported(Extension.GL_PGI_misc_hints); } }
            public static bool PalettedTexture_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_paletted_texture); } }
            public static bool ClipVolumeHint_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_clip_volume_hint); } }
            public static bool ListPriority_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_list_priority); } }
            public static bool IrInstrument1_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_ir_instrument1); } }
            public static bool LXSGIXVideoResize_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_video_resize); } }
            public static bool TextureLodBias_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_texture_lod_bias); } }
            public static bool SGIFilter4Parameters_GLU { get { return Gl.IsExtensionSupported(Extension.GLU_SGI_filter4_parameters); } }
            public static bool LXSGIXDmBuffer_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_dm_buffer); } }
            public static bool ShadowAmbient_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_shadow_ambient); } }
            public static bool LXSGIXSwapGroup_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_swap_group); } }
            public static bool LXSGIXSwapBarrier_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_swap_barrier); } }
            public static bool IndexTexture_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_index_texture); } }
            public static bool IndexMaterial_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_index_material); } }
            public static bool IndexFunc_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_index_func); } }
            public static bool IndexArrayFormats_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_index_array_formats); } }
            public static bool CompiledVertexArray_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_compiled_vertex_array); } }
            public static bool CullVertex_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_cull_vertex); } }
            public static bool EXTNurbsTessellator_GLU { get { return Gl.IsExtensionSupported(Extension.GLU_EXT_nurbs_tessellator); } }
            public static bool Ycrcb_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_ycrcb); } }
            public static bool FragmentLighting_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_fragment_lighting); } }
            public static bool RasterposClip_IBM { get { return Gl.IsExtensionSupported(Extension.GL_IBM_rasterpos_clip); } }
            public static bool TextureLighting_HP { get { return Gl.IsExtensionSupported(Extension.GL_HP_texture_lighting); } }
            public static bool DrawRangeElements_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_draw_range_elements); } }
            public static bool PhongShading_WIN { get { return Gl.IsExtensionSupported(Extension.GL_WIN_phong_shading); } }
            public static bool SpecularFog_WIN { get { return Gl.IsExtensionSupported(Extension.GL_WIN_specular_fog); } }
            public static bool LXSGISColorRange_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIS_color_range); } }
            public static bool ColorRange_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_color_range); } }
            public static bool LightTexture_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_light_texture); } }
            public static bool BlendAlphaMinmax_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_blend_alpha_minmax); } }
            public static bool SceneMarker_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_scene_marker); } }
            public static bool LXEXTSceneMarker_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_scene_marker); } }
            public static bool PixelTextureBits_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_pixel_texture_bits); } }
            public static bool Bgra_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_bgra); } }
            public static bool Async_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_async); } }
            public static bool AsyncPixel_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_async_pixel); } }
            public static bool AsyncHistogram_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_async_histogram); } }
            public static bool TextureScissor_INTEL { get { return Gl.IsExtensionSupported(Extension.GL_INTEL_texture_scissor); } }
            public static bool ParallelArrays_INTEL { get { return Gl.IsExtensionSupported(Extension.GL_INTEL_parallel_arrays); } }
            public static bool OcclusionTest_HP { get { return Gl.IsExtensionSupported(Extension.GL_HP_occlusion_test); } }
            public static bool PixelTransform_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_pixel_transform); } }
            public static bool PixelTransformColorTable_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_pixel_transform_color_table); } }
            public static bool SharedTexturePalette_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_shared_texture_palette); } }
            public static bool LXSGISBlendedOverlay_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIS_blended_overlay); } }
            public static bool SeparateSpecularColor_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_separate_specular_color); } }
            public static bool SecondaryColor_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_secondary_color); } }
            public static bool TextureEnv_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_env); } }
            public static bool TexturePerturbNormal_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_perturb_normal); } }
            public static bool MultiDrawArrays_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_multi_draw_arrays); } }
            public static bool MultiDrawArrays_SUN { get { return Gl.IsExtensionSupported(Extension.GL_SUN_multi_draw_arrays); } }
            public static bool FogCoord_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_fog_coord); } }
            public static bool ScreenCoordinates_REND { get { return Gl.IsExtensionSupported(Extension.GL_REND_screen_coordinates); } }
            public static bool CoordinateFrame_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_coordinate_frame); } }
            public static bool TextureEnvCombine_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_env_combine); } }
            public static bool SpecularVector_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_specular_vector); } }
            public static bool TransformHint_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_transform_hint); } }
            public static bool ConstantData_SUNX { get { return Gl.IsExtensionSupported(Extension.GL_SUNX_constant_data); } }
            public static bool GlobalAlpha_SUN { get { return Gl.IsExtensionSupported(Extension.GL_SUN_global_alpha); } }
            public static bool TriangleList_SUN { get { return Gl.IsExtensionSupported(Extension.GL_SUN_triangle_list); } }
            public static bool Vertex_SUN { get { return Gl.IsExtensionSupported(Extension.GL_SUN_vertex); } }
            public static bool EXTDisplayColorTable_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_display_color_table); } }
            public static bool EXTExtensionsString_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_extensions_string); } }
            public static bool EXTMakeCurrentRead_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_make_current_read); } }
            public static bool EXTPixelFormat_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_pixel_format); } }
            public static bool EXTPbuffer_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_pbuffer); } }
            public static bool EXTSwapControl_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_swap_control); } }
            public static bool BlendFuncSeparate_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_blend_func_separate); } }
            public static bool ColorClamp_INGR { get { return Gl.IsExtensionSupported(Extension.GL_INGR_color_clamp); } }
            public static bool InterlaceRead_INGR { get { return Gl.IsExtensionSupported(Extension.GL_INGR_interlace_read); } }
            public static bool StencilWrap_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_stencil_wrap); } }
            public static bool EXTDepthFloat_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_depth_float); } }
            public static bool _422Pixels_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_422_pixels); } }
            public static bool TexgenReflection_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texgen_reflection); } }
            public static bool TextureRange_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_texture_range); } }
            public static bool ConvolutionBorderModes_SUN { get { return Gl.IsExtensionSupported(Extension.GL_SUN_convolution_border_modes); } }
            public static bool LXSUNGetTransparentIndex_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SUN_get_transparent_index); } }
            public static bool TextureEnvAdd_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_env_add); } }
            public static bool TextureLodBias_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_lod_bias); } }
            public static bool TextureFilterAnisotropic_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_filter_anisotropic); } }
            public static bool VertexWeighting_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_vertex_weighting); } }
            public static bool LightMaxExponent_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_light_max_exponent); } }
            public static bool VertexArrayRange_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_array_range); } }
            public static bool RegisterCombiners_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_register_combiners); } }
            public static bool FogDistance_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fog_distance); } }
            public static bool TexgenEmboss_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texgen_emboss); } }
            public static bool BlendSquare_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_blend_square); } }
            public static bool TextureEnvCombine4_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_env_combine4); } }
            public static bool ResizeBuffers_MESA { get { return Gl.IsExtensionSupported(Extension.GL_MESA_resize_buffers); } }
            public static bool WindowPos_MESA { get { return Gl.IsExtensionSupported(Extension.GL_MESA_window_pos); } }
            public static bool TextureCompressionS3tc_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_compression_s3tc); } }
            public static bool CullVertex_IBM { get { return Gl.IsExtensionSupported(Extension.GL_IBM_cull_vertex); } }
            public static bool MultimodeDrawArrays_IBM { get { return Gl.IsExtensionSupported(Extension.GL_IBM_multimode_draw_arrays); } }
            public static bool VertexArrayLists_IBM { get { return Gl.IsExtensionSupported(Extension.GL_IBM_vertex_array_lists); } }
            public static bool TextureCompressionFXT1_3DFX { get { return Gl.IsExtensionSupported(Extension.GL_3DFX_texture_compression_FXT1); } }
            public static bool Multisample_3DFX { get { return Gl.IsExtensionSupported(Extension.GL_3DFX_multisample); } }
            public static bool Tbuffer_3DFX { get { return Gl.IsExtensionSupported(Extension.GL_3DFX_tbuffer); } }
            public static bool EXTMultisample_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_multisample); } }
            public static bool Multisample_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_multisample); } }
            public static bool VertexPreclip_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_vertex_preclip); } }
            public static bool VertexPreclipHint_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_vertex_preclip_hint); } }
            public static bool ConvolutionAccuracy_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_convolution_accuracy); } }
            public static bool Resample_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_resample); } }
            public static bool PointLineTexgen_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_point_line_texgen); } }
            public static bool TextureColorMask_SGIS { get { return Gl.IsExtensionSupported(Extension.GL_SGIS_texture_color_mask); } }
            public static bool LXMESACopySubBuffer_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_MESA_copy_sub_buffer); } }
            public static bool LXMESAPixmapColormap_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_MESA_pixmap_colormap); } }
            public static bool LXMESAReleaseBuffers_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_MESA_release_buffers); } }
            public static bool LXMESASet3dfxMode_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_MESA_set_3dfx_mode); } }
            public static bool TextureEnvDot3_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_env_dot3); } }
            public static bool TextureMirrorOnce_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_texture_mirror_once); } }
            public static bool Fence_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fence); } }
            public static bool StaticData_IBM { get { return Gl.IsExtensionSupported(Extension.GL_IBM_static_data); } }
            public static bool TextureMirroredRepeat_IBM { get { return Gl.IsExtensionSupported(Extension.GL_IBM_texture_mirrored_repeat); } }
            public static bool Evaluators_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_evaluators); } }
            public static bool PackedDepthStencil_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_packed_depth_stencil); } }
            public static bool RegisterCombiners2_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_register_combiners2); } }
            public static bool TextureCompressionVtc_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_compression_vtc); } }
            public static bool TextureRectangle_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_rectangle); } }
            public static bool TextureShader_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_shader); } }
            public static bool TextureShader2_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_shader2); } }
            public static bool VertexArrayRange2_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_array_range2); } }
            public static bool VertexProgram_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_program); } }
            public static bool LXSGIXVisualSelectGroup_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_visual_select_group); } }
            public static bool TextureCoordinateClamp_SGIX { get { return Gl.IsExtensionSupported(Extension.GL_SGIX_texture_coordinate_clamp); } }
            public static bool LXOMLSwapMethod_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_OML_swap_method); } }
            public static bool LXOMLSyncControl_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_OML_sync_control); } }
            public static bool Interlace_OML { get { return Gl.IsExtensionSupported(Extension.GL_OML_interlace); } }
            public static bool Subsample_OML { get { return Gl.IsExtensionSupported(Extension.GL_OML_subsample); } }
            public static bool Resample_OML { get { return Gl.IsExtensionSupported(Extension.GL_OML_resample); } }
            public static bool OMLSyncControl_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_OML_sync_control); } }
            public static bool CopyDepthToColor_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_copy_depth_to_color); } }
            public static bool EnvmapBumpmap_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_envmap_bumpmap); } }
            public static bool FragmentShader_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_fragment_shader); } }
            public static bool PnTriangles_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_pn_triangles); } }
            public static bool VertexArrayObject_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_vertex_array_object); } }
            public static bool VertexShader_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_vertex_shader); } }
            public static bool VertexStreams_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_vertex_streams); } }
            public static bool I3DDigitalVideoControl_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_I3D_digital_video_control); } }
            public static bool I3DGamma_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_I3D_gamma); } }
            public static bool I3DGenlock_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_I3D_genlock); } }
            public static bool I3DImageBuffer_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_I3D_image_buffer); } }
            public static bool I3DSwapFrameLock_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_I3D_swap_frame_lock); } }
            public static bool I3DSwapFrameUsage_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_I3D_swap_frame_usage); } }
            public static bool ElementArray_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_element_array); } }
            public static bool MeshArray_SUN { get { return Gl.IsExtensionSupported(Extension.GL_SUN_mesh_array); } }
            public static bool SliceAccum_SUN { get { return Gl.IsExtensionSupported(Extension.GL_SUN_slice_accum); } }
            public static bool MultisampleFilterHint_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_multisample_filter_hint); } }
            public static bool DepthClamp_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_depth_clamp); } }
            public static bool OcclusionQuery_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_occlusion_query); } }
            public static bool PointSprite_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_point_sprite); } }
            public static bool NVRenderDepthTexture_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_render_depth_texture); } }
            public static bool NVRenderTextureRectangle_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_render_texture_rectangle); } }
            public static bool TextureShader3_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_shader3); } }
            public static bool VertexProgram11_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_program1_1); } }
            public static bool ShadowFuncs_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_shadow_funcs); } }
            public static bool StencilTwoSide_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_stencil_two_side); } }
            public static bool TextFragmentShader_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_text_fragment_shader); } }
            public static bool ClientStorage_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_client_storage); } }
            public static bool ElementArray_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_element_array); } }
            public static bool Fence_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_fence); } }
            public static bool VertexArrayObject_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_vertex_array_object); } }
            public static bool VertexArrayRange_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_vertex_array_range); } }
            public static bool Ycbcr422_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_ycbcr_422); } }
            public static bool S3tc_S3 { get { return Gl.IsExtensionSupported(Extension.GL_S3_s3tc); } }
            public static bool DrawBuffers_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_draw_buffers); } }
            public static bool ATIPixelFormatFloat_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_ATI_pixel_format_float); } }
            public static bool TextureEnvCombine3_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_texture_env_combine3); } }
            public static bool TextureFloat_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_texture_float); } }
            public static bool FloatBuffer_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_float_buffer); } }
            public static bool NVFloatBuffer_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_float_buffer); } }
            public static bool FragmentProgram_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fragment_program); } }
            public static bool HalfFloat_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_half_float); } }
            public static bool PixelDataRange_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_pixel_data_range); } }
            public static bool PrimitiveRestart_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_primitive_restart); } }
            public static bool TextureExpandNormal_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_expand_normal); } }
            public static bool VertexProgram2_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_program2); } }
            public static bool MapObjectBuffer_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_map_object_buffer); } }
            public static bool SeparateStencil_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_separate_stencil); } }
            public static bool VertexAttribArrayObject_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_vertex_attrib_array_object); } }
            public static bool ByteCoordinates_OES { get { return Gl.IsExtensionSupported(Extension.GL_OES_byte_coordinates); } }
            public static bool FixedPoint_OES { get { return Gl.IsExtensionSupported(Extension.GL_OES_fixed_point); } }
            public static bool SinglePrecision_OES { get { return Gl.IsExtensionSupported(Extension.GL_OES_single_precision); } }
            public static bool CompressedPalettedTexture_OES { get { return Gl.IsExtensionSupported(Extension.GL_OES_compressed_paletted_texture); } }
            public static bool ReadFormat_OES { get { return Gl.IsExtensionSupported(Extension.GL_OES_read_format); } }
            public static bool QueryMatrix_OES { get { return Gl.IsExtensionSupported(Extension.GL_OES_query_matrix); } }
            public static bool DepthBoundsTest_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_depth_bounds_test); } }
            public static bool TextureMirrorClamp_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_mirror_clamp); } }
            public static bool BlendEquationSeparate_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_blend_equation_separate); } }
            public static bool PackInvert_MESA { get { return Gl.IsExtensionSupported(Extension.GL_MESA_pack_invert); } }
            public static bool YcbcrTexture_MESA { get { return Gl.IsExtensionSupported(Extension.GL_MESA_ycbcr_texture); } }
            public static bool PixelBufferObject_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_pixel_buffer_object); } }
            public static bool FragmentProgramOption_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fragment_program_option); } }
            public static bool FragmentProgram2_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fragment_program2); } }
            public static bool VertexProgram2Option_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_program2_option); } }
            public static bool VertexProgram3_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_program3); } }
            public static bool LXSGIXHyperpipe_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_SGIX_hyperpipe); } }
            public static bool LXMESAAgpOffset_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_MESA_agp_offset); } }
            public static bool TextureCompressionDxt1_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_compression_dxt1); } }
            public static bool FramebufferObject_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_framebuffer_object); } }
            public static bool StringMarker_GREMEDY { get { return Gl.IsExtensionSupported(Extension.GL_GREMEDY_string_marker); } }
            public static bool PackedDepthStencil_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_packed_depth_stencil); } }
            public static bool _3DLStereoControl_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_3DL_stereo_control); } }
            public static bool StencilClearTag_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_stencil_clear_tag); } }
            public static bool TextureSRGB_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_sRGB); } }
            public static bool FramebufferBlit_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_framebuffer_blit); } }
            public static bool FramebufferMultisample_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_framebuffer_multisample); } }
            public static bool TextureStack_MESAX { get { return Gl.IsExtensionSupported(Extension.GL_MESAX_texture_stack); } }
            public static bool TimerQuery_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_timer_query); } }
            public static bool GpuProgramParameters_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_gpu_program_parameters); } }
            public static bool FlushBufferRange_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_flush_buffer_range); } }
            public static bool GpuProgram4_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_gpu_program4); } }
            public static bool GeometryProgram4_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_geometry_program4); } }
            public static bool GeometryShader4_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_geometry_shader4); } }
            public static bool VertexProgram4_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_program4); } }
            public static bool GpuShader4_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_gpu_shader4); } }
            public static bool DrawInstanced_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_draw_instanced); } }
            public static bool PackedFloat_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_packed_float); } }
            public static bool EXTPixelFormatPackedFloat_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_pixel_format_packed_float); } }
            public static bool LXEXTFbconfigPackedFloat_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_fbconfig_packed_float); } }
            public static bool TextureArray_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_array); } }
            public static bool TextureBufferObject_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_buffer_object); } }
            public static bool TextureCompressionLatc_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_compression_latc); } }
            public static bool TextureCompressionRgtc_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_compression_rgtc); } }
            public static bool TextureSharedExponent_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_shared_exponent); } }
            public static bool DepthBufferFloat_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_depth_buffer_float); } }
            public static bool FragmentProgram4_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fragment_program4); } }
            public static bool FramebufferMultisampleCoverage_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_framebuffer_multisample_coverage); } }
            public static bool FramebufferSRGB_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_framebuffer_sRGB); } }
            public static bool LXEXTFramebufferSRGB_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_framebuffer_sRGB); } }
            public static bool EXTFramebufferSRGB_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_framebuffer_sRGB); } }
            public static bool GeometryShader4_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_geometry_shader4); } }
            public static bool ParameterBufferObject_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_parameter_buffer_object); } }
            public static bool DrawBuffers2_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_draw_buffers2); } }
            public static bool TransformFeedback_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_transform_feedback); } }
            public static bool BindableUniform_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_bindable_uniform); } }
            public static bool TextureInteger_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_integer); } }
            public static bool LXEXTTextureFromPixmap_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_texture_from_pixmap); } }
            public static bool FrameTerminator_GREMEDY { get { return Gl.IsExtensionSupported(Extension.GL_GREMEDY_frame_terminator); } }
            public static bool ConditionalRender_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_conditional_render); } }
            public static bool PresentVideo_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_present_video); } }
            public static bool LXNVPresentVideo_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_NV_present_video); } }
            public static bool NVPresentVideo_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_present_video); } }
            public static bool LXNVVideoOut_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_NV_video_out); } }
            public static bool NVVideoOutput_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_video_output); } }
            public static bool LXNVSwapGroup_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_NV_swap_group); } }
            public static bool NVSwapGroup_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_swap_group); } }
            public static bool TransformFeedback_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_transform_feedback); } }
            public static bool DirectStateAccess_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_direct_state_access); } }
            public static bool VertexArrayBgra_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_vertex_array_bgra); } }
            public static bool NVGpuAffinity_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_gpu_affinity); } }
            public static bool TextureSwizzle_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_swizzle); } }
            public static bool ExplicitMultisample_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_explicit_multisample); } }
            public static bool TransformFeedback2_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_transform_feedback2); } }
            public static bool Meminfo_ATI { get { return Gl.IsExtensionSupported(Extension.GL_ATI_meminfo); } }
            public static bool PerformanceMonitor_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_performance_monitor); } }
            public static bool AMDGpuAssociation_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_AMD_gpu_association); } }
            public static bool TextureTexture4_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_texture_texture4); } }
            public static bool VertexShaderTessellator_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_vertex_shader_tessellator); } }
            public static bool ProvokingVertex_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_provoking_vertex); } }
            public static bool TextureSnorm_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_snorm); } }
            public static bool DrawBuffersBlend_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_draw_buffers_blend); } }
            public static bool TextureRange_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_texture_range); } }
            public static bool FloatPixels_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_float_pixels); } }
            public static bool VertexProgramEvaluators_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_vertex_program_evaluators); } }
            public static bool AuxDepthStencil_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_aux_depth_stencil); } }
            public static bool ObjectPurgeable_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_object_purgeable); } }
            public static bool RowBytes_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_row_bytes); } }
            public static bool Rgb422_APPLE { get { return Gl.IsExtensionSupported(Extension.GL_APPLE_rgb_422); } }
            public static bool VideoCapture_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_video_capture); } }
            public static bool LXNVVideoCapture_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_NV_video_capture); } }
            public static bool NVVideoCapture_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_video_capture); } }
            public static bool SwapControl_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_swap_control); } }
            public static bool CopyImage_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_copy_image); } }
            public static bool NVCopyImage_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_copy_image); } }
            public static bool LXNVCopyImage_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_NV_copy_image); } }
            public static bool SeparateShaderObjects_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_separate_shader_objects); } }
            public static bool ParameterBufferObject2_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_parameter_buffer_object2); } }
            public static bool ShaderBufferLoad_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_buffer_load); } }
            public static bool VertexBufferUnifiedMemory_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_buffer_unified_memory); } }
            public static bool TextureBarrier_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_barrier); } }
            public static bool ShaderStencilExport_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_shader_stencil_export); } }
            public static bool SeamlessCubemapPerTexture_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_seamless_cubemap_per_texture); } }
            public static bool LXINTELSwapEvent_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_INTEL_swap_event); } }
            public static bool ConservativeDepth_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_conservative_depth); } }
            public static bool ShaderImageLoadStore_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_shader_image_load_store); } }
            public static bool VertexAttrib64bit_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_vertex_attrib_64bit); } }
            public static bool GpuProgram5_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_gpu_program5); } }
            public static bool GpuShader5_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_gpu_shader5); } }
            public static bool ShaderBufferStore_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_buffer_store); } }
            public static bool TessellationProgram5_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_tessellation_program5); } }
            public static bool VertexAttribInteger64bit_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vertex_attrib_integer_64bit); } }
            public static bool MultisampleCoverage_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_multisample_coverage); } }
            public static bool NameGenDelete_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_name_gen_delete); } }
            public static bool DebugOutput_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_debug_output); } }
            public static bool VdpauInterop_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_vdpau_interop); } }
            public static bool TransformFeedback3LinesTriangles_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_transform_feedback3_lines_triangles); } }
            public static bool LXAMDGpuAssociation_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_AMD_gpu_association); } }
            public static bool LXEXTCreateContextEs2Profile_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_create_context_es2_profile); } }
            public static bool LXEXTCreateContextEsProfile_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_create_context_es_profile); } }
            public static bool EXTCreateContextEs2Profile_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_create_context_es2_profile); } }
            public static bool EXTCreateContextEsProfile_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_create_context_es_profile); } }
            public static bool DepthClampSeparate_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_depth_clamp_separate); } }
            public static bool TextureSRGBDecode_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_sRGB_decode); } }
            public static bool TextureMultisample_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_texture_multisample); } }
            public static bool BlendMinmaxFactor_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_blend_minmax_factor); } }
            public static bool SamplePositions_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_sample_positions); } }
            public static bool X11SyncObject_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_x11_sync_object); } }
            public static bool NVDXInterop_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_DX_interop); } }
            public static bool MultiDrawIndirect_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_multi_draw_indirect); } }
            public static bool FramebufferMultisampleBlitScaled_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_framebuffer_multisample_blit_scaled); } }
            public static bool PathRendering_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_path_rendering); } }
            public static bool PinnedMemory_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_pinned_memory); } }
            public static bool NVDXInterop2_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_DX_interop2); } }
            public static bool StencilOperationExtended_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_stencil_operation_extended); } }
            public static bool LXEXTSwapControlTear_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_swap_control_tear); } }
            public static bool EXTSwapControlTear_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_EXT_swap_control_tear); } }
            public static bool VertexShaderViewportIndex_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_vertex_shader_viewport_index); } }
            public static bool VertexShaderLayer_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_vertex_shader_layer); } }
            public static bool BindlessTexture_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_bindless_texture); } }
            public static bool ShaderAtomicFloat_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_atomic_float); } }
            public static bool QueryBufferObject_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_query_buffer_object); } }
            public static bool ComputeProgram5_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_compute_program5); } }
            public static bool ShaderStorageBufferObject_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_storage_buffer_object); } }
            public static bool ShaderAtomicCounters_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_atomic_counters); } }
            public static bool DeepTexture3D_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_deep_texture3D); } }
            public static bool ConditionalRender_NVX { get { return Gl.IsExtensionSupported(Extension.GL_NVX_conditional_render); } }
            public static bool SparseTexture_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_sparse_texture); } }
            public static bool LXEXTBufferAge_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_buffer_age); } }
            public static bool ShaderTrinaryMinmax_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_shader_trinary_minmax); } }
            public static bool MapTexture_INTEL { get { return Gl.IsExtensionSupported(Extension.GL_INTEL_map_texture); } }
            public static bool DrawTexture_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_draw_texture); } }
            public static bool InterleavedElements_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_interleaved_elements); } }
            public static bool BindlessMultiDrawIndirect_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_bindless_multi_draw_indirect); } }
            public static bool BlendEquationAdvanced_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_blend_equation_advanced); } }
            public static bool BlendEquationAdvancedCoherent_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_blend_equation_advanced_coherent); } }
            public static bool GpuProgram5MemExtended_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_gpu_program5_mem_extended); } }
            public static bool ShaderAtomicCounterOps_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_shader_atomic_counter_ops); } }
            public static bool NVDelayBeforeSwap_WGL { get { return Gl.IsExtensionSupported(Extension.WGL_NV_delay_before_swap); } }
            public static bool ShaderIntegerMix_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_shader_integer_mix); } }
            public static bool GpuMemoryInfo_NVX { get { return Gl.IsExtensionSupported(Extension.GL_NVX_gpu_memory_info); } }
            public static bool DebugLabel_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_debug_label); } }
            public static bool DebugMarker_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_debug_marker); } }
            public static bool FragmentShaderOrdering_INTEL { get { return Gl.IsExtensionSupported(Extension.GL_INTEL_fragment_shader_ordering); } }
            public static bool OcclusionQueryEvent_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_occlusion_query_event); } }
            public static bool PerformanceQuery_INTEL { get { return Gl.IsExtensionSupported(Extension.GL_INTEL_performance_query); } }
            public static bool ShaderStencilValueExport_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_shader_stencil_value_export); } }
            public static bool LXNVDelayBeforeSwap_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_NV_delay_before_swap); } }
            public static bool LXMESAQueryRenderer_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_MESA_query_renderer); } }
            public static bool ShaderThreadGroup_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_thread_group); } }
            public static bool ShaderThreadShuffle_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_thread_shuffle); } }
            public static bool ShaderImageLoadFormatted_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_shader_image_load_formatted); } }
            public static bool TransformFeedback4_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_transform_feedback4); } }
            public static bool GpuShaderInt64_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_gpu_shader_int64); } }
            public static bool GlxStereoTree_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_glx_stereo_tree); } }
            public static bool GcnShader_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_gcn_shader); } }
            public static bool ShaderAtomicInt64_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_atomic_int64); } }
            public static bool BindlessMultiDrawIndirectCount_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_bindless_multi_draw_indirect_count); } }
            public static bool LXNVCopyBuffer_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_NV_copy_buffer); } }
            public static bool UniformBufferUnifiedMemory_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_uniform_buffer_unified_memory); } }
            public static bool PolygonOffsetClamp_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_polygon_offset_clamp); } }
            public static bool PostDepthCoverage_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_post_depth_coverage); } }
            public static bool RasterMultisample_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_raster_multisample); } }
            public static bool SparseTexture2_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_sparse_texture2); } }
            public static bool TextureFilterMinmax_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_texture_filter_minmax); } }
            public static bool ConservativeRaster_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_conservative_raster); } }
            public static bool FillRectangle_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fill_rectangle); } }
            public static bool FragmentCoverageToColor_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fragment_coverage_to_color); } }
            public static bool FragmentShaderInterlock_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_fragment_shader_interlock); } }
            public static bool FramebufferMixedSamples_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_framebuffer_mixed_samples); } }
            public static bool GeometryShaderPassthrough_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_geometry_shader_passthrough); } }
            public static bool PathRenderingSharedEdge_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_path_rendering_shared_edge); } }
            public static bool SampleLocations_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_sample_locations); } }
            public static bool SampleMaskOverrideCoverage_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_sample_mask_override_coverage); } }
            public static bool ShaderAtomicFp16Vector_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_atomic_fp16_vector); } }
            public static bool InternalformatSampleQuery_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_internalformat_sample_query); } }
            public static bool ViewportArray2_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_viewport_array2); } }
            public static bool CommandList_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_command_list); } }
            public static bool Multiview_OVR { get { return Gl.IsExtensionSupported(Extension.GL_OVR_multiview); } }
            public static bool Multiview2_OVR { get { return Gl.IsExtensionSupported(Extension.GL_OVR_multiview2); } }
            public static bool ConservativeRasterDilate_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_conservative_raster_dilate); } }
            public static bool FramebufferCMAA_INTEL { get { return Gl.IsExtensionSupported(Extension.GL_INTEL_framebuffer_CMAA); } }
            public static bool LXEXTLibglvnd_GLX { get { return Gl.IsExtensionSupported(Extension.GLX_EXT_libglvnd); } }
            public static bool ViewportSwizzle_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_viewport_swizzle); } }
            public static bool RobustnessVideoMemoryPurge_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_robustness_video_memory_purge); } }
            public static bool ShaderExplicitVertexParameter_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_shader_explicit_vertex_parameter); } }
            public static bool ClipSpaceWScaling_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_clip_space_w_scaling); } }
            public static bool ConservativeRasterPreSnapTriangles_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_conservative_raster_pre_snap_triangles); } }
            public static bool ShaderAtomicFloat64_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_shader_atomic_float64); } }
            public static bool StereoViewRendering_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_stereo_view_rendering); } }
            public static bool WindowRectangles_EXT { get { return Gl.IsExtensionSupported(Extension.GL_EXT_window_rectangles); } }
            public static bool ConservativeRasterization_INTEL { get { return Gl.IsExtensionSupported(Extension.GL_INTEL_conservative_rasterization); } }
            public static bool BlendEquationAdvancedMultiDrawBuffers_NVX { get { return Gl.IsExtensionSupported(Extension.GL_NVX_blend_equation_advanced_multi_draw_buffers); } }
            public static bool LinkedGpuMulticast_NVX { get { return Gl.IsExtensionSupported(Extension.GL_NVX_linked_gpu_multicast); } }
            public static bool GpuMulticast_NV { get { return Gl.IsExtensionSupported(Extension.GL_NV_gpu_multicast); } }
            public static bool ShaderIntegerFunctions_MESA { get { return Gl.IsExtensionSupported(Extension.GL_MESA_shader_integer_functions); } }
            public static bool GpuShaderHalfFloat_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_gpu_shader_half_float); } }
            public static bool ShaderBallot_AMD { get { return Gl.IsExtensionSupported(Extension.GL_AMD_shader_ballot); } }
#pragma warning restore
        }
    }
}
