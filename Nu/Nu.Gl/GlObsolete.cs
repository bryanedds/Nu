using System;
using System.Runtime.InteropServices;

namespace Nu
{
    partial class Gl
    {
        /// <summary>
        /// Record the GL time into a query object after all previous commands have reached the GL server but have not yet necessarily executed.
        /// <para>
        /// glQueryCounter causes the GL to record the current time into the query object named id. target must
        /// be GL_TIMESTAMP. The time is recorded after all previous commands on the GL client and server state
        /// and the framebuffer have been fully realized. When the time is recorded, the query result for that
        /// object is marked available. glQueryCounter timer queries can be used within a glBeginQuery /
        /// glEndQuery block where the target is GL_TIME_ELAPSED and it does not affect the result of that query
        /// object.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specify the name of a query object into which to record the GL time.
        /// </param>
        /// <param name="target">
        /// Specify the counter to query. target must be GL_TIMESTAMP.
        /// </param>
        [Obsolete("QueryCounter(UInt32, Int32) is deprecated, please use QueryCounter(UInt32, QueryTarget) instead.")]
        public static void QueryCounter(UInt32 id, Int32 target)
        {
            Delegates.glQueryCounter(id, (QueryTarget)target);
        }

        /// <summary>
        /// Select a color buffer source for pixels.
        /// <para>
        /// glReadBuffer specifies a color buffer as the source for subsequent glReadPixels, glCopyTexImage1D,
        /// glCopyTexImage2D, glCopyTexSubImage1D, glCopyTexSubImage2D, and glCopyTexSubImage3D commands. mode
        /// accepts one of twelve or more predefined values. In a fully configured system, GL_FRONT, GL_LEFT,
        /// and GL_FRONT_LEFT all name the front left buffer, GL_FRONT_RIGHT and GL_RIGHT name the front right
        /// buffer, and GL_BACK_LEFT and GL_BACK name the back left buffer. Further more, the constants
        /// GL_COLOR_ATTACHMENTi may be used to indicate the ith color attachment where i ranges from zero to
        /// the value of GL_MAX_COLOR_ATTACHMENTS minus one.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glNamedFramebufferReadBuffer function.
        /// </param>
        /// <param name="mode">
        /// Specifies a color buffer. Accepted values are GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT,
        /// GL_BACK_RIGHT, GL_FRONT, GL_BACK, GL_LEFT, GL_RIGHT, and the constants GL_COLOR_ATTACHMENTi.
        /// </param>
        [Obsolete("NamedFramebufferReadBuffer(UInt32, BeginMode) is deprecated, please use NamedFramebufferReadBuffer(ReadBufferMode, BeginMode) instead.")]
        public static void NamedFramebufferReadBuffer(UInt32 framebuffer, BeginMode mode)
        {
            Delegates.glNamedFramebufferReadBuffer((ReadBufferMode)framebuffer, mode);
        }

        /// <summary>
        /// Set sampler parameters.
        /// <para>
        /// glSamplerParameter assigns the value or values in params to the sampler parameter specified as
        /// pname. sampler specifies the sampler object to be modified, and must be the name of a sampler object
        /// previously returned from a call to glGenSamplers. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="sampler">
        /// Specifies the sampler object whose parameter to modify.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a sampler parameter. pname can be one of the following:
        /// GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, GL_TEXTURE_WRAP_R, GL_TEXTURE_MIN_FILTER,
        /// GL_TEXTURE_MAG_FILTER, GL_TEXTURE_BORDER_COLOR, GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD,
        /// GL_TEXTURE_LOD_BIAS GL_TEXTURE_COMPARE_MODE, or GL_TEXTURE_COMPARE_FUNC.
        /// </param>
        /// <param name="param">
        /// For the scalar commands, specifies the value of pname.
        /// </param>
        [Obsolete("SamplerParameterf(UInt32, Int32, Single) is deprecated, please use SamplerParameterf(UInt32, TextureParameterName, Single) instead.")]
        public static void SamplerParameterf(UInt32 sampler, Int32 pname, Single param)
        {
            Delegates.glSamplerParameterf(sampler, (TextureParameterName)pname, param);
        }

        /// <summary>
        /// Set sampler parameters.
        /// <para>
        /// glSamplerParameter assigns the value or values in params to the sampler parameter specified as
        /// pname. sampler specifies the sampler object to be modified, and must be the name of a sampler object
        /// previously returned from a call to glGenSamplers. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="sampler">
        /// Specifies the sampler object whose parameter to modify.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a sampler parameter. pname can be one of the following:
        /// GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, GL_TEXTURE_WRAP_R, GL_TEXTURE_MIN_FILTER,
        /// GL_TEXTURE_MAG_FILTER, GL_TEXTURE_BORDER_COLOR, GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD,
        /// GL_TEXTURE_LOD_BIAS GL_TEXTURE_COMPARE_MODE, or GL_TEXTURE_COMPARE_FUNC.
        /// </param>
        /// <param name="param">
        /// For the scalar commands, specifies the value of pname.
        /// </param>
        [Obsolete("SamplerParameteri(UInt32, Int32, Int32) is deprecated, please use SamplerParameteri(UInt32, TextureParameterName, Int32) instead.")]
        public static void SamplerParameteri(UInt32 sampler, Int32 pname, Int32 param)
        {
            Delegates.glSamplerParameteri(sampler, (TextureParameterName)pname, param);
        }

        /// <summary>
        /// Set sampler parameters.
        /// <para>
        /// glSamplerParameter assigns the value or values in params to the sampler parameter specified as
        /// pname. sampler specifies the sampler object to be modified, and must be the name of a sampler object
        /// previously returned from a call to glGenSamplers. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="sampler">
        /// Specifies the sampler object whose parameter to modify.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a sampler parameter. pname can be one of the following:
        /// GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, GL_TEXTURE_WRAP_R, GL_TEXTURE_MIN_FILTER,
        /// GL_TEXTURE_MAG_FILTER, GL_TEXTURE_BORDER_COLOR, GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD,
        /// GL_TEXTURE_LOD_BIAS GL_TEXTURE_COMPARE_MODE, or GL_TEXTURE_COMPARE_FUNC.
        /// </param>
        /// <param name="params">
        /// For the vector commands (glSamplerParameter*v), specifies a pointer to an array where the value or
        /// values of pname are stored.
        /// </param>
        [Obsolete("SamplerParameterfv(UInt32, Int32, Single[]) is deprecated, please use SamplerParameterfv(UInt32, TextureParameterName, Single[]) instead.")]
        public static void SamplerParameterfv(UInt32 sampler, Int32 pname, Single[] @params)
        {
            Delegates.glSamplerParameterfv(sampler, (TextureParameterName)pname, @params);
        }

        /// <summary>
        /// Set sampler parameters.
        /// <para>
        /// glSamplerParameter assigns the value or values in params to the sampler parameter specified as
        /// pname. sampler specifies the sampler object to be modified, and must be the name of a sampler object
        /// previously returned from a call to glGenSamplers. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="sampler">
        /// Specifies the sampler object whose parameter to modify.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a sampler parameter. pname can be one of the following:
        /// GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, GL_TEXTURE_WRAP_R, GL_TEXTURE_MIN_FILTER,
        /// GL_TEXTURE_MAG_FILTER, GL_TEXTURE_BORDER_COLOR, GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD,
        /// GL_TEXTURE_LOD_BIAS GL_TEXTURE_COMPARE_MODE, or GL_TEXTURE_COMPARE_FUNC.
        /// </param>
        /// <param name="params">
        /// For the vector commands (glSamplerParameter*v), specifies a pointer to an array where the value or
        /// values of pname are stored.
        /// </param>
        [Obsolete("SamplerParameteriv(UInt32, Int32, Int32[]) is deprecated, please use SamplerParameteriv(UInt32, TextureParameterName, Int32[]) instead.")]
        public static void SamplerParameteriv(UInt32 sampler, Int32 pname, Int32[] @params)
        {
            Delegates.glSamplerParameteriv(sampler, (TextureParameterName)pname, @params);
        }

        /// <summary>
        /// Return sampler parameter values.
        /// <para>
        /// glGetSamplerParameter returns in params the value or values of the sampler parameter specified as
        /// pname. sampler defines the target sampler, and must be the name of an existing sampler object,
        /// returned from a previous call to glGenSamplers. pname accepts the same symbols as
        /// glSamplerParameter, with the same interpretations:.
        /// </para>
        /// </summary>
        /// <param name="sampler">
        /// Specifies name of the sampler object from which to retrieve parameters.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a sampler parameter. GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MIN_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T,
        /// GL_TEXTURE_WRAP_R, GL_TEXTURE_BORDER_COLOR, GL_TEXTURE_COMPARE_MODE, and GL_TEXTURE_COMPARE_FUNC are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the sampler parameters.
        /// </param>
        [Obsolete("GetSamplerParameterfv(UInt32, Int32, Int32[]) is deprecated, please use GetSamplerParameterfv(UInt32, TextureParameterName, Single[]) instead.")]
        public static void GetSamplerParameterfv(UInt32 sampler, Int32 pname, [OutAttribute] Single[] @params)
        {
            Delegates.glGetSamplerParameterfv(sampler, (TextureParameterName)pname, @params);
        }

        /// <summary>
        /// Return sampler parameter values.
        /// <para>
        /// glGetSamplerParameter returns in params the value or values of the sampler parameter specified as
        /// pname. sampler defines the target sampler, and must be the name of an existing sampler object,
        /// returned from a previous call to glGenSamplers. pname accepts the same symbols as
        /// glSamplerParameter, with the same interpretations:.
        /// </para>
        /// </summary>
        /// <param name="sampler">
        /// Specifies name of the sampler object from which to retrieve parameters.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a sampler parameter. GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MIN_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T,
        /// GL_TEXTURE_WRAP_R, GL_TEXTURE_BORDER_COLOR, GL_TEXTURE_COMPARE_MODE, and GL_TEXTURE_COMPARE_FUNC are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the sampler parameters.
        /// </param>
        [Obsolete("GetSamplerParameteriv(UInt32, Int32, Int32[]) is deprecated, please use GetSamplerParameteriv(UInt32, TextureParameterName, Int32[]) instead.")]
        public static void GetSamplerParameteriv(UInt32 sampler, Int32 pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetSamplerParameteriv(sampler, (TextureParameterName)pname, @params);
        }

        /// <summary>
        /// Delimit the boundaries of a query object on an indexed target.
        /// <para>
        /// glBeginQueryIndexed and glEndQueryIndexed delimit the boundaries of a query object. query must be a
        /// name previously returned from a call to glGenQueries. If a query object with name id does not yet
        /// exist it is created with the type determined by target. target must be one of GL_SAMPLES_PASSED,
        /// GL_ANY_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED, GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, or
        /// GL_TIME_ELAPSED. The behavior of the query object depends on its type and is as follows.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target type of query object established between glBeginQueryIndexed and the subsequent
        /// glEndQueryIndexed. The symbolic constant must be one of GL_SAMPLES_PASSED, GL_ANY_SAMPLES_PASSED,
        /// GL_PRIMITIVES_GENERATED, GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, or GL_TIME_ELAPSED.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the query target upon which to begin the query.
        /// </param>
        /// <param name="id">
        /// Specifies the name of a query object.
        /// </param>
        [Obsolete("BeginQueryIndexed(UInt32, UInt32, UInt32) is deprecated, please use BeginQueryIndexed(UInt32, UInt32, UInt32) instead.")]
        public static void BeginQueryIndexed(UInt32 target, UInt32 index, UInt32 id)
        {
            Delegates.glBeginQueryIndexed((QueryTarget)target, index, id);
        }
    }
}
