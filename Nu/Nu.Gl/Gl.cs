using System;
using System.Runtime.InteropServices;

namespace Nu
{
    // NOTE: Bindings and other code in this project copied from -
    // https://github.com/giawa/opengl4csharp/blob/5d96cfc0fcf91a9fb33191158987508015a466ce/OpenGL/Core/
    // - which was -
    // Automatically generated from GlCore.cs using BuildGl
    partial class Gl
    {
        /// <summary>
        /// Set the active program object for a program pipeline object.
        /// <para>
        /// glActiveShaderProgram sets the linked program named by program to be the active program for the
        /// program pipeline object pipeline. The active program in the active program pipeline object is the
        /// target of calls to glUniform when no program has been made current through a call to glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="pipeline">
        /// Specifies the program pipeline object to set the active program object for.
        /// </param>
        /// <param name="program">
        /// Specifies the program object to set as the active program pipeline object pipeline.
        /// </param>
        public static void ActiveShaderProgram(UInt32 pipeline, UInt32 program)
        {
            Delegates.glActiveShaderProgram(pipeline, program);
        }

        /// <summary>
        /// Select active texture unit.
        /// <para>
        /// glActiveTexture selects which texture unit subsequent texture state calls will affect. The number of
        /// texture units an implementation supports is implementation dependent, but must be at least 80.
        /// </para>
        /// </summary>
        /// <param name="textureUnitOffset">
        /// The offset from TextureUnit.Texture0.
        /// </param>
        public static void ActiveTexture(int textureUnitOffset)
        {
            Delegates.glActiveTexture(TextureUnit0 + textureUnitOffset);
        }
        /// <summary>
        /// Attaches a shader object to a program object.
        /// <para>
        /// In order to create a complete shader program, there must be a way to specify the list of things that
        /// will be linked together. Program objects provide this mechanism. Shaders that are to be linked
        /// together in a program object must first be attached to that program object. glAttachShader attaches
        /// the shader object specified by shader to the program object specified by program. This indicates
        /// that shader will be included in link operations that will be performed on program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to which a shader object will be attached.
        /// </param>
        /// <param name="shader">
        /// Specifies the shader object that is to be attached.
        /// </param>
        public static void AttachShader(UInt32 program, UInt32 shader)
        {
            Delegates.glAttachShader(program, shader);
        }

        /// <summary>
        /// Start conditional rendering.
        /// <para>
        /// Conditional rendering is started using glBeginConditionalRender and ended using
        /// glEndConditionalRender. During conditional rendering, all vertex array commands, as well as glClear
        /// and glClearBuffer have no effect if the (GL_SAMPLES_PASSED) result of the query object id is zero,
        /// or if the (GL_ANY_SAMPLES_PASSED) result is GL_FALSE. The results of commands setting the current
        /// vertex state, such as glVertexAttrib are undefined. If the (GL_SAMPLES_PASSED) result is non-zero or
        /// if the (GL_ANY_SAMPLES_PASSED) result is GL_TRUE, such commands are not discarded. The id parameter
        /// to glBeginConditionalRender must be the name of a query object previously returned from a call to
        /// glGenQueries. mode specifies how the results of the query object are to be interpreted. If mode is
        /// GL_QUERY_WAIT, the GL waits for the results of the query to be available and then uses the results
        /// to determine if subsequent rendering commands are discarded. If mode is GL_QUERY_NO_WAIT, the GL may
        /// choose to unconditionally execute the subsequent rendering commands without waiting for the query to
        /// complete.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specifies the name of an occlusion query object whose results are used to determine if the rendering
        /// commands are discarded.
        /// </param>
        /// <param name="mode">
        /// Specifies how glBeginConditionalRender interprets the results of the occlusion query.
        /// </param>
        public static void BeginConditionalRender(UInt32 id, ConditionalRenderType mode)
        {
            Delegates.glBeginConditionalRender(id, mode);
        }

        /// <summary>
        /// Start conditional rendering.
        /// <para>
        /// Conditional rendering is started using glBeginConditionalRender and ended using
        /// glEndConditionalRender. During conditional rendering, all vertex array commands, as well as glClear
        /// and glClearBuffer have no effect if the (GL_SAMPLES_PASSED) result of the query object id is zero,
        /// or if the (GL_ANY_SAMPLES_PASSED) result is GL_FALSE. The results of commands setting the current
        /// vertex state, such as glVertexAttrib are undefined. If the (GL_SAMPLES_PASSED) result is non-zero or
        /// if the (GL_ANY_SAMPLES_PASSED) result is GL_TRUE, such commands are not discarded. The id parameter
        /// to glBeginConditionalRender must be the name of a query object previously returned from a call to
        /// glGenQueries. mode specifies how the results of the query object are to be interpreted. If mode is
        /// GL_QUERY_WAIT, the GL waits for the results of the query to be available and then uses the results
        /// to determine if subsequent rendering commands are discarded. If mode is GL_QUERY_NO_WAIT, the GL may
        /// choose to unconditionally execute the subsequent rendering commands without waiting for the query to
        /// complete.
        /// </para>
        /// </summary>
        public static void EndConditionalRender()
        {
            Delegates.glEndConditionalRender();
        }

        /// <summary>
        /// Delimit the boundaries of a query object.
        /// <para>
        /// glBeginQuery and glEndQuery delimit the boundaries of a query object. query must be a name
        /// previously returned from a call to glGenQueries. If a query object with name id does not yet exist
        /// it is created with the type determined by target. target must be one of GL_SAMPLES_PASSED,
        /// GL_ANY_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED, GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, or
        /// GL_TIME_ELAPSED. The behavior of the query object depends on its type and is as follows.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target type of query object established between glBeginQuery and the subsequent
        /// glEndQuery. The symbolic constant must be one of GL_SAMPLES_PASSED, GL_ANY_SAMPLES_PASSED,
        /// GL_ANY_SAMPLES_PASSED_CONSERVATIVE, GL_PRIMITIVES_GENERATED,
        /// GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, or GL_TIME_ELAPSED.
        /// </param>
        /// <param name="id">
        /// Specifies the name of a query object.
        /// </param>
        public static void BeginQuery(QueryTarget target, UInt32 id)
        {
            Delegates.glBeginQuery(target, id);
        }

        /// <summary>
        /// Delimit the boundaries of a query object.
        /// <para>
        /// glBeginQuery and glEndQuery delimit the boundaries of a query object. query must be a name
        /// previously returned from a call to glGenQueries. If a query object with name id does not yet exist
        /// it is created with the type determined by target. target must be one of GL_SAMPLES_PASSED,
        /// GL_ANY_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED, GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, or
        /// GL_TIME_ELAPSED. The behavior of the query object depends on its type and is as follows.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target type of query object to be concluded. The symbolic constant must be one of
        /// GL_SAMPLES_PASSED, GL_ANY_SAMPLES_PASSED, GL_ANY_SAMPLES_PASSED_CONSERVATIVE,
        /// GL_PRIMITIVES_GENERATED, GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, or GL_TIME_ELAPSED.
        /// </param>
        public static void EndQuery(QueryTarget target)
        {
            Delegates.glEndQuery(target);
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
        public static void BeginQueryIndexed(QueryTarget target, UInt32 index, UInt32 id)
        {
            Delegates.glBeginQueryIndexed(target, index, id);
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
        /// Specifies the target type of query object to be concluded. The symbolic constant must be one of
        /// GL_SAMPLES_PASSED, GL_ANY_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED,
        /// GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, or GL_TIME_ELAPSED.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the query target upon which to end the query.
        /// </param>
        public static void EndQueryIndexed(QueryTarget target, UInt32 index)
        {
            Delegates.glEndQueryIndexed(target, index);
        }

        /// <summary>
        /// Start transform feedback operation.
        /// <para>
        /// Transform feedback mode captures the values of varying variables written by the vertex shader (or,
        /// if active, the geometry shader). Transform feedback is said to be active after a call to
        /// glBeginTransformFeedback until a subsequent call to glEndTransformFeedback. Transform feedback
        /// commands must be paired.
        /// </para>
        /// </summary>
        /// <param name="primitiveMode">
        /// Specify the output type of the primitives that will be recorded into the buffer objects that are
        /// bound for transform feedback.
        /// </param>
        public static void BeginTransformFeedback(BeginFeedbackMode primitiveMode)
        {
            Delegates.glBeginTransformFeedback(primitiveMode);
        }

        /// <summary>
        /// Start transform feedback operation.
        /// <para>
        /// Transform feedback mode captures the values of varying variables written by the vertex shader (or,
        /// if active, the geometry shader). Transform feedback is said to be active after a call to
        /// glBeginTransformFeedback until a subsequent call to glEndTransformFeedback. Transform feedback
        /// commands must be paired.
        /// </para>
        /// </summary>
        public static void EndTransformFeedback()
        {
            Delegates.glEndTransformFeedback();
        }

        /// <summary>
        /// Associates a generic vertex attribute index with a named attribute variable.
        /// <para>
        /// glBindAttribLocation is used to associate a user-defined attribute variable in the program object
        /// specified by program with a generic vertex attribute index. The name of the user-defined attribute
        /// variable is passed as a null terminated string in name. The generic vertex attribute index to be
        /// bound to this variable is specified by index. When program is made part of current state, values
        /// provided via the generic vertex attribute index will modify the value of the user-defined attribute
        /// variable specified by name.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program object in which the association is to be made.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be bound.
        /// </param>
        /// <param name="name">
        /// Specifies a null terminated string containing the name of the vertex shader attribute variable to
        /// which index is to be bound.
        /// </param>
        public static void BindAttribLocation(UInt32 program, UInt32 index, String name)
        {
            Delegates.glBindAttribLocation(program, index, name);
        }

        /// <summary>
        /// Associates a generic vertex attribute index with a named attribute variable.
        /// <para>
        /// glBindAttribLocation is used to associate a user-defined attribute variable in the program object
        /// specified by program with a generic vertex attribute index. The name of the user-defined attribute
        /// variable is passed as a null terminated string in name. The generic vertex attribute index to be
        /// bound to this variable is specified by index. When program is made part of current state, values
        /// provided via the generic vertex attribute index will modify the value of the user-defined attribute
        /// variable specified by name.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program object in which the association is to be made.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be bound.
        /// </param>
        /// <param name="name">
        /// Specifies a null terminated string containing the name of the vertex shader attribute variable to
        /// which index is to be bound.
        /// </param>
        public static void BindAttribLocation(UInt32 program, Int32 index, String name)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glBindAttribLocation(program, (UInt32)index, name);
        }

        /// <summary>
        /// Bind a named buffer object.
        /// <para>
        /// glBindBuffer binds a buffer object to the specified buffer binding point. Calling glBindBuffer with
        /// target set to one of the accepted symbolic constants and buffer set to the name of a buffer object
        /// binds that buffer object name to the target. If no buffer object with name buffer exists, one is
        /// created with that name. When a buffer object is bound to a target, the previous binding for that
        /// target is automatically broken.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound, which must be one of the buffer binding
        /// targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER Vertex attributes
        /// GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy source
        /// GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute dispatch
        /// commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex array
        /// indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="buffer">
        /// Specifies the name of a buffer object.
        /// </param>
        public static void BindBuffer(BufferTarget target, UInt32 buffer)
        {
            Delegates.glBindBuffer(target, buffer);
        }

        /// <summary>
        /// Bind a buffer object to an indexed buffer target.
        /// <para>
        /// glBindBufferBase binds the buffer object buffer to the binding point at index index of the array of
        /// targets specified by target. Each target represents an indexed array of buffer binding points, as
        /// well as a single general binding point that can be used by other buffer manipulation functions such
        /// as glBindBuffer or glMapBuffer. In addition to binding buffer to the indexed buffer binding target,
        /// glBindBufferBase also binds buffer to the generic buffer binding point specified by target.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specify the target of the bind operation. target must be one of GL_ATOMIC_COUNTER_BUFFER,
        /// GL_TRANSFORM_FEEDBACK_BUFFER, GL_UNIFORM_BUFFER or GL_SHADER_STORAGE_BUFFER.
        /// </param>
        /// <param name="index">
        /// Specify the index of the binding point within the array specified by target.
        /// </param>
        /// <param name="buffer">
        /// The name of a buffer object to bind to the specified binding point.
        /// </param>
        public static void BindBufferBase(BufferTarget target, UInt32 index, UInt32 buffer)
        {
            Delegates.glBindBufferBase(target, index, buffer);
        }

        /// <summary>
        /// Bind a range within a buffer object to an indexed buffer target.
        /// <para>
        /// glBindBufferRange binds a range the buffer object buffer represented by offset and size to the
        /// binding point at index index of the array of targets specified by target. Each target represents an
        /// indexed array of buffer binding points, as well as a single general binding point that can be used
        /// by other buffer manipulation functions such as glBindBuffer or glMapBuffer. In addition to binding a
        /// range of buffer to the indexed buffer binding target, glBindBufferRange also binds the range to the
        /// generic buffer binding point specified by target.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specify the target of the bind operation. target must be one of GL_ATOMIC_COUNTER_BUFFER,
        /// GL_TRANSFORM_FEEDBACK_BUFFER, GL_UNIFORM_BUFFER, or GL_SHADER_STORAGE_BUFFER.
        /// </param>
        /// <param name="index">
        /// Specify the index of the binding point within the array specified by target.
        /// </param>
        /// <param name="buffer">
        /// The name of a buffer object to bind to the specified binding point.
        /// </param>
        /// <param name="offset">
        /// The starting offset in basic machine units into the buffer object buffer.
        /// </param>
        /// <param name="size">
        /// The amount of data in machine units that can be read from the buffer object while used as an indexed
        /// target.
        /// </param>
        public static void BindBufferRange(BufferTarget target, UInt32 index, UInt32 buffer, IntPtr offset, IntPtr size)
        {
            Delegates.glBindBufferRange(target, index, buffer, offset, size);
        }

        /// <summary>
        /// Bind one or more buffer objects to a sequence of indexed buffer targets.
        /// <para>
        /// glBindBuffersBase binds a set of count buffer objects whose names are given in the array buffers to
        /// the count consecutive binding points starting from index index of the array of targets specified by
        /// target. If buffers is NULL then glBindBuffersBase unbinds any buffers that are currently bound to
        /// the referenced binding points. Assuming no errors are generated, it is equivalent to the following
        /// pseudo-code, which calls glBindBufferBase, with the exception that the non-indexed target is not
        /// changed by glBindBuffersBase:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specify the target of the bind operation. target must be one of GL_ATOMIC_COUNTER_BUFFER,
        /// GL_TRANSFORM_FEEDBACK_BUFFER, GL_UNIFORM_BUFFER or GL_SHADER_STORAGE_BUFFER.
        /// </param>
        /// <param name="first">
        /// Specify the index of the first binding point within the array specified by target.
        /// </param>
        /// <param name="count">
        /// Specify the number of contiguous binding points to which to bind buffers.
        /// </param>
        /// <param name="buffers">
        /// A pointer to an array of names of buffer objects to bind to the targets on the specified binding
        /// point, or NULL.
        /// </param>
        public static void BindBuffersBase(BufferTarget target, UInt32 first, Int32 count, UInt32[] buffers)
        {
            Delegates.glBindBuffersBase(target, first, count, buffers);
        }

        /// <summary>
        /// Bind ranges of one or more buffer objects to a sequence of indexed buffer targets.
        /// <para>
        /// glBindBuffersRange binds a set of count ranges from buffer objects whose names are given in the
        /// array buffers to the count consecutive binding points starting from index index of the array of
        /// targets specified by target. offsets specifies the address of an array containing count starting
        /// offsets within the buffers, and sizes specifies the address of an array of count sizes of the
        /// ranges. If buffers is NULL then offsets and sizes are ignored and glBindBuffersRange unbinds any
        /// buffers that are currently bound to the referenced binding points. Assuming no errors are generated,
        /// it is equivalent to the following pseudo-code, which calls glBindBufferRange, with the exception
        /// that the non-indexed target is not changed by glBindBuffersRange:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specify the target of the bind operation. target must be one of GL_ATOMIC_COUNTER_BUFFER,
        /// GL_TRANSFORM_FEEDBACK_BUFFER, GL_UNIFORM_BUFFER or GL_SHADER_STORAGE_BUFFER.
        /// </param>
        /// <param name="first">
        /// Specify the index of the first binding point within the array specified by target.
        /// </param>
        /// <param name="count">
        /// Specify the number of contiguous binding points to which to bind buffers.
        /// </param>
        /// <param name="buffers">
        /// A pointer to an array of names of buffer objects to bind to the targets on the specified binding
        /// point, or NULL.
        /// </param>
        /// <param name="offsets">
        /// A pointer to an array of offsets into the corresponding buffer in buffers to bind, or NULL if
        /// buffers is NULL.
        /// </param>
        /// <param name="sizes">
        /// A pointer to an array of sizes of the corresponding buffer in buffers to bind, or NULL if buffers is
        /// NULL.
        /// </param>
        public static void BindBuffersRange(BufferTarget target, UInt32 first, Int32 count, UInt32[] buffers, IntPtr[] offsets, IntPtr[] sizes)
        {
            Delegates.glBindBuffersRange(target, first, count, buffers, offsets, sizes);
        }

        /// <summary>
        /// Bind a user-defined varying out variable to a fragment shader color number.
        /// <para>
        /// glBindFragDataLocation explicitly specifies the binding of the user-defined varying out variable
        /// name to fragment shader color number colorNumber for program program. If name was bound previously,
        /// its assigned binding is replaced with colorNumber. name must be a null-terminated string.
        /// colorNumber must be less than GL_MAX_DRAW_BUFFERS.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of the program containing varying out variable whose binding to modify
        /// </param>
        /// <param name="colorNumber">
        /// The color number to bind the user-defined varying out variable to
        /// </param>
        /// <param name="name">
        /// The name of the user-defined varying out variable whose binding to modify
        /// </param>
        public static void BindFragDataLocation(UInt32 program, UInt32 colorNumber, String name)
        {
            Delegates.glBindFragDataLocation(program, colorNumber, name);
        }

        /// <summary>
        /// Bind a user-defined varying out variable to a fragment shader color number and index.
        /// <para>
        /// glBindFragDataLocationIndexed specifies that the varying out variable name in program should be
        /// bound to fragment color colorNumber when the program is next linked. index may be zero or one to
        /// specify that the color be used as either the first or second color input to the blend equation,
        /// respectively.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of the program containing varying out variable whose binding to modify
        /// </param>
        /// <param name="colorNumber">
        /// The color number to bind the user-defined varying out variable to
        /// </param>
        /// <param name="index">
        /// The index of the color input to bind the user-defined varying out variable to
        /// </param>
        /// <param name="name">
        /// The name of the user-defined varying out variable whose binding to modify
        /// </param>
        public static void BindFragDataLocationIndexed(UInt32 program, UInt32 colorNumber, UInt32 index, String name)
        {
            Delegates.glBindFragDataLocationIndexed(program, colorNumber, index, name);
        }

        /// <summary>
        /// Bind a framebuffer to a framebuffer target.
        /// <para>
        /// glBindFramebuffer binds the framebuffer object with name framebuffer to the framebuffer target
        /// specified by target. target must be either GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER or
        /// GL_FRAMEBUFFER. If a framebuffer object is bound to GL_DRAW_FRAMEBUFFER or GL_READ_FRAMEBUFFER, it
        /// becomes the target for rendering or readback operations, respectively, until it is deleted or
        /// another framebuffer is bound to the corresponding bind point. Calling glBindFramebuffer with target
        /// set to GL_FRAMEBUFFER binds framebuffer to both the read and draw framebuffer targets. framebuffer
        /// is the name of a framebuffer object previously returned from a call to glGenFramebuffers, or zero to
        /// break the existing binding of a framebuffer object to target.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the framebuffer target of the binding operation.
        /// </param>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object to bind.
        /// </param>
        public static void BindFramebuffer(FramebufferTarget target, UInt32 framebuffer)
        {
            Delegates.glBindFramebuffer(target, framebuffer);
        }

        /// <summary>
        /// Bind a level of a texture to an image unit.
        /// <para>
        /// glBindImageTexture binds a single level of a texture to an image unit for the purpose of reading and
        /// writing it from shaders. unit specifies the zero-based index of the image unit to which to bind the
        /// texture level. texture specifies the name of an existing texture object to bind to the image unit.
        /// If texture is zero, then any existing binding to the image unit is broken. level specifies the level
        /// of the texture to bind to the image unit.
        /// </para>
        /// </summary>
        /// <param name="unit">
        /// Specifies the index of the image unit to which to bind the texture
        /// </param>
        /// <param name="texture">
        /// Specifies the name of the texture to bind to the image unit.
        /// </param>
        /// <param name="level">
        /// Specifies the level of the texture that is to be bound.
        /// </param>
        /// <param name="layered">
        /// Specifies whether a layered texture binding is to be established.
        /// </param>
        /// <param name="layer">
        /// If layered is GL_FALSE, specifies the layer of texture to be bound to the image unit. Ignored
        /// otherwise.
        /// </param>
        /// <param name="access">
        /// Specifies a token indicating the type of access that will be performed on the image.
        /// </param>
        /// <param name="format">
        /// Specifies the format that the elements of the image will be treated as for the purposes of formatted
        /// stores.
        /// </param>
        public static void BindImageTexture(UInt32 unit, UInt32 texture, Int32 level, Boolean layered, Int32 layer, BufferAccess access, PixelInternalFormat format)
        {
            Delegates.glBindImageTexture(unit, texture, level, layered, layer, access, format);
        }

        /// <summary>
        /// Bind one or more named texture images to a sequence of consecutive image units.
        /// <para>
        /// glBindImageTextures binds images from an array of existing texture objects to a specified number of
        /// consecutive image units. count specifies the number of texture objects whose names are stored in the
        /// array textures. That number of texture names are read from the array and bound to the count
        /// consecutive texture units starting from first. If the name zero appears in the textures array, any
        /// existing binding to the image unit is reset. Any non-zero entry in textures must be the name of an
        /// existing texture object. When a non-zero entry in textures is present, the image at level zero is
        /// bound, the binding is considered layered, with the first layer set to zero, and the image is bound
        /// for read-write access. The image unit format parameter is taken from the internal format of the
        /// image at level zero of the texture object. For cube map textures, the internal format of the
        /// positive X image of level zero is used. If textures is NULL then it is as if an appropriately sized
        /// array containing only zeros had been specified.
        /// </para>
        /// </summary>
        /// <param name="first">
        /// Specifies the first image unit to which a texture is to be bound.
        /// </param>
        /// <param name="count">
        /// Specifies the number of textures to bind.
        /// </param>
        /// <param name="textures">
        /// Specifies the address of an array of names of existing texture objects.
        /// </param>
        public static void BindImageTextures(UInt32 first, Int32 count, UInt32[] textures)
        {
            Delegates.glBindImageTextures(first, count, textures);
        }

        /// <summary>
        /// Bind a program pipeline to the current context.
        /// <para>
        /// glBindProgramPipeline binds a program pipeline object to the current context. pipeline must be a
        /// name previously returned from a call to glGenProgramPipelines. If no program pipeline exists with
        /// name pipeline then a new pipeline object is created with that name and initialized to the default
        /// state vector.
        /// </para>
        /// </summary>
        /// <param name="pipeline">
        /// Specifies the name of the pipeline object to bind to the context.
        /// </param>
        public static void BindProgramPipeline(UInt32 pipeline)
        {
            Delegates.glBindProgramPipeline(pipeline);
        }

        /// <summary>
        /// Bind a renderbuffer to a renderbuffer target.
        /// <para>
        /// glBindRenderbuffer binds the renderbuffer object with name renderbuffer to the renderbuffer target
        /// specified by target. target must be GL_RENDERBUFFER. renderbuffer is the name of a renderbuffer
        /// object previously returned from a call to glGenRenderbuffers, or zero to break the existing binding
        /// of a renderbuffer object to target.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the renderbuffer target of the binding operation. target must be GL_RENDERBUFFER.
        /// </param>
        /// <param name="renderbuffer">
        /// Specifies the name of the renderbuffer object to bind.
        /// </param>
        public static void BindRenderbuffer(RenderbufferTarget target, UInt32 renderbuffer)
        {
            Delegates.glBindRenderbuffer(target, renderbuffer);
        }

        /// <summary>
        /// Bind a named sampler to a texturing target.
        /// <para>
        /// glBindSampler binds sampler to the texture unit at index unit. sampler must be zero or the name of a
        /// sampler object previously returned from a call to glGenSamplers. unit must be less than the value of
        /// GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS.
        /// </para>
        /// </summary>
        /// <param name="unit">
        /// Specifies the index of the texture unit to which the sampler is bound.
        /// </param>
        /// <param name="sampler">
        /// Specifies the name of a sampler.
        /// </param>
        public static void BindSampler(UInt32 unit, UInt32 sampler)
        {
            Delegates.glBindSampler(unit, sampler);
        }

        /// <summary>
        /// Bind one or more named sampler objects to a sequence of consecutive sampler units.
        /// <para>
        /// glBindSamplers binds samplers from an array of existing sampler objects to a specified number of
        /// consecutive sampler units. count specifies the number of sampler objects whose names are stored in
        /// the array samplers. That number of sampler names is read from the array and bound to the count
        /// consecutive sampler units starting from first.
        /// </para>
        /// </summary>
        /// <param name="first">
        /// Specifies the first sampler unit to which a sampler object is to be bound.
        /// </param>
        /// <param name="count">
        /// Specifies the number of samplers to bind.
        /// </param>
        /// <param name="samplers">
        /// Specifies the address of an array of names of existing sampler objects.
        /// </param>
        public static void BindSamplers(UInt32 first, Int32 count, UInt32[] samplers)
        {
            Delegates.glBindSamplers(first, count, samplers);
        }

        /// <summary>
        /// Bind a named texture to a texturing target.
        /// <para>
        /// glBindTexture lets you create or use a named texture. Calling glBindTexture with target set to
        /// GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
        /// GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_BUFFER,
        /// GL_TEXTURE_2D_MULTISAMPLE or GL_TEXTURE_2D_MULTISAMPLE_ARRAY and texture set to the name of the new
        /// texture binds the texture name to the target. When a texture is bound to a target, the previous
        /// binding for that target is automatically broken.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound. Must be one of GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_BUFFER, GL_TEXTURE_2D_MULTISAMPLE or
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of a texture.
        /// </param>
        public static void BindTexture(TextureTarget target, UInt32 texture)
        {
            Delegates.glBindTexture(target, texture);
        }

        /// <summary>
        /// Bind one or more named textures to a sequence of consecutive texture units.
        /// <para>
        /// glBindTextures binds an array of existing texture objects to a specified number of consecutive
        /// texture units. count specifies the number of texture objects whose names are stored in the array
        /// textures. That number of texture names are read from the array and bound to the count consecutive
        /// texture units starting from first. The target, or type of texture is deduced from the texture object
        /// and each texture is bound to the corresponding target of the texture unit. If the name zero appears
        /// in the textures array, any existing binding to any target of the texture unit is reset and the
        /// default texture for that target is bound in its place. Any non-zero entry in textures must be the
        /// name of an existing texture object. If textures is NULL then it is as if an appropriately sized
        /// array containing only zeros had been specified.
        /// </para>
        /// </summary>
        /// <param name="first">
        /// Specifies the first texture unit to which a texture is to be bound.
        /// </param>
        /// <param name="count">
        /// Specifies the number of textures to bind.
        /// </param>
        /// <param name="textures">
        /// Specifies the address of an array of names of existing texture objects.
        /// </param>
        public static void BindTextures(UInt32 first, Int32 count, UInt32[] textures)
        {
            Delegates.glBindTextures(first, count, textures);
        }

        /// <summary>
        /// Bind an existing texture object to the specified texture unit.
        /// <para>
        /// glBindTextureUnit binds an existing texture object to the texture unit numbered unit.
        /// </para>
        /// </summary>
        /// <param name="unit">
        /// Specifies the texture unit, to which the texture object should be bound to.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of a texture.
        /// </param>
        public static void BindTextureUnit(UInt32 unit, UInt32 texture)
        {
            Delegates.glBindTextureUnit(unit, texture);
        }

        /// <summary>
        /// Bind a transform feedback object.
        /// <para>
        /// glBindTransformFeedback binds the transform feedback object with name id to the current GL state. id
        /// must be a name previously returned from a call to glGenTransformFeedbacks. If id has not previously
        /// been bound, a new transform feedback object with name id and initialized with with the default
        /// transform state vector is created.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which to bind the transform feedback object id. target must be
        /// GL_TRANSFORM_FEEDBACK.
        /// </param>
        /// <param name="id">
        /// Specifies the name of a transform feedback object reserved by glGenTransformFeedbacks.
        /// </param>
        public static void BindTransformFeedback(NvTransformFeedback2 target, UInt32 id)
        {
            Delegates.glBindTransformFeedback(target, id);
        }

        /// <summary>
        /// Bind a vertex array object.
        /// <para>
        /// glBindVertexArray binds the vertex array object with name array. array is the name of a vertex array
        /// object previously returned from a call to glGenVertexArrays, or zero to break the existing vertex
        /// array object binding.
        /// </para>
        /// </summary>
        /// <param name="array">
        /// Specifies the name of the vertex array to bind.
        /// </param>
        public static void BindVertexArray(UInt32 array)
        {
            Delegates.glBindVertexArray(array);
        }

        /// <summary>
        /// Bind a buffer to a vertex buffer bind point.
        /// <para>
        /// glBindVertexBuffer and glVertexArrayVertexBuffer bind the buffer named buffer to the vertex buffer
        /// binding point whose index is given by bindingindex. glBindVertexBuffer modifies the binding of the
        /// currently bound vertex array object, whereas glVertexArrayVertexBuffer allows the caller to specify
        /// ID of the vertex array object with an argument named vaobj, for which the binding should be
        /// modified. offset and stride specify the offset of the first element within the buffer and the
        /// distance between elements within the buffer, respectively, and are both measured in basic machine
        /// units. bindingindex must be less than the value of GL_MAX_VERTEX_ATTRIB_BINDINGS. offset and stride
        /// must be greater than or equal to zero. If buffer is zero, then any buffer currently bound to the
        /// specified binding point is unbound.
        /// </para>
        /// </summary>
        /// <param name="bindingindex">
        /// The index of the vertex buffer binding point to which to bind the buffer.
        /// </param>
        /// <param name="buffer">
        /// The name of a buffer to bind to the vertex buffer binding point.
        /// </param>
        /// <param name="offset">
        /// The offset of the first element of the buffer.
        /// </param>
        /// <param name="stride">
        /// The distance between elements within the buffer.
        /// </param>
        public static void BindVertexBuffer(UInt32 bindingindex, UInt32 buffer, IntPtr offset, IntPtr stride)
        {
            Delegates.glBindVertexBuffer(bindingindex, buffer, offset, stride);
        }

        /// <summary>
        /// Bind a buffer to a vertex buffer bind point.
        /// <para>
        /// glBindVertexBuffer and glVertexArrayVertexBuffer bind the buffer named buffer to the vertex buffer
        /// binding point whose index is given by bindingindex. glBindVertexBuffer modifies the binding of the
        /// currently bound vertex array object, whereas glVertexArrayVertexBuffer allows the caller to specify
        /// ID of the vertex array object with an argument named vaobj, for which the binding should be
        /// modified. offset and stride specify the offset of the first element within the buffer and the
        /// distance between elements within the buffer, respectively, and are both measured in basic machine
        /// units. bindingindex must be less than the value of GL_MAX_VERTEX_ATTRIB_BINDINGS. offset and stride
        /// must be greater than or equal to zero. If buffer is zero, then any buffer currently bound to the
        /// specified binding point is unbound.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object to be used by glVertexArrayVertexBuffer function.
        /// </param>
        /// <param name="bindingindex">
        /// The index of the vertex buffer binding point to which to bind the buffer.
        /// </param>
        /// <param name="buffer">
        /// The name of a buffer to bind to the vertex buffer binding point.
        /// </param>
        /// <param name="offset">
        /// The offset of the first element of the buffer.
        /// </param>
        /// <param name="stride">
        /// The distance between elements within the buffer.
        /// </param>
        public static void VertexArrayVertexBuffer(UInt32 vaobj, UInt32 bindingindex, UInt32 buffer, IntPtr offset, Int32 stride)
        {
            Delegates.glVertexArrayVertexBuffer(vaobj, bindingindex, buffer, offset, stride);
        }

        /// <summary>
        /// Attach multiple buffer objects to a vertex array object.
        /// <para>
        /// glBindVertexBuffers and glVertexArrayVertexBuffers bind storage from an array of existing buffer
        /// objects to a specified number of consecutive vertex buffer binding points units in a vertex array
        /// object. For glBindVertexBuffers, the vertex array object is the currently bound vertex array object.
        /// For glVertexArrayVertexBuffers, vaobj is the name of the vertex array object.
        /// </para>
        /// </summary>
        /// <param name="first">
        /// Specifies the first vertex buffer binding point to which a buffer object is to be bound.
        /// </param>
        /// <param name="count">
        /// Specifies the number of buffers to bind.
        /// </param>
        /// <param name="buffers">
        /// Specifies the address of an array of names of existing buffer objects.
        /// </param>
        /// <param name="offsets">
        /// Specifies the address of an array of offsets to associate with the binding points.
        /// </param>
        /// <param name="strides">
        /// Specifies the address of an array of strides to associate with the binding points.
        /// </param>
        public static void BindVertexBuffers(UInt32 first, Int32 count, UInt32[] buffers, IntPtr[] offsets, Int32[] strides)
        {
            Delegates.glBindVertexBuffers(first, count, buffers, offsets, strides);
        }

        /// <summary>
        /// Attach multiple buffer objects to a vertex array object.
        /// <para>
        /// glBindVertexBuffers and glVertexArrayVertexBuffers bind storage from an array of existing buffer
        /// objects to a specified number of consecutive vertex buffer binding points units in a vertex array
        /// object. For glBindVertexBuffers, the vertex array object is the currently bound vertex array object.
        /// For glVertexArrayVertexBuffers, vaobj is the name of the vertex array object.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glVertexArrayVertexBuffers.
        /// </param>
        /// <param name="first">
        /// Specifies the first vertex buffer binding point to which a buffer object is to be bound.
        /// </param>
        /// <param name="count">
        /// Specifies the number of buffers to bind.
        /// </param>
        /// <param name="buffers">
        /// Specifies the address of an array of names of existing buffer objects.
        /// </param>
        /// <param name="offsets">
        /// Specifies the address of an array of offsets to associate with the binding points.
        /// </param>
        /// <param name="strides">
        /// Specifies the address of an array of strides to associate with the binding points.
        /// </param>
        public static void VertexArrayVertexBuffers(UInt32 vaobj, UInt32 first, Int32 count, UInt32[] buffers, IntPtr[] offsets, Int32[] strides)
        {
            Delegates.glVertexArrayVertexBuffers(vaobj, first, count, buffers, offsets, strides);
        }

        /// <summary>
        /// Set the blend color.
        /// <para>
        /// The GL_BLEND_COLOR may be used to calculate the source and destination blending factors. The color
        /// components are clamped to the range [0, 1] before being stored. See glBlendFunc for a complete
        /// description of the blending operations. Initially the GL_BLEND_COLOR is set to (0, 0, 0, 0).
        /// </para>
        /// </summary>
        /// <param name="red">
        /// specify the components of GL_BLEND_COLOR
        /// </param>
        /// <param name="green">
        /// specify the components of GL_BLEND_COLOR
        /// </param>
        /// <param name="blue">
        /// specify the components of GL_BLEND_COLOR
        /// </param>
        /// <param name="alpha">
        /// specify the components of GL_BLEND_COLOR
        /// </param>
        public static void BlendColor(Single red, Single green, Single blue, Single alpha)
        {
            Delegates.glBlendColor(red, green, blue, alpha);
        }

        /// <summary>
        /// Specify the equation used for both the RGB blend equation and the Alpha blend equation.
        /// <para>
        /// The blend equations determine how a new pixel (the ''source'' color) is combined with a pixel
        /// already in the framebuffer (the ''destination'' color). This function sets both the RGB blend
        /// equation and the alpha blend equation to a single equation. glBlendEquationi specifies the blend
        /// equation for a single draw buffer whereas glBlendEquation sets the blend equation for all draw
        /// buffers.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// specifies how source and destination colors are combined. It must be GL_FUNC_ADD, GL_FUNC_SUBTRACT,
        /// GL_FUNC_REVERSE_SUBTRACT, GL_MIN, GL_MAX.
        /// </param>
        public static void BlendEquation(BlendEquationMode mode)
        {
            Delegates.glBlendEquation(mode);
        }

        /// <summary>
        /// Specify the equation used for both the RGB blend equation and the Alpha blend equation.
        /// <para>
        /// The blend equations determine how a new pixel (the ''source'' color) is combined with a pixel
        /// already in the framebuffer (the ''destination'' color). This function sets both the RGB blend
        /// equation and the alpha blend equation to a single equation. glBlendEquationi specifies the blend
        /// equation for a single draw buffer whereas glBlendEquation sets the blend equation for all draw
        /// buffers.
        /// </para>
        /// </summary>
        /// <param name="buf">
        /// for glBlendEquationi, specifies the index of the draw buffer for which to set the blend equation.
        /// </param>
        /// <param name="mode">
        /// specifies how source and destination colors are combined. It must be GL_FUNC_ADD, GL_FUNC_SUBTRACT,
        /// GL_FUNC_REVERSE_SUBTRACT, GL_MIN, GL_MAX.
        /// </param>
        public static void BlendEquationi(UInt32 buf, BlendEquationMode mode)
        {
            Delegates.glBlendEquationi(buf, mode);
        }

        /// <summary>
        /// Set the RGB blend equation and the alpha blend equation separately.
        /// <para>
        /// The blend equations determines how a new pixel (the ''source'' color) is combined with a pixel
        /// already in the framebuffer (the ''destination'' color). These functions specify one blend equation
        /// for the RGB-color components and one blend equation for the alpha component.
        /// glBlendEquationSeparatei specifies the blend equations for a single draw buffer whereas
        /// glBlendEquationSeparate sets the blend equations for all draw buffers.
        /// </para>
        /// </summary>
        /// <param name="modeRGB">
        /// specifies the RGB blend equation, how the red, green, and blue components of the source and
        /// destination colors are combined. It must be GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT,
        /// GL_MIN, GL_MAX.
        /// </param>
        /// <param name="modeAlpha">
        /// specifies the alpha blend equation, how the alpha component of the source and destination colors are
        /// combined. It must be GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT, GL_MIN, GL_MAX.
        /// </param>
        public static void BlendEquationSeparate(BlendEquationMode modeRGB, BlendEquationMode modeAlpha)
        {
            Delegates.glBlendEquationSeparate(modeRGB, modeAlpha);
        }

        /// <summary>
        /// Set the RGB blend equation and the alpha blend equation separately.
        /// <para>
        /// The blend equations determines how a new pixel (the ''source'' color) is combined with a pixel
        /// already in the framebuffer (the ''destination'' color). These functions specify one blend equation
        /// for the RGB-color components and one blend equation for the alpha component.
        /// glBlendEquationSeparatei specifies the blend equations for a single draw buffer whereas
        /// glBlendEquationSeparate sets the blend equations for all draw buffers.
        /// </para>
        /// </summary>
        /// <param name="buf">
        /// for glBlendEquationSeparatei, specifies the index of the draw buffer for which to set the blend
        /// equations.
        /// </param>
        /// <param name="modeRGB">
        /// specifies the RGB blend equation, how the red, green, and blue components of the source and
        /// destination colors are combined. It must be GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT,
        /// GL_MIN, GL_MAX.
        /// </param>
        /// <param name="modeAlpha">
        /// specifies the alpha blend equation, how the alpha component of the source and destination colors are
        /// combined. It must be GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT, GL_MIN, GL_MAX.
        /// </param>
        public static void BlendEquationSeparatei(UInt32 buf, BlendEquationMode modeRGB, BlendEquationMode modeAlpha)
        {
            Delegates.glBlendEquationSeparatei(buf, modeRGB, modeAlpha);
        }

        /// <summary>
        /// Specify pixel arithmetic.
        /// <para>
        /// Pixels can be drawn using a function that blends the incoming (source) RGBA values with the RGBA
        /// values that are already in the frame buffer (the destination values). Blending is initially
        /// disabled. Use glEnable and glDisable with argument GL_BLEND to enable and disable blending.
        /// </para>
        /// </summary>
        /// <param name="sfactor">
        /// Specifies how the red, green, blue, and alpha source blending factors are computed. The initial
        /// value is GL_ONE.
        /// </param>
        /// <param name="dfactor">
        /// Specifies how the red, green, blue, and alpha destination blending factors are computed. The
        /// following symbolic constants are accepted: GL_ZERO, GL_ONE, GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR,
        /// GL_DST_COLOR, GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_DST_ALPHA,
        /// GL_ONE_MINUS_DST_ALPHA. GL_CONSTANT_COLOR, GL_ONE_MINUS_CONSTANT_COLOR, GL_CONSTANT_ALPHA, and
        /// GL_ONE_MINUS_CONSTANT_ALPHA. The initial value is GL_ZERO.
        /// </param>
        public static void BlendFunc(BlendingFactorSrc sfactor, BlendingFactorDest dfactor)
        {
            Delegates.glBlendFunc(sfactor, dfactor);
        }

        /// <summary>
        /// Specify pixel arithmetic.
        /// <para>
        /// Pixels can be drawn using a function that blends the incoming (source) RGBA values with the RGBA
        /// values that are already in the frame buffer (the destination values). Blending is initially
        /// disabled. Use glEnable and glDisable with argument GL_BLEND to enable and disable blending.
        /// </para>
        /// </summary>
        /// <param name="buf">
        /// For glBlendFunci, specifies the index of the draw buffer for which to set the blend function.
        /// </param>
        /// <param name="sfactor">
        /// Specifies how the red, green, blue, and alpha source blending factors are computed. The initial
        /// value is GL_ONE.
        /// </param>
        /// <param name="dfactor">
        /// Specifies how the red, green, blue, and alpha destination blending factors are computed. The
        /// following symbolic constants are accepted: GL_ZERO, GL_ONE, GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR,
        /// GL_DST_COLOR, GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_DST_ALPHA,
        /// GL_ONE_MINUS_DST_ALPHA. GL_CONSTANT_COLOR, GL_ONE_MINUS_CONSTANT_COLOR, GL_CONSTANT_ALPHA, and
        /// GL_ONE_MINUS_CONSTANT_ALPHA. The initial value is GL_ZERO.
        /// </param>
        public static void BlendFunci(UInt32 buf, BlendingFactorSrc sfactor, BlendingFactorDest dfactor)
        {
            Delegates.glBlendFunci(buf, sfactor, dfactor);
        }

        /// <summary>
        /// Specify pixel arithmetic for RGB and alpha components separately.
        /// <para>
        /// Pixels can be drawn using a function that blends the incoming (source) RGBA values with the RGBA
        /// values that are already in the frame buffer (the destination values). Blending is initially
        /// disabled. Use glEnable and glDisable with argument GL_BLEND to enable and disable blending.
        /// </para>
        /// </summary>
        /// <param name="srcRGB">
        /// Specifies how the red, green, and blue blending factors are computed. The initial value is GL_ONE.
        /// </param>
        /// <param name="dstRGB">
        /// Specifies how the red, green, and blue destination blending factors are computed. The initial value
        /// is GL_ZERO.
        /// </param>
        /// <param name="srcAlpha">
        /// Specified how the alpha source blending factor is computed. The initial value is GL_ONE.
        /// </param>
        /// <param name="dstAlpha">
        /// Specified how the alpha destination blending factor is computed. The initial value is GL_ZERO.
        /// </param>
        public static void BlendFuncSeparate(BlendingFactorSrc srcRGB, BlendingFactorDest dstRGB, BlendingFactorSrc srcAlpha, BlendingFactorDest dstAlpha)
        {
            Delegates.glBlendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha);
        }

        /// <summary>
        /// Specify pixel arithmetic for RGB and alpha components separately.
        /// <para>
        /// Pixels can be drawn using a function that blends the incoming (source) RGBA values with the RGBA
        /// values that are already in the frame buffer (the destination values). Blending is initially
        /// disabled. Use glEnable and glDisable with argument GL_BLEND to enable and disable blending.
        /// </para>
        /// </summary>
        /// <param name="buf">
        /// For glBlendFuncSeparatei, specifies the index of the draw buffer for which to set the blend
        /// functions.
        /// </param>
        /// <param name="srcRGB">
        /// Specifies how the red, green, and blue blending factors are computed. The initial value is GL_ONE.
        /// </param>
        /// <param name="dstRGB">
        /// Specifies how the red, green, and blue destination blending factors are computed. The initial value
        /// is GL_ZERO.
        /// </param>
        /// <param name="srcAlpha">
        /// Specified how the alpha source blending factor is computed. The initial value is GL_ONE.
        /// </param>
        /// <param name="dstAlpha">
        /// Specified how the alpha destination blending factor is computed. The initial value is GL_ZERO.
        /// </param>
        public static void BlendFuncSeparatei(UInt32 buf, BlendingFactorSrc srcRGB, BlendingFactorDest dstRGB, BlendingFactorSrc srcAlpha, BlendingFactorDest dstAlpha)
        {
            Delegates.glBlendFuncSeparatei(buf, srcRGB, dstRGB, srcAlpha, dstAlpha);
        }

        /// <summary>
        /// Copy a block of pixels from one framebuffer object to another.
        /// <para>
        /// glBlitFramebuffer and glBlitNamedFramebuffer transfer a rectangle of pixel values from one region of
        /// a read framebuffer to another region of a draw framebuffer.
        /// </para>
        /// </summary>
        /// <param name="srcX0">
        /// Specify the bounds of the source rectangle within the read buffer of the read framebuffer.
        /// </param>
        /// <param name="srcY0">
        /// Specify the bounds of the source rectangle within the read buffer of the read framebuffer.
        /// </param>
        /// <param name="srcX1">
        /// Specify the bounds of the source rectangle within the read buffer of the read framebuffer.
        /// </param>
        /// <param name="srcY1">
        /// Specify the bounds of the source rectangle within the read buffer of the read framebuffer.
        /// </param>
        /// <param name="dstX0">
        /// Specify the bounds of the destination rectangle within the write buffer of the write framebuffer.
        /// </param>
        /// <param name="dstY0">
        /// Specify the bounds of the destination rectangle within the write buffer of the write framebuffer.
        /// </param>
        /// <param name="dstX1">
        /// Specify the bounds of the destination rectangle within the write buffer of the write framebuffer.
        /// </param>
        /// <param name="dstY1">
        /// Specify the bounds of the destination rectangle within the write buffer of the write framebuffer.
        /// </param>
        /// <param name="mask">
        /// The bitwise OR of the flags indicating which buffers are to be copied. The allowed flags are
        /// GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT and GL_STENCIL_BUFFER_BIT.
        /// </param>
        /// <param name="filter">
        /// Specifies the interpolation to be applied if the image is stretched. Must be GL_NEAREST or
        /// GL_LINEAR.
        /// </param>
        public static void BlitFramebuffer(Int32 srcX0, Int32 srcY0, Int32 srcX1, Int32 srcY1, Int32 dstX0, Int32 dstY0, Int32 dstX1, Int32 dstY1, ClearBufferMask mask, BlitFramebufferFilter filter)
        {
            Delegates.glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
        }

        /// <summary>
        /// Copy a block of pixels from one framebuffer object to another.
        /// <para>
        /// glBlitFramebuffer and glBlitNamedFramebuffer transfer a rectangle of pixel values from one region of
        /// a read framebuffer to another region of a draw framebuffer.
        /// </para>
        /// </summary>
        /// <param name="readFramebuffer">
        /// Specifies the name of the source framebuffer object for glBlitNamedFramebuffer.
        /// </param>
        /// <param name="drawFramebuffer">
        /// Specifies the name of the destination framebuffer object for glBlitNamedFramebuffer.
        /// </param>
        /// <param name="srcX0">
        /// Specify the bounds of the source rectangle within the read buffer of the read framebuffer.
        /// </param>
        /// <param name="srcY0">
        /// Specify the bounds of the source rectangle within the read buffer of the read framebuffer.
        /// </param>
        /// <param name="srcX1">
        /// Specify the bounds of the source rectangle within the read buffer of the read framebuffer.
        /// </param>
        /// <param name="srcY1">
        /// Specify the bounds of the source rectangle within the read buffer of the read framebuffer.
        /// </param>
        /// <param name="dstX0">
        /// Specify the bounds of the destination rectangle within the write buffer of the write framebuffer.
        /// </param>
        /// <param name="dstY0">
        /// Specify the bounds of the destination rectangle within the write buffer of the write framebuffer.
        /// </param>
        /// <param name="dstX1">
        /// Specify the bounds of the destination rectangle within the write buffer of the write framebuffer.
        /// </param>
        /// <param name="dstY1">
        /// Specify the bounds of the destination rectangle within the write buffer of the write framebuffer.
        /// </param>
        /// <param name="mask">
        /// The bitwise OR of the flags indicating which buffers are to be copied. The allowed flags are
        /// GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT and GL_STENCIL_BUFFER_BIT.
        /// </param>
        /// <param name="filter">
        /// Specifies the interpolation to be applied if the image is stretched. Must be GL_NEAREST or
        /// GL_LINEAR.
        /// </param>
        public static void BlitNamedFramebuffer(UInt32 readFramebuffer, UInt32 drawFramebuffer, Int32 srcX0, Int32 srcY0, Int32 srcX1, Int32 srcY1, Int32 dstX0, Int32 dstY0, Int32 dstX1, Int32 dstY1, ClearBufferMask mask, BlitFramebufferFilter filter)
        {
            Delegates.glBlitNamedFramebuffer(readFramebuffer, drawFramebuffer, srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
        }

        /// <summary>
        /// Creates and initializes a buffer object's data store.
        /// <para>
        /// glBufferData and glNamedBufferData create a new data store for a buffer object. In case of
        /// glBufferData, the buffer object currently bound to target is used. For glNamedBufferData, a buffer
        /// object associated with ID specified by the caller in buffer will be used instead.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glBufferData, which must be one of the
        /// buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER Vertex
        /// attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy source
        /// GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute dispatch
        /// commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex array
        /// indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="size">
        /// Specifies the size in bytes of the buffer object's new data store.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to data that will be copied into the data store for initialization, or NULL if
        /// no data is to be copied.
        /// </param>
        /// <param name="usage">
        /// Specifies the expected usage pattern of the data store. The symbolic constant must be
        /// GL_STREAM_DRAW, GL_STREAM_READ, GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY,
        /// GL_DYNAMIC_DRAW, GL_DYNAMIC_READ, or GL_DYNAMIC_COPY.
        /// </param>
        public static void BufferData(BufferTarget target, IntPtr size, IntPtr data, BufferUsageHint usage)
        {
            Delegates.glBufferData(target, size, data, usage);
        }

        /// <summary>
        /// Creates and initializes a buffer object's data store.
        /// <para>
        /// glBufferData and glNamedBufferData create a new data store for a buffer object. In case of
        /// glBufferData, the buffer object currently bound to target is used. For glNamedBufferData, a buffer
        /// object associated with ID specified by the caller in buffer will be used instead.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glNamedBufferData function.
        /// </param>
        /// <param name="size">
        /// Specifies the size in bytes of the buffer object's new data store.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to data that will be copied into the data store for initialization, or NULL if
        /// no data is to be copied.
        /// </param>
        /// <param name="usage">
        /// Specifies the expected usage pattern of the data store. The symbolic constant must be
        /// GL_STREAM_DRAW, GL_STREAM_READ, GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY,
        /// GL_DYNAMIC_DRAW, GL_DYNAMIC_READ, or GL_DYNAMIC_COPY.
        /// </param>
        public static void NamedBufferData(UInt32 buffer, Int32 size, IntPtr data, BufferUsageHint usage)
        {
            Delegates.glNamedBufferData(buffer, size, data, usage);
        }

        /// <summary>
        /// Creates and initializes a buffer object's immutable data store.
        /// <para>
        /// glBufferStorage and glNamedBufferStorage create a new immutable data store. For glBufferStorage, the
        /// buffer object currently bound to target will be initialized. For glNamedBufferStorage, buffer is the
        /// name of the buffer object that will be configured. The size of the data store is specified by size.
        /// If an initial data is available, its address may be supplied in data. Otherwise, to create an
        /// uninitialized data store, data should be NULL.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glBufferStorage, which must be one of
        /// the buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER
        /// Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy
        /// source GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute
        /// dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex
        /// array indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="size">
        /// Specifies the size in bytes of the buffer object's new data store.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to data that will be copied into the data store for initialization, or NULL if
        /// no data is to be copied.
        /// </param>
        /// <param name="flags">
        /// Specifies the intended usage of the buffer's data store. Must be a bitwise combination of the
        /// following flags. GL_DYNAMIC_STORAGE_BIT, GL_MAP_READ_BIT GL_MAP_WRITE_BIT, GL_MAP_PERSISTENT_BIT,
        /// GL_MAP_COHERENT_BIT, and GL_CLIENT_STORAGE_BIT.
        /// </param>
        public static void BufferStorage(BufferTarget target, IntPtr size, IntPtr data, UInt32 flags)
        {
            Delegates.glBufferStorage(target, size, data, flags);
        }

        /// <summary>
        /// Creates and initializes a buffer object's immutable data store.
        /// <para>
        /// glBufferStorage and glNamedBufferStorage create a new immutable data store. For glBufferStorage, the
        /// buffer object currently bound to target will be initialized. For glNamedBufferStorage, buffer is the
        /// name of the buffer object that will be configured. The size of the data store is specified by size.
        /// If an initial data is available, its address may be supplied in data. Otherwise, to create an
        /// uninitialized data store, data should be NULL.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glNamedBufferStorage function.
        /// </param>
        /// <param name="size">
        /// Specifies the size in bytes of the buffer object's new data store.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to data that will be copied into the data store for initialization, or NULL if
        /// no data is to be copied.
        /// </param>
        /// <param name="flags">
        /// Specifies the intended usage of the buffer's data store. Must be a bitwise combination of the
        /// following flags. GL_DYNAMIC_STORAGE_BIT, GL_MAP_READ_BIT GL_MAP_WRITE_BIT, GL_MAP_PERSISTENT_BIT,
        /// GL_MAP_COHERENT_BIT, and GL_CLIENT_STORAGE_BIT.
        /// </param>
        public static void NamedBufferStorage(UInt32 buffer, Int32 size, IntPtr data, UInt32 flags)
        {
            Delegates.glNamedBufferStorage(buffer, size, data, flags);
        }

        /// <summary>
        /// Updates a subset of a buffer object's data store.
        /// <para>
        /// glBufferSubData and glNamedBufferSubData redefine some or all of the data store for the specified
        /// buffer object. Data starting at byte offset offset and extending for size bytes is copied to the
        /// data store from the memory pointed to by data. offset and size must define a range lying entirely
        /// within the buffer object's data store.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glBufferSubData, which must be one of
        /// the buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER
        /// Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy
        /// source GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute
        /// dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex
        /// array indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="offset">
        /// Specifies the offset into the buffer object's data store where data replacement will begin, measured
        /// in bytes.
        /// </param>
        /// <param name="size">
        /// Specifies the size in bytes of the data store region being replaced.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the new data that will be copied into the data store.
        /// </param>
        public static void BufferSubData(BufferTarget target, IntPtr offset, IntPtr size, IntPtr data)
        {
            Delegates.glBufferSubData(target, offset, size, data);
        }

        /// <summary>
        /// Updates a subset of a buffer object's data store.
        /// <para>
        /// glBufferSubData and glNamedBufferSubData redefine some or all of the data store for the specified
        /// buffer object. Data starting at byte offset offset and extending for size bytes is copied to the
        /// data store from the memory pointed to by data. offset and size must define a range lying entirely
        /// within the buffer object's data store.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glNamedBufferSubData.
        /// </param>
        /// <param name="offset">
        /// Specifies the offset into the buffer object's data store where data replacement will begin, measured
        /// in bytes.
        /// </param>
        /// <param name="size">
        /// Specifies the size in bytes of the data store region being replaced.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the new data that will be copied into the data store.
        /// </param>
        public static void NamedBufferSubData(UInt32 buffer, IntPtr offset, Int32 size, IntPtr data)
        {
            Delegates.glNamedBufferSubData(buffer, offset, size, data);
        }

        /// <summary>
        /// Check the completeness status of a framebuffer.
        /// <para>
        /// glCheckFramebufferStatus and glCheckNamedFramebufferStatus return the completeness status of a
        /// framebuffer object when treated as a read or draw framebuffer, depending on the value of target.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specify the target to which the framebuffer is bound for glCheckFramebufferStatus, and the target
        /// against which framebuffer completeness of framebuffer is checked for glCheckNamedFramebufferStatus.
        /// </param>
        public static FramebufferErrorCode CheckFramebufferStatus(FramebufferTarget target)
        {
            return Delegates.glCheckFramebufferStatus(target);
        }

        /// <summary>
        /// Check the completeness status of a framebuffer.
        /// <para>
        /// glCheckFramebufferStatus and glCheckNamedFramebufferStatus return the completeness status of a
        /// framebuffer object when treated as a read or draw framebuffer, depending on the value of target.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glCheckNamedFramebufferStatus
        /// </param>
        /// <param name="target">
        /// Specify the target to which the framebuffer is bound for glCheckFramebufferStatus, and the target
        /// against which framebuffer completeness of framebuffer is checked for glCheckNamedFramebufferStatus.
        /// </param>
        public static FramebufferErrorCode CheckNamedFramebufferStatus(UInt32 framebuffer, FramebufferTarget target)
        {
            return Delegates.glCheckNamedFramebufferStatus(framebuffer, target);
        }

        /// <summary>
        /// Specify whether data read via glReadPixels should be clamped.
        /// <para>
        /// glClampColor controls color clamping that is performed during glReadPixels. target must be
        /// GL_CLAMP_READ_COLOR. If clamp is GL_TRUE, read color clamping is enabled; if clamp is GL_FALSE, read
        /// color clamping is disabled. If clamp is GL_FIXED_ONLY, read color clamping is enabled only if the
        /// selected read buffer has fixed point components and disabled otherwise.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Target for color clamping. target must be GL_CLAMP_READ_COLOR.
        /// </param>
        /// <param name="clamp">
        /// Specifies whether to apply color clamping. clamp must be GL_TRUE or GL_FALSE.
        /// </param>
        public static void ClampColor(ClampColorTarget target, ClampColorMode clamp)
        {
            Delegates.glClampColor(target, clamp);
        }

        /// <summary>
        /// Clear buffers to preset values.
        /// <para>
        /// glClear sets the bitplane area of the window to values previously selected by glClearColor,
        /// glClearDepth, and glClearStencil. Multiple color buffers can be cleared simultaneously by selecting
        /// more than one buffer at a time using glDrawBuffer.
        /// </para>
        /// </summary>
        /// <param name="mask">
        /// Bitwise OR of masks that indicate the buffers to be cleared. The three masks are
        /// GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT, and GL_STENCIL_BUFFER_BIT.
        /// </param>
        public static void Clear(ClearBufferMask mask)
        {
            Delegates.glClear(mask);
        }

        /// <summary>
        /// Clear individual buffers of a framebuffer.
        /// <para>
        /// These commands clear a specified buffer of a framebuffer to specified value(s). For glClearBuffer*,
        /// the framebuffer is the currently bound draw framebuffer object. For glClearNamedFramebuffer*,
        /// framebuffer is zero, indicating the default draw framebuffer, or the name of a framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specify the buffer to clear.
        /// </param>
        /// <param name="drawbuffer">
        /// Specify a particular draw buffer to clear.
        /// </param>
        /// <param name="value">
        /// A pointer to the value or values to clear the buffer to.
        /// </param>
        public static void ClearBufferiv(ClearBuffer buffer, Int32 drawbuffer, Int32[] value)
        {
            Delegates.glClearBufferiv(buffer, drawbuffer, value);
        }

        /// <summary>
        /// Clear individual buffers of a framebuffer.
        /// <para>
        /// These commands clear a specified buffer of a framebuffer to specified value(s). For glClearBuffer*,
        /// the framebuffer is the currently bound draw framebuffer object. For glClearNamedFramebuffer*,
        /// framebuffer is zero, indicating the default draw framebuffer, or the name of a framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specify the buffer to clear.
        /// </param>
        /// <param name="drawbuffer">
        /// Specify a particular draw buffer to clear.
        /// </param>
        /// <param name="value">
        /// A pointer to the value or values to clear the buffer to.
        /// </param>
        public static void ClearBufferuiv(ClearBuffer buffer, Int32 drawbuffer, UInt32[] value)
        {
            Delegates.glClearBufferuiv(buffer, drawbuffer, value);
        }

        /// <summary>
        /// Clear individual buffers of a framebuffer.
        /// <para>
        /// These commands clear a specified buffer of a framebuffer to specified value(s). For glClearBuffer*,
        /// the framebuffer is the currently bound draw framebuffer object. For glClearNamedFramebuffer*,
        /// framebuffer is zero, indicating the default draw framebuffer, or the name of a framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specify the buffer to clear.
        /// </param>
        /// <param name="drawbuffer">
        /// Specify a particular draw buffer to clear.
        /// </param>
        /// <param name="value">
        /// A pointer to the value or values to clear the buffer to.
        /// </param>
        public static void ClearBufferfv(ClearBuffer buffer, Int32 drawbuffer, Single[] value)
        {
            Delegates.glClearBufferfv(buffer, drawbuffer, value);
        }

        /// <summary>
        /// Clear individual buffers of a framebuffer.
        /// <para>
        /// These commands clear a specified buffer of a framebuffer to specified value(s). For glClearBuffer*,
        /// the framebuffer is the currently bound draw framebuffer object. For glClearNamedFramebuffer*,
        /// framebuffer is zero, indicating the default draw framebuffer, or the name of a framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specify the buffer to clear.
        /// </param>
        /// <param name="drawbuffer">
        /// Specify a particular draw buffer to clear.
        /// </param>
        /// <param name="depth">
        /// The value to clear the depth buffer to.
        /// </param>
        /// <param name="stencil">
        /// The value to clear the stencil buffer to.
        /// </param>
        public static void ClearBufferfi(ClearBuffer buffer, Int32 drawbuffer, Single depth, Int32 stencil)
        {
            Delegates.glClearBufferfi(buffer, drawbuffer, depth, stencil);
        }

        /// <summary>
        /// Clear individual buffers of a framebuffer.
        /// <para>
        /// These commands clear a specified buffer of a framebuffer to specified value(s). For glClearBuffer*,
        /// the framebuffer is the currently bound draw framebuffer object. For glClearNamedFramebuffer*,
        /// framebuffer is zero, indicating the default draw framebuffer, or the name of a framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glClearNamedFramebuffer*.
        /// </param>
        /// <param name="buffer">
        /// Specify the buffer to clear.
        /// </param>
        /// <param name="drawbuffer">
        /// Specify a particular draw buffer to clear.
        /// </param>
        /// <param name="value">
        /// A pointer to the value or values to clear the buffer to.
        /// </param>
        public static void ClearNamedFramebufferiv(UInt32 framebuffer, ClearBuffer buffer, Int32 drawbuffer, Int32[] value)
        {
            Delegates.glClearNamedFramebufferiv(framebuffer, buffer, drawbuffer, value);
        }

        /// <summary>
        /// Clear individual buffers of a framebuffer.
        /// <para>
        /// These commands clear a specified buffer of a framebuffer to specified value(s). For glClearBuffer*,
        /// the framebuffer is the currently bound draw framebuffer object. For glClearNamedFramebuffer*,
        /// framebuffer is zero, indicating the default draw framebuffer, or the name of a framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glClearNamedFramebuffer*.
        /// </param>
        /// <param name="buffer">
        /// Specify the buffer to clear.
        /// </param>
        /// <param name="drawbuffer">
        /// Specify a particular draw buffer to clear.
        /// </param>
        /// <param name="value">
        /// A pointer to the value or values to clear the buffer to.
        /// </param>
        public static void ClearNamedFramebufferuiv(UInt32 framebuffer, ClearBuffer buffer, Int32 drawbuffer, UInt32[] value)
        {
            Delegates.glClearNamedFramebufferuiv(framebuffer, buffer, drawbuffer, value);
        }

        /// <summary>
        /// Clear individual buffers of a framebuffer.
        /// <para>
        /// These commands clear a specified buffer of a framebuffer to specified value(s). For glClearBuffer*,
        /// the framebuffer is the currently bound draw framebuffer object. For glClearNamedFramebuffer*,
        /// framebuffer is zero, indicating the default draw framebuffer, or the name of a framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glClearNamedFramebuffer*.
        /// </param>
        /// <param name="buffer">
        /// Specify the buffer to clear.
        /// </param>
        /// <param name="drawbuffer">
        /// Specify a particular draw buffer to clear.
        /// </param>
        /// <param name="value">
        /// A pointer to the value or values to clear the buffer to.
        /// </param>
        public static void ClearNamedFramebufferfv(UInt32 framebuffer, ClearBuffer buffer, Int32 drawbuffer, Single[] value)
        {
            Delegates.glClearNamedFramebufferfv(framebuffer, buffer, drawbuffer, value);
        }

        /// <summary>
        /// Clear individual buffers of a framebuffer.
        /// <para>
        /// These commands clear a specified buffer of a framebuffer to specified value(s). For glClearBuffer*,
        /// the framebuffer is the currently bound draw framebuffer object. For glClearNamedFramebuffer*,
        /// framebuffer is zero, indicating the default draw framebuffer, or the name of a framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glClearNamedFramebuffer*.
        /// </param>
        /// <param name="buffer">
        /// Specify the buffer to clear.
        /// </param>
        /// <param name="drawbuffer">
        /// Specify a particular draw buffer to clear.
        /// </param>
        /// <param name="depth">
        /// The value to clear the depth buffer to.
        /// </param>
        /// <param name="stencil">
        /// The value to clear the stencil buffer to.
        /// </param>
        public static void ClearNamedFramebufferfi(UInt32 framebuffer, ClearBuffer buffer, Int32 drawbuffer, Single depth, Int32 stencil)
        {
            Delegates.glClearNamedFramebufferfi(framebuffer, buffer, drawbuffer, depth, stencil);
        }

        /// <summary>
        /// Fill a buffer object's data store with a fixed value.
        /// <para>
        /// glClearBufferData and glClearNamedBufferData fill the entirety of a buffer object's data store with
        /// data from client memory.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glClearBufferData, which must must be
        /// one of the buffer binding targets in the following table: Buffer Binding Target Purpose
        /// GL_ARRAY_BUFFER Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage
        /// GL_COPY_READ_BUFFER Buffer copy source GL_COPY_WRITE_BUFFER Buffer copy destination
        /// GL_DISPATCH_INDIRECT_BUFFER Indirect compute dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect
        /// command arguments GL_ELEMENT_ARRAY_BUFFER Vertex array indices GL_PIXEL_PACK_BUFFER Pixel read
        /// target GL_PIXEL_UNPACK_BUFFER Texture data source GL_QUERY_BUFFER Query result buffer
        /// GL_SHADER_STORAGE_BUFFER Read-write storage for shaders GL_TEXTURE_BUFFER Texture data buffer
        /// GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="internalFormat">
        /// The internal format with which the data will be stored in the buffer object.
        /// </param>
        /// <param name="format">
        /// The format of the data in memory addressed by data.
        /// </param>
        /// <param name="type">
        /// The type of the data in memory addressed by data.
        /// </param>
        /// <param name="data">
        /// The address of a memory location storing the data to be replicated into the buffer's data store.
        /// </param>
        public static void ClearBufferData(BufferTarget target, SizedInternalFormat internalFormat, PixelInternalFormat format, PixelType type, IntPtr data)
        {
            Delegates.glClearBufferData(target, internalFormat, format, type, data);
        }

        /// <summary>
        /// Fill a buffer object's data store with a fixed value.
        /// <para>
        /// glClearBufferData and glClearNamedBufferData fill the entirety of a buffer object's data store with
        /// data from client memory.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glClearNamedBufferData.
        /// </param>
        /// <param name="internalFormat">
        /// The internal format with which the data will be stored in the buffer object.
        /// </param>
        /// <param name="format">
        /// The format of the data in memory addressed by data.
        /// </param>
        /// <param name="type">
        /// The type of the data in memory addressed by data.
        /// </param>
        /// <param name="data">
        /// The address of a memory location storing the data to be replicated into the buffer's data store.
        /// </param>
        public static void ClearNamedBufferData(UInt32 buffer, SizedInternalFormat internalFormat, PixelInternalFormat format, PixelType type, IntPtr data)
        {
            Delegates.glClearNamedBufferData(buffer, internalFormat, format, type, data);
        }

        /// <summary>
        /// Fill all or part of buffer object's data store with a fixed value.
        /// <para>
        /// glClearBufferSubData and glClearNamedBufferSubData fill a specified region of a buffer object's data
        /// store with data from client memory.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glClearBufferSubData, which must be one
        /// of the buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER
        /// Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy
        /// source GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute
        /// dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex
        /// array indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="internalFormat">
        /// The internal format with which the data will be stored in the buffer object.
        /// </param>
        /// <param name="offset">
        /// The offset in basic machine units into the buffer object's data store at which to start filling.
        /// </param>
        /// <param name="size">
        /// The size in basic machine units of the range of the data store to fill.
        /// </param>
        /// <param name="format">
        /// The format of the data in memory addressed by data.
        /// </param>
        /// <param name="type">
        /// The type of the data in memory addressed by data.
        /// </param>
        /// <param name="data">
        /// The address of a memory location storing the data to be replicated into the buffer's data store.
        /// </param>
        public static void ClearBufferSubData(BufferTarget target, SizedInternalFormat internalFormat, IntPtr offset, IntPtr size, PixelInternalFormat format, PixelType type, IntPtr data)
        {
            Delegates.glClearBufferSubData(target, internalFormat, offset, size, format, type, data);
        }

        /// <summary>
        /// Fill all or part of buffer object's data store with a fixed value.
        /// <para>
        /// glClearBufferSubData and glClearNamedBufferSubData fill a specified region of a buffer object's data
        /// store with data from client memory.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glClearNamedBufferSubData.
        /// </param>
        /// <param name="internalFormat">
        /// The internal format with which the data will be stored in the buffer object.
        /// </param>
        /// <param name="offset">
        /// The offset in basic machine units into the buffer object's data store at which to start filling.
        /// </param>
        /// <param name="size">
        /// The size in basic machine units of the range of the data store to fill.
        /// </param>
        /// <param name="format">
        /// The format of the data in memory addressed by data.
        /// </param>
        /// <param name="type">
        /// The type of the data in memory addressed by data.
        /// </param>
        /// <param name="data">
        /// The address of a memory location storing the data to be replicated into the buffer's data store.
        /// </param>
        public static void ClearNamedBufferSubData(UInt32 buffer, SizedInternalFormat internalFormat, IntPtr offset, Int32 size, PixelInternalFormat format, PixelType type, IntPtr data)
        {
            Delegates.glClearNamedBufferSubData(buffer, internalFormat, offset, size, format, type, data);
        }

        /// <summary>
        /// Specify clear values for the color buffers.
        /// <para>
        /// glClearColor specifies the red, green, blue, and alpha values used by glClear to clear the color
        /// buffers. Values specified by glClearColor are clamped to the range [0, 1].
        /// </para>
        /// </summary>
        /// <param name="red">
        /// Specify the red, green, blue, and alpha values used when the color buffers are cleared. The initial
        /// values are all 0.
        /// </param>
        /// <param name="green">
        /// Specify the red, green, blue, and alpha values used when the color buffers are cleared. The initial
        /// values are all 0.
        /// </param>
        /// <param name="blue">
        /// Specify the red, green, blue, and alpha values used when the color buffers are cleared. The initial
        /// values are all 0.
        /// </param>
        /// <param name="alpha">
        /// Specify the red, green, blue, and alpha values used when the color buffers are cleared. The initial
        /// values are all 0.
        /// </param>
        public static void ClearColor(Single red, Single green, Single blue, Single alpha)
        {
            Delegates.glClearColor(red, green, blue, alpha);
        }

        /// <summary>
        /// Specify the clear value for the depth buffer.
        /// <para>
        /// glClearDepth specifies the depth value used by glClear to clear the depth buffer. Values specified
        /// by glClearDepth are clamped to the range [0, 1].
        /// </para>
        /// </summary>
        /// <param name="depth">
        /// Specifies the depth value used when the depth buffer is cleared. The initial value is 1.
        /// </param>
        public static void ClearDepth(Double depth)
        {
            Delegates.glClearDepth(depth);
        }

        /// <summary>
        /// Specify the clear value for the depth buffer.
        /// <para>
        /// glClearDepth specifies the depth value used by glClear to clear the depth buffer. Values specified
        /// by glClearDepth are clamped to the range [0, 1].
        /// </para>
        /// </summary>
        /// <param name="depth">
        /// Specifies the depth value used when the depth buffer is cleared. The initial value is 1.
        /// </param>
        public static void ClearDepthf(Single depth)
        {
            Delegates.glClearDepthf(depth);
        }

        /// <summary>
        /// Specify the clear value for the stencil buffer.
        /// <para>
        /// glClearStencil specifies the index used by glClear to clear the stencil buffer. s is masked with 2 m
        /// - 1 , where m is the number of bits in the stencil buffer.
        /// </para>
        /// </summary>
        /// <param name="s">
        /// Specifies the index used when the stencil buffer is cleared. The initial value is 0.
        /// </param>
        public static void ClearStencil(Int32 s)
        {
            Delegates.glClearStencil(s);
        }

        /// <summary>
        /// Fills all a texture image with a constant value.
        /// <para>
        /// glClearTexImage fills all an image contained in a texture with an application supplied value.
        /// texture must be the name of an existing texture. Further, texture may not be the name of a buffer
        /// texture, nor may its internal format be compressed.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// The name of an existing texture object containing the image to be cleared.
        /// </param>
        /// <param name="level">
        /// The level of texture containing the region to be cleared.
        /// </param>
        /// <param name="format">
        /// The format of the data whose address in memory is given by data.
        /// </param>
        /// <param name="type">
        /// The type of the data whose address in memory is given by data.
        /// </param>
        /// <param name="data">
        /// The address in memory of the data to be used to clear the specified region.
        /// </param>
        public static void ClearTexImage(UInt32 texture, Int32 level, PixelInternalFormat format, PixelType type, IntPtr data)
        {
            Delegates.glClearTexImage(texture, level, format, type, data);
        }

        /// <summary>
        /// Fills all or part of a texture image with a constant value.
        /// <para>
        /// glClearTexSubImage fills all or part of an image contained in a texture with an application supplied
        /// value. texture must be the name of an existing texture. Further, texture may not be the name of a
        /// buffer texture, nor may its internal format be compressed.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// The name of an existing texture object containing the image to be cleared.
        /// </param>
        /// <param name="level">
        /// The level of texture containing the region to be cleared.
        /// </param>
        /// <param name="xoffset">
        /// The coordinate of the left edge of the region to be cleared.
        /// </param>
        /// <param name="yoffset">
        /// The coordinate of the lower edge of the region to be cleared.
        /// </param>
        /// <param name="zoffset">
        /// The coordinate of the front of the region to be cleared.
        /// </param>
        /// <param name="width">
        /// The width of the region to be cleared.
        /// </param>
        /// <param name="height">
        /// The height of the region to be cleared.
        /// </param>
        /// <param name="depth">
        /// The depth of the region to be cleared.
        /// </param>
        /// <param name="format">
        /// The format of the data whose address in memory is given by data.
        /// </param>
        /// <param name="type">
        /// The type of the data whose address in memory is given by data.
        /// </param>
        /// <param name="data">
        /// The address in memory of the data to be used to clear the specified region.
        /// </param>
        public static void ClearTexSubImage(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelInternalFormat format, PixelType type, IntPtr data)
        {
            Delegates.glClearTexSubImage(texture, level, xoffset, yoffset, zoffset, width, height, depth, format, type, data);
        }

        /// <summary>
        /// Block and wait for a sync object to become signaled.
        /// <para>
        /// glClientWaitSync causes the client to block and wait for the sync object specified by sync to become
        /// signaled. If sync is signaled when glClientWaitSync is called, glClientWaitSync returns immediately,
        /// otherwise it will block and wait for up to timeout nanoseconds for sync to become signaled.
        /// </para>
        /// </summary>
        /// <param name="sync">
        /// The sync object whose status to wait on.
        /// </param>
        /// <param name="flags">
        /// A bitfield controlling the command flushing behavior. flags may be GL_SYNC_FLUSH_COMMANDS_BIT.
        /// </param>
        /// <param name="timeout">
        /// The timeout, specified in nanoseconds, for which the implementation should wait for sync to become
        /// signaled.
        /// </param>
        public static ArbSync ClientWaitSync(IntPtr sync, UInt32 flags, UInt64 timeout)
        {
            return Delegates.glClientWaitSync(sync, flags, timeout);
        }

        /// <summary>
        /// Control clip coordinate to window coordinate behavior.
        /// <para>
        /// glClipControl controls the clipping volume behavior and the clip coordinate to window coordinate
        /// transformation behavior.
        /// </para>
        /// </summary>
        /// <param name="origin">
        /// Specifies the clip control origin. Must be one of GL_LOWER_LEFT or GL_UPPER_LEFT.
        /// </param>
        /// <param name="depth">
        /// Specifies the clip control depth mode. Must be one of GL_NEGATIVE_ONE_TO_ONE or GL_ZERO_TO_ONE.
        /// </param>
        public static void ClipControl(ClipControlOrigin origin, ClipControlDepth depth)
        {
            Delegates.glClipControl(origin, depth);
        }

        /// <summary>
        /// Enable and disable writing of frame buffer color components.
        /// <para>
        /// glColorMask and glColorMaski specify whether the individual color components in the frame buffer can
        /// or cannot be written. glColorMaski sets the mask for a specific draw buffer, whereas glColorMask
        /// sets the mask for all draw buffers. If red is GL_FALSE, for example, no change is made to the red
        /// component of any pixel in any of the color buffers, regardless of the drawing operation attempted.
        /// </para>
        /// </summary>
        /// <param name="red">
        /// Specify whether red, green, blue, and alpha are to be written into the frame buffer. The initial
        /// values are all GL_TRUE, indicating that the color components are written.
        /// </param>
        /// <param name="green">
        /// Specify whether red, green, blue, and alpha are to be written into the frame buffer. The initial
        /// values are all GL_TRUE, indicating that the color components are written.
        /// </param>
        /// <param name="blue">
        /// Specify whether red, green, blue, and alpha are to be written into the frame buffer. The initial
        /// values are all GL_TRUE, indicating that the color components are written.
        /// </param>
        /// <param name="alpha">
        /// Specify whether red, green, blue, and alpha are to be written into the frame buffer. The initial
        /// values are all GL_TRUE, indicating that the color components are written.
        /// </param>
        public static void ColorMask(Boolean red, Boolean green, Boolean blue, Boolean alpha)
        {
            Delegates.glColorMask(red, green, blue, alpha);
        }

        /// <summary>
        /// Enable and disable writing of frame buffer color components.
        /// <para>
        /// glColorMask and glColorMaski specify whether the individual color components in the frame buffer can
        /// or cannot be written. glColorMaski sets the mask for a specific draw buffer, whereas glColorMask
        /// sets the mask for all draw buffers. If red is GL_FALSE, for example, no change is made to the red
        /// component of any pixel in any of the color buffers, regardless of the drawing operation attempted.
        /// </para>
        /// </summary>
        /// <param name="buf">
        /// For glColorMaski, specifies the index of the draw buffer whose color mask to set.
        /// </param>
        /// <param name="red">
        /// Specify whether red, green, blue, and alpha are to be written into the frame buffer. The initial
        /// values are all GL_TRUE, indicating that the color components are written.
        /// </param>
        /// <param name="green">
        /// Specify whether red, green, blue, and alpha are to be written into the frame buffer. The initial
        /// values are all GL_TRUE, indicating that the color components are written.
        /// </param>
        /// <param name="blue">
        /// Specify whether red, green, blue, and alpha are to be written into the frame buffer. The initial
        /// values are all GL_TRUE, indicating that the color components are written.
        /// </param>
        /// <param name="alpha">
        /// Specify whether red, green, blue, and alpha are to be written into the frame buffer. The initial
        /// values are all GL_TRUE, indicating that the color components are written.
        /// </param>
        public static void ColorMaski(UInt32 buf, Boolean red, Boolean green, Boolean blue, Boolean alpha)
        {
            Delegates.glColorMaski(buf, red, green, blue, alpha);
        }

        /// <summary>
        /// Compiles a shader object.
        /// <para>
        /// glCompileShader compiles the source code strings that have been stored in the shader object
        /// specified by shader.
        /// </para>
        /// </summary>
        /// <param name="shader">
        /// Specifies the shader object to be compiled.
        /// </param>
        public static void CompileShader(UInt32 shader)
        {
            Delegates.glCompileShader(shader);
        }

        /// <summary>
        /// Specify a one-dimensional texture image in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target texture. Must be GL_TEXTURE_1D or GL_PROXY_TEXTURE_1D.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture image. All implementations support texture images that are at
        /// least 64 texels wide. The height of the 1D texture image is 1.
        /// </param>
        /// <param name="border">
        /// This value must be 0.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTexImage1D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 border, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTexImage1D(target, level, internalFormat, width, border, imageSize, data);
        }

        /// <summary>
        /// Specify a two-dimensional texture image in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target texture. Must be GL_TEXTURE_2D, GL_PROXY_TEXTURE_2D, GL_TEXTURE_1D_ARRAY,
        /// GL_PROXY_TEXTURE_1D_ARRAY, GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, or GL_PROXY_TEXTURE_CUBE_MAP.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture image. All implementations support 2D texture and cube map
        /// texture images that are at least 16384 texels wide.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture image. All implementations support 2D texture and cube map
        /// texture images that are at least 16384 texels high.
        /// </param>
        /// <param name="border">
        /// This value must be 0.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTexImage2D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 border, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTexImage2D(target, level, internalFormat, width, height, border, imageSize, data);
        }

        /// <summary>
        /// Specify a three-dimensional texture image in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target texture. Must be GL_TEXTURE_3D, GL_PROXY_TEXTURE_3D, GL_TEXTURE_2D_ARRAY or
        /// GL_PROXY_TEXTURE_2D_ARRAY.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture image. All implementations support 3D texture images that are at
        /// least 16 texels wide.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture image. All implementations support 3D texture images that are at
        /// least 16 texels high.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture image. All implementations support 3D texture images that are at
        /// least 16 texels deep.
        /// </param>
        /// <param name="border">
        /// This value must be 0.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTexImage3D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Int32 border, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTexImage3D(target, level, internalFormat, width, height, depth, border, imageSize, data);
        }

        /// <summary>
        /// Specify a one-dimensional texture subimage in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target, to which the texture is bound, for glCompressedTexSubImage1D function. Must be
        /// GL_TEXTURE_1D.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTexSubImage1D(TextureTarget target, Int32 level, Int32 xoffset, Int32 width, PixelFormat format, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTexSubImage1D(target, level, xoffset, width, format, imageSize, data);
        }

        /// <summary>
        /// Specify a one-dimensional texture subimage in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glCompressedTextureSubImage1D function.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTextureSubImage1D(UInt32 texture, Int32 level, Int32 xoffset, Int32 width, PixelInternalFormat format, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTextureSubImage1D(texture, level, xoffset, width, format, imageSize, data);
        }

        /// <summary>
        /// Specify a two-dimensional texture subimage in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glCompressedTexSubImage2D function. Must be
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z, or
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_Z.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTexSubImage2D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelFormat format, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTexSubImage2D(target, level, xoffset, yoffset, width, height, format, imageSize, data);
        }

        /// <summary>
        /// Specify a two-dimensional texture subimage in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glCompressedTextureSubImage2D function.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTextureSubImage2D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelInternalFormat format, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTextureSubImage2D(texture, level, xoffset, yoffset, width, height, format, imageSize, data);
        }

        /// <summary>
        /// Specify a three-dimensional texture subimage in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glCompressedTexSubImage3D function. Must be
        /// GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D, or GL_TEXTURE_CUBE_MAP_ARRAY.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="zoffset">
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTexSubImage3D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelFormat format, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTexSubImage3D(target, level, xoffset, yoffset, zoffset, width, height, depth, format, imageSize, data);
        }

        /// <summary>
        /// Specify a three-dimensional texture subimage in a compressed format.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glCompressedTextureSubImage3D function.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="zoffset">
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the compressed image data stored at address data.
        /// </param>
        /// <param name="imageSize">
        /// Specifies the number of unsigned bytes of image data starting at the address specified by data.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the compressed image data in memory.
        /// </param>
        public static void CompressedTextureSubImage3D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelInternalFormat format, Int32 imageSize, IntPtr data)
        {
            Delegates.glCompressedTextureSubImage3D(texture, level, xoffset, yoffset, zoffset, width, height, depth, format, imageSize, data);
        }

        /// <summary>
        /// Copy all or part of the data store of a buffer object to the data store of another buffer object.
        /// <para>
        /// glCopyBufferSubData and glCopyNamedBufferSubData copy part of the data store attached to a source
        /// buffer object to the data store attached to a destination buffer object. The number of basic machine
        /// units indicated by size is copied from the source at offset readOffset to the destination at
        /// writeOffset. readOffset, writeOffset and size are in terms of basic machine units.
        /// </para>
        /// </summary>
        /// <param name="readTarget">
        /// Specifies the target to which the source buffer object is bound for glCopyBufferSubData
        /// </param>
        /// <param name="writeTarget">
        /// Specifies the target to which the destination buffer object is bound for glCopyBufferSubData.
        /// </param>
        /// <param name="readOffset">
        /// Specifies the offset, in basic machine units, within the data store of the source buffer object at
        /// which data will be read.
        /// </param>
        /// <param name="writeOffset">
        /// Specifies the offset, in basic machine units, within the data store of the destination buffer object
        /// at which data will be written.
        /// </param>
        /// <param name="size">
        /// Specifies the size, in basic machine units, of the data to be copied from the source buffer object
        /// to the destination buffer object.
        /// </param>
        public static void CopyBufferSubData(BufferTarget readTarget, BufferTarget writeTarget, IntPtr readOffset, IntPtr writeOffset, IntPtr size)
        {
            Delegates.glCopyBufferSubData(readTarget, writeTarget, readOffset, writeOffset, size);
        }

        /// <summary>
        /// Copy all or part of the data store of a buffer object to the data store of another buffer object.
        /// <para>
        /// glCopyBufferSubData and glCopyNamedBufferSubData copy part of the data store attached to a source
        /// buffer object to the data store attached to a destination buffer object. The number of basic machine
        /// units indicated by size is copied from the source at offset readOffset to the destination at
        /// writeOffset. readOffset, writeOffset and size are in terms of basic machine units.
        /// </para>
        /// </summary>
        /// <param name="readBuffer">
        /// Specifies the name of the source buffer object for glCopyNamedBufferSubData.
        /// </param>
        /// <param name="writeBuffer">
        /// Specifies the name of the destination buffer object for glCopyNamedBufferSubData.
        /// </param>
        /// <param name="readOffset">
        /// Specifies the offset, in basic machine units, within the data store of the source buffer object at
        /// which data will be read.
        /// </param>
        /// <param name="writeOffset">
        /// Specifies the offset, in basic machine units, within the data store of the destination buffer object
        /// at which data will be written.
        /// </param>
        /// <param name="size">
        /// Specifies the size, in basic machine units, of the data to be copied from the source buffer object
        /// to the destination buffer object.
        /// </param>
        public static void CopyNamedBufferSubData(UInt32 readBuffer, UInt32 writeBuffer, IntPtr readOffset, IntPtr writeOffset, Int32 size)
        {
            Delegates.glCopyNamedBufferSubData(readBuffer, writeBuffer, readOffset, writeOffset, size);
        }

        /// <summary>
        /// Perform a raw data copy between two images.
        /// <para>
        /// glCopyImageSubData may be used to copy data from one image (i.e. texture or renderbuffer) to
        /// another. glCopyImageSubData does not perform general-purpose conversions such as scaling, resizing,
        /// blending, color-space, or format conversions. It should be considered to operate in a manner similar
        /// to a CPU memcpy. CopyImageSubData can copy between images with different internal formats, provided
        /// the formats are compatible.
        /// </para>
        /// </summary>
        /// <param name="srcName">
        /// The name of a texture or renderbuffer object from which to copy.
        /// </param>
        /// <param name="srcTarget">
        /// The target representing the namespace of the source name srcName.
        /// </param>
        /// <param name="srcLevel">
        /// The mipmap level to read from the source.
        /// </param>
        /// <param name="srcX">
        /// The X coordinate of the left edge of the souce region to copy.
        /// </param>
        /// <param name="srcY">
        /// The Y coordinate of the top edge of the souce region to copy.
        /// </param>
        /// <param name="srcZ">
        /// The Z coordinate of the near edge of the souce region to copy.
        /// </param>
        /// <param name="dstName">
        /// The name of a texture or renderbuffer object to which to copy.
        /// </param>
        /// <param name="dstTarget">
        /// The target representing the namespace of the destination name dstName.
        /// </param>
        /// <param name="dstLevel">
        /// </param>
        /// <param name="dstX">
        /// The X coordinate of the left edge of the destination region.
        /// </param>
        /// <param name="dstY">
        /// The Y coordinate of the top edge of the destination region.
        /// </param>
        /// <param name="dstZ">
        /// The Z coordinate of the near edge of the destination region.
        /// </param>
        /// <param name="srcWidth">
        /// The width of the region to be copied.
        /// </param>
        /// <param name="srcHeight">
        /// The height of the region to be copied.
        /// </param>
        /// <param name="srcDepth">
        /// The depth of the region to be copied.
        /// </param>
        public static void CopyImageSubData(UInt32 srcName, BufferTarget srcTarget, Int32 srcLevel, Int32 srcX, Int32 srcY, Int32 srcZ, UInt32 dstName, BufferTarget dstTarget, Int32 dstLevel, Int32 dstX, Int32 dstY, Int32 dstZ, Int32 srcWidth, Int32 srcHeight, Int32 srcDepth)
        {
            Delegates.glCopyImageSubData(srcName, srcTarget, srcLevel, srcX, srcY, srcZ, dstName, dstTarget, dstLevel, dstX, dstY, dstZ, srcWidth, srcHeight, srcDepth);
        }

        /// <summary>
        /// Copy pixels into a 1D texture image.
        /// <para>
        /// glCopyTexImage1D defines a one-dimensional texture image with pixels from the current
        /// GL_READ_BUFFER.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target texture. Must be GL_TEXTURE_1D.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format of the texture. Must be one of the following symbolic constants:
        /// GL_COMPRESSED_RED, GL_COMPRESSED_RG, GL_COMPRESSED_RGB, GL_COMPRESSED_RGBA. GL_COMPRESSED_SRGB,
        /// GL_COMPRESSED_SRGB_ALPHA. GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT24,
        /// GL_DEPTH_COMPONENT32, GL_STENCIL_INDEX8, GL_RED, GL_RG, GL_RGB, GL_R3_G3_B2, GL_RGB4, GL_RGB5,
        /// GL_RGB8, GL_RGB10, GL_RGB12, GL_RGB16, GL_RGBA, GL_RGBA2, GL_RGBA4, GL_RGB5_A1, GL_RGBA8,
        /// GL_RGB10_A2, GL_RGBA12, GL_RGBA16, GL_SRGB, GL_SRGB8, GL_SRGB_ALPHA, or GL_SRGB8_ALPHA8.
        /// </param>
        /// <param name="x">
        /// Specify the window coordinates of the left corner of the row of pixels to be copied.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the left corner of the row of pixels to be copied.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture image. The height of the texture image is 1.
        /// </param>
        /// <param name="border">
        /// Must be 0.
        /// </param>
        public static void CopyTexImage1D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 x, Int32 y, Int32 width, Int32 border)
        {
            Delegates.glCopyTexImage1D(target, level, internalFormat, x, y, width, border);
        }

        /// <summary>
        /// Copy pixels into a 2D texture image.
        /// <para>
        /// glCopyTexImage2D defines a two-dimensional texture image, or cube-map texture image with pixels from
        /// the current GL_READ_BUFFER.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target texture. Must be GL_TEXTURE_2D, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, or GL_TEXTURE_CUBE_MAP_NEGATIVE_Z.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format of the texture. Must be one of the following symbolic constants:
        /// GL_COMPRESSED_RED, GL_COMPRESSED_RG, GL_COMPRESSED_RGB, GL_COMPRESSED_RGBA. GL_COMPRESSED_SRGB,
        /// GL_COMPRESSED_SRGB_ALPHA. GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT24,
        /// GL_DEPTH_COMPONENT32, GL_STENCIL_INDEX8, GL_RED, GL_RG, GL_RGB, GL_R3_G3_B2, GL_RGB4, GL_RGB5,
        /// GL_RGB8, GL_RGB10, GL_RGB12, GL_RGB16, GL_RGBA, GL_RGBA2, GL_RGBA4, GL_RGB5_A1, GL_RGBA8,
        /// GL_RGB10_A2, GL_RGBA12, GL_RGBA16, GL_SRGB, GL_SRGB8, GL_SRGB_ALPHA, or GL_SRGB8_ALPHA8.
        /// </param>
        /// <param name="x">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture image.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture image.
        /// </param>
        /// <param name="border">
        /// Must be 0.
        /// </param>
        public static void CopyTexImage2D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 x, Int32 y, Int32 width, Int32 height, Int32 border)
        {
            Delegates.glCopyTexImage2D(target, level, internalFormat, x, y, width, height, border);
        }

        /// <summary>
        /// Copy a one-dimensional texture subimage.
        /// <para>
        /// glCopyTexSubImage1D and glCopyTextureSubImage1D replace a portion of a one-dimensional texture image
        /// with pixels from the current GL_READ_BUFFER (rather than from main memory, as is the case for
        /// glTexSubImage1D). For glCopyTexSubImage1D, the texture object that is bound to target will be used
        /// for the process. For glCopyTextureSubImage1D, texture tells which texture object should be used for
        /// the purpose of the call.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glCopyTexSubImage1D function. Must be
        /// GL_TEXTURE_1D.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies the texel offset within the texture array.
        /// </param>
        /// <param name="x">
        /// Specify the window coordinates of the left corner of the row of pixels to be copied.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the left corner of the row of pixels to be copied.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        public static void CopyTexSubImage1D(TextureTarget target, Int32 level, Int32 xoffset, Int32 x, Int32 y, Int32 width)
        {
            Delegates.glCopyTexSubImage1D(target, level, xoffset, x, y, width);
        }

        /// <summary>
        /// Copy a one-dimensional texture subimage.
        /// <para>
        /// glCopyTexSubImage1D and glCopyTextureSubImage1D replace a portion of a one-dimensional texture image
        /// with pixels from the current GL_READ_BUFFER (rather than from main memory, as is the case for
        /// glTexSubImage1D). For glCopyTexSubImage1D, the texture object that is bound to target will be used
        /// for the process. For glCopyTextureSubImage1D, texture tells which texture object should be used for
        /// the purpose of the call.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glCopyTextureSubImage1D function.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies the texel offset within the texture array.
        /// </param>
        /// <param name="x">
        /// Specify the window coordinates of the left corner of the row of pixels to be copied.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the left corner of the row of pixels to be copied.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        public static void CopyTextureSubImage1D(UInt32 texture, Int32 level, Int32 xoffset, Int32 x, Int32 y, Int32 width)
        {
            Delegates.glCopyTextureSubImage1D(texture, level, xoffset, x, y, width);
        }

        /// <summary>
        /// Copy a two-dimensional texture subimage.
        /// <para>
        /// glCopyTexSubImage2D and glCopyTextureSubImage2D replace a rectangular portion of a two-dimensional
        /// texture image, cube-map texture image, rectangular image, or a linear portion of a number of slices
        /// of a one-dimensional array texture with pixels from the current GL_READ_BUFFER (rather than from
        /// main memory, as is the case for glTexSubImage2D).
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glCopyTexSubImage2D function. Must be
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, or GL_TEXTURE_RECTANGLE.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="x">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        public static void CopyTexSubImage2D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 x, Int32 y, Int32 width, Int32 height)
        {
            Delegates.glCopyTexSubImage2D(target, level, xoffset, yoffset, x, y, width, height);
        }

        /// <summary>
        /// Copy a two-dimensional texture subimage.
        /// <para>
        /// glCopyTexSubImage2D and glCopyTextureSubImage2D replace a rectangular portion of a two-dimensional
        /// texture image, cube-map texture image, rectangular image, or a linear portion of a number of slices
        /// of a one-dimensional array texture with pixels from the current GL_READ_BUFFER (rather than from
        /// main memory, as is the case for glTexSubImage2D).
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glCopyTextureSubImage2D function.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="x">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        public static void CopyTextureSubImage2D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 x, Int32 y, Int32 width, Int32 height)
        {
            Delegates.glCopyTextureSubImage2D(texture, level, xoffset, yoffset, x, y, width, height);
        }

        /// <summary>
        /// Copy a three-dimensional texture subimage.
        /// <para>
        /// glCopyTexSubImage3D and glCopyTextureSubImage3D functions replace a rectangular portion of a
        /// three-dimensional or two-dimensional array texture image with pixels from the current GL_READ_BUFFER
        /// (rather than from main memory, as is the case for glTexSubImage3D).
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glCopyTexSubImage3D function. Must be
        /// GL_TEXTURE_3D or GL_TEXTURE_2D_ARRAY.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="zoffset">
        /// Specifies a texel offset in the z direction within the texture array.
        /// </param>
        /// <param name="x">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        public static void CopyTexSubImage3D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 x, Int32 y, Int32 width, Int32 height)
        {
            Delegates.glCopyTexSubImage3D(target, level, xoffset, yoffset, zoffset, x, y, width, height);
        }

        /// <summary>
        /// Copy a three-dimensional texture subimage.
        /// <para>
        /// glCopyTexSubImage3D and glCopyTextureSubImage3D functions replace a rectangular portion of a
        /// three-dimensional or two-dimensional array texture image with pixels from the current GL_READ_BUFFER
        /// (rather than from main memory, as is the case for glTexSubImage3D).
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glCopyTextureSubImage3D function.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="zoffset">
        /// Specifies a texel offset in the z direction within the texture array.
        /// </param>
        /// <param name="x">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the lower left corner of the rectangular region of pixels to be
        /// copied.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        public static void CopyTextureSubImage3D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 x, Int32 y, Int32 width, Int32 height)
        {
            Delegates.glCopyTextureSubImage3D(texture, level, xoffset, yoffset, zoffset, x, y, width, height);
        }

        /// <summary>
        /// Create buffer objects.
        /// <para>
        /// glCreateBuffers returns n previously unused buffer names in buffers, each representing a new buffer
        /// object initialized as if it had been bound to an unspecified target.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of buffer objects to create.
        /// </param>
        /// <param name="buffers">
        /// Specifies an array in which names of the new buffer objects are stored.
        /// </param>
        public static void CreateBuffers(Int32 n, UInt32[] buffers)
        {
            Delegates.glCreateBuffers(n, buffers);
        }

        /// <summary>
        /// Create framebuffer objects.
        /// <para>
        /// glCreateFramebuffers returns n previously unused framebuffer names in framebuffers, each
        /// representing a new framebuffer object initialized to the default state.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Number of framebuffer objects to create.
        /// </param>
        /// <param name="ids">
        /// </param>
        public static void CreateFramebuffers(Int32 n, UInt32[] ids)
        {
            Delegates.glCreateFramebuffers(n, ids);
        }

        /// <summary>
        /// Creates a program object.
        /// <para>
        /// glCreateProgram creates an empty program object and returns a non-zero value by which it can be
        /// referenced. A program object is an object to which shader objects can be attached. This provides a
        /// mechanism to specify the shader objects that will be linked to create a program. It also provides a
        /// means for checking the compatibility of the shaders that will be used to create a program (for
        /// instance, checking the compatibility between a vertex shader and a fragment shader). When no longer
        /// needed as part of a program object, shader objects can be detached.
        /// </para>
        /// </summary>
        public static UInt32 CreateProgram()
        {
            return Delegates.glCreateProgram();
        }

        /// <summary>
        /// Create program pipeline objects.
        /// <para>
        /// glCreateProgramPipelines returns n previously unused program pipeline names in pipelines, each
        /// representing a new program pipeline object initialized to the default state.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Number of program pipeline objects to create.
        /// </param>
        /// <param name="pipelines">
        /// Specifies an array in which names of the new program pipeline objects are stored.
        /// </param>
        public static void CreateProgramPipelines(Int32 n, UInt32[] pipelines)
        {
            Delegates.glCreateProgramPipelines(n, pipelines);
        }

        /// <summary>
        /// Create query objects.
        /// <para>
        /// glCreateQueries returns n previously unused query object names in ids, each representing a new query
        /// object with the specified target.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target of each created query object.
        /// </param>
        /// <param name="n">
        /// Number of query objects to create.
        /// </param>
        /// <param name="ids">
        /// Specifies an array in which names of the new query objects are stored.
        /// </param>
        public static void CreateQueries(QueryTarget target, Int32 n, UInt32[] ids)
        {
            Delegates.glCreateQueries(target, n, ids);
        }

        /// <summary>
        /// Create renderbuffer objects.
        /// <para>
        /// glCreateRenderbuffers returns n previously unused renderbuffer object names in renderbuffers, each
        /// representing a new renderbuffer object initialized to the default state.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Number of renderbuffer objects to create.
        /// </param>
        /// <param name="renderbuffers">
        /// Specifies an array in which names of the new renderbuffer objects are stored.
        /// </param>
        public static void CreateRenderbuffers(Int32 n, UInt32[] renderbuffers)
        {
            Delegates.glCreateRenderbuffers(n, renderbuffers);
        }

        /// <summary>
        /// Create sampler objects.
        /// <para>
        /// glCreateSamplers returns n previously unused sampler names in samplers, each representing a new
        /// sampler object initialized to the default state.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Number of sampler objects to create.
        /// </param>
        /// <param name="samplers">
        /// Specifies an array in which names of the new sampler objects are stored.
        /// </param>
        public static void CreateSamplers(Int32 n, UInt32[] samplers)
        {
            Delegates.glCreateSamplers(n, samplers);
        }

        /// <summary>
        /// Creates a shader object.
        /// <para>
        /// glCreateShader creates an empty shader object and returns a non-zero value by which it can be
        /// referenced. A shader object is used to maintain the source code strings that define a shader.
        /// shaderType indicates the type of shader to be created. Five types of shader are supported. A shader
        /// of type GL_COMPUTE_SHADER is a shader that is intended to run on the programmable compute processor.
        /// A shader of type GL_VERTEX_SHADER is a shader that is intended to run on the programmable vertex
        /// processor. A shader of type GL_TESS_CONTROL_SHADER is a shader that is intended to run on the
        /// programmable tessellation processor in the control stage. A shader of type GL_TESS_EVALUATION_SHADER
        /// is a shader that is intended to run on the programmable tessellation processor in the evaluation
        /// stage. A shader of type GL_GEOMETRY_SHADER is a shader that is intended to run on the programmable
        /// geometry processor. A shader of type GL_FRAGMENT_SHADER is a shader that is intended to run on the
        /// programmable fragment processor.
        /// </para>
        /// </summary>
        /// <param name="shaderType">
        /// Specifies the type of shader to be created. Must be one of GL_COMPUTE_SHADER, GL_VERTEX_SHADER,
        /// GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER, or GL_FRAGMENT_SHADER.
        /// </param>
        public static UInt32 CreateShader(ShaderType shaderType)
        {
            return Delegates.glCreateShader(shaderType);
        }

        /// <summary>
        /// Create a stand-alone program from an array of null-terminated source code strings.
        /// <para>
        /// glCreateShaderProgram creates a program object containing compiled and linked shaders for a single
        /// stage specified by type. strings refers to an array of count strings from which to create the shader
        /// executables.
        /// </para>
        /// </summary>
        /// <param name="type">
        /// Specifies the type of shader to create.
        /// </param>
        /// <param name="count">
        /// Specifies the number of source code strings in the array strings.
        /// </param>
        /// <param name="strings">
        /// Specifies the address of an array of pointers to source code strings from which to create the
        /// program object.
        /// </param>
        public static UInt32 CreateShaderProgramv(ShaderType type, Int32 count, String strings)
        {
            return Delegates.glCreateShaderProgramv(type, count, strings);
        }

        /// <summary>
        /// Create texture objects.
        /// <para>
        /// glCreateTextures returns n previously unused texture names in textures, each representing a new
        /// texture object of the dimensionality and type specified by target and initialized to the default
        /// values for that texture type.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the effective texture target of each created texture.
        /// </param>
        /// <param name="n">
        /// Number of texture objects to create.
        /// </param>
        /// <param name="textures">
        /// Specifies an array in which names of the new texture objects are stored.
        /// </param>
        public static void CreateTextures(TextureTarget target, Int32 n, UInt32[] textures)
        {
            Delegates.glCreateTextures(target, n, textures);
        }

        /// <summary>
        /// Create transform feedback objects.
        /// <para>
        /// glCreateTransformFeedbacks returns n previously unused transform feedback object names in ids, each
        /// representing a new transform feedback object initialized to the default state.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Number of transform feedback objects to create.
        /// </param>
        /// <param name="ids">
        /// Specifies an array in which names of the new transform feedback objects are stored.
        /// </param>
        public static void CreateTransformFeedbacks(Int32 n, UInt32[] ids)
        {
            Delegates.glCreateTransformFeedbacks(n, ids);
        }

        /// <summary>
        /// Create vertex array objects.
        /// <para>
        /// glCreateVertexArrays returns n previously unused vertex array object names in arrays, each
        /// representing a new vertex array object initialized to the default state.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Number of vertex array objects to create.
        /// </param>
        /// <param name="arrays">
        /// Specifies an array in which names of the new vertex array objects are stored.
        /// </param>
        public static void CreateVertexArrays(Int32 n, UInt32[] arrays)
        {
            Delegates.glCreateVertexArrays(n, arrays);
        }

        /// <summary>
        /// Specify whether front- or back-facing facets can be culled.
        /// <para>
        /// glCullFace specifies whether front- or back-facing facets are culled (as specified by mode) when
        /// facet culling is enabled. Facet culling is initially disabled. To enable and disable facet culling,
        /// call the glEnable and glDisable commands with the argument GL_CULL_FACE. Facets include triangles,
        /// quadrilaterals, polygons, and rectangles.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies whether front- or back-facing facets are candidates for culling. Symbolic constants
        /// GL_FRONT, GL_BACK, and GL_FRONT_AND_BACK are accepted. The initial value is GL_BACK.
        /// </param>
        public static void CullFace(CullFaceMode mode)
        {
            Delegates.glCullFace(mode);
        }

        /// <summary>
        /// Delete named buffer objects.
        /// <para>
        /// glDeleteBuffers deletes n buffer objects named by the elements of the array buffers. After a buffer
        /// object is deleted, it has no contents, and its name is free for reuse (for example by glGenBuffers).
        /// If a buffer object that is currently bound is deleted, the binding reverts to 0 (the absence of any
        /// buffer object).
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of buffer objects to be deleted.
        /// </param>
        /// <param name="buffers">
        /// Specifies an array of buffer objects to be deleted.
        /// </param>
        public static void DeleteBuffers(Int32 n, UInt32[] buffers)
        {
            Delegates.glDeleteBuffers(n, buffers);
        }

        /// <summary>
        /// Delete framebuffer objects.
        /// <para>
        /// glDeleteFramebuffers deletes the n framebuffer objects whose names are stored in the array addressed
        /// by framebuffers. The name zero is reserved by the GL and is silently ignored, should it occur in
        /// framebuffers, as are other unused names. Once a framebuffer object is deleted, its name is again
        /// unused and it has no attachments. If a framebuffer that is currently bound to one or more of the
        /// targets GL_DRAW_FRAMEBUFFER or GL_READ_FRAMEBUFFER is deleted, it is as though glBindFramebuffer had
        /// been executed with the corresponding target and framebuffer zero.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of framebuffer objects to be deleted.
        /// </param>
        /// <param name="framebuffers">
        /// A pointer to an array containing n framebuffer objects to be deleted.
        /// </param>
        public static void DeleteFramebuffers(Int32 n, UInt32[] framebuffers)
        {
            Delegates.glDeleteFramebuffers(n, framebuffers);
        }

        /// <summary>
        /// Deletes a program object.
        /// <para>
        /// glDeleteProgram frees the memory and invalidates the name associated with the program object
        /// specified by program. This command effectively undoes the effects of a call to glCreateProgram.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be deleted.
        /// </param>
        public static void DeleteProgram(UInt32 program)
        {
            Delegates.glDeleteProgram(program);
        }

        /// <summary>
        /// Delete program pipeline objects.
        /// <para>
        /// glDeleteProgramPipelines deletes the n program pipeline objects whose names are stored in the array
        /// pipelines. Unused names in pipelines are ignored, as is the name zero. After a program pipeline
        /// object is deleted, its name is again unused and it has no contents. If program pipeline object that
        /// is currently bound is deleted, the binding for that object reverts to zero and no program pipeline
        /// object becomes current.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of program pipeline objects to delete.
        /// </param>
        /// <param name="pipelines">
        /// Specifies an array of names of program pipeline objects to delete.
        /// </param>
        public static void DeleteProgramPipelines(Int32 n, UInt32[] pipelines)
        {
            Delegates.glDeleteProgramPipelines(n, pipelines);
        }

        /// <summary>
        /// Delete named query objects.
        /// <para>
        /// glDeleteQueries deletes n query objects named by the elements of the array ids. After a query object
        /// is deleted, it has no contents, and its name is free for reuse (for example by glGenQueries).
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of query objects to be deleted.
        /// </param>
        /// <param name="ids">
        /// Specifies an array of query objects to be deleted.
        /// </param>
        public static void DeleteQueries(Int32 n, UInt32[] ids)
        {
            Delegates.glDeleteQueries(n, ids);
        }

        /// <summary>
        /// Delete renderbuffer objects.
        /// <para>
        /// glDeleteRenderbuffers deletes the n renderbuffer objects whose names are stored in the array
        /// addressed by renderbuffers. The name zero is reserved by the GL and is silently ignored, should it
        /// occur in renderbuffers, as are other unused names. Once a renderbuffer object is deleted, its name
        /// is again unused and it has no contents. If a renderbuffer that is currently bound to the target
        /// GL_RENDERBUFFER is deleted, it is as though glBindRenderbuffer had been executed with a target of
        /// GL_RENDERBUFFER and a name of zero.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of renderbuffer objects to be deleted.
        /// </param>
        /// <param name="renderbuffers">
        /// A pointer to an array containing n renderbuffer objects to be deleted.
        /// </param>
        public static void DeleteRenderbuffers(Int32 n, UInt32[] renderbuffers)
        {
            Delegates.glDeleteRenderbuffers(n, renderbuffers);
        }

        /// <summary>
        /// Delete named sampler objects.
        /// <para>
        /// glDeleteSamplers deletes n sampler objects named by the elements of the array samplers. After a
        /// sampler object is deleted, its name is again unused. If a sampler object that is currently bound to
        /// a sampler unit is deleted, it is as though glBindSampler is called with unit set to the unit the
        /// sampler is bound to and sampler zero. Unused names in samplers are silently ignored, as is the
        /// reserved name zero.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of sampler objects to be deleted.
        /// </param>
        /// <param name="samplers">
        /// Specifies an array of sampler objects to be deleted.
        /// </param>
        public static void DeleteSamplers(Int32 n, UInt32[] samplers)
        {
            Delegates.glDeleteSamplers(n, samplers);
        }

        /// <summary>
        /// Deletes a shader object.
        /// <para>
        /// glDeleteShader frees the memory and invalidates the name associated with the shader object specified
        /// by shader. This command effectively undoes the effects of a call to glCreateShader.
        /// </para>
        /// </summary>
        /// <param name="shader">
        /// Specifies the shader object to be deleted.
        /// </param>
        public static void DeleteShader(UInt32 shader)
        {
            Delegates.glDeleteShader(shader);
        }

        /// <summary>
        /// Delete a sync object.
        /// <para>
        /// glDeleteSync deletes the sync object specified by sync. If the fence command corresponding to the
        /// specified sync object has completed, or if no glWaitSync or glClientWaitSync commands are blocking
        /// on sync, the object is deleted immediately. Otherwise, sync is flagged for deletion and will be
        /// deleted when it is no longer associated with any fence command and is no longer blocking any
        /// glWaitSync or glClientWaitSync command. In either case, after glDeleteSync returns, the name sync is
        /// invalid and can no longer be used to refer to the sync object.
        /// </para>
        /// </summary>
        /// <param name="sync">
        /// The sync object to be deleted.
        /// </param>
        public static void DeleteSync(IntPtr sync)
        {
            Delegates.glDeleteSync(sync);
        }

        /// <summary>
        /// Delete named textures.
        /// <para>
        /// glDeleteTextures deletes n textures named by the elements of the array textures. After a texture is
        /// deleted, it has no contents or dimensionality, and its name is free for reuse (for example by
        /// glGenTextures). If a texture that is currently bound is deleted, the binding reverts to 0 (the
        /// default texture).
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of textures to be deleted.
        /// </param>
        /// <param name="textures">
        /// Specifies an array of textures to be deleted.
        /// </param>
        public static void DeleteTextures(Int32 n, UInt32[] textures)
        {
            Delegates.glDeleteTextures(n, textures);
        }

        /// <summary>
        /// Delete transform feedback objects.
        /// <para>
        /// glDeleteTransformFeedbacks deletes the n transform feedback objects whose names are stored in the
        /// array ids. Unused names in ids are ignored, as is the name zero. After a transform feedback object
        /// is deleted, its name is again unused and it has no contents. If an active transform feedback object
        /// is deleted, its name immediately becomes unused, but the underlying object is not deleted until it
        /// is no longer active.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of transform feedback objects to delete.
        /// </param>
        /// <param name="ids">
        /// Specifies an array of names of transform feedback objects to delete.
        /// </param>
        public static void DeleteTransformFeedbacks(Int32 n, UInt32[] ids)
        {
            Delegates.glDeleteTransformFeedbacks(n, ids);
        }

        /// <summary>
        /// Delete vertex array objects.
        /// <para>
        /// glDeleteVertexArrays deletes n vertex array objects whose names are stored in the array addressed by
        /// arrays. Once a vertex array object is deleted it has no contents and its name is again unused. If a
        /// vertex array object that is currently bound is deleted, the binding for that object reverts to zero
        /// and the default vertex array becomes current. Unused names in arrays are silently ignored, as is the
        /// value zero.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of vertex array objects to be deleted.
        /// </param>
        /// <param name="arrays">
        /// Specifies the address of an array containing the n names of the objects to be deleted.
        /// </param>
        public static void DeleteVertexArrays(Int32 n, UInt32[] arrays)
        {
            Delegates.glDeleteVertexArrays(n, arrays);
        }

        /// <summary>
        /// Specify the value used for depth buffer comparisons.
        /// <para>
        /// glDepthFunc specifies the function used to compare each incoming pixel depth value with the depth
        /// value present in the depth buffer. The comparison is performed only if depth testing is enabled.
        /// (See glEnable and glDisable of GL_DEPTH_TEST.).
        /// </para>
        /// </summary>
        /// <param name="func">
        /// Specifies the depth comparison function. Symbolic constants GL_NEVER, GL_LESS, GL_EQUAL, GL_LEQUAL,
        /// GL_GREATER, GL_NOTEQUAL, GL_GEQUAL, and GL_ALWAYS are accepted. The initial value is GL_LESS.
        /// </param>
        public static void DepthFunc(DepthFunction func)
        {
            Delegates.glDepthFunc(func);
        }

        /// <summary>
        /// Enable or disable writing into the depth buffer.
        /// <para>
        /// glDepthMask specifies whether the depth buffer is enabled for writing. If flag is GL_FALSE, depth
        /// buffer writing is disabled. Otherwise, it is enabled. Initially, depth buffer writing is enabled.
        /// </para>
        /// </summary>
        /// <param name="flag">
        /// Specifies whether the depth buffer is enabled for writing. If flag is GL_FALSE, depth buffer writing
        /// is disabled. Otherwise, it is enabled. Initially, depth buffer writing is enabled.
        /// </param>
        public static void DepthMask(Boolean flag)
        {
            Delegates.glDepthMask(flag);
        }

        /// <summary>
        /// Specify mapping of depth values from normalized device coordinates to window coordinates.
        /// <para>
        /// After clipping and division by w, depth coordinates range from -1 to 1, corresponding to the near
        /// and far clipping planes. glDepthRange specifies a linear mapping of the normalized depth coordinates
        /// in this range to window depth coordinates. Regardless of the actual depth buffer implementation,
        /// window coordinate depth values are treated as though they range from 0 through 1 (like color
        /// components). Thus, the values accepted by glDepthRange are both clamped to this range before they
        /// are accepted.
        /// </para>
        /// </summary>
        /// <param name="nearVal">
        /// Specifies the mapping of the near clipping plane to window coordinates. The initial value is 0.
        /// </param>
        /// <param name="farVal">
        /// Specifies the mapping of the far clipping plane to window coordinates. The initial value is 1.
        /// </param>
        public static void DepthRange(Double nearVal, Double farVal)
        {
            Delegates.glDepthRange(nearVal, farVal);
        }

        /// <summary>
        /// Specify mapping of depth values from normalized device coordinates to window coordinates.
        /// <para>
        /// After clipping and division by w, depth coordinates range from -1 to 1, corresponding to the near
        /// and far clipping planes. glDepthRange specifies a linear mapping of the normalized depth coordinates
        /// in this range to window depth coordinates. Regardless of the actual depth buffer implementation,
        /// window coordinate depth values are treated as though they range from 0 through 1 (like color
        /// components). Thus, the values accepted by glDepthRange are both clamped to this range before they
        /// are accepted.
        /// </para>
        /// </summary>
        /// <param name="nearVal">
        /// Specifies the mapping of the near clipping plane to window coordinates. The initial value is 0.
        /// </param>
        /// <param name="farVal">
        /// Specifies the mapping of the far clipping plane to window coordinates. The initial value is 1.
        /// </param>
        public static void DepthRangef(Single nearVal, Single farVal)
        {
            Delegates.glDepthRangef(nearVal, farVal);
        }

        /// <summary>
        /// Specify mapping of depth values from normalized device coordinates to window coordinates for a specified set of viewports.
        /// <para>
        /// After clipping and division by w, depth coordinates range from -1 to 1, corresponding to the near
        /// and far clipping planes. Each viewport has an independent depth range specified as a linear mapping
        /// of the normalized depth coordinates in this range to window depth coordinates. Regardless of the
        /// actual depth buffer implementation, window coordinate depth values are treated as though they range
        /// from 0 through 1 (like color components). glDepthRangeArray specifies a linear mapping of the
        /// normalized depth coordinates in this range to window depth coordinates for each viewport in the
        /// range [first, first + count). Thus, the values accepted by glDepthRangeArray are both clamped to
        /// this range before they are accepted.
        /// </para>
        /// </summary>
        /// <param name="first">
        /// Specifies the index of the first viewport whose depth range to update.
        /// </param>
        /// <param name="count">
        /// Specifies the number of viewports whose depth range to update.
        /// </param>
        /// <param name="v">
        /// Specifies the address of an array containing the near and far values for the depth range of each
        /// modified viewport.
        /// </param>
        public static void DepthRangeArrayv(UInt32 first, Int32 count, Double[] v)
        {
            Delegates.glDepthRangeArrayv(first, count, v);
        }

        /// <summary>
        /// Specify mapping of depth values from normalized device coordinates to window coordinates for a specified viewport.
        /// <para>
        /// After clipping and division by w, depth coordinates range from -1 to 1, corresponding to the near
        /// and far clipping planes. Each viewport has an independent depth range specified as a linear mapping
        /// of the normalized depth coordinates in this range to window depth coordinates. Regardless of the
        /// actual depth buffer implementation, window coordinate depth values are treated as though they range
        /// from 0 through 1 (like color components). glDepthRangeIndexed specifies a linear mapping of the
        /// normalized depth coordinates in this range to window depth coordinates for a specified viewport.
        /// Thus, the values accepted by glDepthRangeIndexed are both clamped to this range before they are
        /// accepted.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the viewport whose depth range to update.
        /// </param>
        /// <param name="nearVal">
        /// Specifies the mapping of the near clipping plane to window coordinates. The initial value is 0.
        /// </param>
        /// <param name="farVal">
        /// Specifies the mapping of the far clipping plane to window coordinates. The initial value is 1.
        /// </param>
        public static void DepthRangeIndexed(UInt32 index, Double nearVal, Double farVal)
        {
            Delegates.glDepthRangeIndexed(index, nearVal, farVal);
        }

        /// <summary>
        /// Detaches a shader object from a program object to which it is attached.
        /// <para>
        /// glDetachShader detaches the shader object specified by shader from the program object specified by
        /// program. This command can be used to undo the effect of the command glAttachShader.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object from which to detach the shader object.
        /// </param>
        /// <param name="shader">
        /// Specifies the shader object to be detached.
        /// </param>
        public static void DetachShader(UInt32 program, UInt32 shader)
        {
            Delegates.glDetachShader(program, shader);
        }

        /// <summary>
        /// Launch one or more compute work groups.
        /// <para>
        /// glDispatchCompute launches one or more compute work groups. Each work group is processed by the
        /// active program object for the compute shader stage. While the individual shader invocations within a
        /// work group are executed as a unit, work groups are executed completely independently and in
        /// unspecified order. num_groups_x, num_groups_y and num_groups_z specify the number of local work
        /// groups that will be dispatched in the X, Y and Z dimensions, respectively.
        /// </para>
        /// </summary>
        /// <param name="num_groups_x">
        /// The number of work groups to be launched in the X dimension.
        /// </param>
        /// <param name="num_groups_y">
        /// The number of work groups to be launched in the Y dimension.
        /// </param>
        /// <param name="num_groups_z">
        /// The number of work groups to be launched in the Z dimension.
        /// </param>
        public static void DispatchCompute(UInt32 num_groups_x, UInt32 num_groups_y, UInt32 num_groups_z)
        {
            Delegates.glDispatchCompute(num_groups_x, num_groups_y, num_groups_z);
        }

        /// <summary>
        /// Launch one or more compute work groups using parameters stored in a buffer.
        /// <para>
        /// glDispatchComputeIndirect launches one or more compute work groups using parameters stored in the
        /// buffer object currently bound to the GL_DISPATCH_INDIRECT_BUFFER target. Each work group is
        /// processed by the active program object for the compute shader stage. While the individual shader
        /// invocations within a work group are executed as a unit, work groups are executed completely
        /// independently and in unspecified order. indirect contains the offset into the data store of the
        /// buffer object bound to the GL_DISPATCH_INDIRECT_BUFFER target at which the parameters are stored.
        /// </para>
        /// </summary>
        /// <param name="indirect">
        /// The offset into the buffer object currently bound to the GL_DISPATCH_INDIRECT_BUFFER buffer target
        /// at which the dispatch parameters are stored.
        /// </param>
        public static void DispatchComputeIndirect(IntPtr indirect)
        {
            Delegates.glDispatchComputeIndirect(indirect);
        }

        /// <summary>
        /// Render primitives from array data.
        /// <para>
        /// glDrawArrays specifies multiple geometric primitives with very few subroutine calls. Instead of
        /// calling a GL procedure to pass each individual vertex, normal, texture coordinate, edge flag, or
        /// color, you can prespecify separate arrays of vertices, normals, and colors and use them to construct
        /// a sequence of primitives with a single call to glDrawArrays.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="first">
        /// Specifies the starting index in the enabled arrays.
        /// </param>
        /// <param name="count">
        /// Specifies the number of indices to be rendered.
        /// </param>
        public static void DrawArrays(BeginMode mode, Int32 first, Int32 count)
        {
            Delegates.glDrawArrays(mode, first, count);
        }

        /// <summary>
        /// Render primitives from array data, taking parameters from memory.
        /// <para>
        /// glDrawArraysIndirect specifies multiple geometric primitives with very few subroutine calls.
        /// glDrawArraysIndirect behaves similarly to glDrawArraysInstancedBaseInstance, execept that the
        /// parameters to glDrawArraysInstancedBaseInstance are stored in memory at the address given by
        /// indirect.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="indirect">
        /// Specifies the address of a structure containing the draw parameters.
        /// </param>
        public static void DrawArraysIndirect(BeginMode mode, IntPtr indirect)
        {
            Delegates.glDrawArraysIndirect(mode, indirect);
        }

        /// <summary>
        /// Draw multiple instances of a range of elements.
        /// <para>
        /// glDrawArraysInstanced behaves identically to glDrawArrays except that primcount instances of the
        /// range of elements are executed and the value of the internal counter instanceID advances for each
        /// iteration. instanceID is an internal 32-bit integer counter that may be read by a vertex shader as
        /// gl_InstanceID.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES GL_LINES_ADJACENCY,
        /// GL_LINE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY and GL_PATCHES are
        /// accepted.
        /// </param>
        /// <param name="first">
        /// Specifies the starting index in the enabled arrays.
        /// </param>
        /// <param name="count">
        /// Specifies the number of indices to be rendered.
        /// </param>
        /// <param name="primcount">
        /// Specifies the number of instances of the specified range of indices to be rendered.
        /// </param>
        public static void DrawArraysInstanced(BeginMode mode, Int32 first, Int32 count, Int32 primcount)
        {
            Delegates.glDrawArraysInstanced(mode, first, count, primcount);
        }

        /// <summary>
        /// Draw multiple instances of a range of elements with offset applied to instanced attributes.
        /// <para>
        /// glDrawArraysInstancedBaseInstance behaves identically to glDrawArrays except that primcount
        /// instances of the range of elements are executed and the value of the internal counter instanceID
        /// advances for each iteration. instanceID is an internal 32-bit integer counter that may be read by a
        /// vertex shader as gl_InstanceID.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES GL_LINES_ADJACENCY,
        /// GL_LINE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY and GL_PATCHES are
        /// accepted.
        /// </param>
        /// <param name="first">
        /// Specifies the starting index in the enabled arrays.
        /// </param>
        /// <param name="count">
        /// Specifies the number of indices to be rendered.
        /// </param>
        /// <param name="primcount">
        /// Specifies the number of instances of the specified range of indices to be rendered.
        /// </param>
        /// <param name="baseinstance">
        /// Specifies the base instance for use in fetching instanced vertex attributes.
        /// </param>
        public static void DrawArraysInstancedBaseInstance(BeginMode mode, Int32 first, Int32 count, Int32 primcount, UInt32 baseinstance)
        {
            Delegates.glDrawArraysInstancedBaseInstance(mode, first, count, primcount, baseinstance);
        }

        /// <summary>
        /// Specify which color buffers are to be drawn into.
        /// <para>
        /// When colors are written to the frame buffer, they are written into the color buffers specified by
        /// glDrawBuffer. One of the following values can be used for default framebuffer:.
        /// </para>
        /// </summary>
        /// <param name="buf">
        /// For default framebuffer, the argument specifies up to four color buffers to be drawn into. Symbolic
        /// constants GL_NONE, GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT, GL_BACK_RIGHT, GL_FRONT, GL_BACK,
        /// GL_LEFT, GL_RIGHT, and GL_FRONT_AND_BACK are accepted. The initial value is GL_FRONT for
        /// single-buffered contexts, and GL_BACK for double-buffered contexts. For framebuffer objects,
        /// GL_COLOR_ATTACHMENT$m$ and GL_NONE enums are accepted, where $m$ is a value between 0 and
        /// GL_MAX_COLOR_ATTACHMENTS.
        /// </param>
        public static void DrawBuffer(DrawBufferMode buf)
        {
            Delegates.glDrawBuffer(buf);
        }

        /// <summary>
        /// Specify which color buffers are to be drawn into.
        /// <para>
        /// When colors are written to the frame buffer, they are written into the color buffers specified by
        /// glDrawBuffer. One of the following values can be used for default framebuffer:.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glNamedFramebufferDrawBuffer function. Must be zero
        /// or the name of a framebuffer object.
        /// </param>
        /// <param name="buf">
        /// For default framebuffer, the argument specifies up to four color buffers to be drawn into. Symbolic
        /// constants GL_NONE, GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT, GL_BACK_RIGHT, GL_FRONT, GL_BACK,
        /// GL_LEFT, GL_RIGHT, and GL_FRONT_AND_BACK are accepted. The initial value is GL_FRONT for
        /// single-buffered contexts, and GL_BACK for double-buffered contexts. For framebuffer objects,
        /// GL_COLOR_ATTACHMENT$m$ and GL_NONE enums are accepted, where $m$ is a value between 0 and
        /// GL_MAX_COLOR_ATTACHMENTS.
        /// </param>
        public static void NamedFramebufferDrawBuffer(UInt32 framebuffer, DrawBufferMode buf)
        {
            Delegates.glNamedFramebufferDrawBuffer(framebuffer, buf);
        }

        /// <summary>
        /// Specifies a list of color buffers to be drawn into.
        /// <para>
        /// glDrawBuffers and glNamedFramebufferDrawBuffers define an array of buffers into which outputs from
        /// the fragment shader data will be written. If a fragment shader writes a value to one or more user
        /// defined output variables, then the value of each variable will be written into the buffer specified
        /// at a location within bufs corresponding to the location assigned to that user defined output. The
        /// draw buffer used for user defined outputs assigned to locations greater than or equal to n is
        /// implicitly set to GL_NONE and any data written to such an output is discarded.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of buffers in bufs.
        /// </param>
        /// <param name="bufs">
        /// Points to an array of symbolic constants specifying the buffers into which fragment colors or data
        /// values will be written.
        /// </param>
        public static void DrawBuffers(Int32 n, DrawBuffersEnum[] bufs)
        {
            Delegates.glDrawBuffers(n, bufs);
        }

        /// <summary>
        /// Specifies a list of color buffers to be drawn into.
        /// <para>
        /// glDrawBuffers and glNamedFramebufferDrawBuffers define an array of buffers into which outputs from
        /// the fragment shader data will be written. If a fragment shader writes a value to one or more user
        /// defined output variables, then the value of each variable will be written into the buffer specified
        /// at a location within bufs corresponding to the location assigned to that user defined output. The
        /// draw buffer used for user defined outputs assigned to locations greater than or equal to n is
        /// implicitly set to GL_NONE and any data written to such an output is discarded.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glNamedFramebufferDrawBuffers.
        /// </param>
        /// <param name="n">
        /// Specifies the number of buffers in bufs.
        /// </param>
        /// <param name="bufs">
        /// Points to an array of symbolic constants specifying the buffers into which fragment colors or data
        /// values will be written.
        /// </param>
        public static void NamedFramebufferDrawBuffers(UInt32 framebuffer, Int32 n, DrawBufferMode[] bufs)
        {
            Delegates.glNamedFramebufferDrawBuffers(framebuffer, n, bufs);
        }

        /// <summary>
        /// Render primitives from array data.
        /// <para>
        /// glDrawElements specifies multiple geometric primitives with very few subroutine calls. Instead of
        /// calling a GL function to pass each individual vertex, normal, texture coordinate, edge flag, or
        /// color, you can prespecify separate arrays of vertices, normals, and so on, and use them to construct
        /// a sequence of primitives with a single call to glDrawElements.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements to be rendered.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        public static void DrawElements(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices)
        {
            Delegates.glDrawElements(mode, count, type, indices);
        }

        /// <summary>
        /// Render primitives from array data with a per-element offset.
        /// <para>
        /// glDrawElementsBaseVertex behaves identically to glDrawElements except that the ith element
        /// transferred by the corresponding draw call will be taken from element indices[i] + basevertex of
        /// each enabled array. If the resulting value is larger than the maximum value representable by type,
        /// it is as if the calculation were upconverted to 32-bit unsigned integers (with wrapping on overflow
        /// conditions). The operation is undefined if the sum would be negative.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES, GL_LINES_ADJACENCY,
        /// GL_LINE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY and GL_PATCHES are
        /// accepted.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements to be rendered.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        /// <param name="basevertex">
        /// Specifies a constant that should be added to each element of indices when chosing elements from the
        /// enabled vertex arrays.
        /// </param>
        public static void DrawElementsBaseVertex(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 basevertex)
        {
            Delegates.glDrawElementsBaseVertex(mode, count, type, indices, basevertex);
        }

        /// <summary>
        /// Render indexed primitives from array data, taking parameters from memory.
        /// <para>
        /// glDrawElementsIndirect specifies multiple indexed geometric primitives with very few subroutine
        /// calls. glDrawElementsIndirect behaves similarly to glDrawElementsInstancedBaseVertexBaseInstance,
        /// execpt that the parameters to glDrawElementsInstancedBaseVertexBaseInstance are stored in memory at
        /// the address given by indirect.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="type">
        /// Specifies the type of data in the buffer bound to the GL_ELEMENT_ARRAY_BUFFER binding.
        /// </param>
        /// <param name="indirect">
        /// Specifies the address of a structure containing the draw parameters.
        /// </param>
        public static void DrawElementsIndirect(BeginMode mode, DrawElementsType type, IntPtr indirect)
        {
            Delegates.glDrawElementsIndirect(mode, type, indirect);
        }

        /// <summary>
        /// Draw multiple instances of a set of elements.
        /// <para>
        /// glDrawElementsInstanced behaves identically to glDrawElements except that primcount instances of the
        /// set of elements are executed and the value of the internal counter instanceID advances for each
        /// iteration. instanceID is an internal 32-bit integer counter that may be read by a vertex shader as
        /// gl_InstanceID.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements to be rendered.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        /// <param name="primcount">
        /// Specifies the number of instances of the specified range of indices to be rendered.
        /// </param>
        public static void DrawElementsInstanced(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 primcount)
        {
            Delegates.glDrawElementsInstanced(mode, count, type, indices, primcount);
        }

        /// <summary>
        /// Draw multiple instances of a set of elements with offset applied to instanced attributes.
        /// <para>
        /// glDrawElementsInstancedBaseInstance behaves identically to glDrawElements except that primcount
        /// instances of the set of elements are executed and the value of the internal counter instanceID
        /// advances for each iteration. instanceID is an internal 32-bit integer counter that may be read by a
        /// vertex shader as gl_InstanceID.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements to be rendered.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        /// <param name="primcount">
        /// Specifies the number of instances of the specified range of indices to be rendered.
        /// </param>
        /// <param name="baseinstance">
        /// Specifies the base instance for use in fetching instanced vertex attributes.
        /// </param>
        public static void DrawElementsInstancedBaseInstance(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 primcount, UInt32 baseinstance)
        {
            Delegates.glDrawElementsInstancedBaseInstance(mode, count, type, indices, primcount, baseinstance);
        }

        /// <summary>
        /// Render multiple instances of a set of primitives from array data with a per-element offset.
        /// <para>
        /// glDrawElementsInstancedBaseVertex behaves identically to glDrawElementsInstanced except that the ith
        /// element transferred by the corresponding draw call will be taken from element indices[i] +
        /// basevertex of each enabled array. If the resulting value is larger than the maximum value
        /// representable by type, it is as if the calculation were upconverted to 32-bit unsigned integers
        /// (with wrapping on overflow conditions). The operation is undefined if the sum would be negative.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES, GL_LINES_ADJACENCY,
        /// GL_LINE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY and GL_PATCHES are
        /// accepted.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements to be rendered.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        /// <param name="primcount">
        /// Specifies the number of instances of the indexed geometry that should be drawn.
        /// </param>
        /// <param name="basevertex">
        /// Specifies a constant that should be added to each element of indices when chosing elements from the
        /// enabled vertex arrays.
        /// </param>
        public static void DrawElementsInstancedBaseVertex(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 primcount, Int32 basevertex)
        {
            Delegates.glDrawElementsInstancedBaseVertex(mode, count, type, indices, primcount, basevertex);
        }

        /// <summary>
        /// Render multiple instances of a set of primitives from array data with a per-element offset.
        /// <para>
        /// glDrawElementsInstancedBaseVertexBaseInstance behaves identically to glDrawElementsInstanced except
        /// that the ith element transferred by the corresponding draw call will be taken from element
        /// indices[i] + basevertex of each enabled array. If the resulting value is larger than the maximum
        /// value representable by type, it is as if the calculation were upconverted to 32-bit unsigned
        /// integers (with wrapping on overflow conditions). The operation is undefined if the sum would be
        /// negative.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES, GL_LINES_ADJACENCY,
        /// GL_LINE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY and GL_PATCHES are
        /// accepted.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements to be rendered.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        /// <param name="primcount">
        /// Specifies the number of instances of the indexed geometry that should be drawn.
        /// </param>
        /// <param name="basevertex">
        /// Specifies a constant that should be added to each element of indices when chosing elements from the
        /// enabled vertex arrays.
        /// </param>
        /// <param name="baseinstance">
        /// Specifies the base instance for use in fetching instanced vertex attributes.
        /// </param>
        public static void DrawElementsInstancedBaseVertexBaseInstance(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices, Int32 primcount, Int32 basevertex, UInt32 baseinstance)
        {
            Delegates.glDrawElementsInstancedBaseVertexBaseInstance(mode, count, type, indices, primcount, basevertex, baseinstance);
        }

        /// <summary>
        /// Render primitives from array data.
        /// <para>
        /// glDrawRangeElements is a restricted form of glDrawElements. mode, and count match the corresponding
        /// arguments to glDrawElements, with the additional constraint that all values in the arrays count must
        /// lie between start and end, inclusive.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="start">
        /// Specifies the minimum array index contained in indices.
        /// </param>
        /// <param name="end">
        /// Specifies the maximum array index contained in indices.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements to be rendered.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        public static void DrawRangeElements(BeginMode mode, UInt32 start, UInt32 end, Int32 count, DrawElementsType type, IntPtr indices)
        {
            Delegates.glDrawRangeElements(mode, start, end, count, type, indices);
        }

        /// <summary>
        /// Render primitives from array data with a per-element offset.
        /// <para>
        /// glDrawRangeElementsBaseVertex is a restricted form of glDrawElementsBaseVertex. mode, count and
        /// basevertex match the corresponding arguments to glDrawElementsBaseVertex, with the additional
        /// constraint that all values in the array indices must lie between start and end, inclusive, prior to
        /// adding basevertex. Index values lying outside the range [start, end] are treated in the same way as
        /// glDrawElementsBaseVertex. The ith element transferred by the corresponding draw call will be taken
        /// from element indices[i] + basevertex of each enabled array. If the resulting value is larger than
        /// the maximum value representable by type, it is as if the calculation were upconverted to 32-bit
        /// unsigned integers (with wrapping on overflow conditions). The operation is undefined if the sum
        /// would be negative.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES, GL_LINES_ADJACENCY,
        /// GL_LINE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY and GL_PATCHES are
        /// accepted.
        /// </param>
        /// <param name="start">
        /// Specifies the minimum array index contained in indices.
        /// </param>
        /// <param name="end">
        /// Specifies the maximum array index contained in indices.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements to be rendered.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        /// <param name="basevertex">
        /// Specifies a constant that should be added to each element of indices when chosing elements from the
        /// enabled vertex arrays.
        /// </param>
        public static void DrawRangeElementsBaseVertex(BeginMode mode, UInt32 start, UInt32 end, Int32 count, DrawElementsType type, IntPtr indices, Int32 basevertex)
        {
            Delegates.glDrawRangeElementsBaseVertex(mode, start, end, count, type, indices, basevertex);
        }

        /// <summary>
        /// Render primitives using a count derived from a transform feedback object.
        /// <para>
        /// glDrawTransformFeedback draws primitives of a type specified by mode using a count retrieved from
        /// the transform feedback specified by id. Calling glDrawTransformFeedback is equivalent to calling
        /// glDrawArrays with mode as specified, first set to zero, and count set to the number of vertices
        /// captured on vertex stream zero the last time transform feedback was active on the transform feedback
        /// object named by id.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="id">
        /// Specifies the name of a transform feedback object from which to retrieve a primitive count.
        /// </param>
        public static void DrawTransformFeedback(NvTransformFeedback2 mode, UInt32 id)
        {
            Delegates.glDrawTransformFeedback(mode, id);
        }

        /// <summary>
        /// Render multiple instances of primitives using a count derived from a transform feedback object.
        /// <para>
        /// glDrawTransformFeedbackInstanced draws multiple copies of a range of primitives of a type specified
        /// by mode using a count retrieved from the transform feedback stream specified by stream of the
        /// transform feedback object specified by id. Calling glDrawTransformFeedbackInstanced is equivalent to
        /// calling glDrawArraysInstanced with mode and primcount as specified, first set to zero, and count set
        /// to the number of vertices captured on vertex stream zero the last time transform feedback was active
        /// on the transform feedback object named by id.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="id">
        /// Specifies the name of a transform feedback object from which to retrieve a primitive count.
        /// </param>
        /// <param name="primcount">
        /// Specifies the number of instances of the geometry to render.
        /// </param>
        public static void DrawTransformFeedbackInstanced(BeginMode mode, UInt32 id, Int32 primcount)
        {
            Delegates.glDrawTransformFeedbackInstanced(mode, id, primcount);
        }

        /// <summary>
        /// Render primitives using a count derived from a specifed stream of a transform feedback object.
        /// <para>
        /// glDrawTransformFeedbackStream draws primitives of a type specified by mode using a count retrieved
        /// from the transform feedback stream specified by stream of the transform feedback object specified by
        /// id. Calling glDrawTransformFeedbackStream is equivalent to calling glDrawArrays with mode as
        /// specified, first set to zero, and count set to the number of vertices captured on vertex stream
        /// stream the last time transform feedback was active on the transform feedback object named by id.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="id">
        /// Specifies the name of a transform feedback object from which to retrieve a primitive count.
        /// </param>
        /// <param name="stream">
        /// Specifies the index of the transform feedback stream from which to retrieve a primitive count.
        /// </param>
        public static void DrawTransformFeedbackStream(NvTransformFeedback2 mode, UInt32 id, UInt32 stream)
        {
            Delegates.glDrawTransformFeedbackStream(mode, id, stream);
        }

        /// <summary>
        /// Render multiple instances of primitives using a count derived from a specifed stream of a transform feedback object.
        /// <para>
        /// glDrawTransformFeedbackStreamInstanced draws multiple copies of a range of primitives of a type
        /// specified by mode using a count retrieved from the transform feedback stream specified by stream of
        /// the transform feedback object specified by id. Calling glDrawTransformFeedbackStreamInstanced is
        /// equivalent to calling glDrawArraysInstanced with mode and primcount as specified, first set to zero,
        /// and count set to the number of vertices captured on vertex stream stream the last time transform
        /// feedback was active on the transform feedback object named by id.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="id">
        /// Specifies the name of a transform feedback object from which to retrieve a primitive count.
        /// </param>
        /// <param name="stream">
        /// Specifies the index of the transform feedback stream from which to retrieve a primitive count.
        /// </param>
        /// <param name="primcount">
        /// Specifies the number of instances of the geometry to render.
        /// </param>
        public static void DrawTransformFeedbackStreamInstanced(BeginMode mode, UInt32 id, UInt32 stream, Int32 primcount)
        {
            Delegates.glDrawTransformFeedbackStreamInstanced(mode, id, stream, primcount);
        }

        /// <summary>
        /// Enable or disable server-side GL capabilities.
        /// <para>
        /// glEnable and glDisable enable and disable various capabilities. Use glIsEnabled or glGet to
        /// determine the current setting of any capability. The initial value for each capability with the
        /// exception of GL_DITHER and GL_MULTISAMPLE is GL_FALSE. The initial value for GL_DITHER and
        /// GL_MULTISAMPLE is GL_TRUE.
        /// </para>
        /// </summary>
        /// <param name="cap">
        /// Specifies a symbolic constant indicating a GL capability.
        /// </param>
        public static void Enable(EnableCap cap)
        {
            Delegates.glEnable(cap);
        }

        /// <summary>
        /// Enable or disable server-side GL capabilities.
        /// <para>
        /// glEnable and glDisable enable and disable various capabilities. Use glIsEnabled or glGet to
        /// determine the current setting of any capability. The initial value for each capability with the
        /// exception of GL_DITHER and GL_MULTISAMPLE is GL_FALSE. The initial value for GL_DITHER and
        /// GL_MULTISAMPLE is GL_TRUE.
        /// </para>
        /// </summary>
        /// <param name="cap">
        /// Specifies a symbolic constant indicating a GL capability.
        /// </param>
        public static void Disable(EnableCap cap)
        {
            Delegates.glDisable(cap);
        }

        /// <summary>
        /// Enable or disable server-side GL capabilities.
        /// <para>
        /// glEnable and glDisable enable and disable various capabilities. Use glIsEnabled or glGet to
        /// determine the current setting of any capability. The initial value for each capability with the
        /// exception of GL_DITHER and GL_MULTISAMPLE is GL_FALSE. The initial value for GL_DITHER and
        /// GL_MULTISAMPLE is GL_TRUE.
        /// </para>
        /// </summary>
        /// <param name="cap">
        /// Specifies a symbolic constant indicating a GL capability.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the switch to disable (for glEnablei and glDisablei only).
        /// </param>
        public static void Enablei(EnableCap cap, UInt32 index)
        {
            Delegates.glEnablei(cap, index);
        }

        /// <summary>
        /// Enable or disable server-side GL capabilities.
        /// <para>
        /// glEnable and glDisable enable and disable various capabilities. Use glIsEnabled or glGet to
        /// determine the current setting of any capability. The initial value for each capability with the
        /// exception of GL_DITHER and GL_MULTISAMPLE is GL_FALSE. The initial value for GL_DITHER and
        /// GL_MULTISAMPLE is GL_TRUE.
        /// </para>
        /// </summary>
        /// <param name="cap">
        /// Specifies a symbolic constant indicating a GL capability.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the switch to disable (for glEnablei and glDisablei only).
        /// </param>
        public static void Disablei(EnableCap cap, UInt32 index)
        {
            Delegates.glDisablei(cap, index);
        }

        /// <summary>
        /// Enable or disable a generic vertex attribute array.
        /// <para>
        /// glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array
        /// specified by index. glEnableVertexAttribArray uses currently bound vertex array object for the
        /// operation, whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be enabled or disabled.
        /// </param>
        public static void EnableVertexAttribArray(UInt32 index)
        {
            Delegates.glEnableVertexAttribArray(index);
        }

        /// <summary>
        /// Enable or disable a generic vertex attribute array.
        /// <para>
        /// glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array
        /// specified by index. glEnableVertexAttribArray uses currently bound vertex array object for the
        /// operation, whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be enabled or disabled.
        /// </param>
        public static void EnableVertexAttribArray(Int32 index)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glEnableVertexAttribArray((UInt32)index);
        }

        /// <summary>
        /// Enable or disable a generic vertex attribute array.
        /// <para>
        /// glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array
        /// specified by index. glEnableVertexAttribArray uses currently bound vertex array object for the
        /// operation, whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be enabled or disabled.
        /// </param>
        public static void DisableVertexAttribArray(UInt32 index)
        {
            Delegates.glDisableVertexAttribArray(index);
        }

        /// <summary>
        /// Enable or disable a generic vertex attribute array.
        /// <para>
        /// glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array
        /// specified by index. glEnableVertexAttribArray uses currently bound vertex array object for the
        /// operation, whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be enabled or disabled.
        /// </param>
        public static void DisableVertexAttribArray(Int32 index)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glDisableVertexAttribArray((UInt32)index);
        }

        /// <summary>
        /// Enable or disable a generic vertex attribute array.
        /// <para>
        /// glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array
        /// specified by index. glEnableVertexAttribArray uses currently bound vertex array object for the
        /// operation, whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glDisableVertexArrayAttrib and
        /// glEnableVertexArrayAttrib functions.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be enabled or disabled.
        /// </param>
        public static void EnableVertexArrayAttrib(UInt32 vaobj, UInt32 index)
        {
            Delegates.glEnableVertexArrayAttrib(vaobj, index);
        }

        /// <summary>
        /// Enable or disable a generic vertex attribute array.
        /// <para>
        /// glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array
        /// specified by index. glEnableVertexAttribArray uses currently bound vertex array object for the
        /// operation, whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glDisableVertexArrayAttrib and
        /// glEnableVertexArrayAttrib functions.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be enabled or disabled.
        /// </param>
        public static void EnableVertexArrayAttrib(UInt32 vaobj, Int32 index)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glEnableVertexArrayAttrib(vaobj, (UInt32)index);
        }

        /// <summary>
        /// Enable or disable a generic vertex attribute array.
        /// <para>
        /// glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array
        /// specified by index. glEnableVertexAttribArray uses currently bound vertex array object for the
        /// operation, whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glDisableVertexArrayAttrib and
        /// glEnableVertexArrayAttrib functions.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be enabled or disabled.
        /// </param>
        public static void DisableVertexArrayAttrib(UInt32 vaobj, UInt32 index)
        {
            Delegates.glDisableVertexArrayAttrib(vaobj, index);
        }

        /// <summary>
        /// Enable or disable a generic vertex attribute array.
        /// <para>
        /// glEnableVertexAttribArray and glEnableVertexArrayAttrib enable the generic vertex attribute array
        /// specified by index. glEnableVertexAttribArray uses currently bound vertex array object for the
        /// operation, whereas glEnableVertexArrayAttrib updates state of the vertex array object with ID vaobj.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glDisableVertexArrayAttrib and
        /// glEnableVertexArrayAttrib functions.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be enabled or disabled.
        /// </param>
        public static void DisableVertexArrayAttrib(UInt32 vaobj, Int32 index)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glDisableVertexArrayAttrib(vaobj, (UInt32)index);
        }

        /// <summary>
        /// Create a new sync object and insert it into the GL command stream.
        /// <para>
        /// glFenceSync creates a new fence sync object, inserts a fence command into the GL command stream and
        /// associates it with that sync object, and returns a non-zero name corresponding to the sync object.
        /// </para>
        /// </summary>
        /// <param name="condition">
        /// Specifies the condition that must be met to set the sync object's state to signaled. condition must
        /// be GL_SYNC_GPU_COMMANDS_COMPLETE.
        /// </param>
        /// <param name="flags">
        /// Specifies a bitwise combination of flags controlling the behavior of the sync object. No flags are
        /// presently defined for this operation and flags must be zero.[1]
        /// </param>
        public static IntPtr FenceSync(ArbSync condition, UInt32 flags)
        {
            return Delegates.glFenceSync(condition, flags);
        }

        /// <summary>
        /// Block until all GL execution is complete.
        /// <para>
        /// glFinish does not return until the effects of all previously called GL commands are complete. Such
        /// effects include all changes to GL state, all changes to connection state, and all changes to the
        /// frame buffer contents.
        /// </para>
        /// </summary>
        public static void Finish()
        {
            Delegates.glFinish();
        }

        /// <summary>
        /// Force execution of GL commands in finite time.
        /// <para>
        /// Different GL implementations buffer commands in several different locations, including network
        /// buffers and the graphics accelerator itself. glFlush empties all of these buffers, causing all
        /// issued commands to be executed as quickly as they are accepted by the actual rendering engine.
        /// Though this execution may not be completed in any particular time period, it does complete in finite
        /// time.
        /// </para>
        /// </summary>
        public static void Flush()
        {
            Delegates.glFlush();
        }

        /// <summary>
        /// Indicate modifications to a range of a mapped buffer.
        /// <para>
        /// glFlushMappedBufferRange indicates that modifications have been made to a range of a mapped buffer
        /// object. The buffer object must previously have been mapped with the GL_MAP_FLUSH_EXPLICIT_BIT flag.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glFlushMappedBufferRange, which must be
        /// one of the buffer binding targets in the following table: Buffer Binding Target Purpose
        /// GL_ARRAY_BUFFER Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage
        /// GL_COPY_READ_BUFFER Buffer copy source GL_COPY_WRITE_BUFFER Buffer copy destination
        /// GL_DISPATCH_INDIRECT_BUFFER Indirect compute dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect
        /// command arguments GL_ELEMENT_ARRAY_BUFFER Vertex array indices GL_PIXEL_PACK_BUFFER Pixel read
        /// target GL_PIXEL_UNPACK_BUFFER Texture data source GL_QUERY_BUFFER Query result buffer
        /// GL_SHADER_STORAGE_BUFFER Read-write storage for shaders GL_TEXTURE_BUFFER Texture data buffer
        /// GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="offset">
        /// Specifies the start of the buffer subrange, in basic machine units.
        /// </param>
        /// <param name="length">
        /// Specifies the length of the buffer subrange, in basic machine units.
        /// </param>
        public static void FlushMappedBufferRange(BufferTarget target, IntPtr offset, IntPtr length)
        {
            Delegates.glFlushMappedBufferRange(target, offset, length);
        }

        /// <summary>
        /// Indicate modifications to a range of a mapped buffer.
        /// <para>
        /// glFlushMappedBufferRange indicates that modifications have been made to a range of a mapped buffer
        /// object. The buffer object must previously have been mapped with the GL_MAP_FLUSH_EXPLICIT_BIT flag.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glFlushMappedNamedBufferRange.
        /// </param>
        /// <param name="offset">
        /// Specifies the start of the buffer subrange, in basic machine units.
        /// </param>
        /// <param name="length">
        /// Specifies the length of the buffer subrange, in basic machine units.
        /// </param>
        public static void FlushMappedNamedBufferRange(UInt32 buffer, IntPtr offset, Int32 length)
        {
            Delegates.glFlushMappedNamedBufferRange(buffer, offset, length);
        }

        /// <summary>
        /// Set a named parameter of a framebuffer object.
        /// <para>
        /// glFramebufferParameteri and glNamedFramebufferParameteri modify the value of the parameter named
        /// pname in the specified framebuffer object. There are no modifiable parameters of the default draw
        /// and read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer is bound for glFramebufferParameteri.
        /// </param>
        /// <param name="pname">
        /// Specifies the framebuffer parameter to be modified.
        /// </param>
        /// <param name="param">
        /// The new value for the parameter named pname.
        /// </param>
        public static void FramebufferParameteri(FramebufferTarget target, FramebufferPName pname, Int32 param)
        {
            Delegates.glFramebufferParameteri(target, pname, param);
        }

        /// <summary>
        /// Set a named parameter of a framebuffer object.
        /// <para>
        /// glFramebufferParameteri and glNamedFramebufferParameteri modify the value of the parameter named
        /// pname in the specified framebuffer object. There are no modifiable parameters of the default draw
        /// and read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glNamedFramebufferParameteri.
        /// </param>
        /// <param name="pname">
        /// Specifies the framebuffer parameter to be modified.
        /// </param>
        /// <param name="param">
        /// The new value for the parameter named pname.
        /// </param>
        public static void NamedFramebufferParameteri(UInt32 framebuffer, FramebufferPName pname, Int32 param)
        {
            Delegates.glNamedFramebufferParameteri(framebuffer, pname, param);
        }

        /// <summary>
        /// Attach a renderbuffer as a logical buffer of a framebuffer object.
        /// <para>
        /// glFramebufferRenderbuffer and glNamedFramebufferRenderbuffer attaches a renderbuffer as one of the
        /// logical buffers of the specified framebuffer object. Renderbuffers cannot be attached to the default
        /// draw and read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer is bound for glFramebufferRenderbuffer.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="renderbuffertarget">
        /// Specifies the renderbuffer target. Must be GL_RENDERBUFFER.
        /// </param>
        /// <param name="renderbuffer">
        /// Specifies the name of an existing renderbuffer object of type renderbuffertarget to attach.
        /// </param>
        public static void FramebufferRenderbuffer(FramebufferTarget target, FramebufferAttachment attachment, RenderbufferTarget renderbuffertarget, UInt32 renderbuffer)
        {
            Delegates.glFramebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffer);
        }

        /// <summary>
        /// Attach a renderbuffer as a logical buffer of a framebuffer object.
        /// <para>
        /// glFramebufferRenderbuffer and glNamedFramebufferRenderbuffer attaches a renderbuffer as one of the
        /// logical buffers of the specified framebuffer object. Renderbuffers cannot be attached to the default
        /// draw and read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glNamedFramebufferRenderbuffer.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="renderbuffertarget">
        /// Specifies the renderbuffer target. Must be GL_RENDERBUFFER.
        /// </param>
        /// <param name="renderbuffer">
        /// Specifies the name of an existing renderbuffer object of type renderbuffertarget to attach.
        /// </param>
        public static void NamedFramebufferRenderbuffer(UInt32 framebuffer, FramebufferAttachment attachment, RenderbufferTarget renderbuffertarget, UInt32 renderbuffer)
        {
            Delegates.glNamedFramebufferRenderbuffer(framebuffer, attachment, renderbuffertarget, renderbuffer);
        }

        /// <summary>
        /// Attach a level of a texture object as a logical buffer of a framebuffer object.
        /// <para>
        /// These commands attach a selected mipmap level or image of a texture object as one of the logical
        /// buffers of the specified framebuffer object. Textures cannot be attached to the default draw and
        /// read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer is bound for all commands except
        /// glNamedFramebufferTexture.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of an existing texture object to attach.
        /// </param>
        /// <param name="level">
        /// Specifies the mipmap level of the texture object to attach.
        /// </param>
        public static void FramebufferTexture(FramebufferTarget target, FramebufferAttachment attachment, UInt32 texture, Int32 level)
        {
            Delegates.glFramebufferTexture(target, attachment, texture, level);
        }

        /// <summary>
        /// Attach a level of a texture object as a logical buffer of a framebuffer object.
        /// <para>
        /// These commands attach a selected mipmap level or image of a texture object as one of the logical
        /// buffers of the specified framebuffer object. Textures cannot be attached to the default draw and
        /// read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer is bound for all commands except
        /// glNamedFramebufferTexture.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="textarget">
        /// For glFramebufferTexture1D, glFramebufferTexture2D and glFramebufferTexture3D, specifies what type
        /// of texture is expected in the texture parameter, or for cube map textures, which face is to be
        /// attached.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of an existing texture object to attach.
        /// </param>
        /// <param name="level">
        /// Specifies the mipmap level of the texture object to attach.
        /// </param>
        public static void FramebufferTexture1D(FramebufferTarget target, FramebufferAttachment attachment, TextureTarget textarget, UInt32 texture, Int32 level)
        {
            Delegates.glFramebufferTexture1D(target, attachment, textarget, texture, level);
        }

        /// <summary>
        /// Attach a level of a texture object as a logical buffer of a framebuffer object.
        /// <para>
        /// These commands attach a selected mipmap level or image of a texture object as one of the logical
        /// buffers of the specified framebuffer object. Textures cannot be attached to the default draw and
        /// read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer is bound for all commands except
        /// glNamedFramebufferTexture.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="textarget">
        /// For glFramebufferTexture1D, glFramebufferTexture2D and glFramebufferTexture3D, specifies what type
        /// of texture is expected in the texture parameter, or for cube map textures, which face is to be
        /// attached.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of an existing texture object to attach.
        /// </param>
        /// <param name="level">
        /// Specifies the mipmap level of the texture object to attach.
        /// </param>
        public static void FramebufferTexture2D(FramebufferTarget target, FramebufferAttachment attachment, TextureTarget textarget, UInt32 texture, Int32 level)
        {
            Delegates.glFramebufferTexture2D(target, attachment, textarget, texture, level);
        }

        /// <summary>
        /// Attach a level of a texture object as a logical buffer of a framebuffer object.
        /// <para>
        /// These commands attach a selected mipmap level or image of a texture object as one of the logical
        /// buffers of the specified framebuffer object. Textures cannot be attached to the default draw and
        /// read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer is bound for all commands except
        /// glNamedFramebufferTexture.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="textarget">
        /// For glFramebufferTexture1D, glFramebufferTexture2D and glFramebufferTexture3D, specifies what type
        /// of texture is expected in the texture parameter, or for cube map textures, which face is to be
        /// attached.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of an existing texture object to attach.
        /// </param>
        /// <param name="level">
        /// Specifies the mipmap level of the texture object to attach.
        /// </param>
        /// <param name="layer">
        /// </param>
        public static void FramebufferTexture3D(FramebufferTarget target, FramebufferAttachment attachment, TextureTarget textarget, UInt32 texture, Int32 level, Int32 layer)
        {
            Delegates.glFramebufferTexture3D(target, attachment, textarget, texture, level, layer);
        }

        /// <summary>
        /// Attach a level of a texture object as a logical buffer of a framebuffer object.
        /// <para>
        /// These commands attach a selected mipmap level or image of a texture object as one of the logical
        /// buffers of the specified framebuffer object. Textures cannot be attached to the default draw and
        /// read framebuffer, so they are not valid targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glNamedFramebufferTexture.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of an existing texture object to attach.
        /// </param>
        /// <param name="level">
        /// Specifies the mipmap level of the texture object to attach.
        /// </param>
        public static void NamedFramebufferTexture(UInt32 framebuffer, FramebufferAttachment attachment, UInt32 texture, Int32 level)
        {
            Delegates.glNamedFramebufferTexture(framebuffer, attachment, texture, level);
        }

        /// <summary>
        /// Attach a single layer of a texture object as a logical buffer of a framebuffer object.
        /// <para>
        /// glFramebufferTextureLayer and glNamedFramebufferTextureLayer attach a single layer of a
        /// three-dimensional or array texture object as one of the logical buffers of the specified framebuffer
        /// object. Textures cannot be attached to the default draw and read framebuffer, so they are not valid
        /// targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer is bound for glFramebufferTextureLayer.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of an existing texture object to attach.
        /// </param>
        /// <param name="level">
        /// Specifies the mipmap level of the texture object to attach.
        /// </param>
        /// <param name="layer">
        /// Specifies the layer of the texture object to attach.
        /// </param>
        public static void FramebufferTextureLayer(FramebufferTarget target, FramebufferAttachment attachment, UInt32 texture, Int32 level, Int32 layer)
        {
            Delegates.glFramebufferTextureLayer(target, attachment, texture, level, layer);
        }

        /// <summary>
        /// Attach a single layer of a texture object as a logical buffer of a framebuffer object.
        /// <para>
        /// glFramebufferTextureLayer and glNamedFramebufferTextureLayer attach a single layer of a
        /// three-dimensional or array texture object as one of the logical buffers of the specified framebuffer
        /// object. Textures cannot be attached to the default draw and read framebuffer, so they are not valid
        /// targets of these commands.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glNamedFramebufferTextureLayer.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment point of the framebuffer.
        /// </param>
        /// <param name="texture">
        /// Specifies the name of an existing texture object to attach.
        /// </param>
        /// <param name="level">
        /// Specifies the mipmap level of the texture object to attach.
        /// </param>
        /// <param name="layer">
        /// Specifies the layer of the texture object to attach.
        /// </param>
        public static void NamedFramebufferTextureLayer(UInt32 framebuffer, FramebufferAttachment attachment, UInt32 texture, Int32 level, Int32 layer)
        {
            Delegates.glNamedFramebufferTextureLayer(framebuffer, attachment, texture, level, layer);
        }

        /// <summary>
        /// Define front- and back-facing polygons.
        /// <para>
        /// In a scene composed entirely of opaque closed surfaces, back-facing polygons are never visible.
        /// Eliminating these invisible polygons has the obvious benefit of speeding up the rendering of the
        /// image. To enable and disable elimination of back-facing polygons, call glEnable and glDisable with
        /// argument GL_CULL_FACE.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies the orientation of front-facing polygons. GL_CW and GL_CCW are accepted. The initial value
        /// is GL_CCW.
        /// </param>
        public static void FrontFace(FrontFaceDirection mode)
        {
            Delegates.glFrontFace(mode);
        }

        /// <summary>
        /// Generate buffer object names.
        /// <para>
        /// glGenBuffers returns n buffer object names in buffers. There is no guarantee that the names form a
        /// contiguous set of integers; however, it is guaranteed that none of the returned names was in use
        /// immediately before the call to glGenBuffers.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of buffer object names to be generated.
        /// </param>
        /// <param name="buffers">
        /// Specifies an array in which the generated buffer object names are stored.
        /// </param>
        public static void GenBuffers(Int32 n, [OutAttribute] UInt32[] buffers)
        {
            Delegates.glGenBuffers(n, buffers);
        }

        /// <summary>
        /// Generate mipmaps for a specified texture object.
        /// <para>
        /// glGenerateMipmap and glGenerateTextureMipmap generates mipmaps for the specified texture object. For
        /// glGenerateMipmap, the texture object is that bound to to target. For glGenerateTextureMipmap,
        /// texture is the name of the texture object.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glGenerateMipmap. Must be one of
        /// GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
        /// GL_TEXTURE_CUBE_MAP, or GL_TEXTURE_CUBE_MAP_ARRAY.
        /// </param>
        public static void GenerateMipmap(GenerateMipmapTarget target)
        {
            Delegates.glGenerateMipmap(target);
        }

        /// <summary>
        /// Generate mipmaps for a specified texture object.
        /// <para>
        /// glGenerateMipmap and glGenerateTextureMipmap generates mipmaps for the specified texture object. For
        /// glGenerateMipmap, the texture object is that bound to to target. For glGenerateTextureMipmap,
        /// texture is the name of the texture object.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glGenerateTextureMipmap.
        /// </param>
        public static void GenerateTextureMipmap(UInt32 texture)
        {
            Delegates.glGenerateTextureMipmap(texture);
        }

        /// <summary>
        /// Generate framebuffer object names.
        /// <para>
        /// glGenFramebuffers returns n framebuffer object names in ids. There is no guarantee that the names
        /// form a contiguous set of integers; however, it is guaranteed that none of the returned names was in
        /// use immediately before the call to glGenFramebuffers.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of framebuffer object names to generate.
        /// </param>
        /// <param name="ids">
        /// Specifies an array in which the generated framebuffer object names are stored.
        /// </param>
        public static void GenFramebuffers(Int32 n, [OutAttribute] UInt32[] ids)
        {
            Delegates.glGenFramebuffers(n, ids);
        }

        /// <summary>
        /// Reserve program pipeline object names.
        /// <para>
        /// glGenProgramPipelines returns n previously unused program pipeline object names in pipelines. These
        /// names are marked as used, for the purposes of glGenProgramPipelines only, but they acquire program
        /// pipeline state only when they are first bound.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of program pipeline object names to reserve.
        /// </param>
        /// <param name="pipelines">
        /// Specifies an array of into which the reserved names will be written.
        /// </param>
        public static void GenProgramPipelines(Int32 n, [OutAttribute] UInt32[] pipelines)
        {
            Delegates.glGenProgramPipelines(n, pipelines);
        }

        /// <summary>
        /// Generate query object names.
        /// <para>
        /// glGenQueries returns n query object names in ids. There is no guarantee that the names form a
        /// contiguous set of integers; however, it is guaranteed that none of the returned names was in use
        /// immediately before the call to glGenQueries.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of query object names to be generated.
        /// </param>
        /// <param name="ids">
        /// Specifies an array in which the generated query object names are stored.
        /// </param>
        public static void GenQueries(Int32 n, [OutAttribute] UInt32[] ids)
        {
            Delegates.glGenQueries(n, ids);
        }

        /// <summary>
        /// Generate renderbuffer object names.
        /// <para>
        /// glGenRenderbuffers returns n renderbuffer object names in renderbuffers. There is no guarantee that
        /// the names form a contiguous set of integers; however, it is guaranteed that none of the returned
        /// names was in use immediately before the call to glGenRenderbuffers.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of renderbuffer object names to generate.
        /// </param>
        /// <param name="renderbuffers">
        /// Specifies an array in which the generated renderbuffer object names are stored.
        /// </param>
        public static void GenRenderbuffers(Int32 n, [OutAttribute] UInt32[] renderbuffers)
        {
            Delegates.glGenRenderbuffers(n, renderbuffers);
        }

        /// <summary>
        /// Generate sampler object names.
        /// <para>
        /// glGenSamplers returns n sampler object names in samplers. There is no guarantee that the names form
        /// a contiguous set of integers; however, it is guaranteed that none of the returned names was in use
        /// immediately before the call to glGenSamplers.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of sampler object names to generate.
        /// </param>
        /// <param name="samplers">
        /// Specifies an array in which the generated sampler object names are stored.
        /// </param>
        public static void GenSamplers(Int32 n, [OutAttribute] UInt32[] samplers)
        {
            Delegates.glGenSamplers(n, samplers);
        }

        /// <summary>
        /// Generate texture names.
        /// <para>
        /// glGenTextures returns n texture names in textures. There is no guarantee that the names form a
        /// contiguous set of integers; however, it is guaranteed that none of the returned names was in use
        /// immediately before the call to glGenTextures.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of texture names to be generated.
        /// </param>
        /// <param name="textures">
        /// Specifies an array in which the generated texture names are stored.
        /// </param>
        public static void GenTextures(Int32 n, [OutAttribute] UInt32[] textures)
        {
            Delegates.glGenTextures(n, textures);
        }

        /// <summary>
        /// Reserve transform feedback object names.
        /// <para>
        /// glGenTransformFeedbacks returns n previously unused transform feedback object names in ids. These
        /// names are marked as used, for the purposes of glGenTransformFeedbacks only, but they acquire
        /// transform feedback state only when they are first bound.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of transform feedback object names to reserve.
        /// </param>
        /// <param name="ids">
        /// Specifies an array of into which the reserved names will be written.
        /// </param>
        public static void GenTransformFeedbacks(Int32 n, [OutAttribute] UInt32[] ids)
        {
            Delegates.glGenTransformFeedbacks(n, ids);
        }

        /// <summary>
        /// Generate vertex array object names.
        /// <para>
        /// glGenVertexArrays returns n vertex array object names in arrays. There is no guarantee that the
        /// names form a contiguous set of integers; however, it is guaranteed that none of the returned names
        /// was in use immediately before the call to glGenVertexArrays.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// Specifies the number of vertex array object names to generate.
        /// </param>
        /// <param name="arrays">
        /// Specifies an array in which the generated vertex array object names are stored.
        /// </param>
        public static void GenVertexArrays(Int32 n, [OutAttribute] UInt32[] arrays)
        {
            Delegates.glGenVertexArrays(n, arrays);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the parameter value to be returned for non-indexed versions of glGet. The symbolic
        /// constants in the list below are accepted.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetBooleanv(GetPName pname, [OutAttribute] Boolean[] data)
        {
            Delegates.glGetBooleanv(pname, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the parameter value to be returned for non-indexed versions of glGet. The symbolic
        /// constants in the list below are accepted.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetDoublev(GetPName pname, [OutAttribute] Double[] data)
        {
            Delegates.glGetDoublev(pname, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the parameter value to be returned for non-indexed versions of glGet. The symbolic
        /// constants in the list below are accepted.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetFloatv(GetPName pname, [OutAttribute] Single[] data)
        {
            Delegates.glGetFloatv(pname, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the parameter value to be returned for non-indexed versions of glGet. The symbolic
        /// constants in the list below are accepted.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetIntegerv(GetPName pname, [OutAttribute] Int32[] data)
        {
            Delegates.glGetIntegerv(pname, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the parameter value to be returned for non-indexed versions of glGet. The symbolic
        /// constants in the list below are accepted.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetInteger64v(ArbSync pname, [OutAttribute] Int64[] data)
        {
            Delegates.glGetInteger64v(pname, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the parameter value to be returned for indexed versions of glGet. The symbolic constants
        /// in the list below are accepted.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the particular element being queried.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetBooleani_v(GetPName target, UInt32 index, [OutAttribute] Boolean[] data)
        {
            Delegates.glGetBooleani_v(target, index, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the parameter value to be returned for indexed versions of glGet. The symbolic constants
        /// in the list below are accepted.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the particular element being queried.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetIntegeri_v(GetPName target, UInt32 index, [OutAttribute] Int32[] data)
        {
            Delegates.glGetIntegeri_v(target, index, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the parameter value to be returned for indexed versions of glGet. The symbolic constants
        /// in the list below are accepted.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the particular element being queried.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetFloati_v(GetPName target, UInt32 index, [OutAttribute] Single[] data)
        {
            Delegates.glGetFloati_v(target, index, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the parameter value to be returned for indexed versions of glGet. The symbolic constants
        /// in the list below are accepted.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the particular element being queried.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetDoublei_v(GetPName target, UInt32 index, [OutAttribute] Double[] data)
        {
            Delegates.glGetDoublei_v(target, index, data);
        }

        /// <summary>
        /// Return the value or values of a selected parameter.
        /// <para>
        /// These commands return values for simple state variables in GL. pname is a symbolic constant
        /// indicating the state variable to be returned, and data is a pointer to an array of the indicated
        /// type in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the parameter value to be returned for indexed versions of glGet. The symbolic constants
        /// in the list below are accepted.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the particular element being queried.
        /// </param>
        /// <param name="data">
        /// Returns the value or values of the specified parameter.
        /// </param>
        public static void GetInteger64i_v(GetPName target, UInt32 index, [OutAttribute] Int64[] data)
        {
            Delegates.glGetInteger64i_v(target, index, data);
        }

        /// <summary>
        /// Retrieve information about the set of active atomic counter buffers for a program.
        /// <para>
        /// glGetActiveAtomicCounterBufferiv retrieves information about the set of active atomic counter
        /// buffers for a program object. program is the name of a program object for which the command
        /// glLinkProgram has been issued in the past. It is not necessary for program to have been linked
        /// successfully. The link may have failed because the number of active atomic counters exceeded the
        /// limits.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of a program object from which to retrieve information.
        /// </param>
        /// <param name="bufferIndex">
        /// Specifies index of an active atomic counter buffer.
        /// </param>
        /// <param name="pname">
        /// Specifies which parameter of the atomic counter buffer to retrieve.
        /// </param>
        /// <param name="params">
        /// Specifies the address of a variable into which to write the retrieved information.
        /// </param>
        public static void GetActiveAtomicCounterBufferiv(UInt32 program, UInt32 bufferIndex, AtomicCounterParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetActiveAtomicCounterBufferiv(program, bufferIndex, pname, @params);
        }

        /// <summary>
        /// Returns information about an active attribute variable for the specified program object.
        /// <para>
        /// glGetActiveAttrib returns information about an active attribute variable in the program object
        /// specified by program. The number of active attributes can be obtained by calling glGetProgram with
        /// the value GL_ACTIVE_ATTRIBUTES. A value of 0 for index selects the first active attribute variable.
        /// Permissible values for index range from zero to the number of active attribute variables minus one.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the attribute variable to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the maximum number of characters OpenGL is allowed to write in the character buffer
        /// indicated by name.
        /// </param>
        /// <param name="length">
        /// Returns the number of characters actually written by OpenGL in the string indicated by name
        /// (excluding the null terminator) if a value other than NULL is passed.
        /// </param>
        /// <param name="size">
        /// Returns the size of the attribute variable.
        /// </param>
        /// <param name="type">
        /// Returns the data type of the attribute variable.
        /// </param>
        /// <param name="name">
        /// Returns a null terminated string containing the name of the attribute variable.
        /// </param>
        public static void GetActiveAttrib(UInt32 program, UInt32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] size, [OutAttribute] ActiveAttribType[] type, [OutAttribute] System.Text.StringBuilder name)
        {
            Delegates.glGetActiveAttrib(program, index, bufSize, length, size, type, name);
        }

        /// <summary>
        /// Returns information about an active attribute variable for the specified program object.
        /// <para>
        /// glGetActiveAttrib returns information about an active attribute variable in the program object
        /// specified by program. The number of active attributes can be obtained by calling glGetProgram with
        /// the value GL_ACTIVE_ATTRIBUTES. A value of 0 for index selects the first active attribute variable.
        /// Permissible values for index range from zero to the number of active attribute variables minus one.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the attribute variable to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the maximum number of characters OpenGL is allowed to write in the character buffer
        /// indicated by name.
        /// </param>
        /// <param name="length">
        /// Returns the number of characters actually written by OpenGL in the string indicated by name
        /// (excluding the null terminator) if a value other than NULL is passed.
        /// </param>
        /// <param name="size">
        /// Returns the size of the attribute variable.
        /// </param>
        /// <param name="type">
        /// Returns the data type of the attribute variable.
        /// </param>
        /// <param name="name">
        /// Returns a null terminated string containing the name of the attribute variable.
        /// </param>
        public static void GetActiveAttrib(UInt32 program, Int32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] size, [OutAttribute] ActiveAttribType[] type, [OutAttribute] System.Text.StringBuilder name)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetActiveAttrib(program, (UInt32)index, bufSize, length, size, type, name);
        }

        /// <summary>
        /// Query the name of an active shader subroutine.
        /// <para>
        /// glGetActiveSubroutineName queries the name of an active shader subroutine uniform from the program
        /// object given in program. index specifies the index of the shader subroutine uniform within the
        /// shader stage given by stage, and must between zero and the value of GL_ACTIVE_SUBROUTINES minus one
        /// for the shader stage.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of the program containing the subroutine.
        /// </param>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query the subroutine name.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the shader subroutine uniform.
        /// </param>
        /// <param name="bufsize">
        /// Specifies the size of the buffer whose address is given in name.
        /// </param>
        /// <param name="length">
        /// Specifies the address of a variable which is to receive the length of the shader subroutine uniform
        /// name.
        /// </param>
        /// <param name="name">
        /// Specifies the address of an array into which the name of the shader subroutine uniform will be
        /// written.
        /// </param>
        public static void GetActiveSubroutineName(UInt32 program, ShaderType shadertype, UInt32 index, Int32 bufsize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder name)
        {
            Delegates.glGetActiveSubroutineName(program, shadertype, index, bufsize, length, name);
        }

        /// <summary>
        /// Query a property of an active shader subroutine uniform.
        /// <para>
        /// glGetActiveSubroutineUniform queries a parameter of an active shader subroutine uniform. program
        /// contains the name of the program containing the uniform. shadertype specifies the stage which which
        /// the uniform location, given by index, is valid. index must be between zero and the value of
        /// GL_ACTIVE_SUBROUTINE_UNIFORMS minus one for the shader stage.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of the program containing the subroutine.
        /// </param>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for the subroutine parameter. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the shader subroutine uniform.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of the shader subroutine uniform to query. pname must be
        /// GL_NUM_COMPATIBLE_SUBROUTINES, GL_COMPATIBLE_SUBROUTINES, GL_UNIFORM_SIZE or GL_UNIFORM_NAME_LENGTH.
        /// </param>
        /// <param name="values">
        /// Specifies the address of a into which the queried value or values will be placed.
        /// </param>
        public static void GetActiveSubroutineUniformiv(UInt32 program, ShaderType shadertype, UInt32 index, SubroutineParameterName pname, [OutAttribute] Int32[] values)
        {
            Delegates.glGetActiveSubroutineUniformiv(program, shadertype, index, pname, values);
        }

        /// <summary>
        /// Query a property of an active shader subroutine uniform.
        /// <para>
        /// glGetActiveSubroutineUniform queries a parameter of an active shader subroutine uniform. program
        /// contains the name of the program containing the uniform. shadertype specifies the stage which which
        /// the uniform location, given by index, is valid. index must be between zero and the value of
        /// GL_ACTIVE_SUBROUTINE_UNIFORMS minus one for the shader stage.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of the program containing the subroutine.
        /// </param>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for the subroutine parameter. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the shader subroutine uniform.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of the shader subroutine uniform to query. pname must be
        /// GL_NUM_COMPATIBLE_SUBROUTINES, GL_COMPATIBLE_SUBROUTINES, GL_UNIFORM_SIZE or GL_UNIFORM_NAME_LENGTH.
        /// </param>
        /// <param name="values">
        /// Specifies the address of a into which the queried value or values will be placed.
        /// </param>
        public static void GetActiveSubroutineUniformiv(UInt32 program, ShaderType shadertype, Int32 index, SubroutineParameterName pname, [OutAttribute] Int32[] values)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetActiveSubroutineUniformiv(program, shadertype, (UInt32)index, pname, values);
        }

        /// <summary>
        /// Query the name of an active shader subroutine uniform.
        /// <para>
        /// glGetActiveSubroutineUniformName retrieves the name of an active shader subroutine uniform. program
        /// contains the name of the program containing the uniform. shadertype specifies the stage for which
        /// which the uniform location, given by index, is valid. index must be between zero and the value of
        /// GL_ACTIVE_SUBROUTINE_UNIFORMS minus one for the shader stage.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of the program containing the subroutine.
        /// </param>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for the subroutine parameter. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the shader subroutine uniform.
        /// </param>
        /// <param name="bufsize">
        /// Specifies the size of the buffer whose address is given in name.
        /// </param>
        /// <param name="length">
        /// Specifies the address of a variable into which is written the number of characters copied into name.
        /// 
        /// </param>
        /// <param name="name">
        /// Specifies the address of a buffer that will receive the name of the specified shader subroutine
        /// uniform.
        /// </param>
        public static void GetActiveSubroutineUniformName(UInt32 program, ShaderType shadertype, UInt32 index, Int32 bufsize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder name)
        {
            Delegates.glGetActiveSubroutineUniformName(program, shadertype, index, bufsize, length, name);
        }

        /// <summary>
        /// Query the name of an active shader subroutine uniform.
        /// <para>
        /// glGetActiveSubroutineUniformName retrieves the name of an active shader subroutine uniform. program
        /// contains the name of the program containing the uniform. shadertype specifies the stage for which
        /// which the uniform location, given by index, is valid. index must be between zero and the value of
        /// GL_ACTIVE_SUBROUTINE_UNIFORMS minus one for the shader stage.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of the program containing the subroutine.
        /// </param>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for the subroutine parameter. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the shader subroutine uniform.
        /// </param>
        /// <param name="bufsize">
        /// Specifies the size of the buffer whose address is given in name.
        /// </param>
        /// <param name="length">
        /// Specifies the address of a variable into which is written the number of characters copied into name.
        /// 
        /// </param>
        /// <param name="name">
        /// Specifies the address of a buffer that will receive the name of the specified shader subroutine
        /// uniform.
        /// </param>
        public static void GetActiveSubroutineUniformName(UInt32 program, ShaderType shadertype, Int32 index, Int32 bufsize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder name)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetActiveSubroutineUniformName(program, shadertype, (UInt32)index, bufsize, length, name);
        }

        /// <summary>
        /// Returns information about an active uniform variable for the specified program object.
        /// <para>
        /// glGetActiveUniform returns information about an active uniform variable in the program object
        /// specified by program. The number of active uniform variables can be obtained by calling glGetProgram
        /// with the value GL_ACTIVE_UNIFORMS. A value of 0 for index selects the first active uniform variable.
        /// Permissible values for index range from zero to the number of active uniform variables minus one.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the uniform variable to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the maximum number of characters OpenGL is allowed to write in the character buffer
        /// indicated by name.
        /// </param>
        /// <param name="length">
        /// Returns the number of characters actually written by OpenGL in the string indicated by name
        /// (excluding the null terminator) if a value other than NULL is passed.
        /// </param>
        /// <param name="size">
        /// Returns the size of the uniform variable.
        /// </param>
        /// <param name="type">
        /// Returns the data type of the uniform variable.
        /// </param>
        /// <param name="name">
        /// Returns a null terminated string containing the name of the uniform variable.
        /// </param>
        public static void GetActiveUniform(UInt32 program, UInt32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] size, [OutAttribute] ActiveUniformType[] type, [OutAttribute] System.Text.StringBuilder name)
        {
            Delegates.glGetActiveUniform(program, index, bufSize, length, size, type, name);
        }

        /// <summary>
        /// Returns information about an active uniform variable for the specified program object.
        /// <para>
        /// glGetActiveUniform returns information about an active uniform variable in the program object
        /// specified by program. The number of active uniform variables can be obtained by calling glGetProgram
        /// with the value GL_ACTIVE_UNIFORMS. A value of 0 for index selects the first active uniform variable.
        /// Permissible values for index range from zero to the number of active uniform variables minus one.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the uniform variable to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the maximum number of characters OpenGL is allowed to write in the character buffer
        /// indicated by name.
        /// </param>
        /// <param name="length">
        /// Returns the number of characters actually written by OpenGL in the string indicated by name
        /// (excluding the null terminator) if a value other than NULL is passed.
        /// </param>
        /// <param name="size">
        /// Returns the size of the uniform variable.
        /// </param>
        /// <param name="type">
        /// Returns the data type of the uniform variable.
        /// </param>
        /// <param name="name">
        /// Returns a null terminated string containing the name of the uniform variable.
        /// </param>
        public static void GetActiveUniform(UInt32 program, Int32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] size, [OutAttribute] ActiveUniformType[] type, [OutAttribute] System.Text.StringBuilder name)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetActiveUniform(program, (UInt32)index, bufSize, length, size, type, name);
        }

        /// <summary>
        /// Query information about an active uniform block.
        /// <para>
        /// glGetActiveUniformBlockiv retrieves information about an active uniform block within program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of a program containing the uniform block.
        /// </param>
        /// <param name="uniformBlockIndex">
        /// Specifies the index of the uniform block within program.
        /// </param>
        /// <param name="pname">
        /// Specifies the name of the parameter to query.
        /// </param>
        /// <param name="params">
        /// Specifies the address of a variable to receive the result of the query.
        /// </param>
        public static void GetActiveUniformBlockiv(UInt32 program, UInt32 uniformBlockIndex, ActiveUniformBlockParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetActiveUniformBlockiv(program, uniformBlockIndex, pname, @params);
        }

        /// <summary>
        /// Retrieve the name of an active uniform block.
        /// <para>
        /// glGetActiveUniformBlockName retrieves the name of the active uniform block at uniformBlockIndex
        /// within program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of a program containing the uniform block.
        /// </param>
        /// <param name="uniformBlockIndex">
        /// Specifies the index of the uniform block within program.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer addressed by uniformBlockName.
        /// </param>
        /// <param name="length">
        /// Specifies the address of a variable to receive the number of characters that were written to
        /// uniformBlockName.
        /// </param>
        /// <param name="uniformBlockName">
        /// Specifies the address an array of characters to receive the name of the uniform block at
        /// uniformBlockIndex.
        /// </param>
        public static void GetActiveUniformBlockName(UInt32 program, UInt32 uniformBlockIndex, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder uniformBlockName)
        {
            Delegates.glGetActiveUniformBlockName(program, uniformBlockIndex, bufSize, length, uniformBlockName);
        }

        /// <summary>
        /// Query the name of an active uniform.
        /// <para>
        /// glGetActiveUniformName returns the name of the active uniform at uniformIndex within program. If
        /// uniformName is not NULL, up to bufSize characters (including a nul-terminator) will be written into
        /// the array whose address is specified by uniformName. If length is not NULL, the number of characters
        /// that were (or would have been) written into uniformName (not including the nul-terminator) will be
        /// placed in the variable whose address is specified in length. If length is NULL, no length is
        /// returned. The length of the longest uniform name in program is given by the value of
        /// GL_ACTIVE_UNIFORM_MAX_LENGTH, which can be queried with glGetProgram.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program containing the active uniform index uniformIndex.
        /// </param>
        /// <param name="uniformIndex">
        /// Specifies the index of the active uniform whose name to query.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer, in units of GLchar, of the buffer whose address is specified in
        /// uniformName.
        /// </param>
        /// <param name="length">
        /// Specifies the address of a variable that will receive the number of characters that were or would
        /// have been written to the buffer addressed by uniformName.
        /// </param>
        /// <param name="uniformName">
        /// Specifies the address of a buffer into which the GL will place the name of the active uniform at
        /// uniformIndex within program.
        /// </param>
        public static void GetActiveUniformName(UInt32 program, UInt32 uniformIndex, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder uniformName)
        {
            Delegates.glGetActiveUniformName(program, uniformIndex, bufSize, length, uniformName);
        }

        /// <summary>
        /// Returns information about several active uniform variables for the specified program object.
        /// <para>
        /// glGetActiveUniformsiv queries the value of the parameter named pname for each of the uniforms within
        /// program whose indices are specified in the array of uniformCount unsigned integers uniformIndices.
        /// Upon success, the value of the parameter for each uniform is written into the corresponding entry in
        /// the array whose address is given in params. If an error is generated, nothing is written into
        /// params.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="uniformCount">
        /// Specifies both the number of elements in the array of indices uniformIndices and the number of
        /// parameters written to params upon successful return.
        /// </param>
        /// <param name="uniformIndices">
        /// Specifies the address of an array of uniformCount integers containing the indices of uniforms within
        /// program whose parameter pname should be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the property of each uniform in uniformIndices that should be written into the
        /// corresponding element of params.
        /// </param>
        /// <param name="params">
        /// Specifies the address of an array of uniformCount integers which are to receive the value of pname
        /// for each uniform in uniformIndices.
        /// </param>
        public static void GetActiveUniformsiv(UInt32 program, Int32 uniformCount, [OutAttribute] UInt32[] uniformIndices, ActiveUniformType pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetActiveUniformsiv(program, uniformCount, uniformIndices, pname, @params);
        }

        /// <summary>
        /// Returns the handles of the shader objects attached to a program object.
        /// <para>
        /// glGetAttachedShaders returns the names of the shader objects attached to program. The names of
        /// shader objects that are attached to program will be returned in shaders. The actual number of shader
        /// names written into shaders is returned in count. If no shader objects are attached to program, count
        /// is set to 0. The maximum number of shader names that may be returned in shaders is specified by
        /// maxCount.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="maxCount">
        /// Specifies the size of the array for storing the returned object names.
        /// </param>
        /// <param name="count">
        /// Returns the number of names actually returned in shaders.
        /// </param>
        /// <param name="shaders">
        /// Specifies an array that is used to return the names of attached shader objects.
        /// </param>
        public static void GetAttachedShaders(UInt32 program, Int32 maxCount, [OutAttribute] Int32[] count, [OutAttribute] UInt32[] shaders)
        {
            Delegates.glGetAttachedShaders(program, maxCount, count, shaders);
        }

        /// <summary>
        /// Returns the location of an attribute variable.
        /// <para>
        /// glGetAttribLocation queries the previously linked program object specified by program for the
        /// attribute variable specified by name and returns the index of the generic vertex attribute that is
        /// bound to that attribute variable. If name is a matrix attribute variable, the index of the first
        /// column of the matrix is returned. If the named attribute variable is not an active attribute in the
        /// specified program object or if name starts with the reserved prefix "gl_", a value of -1 is
        /// returned.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="name">
        /// Points to a null terminated string containing the name of the attribute variable whose location is
        /// to be queried.
        /// </param>
        public static Int32 GetAttribLocation(UInt32 program, String name)
        {
            return Delegates.glGetAttribLocation(program, name);
        }

        /// <summary>
        /// Return parameters of a buffer object.
        /// <para>
        /// These functions return in data a selected parameter of the specified buffer object.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glGetBufferParameteriv and
        /// glGetBufferParameteri64v. Must be one of the buffer binding targets in the following table: Buffer
        /// Binding Target Purpose GL_ARRAY_BUFFER Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter
        /// storage GL_COPY_READ_BUFFER Buffer copy source GL_COPY_WRITE_BUFFER Buffer copy destination
        /// GL_DISPATCH_INDIRECT_BUFFER Indirect compute dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect
        /// command arguments GL_ELEMENT_ARRAY_BUFFER Vertex array indices GL_PIXEL_PACK_BUFFER Pixel read
        /// target GL_PIXEL_UNPACK_BUFFER Texture data source GL_QUERY_BUFFER Query result buffer
        /// GL_SHADER_STORAGE_BUFFER Read-write storage for shaders GL_TEXTURE_BUFFER Texture data buffer
        /// GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="value">
        /// Specifies the name of the buffer object parameter to query.
        /// </param>
        /// <param name="data">
        /// Returns the requested parameter.
        /// </param>
        public static void GetBufferParameteriv(BufferTarget target, BufferParameterName value, [OutAttribute] Int32[] data)
        {
            Delegates.glGetBufferParameteriv(target, value, data);
        }

        /// <summary>
        /// Return parameters of a buffer object.
        /// <para>
        /// These functions return in data a selected parameter of the specified buffer object.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glGetBufferParameteriv and
        /// glGetBufferParameteri64v. Must be one of the buffer binding targets in the following table: Buffer
        /// Binding Target Purpose GL_ARRAY_BUFFER Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter
        /// storage GL_COPY_READ_BUFFER Buffer copy source GL_COPY_WRITE_BUFFER Buffer copy destination
        /// GL_DISPATCH_INDIRECT_BUFFER Indirect compute dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect
        /// command arguments GL_ELEMENT_ARRAY_BUFFER Vertex array indices GL_PIXEL_PACK_BUFFER Pixel read
        /// target GL_PIXEL_UNPACK_BUFFER Texture data source GL_QUERY_BUFFER Query result buffer
        /// GL_SHADER_STORAGE_BUFFER Read-write storage for shaders GL_TEXTURE_BUFFER Texture data buffer
        /// GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="value">
        /// Specifies the name of the buffer object parameter to query.
        /// </param>
        /// <param name="data">
        /// Returns the requested parameter.
        /// </param>
        public static void GetBufferParameteri64v(BufferTarget target, BufferParameterName value, [OutAttribute] Int64[] data)
        {
            Delegates.glGetBufferParameteri64v(target, value, data);
        }

        /// <summary>
        /// Return parameters of a buffer object.
        /// <para>
        /// These functions return in data a selected parameter of the specified buffer object.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glGetNamedBufferParameteriv and
        /// glGetNamedBufferParameteri64v.
        /// </param>
        /// <param name="pname">
        /// </param>
        /// <param name="params">
        /// </param>
        public static void GetNamedBufferParameteriv(UInt32 buffer, BufferParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetNamedBufferParameteriv(buffer, pname, @params);
        }

        /// <summary>
        /// Return parameters of a buffer object.
        /// <para>
        /// These functions return in data a selected parameter of the specified buffer object.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glGetNamedBufferParameteriv and
        /// glGetNamedBufferParameteri64v.
        /// </param>
        /// <param name="pname">
        /// </param>
        /// <param name="params">
        /// </param>
        public static void GetNamedBufferParameteri64v(UInt32 buffer, BufferParameterName pname, [OutAttribute] Int64[] @params)
        {
            Delegates.glGetNamedBufferParameteri64v(buffer, pname, @params);
        }

        /// <summary>
        /// Return the pointer to a mapped buffer object's data store.
        /// <para>
        /// glGetBufferPointerv and glGetNamedBufferPointerv return the buffer pointer pname, which must be
        /// GL_BUFFER_MAP_POINTER. The single buffer map pointer is returned in params. A NULL pointer is
        /// returned if the buffer object's data store is not currently mapped; or if the requesting context did
        /// not map the buffer object's data store, and the implementation is unable to support mappings on
        /// multiple clients.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glGetBufferPointerv, which must be one
        /// of the buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER
        /// Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy
        /// source GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute
        /// dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex
        /// array indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="pname">
        /// Specifies the name of the pointer to be returned. Must be GL_BUFFER_MAP_POINTER.
        /// </param>
        /// <param name="params">
        /// Returns the pointer value specified by pname.
        /// </param>
        public static void GetBufferPointerv(BufferTarget target, BufferPointer pname, [OutAttribute] IntPtr @params)
        {
            Delegates.glGetBufferPointerv(target, pname, @params);
        }

        /// <summary>
        /// Return the pointer to a mapped buffer object's data store.
        /// <para>
        /// glGetBufferPointerv and glGetNamedBufferPointerv return the buffer pointer pname, which must be
        /// GL_BUFFER_MAP_POINTER. The single buffer map pointer is returned in params. A NULL pointer is
        /// returned if the buffer object's data store is not currently mapped; or if the requesting context did
        /// not map the buffer object's data store, and the implementation is unable to support mappings on
        /// multiple clients.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glGetNamedBufferPointerv.
        /// </param>
        /// <param name="pname">
        /// Specifies the name of the pointer to be returned. Must be GL_BUFFER_MAP_POINTER.
        /// </param>
        /// <param name="params">
        /// Returns the pointer value specified by pname.
        /// </param>
        public static void GetNamedBufferPointerv(UInt32 buffer, BufferPointer pname, [OutAttribute] IntPtr @params)
        {
            Delegates.glGetNamedBufferPointerv(buffer, pname, @params);
        }

        /// <summary>
        /// Returns a subset of a buffer object's data store.
        /// <para>
        /// glGetBufferSubData and glGetNamedBufferSubData return some or all of the data contents of the data
        /// store of the specified buffer object. Data starting at byte offset offset and extending for size
        /// bytes is copied from the buffer object's data store to the memory pointed to by data. An error is
        /// thrown if the buffer object is currently mapped, or if offset and size together define a range
        /// beyond the bounds of the buffer object's data store.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glGetBufferSubData, which must be one
        /// of the buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER
        /// Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy
        /// source GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute
        /// dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex
        /// array indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="offset">
        /// Specifies the offset into the buffer object's data store from which data will be returned, measured
        /// in bytes.
        /// </param>
        /// <param name="size">
        /// Specifies the size in bytes of the data store region being returned.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the location where buffer object data is returned.
        /// </param>
        public static void GetBufferSubData(BufferTarget target, IntPtr offset, IntPtr size, [OutAttribute] IntPtr data)
        {
            Delegates.glGetBufferSubData(target, offset, size, data);
        }

        /// <summary>
        /// Returns a subset of a buffer object's data store.
        /// <para>
        /// glGetBufferSubData and glGetNamedBufferSubData return some or all of the data contents of the data
        /// store of the specified buffer object. Data starting at byte offset offset and extending for size
        /// bytes is copied from the buffer object's data store to the memory pointed to by data. An error is
        /// thrown if the buffer object is currently mapped, or if offset and size together define a range
        /// beyond the bounds of the buffer object's data store.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glGetNamedBufferSubData.
        /// </param>
        /// <param name="offset">
        /// Specifies the offset into the buffer object's data store from which data will be returned, measured
        /// in bytes.
        /// </param>
        /// <param name="size">
        /// Specifies the size in bytes of the data store region being returned.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the location where buffer object data is returned.
        /// </param>
        public static void GetNamedBufferSubData(UInt32 buffer, IntPtr offset, Int32 size, [OutAttribute] IntPtr data)
        {
            Delegates.glGetNamedBufferSubData(buffer, offset, size, data);
        }

        /// <summary>
        /// Return a compressed texture image.
        /// <para>
        /// glGetCompressedTexImage and glGetnCompressedTexImage return the compressed texture image associated
        /// with target and lod into pixels. glGetCompressedTextureImage serves the same purpose, but instead of
        /// taking a texture target, it takes the ID of the texture object. pixels should be an array of bufSize
        /// bytes for glGetnCompresedTexImage and glGetCompressedTextureImage functions, and of
        /// GL_TEXTURE_COMPRESSED_IMAGE_SIZE bytes in case of glGetCompressedTexImage. If the actual data takes
        /// less space than bufSize, the remaining bytes will not be touched. target specifies the texture
        /// target, to which the texture the data the function should extract the data from is bound to. lod
        /// specifies the level-of-detail number of the desired image.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetCompressedTexImage and
        /// glGetnCompressedTexImage functions. GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D,
        /// GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, and GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, GL_TEXTURE_RECTANGLE are
        /// accepted.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level
        /// $n$ is the $n$-th mipmap reduction image.
        /// </param>
        /// <param name="pixels">
        /// Returns the compressed texture image.
        /// </param>
        public static void GetCompressedTexImage(TextureTarget target, Int32 level, [OutAttribute] IntPtr pixels)
        {
            Delegates.glGetCompressedTexImage(target, level, pixels);
        }

        /// <summary>
        /// Return a compressed texture image.
        /// <para>
        /// glGetCompressedTexImage and glGetnCompressedTexImage return the compressed texture image associated
        /// with target and lod into pixels. glGetCompressedTextureImage serves the same purpose, but instead of
        /// taking a texture target, it takes the ID of the texture object. pixels should be an array of bufSize
        /// bytes for glGetnCompresedTexImage and glGetCompressedTextureImage functions, and of
        /// GL_TEXTURE_COMPRESSED_IMAGE_SIZE bytes in case of glGetCompressedTexImage. If the actual data takes
        /// less space than bufSize, the remaining bytes will not be touched. target specifies the texture
        /// target, to which the texture the data the function should extract the data from is bound to. lod
        /// specifies the level-of-detail number of the desired image.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetCompressedTexImage and
        /// glGetnCompressedTexImage functions. GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D,
        /// GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, and GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, GL_TEXTURE_RECTANGLE are
        /// accepted.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level
        /// $n$ is the $n$-th mipmap reduction image.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer pixels for glGetCompressedTextureImage and glGetnCompressedTexImage
        /// functions.
        /// </param>
        /// <param name="pixels">
        /// Returns the compressed texture image.
        /// </param>
        public static void GetnCompressedTexImage(TextureTarget target, Int32 level, Int32 bufSize, [OutAttribute] IntPtr pixels)
        {
            Delegates.glGetnCompressedTexImage(target, level, bufSize, pixels);
        }

        /// <summary>
        /// Return a compressed texture image.
        /// <para>
        /// glGetCompressedTexImage and glGetnCompressedTexImage return the compressed texture image associated
        /// with target and lod into pixels. glGetCompressedTextureImage serves the same purpose, but instead of
        /// taking a texture target, it takes the ID of the texture object. pixels should be an array of bufSize
        /// bytes for glGetnCompresedTexImage and glGetCompressedTextureImage functions, and of
        /// GL_TEXTURE_COMPRESSED_IMAGE_SIZE bytes in case of glGetCompressedTexImage. If the actual data takes
        /// less space than bufSize, the remaining bytes will not be touched. target specifies the texture
        /// target, to which the texture the data the function should extract the data from is bound to. lod
        /// specifies the level-of-detail number of the desired image.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glGetCompressedTextureImage function.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level
        /// $n$ is the $n$-th mipmap reduction image.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer pixels for glGetCompressedTextureImage and glGetnCompressedTexImage
        /// functions.
        /// </param>
        /// <param name="pixels">
        /// Returns the compressed texture image.
        /// </param>
        public static void GetCompressedTextureImage(UInt32 texture, Int32 level, Int32 bufSize, [OutAttribute] IntPtr pixels)
        {
            Delegates.glGetCompressedTextureImage(texture, level, bufSize, pixels);
        }

        /// <summary>
        /// Retrieve a sub-region of a compressed texture image from a compressed texture object.
        /// <para>
        /// glGetCompressedTextureSubImage can be used to obtain a sub-region of a compressed texture image
        /// instead of the whole image, as long as the compressed data are arranged into fixed-size blocks of
        /// texels. texture is the name of the texture object, and must not be a buffer or multisample texture.
        /// The effective target is the value of GL_TEXTURE_TARGET for texture. level and pixels have the same
        /// meaning as the corresponding arguments of glCompressedTexSubImage3D. bufSize indicates the size of
        /// the buffer to receive the retrieved pixel data.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the name of the source texture object. Must be GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY,
        /// GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY or
        /// GL_TEXTURE_RECTANGLE. In specific, buffer and multisample textures are not permitted.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level $n$ is the $n$th mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="zoffset">
        /// Specifies a texel offset in the z direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage. Must be a multiple of the compressed block's width,
        /// unless the offset is zero and the size equals the texture image size.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage. Must be a multiple of the compressed block's height,
        /// unless the offset is zero and the size equals the texture image size.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture subimage. Must be a multiple of the compressed block's depth,
        /// unless the offset is zero and the size equals the texture image size.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer to receive the retrieved pixel data.
        /// </param>
        /// <param name="pixels">
        /// Returns the texture subimage. Should be a pointer to an array of the type specified by type.
        /// </param>
        public static void GetCompressedTextureSubImage(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, Int32 bufSize, [OutAttribute] IntPtr pixels)
        {
            Delegates.glGetCompressedTextureSubImage(texture, level, xoffset, yoffset, zoffset, width, height, depth, bufSize, pixels);
        }

        /// <summary>
        /// Return error information.
        /// <para>
        /// glGetError returns the value of the error flag. Each detectable error is assigned a numeric code and
        /// symbolic name. When an error occurs, the error flag is set to the appropriate error code value. No
        /// other errors are recorded until glGetError is called, the error code is returned, and the flag is
        /// reset to GL_NO_ERROR. If a call to glGetError returns GL_NO_ERROR, there has been no detectable
        /// error since the last call to glGetError, or since the GL was initialized.
        /// </para>
        /// </summary>
        public static ErrorCode GetError()
        {
            return Delegates.glGetError();
        }

        /// <summary>
        /// Query the bindings of color indices to user-defined varying out variables.
        /// <para>
        /// glGetFragDataIndex returns the index of the fragment color to which the variable name was bound when
        /// the program object program was last linked. If name is not a varying out variable of program, or if
        /// an error occurs, -1 will be returned.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of the program containing varying out variable whose binding to query
        /// </param>
        /// <param name="name">
        /// The name of the user-defined varying out variable whose index to query
        /// </param>
        public static Int32 GetFragDataIndex(UInt32 program, String name)
        {
            return Delegates.glGetFragDataIndex(program, name);
        }

        /// <summary>
        /// Query the bindings of color numbers to user-defined varying out variables.
        /// <para>
        /// glGetFragDataLocation retrieves the assigned color number binding for the user-defined varying out
        /// variable name for program program. program must have previously been linked. name must be a
        /// null-terminated string. If name is not the name of an active user-defined varying out fragment
        /// shader variable within program, -1 will be returned.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of the program containing varying out variable whose binding to query
        /// </param>
        /// <param name="name">
        /// The name of the user-defined varying out variable whose binding to query
        /// </param>
        public static Int32 GetFragDataLocation(UInt32 program, String name)
        {
            return Delegates.glGetFragDataLocation(program, name);
        }

        /// <summary>
        /// Retrieve information about attachments of a framebuffer object.
        /// <para>
        /// glGetFramebufferAttachmentParameteriv and glGetNamedFramebufferAttachmentParameteriv return
        /// parameters of attachments of a specified framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer object is bound for
        /// glGetFramebufferAttachmentParameteriv.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment of the framebuffer object to query.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of attachment to query.
        /// </param>
        /// <param name="params">
        /// Returns the value of parameter pname for attachment.
        /// </param>
        public static void GetFramebufferAttachmentParameteriv(FramebufferTarget target, FramebufferAttachment attachment, FramebufferParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetFramebufferAttachmentParameteriv(target, attachment, pname, @params);
        }

        /// <summary>
        /// Retrieve information about attachments of a framebuffer object.
        /// <para>
        /// glGetFramebufferAttachmentParameteriv and glGetNamedFramebufferAttachmentParameteriv return
        /// parameters of attachments of a specified framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glGetNamedFramebufferAttachmentParameteriv.
        /// </param>
        /// <param name="attachment">
        /// Specifies the attachment of the framebuffer object to query.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of attachment to query.
        /// </param>
        /// <param name="params">
        /// Returns the value of parameter pname for attachment.
        /// </param>
        public static void GetNamedFramebufferAttachmentParameteriv(UInt32 framebuffer, FramebufferAttachment attachment, FramebufferParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetNamedFramebufferAttachmentParameteriv(framebuffer, attachment, pname, @params);
        }

        /// <summary>
        /// Query a named parameter of a framebuffer object.
        /// <para>
        /// glGetFramebufferParameteriv and glGetNamedFramebufferParameteriv query parameters of a specified
        /// framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer object is bound for glGetFramebufferParameteriv.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of the framebuffer object to query.
        /// </param>
        /// <param name="params">
        /// Returns the value of parameter pname for the framebuffer object.
        /// </param>
        public static void GetFramebufferParameteriv(FramebufferTarget target, FramebufferPName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetFramebufferParameteriv(target, pname, @params);
        }

        /// <summary>
        /// Query a named parameter of a framebuffer object.
        /// <para>
        /// glGetFramebufferParameteriv and glGetNamedFramebufferParameteriv query parameters of a specified
        /// framebuffer object.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glGetNamedFramebufferParameteriv.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of the framebuffer object to query.
        /// </param>
        /// <param name="param">
        /// </param>
        public static void GetNamedFramebufferParameteriv(UInt32 framebuffer, FramebufferPName pname, [OutAttribute] Int32[] param)
        {
            Delegates.glGetNamedFramebufferParameteriv(framebuffer, pname, param);
        }

        /// <summary>
        /// Check if the rendering context has not been lost due to software or hardware issues.
        /// <para>
        /// Certain events can result in a reset of the GL context. Such a reset causes all context state to be
        /// lost and requires the application to recreate all objects in the affected context.
        /// </para>
        /// </summary>
        public static GraphicResetStatus GetGraphicsResetStatus()
        {
            return Delegates.glGetGraphicsResetStatus();
        }

        /// <summary>
        /// Retrieve information about implementation-dependent support for internal formats.
        /// <para>
        /// glGetInternalformativ and glGetInternalformati64v retrieve information about
        /// implementation-dependent support for internal formats. target indicates the target with which the
        /// internal format will be used and must be one of GL_RENDERBUFFER, GL_TEXTURE_2D_MULTISAMPLE, or
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, corresponding to usage as a renderbuffer, two-dimensional
        /// multisample texture or two-dimensional multisample array texture, respectively.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Indicates the usage of the internal format. target must be GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY,
        /// GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY,
        /// GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER, GL_RENDERBUFFER, GL_TEXTURE_2D_MULTISAMPLE or
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format about which to retrieve information.
        /// </param>
        /// <param name="pname">
        /// Specifies the type of information to query.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the maximum number of integers of the specified width that may be written to params by the
        /// function.
        /// </param>
        /// <param name="params">
        /// Specifies the address of a variable into which to write the retrieved information.
        /// </param>
        public static void GetInternalformativ(TextureTarget target, PixelInternalFormat internalFormat, GetPName pname, Int32 bufSize, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetInternalformativ(target, internalFormat, pname, bufSize, @params);
        }

        /// <summary>
        /// Retrieve information about implementation-dependent support for internal formats.
        /// <para>
        /// glGetInternalformativ and glGetInternalformati64v retrieve information about
        /// implementation-dependent support for internal formats. target indicates the target with which the
        /// internal format will be used and must be one of GL_RENDERBUFFER, GL_TEXTURE_2D_MULTISAMPLE, or
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, corresponding to usage as a renderbuffer, two-dimensional
        /// multisample texture or two-dimensional multisample array texture, respectively.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Indicates the usage of the internal format. target must be GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY,
        /// GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY,
        /// GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER, GL_RENDERBUFFER, GL_TEXTURE_2D_MULTISAMPLE or
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format about which to retrieve information.
        /// </param>
        /// <param name="pname">
        /// Specifies the type of information to query.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the maximum number of integers of the specified width that may be written to params by the
        /// function.
        /// </param>
        /// <param name="params">
        /// Specifies the address of a variable into which to write the retrieved information.
        /// </param>
        public static void GetInternalformati64v(TextureTarget target, PixelInternalFormat internalFormat, GetPName pname, Int32 bufSize, [OutAttribute] Int64[] @params)
        {
            Delegates.glGetInternalformati64v(target, internalFormat, pname, bufSize, @params);
        }

        /// <summary>
        /// Retrieve the location of a sample.
        /// <para>
        /// glGetMultisamplefv queries the location of a given sample. pname specifies the sample parameter to
        /// retrieve and must be GL_SAMPLE_POSITION. index corresponds to the sample for which the location
        /// should be returned. The sample location is returned as two floating-point values in val[0] and
        /// val[1], each between 0 and 1, corresponding to the x and y locations respectively in the GL pixel
        /// space of that sample. (0.5, 0.5) this corresponds to the pixel center. index must be between zero
        /// and the value of GL_SAMPLES minus one.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the sample parameter name. pname must be GL_SAMPLE_POSITION.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the sample whose position to query.
        /// </param>
        /// <param name="val">
        /// Specifies the address of an array to receive the position of the sample.
        /// </param>
        public static void GetMultisamplefv(GetMultisamplePName pname, UInt32 index, [OutAttribute] Single[] val)
        {
            Delegates.glGetMultisamplefv(pname, index, val);
        }

        /// <summary>
        /// Retrieve the label of a named object identified within a namespace.
        /// <para>
        /// glGetObjectLabel retrieves the label of the object identified by name within the namespace given by
        /// identifier. identifier must be one of GL_BUFFER, GL_SHADER, GL_PROGRAM, GL_VERTEX_ARRAY, GL_QUERY,
        /// GL_PROGRAM_PIPELINE, GL_TRANSFORM_FEEDBACK, GL_SAMPLER, GL_TEXTURE, GL_RENDERBUFFER, GL_FRAMEBUFFER,
        /// to indicate the namespace containing the names of buffers, shaders, programs, vertex array objects,
        /// query objects, program pipelines, transform feedback objects, samplers, textures, renderbuffers and
        /// frame buffers, respectively.
        /// </para>
        /// </summary>
        /// <param name="identifier">
        /// The namespace from which the name of the object is allocated.
        /// </param>
        /// <param name="name">
        /// The name of the object whose label to retrieve.
        /// </param>
        /// <param name="bufSize">
        /// The length of the buffer whose address is in label.
        /// </param>
        /// <param name="length">
        /// The address of a variable to receive the length of the object label.
        /// </param>
        /// <param name="label">
        /// The address of a string that will receive the object label.
        /// </param>
        public static void GetObjectLabel(ObjectLabelEnum identifier, UInt32 name, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder label)
        {
            Delegates.glGetObjectLabel(identifier, name, bufSize, length, label);
        }

        /// <summary>
        /// Retrieve the label of a sync object identified by a pointer.
        /// <para>
        /// glGetObjectPtrLabel retrieves the label of the sync object identified by ptr.
        /// </para>
        /// </summary>
        /// <param name="ptr">
        /// The name of the sync object whose label to retrieve.
        /// </param>
        /// <param name="bufSize">
        /// The length of the buffer whose address is in label.
        /// </param>
        /// <param name="length">
        /// The address of a variable to receive the length of the object label.
        /// </param>
        /// <param name="label">
        /// The address of a string that will receive the object label.
        /// </param>
        public static void GetObjectPtrLabel([OutAttribute] IntPtr ptr, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder label)
        {
            Delegates.glGetObjectPtrLabel(ptr, bufSize, length, label);
        }

        /// <summary>
        /// Return the address of the specified pointer.
        /// <para>
        /// glGetPointerv returns pointer information. pname indicates the pointer to be returned, and params is
        /// a pointer to a location in which to place the returned data. The parameters that may be queried
        /// include:.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the pointer to be returned. Must be one of GL_DEBUG_CALLBACK_FUNCTION or
        /// GL_DEBUG_CALLBACK_USER_PARAM.
        /// </param>
        /// <param name="params">
        /// Returns the pointer value specified by pname.
        /// </param>
        public static void GetPointerv(GetPointerParameter pname, [OutAttribute] IntPtr @params)
        {
            Delegates.glGetPointerv(pname, @params);
        }

        /// <summary>
        /// Returns a parameter from a program object.
        /// <para>
        /// glGetProgram returns in params the value of a parameter for a specific program object. The following
        /// parameters are defined:.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the object parameter. Accepted symbolic names are GL_DELETE_STATUS, GL_LINK_STATUS,
        /// GL_VALIDATE_STATUS, GL_INFO_LOG_LENGTH, GL_ATTACHED_SHADERS, GL_ACTIVE_ATOMIC_COUNTER_BUFFERS,
        /// GL_ACTIVE_ATTRIBUTES, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH, GL_ACTIVE_UNIFORMS, GL_ACTIVE_UNIFORM_BLOCKS,
        /// GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH, GL_ACTIVE_UNIFORM_MAX_LENGTH, GL_COMPUTE_WORK_GROUP_SIZE
        /// GL_PROGRAM_BINARY_LENGTH, GL_TRANSFORM_FEEDBACK_BUFFER_MODE, GL_TRANSFORM_FEEDBACK_VARYINGS,
        /// GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH, GL_GEOMETRY_VERTICES_OUT, GL_GEOMETRY_INPUT_TYPE, and
        /// GL_GEOMETRY_OUTPUT_TYPE.
        /// </param>
        /// <param name="params">
        /// Returns the requested object parameter.
        /// </param>
        public static void GetProgramiv(UInt32 program, ProgramParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetProgramiv(program, pname, @params);
        }

        /// <summary>
        /// Return a binary representation of a program object's compiled and linked executable source.
        /// <para>
        /// glGetProgramBinary returns a binary representation of the compiled and linked executable for program
        /// into the array of bytes whose address is specified in binary. The maximum number of bytes that may
        /// be written into binary is specified by bufSize. If the program binary is greater in size than
        /// bufSize bytes, then an error is generated, otherwise the actual number of bytes written into binary
        /// is returned in the variable whose address is given by length. If length is NULL, then no length is
        /// returned.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of a program object whose binary representation to retrieve.
        /// </param>
        /// <param name="bufsize">
        /// Specifies the size of the buffer whose address is given by binary.
        /// </param>
        /// <param name="length">
        /// Specifies the address of a variable to receive the number of bytes written into binary.
        /// </param>
        /// <param name="binaryFormat">
        /// Specifies the address of a variable to receive a token indicating the format of the binary data
        /// returned by the GL.
        /// </param>
        /// <param name="binary">
        /// Specifies the address an array into which the GL will return program's binary representation.
        /// </param>
        public static void GetProgramBinary(UInt32 program, Int32 bufsize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] binaryFormat, [OutAttribute] IntPtr binary)
        {
            Delegates.glGetProgramBinary(program, bufsize, length, binaryFormat, binary);
        }

        /// <summary>
        /// Returns the information log for a program object.
        /// <para>
        /// glGetProgramInfoLog returns the information log for the specified program object. The information
        /// log for a program object is modified when the program object is linked or validated. The string that
        /// is returned will be null terminated.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object whose information log is to be queried.
        /// </param>
        /// <param name="maxLength">
        /// Specifies the size of the character buffer for storing the returned information log.
        /// </param>
        /// <param name="length">
        /// Returns the length of the string returned in infoLog (excluding the null terminator).
        /// </param>
        /// <param name="infoLog">
        /// Specifies an array of characters that is used to return the information log.
        /// </param>
        public static void GetProgramInfoLog(UInt32 program, Int32 maxLength, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder infoLog)
        {
            Delegates.glGetProgramInfoLog(program, maxLength, length, infoLog);
        }

        /// <summary>
        /// Query a property of an interface in a program.
        /// <para>
        /// glGetProgramInterfaceiv queries the property of the interface identifed by programInterface in
        /// program, the property name of which is given by pname.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of a program object whose interface to query.
        /// </param>
        /// <param name="programInterface">
        /// A token identifying the interface within program to query.
        /// </param>
        /// <param name="pname">
        /// The name of the parameter within programInterface to query.
        /// </param>
        /// <param name="params">
        /// The address of a variable to retrieve the value of pname for the program interface.
        /// </param>
        public static void GetProgramInterfaceiv(UInt32 program, ProgramInterface programInterface, ProgramInterfaceParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetProgramInterfaceiv(program, programInterface, pname, @params);
        }

        /// <summary>
        /// Retrieve properties of a program pipeline object.
        /// <para>
        /// glGetProgramPipelineiv retrieves the value of a property of the program pipeline object pipeline.
        /// pname specifies the name of the parameter whose value to retrieve. The value of the parameter is
        /// written to the variable whose address is given by params.
        /// </para>
        /// </summary>
        /// <param name="pipeline">
        /// Specifies the name of a program pipeline object whose parameter retrieve.
        /// </param>
        /// <param name="pname">
        /// Specifies the name of the parameter to retrieve.
        /// </param>
        /// <param name="params">
        /// Specifies the address of a variable into which will be written the value or values of pname for
        /// pipeline.
        /// </param>
        public static void GetProgramPipelineiv(UInt32 pipeline, Int32 pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetProgramPipelineiv(pipeline, pname, @params);
        }

        /// <summary>
        /// Retrieve the info log string from a program pipeline object.
        /// <para>
        /// glGetProgramPipelineInfoLog retrieves the info log for the program pipeline object pipeline. The
        /// info log, including its null terminator, is written into the array of characters whose address is
        /// given by infoLog. The maximum number of characters that may be written into infoLog is given by
        /// bufSize, and the actual number of characters written into infoLog is returned in the integer whose
        /// address is given by length. If length is NULL, no length is returned.
        /// </para>
        /// </summary>
        /// <param name="pipeline">
        /// Specifies the name of a program pipeline object from which to retrieve the info log.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the maximum number of characters, including the null terminator, that may be written into
        /// infoLog.
        /// </param>
        /// <param name="length">
        /// Specifies the address of a variable into which will be written the number of characters written into
        /// infoLog.
        /// </param>
        /// <param name="infoLog">
        /// Specifies the address of an array of characters into which will be written the info log for
        /// pipeline.
        /// </param>
        public static void GetProgramPipelineInfoLog(UInt32 pipeline, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder infoLog)
        {
            Delegates.glGetProgramPipelineInfoLog(pipeline, bufSize, length, infoLog);
        }

        /// <summary>
        /// Retrieve values for multiple properties of a single active resource within a program object.
        /// <para>
        /// glGetProgramResourceiv returns values for multiple properties of a single active resource with an
        /// index of index in the interface programInterface of program object program. For each resource,
        /// values for propCount properties specified by the array props are returned. propCount may not be
        /// zero. An error is generated if any value in props is not one of the properties described immediately
        /// belowor if any value in props is not allowed for programInterface. The set of allowed
        /// programInterface values for each property can be found in the following table:.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of a program object whose resources to query.
        /// </param>
        /// <param name="programInterface">
        /// A token identifying the interface within program containing the resource named name.
        /// </param>
        /// <param name="index">
        /// </param>
        /// <param name="propCount">
        /// </param>
        /// <param name="props">
        /// </param>
        /// <param name="bufSize">
        /// </param>
        /// <param name="length">
        /// </param>
        /// <param name="params">
        /// </param>
        public static void GetProgramResourceiv(UInt32 program, ProgramInterface programInterface, UInt32 index, Int32 propCount, [OutAttribute] ProgramResourceParameterName[] props, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetProgramResourceiv(program, programInterface, index, propCount, props, bufSize, length, @params);
        }

        /// <summary>
        /// Query the index of a named resource within a program.
        /// <para>
        /// glGetProgramResourceIndex returns the unsigned integer index assigned to a resource named name in
        /// the interface type programInterface of program object program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of a program object whose resources to query.
        /// </param>
        /// <param name="programInterface">
        /// A token identifying the interface within program containing the resource named name.
        /// </param>
        /// <param name="name">
        /// The name of the resource to query the index of.
        /// </param>
        public static UInt32 GetProgramResourceIndex(UInt32 program, ProgramInterface programInterface, String name)
        {
            return Delegates.glGetProgramResourceIndex(program, programInterface, name);
        }

        /// <summary>
        /// Query the location of a named resource within a program.
        /// <para>
        /// glGetProgramResourceLocation returns the location assigned to the variable named name in interface
        /// programInterface of program object program. program must be the name of a program that has been
        /// linked successfully. programInterface must be one of GL_UNIFORM, GL_PROGRAM_INPUT,
        /// GL_PROGRAM_OUTPUT, GL_VERTEX_SUBROUTINE_UNIFORM, GL_TESS_CONTROL_SUBROUTINE_UNIFORM,
        /// GL_TESS_EVALUATION_SUBROUTINE_UNIFORM, GL_GEOMETRY_SUBROUTINE_UNIFORM,
        /// GL_FRAGMENT_SUBROUTINE_UNIFORM, GL_COMPUTE_SUBROUTINE_UNIFORM, or GL_TRANSFORM_FEEDBACK_BUFFER.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of a program object whose resources to query.
        /// </param>
        /// <param name="programInterface">
        /// A token identifying the interface within program containing the resource named name.
        /// </param>
        /// <param name="name">
        /// The name of the resource to query the location of.
        /// </param>
        public static Int32 GetProgramResourceLocation(UInt32 program, ProgramInterface programInterface, String name)
        {
            return Delegates.glGetProgramResourceLocation(program, programInterface, name);
        }

        /// <summary>
        /// Query the fragment color index of a named variable within a program.
        /// <para>
        /// glGetProgramResourceLocationIndex returns the fragment color index assigned to the variable named
        /// name in interface programInterface of program object program. program must be the name of a program
        /// that has been linked successfully. programInterface must be GL_PROGRAM_OUTPUT.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of a program object whose resources to query.
        /// </param>
        /// <param name="programInterface">
        /// A token identifying the interface within program containing the resource named name.
        /// </param>
        /// <param name="name">
        /// The name of the resource to query the location of.
        /// </param>
        public static Int32 GetProgramResourceLocationIndex(UInt32 program, ProgramInterface programInterface, String name)
        {
            return Delegates.glGetProgramResourceLocationIndex(program, programInterface, name);
        }

        /// <summary>
        /// Query the name of an indexed resource within a program.
        /// <para>
        /// glGetProgramResourceName retrieves the name string assigned to the single active resource with an
        /// index of index in the interface programInterface of program object program. index must be less than
        /// the number of entries in the active resource list for programInterface.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of a program object whose resources to query.
        /// </param>
        /// <param name="programInterface">
        /// A token identifying the interface within program containing the indexed resource.
        /// </param>
        /// <param name="index">
        /// The index of the resource within programInterface of program.
        /// </param>
        /// <param name="bufSize">
        /// The size of the character array whose address is given by name.
        /// </param>
        /// <param name="length">
        /// The address of a variable which will receive the length of the resource name.
        /// </param>
        /// <param name="name">
        /// The address of a character array into which will be written the name of the resource.
        /// </param>
        public static void GetProgramResourceName(UInt32 program, ProgramInterface programInterface, UInt32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder name)
        {
            Delegates.glGetProgramResourceName(program, programInterface, index, bufSize, length, name);
        }

        /// <summary>
        /// Retrieve properties of a program object corresponding to a specified shader stage.
        /// <para>
        /// glGetProgramStage queries a parameter of a shader stage attached to a program object. program
        /// contains the name of the program to which the shader is attached. shadertype specifies the stage
        /// from which to query the parameter. pname specifies which parameter should be queried. The value or
        /// values of the parameter to be queried is returned in the variable whose address is given in values.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of the program containing shader stage.
        /// </param>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for the subroutine parameter. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of the shader to query. pname must be GL_ACTIVE_SUBROUTINE_UNIFORMS,
        /// GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS, GL_ACTIVE_SUBROUTINES,
        /// GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH, or GL_ACTIVE_SUBROUTINE_MAX_LENGTH.
        /// </param>
        /// <param name="values">
        /// Specifies the address of a variable into which the queried value or values will be placed.
        /// </param>
        public static void GetProgramStageiv(UInt32 program, ShaderType shadertype, ProgramStageParameterName pname, [OutAttribute] Int32[] values)
        {
            Delegates.glGetProgramStageiv(program, shadertype, pname, values);
        }

        /// <summary>
        /// Return parameters of an indexed query object target.
        /// <para>
        /// glGetQueryIndexediv returns in params a selected parameter of the indexed query object target
        /// specified by target and index. index specifies the index of the query object target and must be
        /// between zero and a target-specific maxiumum.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies a query object target. Must be GL_SAMPLES_PASSED, GL_ANY_SAMPLES_PASSED,
        /// GL_ANY_SAMPLES_PASSED_CONSERVATIVE GL_PRIMITIVES_GENERATED,
        /// GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, GL_TIME_ELAPSED, or GL_TIMESTAMP.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the query object target.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a query object target parameter. Accepted values are GL_CURRENT_QUERY
        /// or GL_QUERY_COUNTER_BITS.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetQueryIndexediv(QueryTarget target, UInt32 index, GetQueryParam pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetQueryIndexediv(target, index, pname, @params);
        }

        /// <summary>
        /// Return parameters of a query object target.
        /// <para>
        /// glGetQueryiv returns in params a selected parameter of the query object target specified by target.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies a query object target. Must be GL_SAMPLES_PASSED, GL_ANY_SAMPLES_PASSED,
        /// GL_ANY_SAMPLES_PASSED_CONSERVATIVE GL_PRIMITIVES_GENERATED,
        /// GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, GL_TIME_ELAPSED, or GL_TIMESTAMP.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a query object target parameter. Accepted values are GL_CURRENT_QUERY
        /// or GL_QUERY_COUNTER_BITS.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetQueryiv(QueryTarget target, GetQueryParam pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetQueryiv(target, pname, @params);
        }

        /// <summary>
        /// Return parameters of a query object.
        /// <para>
        /// glGetQueryObject returns in params a selected parameter of the query object specified by id.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specifies the name of a query object.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a query object parameter. Accepted values are GL_QUERY_RESULT or
        /// GL_QUERY_RESULT_AVAILABLE.
        /// </param>
        /// <param name="params">
        /// If a buffer is bound to the GL_QUERY_RESULT_BUFFER target, then params is treated as an offset to a
        /// location within that buffer's data store to receive the result of the query. If no buffer is bound
        /// to GL_QUERY_RESULT_BUFFER, then params is treated as an address in client memory of a variable to
        /// receive the resulting data.
        /// </param>
        public static void GetQueryObjectiv(UInt32 id, GetQueryObjectParam pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetQueryObjectiv(id, pname, @params);
        }

        /// <summary>
        /// Return parameters of a query object.
        /// <para>
        /// glGetQueryObject returns in params a selected parameter of the query object specified by id.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specifies the name of a query object.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a query object parameter. Accepted values are GL_QUERY_RESULT or
        /// GL_QUERY_RESULT_AVAILABLE.
        /// </param>
        /// <param name="params">
        /// If a buffer is bound to the GL_QUERY_RESULT_BUFFER target, then params is treated as an offset to a
        /// location within that buffer's data store to receive the result of the query. If no buffer is bound
        /// to GL_QUERY_RESULT_BUFFER, then params is treated as an address in client memory of a variable to
        /// receive the resulting data.
        /// </param>
        public static void GetQueryObjectuiv(UInt32 id, GetQueryObjectParam pname, [OutAttribute] UInt32[] @params)
        {
            Delegates.glGetQueryObjectuiv(id, pname, @params);
        }

        /// <summary>
        /// Return parameters of a query object.
        /// <para>
        /// glGetQueryObject returns in params a selected parameter of the query object specified by id.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specifies the name of a query object.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a query object parameter. Accepted values are GL_QUERY_RESULT or
        /// GL_QUERY_RESULT_AVAILABLE.
        /// </param>
        /// <param name="params">
        /// If a buffer is bound to the GL_QUERY_RESULT_BUFFER target, then params is treated as an offset to a
        /// location within that buffer's data store to receive the result of the query. If no buffer is bound
        /// to GL_QUERY_RESULT_BUFFER, then params is treated as an address in client memory of a variable to
        /// receive the resulting data.
        /// </param>
        public static void GetQueryObjecti64v(UInt32 id, GetQueryObjectParam pname, [OutAttribute] Int64[] @params)
        {
            Delegates.glGetQueryObjecti64v(id, pname, @params);
        }

        /// <summary>
        /// Return parameters of a query object.
        /// <para>
        /// glGetQueryObject returns in params a selected parameter of the query object specified by id.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specifies the name of a query object.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a query object parameter. Accepted values are GL_QUERY_RESULT or
        /// GL_QUERY_RESULT_AVAILABLE.
        /// </param>
        /// <param name="params">
        /// If a buffer is bound to the GL_QUERY_RESULT_BUFFER target, then params is treated as an offset to a
        /// location within that buffer's data store to receive the result of the query. If no buffer is bound
        /// to GL_QUERY_RESULT_BUFFER, then params is treated as an address in client memory of a variable to
        /// receive the resulting data.
        /// </param>
        public static void GetQueryObjectui64v(UInt32 id, GetQueryObjectParam pname, [OutAttribute] UInt64[] @params)
        {
            Delegates.glGetQueryObjectui64v(id, pname, @params);
        }

        /// <summary>
        /// Query a named parameter of a renderbuffer object.
        /// <para>
        /// glGetRenderbufferParameteriv and glGetNamedRenderbufferParameteriv query parameters of a specified
        /// renderbuffer object.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the renderbuffer object is bound for glGetRenderbufferParameteriv.
        /// target must be GL_RENDERBUFFER.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of the renderbuffer object to query.
        /// </param>
        /// <param name="params">
        /// Returns the value of parameter pname for the renderbuffer object.
        /// </param>
        public static void GetRenderbufferParameteriv(RenderbufferTarget target, RenderbufferParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetRenderbufferParameteriv(target, pname, @params);
        }

        /// <summary>
        /// Query a named parameter of a renderbuffer object.
        /// <para>
        /// glGetRenderbufferParameteriv and glGetNamedRenderbufferParameteriv query parameters of a specified
        /// renderbuffer object.
        /// </para>
        /// </summary>
        /// <param name="renderbuffer">
        /// Specifies the name of the renderbuffer object for glGetNamedRenderbufferParameteriv.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter of the renderbuffer object to query.
        /// </param>
        /// <param name="params">
        /// Returns the value of parameter pname for the renderbuffer object.
        /// </param>
        public static void GetNamedRenderbufferParameteriv(UInt32 renderbuffer, RenderbufferParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetNamedRenderbufferParameteriv(renderbuffer, pname, @params);
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
        public static void GetSamplerParameterfv(UInt32 sampler, TextureParameterName pname, [OutAttribute] Single[] @params)
        {
            Delegates.glGetSamplerParameterfv(sampler, pname, @params);
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
        public static void GetSamplerParameteriv(UInt32 sampler, TextureParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetSamplerParameteriv(sampler, pname, @params);
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
        public static void GetSamplerParameterIiv(UInt32 sampler, TextureParameterName pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetSamplerParameterIiv(sampler, pname, @params);
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
        public static void GetSamplerParameterIuiv(UInt32 sampler, TextureParameterName pname, [OutAttribute] UInt32[] @params)
        {
            Delegates.glGetSamplerParameterIuiv(sampler, pname, @params);
        }

        /// <summary>
        /// Returns a parameter from a shader object.
        /// <para>
        /// glGetShader returns in params the value of a parameter for a specific shader object. The following
        /// parameters are defined:.
        /// </para>
        /// </summary>
        /// <param name="shader">
        /// Specifies the shader object to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the object parameter. Accepted symbolic names are GL_SHADER_TYPE, GL_DELETE_STATUS,
        /// GL_COMPILE_STATUS, GL_INFO_LOG_LENGTH, GL_SHADER_SOURCE_LENGTH.
        /// </param>
        /// <param name="params">
        /// Returns the requested object parameter.
        /// </param>
        public static void GetShaderiv(UInt32 shader, ShaderParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetShaderiv(shader, pname, @params);
        }

        /// <summary>
        /// Returns the information log for a shader object.
        /// <para>
        /// glGetShaderInfoLog returns the information log for the specified shader object. The information log
        /// for a shader object is modified when the shader is compiled. The string that is returned will be
        /// null terminated.
        /// </para>
        /// </summary>
        /// <param name="shader">
        /// Specifies the shader object whose information log is to be queried.
        /// </param>
        /// <param name="maxLength">
        /// Specifies the size of the character buffer for storing the returned information log.
        /// </param>
        /// <param name="length">
        /// Returns the length of the string returned in infoLog (excluding the null terminator).
        /// </param>
        /// <param name="infoLog">
        /// Specifies an array of characters that is used to return the information log.
        /// </param>
        public static void GetShaderInfoLog(UInt32 shader, Int32 maxLength, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder infoLog)
        {
            Delegates.glGetShaderInfoLog(shader, maxLength, length, infoLog);
        }

        /// <summary>
        /// Retrieve the range and precision for numeric formats supported by the shader compiler.
        /// <para>
        /// glGetShaderPrecisionFormat retrieves the numeric range and precision for the implementation's
        /// representation of quantities in different numeric formats in specified shader type. shaderType
        /// specifies the type of shader for which the numeric precision and range is to be retrieved and must
        /// be one of GL_VERTEX_SHADER or GL_FRAGMENT_SHADER. precisionType specifies the numeric format to
        /// query and must be one of GL_LOW_FLOAT, GL_MEDIUM_FLOAT GL_HIGH_FLOAT, GL_LOW_INT, GL_MEDIUM_INT, or
        /// GL_HIGH_INT.
        /// </para>
        /// </summary>
        /// <param name="shaderType">
        /// Specifies the type of shader whose precision to query. shaderType must be GL_VERTEX_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="precisionType">
        /// Specifies the numeric format whose precision and range to query.
        /// </param>
        /// <param name="range">
        /// Specifies the address of array of two integers into which encodings of the implementation's numeric
        /// range are returned.
        /// </param>
        /// <param name="precision">
        /// Specifies the address of an integer into which the numeric precision of the implementation is
        /// written.
        /// </param>
        public static void GetShaderPrecisionFormat(ShaderType shaderType, Int32 precisionType, [OutAttribute] Int32[] range, [OutAttribute] Int32[] precision)
        {
            Delegates.glGetShaderPrecisionFormat(shaderType, precisionType, range, precision);
        }

        /// <summary>
        /// Returns the source code string from a shader object.
        /// <para>
        /// glGetShaderSource returns the concatenation of the source code strings from the shader object
        /// specified by shader. The source code strings for a shader object are the result of a previous call
        /// to glShaderSource. The string returned by the function will be null terminated.
        /// </para>
        /// </summary>
        /// <param name="shader">
        /// Specifies the shader object to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the character buffer for storing the returned source code string.
        /// </param>
        /// <param name="length">
        /// Returns the length of the string returned in source (excluding the null terminator).
        /// </param>
        /// <param name="source">
        /// Specifies an array of characters that is used to return the source code string.
        /// </param>
        public static void GetShaderSource(UInt32 shader, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] System.Text.StringBuilder source)
        {
            Delegates.glGetShaderSource(shader, bufSize, length, source);
        }

        /// <summary>
        /// Return a string describing the current GL connection.
        /// <para>
        /// glGetString returns a pointer to a static string describing some aspect of the current GL
        /// connection. name can be one of the following:.
        /// </para>
        /// </summary>
        /// <param name="name">
        /// Specifies a symbolic constant, one of GL_VENDOR, GL_RENDERER, GL_VERSION, or
        /// GL_SHADING_LANGUAGE_VERSION. Additionally, glGetStringi accepts the GL_EXTENSIONS token.
        /// </param>
        public static String GetString(StringName name)
        {
            return System.Runtime.InteropServices.Marshal.PtrToStringAnsi(Delegates.glGetString(name));
        }

        /// <summary>
        /// Return a string describing the current GL connection.
        /// <para>
        /// glGetString returns a pointer to a static string describing some aspect of the current GL
        /// connection. name can be one of the following:.
        /// </para>
        /// </summary>
        /// <param name="name">
        /// Specifies a symbolic constant, one of GL_VENDOR, GL_RENDERER, GL_VERSION, or
        /// GL_SHADING_LANGUAGE_VERSION. Additionally, glGetStringi accepts the GL_EXTENSIONS token.
        /// </param>
        /// <param name="index">
        /// For glGetStringi, specifies the index of the string to return.
        /// </param>
        public static String GetStringi(StringName name, UInt32 index)
        {
            return System.Runtime.InteropServices.Marshal.PtrToStringAnsi(Delegates.glGetStringi(name, index));
        }

        /// <summary>
        /// Retrieve the index of a subroutine uniform of a given shader stage within a program.
        /// <para>
        /// glGetSubroutineIndex returns the index of a subroutine uniform within a shader stage attached to a
        /// program object. program contains the name of the program to which the shader is attached. shadertype
        /// specifies the stage from which to query shader subroutine index. name contains the null-terminated
        /// name of the subroutine uniform whose name to query.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of the program containing shader stage.
        /// </param>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for subroutine uniform index. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="name">
        /// Specifies the name of the subroutine uniform whose index to query.
        /// </param>
        public static UInt32 GetSubroutineIndex(UInt32 program, ShaderType shadertype, String name)
        {
            return Delegates.glGetSubroutineIndex(program, shadertype, name);
        }

        /// <summary>
        /// Retrieve the location of a subroutine uniform of a given shader stage within a program.
        /// <para>
        /// glGetSubroutineUniformLocation returns the location of the subroutine uniform variable name in the
        /// shader stage of type shadertype attached to program, with behavior otherwise identical to
        /// glGetUniformLocation.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of the program containing shader stage.
        /// </param>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for subroutine uniform index. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="name">
        /// Specifies the name of the subroutine uniform whose index to query.
        /// </param>
        public static Int32 GetSubroutineUniformLocation(UInt32 program, ShaderType shadertype, String name)
        {
            return Delegates.glGetSubroutineUniformLocation(program, shadertype, name);
        }

        /// <summary>
        /// Query the properties of a sync object.
        /// <para>
        /// glGetSynciv retrieves properties of a sync object. sync specifies the name of the sync object whose
        /// properties to retrieve.
        /// </para>
        /// </summary>
        /// <param name="sync">
        /// Specifies the sync object whose properties to query.
        /// </param>
        /// <param name="pname">
        /// Specifies the parameter whose value to retrieve from the sync object specified in sync.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer whose address is given in values.
        /// </param>
        /// <param name="length">
        /// Specifies the address of an variable to receive the number of integers placed in values.
        /// </param>
        /// <param name="values">
        /// Specifies the address of an array to receive the values of the queried parameter.
        /// </param>
        public static void GetSynciv(IntPtr sync, ArbSync pname, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] values)
        {
            Delegates.glGetSynciv(sync, pname, bufSize, length, values);
        }

        /// <summary>
        /// Return a texture image.
        /// <para>
        /// glGetTexImage, glGetnTexImage and glGetTextureImage functions return a texture image into pixels.
        /// For glGetTexImage and glGetnTexImage, target specifies whether the desired texture image is one
        /// specified by glTexImage1D (GL_TEXTURE_1D), glTexImage2D (GL_TEXTURE_1D_ARRAY, GL_TEXTURE_RECTANGLE,
        /// GL_TEXTURE_2D or any of GL_TEXTURE_CUBE_MAP_*), or glTexImage3D (GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D,
        /// GL_TEXTURE_CUBE_MAP_ARRAY). For glGetTextureImage, texture specifies the texture object name. In
        /// addition to types of textures accepted by glGetTexImage and glGetnTexImage, the function also
        /// accepts cube map texture objects (with effective target GL_TEXTURE_CUBE_MAP). level specifies the
        /// level-of-detail number of the desired image. format and type specify the format and type of the
        /// desired image array. See the reference page for glTexImage1D for a description of the acceptable
        /// values for the format and type parameters, respectively. For glGetnTexImage and glGetTextureImage
        /// functions, bufSize tells the size of the buffer to receive the retrieved pixel data. glGetnTexImage
        /// and glGetTextureImage do not write more than bufSize bytes into pixels.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetTexImage and glGetnTexImage functions.
        /// GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
        /// GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, and GL_TEXTURE_CUBE_MAP_ARRAY are acceptable.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level n
        /// is the nth mipmap reduction image.
        /// </param>
        /// <param name="format">
        /// Specifies a pixel format for the returned data. The supported formats are GL_STENCIL_INDEX,
        /// GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL, GL_RED, GL_GREEN, GL_BLUE, GL_RG, GL_RGB, GL_RGBA, GL_BGR,
        /// GL_BGRA, GL_RED_INTEGER, GL_GREEN_INTEGER, GL_BLUE_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER,
        /// GL_RGBA_INTEGER, GL_BGR_INTEGER, GL_BGRA_INTEGER.
        /// </param>
        /// <param name="type">
        /// Specifies a pixel type for the returned data. The supported types are GL_UNSIGNED_BYTE, GL_BYTE,
        /// GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_HALF_FLOAT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, GL_UNSIGNED_INT_2_10_10_10_REV,
        /// GL_UNSIGNED_INT_24_8, GL_UNSIGNED_INT_10F_11F_11F_REV, GL_UNSIGNED_INT_5_9_9_9_REV, and
        /// GL_FLOAT_32_UNSIGNED_INT_24_8_REV.
        /// </param>
        /// <param name="pixels">
        /// Returns the texture image. Should be a pointer to an array of the type specified by type.
        /// </param>
        public static void GetTexImage(TextureTarget target, Int32 level, PixelFormat format, PixelType type, [OutAttribute] IntPtr pixels)
        {
            Delegates.glGetTexImage(target, level, format, type, pixels);
        }

        /// <summary>
        /// Return a texture image.
        /// <para>
        /// glGetTexImage, glGetnTexImage and glGetTextureImage functions return a texture image into pixels.
        /// For glGetTexImage and glGetnTexImage, target specifies whether the desired texture image is one
        /// specified by glTexImage1D (GL_TEXTURE_1D), glTexImage2D (GL_TEXTURE_1D_ARRAY, GL_TEXTURE_RECTANGLE,
        /// GL_TEXTURE_2D or any of GL_TEXTURE_CUBE_MAP_*), or glTexImage3D (GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D,
        /// GL_TEXTURE_CUBE_MAP_ARRAY). For glGetTextureImage, texture specifies the texture object name. In
        /// addition to types of textures accepted by glGetTexImage and glGetnTexImage, the function also
        /// accepts cube map texture objects (with effective target GL_TEXTURE_CUBE_MAP). level specifies the
        /// level-of-detail number of the desired image. format and type specify the format and type of the
        /// desired image array. See the reference page for glTexImage1D for a description of the acceptable
        /// values for the format and type parameters, respectively. For glGetnTexImage and glGetTextureImage
        /// functions, bufSize tells the size of the buffer to receive the retrieved pixel data. glGetnTexImage
        /// and glGetTextureImage do not write more than bufSize bytes into pixels.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetTexImage and glGetnTexImage functions.
        /// GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
        /// GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, and GL_TEXTURE_CUBE_MAP_ARRAY are acceptable.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level n
        /// is the nth mipmap reduction image.
        /// </param>
        /// <param name="format">
        /// Specifies a pixel format for the returned data. The supported formats are GL_STENCIL_INDEX,
        /// GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL, GL_RED, GL_GREEN, GL_BLUE, GL_RG, GL_RGB, GL_RGBA, GL_BGR,
        /// GL_BGRA, GL_RED_INTEGER, GL_GREEN_INTEGER, GL_BLUE_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER,
        /// GL_RGBA_INTEGER, GL_BGR_INTEGER, GL_BGRA_INTEGER.
        /// </param>
        /// <param name="type">
        /// Specifies a pixel type for the returned data. The supported types are GL_UNSIGNED_BYTE, GL_BYTE,
        /// GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_HALF_FLOAT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, GL_UNSIGNED_INT_2_10_10_10_REV,
        /// GL_UNSIGNED_INT_24_8, GL_UNSIGNED_INT_10F_11F_11F_REV, GL_UNSIGNED_INT_5_9_9_9_REV, and
        /// GL_FLOAT_32_UNSIGNED_INT_24_8_REV.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer pixels for glGetnTexImage and glGetTextureImage functions.
        /// </param>
        /// <param name="pixels">
        /// Returns the texture image. Should be a pointer to an array of the type specified by type.
        /// </param>
        public static void GetnTexImage(TextureTarget target, Int32 level, PixelFormat format, PixelType type, Int32 bufSize, [OutAttribute] IntPtr pixels)
        {
            Delegates.glGetnTexImage(target, level, format, type, bufSize, pixels);
        }

        /// <summary>
        /// Return a texture image.
        /// <para>
        /// glGetTexImage, glGetnTexImage and glGetTextureImage functions return a texture image into pixels.
        /// For glGetTexImage and glGetnTexImage, target specifies whether the desired texture image is one
        /// specified by glTexImage1D (GL_TEXTURE_1D), glTexImage2D (GL_TEXTURE_1D_ARRAY, GL_TEXTURE_RECTANGLE,
        /// GL_TEXTURE_2D or any of GL_TEXTURE_CUBE_MAP_*), or glTexImage3D (GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D,
        /// GL_TEXTURE_CUBE_MAP_ARRAY). For glGetTextureImage, texture specifies the texture object name. In
        /// addition to types of textures accepted by glGetTexImage and glGetnTexImage, the function also
        /// accepts cube map texture objects (with effective target GL_TEXTURE_CUBE_MAP). level specifies the
        /// level-of-detail number of the desired image. format and type specify the format and type of the
        /// desired image array. See the reference page for glTexImage1D for a description of the acceptable
        /// values for the format and type parameters, respectively. For glGetnTexImage and glGetTextureImage
        /// functions, bufSize tells the size of the buffer to receive the retrieved pixel data. glGetnTexImage
        /// and glGetTextureImage do not write more than bufSize bytes into pixels.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level n
        /// is the nth mipmap reduction image.
        /// </param>
        /// <param name="format">
        /// Specifies a pixel format for the returned data. The supported formats are GL_STENCIL_INDEX,
        /// GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL, GL_RED, GL_GREEN, GL_BLUE, GL_RG, GL_RGB, GL_RGBA, GL_BGR,
        /// GL_BGRA, GL_RED_INTEGER, GL_GREEN_INTEGER, GL_BLUE_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER,
        /// GL_RGBA_INTEGER, GL_BGR_INTEGER, GL_BGRA_INTEGER.
        /// </param>
        /// <param name="type">
        /// Specifies a pixel type for the returned data. The supported types are GL_UNSIGNED_BYTE, GL_BYTE,
        /// GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_HALF_FLOAT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, GL_UNSIGNED_INT_2_10_10_10_REV,
        /// GL_UNSIGNED_INT_24_8, GL_UNSIGNED_INT_10F_11F_11F_REV, GL_UNSIGNED_INT_5_9_9_9_REV, and
        /// GL_FLOAT_32_UNSIGNED_INT_24_8_REV.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer pixels for glGetnTexImage and glGetTextureImage functions.
        /// </param>
        /// <param name="pixels">
        /// Returns the texture image. Should be a pointer to an array of the type specified by type.
        /// </param>
        public static void GetTextureImage(UInt32 texture, Int32 level, PixelFormat format, PixelType type, Int32 bufSize, [OutAttribute] IntPtr pixels)
        {
            Delegates.glGetTextureImage(texture, level, format, type, bufSize, pixels);
        }

        /// <summary>
        /// Return texture parameter values for a specific level of detail.
        /// <para>
        /// glGetTexLevelParameterfv, glGetTexLevelParameteriv, glGetTextureLevelParameterfv and
        /// glGetTextureLevelParameteriv return in params texture parameter values for a specific
        /// level-of-detail value, specified as level. For the first two functions, target defines the target
        /// texture, either GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_PROXY_TEXTURE_1D,
        /// GL_PROXY_TEXTURE_2D, GL_PROXY_TEXTURE_3D, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, or GL_PROXY_TEXTURE_CUBE_MAP. The
        /// remaining two take a texture argument which specifies the name of the texture object.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetTexLevelParameterfv and
        /// glGetTexLevelParameteriv functions. Must be one of the following values: GL_TEXTURE_1D,
        /// GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE,
        /// GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, GL_PROXY_TEXTURE_1D,
        /// GL_PROXY_TEXTURE_2D, GL_PROXY_TEXTURE_3D, GL_PROXY_TEXTURE_1D_ARRAY, GL_PROXY_TEXTURE_2D_ARRAY,
        /// GL_PROXY_TEXTURE_RECTANGLE, GL_PROXY_TEXTURE_2D_MULTISAMPLE, GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY,
        /// GL_PROXY_TEXTURE_CUBE_MAP, or GL_TEXTURE_BUFFER.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level n
        /// is the nth mipmap reduction image.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_TEXTURE_WIDTH, GL_TEXTURE_HEIGHT,
        /// GL_TEXTURE_DEPTH, GL_TEXTURE_INTERNAL_FORMAT, GL_TEXTURE_RED_SIZE, GL_TEXTURE_GREEN_SIZE,
        /// GL_TEXTURE_BLUE_SIZE, GL_TEXTURE_ALPHA_SIZE, GL_TEXTURE_DEPTH_SIZE, GL_TEXTURE_COMPRESSED,
        /// GL_TEXTURE_COMPRESSED_IMAGE_SIZE, and GL_TEXTURE_BUFFER_OFFSET are accepted.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetTexLevelParameterfv(GetPName target, Int32 level, GetTextureLevelParameter pname, [OutAttribute] Single[] @params)
        {
            Delegates.glGetTexLevelParameterfv(target, level, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values for a specific level of detail.
        /// <para>
        /// glGetTexLevelParameterfv, glGetTexLevelParameteriv, glGetTextureLevelParameterfv and
        /// glGetTextureLevelParameteriv return in params texture parameter values for a specific
        /// level-of-detail value, specified as level. For the first two functions, target defines the target
        /// texture, either GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_PROXY_TEXTURE_1D,
        /// GL_PROXY_TEXTURE_2D, GL_PROXY_TEXTURE_3D, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, or GL_PROXY_TEXTURE_CUBE_MAP. The
        /// remaining two take a texture argument which specifies the name of the texture object.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetTexLevelParameterfv and
        /// glGetTexLevelParameteriv functions. Must be one of the following values: GL_TEXTURE_1D,
        /// GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE,
        /// GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, GL_PROXY_TEXTURE_1D,
        /// GL_PROXY_TEXTURE_2D, GL_PROXY_TEXTURE_3D, GL_PROXY_TEXTURE_1D_ARRAY, GL_PROXY_TEXTURE_2D_ARRAY,
        /// GL_PROXY_TEXTURE_RECTANGLE, GL_PROXY_TEXTURE_2D_MULTISAMPLE, GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY,
        /// GL_PROXY_TEXTURE_CUBE_MAP, or GL_TEXTURE_BUFFER.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level n
        /// is the nth mipmap reduction image.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_TEXTURE_WIDTH, GL_TEXTURE_HEIGHT,
        /// GL_TEXTURE_DEPTH, GL_TEXTURE_INTERNAL_FORMAT, GL_TEXTURE_RED_SIZE, GL_TEXTURE_GREEN_SIZE,
        /// GL_TEXTURE_BLUE_SIZE, GL_TEXTURE_ALPHA_SIZE, GL_TEXTURE_DEPTH_SIZE, GL_TEXTURE_COMPRESSED,
        /// GL_TEXTURE_COMPRESSED_IMAGE_SIZE, and GL_TEXTURE_BUFFER_OFFSET are accepted.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetTexLevelParameteriv(GetPName target, Int32 level, GetTextureLevelParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetTexLevelParameteriv(target, level, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values for a specific level of detail.
        /// <para>
        /// glGetTexLevelParameterfv, glGetTexLevelParameteriv, glGetTextureLevelParameterfv and
        /// glGetTextureLevelParameteriv return in params texture parameter values for a specific
        /// level-of-detail value, specified as level. For the first two functions, target defines the target
        /// texture, either GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_PROXY_TEXTURE_1D,
        /// GL_PROXY_TEXTURE_2D, GL_PROXY_TEXTURE_3D, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, or GL_PROXY_TEXTURE_CUBE_MAP. The
        /// remaining two take a texture argument which specifies the name of the texture object.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glGetTextureLevelParameterfv and glGetTextureLevelParameteriv
        /// functions.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level n
        /// is the nth mipmap reduction image.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_TEXTURE_WIDTH, GL_TEXTURE_HEIGHT,
        /// GL_TEXTURE_DEPTH, GL_TEXTURE_INTERNAL_FORMAT, GL_TEXTURE_RED_SIZE, GL_TEXTURE_GREEN_SIZE,
        /// GL_TEXTURE_BLUE_SIZE, GL_TEXTURE_ALPHA_SIZE, GL_TEXTURE_DEPTH_SIZE, GL_TEXTURE_COMPRESSED,
        /// GL_TEXTURE_COMPRESSED_IMAGE_SIZE, and GL_TEXTURE_BUFFER_OFFSET are accepted.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetTextureLevelParameterfv(UInt32 texture, Int32 level, GetTextureLevelParameter pname, [OutAttribute] Single[] @params)
        {
            Delegates.glGetTextureLevelParameterfv(texture, level, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values for a specific level of detail.
        /// <para>
        /// glGetTexLevelParameterfv, glGetTexLevelParameteriv, glGetTextureLevelParameterfv and
        /// glGetTextureLevelParameteriv return in params texture parameter values for a specific
        /// level-of-detail value, specified as level. For the first two functions, target defines the target
        /// texture, either GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_PROXY_TEXTURE_1D,
        /// GL_PROXY_TEXTURE_2D, GL_PROXY_TEXTURE_3D, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, or GL_PROXY_TEXTURE_CUBE_MAP. The
        /// remaining two take a texture argument which specifies the name of the texture object.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glGetTextureLevelParameterfv and glGetTextureLevelParameteriv
        /// functions.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number of the desired image. Level 0 is the base image level. Level n
        /// is the nth mipmap reduction image.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_TEXTURE_WIDTH, GL_TEXTURE_HEIGHT,
        /// GL_TEXTURE_DEPTH, GL_TEXTURE_INTERNAL_FORMAT, GL_TEXTURE_RED_SIZE, GL_TEXTURE_GREEN_SIZE,
        /// GL_TEXTURE_BLUE_SIZE, GL_TEXTURE_ALPHA_SIZE, GL_TEXTURE_DEPTH_SIZE, GL_TEXTURE_COMPRESSED,
        /// GL_TEXTURE_COMPRESSED_IMAGE_SIZE, and GL_TEXTURE_BUFFER_OFFSET are accepted.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetTextureLevelParameteriv(UInt32 texture, Int32 level, GetTextureLevelParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetTextureLevelParameteriv(texture, level, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values.
        /// <para>
        /// glGetTexParameter and glGetTextureParameter return in params the value or values of the texture
        /// parameter specified as pname. target defines the target texture. GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, or GL_TEXTURE_2D_MULTISAMPLE_ARRAY specify
        /// one-, two-, or three-dimensional, one-dimensional array, two-dimensional array, rectangle,
        /// cube-mapped or cube-mapped array, two-dimensional multisample, or two-dimensional multisample array
        /// texturing, respectively. pname accepts the same symbols as glTexParameter, with the same
        /// interpretations:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetTexParameterfv, glGetTexParameteriv,
        /// glGetTexParameterIiv, and glGetTexParameterIuiv functions. GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY,
        /// GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
        /// GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_RECTANGLE, and GL_TEXTURE_CUBE_MAP_ARRAY are
        /// accepted.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_DEPTH_STENCIL_TEXTURE_MODE,
        /// GL_IMAGE_FORMAT_COMPATIBILITY_TYPE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_BORDER_COLOR,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_TEXTURE_IMMUTABLE_FORMAT,
        /// GL_TEXTURE_IMMUTABLE_LEVELS, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MAX_LEVEL,
        /// GL_TEXTURE_MAX_LOD, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MIN_LOD, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_SWIZZLE_RGBA,
        /// GL_TEXTURE_TARGET, GL_TEXTURE_VIEW_MIN_LAYER, GL_TEXTURE_VIEW_MIN_LEVEL, GL_TEXTURE_VIEW_NUM_LAYERS,
        /// GL_TEXTURE_VIEW_NUM_LEVELS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and GL_TEXTURE_WRAP_R are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the texture parameters.
        /// </param>
        public static void GetTexParameterfv(TextureTarget target, GetTextureParameter pname, [OutAttribute] Single[] @params)
        {
            Delegates.glGetTexParameterfv(target, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values.
        /// <para>
        /// glGetTexParameter and glGetTextureParameter return in params the value or values of the texture
        /// parameter specified as pname. target defines the target texture. GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, or GL_TEXTURE_2D_MULTISAMPLE_ARRAY specify
        /// one-, two-, or three-dimensional, one-dimensional array, two-dimensional array, rectangle,
        /// cube-mapped or cube-mapped array, two-dimensional multisample, or two-dimensional multisample array
        /// texturing, respectively. pname accepts the same symbols as glTexParameter, with the same
        /// interpretations:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetTexParameterfv, glGetTexParameteriv,
        /// glGetTexParameterIiv, and glGetTexParameterIuiv functions. GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY,
        /// GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
        /// GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_RECTANGLE, and GL_TEXTURE_CUBE_MAP_ARRAY are
        /// accepted.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_DEPTH_STENCIL_TEXTURE_MODE,
        /// GL_IMAGE_FORMAT_COMPATIBILITY_TYPE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_BORDER_COLOR,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_TEXTURE_IMMUTABLE_FORMAT,
        /// GL_TEXTURE_IMMUTABLE_LEVELS, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MAX_LEVEL,
        /// GL_TEXTURE_MAX_LOD, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MIN_LOD, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_SWIZZLE_RGBA,
        /// GL_TEXTURE_TARGET, GL_TEXTURE_VIEW_MIN_LAYER, GL_TEXTURE_VIEW_MIN_LEVEL, GL_TEXTURE_VIEW_NUM_LAYERS,
        /// GL_TEXTURE_VIEW_NUM_LEVELS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and GL_TEXTURE_WRAP_R are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the texture parameters.
        /// </param>
        public static void GetTexParameteriv(TextureTarget target, GetTextureParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetTexParameteriv(target, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values.
        /// <para>
        /// glGetTexParameter and glGetTextureParameter return in params the value or values of the texture
        /// parameter specified as pname. target defines the target texture. GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, or GL_TEXTURE_2D_MULTISAMPLE_ARRAY specify
        /// one-, two-, or three-dimensional, one-dimensional array, two-dimensional array, rectangle,
        /// cube-mapped or cube-mapped array, two-dimensional multisample, or two-dimensional multisample array
        /// texturing, respectively. pname accepts the same symbols as glTexParameter, with the same
        /// interpretations:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetTexParameterfv, glGetTexParameteriv,
        /// glGetTexParameterIiv, and glGetTexParameterIuiv functions. GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY,
        /// GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
        /// GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_RECTANGLE, and GL_TEXTURE_CUBE_MAP_ARRAY are
        /// accepted.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_DEPTH_STENCIL_TEXTURE_MODE,
        /// GL_IMAGE_FORMAT_COMPATIBILITY_TYPE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_BORDER_COLOR,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_TEXTURE_IMMUTABLE_FORMAT,
        /// GL_TEXTURE_IMMUTABLE_LEVELS, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MAX_LEVEL,
        /// GL_TEXTURE_MAX_LOD, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MIN_LOD, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_SWIZZLE_RGBA,
        /// GL_TEXTURE_TARGET, GL_TEXTURE_VIEW_MIN_LAYER, GL_TEXTURE_VIEW_MIN_LEVEL, GL_TEXTURE_VIEW_NUM_LAYERS,
        /// GL_TEXTURE_VIEW_NUM_LEVELS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and GL_TEXTURE_WRAP_R are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the texture parameters.
        /// </param>
        public static void GetTexParameterIiv(TextureTarget target, GetTextureParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetTexParameterIiv(target, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values.
        /// <para>
        /// glGetTexParameter and glGetTextureParameter return in params the value or values of the texture
        /// parameter specified as pname. target defines the target texture. GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, or GL_TEXTURE_2D_MULTISAMPLE_ARRAY specify
        /// one-, two-, or three-dimensional, one-dimensional array, two-dimensional array, rectangle,
        /// cube-mapped or cube-mapped array, two-dimensional multisample, or two-dimensional multisample array
        /// texturing, respectively. pname accepts the same symbols as glTexParameter, with the same
        /// interpretations:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glGetTexParameterfv, glGetTexParameteriv,
        /// glGetTexParameterIiv, and glGetTexParameterIuiv functions. GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY,
        /// GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
        /// GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_RECTANGLE, and GL_TEXTURE_CUBE_MAP_ARRAY are
        /// accepted.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_DEPTH_STENCIL_TEXTURE_MODE,
        /// GL_IMAGE_FORMAT_COMPATIBILITY_TYPE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_BORDER_COLOR,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_TEXTURE_IMMUTABLE_FORMAT,
        /// GL_TEXTURE_IMMUTABLE_LEVELS, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MAX_LEVEL,
        /// GL_TEXTURE_MAX_LOD, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MIN_LOD, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_SWIZZLE_RGBA,
        /// GL_TEXTURE_TARGET, GL_TEXTURE_VIEW_MIN_LAYER, GL_TEXTURE_VIEW_MIN_LEVEL, GL_TEXTURE_VIEW_NUM_LAYERS,
        /// GL_TEXTURE_VIEW_NUM_LEVELS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and GL_TEXTURE_WRAP_R are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the texture parameters.
        /// </param>
        public static void GetTexParameterIuiv(TextureTarget target, GetTextureParameter pname, [OutAttribute] UInt32[] @params)
        {
            Delegates.glGetTexParameterIuiv(target, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values.
        /// <para>
        /// glGetTexParameter and glGetTextureParameter return in params the value or values of the texture
        /// parameter specified as pname. target defines the target texture. GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, or GL_TEXTURE_2D_MULTISAMPLE_ARRAY specify
        /// one-, two-, or three-dimensional, one-dimensional array, two-dimensional array, rectangle,
        /// cube-mapped or cube-mapped array, two-dimensional multisample, or two-dimensional multisample array
        /// texturing, respectively. pname accepts the same symbols as glTexParameter, with the same
        /// interpretations:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glGetTextureParameterfv, glGetTextureParameteriv,
        /// glGetTextureParameterIiv, and glGetTextureParameterIuiv functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_DEPTH_STENCIL_TEXTURE_MODE,
        /// GL_IMAGE_FORMAT_COMPATIBILITY_TYPE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_BORDER_COLOR,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_TEXTURE_IMMUTABLE_FORMAT,
        /// GL_TEXTURE_IMMUTABLE_LEVELS, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MAX_LEVEL,
        /// GL_TEXTURE_MAX_LOD, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MIN_LOD, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_SWIZZLE_RGBA,
        /// GL_TEXTURE_TARGET, GL_TEXTURE_VIEW_MIN_LAYER, GL_TEXTURE_VIEW_MIN_LEVEL, GL_TEXTURE_VIEW_NUM_LAYERS,
        /// GL_TEXTURE_VIEW_NUM_LEVELS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and GL_TEXTURE_WRAP_R are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the texture parameters.
        /// </param>
        public static void GetTextureParameterfv(UInt32 texture, GetTextureParameter pname, [OutAttribute] Single[] @params)
        {
            Delegates.glGetTextureParameterfv(texture, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values.
        /// <para>
        /// glGetTexParameter and glGetTextureParameter return in params the value or values of the texture
        /// parameter specified as pname. target defines the target texture. GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, or GL_TEXTURE_2D_MULTISAMPLE_ARRAY specify
        /// one-, two-, or three-dimensional, one-dimensional array, two-dimensional array, rectangle,
        /// cube-mapped or cube-mapped array, two-dimensional multisample, or two-dimensional multisample array
        /// texturing, respectively. pname accepts the same symbols as glTexParameter, with the same
        /// interpretations:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glGetTextureParameterfv, glGetTextureParameteriv,
        /// glGetTextureParameterIiv, and glGetTextureParameterIuiv functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_DEPTH_STENCIL_TEXTURE_MODE,
        /// GL_IMAGE_FORMAT_COMPATIBILITY_TYPE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_BORDER_COLOR,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_TEXTURE_IMMUTABLE_FORMAT,
        /// GL_TEXTURE_IMMUTABLE_LEVELS, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MAX_LEVEL,
        /// GL_TEXTURE_MAX_LOD, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MIN_LOD, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_SWIZZLE_RGBA,
        /// GL_TEXTURE_TARGET, GL_TEXTURE_VIEW_MIN_LAYER, GL_TEXTURE_VIEW_MIN_LEVEL, GL_TEXTURE_VIEW_NUM_LAYERS,
        /// GL_TEXTURE_VIEW_NUM_LEVELS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and GL_TEXTURE_WRAP_R are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the texture parameters.
        /// </param>
        public static void GetTextureParameteriv(UInt32 texture, GetTextureParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetTextureParameteriv(texture, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values.
        /// <para>
        /// glGetTexParameter and glGetTextureParameter return in params the value or values of the texture
        /// parameter specified as pname. target defines the target texture. GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, or GL_TEXTURE_2D_MULTISAMPLE_ARRAY specify
        /// one-, two-, or three-dimensional, one-dimensional array, two-dimensional array, rectangle,
        /// cube-mapped or cube-mapped array, two-dimensional multisample, or two-dimensional multisample array
        /// texturing, respectively. pname accepts the same symbols as glTexParameter, with the same
        /// interpretations:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glGetTextureParameterfv, glGetTextureParameteriv,
        /// glGetTextureParameterIiv, and glGetTextureParameterIuiv functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_DEPTH_STENCIL_TEXTURE_MODE,
        /// GL_IMAGE_FORMAT_COMPATIBILITY_TYPE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_BORDER_COLOR,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_TEXTURE_IMMUTABLE_FORMAT,
        /// GL_TEXTURE_IMMUTABLE_LEVELS, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MAX_LEVEL,
        /// GL_TEXTURE_MAX_LOD, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MIN_LOD, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_SWIZZLE_RGBA,
        /// GL_TEXTURE_TARGET, GL_TEXTURE_VIEW_MIN_LAYER, GL_TEXTURE_VIEW_MIN_LEVEL, GL_TEXTURE_VIEW_NUM_LAYERS,
        /// GL_TEXTURE_VIEW_NUM_LEVELS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and GL_TEXTURE_WRAP_R are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the texture parameters.
        /// </param>
        public static void GetTextureParameterIiv(UInt32 texture, GetTextureParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetTextureParameterIiv(texture, pname, @params);
        }

        /// <summary>
        /// Return texture parameter values.
        /// <para>
        /// glGetTexParameter and glGetTextureParameter return in params the value or values of the texture
        /// parameter specified as pname. target defines the target texture. GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP,
        /// GL_TEXTURE_CUBE_MAP_ARRAY, GL_TEXTURE_2D_MULTISAMPLE, or GL_TEXTURE_2D_MULTISAMPLE_ARRAY specify
        /// one-, two-, or three-dimensional, one-dimensional array, two-dimensional array, rectangle,
        /// cube-mapped or cube-mapped array, two-dimensional multisample, or two-dimensional multisample array
        /// texturing, respectively. pname accepts the same symbols as glTexParameter, with the same
        /// interpretations:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glGetTextureParameterfv, glGetTextureParameteriv,
        /// glGetTextureParameterIiv, and glGetTextureParameterIuiv functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a texture parameter. GL_DEPTH_STENCIL_TEXTURE_MODE,
        /// GL_IMAGE_FORMAT_COMPATIBILITY_TYPE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_BORDER_COLOR,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_COMPARE_FUNC, GL_TEXTURE_IMMUTABLE_FORMAT,
        /// GL_TEXTURE_IMMUTABLE_LEVELS, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MAX_LEVEL,
        /// GL_TEXTURE_MAX_LOD, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MIN_LOD, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_SWIZZLE_RGBA,
        /// GL_TEXTURE_TARGET, GL_TEXTURE_VIEW_MIN_LAYER, GL_TEXTURE_VIEW_MIN_LEVEL, GL_TEXTURE_VIEW_NUM_LAYERS,
        /// GL_TEXTURE_VIEW_NUM_LEVELS, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and GL_TEXTURE_WRAP_R are
        /// accepted.
        /// </param>
        /// <param name="params">
        /// Returns the texture parameters.
        /// </param>
        public static void GetTextureParameterIuiv(UInt32 texture, GetTextureParameter pname, [OutAttribute] UInt32[] @params)
        {
            Delegates.glGetTextureParameterIuiv(texture, pname, @params);
        }

        /// <summary>
        /// Retrieve a sub-region of a texture image from a texture object.
        /// <para>
        /// glGetTextureSubImage returns a texture subimage into pixels.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the name of the source texture object. Must be GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY,
        /// GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY or
        /// GL_TEXTURE_RECTANGLE. In specific, buffer and multisample textures are not permitted.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level $n$ is the $n$th mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="zoffset">
        /// Specifies a texel offset in the z direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_BGRA, GL_DEPTH_COMPONENT and GL_STENCIL_INDEX.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer to receive the retrieved pixel data.
        /// </param>
        /// <param name="pixels">
        /// Returns the texture subimage. Should be a pointer to an array of the type specified by type.
        /// </param>
        public static void GetTextureSubImage(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelFormat format, PixelType type, Int32 bufSize, [OutAttribute] IntPtr pixels)
        {
            Delegates.glGetTextureSubImage(texture, level, xoffset, yoffset, zoffset, width, height, depth, format, type, bufSize, pixels);
        }

        /// <summary>
        /// Query the state of a transform feedback object.
        /// <para>
        /// In order to use the Transform Feedback functionality, you need to configure the Transform Feedback
        /// Buffer indexed bindings. This can be achieved by either using glBindBufferBase or glBindBuffersBase
        /// to associate whole buffer object storage to one of the Transform Feedback Binding Points, or by
        /// calling glBindBufferRange or glBindBuffersRange to use a region of a buffer object storage for the
        /// binding. You may want to (but are not required to) bind a Transform Feedback Object first, in order
        /// to cache the binding configuration. This usually allows you to restore the Transform Feedback
        /// configuration faster, than if you were to execute a list of API calls necessary to set up the
        /// Transform Feedback state of your liking.
        /// </para>
        /// </summary>
        /// <param name="xfb">
        /// The name of an existing transform feedback object, or zero for the default transform feedback
        /// object.
        /// </param>
        /// <param name="pname">
        /// Property to use for the query. Must be one of the values: GL_TRANSFORM_FEEDBACK_BUFFER_BINDING,
        /// GL_TRANSFORM_FEEDBACK_BUFFER_START, GL_TRANSFORM_FEEDBACK_BUFFER_SIZE, GL_TRANSFORM_FEEDBACK_PAUSED,
        /// GL_TRANSFORM_FEEDBACK_ACTIVE.
        /// </param>
        /// <param name="param">
        /// The address of a buffer into which will be written the requested state information.
        /// </param>
        public static void GetTransformFeedbackiv(UInt32 xfb, TransformFeedbackParameterName pname, [OutAttribute] Int32[] param)
        {
            Delegates.glGetTransformFeedbackiv(xfb, pname, param);
        }

        /// <summary>
        /// Query the state of a transform feedback object.
        /// <para>
        /// In order to use the Transform Feedback functionality, you need to configure the Transform Feedback
        /// Buffer indexed bindings. This can be achieved by either using glBindBufferBase or glBindBuffersBase
        /// to associate whole buffer object storage to one of the Transform Feedback Binding Points, or by
        /// calling glBindBufferRange or glBindBuffersRange to use a region of a buffer object storage for the
        /// binding. You may want to (but are not required to) bind a Transform Feedback Object first, in order
        /// to cache the binding configuration. This usually allows you to restore the Transform Feedback
        /// configuration faster, than if you were to execute a list of API calls necessary to set up the
        /// Transform Feedback state of your liking.
        /// </para>
        /// </summary>
        /// <param name="xfb">
        /// The name of an existing transform feedback object, or zero for the default transform feedback
        /// object.
        /// </param>
        /// <param name="pname">
        /// Property to use for the query. Must be one of the values: GL_TRANSFORM_FEEDBACK_BUFFER_BINDING,
        /// GL_TRANSFORM_FEEDBACK_BUFFER_START, GL_TRANSFORM_FEEDBACK_BUFFER_SIZE, GL_TRANSFORM_FEEDBACK_PAUSED,
        /// GL_TRANSFORM_FEEDBACK_ACTIVE.
        /// </param>
        /// <param name="index">
        /// Index of the transform feedback stream (for indexed state).
        /// </param>
        /// <param name="param">
        /// The address of a buffer into which will be written the requested state information.
        /// </param>
        public static void GetTransformFeedbacki_v(UInt32 xfb, TransformFeedbackParameterName pname, UInt32 index, [OutAttribute] Int32[] param)
        {
            Delegates.glGetTransformFeedbacki_v(xfb, pname, index, param);
        }

        /// <summary>
        /// Query the state of a transform feedback object.
        /// <para>
        /// In order to use the Transform Feedback functionality, you need to configure the Transform Feedback
        /// Buffer indexed bindings. This can be achieved by either using glBindBufferBase or glBindBuffersBase
        /// to associate whole buffer object storage to one of the Transform Feedback Binding Points, or by
        /// calling glBindBufferRange or glBindBuffersRange to use a region of a buffer object storage for the
        /// binding. You may want to (but are not required to) bind a Transform Feedback Object first, in order
        /// to cache the binding configuration. This usually allows you to restore the Transform Feedback
        /// configuration faster, than if you were to execute a list of API calls necessary to set up the
        /// Transform Feedback state of your liking.
        /// </para>
        /// </summary>
        /// <param name="xfb">
        /// The name of an existing transform feedback object, or zero for the default transform feedback
        /// object.
        /// </param>
        /// <param name="pname">
        /// Property to use for the query. Must be one of the values: GL_TRANSFORM_FEEDBACK_BUFFER_BINDING,
        /// GL_TRANSFORM_FEEDBACK_BUFFER_START, GL_TRANSFORM_FEEDBACK_BUFFER_SIZE, GL_TRANSFORM_FEEDBACK_PAUSED,
        /// GL_TRANSFORM_FEEDBACK_ACTIVE.
        /// </param>
        /// <param name="index">
        /// Index of the transform feedback stream (for indexed state).
        /// </param>
        /// <param name="param">
        /// The address of a buffer into which will be written the requested state information.
        /// </param>
        public static void GetTransformFeedbacki64_v(UInt32 xfb, TransformFeedbackParameterName pname, UInt32 index, [OutAttribute] Int64[] param)
        {
            Delegates.glGetTransformFeedbacki64_v(xfb, pname, index, param);
        }

        /// <summary>
        /// Retrieve information about varying variables selected for transform feedback.
        /// <para>
        /// Information about the set of varying variables in a linked program that will be captured during
        /// transform feedback may be retrieved by calling glGetTransformFeedbackVarying.
        /// glGetTransformFeedbackVarying provides information about the varying variable selected by index. An
        /// index of 0 selects the first varying variable specified in the varyings array passed to
        /// glTransformFeedbackVaryings, and an index of the value of GL_TRANSFORM_FEEDBACK_VARYINGS minus one
        /// selects the last such variable.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of the target program object.
        /// </param>
        /// <param name="index">
        /// The index of the varying variable whose information to retrieve.
        /// </param>
        /// <param name="bufSize">
        /// The maximum number of characters, including the null terminator, that may be written into name.
        /// </param>
        /// <param name="length">
        /// The address of a variable which will receive the number of characters written into name, excluding
        /// the null-terminator. If length is NULL no length is returned.
        /// </param>
        /// <param name="size">
        /// The address of a variable that will receive the size of the varying.
        /// </param>
        /// <param name="type">
        /// The address of a variable that will recieve the type of the varying.
        /// </param>
        /// <param name="name">
        /// The address of a buffer into which will be written the name of the varying.
        /// </param>
        public static void GetTransformFeedbackVarying(UInt32 program, UInt32 index, Int32 bufSize, [OutAttribute] Int32[] length, [OutAttribute] Int32[] size, [OutAttribute] ActiveAttribType[] type, [OutAttribute] System.Text.StringBuilder name)
        {
            Delegates.glGetTransformFeedbackVarying(program, index, bufSize, length, size, type, name);
        }

        /// <summary>
        /// Returns the value of a uniform variable.
        /// <para>
        /// glGetUniform and glGetnUniform return in params the value(s) of the specified uniform variable. The
        /// type of the uniform variable specified by location determines the number of values returned. If the
        /// uniform variable is defined in the shader as a boolean, int, or float, a single value will be
        /// returned. If it is defined as a vec2, ivec2, or bvec2, two values will be returned. If it is defined
        /// as a vec3, ivec3, or bvec3, three values will be returned, and so on. To query values stored in
        /// uniform variables declared as arrays, call glGetUniform for each element of the array. To query
        /// values stored in uniform variables declared as structures, call glGetUniform for each field in the
        /// structure. The values for uniform variables declared as a matrix will be returned in column major
        /// order.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be queried.
        /// </param>
        /// <param name="params">
        /// Returns the value of the specified uniform variable.
        /// </param>
        public static void GetUniformfv(UInt32 program, Int32 location, [OutAttribute] Single[] @params)
        {
            Delegates.glGetUniformfv(program, location, @params);
        }

        /// <summary>
        /// Returns the value of a uniform variable.
        /// <para>
        /// glGetUniform and glGetnUniform return in params the value(s) of the specified uniform variable. The
        /// type of the uniform variable specified by location determines the number of values returned. If the
        /// uniform variable is defined in the shader as a boolean, int, or float, a single value will be
        /// returned. If it is defined as a vec2, ivec2, or bvec2, two values will be returned. If it is defined
        /// as a vec3, ivec3, or bvec3, three values will be returned, and so on. To query values stored in
        /// uniform variables declared as arrays, call glGetUniform for each element of the array. To query
        /// values stored in uniform variables declared as structures, call glGetUniform for each field in the
        /// structure. The values for uniform variables declared as a matrix will be returned in column major
        /// order.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be queried.
        /// </param>
        /// <param name="params">
        /// Returns the value of the specified uniform variable.
        /// </param>
        public static void GetUniformiv(UInt32 program, Int32 location, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetUniformiv(program, location, @params);
        }

        /// <summary>
        /// Returns the value of a uniform variable.
        /// <para>
        /// glGetUniform and glGetnUniform return in params the value(s) of the specified uniform variable. The
        /// type of the uniform variable specified by location determines the number of values returned. If the
        /// uniform variable is defined in the shader as a boolean, int, or float, a single value will be
        /// returned. If it is defined as a vec2, ivec2, or bvec2, two values will be returned. If it is defined
        /// as a vec3, ivec3, or bvec3, three values will be returned, and so on. To query values stored in
        /// uniform variables declared as arrays, call glGetUniform for each element of the array. To query
        /// values stored in uniform variables declared as structures, call glGetUniform for each field in the
        /// structure. The values for uniform variables declared as a matrix will be returned in column major
        /// order.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be queried.
        /// </param>
        /// <param name="params">
        /// Returns the value of the specified uniform variable.
        /// </param>
        public static void GetUniformuiv(UInt32 program, Int32 location, [OutAttribute] UInt32[] @params)
        {
            Delegates.glGetUniformuiv(program, location, @params);
        }

        /// <summary>
        /// Returns the value of a uniform variable.
        /// <para>
        /// glGetUniform and glGetnUniform return in params the value(s) of the specified uniform variable. The
        /// type of the uniform variable specified by location determines the number of values returned. If the
        /// uniform variable is defined in the shader as a boolean, int, or float, a single value will be
        /// returned. If it is defined as a vec2, ivec2, or bvec2, two values will be returned. If it is defined
        /// as a vec3, ivec3, or bvec3, three values will be returned, and so on. To query values stored in
        /// uniform variables declared as arrays, call glGetUniform for each element of the array. To query
        /// values stored in uniform variables declared as structures, call glGetUniform for each field in the
        /// structure. The values for uniform variables declared as a matrix will be returned in column major
        /// order.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be queried.
        /// </param>
        /// <param name="params">
        /// Returns the value of the specified uniform variable.
        /// </param>
        public static void GetUniformdv(UInt32 program, Int32 location, [OutAttribute] Double[] @params)
        {
            Delegates.glGetUniformdv(program, location, @params);
        }

        /// <summary>
        /// Returns the value of a uniform variable.
        /// <para>
        /// glGetUniform and glGetnUniform return in params the value(s) of the specified uniform variable. The
        /// type of the uniform variable specified by location determines the number of values returned. If the
        /// uniform variable is defined in the shader as a boolean, int, or float, a single value will be
        /// returned. If it is defined as a vec2, ivec2, or bvec2, two values will be returned. If it is defined
        /// as a vec3, ivec3, or bvec3, three values will be returned, and so on. To query values stored in
        /// uniform variables declared as arrays, call glGetUniform for each element of the array. To query
        /// values stored in uniform variables declared as structures, call glGetUniform for each field in the
        /// structure. The values for uniform variables declared as a matrix will be returned in column major
        /// order.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer params.
        /// </param>
        /// <param name="params">
        /// Returns the value of the specified uniform variable.
        /// </param>
        public static void GetnUniformfv(UInt32 program, Int32 location, Int32 bufSize, [OutAttribute] Single[] @params)
        {
            Delegates.glGetnUniformfv(program, location, bufSize, @params);
        }

        /// <summary>
        /// Returns the value of a uniform variable.
        /// <para>
        /// glGetUniform and glGetnUniform return in params the value(s) of the specified uniform variable. The
        /// type of the uniform variable specified by location determines the number of values returned. If the
        /// uniform variable is defined in the shader as a boolean, int, or float, a single value will be
        /// returned. If it is defined as a vec2, ivec2, or bvec2, two values will be returned. If it is defined
        /// as a vec3, ivec3, or bvec3, three values will be returned, and so on. To query values stored in
        /// uniform variables declared as arrays, call glGetUniform for each element of the array. To query
        /// values stored in uniform variables declared as structures, call glGetUniform for each field in the
        /// structure. The values for uniform variables declared as a matrix will be returned in column major
        /// order.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer params.
        /// </param>
        /// <param name="params">
        /// Returns the value of the specified uniform variable.
        /// </param>
        public static void GetnUniformiv(UInt32 program, Int32 location, Int32 bufSize, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetnUniformiv(program, location, bufSize, @params);
        }

        /// <summary>
        /// Returns the value of a uniform variable.
        /// <para>
        /// glGetUniform and glGetnUniform return in params the value(s) of the specified uniform variable. The
        /// type of the uniform variable specified by location determines the number of values returned. If the
        /// uniform variable is defined in the shader as a boolean, int, or float, a single value will be
        /// returned. If it is defined as a vec2, ivec2, or bvec2, two values will be returned. If it is defined
        /// as a vec3, ivec3, or bvec3, three values will be returned, and so on. To query values stored in
        /// uniform variables declared as arrays, call glGetUniform for each element of the array. To query
        /// values stored in uniform variables declared as structures, call glGetUniform for each field in the
        /// structure. The values for uniform variables declared as a matrix will be returned in column major
        /// order.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer params.
        /// </param>
        /// <param name="params">
        /// Returns the value of the specified uniform variable.
        /// </param>
        public static void GetnUniformuiv(UInt32 program, Int32 location, Int32 bufSize, [OutAttribute] UInt32[] @params)
        {
            Delegates.glGetnUniformuiv(program, location, bufSize, @params);
        }

        /// <summary>
        /// Returns the value of a uniform variable.
        /// <para>
        /// glGetUniform and glGetnUniform return in params the value(s) of the specified uniform variable. The
        /// type of the uniform variable specified by location determines the number of values returned. If the
        /// uniform variable is defined in the shader as a boolean, int, or float, a single value will be
        /// returned. If it is defined as a vec2, ivec2, or bvec2, two values will be returned. If it is defined
        /// as a vec3, ivec3, or bvec3, three values will be returned, and so on. To query values stored in
        /// uniform variables declared as arrays, call glGetUniform for each element of the array. To query
        /// values stored in uniform variables declared as structures, call glGetUniform for each field in the
        /// structure. The values for uniform variables declared as a matrix will be returned in column major
        /// order.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be queried.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer params.
        /// </param>
        /// <param name="params">
        /// Returns the value of the specified uniform variable.
        /// </param>
        public static void GetnUniformdv(UInt32 program, Int32 location, Int32 bufSize, [OutAttribute] Double[] @params)
        {
            Delegates.glGetnUniformdv(program, location, bufSize, @params);
        }

        /// <summary>
        /// Retrieve the index of a named uniform block.
        /// <para>
        /// glGetUniformBlockIndex retrieves the index of a uniform block within program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of a program containing the uniform block.
        /// </param>
        /// <param name="uniformBlockName">
        /// Specifies the address an array of characters to containing the name of the uniform block whose index
        /// to retrieve.
        /// </param>
        public static UInt32 GetUniformBlockIndex(UInt32 program, String uniformBlockName)
        {
            UseProgram(program);    // take care of a crash that can occur on NVIDIA drivers by using the program first
            return Delegates.glGetUniformBlockIndex(program, uniformBlockName);
        }

        /// <summary>
        /// Retrieve the index of a named uniform block.
        /// <para>
        /// glGetUniformIndices retrieves the indices of a number of uniforms within program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of a program containing uniforms whose indices to query.
        /// </param>
        /// <param name="uniformCount">
        /// Specifies the number of uniforms whose indices to query.
        /// </param>
        /// <param name="uniformNames">
        /// Specifies the address of an array of pointers to buffers containing the names of the queried
        /// uniforms.
        /// </param>
        /// <param name="uniformIndices">
        /// Specifies the address of an array that will receive the indices of the uniforms.
        /// </param>
        public static void GetUniformIndices(UInt32 program, Int32 uniformCount, String uniformNames, [OutAttribute] UInt32[] uniformIndices)
        {
            Delegates.glGetUniformIndices(program, uniformCount, uniformNames, uniformIndices);
        }

        /// <summary>
        /// Returns the location of a uniform variable.
        /// <para>
        /// glGetUniformLocation returns an integer that represents the location of a specific uniform variable
        /// within a program object. name must be a null terminated string that contains no white space. name
        /// must be an active uniform variable name in program that is not a structure, an array of structures,
        /// or a subcomponent of a vector or a matrix. This function returns -1 if name does not correspond to
        /// an active uniform variable in program, if name starts with the reserved prefix "gl_", or if name is
        /// associated with an atomic counter or a named uniform block.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the program object to be queried.
        /// </param>
        /// <param name="name">
        /// Points to a null terminated string containing the name of the uniform variable whose location is to
        /// be queried.
        /// </param>
        public static Int32 GetUniformLocation(UInt32 program, String name)
        {
            return Delegates.glGetUniformLocation(program, name);
        }

        /// <summary>
        /// Retrieve the value of a subroutine uniform of a given shader stage of the current program.
        /// <para>
        /// glGetUniformSubroutine retrieves the value of the subroutine uniform at location location for shader
        /// stage shadertype of the current program. location must be less than the value of
        /// GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS for the shader currently in use at shader stage shadertype.
        /// The value of the subroutine uniform is returned in values.
        /// </para>
        /// </summary>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for subroutine uniform index. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the subroutine uniform.
        /// </param>
        /// <param name="values">
        /// Specifies the address of a variable to receive the value or values of the subroutine uniform.
        /// </param>
        public static void GetUniformSubroutineuiv(ShaderType shadertype, Int32 location, [OutAttribute] UInt32[] values)
        {
            Delegates.glGetUniformSubroutineuiv(shadertype, location, values);
        }

        /// <summary>
        /// Retrieve parameters of an attribute of a vertex array object.
        /// <para>
        /// glGetVertexArrayIndexediv and glGetVertexArrayIndexed64iv provide a way of querying parameters of an
        /// attribute at an user-specified index of a vertex array object. The vertex array object does not have
        /// to be bound to the rendering context at the time of the call, but must have been bound at least once
        /// prior to this call.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of a vertex array object.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the vertex array object attribute. Must 	 be a number between 0 and
        /// (GL_MAX_VERTEX_ATTRIBS - 1).
        /// </param>
        /// <param name="pname">
        /// Specifies the property to be used for the query. For 	 glGetVertexArrayIndexediv, it must be one of
        /// the following values: 	 GL_VERTEX_ATTRIB_ARRAY_ENABLED, 	 GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, 	 GL_VERTEX_ATTRIB_ARRAY_TYPE, 	 GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, 	 GL_VERTEX_ATTRIB_ARRAY_LONG, 	 GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or
        /// GL_VERTEX_ATTRIB_RELATIVE_OFFSET. For 	 glGetVertexArrayIndexed64v, it must be equal to
        /// GL_VERTEX_BINDING_OFFSET.
        /// </param>
        /// <param name="param">
        /// Returns the requested value.
        /// </param>
        public static void GetVertexArrayIndexed64iv(UInt32 vaobj, UInt32 index, VertexAttribParameter pname, [OutAttribute] Int64[] param)
        {
            Delegates.glGetVertexArrayIndexed64iv(vaobj, index, pname, param);
        }

        /// <summary>
        /// Retrieve parameters of an attribute of a vertex array object.
        /// <para>
        /// glGetVertexArrayIndexediv and glGetVertexArrayIndexed64iv provide a way of querying parameters of an
        /// attribute at an user-specified index of a vertex array object. The vertex array object does not have
        /// to be bound to the rendering context at the time of the call, but must have been bound at least once
        /// prior to this call.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of a vertex array object.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the vertex array object attribute. Must 	 be a number between 0 and
        /// (GL_MAX_VERTEX_ATTRIBS - 1).
        /// </param>
        /// <param name="pname">
        /// Specifies the property to be used for the query. For 	 glGetVertexArrayIndexediv, it must be one of
        /// the following values: 	 GL_VERTEX_ATTRIB_ARRAY_ENABLED, 	 GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, 	 GL_VERTEX_ATTRIB_ARRAY_TYPE, 	 GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, 	 GL_VERTEX_ATTRIB_ARRAY_LONG, 	 GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or
        /// GL_VERTEX_ATTRIB_RELATIVE_OFFSET. For 	 glGetVertexArrayIndexed64v, it must be equal to
        /// GL_VERTEX_BINDING_OFFSET.
        /// </param>
        /// <param name="param">
        /// Returns the requested value.
        /// </param>
        public static void GetVertexArrayIndexediv(UInt32 vaobj, UInt32 index, VertexAttribParameter pname, [OutAttribute] Int32[] param)
        {
            Delegates.glGetVertexArrayIndexediv(vaobj, index, pname, param);
        }

        /// <summary>
        /// Retrieve parameters of a vertex array object.
        /// <para>
        /// This function provides a mean of querying properties of an existing vertex array object. The vertex
        /// array object does not have to be bound to the rendering context at the time of the call, but must
        /// have been bound at least once prior to this call.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// specifies the name of the vertex array object to use for the query.
        /// </param>
        /// <param name="pname">
        /// Name of the property to use for the query. Must be GL_ELEMENT_ARRAY_BUFFER_BINDING.
        /// </param>
        /// <param name="param">
        /// Returns the requested value.
        /// </param>
        public static void GetVertexArrayiv(UInt32 vaobj, VertexAttribParameter pname, [OutAttribute] Int32[] param)
        {
            Delegates.glGetVertexArrayiv(vaobj, pname, param);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribdv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Double[] @params)
        {
            Delegates.glGetVertexAttribdv(index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribdv(Int32 index, VertexAttribParameter pname, [OutAttribute] Double[] @params)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetVertexAttribdv((UInt32)index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribfv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Single[] @params)
        {
            Delegates.glGetVertexAttribfv(index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribfv(Int32 index, VertexAttribParameter pname, [OutAttribute] Single[] @params)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetVertexAttribfv((UInt32)index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribiv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetVertexAttribiv(index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribiv(Int32 index, VertexAttribParameter pname, [OutAttribute] Int32[] @params)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetVertexAttribiv((UInt32)index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribIiv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Int32[] @params)
        {
            Delegates.glGetVertexAttribIiv(index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribIiv(Int32 index, VertexAttribParameter pname, [OutAttribute] Int32[] @params)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetVertexAttribIiv((UInt32)index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribIuiv(UInt32 index, VertexAttribParameter pname, [OutAttribute] UInt32[] @params)
        {
            Delegates.glGetVertexAttribIuiv(index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribIuiv(Int32 index, VertexAttribParameter pname, [OutAttribute] UInt32[] @params)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetVertexAttribIuiv((UInt32)index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribLdv(UInt32 index, VertexAttribParameter pname, [OutAttribute] Double[] @params)
        {
            Delegates.glGetVertexAttribLdv(index, pname, @params);
        }

        /// <summary>
        /// Return a generic vertex attribute parameter.
        /// <para>
        /// glGetVertexAttrib returns in params the value of a generic vertex attribute parameter. The generic
        /// vertex attribute to be queried is specified by index, and the parameter to be queried is specified
        /// by pname.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be queried.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the vertex attribute parameter to be queried. Accepted values are
        /// GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, GL_VERTEX_ATTRIB_ARRAY_ENABLED, GL_VERTEX_ATTRIB_ARRAY_SIZE,
        /// GL_VERTEX_ATTRIB_ARRAY_STRIDE, GL_VERTEX_ATTRIB_ARRAY_TYPE, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED,
        /// GL_VERTEX_ATTRIB_ARRAY_INTEGER, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, or GL_CURRENT_VERTEX_ATTRIB.
        /// </param>
        /// <param name="params">
        /// Returns the requested data.
        /// </param>
        public static void GetVertexAttribLdv(Int32 index, VertexAttribParameter pname, [OutAttribute] Double[] @params)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetVertexAttribLdv((UInt32)index, pname, @params);
        }

        /// <summary>
        /// Return the address of the specified generic vertex attribute pointer.
        /// <para>
        /// glGetVertexAttribPointerv returns pointer information. index is the generic vertex attribute to be
        /// queried, pname is a symbolic constant indicating the pointer to be returned, and params is a pointer
        /// to a location in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be returned.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the generic vertex attribute parameter to be returned. Must be
        /// GL_VERTEX_ATTRIB_ARRAY_POINTER.
        /// </param>
        /// <param name="pointer">
        /// Returns the pointer value.
        /// </param>
        public static void GetVertexAttribPointerv(UInt32 index, VertexAttribPointerParameter pname, [OutAttribute] IntPtr pointer)
        {
            Delegates.glGetVertexAttribPointerv(index, pname, pointer);
        }

        /// <summary>
        /// Return the address of the specified generic vertex attribute pointer.
        /// <para>
        /// glGetVertexAttribPointerv returns pointer information. index is the generic vertex attribute to be
        /// queried, pname is a symbolic constant indicating the pointer to be returned, and params is a pointer
        /// to a location in which to place the returned data.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the generic vertex attribute parameter to be returned.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of the generic vertex attribute parameter to be returned. Must be
        /// GL_VERTEX_ATTRIB_ARRAY_POINTER.
        /// </param>
        /// <param name="pointer">
        /// Returns the pointer value.
        /// </param>
        public static void GetVertexAttribPointerv(Int32 index, VertexAttribPointerParameter pname, [OutAttribute] IntPtr pointer)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glGetVertexAttribPointerv((UInt32)index, pname, pointer);
        }

        /// <summary>
        /// Specify implementation-specific hints.
        /// <para>
        /// Certain aspects of GL behavior, when there is room for interpretation, can be controlled with hints.
        /// A hint is specified with two arguments. target is a symbolic constant indicating the behavior to be
        /// controlled, and mode is another symbolic constant indicating the desired behavior. The initial value
        /// for each target is GL_DONT_CARE. mode can be one of the following:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies a symbolic constant indicating the behavior to be controlled. GL_LINE_SMOOTH_HINT,
        /// GL_POLYGON_SMOOTH_HINT, GL_TEXTURE_COMPRESSION_HINT, and GL_FRAGMENT_SHADER_DERIVATIVE_HINT are
        /// accepted.
        /// </param>
        /// <param name="mode">
        /// Specifies a symbolic constant indicating the desired behavior. GL_FASTEST, GL_NICEST, and
        /// GL_DONT_CARE are accepted.
        /// </param>
        public static void Hint(HintTarget target, HintMode mode)
        {
            Delegates.glHint(target, mode);
        }

        /// <summary>
        /// Invalidate the content of a buffer object's data store.
        /// <para>
        /// glInvalidateBufferData invalidates all of the content of the data store of a buffer object. After
        /// invalidation, the content of the buffer's data store becomes undefined.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// The name of a buffer object whose data store to invalidate.
        /// </param>
        public static void InvalidateBufferData(UInt32 buffer)
        {
            Delegates.glInvalidateBufferData(buffer);
        }

        /// <summary>
        /// Invalidate a region of a buffer object's data store.
        /// <para>
        /// glInvalidateBufferSubData invalidates all or part of the content of the data store of a buffer
        /// object. After invalidation, the content of the specified range of the buffer's data store becomes
        /// undefined. The start of the range is given by offset and its size is given by length, both measured
        /// in basic machine units.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// The name of a buffer object, a subrange of whose data store to invalidate.
        /// </param>
        /// <param name="offset">
        /// The offset within the buffer's data store of the start of the range to be invalidated.
        /// </param>
        /// <param name="length">
        /// The length of the range within the buffer's data store to be invalidated.
        /// </param>
        public static void InvalidateBufferSubData(UInt32 buffer, IntPtr offset, IntPtr length)
        {
            Delegates.glInvalidateBufferSubData(buffer, offset, length);
        }

        /// <summary>
        /// Invalidate the content of some or all of a framebuffer's attachments.
        /// <para>
        /// glInvalidateFramebuffer and glInvalidateNamedFramebufferData invalidate the entire contents of a
        /// specified set of attachments of a framebuffer.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer object is attached for glInvalidateFramebuffer.
        /// </param>
        /// <param name="numAttachments">
        /// Specifies the number of entries in the attachments array.
        /// </param>
        /// <param name="attachments">
        /// Specifies a pointer to an array identifying the attachments to be invalidated.
        /// </param>
        public static void InvalidateFramebuffer(FramebufferTarget target, Int32 numAttachments, FramebufferAttachment[] attachments)
        {
            Delegates.glInvalidateFramebuffer(target, numAttachments, attachments);
        }

        /// <summary>
        /// Invalidate the content of some or all of a framebuffer's attachments.
        /// <para>
        /// glInvalidateFramebuffer and glInvalidateNamedFramebufferData invalidate the entire contents of a
        /// specified set of attachments of a framebuffer.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glInvalidateNamedFramebufferData.
        /// </param>
        /// <param name="numAttachments">
        /// Specifies the number of entries in the attachments array.
        /// </param>
        /// <param name="attachments">
        /// Specifies a pointer to an array identifying the attachments to be invalidated.
        /// </param>
        public static void InvalidateNamedFramebufferData(UInt32 framebuffer, Int32 numAttachments, FramebufferAttachment[] attachments)
        {
            Delegates.glInvalidateNamedFramebufferData(framebuffer, numAttachments, attachments);
        }

        /// <summary>
        /// Invalidate the content of a region of some or all of a framebuffer's attachments.
        /// <para>
        /// glInvalidateSubFramebuffer and glInvalidateNamedFramebufferSubData invalidate the contents of a
        /// specified region of a specified set of attachments of a framebuffer.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the framebuffer object is attached for glInvalidateSubFramebuffer.
        /// </param>
        /// <param name="numAttachments">
        /// Specifies the number of entries in the attachments array.
        /// </param>
        /// <param name="attachments">
        /// Specifies a pointer to an array identifying the attachments to be invalidated.
        /// </param>
        /// <param name="x">
        /// Specifies the X offset of the region to be invalidated.
        /// </param>
        /// <param name="y">
        /// Specifies the Y offset of the region to be invalidated.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the region to be invalidated.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the region to be invalidated.
        /// </param>
        public static void InvalidateSubFramebuffer(FramebufferTarget target, Int32 numAttachments, FramebufferAttachment[] attachments, Int32 x, Int32 y, Int32 width, Int32 height)
        {
            Delegates.glInvalidateSubFramebuffer(target, numAttachments, attachments, x, y, width, height);
        }

        /// <summary>
        /// Invalidate the content of a region of some or all of a framebuffer's attachments.
        /// <para>
        /// glInvalidateSubFramebuffer and glInvalidateNamedFramebufferSubData invalidate the contents of a
        /// specified region of a specified set of attachments of a framebuffer.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies the name of the framebuffer object for glInvalidateNamedFramebufferSubData.
        /// </param>
        /// <param name="numAttachments">
        /// Specifies the number of entries in the attachments array.
        /// </param>
        /// <param name="attachments">
        /// Specifies a pointer to an array identifying the attachments to be invalidated.
        /// </param>
        /// <param name="x">
        /// Specifies the X offset of the region to be invalidated.
        /// </param>
        /// <param name="y">
        /// Specifies the Y offset of the region to be invalidated.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the region to be invalidated.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the region to be invalidated.
        /// </param>
        public static void InvalidateNamedFramebufferSubData(UInt32 framebuffer, Int32 numAttachments, FramebufferAttachment[] attachments, Int32 x, Int32 y, Int32 width, Int32 height)
        {
            Delegates.glInvalidateNamedFramebufferSubData(framebuffer, numAttachments, attachments, x, y, width, height);
        }

        /// <summary>
        /// Invalidate the entirety a texture image.
        /// <para>
        /// glInvalidateTexSubImage invalidates all of a texture image. texture and level indicated which
        /// texture image is being invalidated. After this command, data in the texture image has undefined
        /// values.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// The name of a texture object to invalidate.
        /// </param>
        /// <param name="level">
        /// The level of detail of the texture object to invalidate.
        /// </param>
        public static void InvalidateTexImage(UInt32 texture, Int32 level)
        {
            Delegates.glInvalidateTexImage(texture, level);
        }

        /// <summary>
        /// Invalidate a region of a texture image.
        /// <para>
        /// glInvalidateTexSubImage invalidates all or part of a texture image. texture and level indicated
        /// which texture image is being invalidated. After this command, data in that subregion have undefined
        /// values. xoffset, yoffset, zoffset, width, height, and depth are interpreted as they are in
        /// glTexSubImage3D. For texture targets that don't have certain dimensions, this command treats those
        /// dimensions as having a size of 1. For example, to invalidate a portion of a two- dimensional
        /// texture, the application would use zoffset equal to zero and depth equal to one. Cube map textures
        /// are treated as an array of six slices in the z-dimension, where a value of zoffset is interpreted as
        /// specifying face GL_TEXTURE_CUBE_MAP_POSITIVE_X + zoffset.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// The name of a texture object a subregion of which to invalidate.
        /// </param>
        /// <param name="level">
        /// The level of detail of the texture object within which the region resides.
        /// </param>
        /// <param name="xoffset">
        /// The X offset of the region to be invalidated.
        /// </param>
        /// <param name="yoffset">
        /// The Y offset of the region to be invalidated.
        /// </param>
        /// <param name="zoffset">
        /// The Z offset of the region to be invalidated.
        /// </param>
        /// <param name="width">
        /// The width of the region to be invalidated.
        /// </param>
        /// <param name="height">
        /// The height of the region to be invalidated.
        /// </param>
        /// <param name="depth">
        /// The depth of the region to be invalidated.
        /// </param>
        public static void InvalidateTexSubImage(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth)
        {
            Delegates.glInvalidateTexSubImage(texture, level, xoffset, yoffset, zoffset, width, height, depth);
        }

        /// <summary>
        /// Determine if a name corresponds to a buffer object.
        /// <para>
        /// glIsBuffer returns GL_TRUE if buffer is currently the name of a buffer object. If buffer is zero, or
        /// is a non-zero value that is not currently the name of a buffer object, or if an error occurs,
        /// glIsBuffer returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies a value that may be the name of a buffer object.
        /// </param>
        public static Boolean IsBuffer(UInt32 buffer)
        {
            return Delegates.glIsBuffer(buffer);
        }

        /// <summary>
        /// Test whether a capability is enabled.
        /// <para>
        /// glIsEnabled returns GL_TRUE if cap is an enabled capability and returns GL_FALSE otherwise. Boolean
        /// states that are indexed may be tested with glIsEnabledi. For glIsEnabledi, index specifies the index
        /// of the capability to test. index must be between zero and the count of indexed capabilities for cap.
        /// Initially all capabilities except GL_DITHER are disabled; GL_DITHER is initially enabled.
        /// </para>
        /// </summary>
        /// <param name="cap">
        /// Specifies a symbolic constant indicating a GL capability.
        /// </param>
        public static Boolean IsEnabled(EnableCap cap)
        {
            return Delegates.glIsEnabled(cap);
        }

        /// <summary>
        /// Test whether a capability is enabled.
        /// <para>
        /// glIsEnabled returns GL_TRUE if cap is an enabled capability and returns GL_FALSE otherwise. Boolean
        /// states that are indexed may be tested with glIsEnabledi. For glIsEnabledi, index specifies the index
        /// of the capability to test. index must be between zero and the count of indexed capabilities for cap.
        /// Initially all capabilities except GL_DITHER are disabled; GL_DITHER is initially enabled.
        /// </para>
        /// </summary>
        /// <param name="cap">
        /// Specifies a symbolic constant indicating a GL capability.
        /// </param>
        /// <param name="index">
        /// Specifies the index of the capability.
        /// </param>
        public static Boolean IsEnabledi(EnableCap cap, UInt32 index)
        {
            return Delegates.glIsEnabledi(cap, index);
        }

        /// <summary>
        /// Determine if a name corresponds to a framebuffer object.
        /// <para>
        /// glIsFramebuffer returns GL_TRUE if framebuffer is currently the name of a framebuffer object. If
        /// framebuffer is zero, or if framebuffer is not the name of a framebuffer object, or if an error
        /// occurs, glIsFramebuffer returns GL_FALSE. If framebuffer is a name returned by glGenFramebuffers, by
        /// that has not yet been bound through a call to glBindFramebuffer, then the name is not a framebuffer
        /// object and glIsFramebuffer returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="framebuffer">
        /// Specifies a value that may be the name of a framebuffer object.
        /// </param>
        public static Boolean IsFramebuffer(UInt32 framebuffer)
        {
            return Delegates.glIsFramebuffer(framebuffer);
        }

        /// <summary>
        /// Determines if a name corresponds to a program object.
        /// <para>
        /// glIsProgram returns GL_TRUE if program is the name of a program object previously created with
        /// glCreateProgram and not yet deleted with glDeleteProgram. If program is zero or a non-zero value
        /// that is not the name of a program object, or if an error occurs, glIsProgram returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies a potential program object.
        /// </param>
        public static Boolean IsProgram(UInt32 program)
        {
            return Delegates.glIsProgram(program);
        }

        /// <summary>
        /// Determine if a name corresponds to a program pipeline object.
        /// <para>
        /// glIsProgramPipeline returns GL_TRUE if pipeline is currently the name of a program pipeline object.
        /// If pipeline is zero, or if pipeline is not the name of a program pipeline object, or if an error
        /// occurs, glIsProgramPipeline returns GL_FALSE. If pipeline is a name returned by
        /// glGenProgramPipelines, but that has not yet been bound through a call to glBindProgramPipeline, then
        /// the name is not a program pipeline object and glIsProgramPipeline returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="pipeline">
        /// Specifies a value that may be the name of a program pipeline object.
        /// </param>
        public static Boolean IsProgramPipeline(UInt32 pipeline)
        {
            return Delegates.glIsProgramPipeline(pipeline);
        }

        /// <summary>
        /// Determine if a name corresponds to a query object.
        /// <para>
        /// glIsQuery returns GL_TRUE if id is currently the name of a query object. If id is zero, or is a
        /// non-zero value that is not currently the name of a query object, or if an error occurs, glIsQuery
        /// returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specifies a value that may be the name of a query object.
        /// </param>
        public static Boolean IsQuery(UInt32 id)
        {
            return Delegates.glIsQuery(id);
        }

        /// <summary>
        /// Determine if a name corresponds to a renderbuffer object.
        /// <para>
        /// glIsRenderbuffer returns GL_TRUE if renderbuffer is currently the name of a renderbuffer object. If
        /// renderbuffer is zero, or if renderbuffer is not the name of a renderbuffer object, or if an error
        /// occurs, glIsRenderbuffer returns GL_FALSE. If renderbuffer is a name returned by glGenRenderbuffers,
        /// by that has not yet been bound through a call to glBindRenderbuffer or glFramebufferRenderbuffer,
        /// then the name is not a renderbuffer object and glIsRenderbuffer returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="renderbuffer">
        /// Specifies a value that may be the name of a renderbuffer object.
        /// </param>
        public static Boolean IsRenderbuffer(UInt32 renderbuffer)
        {
            return Delegates.glIsRenderbuffer(renderbuffer);
        }

        /// <summary>
        /// Determine if a name corresponds to a sampler object.
        /// <para>
        /// glIsSampler returns GL_TRUE if id is currently the name of a sampler object. If id is zero, or is a
        /// non-zero value that is not currently the name of a sampler object, or if an error occurs,
        /// glIsSampler returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specifies a value that may be the name of a sampler object.
        /// </param>
        public static Boolean IsSampler(UInt32 id)
        {
            return Delegates.glIsSampler(id);
        }

        /// <summary>
        /// Determines if a name corresponds to a shader object.
        /// <para>
        /// glIsShader returns GL_TRUE if shader is the name of a shader object previously created with
        /// glCreateShader and not yet deleted with glDeleteShader. If shader is zero or a non-zero value that
        /// is not the name of a shader object, or if an error occurs, glIsShader returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="shader">
        /// Specifies a potential shader object.
        /// </param>
        public static Boolean IsShader(UInt32 shader)
        {
            return Delegates.glIsShader(shader);
        }

        /// <summary>
        /// Determine if a name corresponds to a sync object.
        /// <para>
        /// glIsSync returns GL_TRUE if sync is currently the name of a sync object. If sync is not the name of
        /// a sync object, or if an error occurs, glIsSync returns GL_FALSE. Note that zero is not the name of a
        /// sync object.
        /// </para>
        /// </summary>
        /// <param name="sync">
        /// Specifies a value that may be the name of a sync object.
        /// </param>
        public static Boolean IsSync(IntPtr sync)
        {
            return Delegates.glIsSync(sync);
        }

        /// <summary>
        /// Determine if a name corresponds to a texture.
        /// <para>
        /// glIsTexture returns GL_TRUE if texture is currently the name of a texture. If texture is zero, or is
        /// a non-zero value that is not currently the name of a texture, or if an error occurs, glIsTexture
        /// returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies a value that may be the name of a texture.
        /// </param>
        public static Boolean IsTexture(UInt32 texture)
        {
            return Delegates.glIsTexture(texture);
        }

        /// <summary>
        /// Determine if a name corresponds to a transform feedback object.
        /// <para>
        /// glIsTransformFeedback returns GL_TRUE if id is currently the name of a transform feedback object. If
        /// id is zero, or if id is not the name of a transform feedback object, or if an error occurs,
        /// glIsTransformFeedback returns GL_FALSE. If id is a name returned by glGenTransformFeedbacks, but
        /// that has not yet been bound through a call to glBindTransformFeedback, then the name is not a
        /// transform feedback object and glIsTransformFeedback returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// Specifies a value that may be the name of a transform feedback object.
        /// </param>
        public static Boolean IsTransformFeedback(UInt32 id)
        {
            return Delegates.glIsTransformFeedback(id);
        }

        /// <summary>
        /// Determine if a name corresponds to a vertex array object.
        /// <para>
        /// glIsVertexArray returns GL_TRUE if array is currently the name of a vertex array object. If array is
        /// zero, or if array is not the name of a vertex array object, or if an error occurs, glIsVertexArray
        /// returns GL_FALSE. If array is a name returned by glGenVertexArrays, by that has not yet been bound
        /// through a call to glBindVertexArray, then the name is not a vertex array object and glIsVertexArray
        /// returns GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="array">
        /// Specifies a value that may be the name of a vertex array object.
        /// </param>
        public static Boolean IsVertexArray(UInt32 array)
        {
            return Delegates.glIsVertexArray(array);
        }

        /// <summary>
        /// Specify the width of rasterized lines.
        /// <para>
        /// glLineWidth specifies the rasterized width of both aliased and antialiased lines. Using a line width
        /// other than 1 has different effects, depending on whether line antialiasing is enabled. To enable and
        /// disable line antialiasing, call glEnable and glDisable with argument GL_LINE_SMOOTH. Line
        /// antialiasing is initially disabled.
        /// </para>
        /// </summary>
        /// <param name="width">
        /// Specifies the width of rasterized lines. The initial value is 1.
        /// </param>
        public static void LineWidth(Single width)
        {
            Delegates.glLineWidth(width);
        }

        /// <summary>
        /// Links a program object.
        /// <para>
        /// glLinkProgram links the program object specified by program. If any shader objects of type
        /// GL_VERTEX_SHADER are attached to program, they will be used to create an executable that will run on
        /// the programmable vertex processor. If any shader objects of type GL_GEOMETRY_SHADER are attached to
        /// program, they will be used to create an executable that will run on the programmable geometry
        /// processor. If any shader objects of type GL_FRAGMENT_SHADER are attached to program, they will be
        /// used to create an executable that will run on the programmable fragment processor.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program object to be linked.
        /// </param>
        public static void LinkProgram(UInt32 program)
        {
            Delegates.glLinkProgram(program);
        }

        /// <summary>
        /// Specify a logical pixel operation for rendering.
        /// <para>
        /// glLogicOp specifies a logical operation that, when enabled, is applied between the incoming RGBA
        /// color and the RGBA color at the corresponding location in the frame buffer. To enable or disable the
        /// logical operation, call glEnable and glDisable using the symbolic constant GL_COLOR_LOGIC_OP. The
        /// initial value is disabled.
        /// </para>
        /// </summary>
        /// <param name="opcode">
        /// Specifies a symbolic constant that selects a logical operation. The following symbols are accepted:
        /// GL_CLEAR, GL_SET, GL_COPY, GL_COPY_INVERTED, GL_NOOP, GL_INVERT, GL_AND, GL_NAND, GL_OR, GL_NOR,
        /// GL_XOR, GL_EQUIV, GL_AND_REVERSE, GL_AND_INVERTED, GL_OR_REVERSE, and GL_OR_INVERTED. The initial
        /// value is GL_COPY.
        /// </param>
        public static void LogicOp(LogicOpEnum opcode)
        {
            Delegates.glLogicOp(opcode);
        }

        /// <summary>
        /// Map all of a buffer object's data store into the client's address space.
        /// <para>
        /// glMapBuffer and glMapNamedBuffer map the entire data store of a specified buffer object into the
        /// client's address space. The data can then be directly read and/or written relative to the returned
        /// pointer, depending on the specified access policy.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glMapBuffer, which must be one of the
        /// buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER Vertex
        /// attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy source
        /// GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute dispatch
        /// commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex array
        /// indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="access">
        /// Specifies the access policy for glMapBuffer and glMapNamedBuffer, indicating whether it will be
        /// possible to read from, write to, or both read from and write to the buffer object's mapped data
        /// store. The symbolic constant must be GL_READ_ONLY, GL_WRITE_ONLY, or GL_READ_WRITE.
        /// </param>
        public static IntPtr MapBuffer(BufferTarget target, BufferAccess access)
        {
            return Delegates.glMapBuffer(target, access);
        }

        /// <summary>
        /// Map all of a buffer object's data store into the client's address space.
        /// <para>
        /// glMapBuffer and glMapNamedBuffer map the entire data store of a specified buffer object into the
        /// client's address space. The data can then be directly read and/or written relative to the returned
        /// pointer, depending on the specified access policy.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glMapNamedBuffer.
        /// </param>
        /// <param name="access">
        /// Specifies the access policy for glMapBuffer and glMapNamedBuffer, indicating whether it will be
        /// possible to read from, write to, or both read from and write to the buffer object's mapped data
        /// store. The symbolic constant must be GL_READ_ONLY, GL_WRITE_ONLY, or GL_READ_WRITE.
        /// </param>
        public static IntPtr MapNamedBuffer(UInt32 buffer, BufferAccess access)
        {
            return Delegates.glMapNamedBuffer(buffer, access);
        }

        /// <summary>
        /// Map all or part of a buffer object's data store into the client's address space.
        /// <para>
        /// glMapBufferRange and glMapNamedBufferRange map all or part of the data store of a specified buffer
        /// object into the client's address space. offset and length indicate the range of data in the buffer
        /// object that is to be mapped, in terms of basic machine units. access is a bitfield containing flags
        /// which describe the requested mapping. These flags are described below.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glMapBufferRange, which must be one of
        /// the buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER
        /// Vertex attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy
        /// source GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute
        /// dispatch commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex
        /// array indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        /// <param name="offset">
        /// Specifies the starting offset within the buffer of the range to be mapped.
        /// </param>
        /// <param name="length">
        /// Specifies the length of the range to be mapped.
        /// </param>
        /// <param name="access">
        /// Specifies a combination of access flags indicating the desired access to the mapped range.
        /// </param>
        public static IntPtr MapBufferRange(BufferTarget target, IntPtr offset, IntPtr length, BufferAccessMask access)
        {
            return Delegates.glMapBufferRange(target, offset, length, access);
        }

        /// <summary>
        /// Map all or part of a buffer object's data store into the client's address space.
        /// <para>
        /// glMapBufferRange and glMapNamedBufferRange map all or part of the data store of a specified buffer
        /// object into the client's address space. offset and length indicate the range of data in the buffer
        /// object that is to be mapped, in terms of basic machine units. access is a bitfield containing flags
        /// which describe the requested mapping. These flags are described below.
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glMapNamedBufferRange.
        /// </param>
        /// <param name="offset">
        /// Specifies the starting offset within the buffer of the range to be mapped.
        /// </param>
        /// <param name="length">
        /// Specifies the length of the range to be mapped.
        /// </param>
        /// <param name="access">
        /// Specifies a combination of access flags indicating the desired access to the mapped range.
        /// </param>
        public static IntPtr MapNamedBufferRange(UInt32 buffer, IntPtr offset, Int32 length, UInt32 access)
        {
            return Delegates.glMapNamedBufferRange(buffer, offset, length, access);
        }

        /// <summary>
        /// Defines a barrier ordering memory transactions.
        /// <para>
        /// Encoding="UTF-8" standalone="no"?> glMemoryBarrier - OpenGL 4 Reference Pages MathJax.Hub.Config({
        /// MathML: { extensions: ["content-mathml.js"] }, tex2jax: { inlineMath: [['$','$'], ['\\(','\\)']] }
        /// }); Name glMemoryBarrier â€” defines a barrier ordering memory transactions.
        /// </para>
        /// </summary>
        /// <param name="barriers">
        /// Specifies the barriers to insert. For glMemoryBarrier, must be a bitwise combination of any of
        /// GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT, GL_ELEMENT_ARRAY_BARRIER_BIT, GL_UNIFORM_BARRIER_BIT,
        /// GL_TEXTURE_FETCH_BARRIER_BIT, GL_SHADER_IMAGE_ACCESS_BARRIER_BIT, GL_COMMAND_BARRIER_BIT,
        /// GL_PIXEL_BUFFER_BARRIER_BIT, GL_TEXTURE_UPDATE_BARRIER_BIT, GL_BUFFER_UPDATE_BARRIER_BIT,
        /// GL_FRAMEBUFFER_BARRIER_BIT, GL_TRANSFORM_FEEDBACK_BARRIER_BIT, GL_ATOMIC_COUNTER_BARRIER_BIT, or
        /// GL_SHADER_STORAGE_BARRIER_BIT. For glMemoryBarrier, must be a bitwise combination of any of
        /// GL_ATOMIC_COUNTER_BARRIER_BIT, or GL_FRAMEBUFFER_BARRIER_BIT, GL_SHADER_IMAGE_ACCESS_BARRIER_BIT,
        /// GL_SHADER_STORAGE_BARRIER_BIT. GL_TEXTURE_FETCH_BARRIER_BIT, or GL_UNIFORM_BARRIER_BIT. If the
        /// special value GL_ALL_BARRIER_BITS is specified, all supported barriers for the corresponding command
        /// will be inserted.
        /// </param>
        public static void MemoryBarrier(UInt32 barriers)
        {
            Delegates.glMemoryBarrier(barriers);
        }

        /// <summary>
        /// Defines a barrier ordering memory transactions.
        /// <para>
        /// Encoding="UTF-8" standalone="no"?> glMemoryBarrier - OpenGL 4 Reference Pages MathJax.Hub.Config({
        /// MathML: { extensions: ["content-mathml.js"] }, tex2jax: { inlineMath: [['$','$'], ['\\(','\\)']] }
        /// }); Name glMemoryBarrier â€” defines a barrier ordering memory transactions.
        /// </para>
        /// </summary>
        /// <param name="barriers">
        /// Specifies the barriers to insert. For glMemoryBarrier, must be a bitwise combination of any of
        /// GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT, GL_ELEMENT_ARRAY_BARRIER_BIT, GL_UNIFORM_BARRIER_BIT,
        /// GL_TEXTURE_FETCH_BARRIER_BIT, GL_SHADER_IMAGE_ACCESS_BARRIER_BIT, GL_COMMAND_BARRIER_BIT,
        /// GL_PIXEL_BUFFER_BARRIER_BIT, GL_TEXTURE_UPDATE_BARRIER_BIT, GL_BUFFER_UPDATE_BARRIER_BIT,
        /// GL_FRAMEBUFFER_BARRIER_BIT, GL_TRANSFORM_FEEDBACK_BARRIER_BIT, GL_ATOMIC_COUNTER_BARRIER_BIT, or
        /// GL_SHADER_STORAGE_BARRIER_BIT. For glMemoryBarrier, must be a bitwise combination of any of
        /// GL_ATOMIC_COUNTER_BARRIER_BIT, or GL_FRAMEBUFFER_BARRIER_BIT, GL_SHADER_IMAGE_ACCESS_BARRIER_BIT,
        /// GL_SHADER_STORAGE_BARRIER_BIT. GL_TEXTURE_FETCH_BARRIER_BIT, or GL_UNIFORM_BARRIER_BIT. If the
        /// special value GL_ALL_BARRIER_BITS is specified, all supported barriers for the corresponding command
        /// will be inserted.
        /// </param>
        public static void MemoryBarrierByRegion(UInt32 barriers)
        {
            Delegates.glMemoryBarrierByRegion(barriers);
        }

        /// <summary>
        /// Specifies minimum rate at which sample shaing takes place.
        /// <para>
        /// glMinSampleShading specifies the rate at which samples are shaded within a covered pixel.
        /// Sample-rate shading is enabled by calling glEnable with the parameter GL_SAMPLE_SHADING. If
        /// GL_MULTISAMPLE or GL_SAMPLE_SHADING is disabled, sample shading has no effect. Otherwise, an
        /// implementation must provide at least as many unique color values for each covered fragment as
        /// specified by value times samples where samples is the value of GL_SAMPLES for the current
        /// framebuffer. At least 1 sample for each covered fragment is generated.
        /// </para>
        /// </summary>
        /// <param name="value">
        /// Specifies the rate at which samples are shaded within each covered pixel.
        /// </param>
        public static void MinSampleShading(Single value)
        {
            Delegates.glMinSampleShading(value);
        }

        /// <summary>
        /// Render multiple sets of primitives from array data.
        /// <para>
        /// glMultiDrawArrays specifies multiple sets of geometric primitives with very few subroutine calls.
        /// Instead of calling a GL procedure to pass each individual vertex, normal, texture coordinate, edge
        /// flag, or color, you can prespecify separate arrays of vertices, normals, and colors and use them to
        /// construct a sequence of primitives with a single call to glMultiDrawArrays.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="first">
        /// Points to an array of starting indices in the enabled arrays.
        /// </param>
        /// <param name="count">
        /// Points to an array of the number of indices to be rendered.
        /// </param>
        /// <param name="drawcount">
        /// Specifies the size of the first and count
        /// </param>
        public static void MultiDrawArrays(BeginMode mode, Int32[] first, Int32[] count, Int32 drawcount)
        {
            Delegates.glMultiDrawArrays(mode, first, count, drawcount);
        }

        /// <summary>
        /// Render multiple sets of primitives from array data, taking parameters from memory.
        /// <para>
        /// glMultiDrawArraysIndirect specifies multiple geometric primitives with very few subroutine calls.
        /// glMultiDrawArraysIndirect behaves similarly to a multitude of calls to
        /// glDrawArraysInstancedBaseInstance, execept that the parameters to each call to
        /// glDrawArraysInstancedBaseInstance are stored in an array in memory at the address given by indirect,
        /// separated by the stride, in basic machine units, specified by stride. If stride is zero, then the
        /// array is assumed to be tightly packed in memory.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="indirect">
        /// Specifies the address of an array of structures containing the draw parameters.
        /// </param>
        /// <param name="drawcount">
        /// Specifies the the number of elements in the array of draw parameter structures.
        /// </param>
        /// <param name="stride">
        /// Specifies the distance in basic machine units between elements of the draw parameter array.
        /// </param>
        public static void MultiDrawArraysIndirect(BeginMode mode, IntPtr indirect, Int32 drawcount, Int32 stride)
        {
            Delegates.glMultiDrawArraysIndirect(mode, indirect, drawcount, stride);
        }

        /// <summary>
        /// Render multiple sets of primitives by specifying indices of array data elements.
        /// <para>
        /// glMultiDrawElements specifies multiple sets of geometric primitives with very few subroutine calls.
        /// Instead of calling a GL function to pass each individual vertex, normal, texture coordinate, edge
        /// flag, or color, you can prespecify separate arrays of vertices, normals, and so on, and use them to
        /// construct a sequence of primitives with a single call to glMultiDrawElements.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="count">
        /// Points to an array of the elements counts.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        /// <param name="drawcount">
        /// Specifies the size of the count and indices arrays.
        /// </param>
        public static void MultiDrawElements(BeginMode mode, Int32[] count, DrawElementsType type, IntPtr indices, Int32 drawcount)
        {
            Delegates.glMultiDrawElements(mode, count, type, indices, drawcount);
        }

        /// <summary>
        /// Render multiple sets of primitives by specifying indices of array data elements and an index to apply to each index.
        /// <para>
        /// glMultiDrawElementsBaseVertex behaves identically to glDrawElementsBaseVertex, except that drawcount
        /// separate lists of elements are specifried instead.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="count">
        /// Points to an array of the elements counts.
        /// </param>
        /// <param name="type">
        /// Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
        /// GL_UNSIGNED_INT.
        /// </param>
        /// <param name="indices">
        /// Specifies a pointer to the location where the indices are stored.
        /// </param>
        /// <param name="drawcount">
        /// Specifies the size of the count, indices and basevertex arrays.
        /// </param>
        /// <param name="basevertex">
        /// Specifies a pointer to the location where the base vertices are stored.
        /// </param>
        public static void MultiDrawElementsBaseVertex(BeginMode mode, Int32[] count, DrawElementsType type, IntPtr indices, Int32 drawcount, Int32[] basevertex)
        {
            Delegates.glMultiDrawElementsBaseVertex(mode, count, type, indices, drawcount, basevertex);
        }

        /// <summary>
        /// Render indexed primitives from array data, taking parameters from memory.
        /// <para>
        /// glMultiDrawElementsIndirect specifies multiple indexed geometric primitives with very few subroutine
        /// calls. glMultiDrawElementsIndirect behaves similarly to a multitude of calls to
        /// glDrawElementsInstancedBaseVertexBaseInstance, execpt that the parameters to
        /// glDrawElementsInstancedBaseVertexBaseInstance are stored in an array in memory at the address given
        /// by indirect, separated by the stride, in basic machine units, specified by stride. If stride is
        /// zero, then the array is assumed to be tightly packed in memory.
        /// </para>
        /// </summary>
        /// <param name="mode">
        /// Specifies what kind of primitives to render. Symbolic constants GL_POINTS, GL_LINE_STRIP,
        /// GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY, GL_TRIANGLE_STRIP,
        /// GL_TRIANGLE_FAN, GL_TRIANGLES, GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, and GL_PATCHES
        /// are accepted.
        /// </param>
        /// <param name="type">
        /// Specifies the type of data in the buffer bound to the GL_ELEMENT_ARRAY_BUFFER binding.
        /// </param>
        /// <param name="indirect">
        /// Specifies the address of a structure containing an array of draw parameters.
        /// </param>
        /// <param name="drawcount">
        /// Specifies the number of elements in the array addressed by indirect.
        /// </param>
        /// <param name="stride">
        /// Specifies the distance in basic machine units between elements of the draw parameter array.
        /// </param>
        public static void MultiDrawElementsIndirect(BeginMode mode, DrawElementsType type, IntPtr indirect, Int32 drawcount, Int32 stride)
        {
            Delegates.glMultiDrawElementsIndirect(mode, type, indirect, drawcount, stride);
        }

        /// <summary>
        /// Label a named object identified within a namespace.
        /// <para>
        /// glObjectLabel labels the object identified by name within the namespace given by identifier.
        /// identifier must be one of GL_BUFFER, GL_SHADER, GL_PROGRAM, GL_VERTEX_ARRAY, GL_QUERY,
        /// GL_PROGRAM_PIPELINE, GL_TRANSFORM_FEEDBACK, GL_SAMPLER, GL_TEXTURE, GL_RENDERBUFFER, GL_FRAMEBUFFER,
        /// to indicate the namespace containing the names of buffers, shaders, programs, vertex array objects,
        /// query objects, program pipelines, transform feedback objects, samplers, textures, renderbuffers and
        /// frame buffers, respectively.
        /// </para>
        /// </summary>
        /// <param name="identifier">
        /// The namespace from which the name of the object is allocated.
        /// </param>
        /// <param name="name">
        /// The name of the object to label.
        /// </param>
        /// <param name="length">
        /// The length of the label to be used for the object.
        /// </param>
        /// <param name="label">
        /// The address of a string containing the label to assign to the object.
        /// </param>
        public static void ObjectLabel(ObjectLabelEnum identifier, UInt32 name, Int32 length, String label)
        {
            Delegates.glObjectLabel(identifier, name, length, label);
        }

        /// <summary>
        /// Label a a sync object identified by a pointer.
        /// <para>
        /// glObjectPtrLabel labels the sync object identified by ptr.
        /// </para>
        /// </summary>
        /// <param name="ptr">
        /// A pointer identifying a sync object.
        /// </param>
        /// <param name="length">
        /// The length of the label to be used for the object.
        /// </param>
        /// <param name="label">
        /// The address of a string containing the label to assign to the object.
        /// </param>
        public static void ObjectPtrLabel(IntPtr ptr, Int32 length, String label)
        {
            Delegates.glObjectPtrLabel(ptr, length, label);
        }

        /// <summary>
        /// Specifies the parameters for patch primitives.
        /// <para>
        /// glPatchParameter specifies the parameters that will be used for patch primitives. pname specifies
        /// the parameter to modify and must be either GL_PATCH_VERTICES, GL_PATCH_DEFAULT_OUTER_LEVEL or
        /// GL_PATCH_DEFAULT_INNER_LEVEL. For glPatchParameteri, value specifies the new value for the parameter
        /// specified by pname. For glPatchParameterfv, values specifies the address of an array containing the
        /// new values for the parameter specified by pname.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the name of the parameter to set. The symbolc constants GL_PATCH_VERTICES,
        /// GL_PATCH_DEFAULT_OUTER_LEVEL, and GL_PATCH_DEFAULT_INNER_LEVEL are accepted.
        /// </param>
        /// <param name="value">
        /// Specifies the new value for the parameter given by pname.
        /// </param>
        public static void PatchParameteri(Int32 pname, Int32 value)
        {
            Delegates.glPatchParameteri(pname, value);
        }

        /// <summary>
        /// Specifies the parameters for patch primitives.
        /// <para>
        /// glPatchParameter specifies the parameters that will be used for patch primitives. pname specifies
        /// the parameter to modify and must be either GL_PATCH_VERTICES, GL_PATCH_DEFAULT_OUTER_LEVEL or
        /// GL_PATCH_DEFAULT_INNER_LEVEL. For glPatchParameteri, value specifies the new value for the parameter
        /// specified by pname. For glPatchParameterfv, values specifies the address of an array containing the
        /// new values for the parameter specified by pname.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the name of the parameter to set. The symbolc constants GL_PATCH_VERTICES,
        /// GL_PATCH_DEFAULT_OUTER_LEVEL, and GL_PATCH_DEFAULT_INNER_LEVEL are accepted.
        /// </param>
        /// <param name="values">
        /// Specifies the address of an array containing the new values for the parameter given by pname.
        /// </param>
        public static void PatchParameterfv(Int32 pname, Single[] values)
        {
            Delegates.glPatchParameterfv(pname, values);
        }

        /// <summary>
        /// Set pixel storage modes.
        /// <para>
        /// glPixelStore sets pixel storage modes that affect the operation of subsequent glReadPixels as well
        /// as the unpacking of texture patterns (see glTexImage1D, glTexImage2D, glTexImage3D, glTexSubImage1D,
        /// glTexSubImage2D, glTexSubImage3D), glCompressedTexImage1D, glCompressedTexImage2D,
        /// glCompressedTexImage3D, glCompressedTexSubImage1D, glCompressedTexSubImage2D or
        /// glCompressedTexSubImage1D.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the symbolic name of the parameter to be set. Six values affect the packing of pixel data
        /// into memory: GL_PACK_SWAP_BYTES, GL_PACK_LSB_FIRST, GL_PACK_ROW_LENGTH, GL_PACK_IMAGE_HEIGHT,
        /// GL_PACK_SKIP_PIXELS, GL_PACK_SKIP_ROWS, GL_PACK_SKIP_IMAGES, and GL_PACK_ALIGNMENT. Six more affect
        /// the unpacking of pixel data from memory: GL_UNPACK_SWAP_BYTES, GL_UNPACK_LSB_FIRST,
        /// GL_UNPACK_ROW_LENGTH, GL_UNPACK_IMAGE_HEIGHT, GL_UNPACK_SKIP_PIXELS, GL_UNPACK_SKIP_ROWS,
        /// GL_UNPACK_SKIP_IMAGES, and GL_UNPACK_ALIGNMENT.
        /// </param>
        /// <param name="param">
        /// Specifies the value that pname is set to.
        /// </param>
        public static void PixelStoref(PixelStoreParameter pname, Single param)
        {
            Delegates.glPixelStoref(pname, param);
        }

        /// <summary>
        /// Set pixel storage modes.
        /// <para>
        /// glPixelStore sets pixel storage modes that affect the operation of subsequent glReadPixels as well
        /// as the unpacking of texture patterns (see glTexImage1D, glTexImage2D, glTexImage3D, glTexSubImage1D,
        /// glTexSubImage2D, glTexSubImage3D), glCompressedTexImage1D, glCompressedTexImage2D,
        /// glCompressedTexImage3D, glCompressedTexSubImage1D, glCompressedTexSubImage2D or
        /// glCompressedTexSubImage1D.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies the symbolic name of the parameter to be set. Six values affect the packing of pixel data
        /// into memory: GL_PACK_SWAP_BYTES, GL_PACK_LSB_FIRST, GL_PACK_ROW_LENGTH, GL_PACK_IMAGE_HEIGHT,
        /// GL_PACK_SKIP_PIXELS, GL_PACK_SKIP_ROWS, GL_PACK_SKIP_IMAGES, and GL_PACK_ALIGNMENT. Six more affect
        /// the unpacking of pixel data from memory: GL_UNPACK_SWAP_BYTES, GL_UNPACK_LSB_FIRST,
        /// GL_UNPACK_ROW_LENGTH, GL_UNPACK_IMAGE_HEIGHT, GL_UNPACK_SKIP_PIXELS, GL_UNPACK_SKIP_ROWS,
        /// GL_UNPACK_SKIP_IMAGES, and GL_UNPACK_ALIGNMENT.
        /// </param>
        /// <param name="param">
        /// Specifies the value that pname is set to.
        /// </param>
        public static void PixelStorei(PixelStoreParameter pname, Int32 param)
        {
            Delegates.glPixelStorei(pname, param);
        }

        /// <summary>
        /// Specify point parameters.
        /// <para>
        /// The following values are accepted for pname:.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies a single-valued point parameter. GL_POINT_FADE_THRESHOLD_SIZE, and
        /// GL_POINT_SPRITE_COORD_ORIGIN are accepted.
        /// </param>
        /// <param name="param">
        /// For glPointParameterf and glPointParameteri, specifies the value that pname will be set to.
        /// </param>
        public static void PointParameterf(PointParameterName pname, Single param)
        {
            Delegates.glPointParameterf(pname, param);
        }

        /// <summary>
        /// Specify point parameters.
        /// <para>
        /// The following values are accepted for pname:.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies a single-valued point parameter. GL_POINT_FADE_THRESHOLD_SIZE, and
        /// GL_POINT_SPRITE_COORD_ORIGIN are accepted.
        /// </param>
        /// <param name="param">
        /// For glPointParameterf and glPointParameteri, specifies the value that pname will be set to.
        /// </param>
        public static void PointParameteri(PointParameterName pname, Int32 param)
        {
            Delegates.glPointParameteri(pname, param);
        }

        /// <summary>
        /// Specify point parameters.
        /// <para>
        /// The following values are accepted for pname:.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies a single-valued point parameter. GL_POINT_FADE_THRESHOLD_SIZE, and
        /// GL_POINT_SPRITE_COORD_ORIGIN are accepted.
        /// </param>
        /// <param name="params">
        /// For glPointParameterfv and glPointParameteriv, specifies a pointer to an array where the value or
        /// values to be assigned to pname are stored.
        /// </param>
        public static void PointParameterfv(PointParameterName pname, Single[] @params)
        {
            Delegates.glPointParameterfv(pname, @params);
        }

        /// <summary>
        /// Specify point parameters.
        /// <para>
        /// The following values are accepted for pname:.
        /// </para>
        /// </summary>
        /// <param name="pname">
        /// Specifies a single-valued point parameter. GL_POINT_FADE_THRESHOLD_SIZE, and
        /// GL_POINT_SPRITE_COORD_ORIGIN are accepted.
        /// </param>
        /// <param name="params">
        /// For glPointParameterfv and glPointParameteriv, specifies a pointer to an array where the value or
        /// values to be assigned to pname are stored.
        /// </param>
        public static void PointParameteriv(PointParameterName pname, Int32[] @params)
        {
            Delegates.glPointParameteriv(pname, @params);
        }

        /// <summary>
        /// Specify the diameter of rasterized points.
        /// <para>
        /// glPointSize specifies the rasterized diameter of points. If point size mode is disabled (see
        /// glEnable with parameter GL_PROGRAM_POINT_SIZE), this value will be used to rasterize points.
        /// Otherwise, the value written to the shading language built-in variable gl_PointSize will be used.
        /// </para>
        /// </summary>
        /// <param name="size">
        /// Specifies the diameter of rasterized points. The initial value is 1.
        /// </param>
        public static void PointSize(Single size)
        {
            Delegates.glPointSize(size);
        }

        /// <summary>
        /// Select a polygon rasterization mode.
        /// <para>
        /// glPolygonMode controls the interpretation of polygons for rasterization. face describes which
        /// polygons mode applies to: both front and back-facing polygons (GL_FRONT_AND_BACK). The polygon mode
        /// affects only the final rasterization of polygons. In particular, a polygon's vertices are lit and
        /// the polygon is clipped and possibly culled before these modes are applied.
        /// </para>
        /// </summary>
        /// <param name="face">
        /// Specifies the polygons that mode applies to. Must be GL_FRONT_AND_BACK for front- and back-facing
        /// polygons.
        /// </param>
        /// <param name="mode">
        /// Specifies how polygons will be rasterized. Accepted values are GL_POINT, GL_LINE, and GL_FILL. The
        /// initial value is GL_FILL for both front- and back-facing polygons.
        /// </param>
        public static void PolygonMode(MaterialFace face, PolygonModeEnum mode)
        {
            Delegates.glPolygonMode(face, mode);
        }

        /// <summary>
        /// Set the scale and units used to calculate depth values.
        /// <para>
        /// When GL_POLYGON_OFFSET_FILL, GL_POLYGON_OFFSET_LINE, or GL_POLYGON_OFFSET_POINT is enabled, each
        /// fragment's depth value will be offset after it is interpolated from the depth values of the
        /// appropriate vertices. The value of the offset is factor Ã— DZ + r Ã— units , where DZ is a
        /// measurement of the change in depth relative to the screen area of the polygon, and r is the smallest
        /// value that is guaranteed to produce a resolvable offset for a given implementation. The offset is
        /// added before the depth test is performed and before the value is written into the depth buffer.
        /// </para>
        /// </summary>
        /// <param name="factor">
        /// Specifies a scale factor that is used to create a variable depth offset for each polygon. The
        /// initial value is 0.
        /// </param>
        /// <param name="units">
        /// Is multiplied by an implementation-specific value to create a constant depth offset. The initial
        /// value is 0.
        /// </param>
        public static void PolygonOffset(Single factor, Single units)
        {
            Delegates.glPolygonOffset(factor, units);
        }

        /// <summary>
        /// Specify the primitive restart index.
        /// <para>
        /// glPrimitiveRestartIndex specifies a vertex array element that is treated specially when primitive
        /// restarting is enabled. This is known as the primitive restart index.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the value to be interpreted as the primitive restart index.
        /// </param>
        public static void PrimitiveRestartIndex(UInt32 index)
        {
            Delegates.glPrimitiveRestartIndex(index);
        }

        /// <summary>
        /// Load a program object with a program binary.
        /// <para>
        /// glProgramBinary loads a program object with a program binary previously returned from
        /// glGetProgramBinary. binaryFormat and binary must be those returned by a previous call to
        /// glGetProgramBinary, and length must be the length returned by glGetProgramBinary, or by glGetProgram
        /// when called with pname set to GL_PROGRAM_BINARY_LENGTH. If these conditions are not met, loading the
        /// program binary will fail and program's GL_LINK_STATUS will be set to GL_FALSE.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of a program object into which to load a program binary.
        /// </param>
        /// <param name="binaryFormat">
        /// Specifies the format of the binary data in binary.
        /// </param>
        /// <param name="binary">
        /// Specifies the address an array containing the binary to be loaded into program.
        /// </param>
        /// <param name="length">
        /// Specifies the number of bytes contained in binary.
        /// </param>
        public static void ProgramBinary(UInt32 program, Int32 binaryFormat, IntPtr binary, Int32 length)
        {
            Delegates.glProgramBinary(program, binaryFormat, binary, length);
        }

        /// <summary>
        /// Specify a parameter for a program object.
        /// <para>
        /// glProgramParameter specifies a new value for the parameter nameed by pname for the program object
        /// program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the name of a program object whose parameter to modify.
        /// </param>
        /// <param name="pname">
        /// Specifies the name of the parameter to modify.
        /// </param>
        /// <param name="value">
        /// Specifies the new value of the parameter specified by pname for program.
        /// </param>
        public static void ProgramParameteri(UInt32 program, Version32 pname, Int32 value)
        {
            Delegates.glProgramParameteri(program, pname, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform1f(UInt32 program, Int32 location, Single v0)
        {
            Delegates.glProgramUniform1f(program, location, v0);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform2f(UInt32 program, Int32 location, Single v0, Single v1)
        {
            Delegates.glProgramUniform2f(program, location, v0, v1);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform3f(UInt32 program, Int32 location, Single v0, Single v1, Single v2)
        {
            Delegates.glProgramUniform3f(program, location, v0, v1, v2);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform4f(UInt32 program, Int32 location, Single v0, Single v1, Single v2, Single v3)
        {
            Delegates.glProgramUniform4f(program, location, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform1i(UInt32 program, Int32 location, Int32 v0)
        {
            Delegates.glProgramUniform1i(program, location, v0);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform2i(UInt32 program, Int32 location, Int32 v0, Int32 v1)
        {
            Delegates.glProgramUniform2i(program, location, v0, v1);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform3i(UInt32 program, Int32 location, Int32 v0, Int32 v1, Int32 v2)
        {
            Delegates.glProgramUniform3i(program, location, v0, v1, v2);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform4i(UInt32 program, Int32 location, Int32 v0, Int32 v1, Int32 v2, Int32 v3)
        {
            Delegates.glProgramUniform4i(program, location, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform1ui(UInt32 program, Int32 location, UInt32 v0)
        {
            Delegates.glProgramUniform1ui(program, location, v0);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform2ui(UInt32 program, Int32 location, Int32 v0, UInt32 v1)
        {
            Delegates.glProgramUniform2ui(program, location, v0, v1);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform3ui(UInt32 program, Int32 location, Int32 v0, Int32 v1, UInt32 v2)
        {
            Delegates.glProgramUniform3ui(program, location, v0, v1, v2);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void ProgramUniform4ui(UInt32 program, Int32 location, Int32 v0, Int32 v1, Int32 v2, UInt32 v3)
        {
            Delegates.glProgramUniform4ui(program, location, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform1fv(UInt32 program, Int32 location, Int32 count, Single[] value)
        {
            Delegates.glProgramUniform1fv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform2fv(UInt32 program, Int32 location, Int32 count, Single[] value)
        {
            Delegates.glProgramUniform2fv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform3fv(UInt32 program, Int32 location, Int32 count, Single[] value)
        {
            Delegates.glProgramUniform3fv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform4fv(UInt32 program, Int32 location, Int32 count, Single[] value)
        {
            Delegates.glProgramUniform4fv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform1iv(UInt32 program, Int32 location, Int32 count, Int32[] value)
        {
            Delegates.glProgramUniform1iv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform2iv(UInt32 program, Int32 location, Int32 count, Int32[] value)
        {
            Delegates.glProgramUniform2iv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform3iv(UInt32 program, Int32 location, Int32 count, Int32[] value)
        {
            Delegates.glProgramUniform3iv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform4iv(UInt32 program, Int32 location, Int32 count, Int32[] value)
        {
            Delegates.glProgramUniform4iv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform1uiv(UInt32 program, Int32 location, Int32 count, UInt32[] value)
        {
            Delegates.glProgramUniform1uiv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform2uiv(UInt32 program, Int32 location, Int32 count, UInt32[] value)
        {
            Delegates.glProgramUniform2uiv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform3uiv(UInt32 program, Int32 location, Int32 count, UInt32[] value)
        {
            Delegates.glProgramUniform3uiv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniform4uiv(UInt32 program, Int32 location, Int32 count, UInt32[] value)
        {
            Delegates.glProgramUniform4uiv(program, location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix2fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix2fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix3fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix3fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix4fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix4fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix2x3fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix2x3fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix3x2fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix3x2fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix2x4fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix2x4fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix4x2fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix4x2fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix3x4fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix3x4fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for a specified program object.
        /// <para>
        /// glProgramUniform modifies the value of a uniform variable or a uniform variable array. The location
        /// of the uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glProgramUniform operates on the program object specified by program.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program containing the uniform variable to be modified.
        /// </param>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector commands (glProgramUniform*v), specifies the number of elements that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is
        /// an array. For the matrix commands (glProgramUniformMatrix*), specifies the number of matrices that
        /// are to be modified. This should be 1 if the targeted uniform variable is not an array of matrices,
        /// and 1 or more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void ProgramUniformMatrix4x3fv(UInt32 program, Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glProgramUniformMatrix4x3fv(program, location, count, transpose, value);
        }

        /// <summary>
        /// Specifiy the vertex to be used as the source of data for flat shaded varyings.
        /// <para>
        /// Flatshading a vertex shader varying output means to assign all vetices of the primitive the same
        /// value for that output. The vertex from which these values is derived is known as the provoking
        /// vertex and glProvokingVertex specifies which vertex is to be used as the source of data for flat
        /// shaded varyings.
        /// </para>
        /// </summary>
        /// <param name="provokeMode">
        /// Specifies the vertex to be used as the source of data for flat shaded varyings.
        /// </param>
        public static void ProvokingVertex(ProvokingVertexMode provokeMode)
        {
            Delegates.glProvokingVertex(provokeMode);
        }

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
        public static void QueryCounter(UInt32 id, QueryTarget target)
        {
            Delegates.glQueryCounter(id, target);
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
        /// <param name="mode">
        /// Specifies a color buffer. Accepted values are GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT,
        /// GL_BACK_RIGHT, GL_FRONT, GL_BACK, GL_LEFT, GL_RIGHT, and the constants GL_COLOR_ATTACHMENTi.
        /// </param>
        public static void ReadBuffer(ReadBufferMode mode)
        {
            Delegates.glReadBuffer(mode);
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
        public static void NamedFramebufferReadBuffer(ReadBufferMode framebuffer, BeginMode mode)
        {
            Delegates.glNamedFramebufferReadBuffer(framebuffer, mode);
        }

        /// <summary>
        /// Read a block of pixels from the frame buffer.
        /// <para>
        /// glReadPixels and glReadnPixels return pixel data from the frame buffer, starting with the pixel
        /// whose lower left corner is at location (x, y), into client memory starting at location data. Several
        /// parameters control the processing of the pixel data before it is placed into client memory. These
        /// parameters are set with glPixelStore. This reference page describes the effects on glReadPixels and
        /// glReadnPixels of most, but not all of the parameters specified by these three commands.
        /// </para>
        /// </summary>
        /// <param name="x">
        /// Specify the window coordinates of the first pixel that is read from the frame buffer. This location
        /// is the lower left corner of a rectangular block of pixels.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the first pixel that is read from the frame buffer. This location
        /// is the lower left corner of a rectangular block of pixels.
        /// </param>
        /// <param name="width">
        /// Specify the dimensions of the pixel rectangle. width and height of one correspond to a single pixel.
        /// </param>
        /// <param name="height">
        /// Specify the dimensions of the pixel rectangle. width and height of one correspond to a single pixel.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted:
        /// GL_STENCIL_INDEX, GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL, GL_RED, GL_GREEN, GL_BLUE, GL_RGB, GL_BGR,
        /// GL_RGBA, and GL_BGRA.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. Must be one of GL_UNSIGNED_BYTE, GL_BYTE,
        /// GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_HALF_FLOAT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, GL_UNSIGNED_INT_2_10_10_10_REV,
        /// GL_UNSIGNED_INT_24_8, GL_UNSIGNED_INT_10F_11F_11F_REV, GL_UNSIGNED_INT_5_9_9_9_REV, or
        /// GL_FLOAT_32_UNSIGNED_INT_24_8_REV.
        /// </param>
        /// <param name="data">
        /// Returns the pixel data.
        /// </param>
        public static void ReadPixels(Int32 x, Int32 y, Int32 width, Int32 height, PixelFormat format, PixelType type, Int32[] data)
        {
            Delegates.glReadPixels(x, y, width, height, format, type, data);
        }

        /// <summary>
        /// Read a block of pixels from the frame buffer.
        /// <para>
        /// glReadPixels and glReadnPixels return pixel data from the frame buffer, starting with the pixel
        /// whose lower left corner is at location (x, y), into client memory starting at location data. Several
        /// parameters control the processing of the pixel data before it is placed into client memory. These
        /// parameters are set with glPixelStore. This reference page describes the effects on glReadPixels and
        /// glReadnPixels of most, but not all of the parameters specified by these three commands.
        /// </para>
        /// </summary>
        /// <param name="x">
        /// Specify the window coordinates of the first pixel that is read from the frame buffer. This location
        /// is the lower left corner of a rectangular block of pixels.
        /// </param>
        /// <param name="y">
        /// Specify the window coordinates of the first pixel that is read from the frame buffer. This location
        /// is the lower left corner of a rectangular block of pixels.
        /// </param>
        /// <param name="width">
        /// Specify the dimensions of the pixel rectangle. width and height of one correspond to a single pixel.
        /// </param>
        /// <param name="height">
        /// Specify the dimensions of the pixel rectangle. width and height of one correspond to a single pixel.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted:
        /// GL_STENCIL_INDEX, GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL, GL_RED, GL_GREEN, GL_BLUE, GL_RGB, GL_BGR,
        /// GL_RGBA, and GL_BGRA.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. Must be one of GL_UNSIGNED_BYTE, GL_BYTE,
        /// GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_HALF_FLOAT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, GL_UNSIGNED_INT_2_10_10_10_REV,
        /// GL_UNSIGNED_INT_24_8, GL_UNSIGNED_INT_10F_11F_11F_REV, GL_UNSIGNED_INT_5_9_9_9_REV, or
        /// GL_FLOAT_32_UNSIGNED_INT_24_8_REV.
        /// </param>
        /// <param name="bufSize">
        /// Specifies the size of the buffer data for glReadnPixels function.
        /// </param>
        /// <param name="data">
        /// Returns the pixel data.
        /// </param>
        public static void ReadnPixels(Int32 x, Int32 y, Int32 width, Int32 height, PixelFormat format, PixelType type, Int32 bufSize, Int32[] data)
        {
            Delegates.glReadnPixels(x, y, width, height, format, type, bufSize, data);
        }

        /// <summary>
        /// Establish data storage, format and dimensions of a renderbuffer object's image.
        /// <para>
        /// glRenderbufferStorage is equivalent to calling glRenderbufferStorageMultisample with the samples set
        /// to zero, and glNamedRenderbufferStorage is equivalent to calling
        /// glNamedRenderbufferStorageMultisample with the samples set to zero.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies a binding target of the allocation for glRenderbufferStorage function. Must be
        /// GL_RENDERBUFFER.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format to use for the renderbuffer object's image.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the renderbuffer, in pixels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the renderbuffer, in pixels.
        /// </param>
        public static void RenderbufferStorage(RenderbufferTarget target, RenderbufferStorageEnum internalFormat, Int32 width, Int32 height)
        {
            Delegates.glRenderbufferStorage(target, internalFormat, width, height);
        }

        /// <summary>
        /// Establish data storage, format and dimensions of a renderbuffer object's image.
        /// <para>
        /// glRenderbufferStorage is equivalent to calling glRenderbufferStorageMultisample with the samples set
        /// to zero, and glNamedRenderbufferStorage is equivalent to calling
        /// glNamedRenderbufferStorageMultisample with the samples set to zero.
        /// </para>
        /// </summary>
        /// <param name="renderbuffer">
        /// Specifies the name of the renderbuffer object for glNamedRenderbufferStorage function.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format to use for the renderbuffer object's image.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the renderbuffer, in pixels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the renderbuffer, in pixels.
        /// </param>
        public static void NamedRenderbufferStorage(UInt32 renderbuffer, RenderbufferStorageEnum internalFormat, Int32 width, Int32 height)
        {
            Delegates.glNamedRenderbufferStorage(renderbuffer, internalFormat, width, height);
        }

        /// <summary>
        /// Establish data storage, format, dimensions and sample count of a renderbuffer object's image.
        /// <para>
        /// glRenderbufferStorageMultisample and glNamedRenderbufferStorageMultisample establish the data
        /// storage, format, dimensions and number of samples of a renderbuffer object's image.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies a binding target of the allocation for glRenderbufferStorageMultisample function. Must be
        /// GL_RENDERBUFFER.
        /// </param>
        /// <param name="samples">
        /// Specifies the number of samples to be used for the renderbuffer object's storage.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format to use for the renderbuffer object's image.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the renderbuffer, in pixels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the renderbuffer, in pixels.
        /// </param>
        public static void RenderbufferStorageMultisample(RenderbufferTarget target, Int32 samples, RenderbufferStorageEnum internalFormat, Int32 width, Int32 height)
        {
            Delegates.glRenderbufferStorageMultisample(target, samples, internalFormat, width, height);
        }

        /// <summary>
        /// Establish data storage, format, dimensions and sample count of a renderbuffer object's image.
        /// <para>
        /// glRenderbufferStorageMultisample and glNamedRenderbufferStorageMultisample establish the data
        /// storage, format, dimensions and number of samples of a renderbuffer object's image.
        /// </para>
        /// </summary>
        /// <param name="renderbuffer">
        /// Specifies the name of the renderbuffer object for glNamedRenderbufferStorageMultisample function.
        /// </param>
        /// <param name="samples">
        /// Specifies the number of samples to be used for the renderbuffer object's storage.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format to use for the renderbuffer object's image.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the renderbuffer, in pixels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the renderbuffer, in pixels.
        /// </param>
        public static void NamedRenderbufferStorageMultisample(UInt32 renderbuffer, Int32 samples, RenderbufferStorageEnum internalFormat, Int32 width, Int32 height)
        {
            Delegates.glNamedRenderbufferStorageMultisample(renderbuffer, samples, internalFormat, width, height);
        }

        /// <summary>
        /// Specify multisample coverage parameters.
        /// <para>
        /// Multisampling samples a pixel multiple times at various implementation-dependent subpixel locations
        /// to generate antialiasing effects. Multisampling transparently antialiases points, lines, polygons,
        /// and images if it is enabled.
        /// </para>
        /// </summary>
        /// <param name="value">
        /// Specify a single floating-point sample coverage value. The value is clamped to the range [0, 1]. The
        /// initial value is 1.0.
        /// </param>
        /// <param name="invert">
        /// Specify a single boolean value representing if the coverage masks should be inverted. GL_TRUE and
        /// GL_FALSE are accepted. The initial value is GL_FALSE.
        /// </param>
        public static void SampleCoverage(Single value, Boolean invert)
        {
            Delegates.glSampleCoverage(value, invert);
        }

        /// <summary>
        /// Set the value of a sub-word of the sample mask.
        /// <para>
        /// glSampleMaski sets one 32-bit sub-word of the multi-word sample mask, GL_SAMPLE_MASK_VALUE.
        /// </para>
        /// </summary>
        /// <param name="maskNumber">
        /// Specifies which 32-bit sub-word of the sample mask to update.
        /// </param>
        /// <param name="mask">
        /// Specifies the new value of the mask sub-word.
        /// </param>
        public static void SampleMaski(UInt32 maskNumber, UInt32 mask)
        {
            Delegates.glSampleMaski(maskNumber, mask);
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
        public static void SamplerParameterf(UInt32 sampler, TextureParameterName pname, Single param)
        {
            Delegates.glSamplerParameterf(sampler, pname, param);
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
        public static void SamplerParameteri(UInt32 sampler, TextureParameterName pname, Int32 param)
        {
            Delegates.glSamplerParameteri(sampler, pname, param);
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
        public static void SamplerParameterfv(UInt32 sampler, TextureParameterName pname, Single[] @params)
        {
            Delegates.glSamplerParameterfv(sampler, pname, @params);
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
        public static void SamplerParameteriv(UInt32 sampler, TextureParameterName pname, Int32[] @params)
        {
            Delegates.glSamplerParameteriv(sampler, pname, @params);
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
        public static void SamplerParameterIiv(UInt32 sampler, TextureParameterName pname, Int32[] @params)
        {
            Delegates.glSamplerParameterIiv(sampler, pname, @params);
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
        public static void SamplerParameterIuiv(UInt32 sampler, TextureParameterName pname, UInt32[] @params)
        {
            Delegates.glSamplerParameterIuiv(sampler, pname, @params);
        }

        /// <summary>
        /// Define the scissor box.
        /// <para>
        /// glScissor defines a rectangle, called the scissor box, in window coordinates. The first two
        /// arguments, x and y, specify the lower left corner of the box. width and height specify the width and
        /// height of the box.
        /// </para>
        /// </summary>
        /// <param name="x">
        /// Specify the lower left corner of the scissor box. Initially (0, 0).
        /// </param>
        /// <param name="y">
        /// Specify the lower left corner of the scissor box. Initially (0, 0).
        /// </param>
        /// <param name="width">
        /// Specify the width and height of the scissor box. When a GL context is first attached to a window,
        /// width and height are set to the dimensions of that window.
        /// </param>
        /// <param name="height">
        /// Specify the width and height of the scissor box. When a GL context is first attached to a window,
        /// width and height are set to the dimensions of that window.
        /// </param>
        public static void Scissor(Int32 x, Int32 y, Int32 width, Int32 height)
        {
            Delegates.glScissor(x, y, width, height);
        }

        /// <summary>
        /// Define the scissor box for multiple viewports.
        /// <para>
        /// glScissorArrayv defines rectangles, called scissor boxes, in window coordinates for each viewport.
        /// first specifies the index of the first scissor box to modify and count specifies the number of
        /// scissor boxes to modify. first must be less than the value of GL_MAX_VIEWPORTS, and first + count
        /// must be less than or equal to the value of GL_MAX_VIEWPORTS. v specifies the address of an array
        /// containing integers specifying the lower left corner of the scissor boxes, and the width and height
        /// of the scissor boxes, in that order.
        /// </para>
        /// </summary>
        /// <param name="first">
        /// Specifies the index of the first viewport whose scissor box to modify.
        /// </param>
        /// <param name="count">
        /// Specifies the number of scissor boxes to modify.
        /// </param>
        /// <param name="v">
        /// Specifies the address of an array containing the left, bottom, width and height of each scissor box,
        /// in that order.
        /// </param>
        public static void ScissorArrayv(UInt32 first, Int32 count, Int32[] v)
        {
            Delegates.glScissorArrayv(first, count, v);
        }

        /// <summary>
        /// Define the scissor box for a specific viewport.
        /// <para>
        /// glScissorIndexed defines the scissor box for a specified viewport. index specifies the index of
        /// scissor box to modify. index must be less than the value of GL_MAX_VIEWPORTS. For glScissorIndexed,
        /// left, bottom, width and height specify the left, bottom, width and height of the scissor box, in
        /// pixels, respectively. For glScissorIndexedv, v specifies the address of an array containing integers
        /// specifying the lower left corner of the scissor box, and the width and height of the scissor box, in
        /// that order.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the viewport whose scissor box to modify.
        /// </param>
        /// <param name="left">
        /// Specify the coordinate of the bottom left corner of the scissor box, in pixels.
        /// </param>
        /// <param name="bottom">
        /// Specify the coordinate of the bottom left corner of the scissor box, in pixels.
        /// </param>
        /// <param name="width">
        /// Specify ths dimensions of the scissor box, in pixels.
        /// </param>
        /// <param name="height">
        /// Specify ths dimensions of the scissor box, in pixels.
        /// </param>
        public static void ScissorIndexed(UInt32 index, Int32 left, Int32 bottom, Int32 width, Int32 height)
        {
            Delegates.glScissorIndexed(index, left, bottom, width, height);
        }

        /// <summary>
        /// Define the scissor box for a specific viewport.
        /// <para>
        /// glScissorIndexed defines the scissor box for a specified viewport. index specifies the index of
        /// scissor box to modify. index must be less than the value of GL_MAX_VIEWPORTS. For glScissorIndexed,
        /// left, bottom, width and height specify the left, bottom, width and height of the scissor box, in
        /// pixels, respectively. For glScissorIndexedv, v specifies the address of an array containing integers
        /// specifying the lower left corner of the scissor box, and the width and height of the scissor box, in
        /// that order.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the viewport whose scissor box to modify.
        /// </param>
        /// <param name="v">
        /// For glScissorIndexedv, specifies the address of an array containing the left, bottom, width and
        /// height of each scissor box, in that order.
        /// </param>
        public static void ScissorIndexedv(UInt32 index, Int32[] v)
        {
            Delegates.glScissorIndexedv(index, v);
        }

        /// <summary>
        /// Load pre-compiled shader binaries.
        /// <para>
        /// glShaderBinary loads pre-compiled shader binary code into the count shader objects whose handles are
        /// given in shaders. binary points to length bytes of binary shader code stored in client memory.
        /// binaryFormat specifies the format of the pre-compiled code.
        /// </para>
        /// </summary>
        /// <param name="count">
        /// Specifies the number of shader object handles contained in shaders.
        /// </param>
        /// <param name="shaders">
        /// Specifies the address of an array of shader handles into which to load pre-compiled shader binaries.
        /// 
        /// </param>
        /// <param name="binaryFormat">
        /// Specifies the format of the shader binaries contained in binary.
        /// </param>
        /// <param name="binary">
        /// Specifies the address of an array of bytes containing pre-compiled binary shader code.
        /// </param>
        /// <param name="length">
        /// Specifies the length of the array whose address is given in binary.
        /// </param>
        public static void ShaderBinary(Int32 count, UInt32[] shaders, Int32 binaryFormat, IntPtr binary, Int32 length)
        {
            Delegates.glShaderBinary(count, shaders, binaryFormat, binary, length);
        }

        /// <summary>
        /// Replaces the source code in a shader object.
        /// <para>
        /// glShaderSource sets the source code in shader to the source code in the array of strings specified
        /// by string. Any source code previously stored in the shader object is completely replaced. The number
        /// of strings in the array is specified by count. If length is NULL, each string is assumed to be null
        /// terminated. If length is a value other than NULL, it points to an array containing a string length
        /// for each of the corresponding elements of string. Each element in the length array may contain the
        /// length of the corresponding string (the null character is not counted as part of the string length)
        /// or a value less than 0 to indicate that the string is null terminated. The source code strings are
        /// not scanned or parsed at this time; they are simply copied into the specified shader object.
        /// </para>
        /// </summary>
        /// <param name="shader">
        /// Specifies the handle of the shader object whose source code is to be replaced.
        /// </param>
        /// <param name="count">
        /// Specifies the number of elements in the string and length arrays.
        /// </param>
        /// <param name="string">
        /// Specifies an array of pointers to strings containing the source code to be loaded into the shader.
        /// </param>
        /// <param name="length">
        /// Specifies an array of string lengths.
        /// </param>
        public static void ShaderSource(UInt32 shader, Int32 count, String[] @string, Int32[] length)
        {
            Delegates.glShaderSource(shader, count, @string, length);
        }

        /// <summary>
        /// Change an active shader storage block binding.
        /// <para>
        /// glShaderStorageBlockBinding, changes the active shader storage block with an assigned index of
        /// storageBlockIndex in program object program. storageBlockIndex must be an active shader storage
        /// block index in program. storageBlockBinding must be less than the value of
        /// GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS. If successful, glShaderStorageBlockBinding specifies that
        /// program will use the data store of the buffer object bound to the binding point storageBlockBinding
        /// to read and write the values of the buffer variables in the shader storage block identified by
        /// storageBlockIndex.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of the program containing the block whose binding to change.
        /// </param>
        /// <param name="storageBlockIndex">
        /// The index storage block within the program.
        /// </param>
        /// <param name="storageBlockBinding">
        /// The index storage block binding to associate with the specified storage block.
        /// </param>
        public static void ShaderStorageBlockBinding(UInt32 program, UInt32 storageBlockIndex, UInt32 storageBlockBinding)
        {
            Delegates.glShaderStorageBlockBinding(program, storageBlockIndex, storageBlockBinding);
        }

        /// <summary>
        /// Set front and back function and reference value for stencil testing.
        /// <para>
        /// Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis. Stencil planes
        /// are first drawn into using GL drawing primitives, then geometry and images are rendered using the
        /// stencil planes to mask out portions of the screen. Stenciling is typically used in multipass
        /// rendering algorithms to achieve special effects, such as decals, outlining, and constructive solid
        /// geometry rendering.
        /// </para>
        /// </summary>
        /// <param name="func">
        /// Specifies the test function. Eight symbolic constants are valid: GL_NEVER, GL_LESS, GL_LEQUAL,
        /// GL_GREATER, GL_GEQUAL, GL_EQUAL, GL_NOTEQUAL, and GL_ALWAYS. The initial value is GL_ALWAYS.
        /// </param>
        /// <param name="ref">
        /// Specifies the reference value for the stencil test. ref is clamped to the range [0, 2^n - 1], where
        /// n is the number of bitplanes in the stencil buffer. The initial value is 0.
        /// </param>
        /// <param name="mask">
        /// Specifies a mask that is ANDed with both the reference value and the stored stencil value when the
        /// test is done. The initial value is all 1's.
        /// </param>
        public static void StencilFunc(StencilFunction func, Int32 @ref, UInt32 mask)
        {
            Delegates.glStencilFunc(func, @ref, mask);
        }

        /// <summary>
        /// Set front and/or back function and reference value for stencil testing.
        /// <para>
        /// Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis. You draw into
        /// the stencil planes using GL drawing primitives, then render geometry and images, using the stencil
        /// planes to mask out portions of the screen. Stenciling is typically used in multipass rendering
        /// algorithms to achieve special effects, such as decals, outlining, and constructive solid geometry
        /// rendering.
        /// </para>
        /// </summary>
        /// <param name="face">
        /// Specifies whether front and/or back stencil state is updated. Three symbolic constants are valid:
        /// GL_FRONT, GL_BACK, and GL_FRONT_AND_BACK.
        /// </param>
        /// <param name="func">
        /// Specifies the test function. Eight symbolic constants are valid: GL_NEVER, GL_LESS, GL_LEQUAL,
        /// GL_GREATER, GL_GEQUAL, GL_EQUAL, GL_NOTEQUAL, and GL_ALWAYS. The initial value is GL_ALWAYS.
        /// </param>
        /// <param name="ref">
        /// Specifies the reference value for the stencil test. ref is clamped to the range [0, 2^n - 1], where
        /// n is the number of bitplanes in the stencil buffer. The initial value is 0.
        /// </param>
        /// <param name="mask">
        /// Specifies a mask that is ANDed with both the reference value and the stored stencil value when the
        /// test is done. The initial value is all 1's.
        /// </param>
        public static void StencilFuncSeparate(StencilFace face, StencilFunction func, Int32 @ref, UInt32 mask)
        {
            Delegates.glStencilFuncSeparate(face, func, @ref, mask);
        }

        /// <summary>
        /// Control the front and back writing of individual bits in the stencil planes.
        /// <para>
        /// glStencilMask controls the writing of individual bits in the stencil planes. The least significant n
        /// bits of mask, where n is the number of bits in the stencil buffer, specify a mask. Where a 1 appears
        /// in the mask, it's possible to write to the corresponding bit in the stencil buffer. Where a 0
        /// appears, the corresponding bit is write-protected. Initially, all bits are enabled for writing.
        /// </para>
        /// </summary>
        /// <param name="mask">
        /// Specifies a bit mask to enable and disable writing of individual bits in the stencil planes.
        /// Initially, the mask is all 1's.
        /// </param>
        public static void StencilMask(UInt32 mask)
        {
            Delegates.glStencilMask(mask);
        }

        /// <summary>
        /// Control the front and/or back writing of individual bits in the stencil planes.
        /// <para>
        /// glStencilMaskSeparate controls the writing of individual bits in the stencil planes. The least
        /// significant n bits of mask, where n is the number of bits in the stencil buffer, specify a mask.
        /// Where a 1 appears in the mask, it's possible to write to the corresponding bit in the stencil
        /// buffer. Where a 0 appears, the corresponding bit is write-protected. Initially, all bits are enabled
        /// for writing.
        /// </para>
        /// </summary>
        /// <param name="face">
        /// Specifies whether the front and/or back stencil writemask is updated. Three symbolic constants are
        /// valid: GL_FRONT, GL_BACK, and GL_FRONT_AND_BACK.
        /// </param>
        /// <param name="mask">
        /// Specifies a bit mask to enable and disable writing of individual bits in the stencil planes.
        /// Initially, the mask is all 1's.
        /// </param>
        public static void StencilMaskSeparate(StencilFace face, UInt32 mask)
        {
            Delegates.glStencilMaskSeparate(face, mask);
        }

        /// <summary>
        /// Set front and back stencil test actions.
        /// <para>
        /// Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis. You draw into
        /// the stencil planes using GL drawing primitives, then render geometry and images, using the stencil
        /// planes to mask out portions of the screen. Stenciling is typically used in multipass rendering
        /// algorithms to achieve special effects, such as decals, outlining, and constructive solid geometry
        /// rendering.
        /// </para>
        /// </summary>
        /// <param name="sfail">
        /// Specifies the action to take when the stencil test fails. Eight symbolic constants are accepted:
        /// GL_KEEP, GL_ZERO, GL_REPLACE, GL_INCR, GL_INCR_WRAP, GL_DECR, GL_DECR_WRAP, and GL_INVERT. The
        /// initial value is GL_KEEP.
        /// </param>
        /// <param name="dpfail">
        /// Specifies the stencil action when the stencil test passes, but the depth test fails. dpfail accepts
        /// the same symbolic constants as sfail. The initial value is GL_KEEP.
        /// </param>
        /// <param name="dppass">
        /// Specifies the stencil action when both the stencil test and the depth test pass, or when the stencil
        /// test passes and either there is no depth buffer or depth testing is not enabled. dppass accepts the
        /// same symbolic constants as sfail. The initial value is GL_KEEP.
        /// </param>
        public static void StencilOp(StencilOpEnum sfail, StencilOpEnum dpfail, StencilOpEnum dppass)
        {
            Delegates.glStencilOp(sfail, dpfail, dppass);
        }

        /// <summary>
        /// Set front and/or back stencil test actions.
        /// <para>
        /// Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis. You draw into
        /// the stencil planes using GL drawing primitives, then render geometry and images, using the stencil
        /// planes to mask out portions of the screen. Stenciling is typically used in multipass rendering
        /// algorithms to achieve special effects, such as decals, outlining, and constructive solid geometry
        /// rendering.
        /// </para>
        /// </summary>
        /// <param name="face">
        /// Specifies whether front and/or back stencil state is updated. Three symbolic constants are valid:
        /// GL_FRONT, GL_BACK, and GL_FRONT_AND_BACK.
        /// </param>
        /// <param name="sfail">
        /// Specifies the action to take when the stencil test fails. Eight symbolic constants are accepted:
        /// GL_KEEP, GL_ZERO, GL_REPLACE, GL_INCR, GL_INCR_WRAP, GL_DECR, GL_DECR_WRAP, and GL_INVERT. The
        /// initial value is GL_KEEP.
        /// </param>
        /// <param name="dpfail">
        /// Specifies the stencil action when the stencil test passes, but the depth test fails. dpfail accepts
        /// the same symbolic constants as sfail. The initial value is GL_KEEP.
        /// </param>
        /// <param name="dppass">
        /// Specifies the stencil action when both the stencil test and the depth test pass, or when the stencil
        /// test passes and either there is no depth buffer or depth testing is not enabled. dppass accepts the
        /// same symbolic constants as sfail. The initial value is GL_KEEP.
        /// </param>
        public static void StencilOpSeparate(StencilFace face, StencilOpEnum sfail, StencilOpEnum dpfail, StencilOpEnum dppass)
        {
            Delegates.glStencilOpSeparate(face, sfail, dpfail, dppass);
        }

        /// <summary>
        /// Attach a buffer object's data store to a buffer texture object.
        /// <para>
        /// glTexBuffer and glTextureBuffer attaches the data store of a specified buffer object to a specified
        /// texture object, and specify the storage format for the texture image found found in the buffer
        /// object. The texture object must be a buffer texture.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexBuffer. Must be GL_TEXTURE_BUFFER.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format of the data in the store belonging to buffer.
        /// </param>
        /// <param name="buffer">
        /// Specifies the name of the buffer object whose storage to attach to the active buffer texture.
        /// </param>
        public static void TexBuffer(TextureBufferTarget target, SizedInternalFormat internalFormat, UInt32 buffer)
        {
            Delegates.glTexBuffer(target, internalFormat, buffer);
        }

        /// <summary>
        /// Attach a buffer object's data store to a buffer texture object.
        /// <para>
        /// glTexBuffer and glTextureBuffer attaches the data store of a specified buffer object to a specified
        /// texture object, and specify the storage format for the texture image found found in the buffer
        /// object. The texture object must be a buffer texture.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureBuffer.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format of the data in the store belonging to buffer.
        /// </param>
        /// <param name="buffer">
        /// Specifies the name of the buffer object whose storage to attach to the active buffer texture.
        /// </param>
        public static void TextureBuffer(UInt32 texture, SizedInternalFormat internalFormat, UInt32 buffer)
        {
            Delegates.glTextureBuffer(texture, internalFormat, buffer);
        }

        /// <summary>
        /// Attach a range of a buffer object's data store to a buffer texture object.
        /// <para>
        /// glTexBufferRange and glTextureBufferRange attach a range of the data store of a specified buffer
        /// object to a specified texture object, and specify the storage format for the texture image found
        /// found in the buffer object. The texture object must be a buffer texture.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glTexBufferRange. Must be
        /// GL_TEXTURE_BUFFER.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format of the data in the store belonging to buffer.
        /// </param>
        /// <param name="buffer">
        /// Specifies the name of the buffer object whose storage to attach to the active buffer texture.
        /// </param>
        /// <param name="offset">
        /// Specifies the offset of the start of the range of the buffer's data store to attach.
        /// </param>
        /// <param name="size">
        /// Specifies the size of the range of the buffer's data store to attach.
        /// </param>
        public static void TexBufferRange(BufferTarget target, SizedInternalFormat internalFormat, UInt32 buffer, IntPtr offset, IntPtr size)
        {
            Delegates.glTexBufferRange(target, internalFormat, buffer, offset, size);
        }

        /// <summary>
        /// Attach a range of a buffer object's data store to a buffer texture object.
        /// <para>
        /// glTexBufferRange and glTextureBufferRange attach a range of the data store of a specified buffer
        /// object to a specified texture object, and specify the storage format for the texture image found
        /// found in the buffer object. The texture object must be a buffer texture.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureBufferRange.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format of the data in the store belonging to buffer.
        /// </param>
        /// <param name="buffer">
        /// Specifies the name of the buffer object whose storage to attach to the active buffer texture.
        /// </param>
        /// <param name="offset">
        /// Specifies the offset of the start of the range of the buffer's data store to attach.
        /// </param>
        /// <param name="size">
        /// Specifies the size of the range of the buffer's data store to attach.
        /// </param>
        public static void TextureBufferRange(UInt32 texture, SizedInternalFormat internalFormat, UInt32 buffer, IntPtr offset, Int32 size)
        {
            Delegates.glTextureBufferRange(texture, internalFormat, buffer, offset, size);
        }

        /// <summary>
        /// Specify a one-dimensional texture image.
        /// <para>
        /// Texturing maps a portion of a specified texture image onto each graphical primitive for which
        /// texturing is enabled. To enable and disable one-dimensional texturing, call glEnable and glDisable
        /// with argument GL_TEXTURE_1D.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target texture. Must be GL_TEXTURE_1D or GL_PROXY_TEXTURE_1D.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the number of color components in the texture. Must be one of base internal formats given
        /// in Table 1, one of the sized internal formats given in Table 2, or one of the compressed internal
        /// formats given in Table 3, below.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture image. All implementations support texture images that are at
        /// least 1024 texels wide. The height of the 1D texture image is 1.
        /// </param>
        /// <param name="border">
        /// This value must be 0.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_BGRA, GL_RED_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER, GL_BGR_INTEGER,
        /// GL_RGBA_INTEGER, GL_BGRA_INTEGER, GL_STENCIL_INDEX, GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TexImage1D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 border, PixelFormat format, PixelType type, IntPtr data)
        {
            Delegates.glTexImage1D(target, level, internalFormat, width, border, format, type, data);
        }

        /// <summary>
        /// Specify a two-dimensional texture image.
        /// <para>
        /// Texturing allows elements of an image array to be read by shaders.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target texture. Must be GL_TEXTURE_2D, GL_PROXY_TEXTURE_2D, GL_TEXTURE_1D_ARRAY,
        /// GL_PROXY_TEXTURE_1D_ARRAY, GL_TEXTURE_RECTANGLE, GL_PROXY_TEXTURE_RECTANGLE,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, or
        /// GL_PROXY_TEXTURE_CUBE_MAP.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image. If target is GL_TEXTURE_RECTANGLE or GL_PROXY_TEXTURE_RECTANGLE, level must be 0.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the number of color components in the texture. Must be one of base internal formats given
        /// in Table 1, one of the sized internal formats given in Table 2, or one of the compressed internal
        /// formats given in Table 3, below.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture image. All implementations support texture images that are at
        /// least 1024 texels wide.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture image, or the number of layers in a texture array, in the case
        /// of the GL_TEXTURE_1D_ARRAY and GL_PROXY_TEXTURE_1D_ARRAY targets. All implementations support 2D
        /// texture images that are at least 1024 texels high, and texture arrays that are at least 256 layers
        /// deep.
        /// </param>
        /// <param name="border">
        /// This value must be 0.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_BGRA, GL_RED_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER, GL_BGR_INTEGER,
        /// GL_RGBA_INTEGER, GL_BGRA_INTEGER, GL_STENCIL_INDEX, GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TexImage2D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 border, PixelFormat format, PixelType type, IntPtr data)
        {
            Delegates.glTexImage2D(target, level, internalFormat, width, height, border, format, type, data);
        }

        /// <summary>
        /// Establish the data storage, format, dimensions, and number of samples of a multisample texture's image.
        /// <para>
        /// glTexImage2DMultisample establishes the data storage, format, dimensions and number of samples of a
        /// multisample texture's image.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target of the operation. target must be GL_TEXTURE_2D_MULTISAMPLE or
        /// GL_PROXY_TEXTURE_2D_MULTISAMPLE.
        /// </param>
        /// <param name="samples">
        /// The number of samples in the multisample texture's image.
        /// </param>
        /// <param name="internalFormat">
        /// The internal format to be used to store the multisample texture's image. internalformat must specify
        /// a color-renderable, depth-renderable, or stencil-renderable format.
        /// </param>
        /// <param name="width">
        /// The width of the multisample texture's image, in texels.
        /// </param>
        /// <param name="height">
        /// The height of the multisample texture's image, in texels.
        /// </param>
        /// <param name="fixedsamplelocations">
        /// Specifies whether the image will use identical sample locations and the same number of samples for
        /// all texels in the image, and the sample locations will not depend on the internal format or size of
        /// the image.
        /// </param>
        public static void TexImage2DMultisample(TextureTargetMultisample target, Int32 samples, PixelInternalFormat internalFormat, Int32 width, Int32 height, Boolean fixedsamplelocations)
        {
            Delegates.glTexImage2DMultisample(target, samples, internalFormat, width, height, fixedsamplelocations);
        }

        /// <summary>
        /// Specify a three-dimensional texture image.
        /// <para>
        /// Texturing maps a portion of a specified texture image onto each graphical primitive for which
        /// texturing is enabled. To enable and disable three-dimensional texturing, call glEnable and glDisable
        /// with argument GL_TEXTURE_3D.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target texture. Must be one of GL_TEXTURE_3D, GL_PROXY_TEXTURE_3D, GL_TEXTURE_2D_ARRAY
        /// or GL_PROXY_TEXTURE_2D_ARRAY.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the n th mipmap
        /// reduction image.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the number of color components in the texture. Must be one of base internal formats given
        /// in Table 1, one of the sized internal formats given in Table 2, or one of the compressed internal
        /// formats given in Table 3, below.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture image. All implementations support 3D texture images that are at
        /// least 16 texels wide.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture image. All implementations support 3D texture images that are at
        /// least 256 texels high.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture image, or the number of layers in a texture array. All
        /// implementations support 3D texture images that are at least 256 texels deep, and texture arrays that
        /// are at least 256 layers deep.
        /// </param>
        /// <param name="border">
        /// This value must be 0.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_BGRA, GL_RED_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER, GL_BGR_INTEGER,
        /// GL_RGBA_INTEGER, GL_BGRA_INTEGER, GL_STENCIL_INDEX, GL_DEPTH_COMPONENT, GL_DEPTH_STENCIL.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="data">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TexImage3D(TextureTarget target, Int32 level, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Int32 border, PixelFormat format, PixelType type, IntPtr data)
        {
            Delegates.glTexImage3D(target, level, internalFormat, width, height, depth, border, format, type, data);
        }

        /// <summary>
        /// Establish the data storage, format, dimensions, and number of samples of a multisample texture's image.
        /// <para>
        /// glTexImage3DMultisample establishes the data storage, format, dimensions and number of samples of a
        /// multisample texture's image.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target of the operation. target must be GL_TEXTURE_2D_MULTISAMPLE_ARRAY or
        /// GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY.
        /// </param>
        /// <param name="samples">
        /// The number of samples in the multisample texture's image.
        /// </param>
        /// <param name="internalFormat">
        /// The internal format to be used to store the multisample texture's image. internalformat must specify
        /// a color-renderable, depth-renderable, or stencil-renderable format.
        /// </param>
        /// <param name="width">
        /// The width of the multisample texture's image, in texels.
        /// </param>
        /// <param name="height">
        /// The height of the multisample texture's image, in texels.
        /// </param>
        /// <param name="depth">
        /// </param>
        /// <param name="fixedsamplelocations">
        /// Specifies whether the image will use identical sample locations and the same number of samples for
        /// all texels in the image, and the sample locations will not depend on the internal format or size of
        /// the image.
        /// </param>
        public static void TexImage3DMultisample(TextureTargetMultisample target, Int32 samples, PixelInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Boolean fixedsamplelocations)
        {
            Delegates.glTexImage3DMultisample(target, samples, internalFormat, width, height, depth, fixedsamplelocations);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexParameter functions. Must be one of
        /// GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="param">
        /// For the scalar commands, specifies the value of pname.
        /// </param>
        public static void TexParameterf(TextureTarget target, TextureParameterName pname, Single param)
        {
            Delegates.glTexParameterf(target, pname, param);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexParameter functions. Must be one of
        /// GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="param">
        /// For the scalar commands, specifies the value of pname.
        /// </param>
        public static void TexParameteri(TextureTarget target, TextureParameterName pname, Int32 param)
        {
            Delegates.glTexParameteri(target, pname, param);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureParameter functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="param">
        /// For the scalar commands, specifies the value of pname.
        /// </param>
        public static void TextureParameterf(UInt32 texture, TextureParameter pname, Single param)
        {
            Delegates.glTextureParameterf(texture, pname, param);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureParameter functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="param">
        /// For the scalar commands, specifies the value of pname.
        /// </param>
        public static void TextureParameteri(UInt32 texture, TextureParameter pname, Int32 param)
        {
            Delegates.glTextureParameteri(texture, pname, param);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexParameter functions. Must be one of
        /// GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="params">
        /// For the vector commands, specifies a pointer to an array where the value or values of pname are
        /// stored.
        /// </param>
        public static void TexParameterfv(TextureTarget target, TextureParameterName pname, Single[] @params)
        {
            Delegates.glTexParameterfv(target, pname, @params);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexParameter functions. Must be one of
        /// GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="params">
        /// For the vector commands, specifies a pointer to an array where the value or values of pname are
        /// stored.
        /// </param>
        public static void TexParameteriv(TextureTarget target, TextureParameterName pname, Int32[] @params)
        {
            Delegates.glTexParameteriv(target, pname, @params);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexParameter functions. Must be one of
        /// GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="params">
        /// For the vector commands, specifies a pointer to an array where the value or values of pname are
        /// stored.
        /// </param>
        public static void TexParameterIiv(TextureTarget target, TextureParameterName pname, Int32[] @params)
        {
            Delegates.glTexParameterIiv(target, pname, @params);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexParameter functions. Must be one of
        /// GL_TEXTURE_1D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="params">
        /// For the vector commands, specifies a pointer to an array where the value or values of pname are
        /// stored.
        /// </param>
        public static void TexParameterIuiv(TextureTarget target, TextureParameterName pname, UInt32[] @params)
        {
            Delegates.glTexParameterIuiv(target, pname, @params);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureParameter functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="paramtexture">
        /// </param>
        public static void TextureParameterfv(UInt32 texture, TextureParameter pname, Single[] paramtexture)
        {
            Delegates.glTextureParameterfv(texture, pname, paramtexture);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureParameter functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="param">
        /// For the scalar commands, specifies the value of pname.
        /// </param>
        public static void TextureParameteriv(UInt32 texture, TextureParameter pname, Int32[] param)
        {
            Delegates.glTextureParameteriv(texture, pname, param);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureParameter functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="params">
        /// For the vector commands, specifies a pointer to an array where the value or values of pname are
        /// stored.
        /// </param>
        public static void TextureParameterIiv(UInt32 texture, TextureParameter pname, Int32[] @params)
        {
            Delegates.glTextureParameterIiv(texture, pname, @params);
        }

        /// <summary>
        /// Set texture parameters.
        /// <para>
        /// glTexParameter and glTextureParameter assign the value or values in params to the texture parameter
        /// specified as pname. For glTexParameter, target defines the target texture, either GL_TEXTURE_1D,
        /// GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_2D_MULTISAMPLE,
        /// GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_ARRAY, or
        /// GL_TEXTURE_RECTANGLE. The following symbols are accepted in pname:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureParameter functions.
        /// </param>
        /// <param name="pname">
        /// Specifies the symbolic name of a single-valued texture parameter. pname can be one of the following:
        /// GL_DEPTH_STENCIL_TEXTURE_MODE, GL_TEXTURE_BASE_LEVEL, GL_TEXTURE_COMPARE_FUNC,
        /// GL_TEXTURE_COMPARE_MODE, GL_TEXTURE_LOD_BIAS, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_MAG_FILTER,
        /// GL_TEXTURE_MIN_LOD, GL_TEXTURE_MAX_LOD, GL_TEXTURE_MAX_LEVEL, GL_TEXTURE_SWIZZLE_R,
        /// GL_TEXTURE_SWIZZLE_G, GL_TEXTURE_SWIZZLE_B, GL_TEXTURE_SWIZZLE_A, GL_TEXTURE_WRAP_S,
        /// GL_TEXTURE_WRAP_T, or GL_TEXTURE_WRAP_R. For the vector commands (glTexParameter*v), pname can also
        /// be one of GL_TEXTURE_BORDER_COLOR or GL_TEXTURE_SWIZZLE_RGBA.
        /// </param>
        /// <param name="params">
        /// For the vector commands, specifies a pointer to an array where the value or values of pname are
        /// stored.
        /// </param>
        public static void TextureParameterIuiv(UInt32 texture, TextureParameter pname, UInt32[] @params)
        {
            Delegates.glTextureParameterIuiv(texture, pname, @params);
        }

        /// <summary>
        /// Simultaneously specify storage for all levels of a one-dimensional texture.
        /// <para>
        /// glTexStorage1D and glTextureStorage1D specify the storage requirements for all levels of a
        /// one-dimensional texture simultaneously. Once a texture is specified with this command, the format
        /// and dimensions of all levels become immutable unless it is a proxy texture. The contents of the
        /// image may still be modified, however, its storage requirements may not change. Such a texture is
        /// referred to as an immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glTexStorage1D. Must be one of
        /// GL_TEXTURE_1D or GL_PROXY_TEXTURE_1D.
        /// </param>
        /// <param name="levels">
        /// Specify the number of texture levels.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        public static void TexStorage1D(TextureTarget target, Int32 levels, SizedInternalFormat internalFormat, Int32 width)
        {
            Delegates.glTexStorage1D(target, levels, internalFormat, width);
        }

        /// <summary>
        /// Simultaneously specify storage for all levels of a one-dimensional texture.
        /// <para>
        /// glTexStorage1D and glTextureStorage1D specify the storage requirements for all levels of a
        /// one-dimensional texture simultaneously. Once a texture is specified with this command, the format
        /// and dimensions of all levels become immutable unless it is a proxy texture. The contents of the
        /// image may still be modified, however, its storage requirements may not change. Such a texture is
        /// referred to as an immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureStorage1D. The effective target of texture must be
        /// one of the valid non-proxy target values above.
        /// </param>
        /// <param name="levels">
        /// Specify the number of texture levels.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        public static void TextureStorage1D(UInt32 texture, Int32 levels, SizedInternalFormat internalFormat, Int32 width)
        {
            Delegates.glTextureStorage1D(texture, levels, internalFormat, width);
        }

        /// <summary>
        /// Simultaneously specify storage for all levels of a two-dimensional or one-dimensional array texture.
        /// <para>
        /// glTexStorage2D and glTextureStorage2D specify the storage requirements for all levels of a
        /// two-dimensional texture or one-dimensional texture array simultaneously. Once a texture is specified
        /// with this command, the format and dimensions of all levels become immutable unless it is a proxy
        /// texture. The contents of the image may still be modified, however, its storage requirements may not
        /// change. Such a texture is referred to as an immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glTexStorage2D. Must be one of
        /// GL_TEXTURE_2D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_RECTANGLE, GL_PROXY_TEXTURE_2D,
        /// GL_PROXY_TEXTURE_1D_ARRAY, GL_PROXY_TEXTURE_RECTANGLE, or GL_PROXY_TEXTURE_CUBE_MAP.
        /// </param>
        /// <param name="levels">
        /// Specify the number of texture levels.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture, in texels.
        /// </param>
        public static void TexStorage2D(TextureTarget target, Int32 levels, SizedInternalFormat internalFormat, Int32 width, Int32 height)
        {
            Delegates.glTexStorage2D(target, levels, internalFormat, width, height);
        }

        /// <summary>
        /// Simultaneously specify storage for all levels of a two-dimensional or one-dimensional array texture.
        /// <para>
        /// glTexStorage2D and glTextureStorage2D specify the storage requirements for all levels of a
        /// two-dimensional texture or one-dimensional texture array simultaneously. Once a texture is specified
        /// with this command, the format and dimensions of all levels become immutable unless it is a proxy
        /// texture. The contents of the image may still be modified, however, its storage requirements may not
        /// change. Such a texture is referred to as an immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureStorage2D. The effective target of texture must be
        /// one of the valid non-proxy target values above.
        /// </param>
        /// <param name="levels">
        /// Specify the number of texture levels.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture, in texels.
        /// </param>
        public static void TextureStorage2D(UInt32 texture, Int32 levels, SizedInternalFormat internalFormat, Int32 width, Int32 height)
        {
            Delegates.glTextureStorage2D(texture, levels, internalFormat, width, height);
        }

        /// <summary>
        /// Specify storage for a two-dimensional multisample texture.
        /// <para>
        /// glTexStorage2DMultisample and glTextureStorage2DMultisample specify the storage requirements for a
        /// two-dimensional multisample texture. Once a texture is specified with this command, its format and
        /// dimensions become immutable unless it is a proxy texture. The contents of the image may still be
        /// modified, however, its storage requirements may not change. Such a texture is referred to as an
        /// immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glTexStorage2DMultisample. Must be one
        /// of GL_TEXTURE_2D_MULTISAMPLE or GL_PROXY_TEXTURE_2D_MULTISAMPLE.
        /// </param>
        /// <param name="samples">
        /// Specify the number of samples in the texture.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture, in texels.
        /// </param>
        /// <param name="fixedsamplelocations">
        /// Specifies whether the image will use identical sample locations and the same number of samples for
        /// all texels in the image, and the sample locations will not depend on the internal format or size of
        /// the image.
        /// </param>
        public static void TexStorage2DMultisample(TextureTarget target, Int32 samples, SizedInternalFormat internalFormat, Int32 width, Int32 height, Boolean fixedsamplelocations)
        {
            Delegates.glTexStorage2DMultisample(target, samples, internalFormat, width, height, fixedsamplelocations);
        }

        /// <summary>
        /// Specify storage for a two-dimensional multisample texture.
        /// <para>
        /// glTexStorage2DMultisample and glTextureStorage2DMultisample specify the storage requirements for a
        /// two-dimensional multisample texture. Once a texture is specified with this command, its format and
        /// dimensions become immutable unless it is a proxy texture. The contents of the image may still be
        /// modified, however, its storage requirements may not change. Such a texture is referred to as an
        /// immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureStorage2DMultisample. The effective target of texture
        /// must be one of the valid non-proxy target values above.
        /// </param>
        /// <param name="samples">
        /// Specify the number of samples in the texture.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture, in texels.
        /// </param>
        /// <param name="fixedsamplelocations">
        /// Specifies whether the image will use identical sample locations and the same number of samples for
        /// all texels in the image, and the sample locations will not depend on the internal format or size of
        /// the image.
        /// </param>
        public static void TextureStorage2DMultisample(UInt32 texture, Int32 samples, SizedInternalFormat internalFormat, Int32 width, Int32 height, Boolean fixedsamplelocations)
        {
            Delegates.glTextureStorage2DMultisample(texture, samples, internalFormat, width, height, fixedsamplelocations);
        }

        /// <summary>
        /// Simultaneously specify storage for all levels of a three-dimensional, two-dimensional array or cube-map array texture.
        /// <para>
        /// glTexStorage3D and glTextureStorage3D specify specify the storage requirements for all levels of a
        /// three-dimensional, two-dimensional array or cube-map array texture simultaneously. Once a texture is
        /// specified with this command, the format and dimensions of all levels become immutable unless it is a
        /// proxy texture. The contents of the image may still be modified, however, its storage requirements
        /// may not change. Such a texture is referred to as an immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glTexStorage3D. Must be one of
        /// GL_TEXTURE_3D, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_CUBE_ARRAY, GL_PROXY_TEXTURE_3D,
        /// GL_PROXY_TEXTURE_2D_ARRAY or GL_PROXY_TEXTURE_CUBE_ARRAY.
        /// </param>
        /// <param name="levels">
        /// Specify the number of texture levels.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture, in texels.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture, in texels.
        /// </param>
        public static void TexStorage3D(TextureTarget target, Int32 levels, SizedInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth)
        {
            Delegates.glTexStorage3D(target, levels, internalFormat, width, height, depth);
        }

        /// <summary>
        /// Simultaneously specify storage for all levels of a three-dimensional, two-dimensional array or cube-map array texture.
        /// <para>
        /// glTexStorage3D and glTextureStorage3D specify specify the storage requirements for all levels of a
        /// three-dimensional, two-dimensional array or cube-map array texture simultaneously. Once a texture is
        /// specified with this command, the format and dimensions of all levels become immutable unless it is a
        /// proxy texture. The contents of the image may still be modified, however, its storage requirements
        /// may not change. Such a texture is referred to as an immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureStorage3D. The effective target of texture must be
        /// one of the valid non-proxy target values above.
        /// </param>
        /// <param name="levels">
        /// Specify the number of texture levels.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture, in texels.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture, in texels.
        /// </param>
        public static void TextureStorage3D(UInt32 texture, Int32 levels, SizedInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth)
        {
            Delegates.glTextureStorage3D(texture, levels, internalFormat, width, height, depth);
        }

        /// <summary>
        /// Specify storage for a two-dimensional multisample array texture.
        /// <para>
        /// glTexStorage3DMultisample and glTextureStorage3DMultisample specify the storage requirements for a
        /// two-dimensional multisample array texture. Once a texture is specified with this command, its format
        /// and dimensions become immutable unless it is a proxy texture. The contents of the image may still be
        /// modified, however, its storage requirements may not change. Such a texture is referred to as an
        /// immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture object is bound for glTexStorage3DMultisample. Must be one
        /// of GL_TEXTURE_2D_MULTISAMPLE_ARRAY or GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY.
        /// </param>
        /// <param name="samples">
        /// Specify the number of samples in the texture.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture, in texels.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture, in layers.
        /// </param>
        /// <param name="fixedsamplelocations">
        /// Specifies whether the image will use identical sample locations and the same number of samples for
        /// all texels in the image, and the sample locations will not depend on the internal format or size of
        /// the image.
        /// </param>
        public static void TexStorage3DMultisample(TextureTarget target, Int32 samples, SizedInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Boolean fixedsamplelocations)
        {
            Delegates.glTexStorage3DMultisample(target, samples, internalFormat, width, height, depth, fixedsamplelocations);
        }

        /// <summary>
        /// Specify storage for a two-dimensional multisample array texture.
        /// <para>
        /// glTexStorage3DMultisample and glTextureStorage3DMultisample specify the storage requirements for a
        /// two-dimensional multisample array texture. Once a texture is specified with this command, its format
        /// and dimensions become immutable unless it is a proxy texture. The contents of the image may still be
        /// modified, however, its storage requirements may not change. Such a texture is referred to as an
        /// immutable-format texture.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureStorage3DMultisample. The effective target of texture
        /// must be one of the valid non-proxy target values above.
        /// </param>
        /// <param name="samples">
        /// Specify the number of samples in the texture.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the sized internal format to be used to store texture image data.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture, in texels.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture, in texels.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture, in layers.
        /// </param>
        /// <param name="fixedsamplelocations">
        /// Specifies whether the image will use identical sample locations and the same number of samples for
        /// all texels in the image, and the sample locations will not depend on the internal format or size of
        /// the image.
        /// </param>
        public static void TextureStorage3DMultisample(UInt32 texture, Int32 samples, SizedInternalFormat internalFormat, Int32 width, Int32 height, Int32 depth, Boolean fixedsamplelocations)
        {
            Delegates.glTextureStorage3DMultisample(texture, samples, internalFormat, width, height, depth, fixedsamplelocations);
        }

        /// <summary>
        /// Specify a one-dimensional texture subimage.
        /// <para>
        /// Texturing maps a portion of a specified texture image onto each graphical primitive for which
        /// texturing is enabled. To enable or disable one-dimensional texturing, call glEnable and glDisable
        /// with argument GL_TEXTURE_1D.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexSubImage1D. Must be GL_TEXTURE_1D.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_DEPTH_COMPONENT, and GL_STENCIL_INDEX.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="pixels">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TexSubImage1D(TextureTarget target, Int32 level, Int32 xoffset, Int32 width, PixelFormat format, PixelType type, IntPtr pixels)
        {
            Delegates.glTexSubImage1D(target, level, xoffset, width, format, type, pixels);
        }

        /// <summary>
        /// Specify a one-dimensional texture subimage.
        /// <para>
        /// Texturing maps a portion of a specified texture image onto each graphical primitive for which
        /// texturing is enabled. To enable or disable one-dimensional texturing, call glEnable and glDisable
        /// with argument GL_TEXTURE_1D.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureSubImage1D. The effective target of texture must be
        /// one of the valid target values above.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_DEPTH_COMPONENT, and GL_STENCIL_INDEX.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="pixels">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TextureSubImage1D(UInt32 texture, Int32 level, Int32 xoffset, Int32 width, PixelFormat format, PixelType type, IntPtr pixels)
        {
            Delegates.glTextureSubImage1D(texture, level, xoffset, width, format, type, pixels);
        }

        /// <summary>
        /// Specify a two-dimensional texture subimage.
        /// <para>
        /// Texturing maps a portion of a specified texture image onto each graphical primitive for which
        /// texturing is enabled.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexSubImage2D. Must be GL_TEXTURE_2D,
        /// GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
        /// GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, or
        /// GL_TEXTURE_1D_ARRAY.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_BGRA, GL_DEPTH_COMPONENT, and GL_STENCIL_INDEX.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="pixels">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TexSubImage2D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelFormat format, PixelType type, IntPtr pixels)
        {
            Delegates.glTexSubImage2D(target, level, xoffset, yoffset, width, height, format, type, pixels);
        }

        /// <summary>
        /// Specify a two-dimensional texture subimage.
        /// <para>
        /// Texturing maps a portion of a specified texture image onto each graphical primitive for which
        /// texturing is enabled.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureSubImage2D. The effective target of texture must be
        /// one of the valid target values above.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_BGRA, GL_DEPTH_COMPONENT, and GL_STENCIL_INDEX.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="pixels">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TextureSubImage2D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelFormat format, PixelType type, IntPtr pixels)
        {
            Delegates.glTextureSubImage2D(texture, level, xoffset, yoffset, width, height, format, type, pixels);
        }

        /// <summary>
        /// Specify a three-dimensional texture subimage.
        /// <para>
        /// Texturing maps a portion of a specified texture image onto each graphical primitive for which
        /// texturing is enabled.
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the texture is bound for glTexSubImage3D. Must be GL_TEXTURE_3D or
        /// GL_TEXTURE_2D_ARRAY.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="zoffset">
        /// Specifies a texel offset in the z direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_DEPTH_COMPONENT, and GL_STENCIL_INDEX.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="pixels">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TexSubImage3D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelFormat format, PixelType type, IntPtr pixels)
        {
            Delegates.glTexSubImage3D(target, level, xoffset, yoffset, zoffset, width, height, depth, format, type, pixels);
        }

        /// <summary>
        /// Specify a three-dimensional texture subimage.
        /// <para>
        /// Texturing maps a portion of a specified texture image onto each graphical primitive for which
        /// texturing is enabled.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object name for glTextureSubImage3D. The effective target of texture must be
        /// one of the valid target values above.
        /// </param>
        /// <param name="level">
        /// Specifies the level-of-detail number. Level 0 is the base image level. Level n is the nth mipmap
        /// reduction image.
        /// </param>
        /// <param name="xoffset">
        /// Specifies a texel offset in the x direction within the texture array.
        /// </param>
        /// <param name="yoffset">
        /// Specifies a texel offset in the y direction within the texture array.
        /// </param>
        /// <param name="zoffset">
        /// Specifies a texel offset in the z direction within the texture array.
        /// </param>
        /// <param name="width">
        /// Specifies the width of the texture subimage.
        /// </param>
        /// <param name="height">
        /// Specifies the height of the texture subimage.
        /// </param>
        /// <param name="depth">
        /// Specifies the depth of the texture subimage.
        /// </param>
        /// <param name="format">
        /// Specifies the format of the pixel data. The following symbolic values are accepted: GL_RED, GL_RG,
        /// GL_RGB, GL_BGR, GL_RGBA, GL_DEPTH_COMPONENT, and GL_STENCIL_INDEX.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of the pixel data. The following symbolic values are accepted:
        /// GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
        /// GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5,
        /// GL_UNSIGNED_SHORT_5_6_5_REV, GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV,
        /// GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8,
        /// GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV.
        /// </param>
        /// <param name="pixels">
        /// Specifies a pointer to the image data in memory.
        /// </param>
        public static void TextureSubImage3D(UInt32 texture, Int32 level, Int32 xoffset, Int32 yoffset, Int32 zoffset, Int32 width, Int32 height, Int32 depth, PixelFormat format, PixelType type, IntPtr pixels)
        {
            Delegates.glTextureSubImage3D(texture, level, xoffset, yoffset, zoffset, width, height, depth, format, type, pixels);
        }

        /// <summary>
        /// Controls the ordering of reads and writes to rendered fragments across drawing commands.
        /// <para>
        /// The values of rendered fragments are undefined when a shader stage fetches texels and the same
        /// texels are written via fragment shader outputs, even if the reads and writes are not in the same
        /// drawing command. To safely read the result of a written texel via a texel fetch in a subsequent
        /// drawing command, call glTextureBarrier between the two drawing commands to guarantee that writes
        /// have completed and caches have been invalidated before subsequent drawing commands are executed.
        /// </para>
        /// </summary>
        public static void TextureBarrier()
        {
            Delegates.glTextureBarrier();
        }

        /// <summary>
        /// Initialize a texture as a data alias of another texture's data store.
        /// <para>
        /// glTextureView initializes a texture object as an alias, or view of another texture object, sharing
        /// some or all of the parent texture's data store with the initialized texture. texture specifies a
        /// name previously reserved by a successful call to glGenTextures but that has not yet been bound or
        /// given a target. target specifies the target for the newly initialized texture and must be compatible
        /// with the target of the parent texture, given in origtexture as specified in the following table:.
        /// </para>
        /// </summary>
        /// <param name="texture">
        /// Specifies the texture object to be initialized as a view.
        /// </param>
        /// <param name="target">
        /// Specifies the target to be used for the newly initialized texture.
        /// </param>
        /// <param name="origtexture">
        /// Specifies the name of a texture object of which to make a view.
        /// </param>
        /// <param name="internalFormat">
        /// Specifies the internal format for the newly created view.
        /// </param>
        /// <param name="minlevel">
        /// Specifies lowest level of detail of the view.
        /// </param>
        /// <param name="numlevels">
        /// Specifies the number of levels of detail to include in the view.
        /// </param>
        /// <param name="minlayer">
        /// Specifies the index of the first layer to include in the view.
        /// </param>
        /// <param name="numlayers">
        /// Specifies the number of layers to include in the view.
        /// </param>
        public static void TextureView(UInt32 texture, TextureTarget target, UInt32 origtexture, PixelInternalFormat internalFormat, UInt32 minlevel, UInt32 numlevels, UInt32 minlayer, UInt32 numlayers)
        {
            Delegates.glTextureView(texture, target, origtexture, internalFormat, minlevel, numlevels, minlayer, numlayers);
        }

        /// <summary>
        /// Bind a buffer object to a transform feedback buffer object.
        /// <para>
        /// glTransformFeedbackBufferBase binds the buffer object buffer to the binding point at index index of
        /// the transform feedback object xfb.
        /// </para>
        /// </summary>
        /// <param name="xfb">
        /// Name of the transform feedback buffer object.
        /// </param>
        /// <param name="index">
        /// Index of the binding point within xfb.
        /// </param>
        /// <param name="buffer">
        /// Name of the buffer object to bind to the specified binding point.
        /// </param>
        public static void TransformFeedbackBufferBase(UInt32 xfb, UInt32 index, UInt32 buffer)
        {
            Delegates.glTransformFeedbackBufferBase(xfb, index, buffer);
        }

        /// <summary>
        /// Bind a range within a buffer object to a transform feedback buffer object.
        /// <para>
        /// glTransformFeedbackBufferRange binds a range of the buffer object buffer represented by offset and
        /// size to the binding point at index index of the transform feedback object xfb.
        /// </para>
        /// </summary>
        /// <param name="xfb">
        /// Name of the transform feedback buffer object.
        /// </param>
        /// <param name="index">
        /// Index of the binding point within xfb.
        /// </param>
        /// <param name="buffer">
        /// Name of the buffer object to bind to the specified binding point.
        /// </param>
        /// <param name="offset">
        /// The starting offset in basic machine units into the buffer object.
        /// </param>
        /// <param name="size">
        /// The amount of data in basic machine units that can be read from or written to the buffer object
        /// while used as an indexed target.
        /// </param>
        public static void TransformFeedbackBufferRange(UInt32 xfb, UInt32 index, UInt32 buffer, IntPtr offset, Int32 size)
        {
            Delegates.glTransformFeedbackBufferRange(xfb, index, buffer, offset, size);
        }

        /// <summary>
        /// Specify values to record in transform feedback buffers.
        /// <para>
        /// The names of the vertex or geometry shader outputs to be recorded in transform feedback mode are
        /// specified using glTransformFeedbackVaryings. When a geometry shader is active, transform feedback
        /// records the values of selected geometry shader output variables from the emitted vertices.
        /// Otherwise, the values of the selected vertex shader outputs are recorded.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of the target program object.
        /// </param>
        /// <param name="count">
        /// The number of varying variables used for transform feedback.
        /// </param>
        /// <param name="varyings">
        /// An array of count zero-terminated strings specifying the names of the varying variables to use for
        /// transform feedback.
        /// </param>
        /// <param name="bufferMode">
        /// Identifies the mode used to capture the varying variables when transform feedback is active.
        /// bufferMode must be GL_INTERLEAVED_ATTRIBS or GL_SEPARATE_ATTRIBS.
        /// </param>
        public static void TransformFeedbackVaryings(UInt32 program, Int32 count, String[] varyings, TransformFeedbackMode bufferMode)
        {
            Delegates.glTransformFeedbackVaryings(program, count, varyings, bufferMode);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform1f(Int32 location, Single v0)
        {
            Delegates.glUniform1f(location, v0);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform2f(Int32 location, Single v0, Single v1)
        {
            Delegates.glUniform2f(location, v0, v1);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform3f(Int32 location, Single v0, Single v1, Single v2)
        {
            Delegates.glUniform3f(location, v0, v1, v2);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform4f(Int32 location, Single v0, Single v1, Single v2, Single v3)
        {
            Delegates.glUniform4f(location, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform1i(Int32 location, Int32 v0)
        {
            Delegates.glUniform1i(location, v0);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform2i(Int32 location, Int32 v0, Int32 v1)
        {
            Delegates.glUniform2i(location, v0, v1);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform3i(Int32 location, Int32 v0, Int32 v1, Int32 v2)
        {
            Delegates.glUniform3i(location, v0, v1, v2);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform4i(Int32 location, Int32 v0, Int32 v1, Int32 v2, Int32 v3)
        {
            Delegates.glUniform4i(location, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform1ui(Int32 location, UInt32 v0)
        {
            Delegates.glUniform1ui(location, v0);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform2ui(Int32 location, UInt32 v0, UInt32 v1)
        {
            Delegates.glUniform2ui(location, v0, v1);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform3ui(Int32 location, UInt32 v0, UInt32 v1, UInt32 v2)
        {
            Delegates.glUniform3ui(location, v0, v1, v2);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified uniform variable.
        /// </param>
        public static void Uniform4ui(Int32 location, UInt32 v0, UInt32 v1, UInt32 v2, UInt32 v3)
        {
            Delegates.glUniform4ui(location, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform1fv(Int32 location, Int32 count, Single[] value)
        {
            Delegates.glUniform1fv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform2fv(Int32 location, Int32 count, Single[] value)
        {
            Delegates.glUniform2fv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform3fv(Int32 location, Int32 count, Single[] value)
        {
            Delegates.glUniform3fv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform4fv(Int32 location, Int32 count, Single[] value)
        {
            Delegates.glUniform4fv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform1iv(Int32 location, Int32 count, Int32[] value)
        {
            Delegates.glUniform1iv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform2iv(Int32 location, Int32 count, Int32[] value)
        {
            Delegates.glUniform2iv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform3iv(Int32 location, Int32 count, Int32[] value)
        {
            Delegates.glUniform3iv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform4iv(Int32 location, Int32 count, Int32[] value)
        {
            Delegates.glUniform4iv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform1uiv(Int32 location, Int32 count, UInt32[] value)
        {
            Delegates.glUniform1uiv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform2uiv(Int32 location, Int32 count, UInt32[] value)
        {
            Delegates.glUniform2uiv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform3uiv(Int32 location, Int32 count, UInt32[] value)
        {
            Delegates.glUniform3uiv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void Uniform4uiv(Int32 location, Int32 count, UInt32[] value)
        {
            Delegates.glUniform4uiv(location, count, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix2fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix2fv(location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix3fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix3fv(location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix4fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix4fv(location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix2x3fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix2x3fv(location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix3x2fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix3x2fv(location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix2x4fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix2x4fv(location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix4x2fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix4x2fv(location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix3x4fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix3x4fv(location, count, transpose, value);
        }

        /// <summary>
        /// Specify the value of a uniform variable for the current program object.
        /// <para>
        /// glUniform modifies the value of a uniform variable or a uniform variable array. The location of the
        /// uniform variable to be modified is specified by location, which should be a value returned by
        /// glGetUniformLocation. glUniform operates on the program object that was made part of current state
        /// by calling glUseProgram.
        /// </para>
        /// </summary>
        /// <param name="location">
        /// Specifies the location of the uniform variable to be modified.
        /// </param>
        /// <param name="count">
        /// For the vector (glUniform*v) commands, specifies the number of elements that are to be modified.
        /// This should be 1 if the targeted uniform variable is not an array, and 1 or more if it is an array.
        /// For the matrix (glUniformMatrix*) commands, specifies the number of matrices that are to be
        /// modified. This should be 1 if the targeted uniform variable is not an array of matrices, and 1 or
        /// more if it is an array of matrices.
        /// </param>
        /// <param name="transpose">
        /// For the matrix commands, specifies whether to transpose the matrix as the values are loaded into the
        /// uniform variable.
        /// </param>
        /// <param name="value">
        /// For the vector and matrix commands, specifies a pointer to an array of count values that will be
        /// used to update the specified uniform variable.
        /// </param>
        public static void UniformMatrix4x3fv(Int32 location, Int32 count, Boolean transpose, Single[] value)
        {
            Delegates.glUniformMatrix4x3fv(location, count, transpose, value);
        }

        /// <summary>
        /// Assign a binding point to an active uniform block.
        /// <para>
        /// Binding points for active uniform blocks are assigned using glUniformBlockBinding. Each of a
        /// program's active uniform blocks has a corresponding uniform buffer binding point. program is the
        /// name of a program object for which the command glLinkProgram has been issued in the past.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// The name of a program object containing the active uniform block whose binding to assign.
        /// </param>
        /// <param name="uniformBlockIndex">
        /// The index of the active uniform block within program whose binding to assign.
        /// </param>
        /// <param name="uniformBlockBinding">
        /// Specifies the binding point to which to bind the uniform block with index uniformBlockIndex within
        /// program.
        /// </param>
        public static void UniformBlockBinding(UInt32 program, UInt32 uniformBlockIndex, UInt32 uniformBlockBinding)
        {
            Delegates.glUniformBlockBinding(program, uniformBlockIndex, uniformBlockBinding);
        }

        /// <summary>
        /// Load active subroutine uniforms.
        /// <para>
        /// glUniformSubroutines loads all active subroutine uniforms for shader stage shadertype of the current
        /// program with subroutine indices from indices, storing indices[i] into the uniform at location i.
        /// count must be equal to the value of GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS for the program currently
        /// in use at shader stage shadertype. Furthermore, all values in indices must be less than the value of
        /// GL_ACTIVE_SUBROUTINES for the shader stage.
        /// </para>
        /// </summary>
        /// <param name="shadertype">
        /// Specifies the shader stage from which to query for subroutine uniform index. shadertype must be one
        /// of GL_VERTEX_SHADER, GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER or
        /// GL_FRAGMENT_SHADER.
        /// </param>
        /// <param name="count">
        /// Specifies the number of uniform indices stored in indices.
        /// </param>
        /// <param name="indices">
        /// Specifies the address of an array holding the indices to load into the shader subroutine variables.
        /// </param>
        public static void UniformSubroutinesuiv(ShaderType shadertype, Int32 count, UInt32[] indices)
        {
            Delegates.glUniformSubroutinesuiv(shadertype, count, indices);
        }

        /// <summary>
        /// Release the mapping of a buffer object's data store into the client's address space.
        /// <para>
        /// glUnmapBuffer and glUnmapNamedBuffer unmap (release) any mapping of a specified buffer object into
        /// the client's address space (see glMapBufferRange and glMapBuffer).
        /// </para>
        /// </summary>
        /// <param name="target">
        /// Specifies the target to which the buffer object is bound for glUnmapBuffer, which must be one of the
        /// buffer binding targets in the following table: Buffer Binding Target Purpose GL_ARRAY_BUFFER Vertex
        /// attributes GL_ATOMIC_COUNTER_BUFFER Atomic counter storage GL_COPY_READ_BUFFER Buffer copy source
        /// GL_COPY_WRITE_BUFFER Buffer copy destination GL_DISPATCH_INDIRECT_BUFFER Indirect compute dispatch
        /// commands GL_DRAW_INDIRECT_BUFFER Indirect command arguments GL_ELEMENT_ARRAY_BUFFER Vertex array
        /// indices GL_PIXEL_PACK_BUFFER Pixel read target GL_PIXEL_UNPACK_BUFFER Texture data source
        /// GL_QUERY_BUFFER Query result buffer GL_SHADER_STORAGE_BUFFER Read-write storage for shaders
        /// GL_TEXTURE_BUFFER Texture data buffer GL_TRANSFORM_FEEDBACK_BUFFER Transform feedback buffer
        /// GL_UNIFORM_BUFFER Uniform block storage
        /// </param>
        public static Boolean UnmapBuffer(BufferTarget target)
        {
            return Delegates.glUnmapBuffer(target);
        }

        /// <summary>
        /// Release the mapping of a buffer object's data store into the client's address space.
        /// <para>
        /// glUnmapBuffer and glUnmapNamedBuffer unmap (release) any mapping of a specified buffer object into
        /// the client's address space (see glMapBufferRange and glMapBuffer).
        /// </para>
        /// </summary>
        /// <param name="buffer">
        /// Specifies the name of the buffer object for glUnmapNamedBuffer.
        /// </param>
        public static Boolean UnmapNamedBuffer(UInt32 buffer)
        {
            return Delegates.glUnmapNamedBuffer(buffer);
        }

        /// <summary>
        /// Installs a program object as part of current rendering state.
        /// <para>
        /// glUseProgram installs the program object specified by program as part of current rendering state.
        /// One or more executables are created in a program object by successfully attaching shader objects to
        /// it with glAttachShader, successfully compiling the shader objects with glCompileShader, and
        /// successfully linking the program object with glLinkProgram.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program object whose executables are to be used as part of current
        /// rendering state.
        /// </param>
        public static void UseProgram(UInt32 program)
        {
            Gl.currentProgram = program;
            Delegates.glUseProgram(program);
        }

        /// <summary>
        /// Bind stages of a program object to a program pipeline.
        /// <para>
        /// glUseProgramStages binds executables from a program object associated with a specified set of shader
        /// stages to the program pipeline object given by pipeline. pipeline specifies the program pipeline
        /// object to which to bind the executables. stages contains a logical combination of bits indicating
        /// the shader stages to use within program with the program pipeline object pipeline. stages must be a
        /// logical combination of GL_VERTEX_SHADER_BIT, GL_TESS_CONTROL_SHADER_BIT,
        /// GL_TESS_EVALUATION_SHADER_BIT, GL_GEOMETRY_SHADER_BIT, GL_FRAGMENT_SHADER_BIT and
        /// GL_COMPUTE_SHADER_BIT. Additionally, the special value GL_ALL_SHADER_BITS may be specified to
        /// indicate that all executables contained in program should be installed in pipeline.
        /// </para>
        /// </summary>
        /// <param name="pipeline">
        /// Specifies the program pipeline object to which to bind stages from program.
        /// </param>
        /// <param name="stages">
        /// Specifies a set of program stages to bind to the program pipeline object.
        /// </param>
        /// <param name="program">
        /// Specifies the program object containing the shader executables to use in pipeline.
        /// </param>
        public static void UseProgramStages(UInt32 pipeline, UInt32 stages, UInt32 program)
        {
            Gl.currentProgram = program;
            Delegates.glUseProgramStages(pipeline, stages, program);
        }

        /// <summary>
        /// Validates a program object.
        /// <para>
        /// glValidateProgram checks to see whether the executables contained in program can execute given the
        /// current OpenGL state. The information generated by the validation process will be stored in
        /// program's information log. The validation information may consist of an empty string, or it may be a
        /// string containing information about how the current program object interacts with the rest of
        /// current OpenGL state. This provides a way for OpenGL implementers to convey more information about
        /// why the current program is inefficient, suboptimal, failing to execute, and so on.
        /// </para>
        /// </summary>
        /// <param name="program">
        /// Specifies the handle of the program object to be validated.
        /// </param>
        public static void ValidateProgram(UInt32 program)
        {
            Delegates.glValidateProgram(program);
        }

        /// <summary>
        /// Validate a program pipeline object against current GL state.
        /// <para>
        /// glValidateProgramPipeline instructs the implementation to validate the shader executables contained
        /// in pipeline against the current GL state. The implementation may use this as an opportunity to
        /// perform any internal shader modifications that may be required to ensure correct operation of the
        /// installed shaders given the current GL state.
        /// </para>
        /// </summary>
        /// <param name="pipeline">
        /// Specifies the name of a program pipeline object to validate.
        /// </param>
        public static void ValidateProgramPipeline(UInt32 pipeline)
        {
            Delegates.glValidateProgramPipeline(pipeline);
        }

        /// <summary>
        /// Configures element array buffer binding of a vertex array object.
        /// <para>
        /// glVertexArrayElementBuffer binds a buffer object with id buffer to the element array buffer bind
        /// point of a vertex array object with id vaobj. If buffer is zero, any existing element array buffer
        /// binding to vaobj is removed.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object.
        /// </param>
        /// <param name="buffer">
        /// Specifies the name of the buffer object to use for the element array buffer binding.
        /// </param>
        public static void VertexArrayElementBuffer(UInt32 vaobj, UInt32 buffer)
        {
            Delegates.glVertexArrayElementBuffer(vaobj, buffer);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib1f(UInt32 index, Single v0)
        {
            Delegates.glVertexAttrib1f(index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib1f(Int32 index, Single v0)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib1f((UInt32)index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib1s(UInt32 index, Int16 v0)
        {
            Delegates.glVertexAttrib1s(index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib1s(Int32 index, Int16 v0)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib1s((UInt32)index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib1d(UInt32 index, Double v0)
        {
            Delegates.glVertexAttrib1d(index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib1d(Int32 index, Double v0)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib1d((UInt32)index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI1i(UInt32 index, Int32 v0)
        {
            Delegates.glVertexAttribI1i(index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI1i(Int32 index, Int32 v0)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI1i((UInt32)index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI1ui(UInt32 index, UInt32 v0)
        {
            Delegates.glVertexAttribI1ui(index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI1ui(Int32 index, UInt32 v0)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI1ui((UInt32)index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib2f(UInt32 index, Single v0, Single v1)
        {
            Delegates.glVertexAttrib2f(index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib2f(Int32 index, Single v0, Single v1)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib2f((UInt32)index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib2s(UInt32 index, Int16 v0, Int16 v1)
        {
            Delegates.glVertexAttrib2s(index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib2s(Int32 index, Int16 v0, Int16 v1)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib2s((UInt32)index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib2d(UInt32 index, Double v0, Double v1)
        {
            Delegates.glVertexAttrib2d(index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib2d(Int32 index, Double v0, Double v1)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib2d((UInt32)index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI2i(UInt32 index, Int32 v0, Int32 v1)
        {
            Delegates.glVertexAttribI2i(index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI2i(Int32 index, Int32 v0, Int32 v1)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI2i((UInt32)index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI2ui(UInt32 index, UInt32 v0, UInt32 v1)
        {
            Delegates.glVertexAttribI2ui(index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI2ui(Int32 index, UInt32 v0, UInt32 v1)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI2ui((UInt32)index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib3f(UInt32 index, Single v0, Single v1, Single v2)
        {
            Delegates.glVertexAttrib3f(index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib3f(Int32 index, Single v0, Single v1, Single v2)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib3f((UInt32)index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib3s(UInt32 index, Int16 v0, Int16 v1, Int16 v2)
        {
            Delegates.glVertexAttrib3s(index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib3s(Int32 index, Int16 v0, Int16 v1, Int16 v2)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib3s((UInt32)index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib3d(UInt32 index, Double v0, Double v1, Double v2)
        {
            Delegates.glVertexAttrib3d(index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib3d(Int32 index, Double v0, Double v1, Double v2)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib3d((UInt32)index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI3i(UInt32 index, Int32 v0, Int32 v1, Int32 v2)
        {
            Delegates.glVertexAttribI3i(index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI3i(Int32 index, Int32 v0, Int32 v1, Int32 v2)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI3i((UInt32)index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI3ui(UInt32 index, UInt32 v0, UInt32 v1, UInt32 v2)
        {
            Delegates.glVertexAttribI3ui(index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI3ui(Int32 index, UInt32 v0, UInt32 v1, UInt32 v2)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI3ui((UInt32)index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib4f(UInt32 index, Single v0, Single v1, Single v2, Single v3)
        {
            Delegates.glVertexAttrib4f(index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib4f(Int32 index, Single v0, Single v1, Single v2, Single v3)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4f((UInt32)index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib4s(UInt32 index, Int16 v0, Int16 v1, Int16 v2, Int16 v3)
        {
            Delegates.glVertexAttrib4s(index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib4s(Int32 index, Int16 v0, Int16 v1, Int16 v2, Int16 v3)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4s((UInt32)index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib4d(UInt32 index, Double v0, Double v1, Double v2, Double v3)
        {
            Delegates.glVertexAttrib4d(index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib4d(Int32 index, Double v0, Double v1, Double v2, Double v3)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4d((UInt32)index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib4Nub(UInt32 index, Byte v0, Byte v1, Byte v2, Byte v3)
        {
            Delegates.glVertexAttrib4Nub(index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttrib4Nub(Int32 index, Byte v0, Byte v1, Byte v2, Byte v3)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4Nub((UInt32)index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI4i(UInt32 index, Int32 v0, Int32 v1, Int32 v2, Int32 v3)
        {
            Delegates.glVertexAttribI4i(index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI4i(Int32 index, Int32 v0, Int32 v1, Int32 v2, Int32 v3)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI4i((UInt32)index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI4ui(UInt32 index, UInt32 v0, UInt32 v1, UInt32 v2, UInt32 v3)
        {
            Delegates.glVertexAttribI4ui(index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribI4ui(Int32 index, UInt32 v0, UInt32 v1, UInt32 v2, UInt32 v3)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI4ui((UInt32)index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribL1d(UInt32 index, Double v0)
        {
            Delegates.glVertexAttribL1d(index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribL1d(Int32 index, Double v0)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribL1d((UInt32)index, v0);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribL2d(UInt32 index, Double v0, Double v1)
        {
            Delegates.glVertexAttribL2d(index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribL2d(Int32 index, Double v0, Double v1)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribL2d((UInt32)index, v0, v1);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribL3d(UInt32 index, Double v0, Double v1, Double v2)
        {
            Delegates.glVertexAttribL3d(index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribL3d(Int32 index, Double v0, Double v1, Double v2)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribL3d((UInt32)index, v0, v1, v2);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribL4d(UInt32 index, Double v0, Double v1, Double v2, Double v3)
        {
            Delegates.glVertexAttribL4d(index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v0">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v1">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v2">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        /// <param name="v3">
        /// For the scalar commands, specifies the new values to be used for the specified vertex attribute.
        /// </param>
        public static void VertexAttribL4d(Int32 index, Double v0, Double v1, Double v2, Double v3)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribL4d((UInt32)index, v0, v1, v2, v3);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib1fv(UInt32 index, Single[] v)
        {
            Delegates.glVertexAttrib1fv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib1fv(Int32 index, Single[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib1fv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib1sv(UInt32 index, Int16[] v)
        {
            Delegates.glVertexAttrib1sv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib1sv(Int32 index, Int16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib1sv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib1dv(UInt32 index, Double[] v)
        {
            Delegates.glVertexAttrib1dv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib1dv(Int32 index, Double[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib1dv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI1iv(UInt32 index, Int32[] v)
        {
            Delegates.glVertexAttribI1iv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI1iv(Int32 index, Int32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI1iv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI1uiv(UInt32 index, UInt32[] v)
        {
            Delegates.glVertexAttribI1uiv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI1uiv(Int32 index, UInt32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI1uiv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib2fv(UInt32 index, Single[] v)
        {
            Delegates.glVertexAttrib2fv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib2fv(Int32 index, Single[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib2fv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib2sv(UInt32 index, Int16[] v)
        {
            Delegates.glVertexAttrib2sv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib2sv(Int32 index, Int16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib2sv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib2dv(UInt32 index, Double[] v)
        {
            Delegates.glVertexAttrib2dv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib2dv(Int32 index, Double[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib2dv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI2iv(UInt32 index, Int32[] v)
        {
            Delegates.glVertexAttribI2iv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI2iv(Int32 index, Int32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI2iv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI2uiv(UInt32 index, UInt32[] v)
        {
            Delegates.glVertexAttribI2uiv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI2uiv(Int32 index, UInt32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI2uiv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib3fv(UInt32 index, Single[] v)
        {
            Delegates.glVertexAttrib3fv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib3fv(Int32 index, Single[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib3fv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib3sv(UInt32 index, Int16[] v)
        {
            Delegates.glVertexAttrib3sv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib3sv(Int32 index, Int16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib3sv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib3dv(UInt32 index, Double[] v)
        {
            Delegates.glVertexAttrib3dv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib3dv(Int32 index, Double[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib3dv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI3iv(UInt32 index, Int32[] v)
        {
            Delegates.glVertexAttribI3iv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI3iv(Int32 index, Int32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI3iv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI3uiv(UInt32 index, UInt32[] v)
        {
            Delegates.glVertexAttribI3uiv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI3uiv(Int32 index, UInt32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI3uiv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4fv(UInt32 index, Single[] v)
        {
            Delegates.glVertexAttrib4fv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4fv(Int32 index, Single[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4fv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4sv(UInt32 index, Int16[] v)
        {
            Delegates.glVertexAttrib4sv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4sv(Int32 index, Int16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4sv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4dv(UInt32 index, Double[] v)
        {
            Delegates.glVertexAttrib4dv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4dv(Int32 index, Double[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4dv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4iv(UInt32 index, Int32[] v)
        {
            Delegates.glVertexAttrib4iv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4iv(Int32 index, Int32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4iv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4bv(UInt32 index, SByte[] v)
        {
            Delegates.glVertexAttrib4bv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4bv(Int32 index, SByte[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4bv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4ubv(UInt32 index, Byte[] v)
        {
            Delegates.glVertexAttrib4ubv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4ubv(Int32 index, Byte[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4ubv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4usv(UInt32 index, UInt16[] v)
        {
            Delegates.glVertexAttrib4usv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4usv(Int32 index, UInt16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4usv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4uiv(UInt32 index, UInt32[] v)
        {
            Delegates.glVertexAttrib4uiv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4uiv(Int32 index, UInt32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4uiv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nbv(UInt32 index, SByte[] v)
        {
            Delegates.glVertexAttrib4Nbv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nbv(Int32 index, SByte[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4Nbv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nsv(UInt32 index, Int16[] v)
        {
            Delegates.glVertexAttrib4Nsv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nsv(Int32 index, Int16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4Nsv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Niv(UInt32 index, Int32[] v)
        {
            Delegates.glVertexAttrib4Niv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Niv(Int32 index, Int32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4Niv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nubv(UInt32 index, Byte[] v)
        {
            Delegates.glVertexAttrib4Nubv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nubv(Int32 index, Byte[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4Nubv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nusv(UInt32 index, UInt16[] v)
        {
            Delegates.glVertexAttrib4Nusv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nusv(Int32 index, UInt16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4Nusv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nuiv(UInt32 index, UInt32[] v)
        {
            Delegates.glVertexAttrib4Nuiv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttrib4Nuiv(Int32 index, UInt32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttrib4Nuiv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4bv(UInt32 index, SByte[] v)
        {
            Delegates.glVertexAttribI4bv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4bv(Int32 index, SByte[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI4bv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4ubv(UInt32 index, Byte[] v)
        {
            Delegates.glVertexAttribI4ubv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4ubv(Int32 index, Byte[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI4ubv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4sv(UInt32 index, Int16[] v)
        {
            Delegates.glVertexAttribI4sv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4sv(Int32 index, Int16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI4sv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4usv(UInt32 index, UInt16[] v)
        {
            Delegates.glVertexAttribI4usv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4usv(Int32 index, UInt16[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI4usv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4iv(UInt32 index, Int32[] v)
        {
            Delegates.glVertexAttribI4iv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4iv(Int32 index, Int32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI4iv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4uiv(UInt32 index, UInt32[] v)
        {
            Delegates.glVertexAttribI4uiv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribI4uiv(Int32 index, UInt32[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribI4uiv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribL1dv(UInt32 index, Double[] v)
        {
            Delegates.glVertexAttribL1dv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribL1dv(Int32 index, Double[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribL1dv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribL2dv(UInt32 index, Double[] v)
        {
            Delegates.glVertexAttribL2dv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribL2dv(Int32 index, Double[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribL2dv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribL3dv(UInt32 index, Double[] v)
        {
            Delegates.glVertexAttribL3dv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribL3dv(Int32 index, Double[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribL3dv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribL4dv(UInt32 index, Double[] v)
        {
            Delegates.glVertexAttribL4dv(index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="v">
        /// For the vector commands (glVertexAttrib*v), specifies a pointer to an array of values to be used for
        /// the generic vertex attribute.
        /// </param>
        public static void VertexAttribL4dv(Int32 index, Double[] v)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribL4dv((UInt32)index, v);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="type">
        /// For the packed commands (glVertexAttribP*), specified the type of packing used on the data. This
        /// parameter must be GL_INT_2_10_10_10_REV or GL_UNSIGNED_INT_2_10_10_10_REV, to specify signed or
        /// unsigned data, respectively, or GL_UNSIGNED_INT_10F_11F_11F_REV to specify floating point data.
        /// </param>
        /// <param name="normalized">
        /// For the packed commands, if GL_TRUE, then the values are to be converted to floating point values by
        /// normalizing. Otherwise, they are converted directly to floating-point values. If type indicates a
        /// floating-pont format, then normalized value must be GL_FALSE.
        /// </param>
        /// <param name="value">
        /// For the packed commands, specifies the new packed value to be used for the specified vertex
        /// attribute.
        /// </param>
        public static void VertexAttribP1ui(UInt32 index, VertexAttribPType type, Boolean normalized, UInt32 value)
        {
            Delegates.glVertexAttribP1ui(index, type, normalized, value);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="type">
        /// For the packed commands (glVertexAttribP*), specified the type of packing used on the data. This
        /// parameter must be GL_INT_2_10_10_10_REV or GL_UNSIGNED_INT_2_10_10_10_REV, to specify signed or
        /// unsigned data, respectively, or GL_UNSIGNED_INT_10F_11F_11F_REV to specify floating point data.
        /// </param>
        /// <param name="normalized">
        /// For the packed commands, if GL_TRUE, then the values are to be converted to floating point values by
        /// normalizing. Otherwise, they are converted directly to floating-point values. If type indicates a
        /// floating-pont format, then normalized value must be GL_FALSE.
        /// </param>
        /// <param name="value">
        /// For the packed commands, specifies the new packed value to be used for the specified vertex
        /// attribute.
        /// </param>
        public static void VertexAttribP1ui(Int32 index, VertexAttribPType type, Boolean normalized, UInt32 value)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribP1ui((UInt32)index, type, normalized, value);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="type">
        /// For the packed commands (glVertexAttribP*), specified the type of packing used on the data. This
        /// parameter must be GL_INT_2_10_10_10_REV or GL_UNSIGNED_INT_2_10_10_10_REV, to specify signed or
        /// unsigned data, respectively, or GL_UNSIGNED_INT_10F_11F_11F_REV to specify floating point data.
        /// </param>
        /// <param name="normalized">
        /// For the packed commands, if GL_TRUE, then the values are to be converted to floating point values by
        /// normalizing. Otherwise, they are converted directly to floating-point values. If type indicates a
        /// floating-pont format, then normalized value must be GL_FALSE.
        /// </param>
        /// <param name="value">
        /// For the packed commands, specifies the new packed value to be used for the specified vertex
        /// attribute.
        /// </param>
        public static void VertexAttribP2ui(UInt32 index, VertexAttribPType type, Boolean normalized, UInt32 value)
        {
            Delegates.glVertexAttribP2ui(index, type, normalized, value);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="type">
        /// For the packed commands (glVertexAttribP*), specified the type of packing used on the data. This
        /// parameter must be GL_INT_2_10_10_10_REV or GL_UNSIGNED_INT_2_10_10_10_REV, to specify signed or
        /// unsigned data, respectively, or GL_UNSIGNED_INT_10F_11F_11F_REV to specify floating point data.
        /// </param>
        /// <param name="normalized">
        /// For the packed commands, if GL_TRUE, then the values are to be converted to floating point values by
        /// normalizing. Otherwise, they are converted directly to floating-point values. If type indicates a
        /// floating-pont format, then normalized value must be GL_FALSE.
        /// </param>
        /// <param name="value">
        /// For the packed commands, specifies the new packed value to be used for the specified vertex
        /// attribute.
        /// </param>
        public static void VertexAttribP2ui(Int32 index, VertexAttribPType type, Boolean normalized, UInt32 value)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribP2ui((UInt32)index, type, normalized, value);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="type">
        /// For the packed commands (glVertexAttribP*), specified the type of packing used on the data. This
        /// parameter must be GL_INT_2_10_10_10_REV or GL_UNSIGNED_INT_2_10_10_10_REV, to specify signed or
        /// unsigned data, respectively, or GL_UNSIGNED_INT_10F_11F_11F_REV to specify floating point data.
        /// </param>
        /// <param name="normalized">
        /// For the packed commands, if GL_TRUE, then the values are to be converted to floating point values by
        /// normalizing. Otherwise, they are converted directly to floating-point values. If type indicates a
        /// floating-pont format, then normalized value must be GL_FALSE.
        /// </param>
        /// <param name="value">
        /// For the packed commands, specifies the new packed value to be used for the specified vertex
        /// attribute.
        /// </param>
        public static void VertexAttribP3ui(UInt32 index, VertexAttribPType type, Boolean normalized, UInt32 value)
        {
            Delegates.glVertexAttribP3ui(index, type, normalized, value);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="type">
        /// For the packed commands (glVertexAttribP*), specified the type of packing used on the data. This
        /// parameter must be GL_INT_2_10_10_10_REV or GL_UNSIGNED_INT_2_10_10_10_REV, to specify signed or
        /// unsigned data, respectively, or GL_UNSIGNED_INT_10F_11F_11F_REV to specify floating point data.
        /// </param>
        /// <param name="normalized">
        /// For the packed commands, if GL_TRUE, then the values are to be converted to floating point values by
        /// normalizing. Otherwise, they are converted directly to floating-point values. If type indicates a
        /// floating-pont format, then normalized value must be GL_FALSE.
        /// </param>
        /// <param name="value">
        /// For the packed commands, specifies the new packed value to be used for the specified vertex
        /// attribute.
        /// </param>
        public static void VertexAttribP3ui(Int32 index, VertexAttribPType type, Boolean normalized, UInt32 value)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribP3ui((UInt32)index, type, normalized, value);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="type">
        /// For the packed commands (glVertexAttribP*), specified the type of packing used on the data. This
        /// parameter must be GL_INT_2_10_10_10_REV or GL_UNSIGNED_INT_2_10_10_10_REV, to specify signed or
        /// unsigned data, respectively, or GL_UNSIGNED_INT_10F_11F_11F_REV to specify floating point data.
        /// </param>
        /// <param name="normalized">
        /// For the packed commands, if GL_TRUE, then the values are to be converted to floating point values by
        /// normalizing. Otherwise, they are converted directly to floating-point values. If type indicates a
        /// floating-pont format, then normalized value must be GL_FALSE.
        /// </param>
        /// <param name="value">
        /// For the packed commands, specifies the new packed value to be used for the specified vertex
        /// attribute.
        /// </param>
        public static void VertexAttribP4ui(UInt32 index, VertexAttribPType type, Boolean normalized, UInt32 value)
        {
            Delegates.glVertexAttribP4ui(index, type, normalized, value);
        }

        /// <summary>
        /// Specifies the value of a generic vertex attribute.
        /// <para>
        /// The glVertexAttrib family of entry points allows an application to pass generic vertex attributes in
        /// numbered locations.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="type">
        /// For the packed commands (glVertexAttribP*), specified the type of packing used on the data. This
        /// parameter must be GL_INT_2_10_10_10_REV or GL_UNSIGNED_INT_2_10_10_10_REV, to specify signed or
        /// unsigned data, respectively, or GL_UNSIGNED_INT_10F_11F_11F_REV to specify floating point data.
        /// </param>
        /// <param name="normalized">
        /// For the packed commands, if GL_TRUE, then the values are to be converted to floating point values by
        /// normalizing. Otherwise, they are converted directly to floating-point values. If type indicates a
        /// floating-pont format, then normalized value must be GL_FALSE.
        /// </param>
        /// <param name="value">
        /// For the packed commands, specifies the new packed value to be used for the specified vertex
        /// attribute.
        /// </param>
        public static void VertexAttribP4ui(Int32 index, VertexAttribPType type, Boolean normalized, UInt32 value)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribP4ui((UInt32)index, type, normalized, value);
        }

        /// <summary>
        /// Associate a vertex attribute and a vertex buffer binding for a vertex array object.
        /// <para>
        /// glVertexAttribBinding and glVertexArrayAttribBinding establishes an association between the generic
        /// vertex attribute of a vertex array object whose index is given by attribindex, and a vertex buffer
        /// binding whose index is given by bindingindex. For glVertexAttribBinding, the vertex array object
        /// affected is that currently bound. For glVertexArrayAttribBinding, vaobj is the name of the vertex
        /// array object.
        /// </para>
        /// </summary>
        /// <param name="attribindex">
        /// The index of the attribute to associate with a vertex buffer binding.
        /// </param>
        /// <param name="bindingindex">
        /// The index of the vertex buffer binding with which to associate the generic vertex attribute.
        /// </param>
        public static void VertexAttribBinding(UInt32 attribindex, UInt32 bindingindex)
        {
            Delegates.glVertexAttribBinding(attribindex, bindingindex);
        }

        /// <summary>
        /// Associate a vertex attribute and a vertex buffer binding for a vertex array object.
        /// <para>
        /// glVertexAttribBinding and glVertexArrayAttribBinding establishes an association between the generic
        /// vertex attribute of a vertex array object whose index is given by attribindex, and a vertex buffer
        /// binding whose index is given by bindingindex. For glVertexAttribBinding, the vertex array object
        /// affected is that currently bound. For glVertexArrayAttribBinding, vaobj is the name of the vertex
        /// array object.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glVertexArrayAttribBinding.
        /// </param>
        /// <param name="attribindex">
        /// The index of the attribute to associate with a vertex buffer binding.
        /// </param>
        /// <param name="bindingindex">
        /// The index of the vertex buffer binding with which to associate the generic vertex attribute.
        /// </param>
        public static void VertexArrayAttribBinding(UInt32 vaobj, UInt32 attribindex, UInt32 bindingindex)
        {
            Delegates.glVertexArrayAttribBinding(vaobj, attribindex, bindingindex);
        }

        /// <summary>
        /// Modify the rate at which generic vertex attributes advance during instanced rendering.
        /// <para>
        /// glVertexAttribDivisor modifies the rate at which generic vertex attributes advance when rendering
        /// multiple instances of primitives in a single draw call. If divisor is zero, the attribute at slot
        /// index advances once per vertex. If divisor is non-zero, the attribute advances once per divisor
        /// instances of the set(s) of vertices being rendered. An attribute is referred to as instanced if its
        /// GL_VERTEX_ATTRIB_ARRAY_DIVISOR value is non-zero.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specify the index of the generic vertex attribute.
        /// </param>
        /// <param name="divisor">
        /// Specify the number of instances that will pass between updates of the generic attribute at slot
        /// index.
        /// </param>
        public static void VertexAttribDivisor(UInt32 index, UInt32 divisor)
        {
            Delegates.glVertexAttribDivisor(index, divisor);
        }

        /// <summary>
        /// Modify the rate at which generic vertex attributes advance during instanced rendering.
        /// <para>
        /// glVertexAttribDivisor modifies the rate at which generic vertex attributes advance when rendering
        /// multiple instances of primitives in a single draw call. If divisor is zero, the attribute at slot
        /// index advances once per vertex. If divisor is non-zero, the attribute advances once per divisor
        /// instances of the set(s) of vertices being rendered. An attribute is referred to as instanced if its
        /// GL_VERTEX_ATTRIB_ARRAY_DIVISOR value is non-zero.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specify the index of the generic vertex attribute.
        /// </param>
        /// <param name="divisor">
        /// Specify the number of instances that will pass between updates of the generic attribute at slot
        /// index.
        /// </param>
        public static void VertexAttribDivisor(Int32 index, UInt32 divisor)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribDivisor((UInt32)index, divisor);
        }

        /// <summary>
        /// Specify the organization of vertex arrays.
        /// <para>
        /// glVertexAttribFormat, glVertexAttribIFormat and glVertexAttribLFormat, as well as
        /// glVertexArrayAttribFormat, glVertexArrayAttribIFormat and glVertexArrayAttribLFormat specify the
        /// organization of data in vertex arrays. The first three calls operate on the bound vertex array
        /// object, whereas the last three ones modify the state of a vertex array object with ID vaobj.
        /// attribindex specifies the index of the generic vertex attribute array whose data layout is being
        /// described, and must be less than the value of GL_MAX_VERTEX_ATTRIBS.
        /// </para>
        /// </summary>
        /// <param name="attribindex">
        /// The generic vertex attribute array being described.
        /// </param>
        /// <param name="size">
        /// The number of values per vertex that are stored in the array.
        /// </param>
        /// <param name="type">
        /// The type of the data stored in the array.
        /// </param>
        /// <param name="normalized">
        /// Specifies whether fixed-point data values should be normalized (GL_TRUE) or 		 converted directly as
        /// fixed-point values (GL_FALSE) when they are accessed. This parameter is ignored if type is GL_FIXED.
        /// </param>
        /// <param name="relativeoffset">
        /// The distance between elements within the buffer.
        /// </param>
        public static void VertexAttribFormat(UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, Boolean normalized, UInt32 relativeoffset)
        {
            Delegates.glVertexAttribFormat(attribindex, size, type, normalized, relativeoffset);
        }

        /// <summary>
        /// Specify the organization of vertex arrays.
        /// <para>
        /// glVertexAttribFormat, glVertexAttribIFormat and glVertexAttribLFormat, as well as
        /// glVertexArrayAttribFormat, glVertexArrayAttribIFormat and glVertexArrayAttribLFormat specify the
        /// organization of data in vertex arrays. The first three calls operate on the bound vertex array
        /// object, whereas the last three ones modify the state of a vertex array object with ID vaobj.
        /// attribindex specifies the index of the generic vertex attribute array whose data layout is being
        /// described, and must be less than the value of GL_MAX_VERTEX_ATTRIBS.
        /// </para>
        /// </summary>
        /// <param name="attribindex">
        /// The generic vertex attribute array being described.
        /// </param>
        /// <param name="size">
        /// The number of values per vertex that are stored in the array.
        /// </param>
        /// <param name="type">
        /// The type of the data stored in the array.
        /// </param>
        /// <param name="relativeoffset">
        /// The distance between elements within the buffer.
        /// </param>
        public static void VertexAttribIFormat(UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, UInt32 relativeoffset)
        {
            Delegates.glVertexAttribIFormat(attribindex, size, type, relativeoffset);
        }

        /// <summary>
        /// Specify the organization of vertex arrays.
        /// <para>
        /// glVertexAttribFormat, glVertexAttribIFormat and glVertexAttribLFormat, as well as
        /// glVertexArrayAttribFormat, glVertexArrayAttribIFormat and glVertexArrayAttribLFormat specify the
        /// organization of data in vertex arrays. The first three calls operate on the bound vertex array
        /// object, whereas the last three ones modify the state of a vertex array object with ID vaobj.
        /// attribindex specifies the index of the generic vertex attribute array whose data layout is being
        /// described, and must be less than the value of GL_MAX_VERTEX_ATTRIBS.
        /// </para>
        /// </summary>
        /// <param name="attribindex">
        /// The generic vertex attribute array being described.
        /// </param>
        /// <param name="size">
        /// The number of values per vertex that are stored in the array.
        /// </param>
        /// <param name="type">
        /// The type of the data stored in the array.
        /// </param>
        /// <param name="relativeoffset">
        /// The distance between elements within the buffer.
        /// </param>
        public static void VertexAttribLFormat(UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, UInt32 relativeoffset)
        {
            Delegates.glVertexAttribLFormat(attribindex, size, type, relativeoffset);
        }

        /// <summary>
        /// Specify the organization of vertex arrays.
        /// <para>
        /// glVertexAttribFormat, glVertexAttribIFormat and glVertexAttribLFormat, as well as
        /// glVertexArrayAttribFormat, glVertexArrayAttribIFormat and glVertexArrayAttribLFormat specify the
        /// organization of data in vertex arrays. The first three calls operate on the bound vertex array
        /// object, whereas the last three ones modify the state of a vertex array object with ID vaobj.
        /// attribindex specifies the index of the generic vertex attribute array whose data layout is being
        /// described, and must be less than the value of GL_MAX_VERTEX_ATTRIBS.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glVertexArrayAttrib{I, L}Format functions.
        /// </param>
        /// <param name="attribindex">
        /// The generic vertex attribute array being described.
        /// </param>
        /// <param name="size">
        /// The number of values per vertex that are stored in the array.
        /// </param>
        /// <param name="type">
        /// The type of the data stored in the array.
        /// </param>
        /// <param name="normalized">
        /// Specifies whether fixed-point data values should be normalized (GL_TRUE) or 		 converted directly as
        /// fixed-point values (GL_FALSE) when they are accessed. This parameter is ignored if type is GL_FIXED.
        /// </param>
        /// <param name="relativeoffset">
        /// The distance between elements within the buffer.
        /// </param>
        public static void VertexArrayAttribFormat(UInt32 vaobj, UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, Boolean normalized, UInt32 relativeoffset)
        {
            Delegates.glVertexArrayAttribFormat(vaobj, attribindex, size, type, normalized, relativeoffset);
        }

        /// <summary>
        /// Specify the organization of vertex arrays.
        /// <para>
        /// glVertexAttribFormat, glVertexAttribIFormat and glVertexAttribLFormat, as well as
        /// glVertexArrayAttribFormat, glVertexArrayAttribIFormat and glVertexArrayAttribLFormat specify the
        /// organization of data in vertex arrays. The first three calls operate on the bound vertex array
        /// object, whereas the last three ones modify the state of a vertex array object with ID vaobj.
        /// attribindex specifies the index of the generic vertex attribute array whose data layout is being
        /// described, and must be less than the value of GL_MAX_VERTEX_ATTRIBS.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glVertexArrayAttrib{I, L}Format functions.
        /// </param>
        /// <param name="attribindex">
        /// The generic vertex attribute array being described.
        /// </param>
        /// <param name="size">
        /// The number of values per vertex that are stored in the array.
        /// </param>
        /// <param name="type">
        /// The type of the data stored in the array.
        /// </param>
        /// <param name="relativeoffset">
        /// The distance between elements within the buffer.
        /// </param>
        public static void VertexArrayAttribIFormat(UInt32 vaobj, UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, UInt32 relativeoffset)
        {
            Delegates.glVertexArrayAttribIFormat(vaobj, attribindex, size, type, relativeoffset);
        }

        /// <summary>
        /// Specify the organization of vertex arrays.
        /// <para>
        /// glVertexAttribFormat, glVertexAttribIFormat and glVertexAttribLFormat, as well as
        /// glVertexArrayAttribFormat, glVertexArrayAttribIFormat and glVertexArrayAttribLFormat specify the
        /// organization of data in vertex arrays. The first three calls operate on the bound vertex array
        /// object, whereas the last three ones modify the state of a vertex array object with ID vaobj.
        /// attribindex specifies the index of the generic vertex attribute array whose data layout is being
        /// described, and must be less than the value of GL_MAX_VERTEX_ATTRIBS.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glVertexArrayAttrib{I, L}Format functions.
        /// </param>
        /// <param name="attribindex">
        /// The generic vertex attribute array being described.
        /// </param>
        /// <param name="size">
        /// The number of values per vertex that are stored in the array.
        /// </param>
        /// <param name="type">
        /// The type of the data stored in the array.
        /// </param>
        /// <param name="relativeoffset">
        /// The distance between elements within the buffer.
        /// </param>
        public static void VertexArrayAttribLFormat(UInt32 vaobj, UInt32 attribindex, Int32 size, VertexAttribFormatEnum type, UInt32 relativeoffset)
        {
            Delegates.glVertexArrayAttribLFormat(vaobj, attribindex, size, type, relativeoffset);
        }

        /// <summary>
        /// Define an array of generic vertex attribute data.
        /// <para>
        /// glVertexAttribPointer, glVertexAttribIPointer and glVertexAttribLPointer specify the location and
        /// data format of the array of generic vertex attributes at index index to use when rendering. size
        /// specifies the number of components per attribute and must be 1, 2, 3, 4, or GL_BGRA. type specifies
        /// the data type of each component, and stride specifies the byte stride from one attribute to the
        /// next, allowing vertices and attributes to be packed into a single array or stored in separate
        /// arrays.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="size">
        /// Specifies the number of components per generic vertex attribute. Must be 1, 2, 3, 4. Additionally,
        /// the symbolic constant GL_BGRA is accepted by glVertexAttribPointer. The initial value is 4.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of each component in the array. The symbolic constants GL_BYTE,
        /// GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, and GL_UNSIGNED_INT are accepted by
        /// glVertexAttribPointer and glVertexAttribIPointer. Additionally GL_HALF_FLOAT, GL_FLOAT, GL_DOUBLE,
        /// GL_FIXED, GL_INT_2_10_10_10_REV, GL_UNSIGNED_INT_2_10_10_10_REV and GL_UNSIGNED_INT_10F_11F_11F_REV
        /// are accepted by glVertexAttribPointer. GL_DOUBLE is also accepted by glVertexAttribLPointer and is
        /// the only token accepted by the type parameter for that function. The initial value is GL_FLOAT.
        /// </param>
        /// <param name="normalized">
        /// For glVertexAttribPointer, specifies whether fixed-point data values should be normalized (GL_TRUE)
        /// or converted directly as fixed-point values (GL_FALSE) when they are accessed.
        /// </param>
        /// <param name="stride">
        /// Specifies the byte offset between consecutive generic vertex attributes. If stride is 0, the generic
        /// vertex attributes are understood to be tightly packed in the array. The initial value is 0.
        /// </param>
        /// <param name="pointer">
        /// Specifies a offset of the first component of the first generic vertex attribute in the array in the
        /// data store of the buffer currently bound to the GL_ARRAY_BUFFER target. The initial value is 0.
        /// </param>
        public static void VertexAttribPointer(UInt32 index, Int32 size, VertexAttribPointerType type, Boolean normalized, Int32 stride, IntPtr pointer)
        {
            Delegates.glVertexAttribPointer(index, size, type, normalized, stride, pointer);
        }

        /// <summary>
        /// Define an array of generic vertex attribute data.
        /// <para>
        /// glVertexAttribPointer, glVertexAttribIPointer and glVertexAttribLPointer specify the location and
        /// data format of the array of generic vertex attributes at index index to use when rendering. size
        /// specifies the number of components per attribute and must be 1, 2, 3, 4, or GL_BGRA. type specifies
        /// the data type of each component, and stride specifies the byte stride from one attribute to the
        /// next, allowing vertices and attributes to be packed into a single array or stored in separate
        /// arrays.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="size">
        /// Specifies the number of components per generic vertex attribute. Must be 1, 2, 3, 4. Additionally,
        /// the symbolic constant GL_BGRA is accepted by glVertexAttribPointer. The initial value is 4.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of each component in the array. The symbolic constants GL_BYTE,
        /// GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, and GL_UNSIGNED_INT are accepted by
        /// glVertexAttribPointer and glVertexAttribIPointer. Additionally GL_HALF_FLOAT, GL_FLOAT, GL_DOUBLE,
        /// GL_FIXED, GL_INT_2_10_10_10_REV, GL_UNSIGNED_INT_2_10_10_10_REV and GL_UNSIGNED_INT_10F_11F_11F_REV
        /// are accepted by glVertexAttribPointer. GL_DOUBLE is also accepted by glVertexAttribLPointer and is
        /// the only token accepted by the type parameter for that function. The initial value is GL_FLOAT.
        /// </param>
        /// <param name="normalized">
        /// For glVertexAttribPointer, specifies whether fixed-point data values should be normalized (GL_TRUE)
        /// or converted directly as fixed-point values (GL_FALSE) when they are accessed.
        /// </param>
        /// <param name="stride">
        /// Specifies the byte offset between consecutive generic vertex attributes. If stride is 0, the generic
        /// vertex attributes are understood to be tightly packed in the array. The initial value is 0.
        /// </param>
        /// <param name="pointer">
        /// Specifies a offset of the first component of the first generic vertex attribute in the array in the
        /// data store of the buffer currently bound to the GL_ARRAY_BUFFER target. The initial value is 0.
        /// </param>
        public static void VertexAttribPointer(Int32 index, Int32 size, VertexAttribPointerType type, Boolean normalized, Int32 stride, IntPtr pointer)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribPointer((UInt32)index, size, type, normalized, stride, pointer);
        }

        /// <summary>
        /// Define an array of generic vertex attribute data.
        /// <para>
        /// glVertexAttribPointer, glVertexAttribIPointer and glVertexAttribLPointer specify the location and
        /// data format of the array of generic vertex attributes at index index to use when rendering. size
        /// specifies the number of components per attribute and must be 1, 2, 3, 4, or GL_BGRA. type specifies
        /// the data type of each component, and stride specifies the byte stride from one attribute to the
        /// next, allowing vertices and attributes to be packed into a single array or stored in separate
        /// arrays.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="size">
        /// Specifies the number of components per generic vertex attribute. Must be 1, 2, 3, 4. Additionally,
        /// the symbolic constant GL_BGRA is accepted by glVertexAttribPointer. The initial value is 4.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of each component in the array. The symbolic constants GL_BYTE,
        /// GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, and GL_UNSIGNED_INT are accepted by
        /// glVertexAttribPointer and glVertexAttribIPointer. Additionally GL_HALF_FLOAT, GL_FLOAT, GL_DOUBLE,
        /// GL_FIXED, GL_INT_2_10_10_10_REV, GL_UNSIGNED_INT_2_10_10_10_REV and GL_UNSIGNED_INT_10F_11F_11F_REV
        /// are accepted by glVertexAttribPointer. GL_DOUBLE is also accepted by glVertexAttribLPointer and is
        /// the only token accepted by the type parameter for that function. The initial value is GL_FLOAT.
        /// </param>
        /// <param name="stride">
        /// Specifies the byte offset between consecutive generic vertex attributes. If stride is 0, the generic
        /// vertex attributes are understood to be tightly packed in the array. The initial value is 0.
        /// </param>
        /// <param name="pointer">
        /// Specifies a offset of the first component of the first generic vertex attribute in the array in the
        /// data store of the buffer currently bound to the GL_ARRAY_BUFFER target. The initial value is 0.
        /// </param>
        public static void VertexAttribIPointer(UInt32 index, Int32 size, VertexAttribPointerType type, Int32 stride, IntPtr pointer)
        {
            Delegates.glVertexAttribIPointer(index, size, type, stride, pointer);
        }

        /// <summary>
        /// Define an array of generic vertex attribute data.
        /// <para>
        /// glVertexAttribPointer, glVertexAttribIPointer and glVertexAttribLPointer specify the location and
        /// data format of the array of generic vertex attributes at index index to use when rendering. size
        /// specifies the number of components per attribute and must be 1, 2, 3, 4, or GL_BGRA. type specifies
        /// the data type of each component, and stride specifies the byte stride from one attribute to the
        /// next, allowing vertices and attributes to be packed into a single array or stored in separate
        /// arrays.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="size">
        /// Specifies the number of components per generic vertex attribute. Must be 1, 2, 3, 4. Additionally,
        /// the symbolic constant GL_BGRA is accepted by glVertexAttribPointer. The initial value is 4.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of each component in the array. The symbolic constants GL_BYTE,
        /// GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, and GL_UNSIGNED_INT are accepted by
        /// glVertexAttribPointer and glVertexAttribIPointer. Additionally GL_HALF_FLOAT, GL_FLOAT, GL_DOUBLE,
        /// GL_FIXED, GL_INT_2_10_10_10_REV, GL_UNSIGNED_INT_2_10_10_10_REV and GL_UNSIGNED_INT_10F_11F_11F_REV
        /// are accepted by glVertexAttribPointer. GL_DOUBLE is also accepted by glVertexAttribLPointer and is
        /// the only token accepted by the type parameter for that function. The initial value is GL_FLOAT.
        /// </param>
        /// <param name="stride">
        /// Specifies the byte offset between consecutive generic vertex attributes. If stride is 0, the generic
        /// vertex attributes are understood to be tightly packed in the array. The initial value is 0.
        /// </param>
        /// <param name="pointer">
        /// Specifies a offset of the first component of the first generic vertex attribute in the array in the
        /// data store of the buffer currently bound to the GL_ARRAY_BUFFER target. The initial value is 0.
        /// </param>
        public static void VertexAttribIPointer(Int32 index, Int32 size, VertexAttribPointerType type, Int32 stride, IntPtr pointer)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribIPointer((UInt32)index, size, type, stride, pointer);
        }

        /// <summary>
        /// Define an array of generic vertex attribute data.
        /// <para>
        /// glVertexAttribPointer, glVertexAttribIPointer and glVertexAttribLPointer specify the location and
        /// data format of the array of generic vertex attributes at index index to use when rendering. size
        /// specifies the number of components per attribute and must be 1, 2, 3, 4, or GL_BGRA. type specifies
        /// the data type of each component, and stride specifies the byte stride from one attribute to the
        /// next, allowing vertices and attributes to be packed into a single array or stored in separate
        /// arrays.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="size">
        /// Specifies the number of components per generic vertex attribute. Must be 1, 2, 3, 4. Additionally,
        /// the symbolic constant GL_BGRA is accepted by glVertexAttribPointer. The initial value is 4.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of each component in the array. The symbolic constants GL_BYTE,
        /// GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, and GL_UNSIGNED_INT are accepted by
        /// glVertexAttribPointer and glVertexAttribIPointer. Additionally GL_HALF_FLOAT, GL_FLOAT, GL_DOUBLE,
        /// GL_FIXED, GL_INT_2_10_10_10_REV, GL_UNSIGNED_INT_2_10_10_10_REV and GL_UNSIGNED_INT_10F_11F_11F_REV
        /// are accepted by glVertexAttribPointer. GL_DOUBLE is also accepted by glVertexAttribLPointer and is
        /// the only token accepted by the type parameter for that function. The initial value is GL_FLOAT.
        /// </param>
        /// <param name="stride">
        /// Specifies the byte offset between consecutive generic vertex attributes. If stride is 0, the generic
        /// vertex attributes are understood to be tightly packed in the array. The initial value is 0.
        /// </param>
        /// <param name="pointer">
        /// Specifies a offset of the first component of the first generic vertex attribute in the array in the
        /// data store of the buffer currently bound to the GL_ARRAY_BUFFER target. The initial value is 0.
        /// </param>
        public static void VertexAttribLPointer(UInt32 index, Int32 size, VertexAttribPointerType type, Int32 stride, IntPtr pointer)
        {
            Delegates.glVertexAttribLPointer(index, size, type, stride, pointer);
        }

        /// <summary>
        /// Define an array of generic vertex attribute data.
        /// <para>
        /// glVertexAttribPointer, glVertexAttribIPointer and glVertexAttribLPointer specify the location and
        /// data format of the array of generic vertex attributes at index index to use when rendering. size
        /// specifies the number of components per attribute and must be 1, 2, 3, 4, or GL_BGRA. type specifies
        /// the data type of each component, and stride specifies the byte stride from one attribute to the
        /// next, allowing vertices and attributes to be packed into a single array or stored in separate
        /// arrays.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specifies the index of the generic vertex attribute to be modified.
        /// </param>
        /// <param name="size">
        /// Specifies the number of components per generic vertex attribute. Must be 1, 2, 3, 4. Additionally,
        /// the symbolic constant GL_BGRA is accepted by glVertexAttribPointer. The initial value is 4.
        /// </param>
        /// <param name="type">
        /// Specifies the data type of each component in the array. The symbolic constants GL_BYTE,
        /// GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, and GL_UNSIGNED_INT are accepted by
        /// glVertexAttribPointer and glVertexAttribIPointer. Additionally GL_HALF_FLOAT, GL_FLOAT, GL_DOUBLE,
        /// GL_FIXED, GL_INT_2_10_10_10_REV, GL_UNSIGNED_INT_2_10_10_10_REV and GL_UNSIGNED_INT_10F_11F_11F_REV
        /// are accepted by glVertexAttribPointer. GL_DOUBLE is also accepted by glVertexAttribLPointer and is
        /// the only token accepted by the type parameter for that function. The initial value is GL_FLOAT.
        /// </param>
        /// <param name="stride">
        /// Specifies the byte offset between consecutive generic vertex attributes. If stride is 0, the generic
        /// vertex attributes are understood to be tightly packed in the array. The initial value is 0.
        /// </param>
        /// <param name="pointer">
        /// Specifies a offset of the first component of the first generic vertex attribute in the array in the
        /// data store of the buffer currently bound to the GL_ARRAY_BUFFER target. The initial value is 0.
        /// </param>
        public static void VertexAttribLPointer(Int32 index, Int32 size, VertexAttribPointerType type, Int32 stride, IntPtr pointer)
        {
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            Delegates.glVertexAttribLPointer((UInt32)index, size, type, stride, pointer);
        }

        /// <summary>
        /// Modify the rate at which generic vertex attributes advance.
        /// <para>
        /// glVertexBindingDivisor and glVertexArrayBindingDivisor modify the rate at which generic vertex
        /// attributes advance when rendering multiple instances of primitives in a single draw command. If
        /// divisor is zero, the attributes using the buffer bound to bindingindex advance once per vertex. If
        /// divisor is non-zero, the attributes advance once per divisor instances of the set(s) of vertices
        /// being rendered. An attribute is referred to as instanced if the corresponding divisor value is
        /// non-zero.
        /// </para>
        /// </summary>
        /// <param name="bindingindex">
        /// The index of the binding whose divisor to modify.
        /// </param>
        /// <param name="divisor">
        /// The new value for the instance step rate to apply.
        /// </param>
        public static void VertexBindingDivisor(UInt32 bindingindex, UInt32 divisor)
        {
            Delegates.glVertexBindingDivisor(bindingindex, divisor);
        }

        /// <summary>
        /// Modify the rate at which generic vertex attributes advance.
        /// <para>
        /// glVertexBindingDivisor and glVertexArrayBindingDivisor modify the rate at which generic vertex
        /// attributes advance when rendering multiple instances of primitives in a single draw command. If
        /// divisor is zero, the attributes using the buffer bound to bindingindex advance once per vertex. If
        /// divisor is non-zero, the attributes advance once per divisor instances of the set(s) of vertices
        /// being rendered. An attribute is referred to as instanced if the corresponding divisor value is
        /// non-zero.
        /// </para>
        /// </summary>
        /// <param name="vaobj">
        /// Specifies the name of the vertex array object for glVertexArrayBindingDivisor function.
        /// </param>
        /// <param name="bindingindex">
        /// The index of the binding whose divisor to modify.
        /// </param>
        /// <param name="divisor">
        /// The new value for the instance step rate to apply.
        /// </param>
        public static void VertexArrayBindingDivisor(UInt32 vaobj, UInt32 bindingindex, UInt32 divisor)
        {
            Delegates.glVertexArrayBindingDivisor(vaobj, bindingindex, divisor);
        }

        /// <summary>
        /// Set the viewport.
        /// <para>
        /// glViewport specifies the affine transformation of x and y from normalized device coordinates to
        /// window coordinates. Let x nd y nd be normalized device coordinates. Then the window coordinates x w
        /// y w are computed as follows:.
        /// </para>
        /// </summary>
        /// <param name="x">
        /// Specify the lower left corner of the viewport rectangle, in pixels. The initial value is (0,0).
        /// </param>
        /// <param name="y">
        /// Specify the lower left corner of the viewport rectangle, in pixels. The initial value is (0,0).
        /// </param>
        /// <param name="width">
        /// Specify the width and height of the viewport. When a GL context is first attached to a window, width
        /// and height are set to the dimensions of that window.
        /// </param>
        /// <param name="height">
        /// Specify the width and height of the viewport. When a GL context is first attached to a window, width
        /// and height are set to the dimensions of that window.
        /// </param>
        public static void Viewport(Int32 x, Int32 y, Int32 width, Int32 height)
        {
            Delegates.glViewport(x, y, width, height);
        }

        /// <summary>
        /// Set multiple viewports.
        /// <para>
        /// glViewportArrayv specifies the parameters for multiple viewports simulataneously. first specifies
        /// the index of the first viewport to modify and count specifies the number of viewports to modify.
        /// first must be less than the value of GL_MAX_VIEWPORTS, and first + count must be less than or equal
        /// to the value of GL_MAX_VIEWPORTS. Viewports whose indices lie outside the range [first, first +
        /// count) are not modified. v contains the address of an array of floating point values specifying the
        /// left (x), bottom (y), width (w), and height (h) of each viewport, in that order. x and y give the
        /// location of the viewport's lower left corner, and w and h give the width and height of the viewport,
        /// respectively. The viewport specifies the affine transformation of x and y from normalized device
        /// coordinates to window coordinates. Let x nd y nd be normalized device coordinates. Then the window
        /// coordinates x w y w are computed as follows:.
        /// </para>
        /// </summary>
        /// <param name="first">
        /// Specify the first viewport to set.
        /// </param>
        /// <param name="count">
        /// Specify the number of viewports to set.
        /// </param>
        /// <param name="v">
        /// Specify the address of an array containing the viewport parameters.
        /// </param>
        public static void ViewportArrayv(UInt32 first, Int32 count, Single[] v)
        {
            Delegates.glViewportArrayv(first, count, v);
        }

        /// <summary>
        /// Set a specified viewport.
        /// <para>
        /// glViewportIndexedf and glViewportIndexedfv specify the parameters for a single viewport. index
        /// specifies the index of the viewport to modify. index must be less than the value of
        /// GL_MAX_VIEWPORTS. For glViewportIndexedf, x, y, w, and h specify the left, bottom, width and height
        /// of the viewport in pixels, respectively. For glViewportIndexedfv, v contains the address of an array
        /// of floating point values specifying the left (x), bottom (y), width (w), and height (h) of each
        /// viewport, in that order. x and y give the location of the viewport's lower left corner, and w and h
        /// give the width and height of the viewport, respectively. The viewport specifies the affine
        /// transformation of x and y from normalized device coordinates to window coordinates. Let x nd y nd be
        /// normalized device coordinates. Then the window coordinates x w y w are computed as follows:.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specify the first viewport to set.
        /// </param>
        /// <param name="x">
        /// For glViewportIndexedf, specifies the lower left corner of the viewport rectangle, in pixels. The
        /// initial value is (0,0).
        /// </param>
        /// <param name="y">
        /// For glViewportIndexedf, specifies the lower left corner of the viewport rectangle, in pixels. The
        /// initial value is (0,0).
        /// </param>
        /// <param name="w">
        /// For glViewportIndexedf, specifies the width and height of the viewport. When a GL context is first
        /// attached to a window, width and height are set to the dimensions of that window.
        /// </param>
        /// <param name="h">
        /// For glViewportIndexedf, specifies the width and height of the viewport. When a GL context is first
        /// attached to a window, width and height are set to the dimensions of that window.
        /// </param>
        public static void ViewportIndexedf(UInt32 index, Single x, Single y, Single w, Single h)
        {
            Delegates.glViewportIndexedf(index, x, y, w, h);
        }

        /// <summary>
        /// Set a specified viewport.
        /// <para>
        /// glViewportIndexedf and glViewportIndexedfv specify the parameters for a single viewport. index
        /// specifies the index of the viewport to modify. index must be less than the value of
        /// GL_MAX_VIEWPORTS. For glViewportIndexedf, x, y, w, and h specify the left, bottom, width and height
        /// of the viewport in pixels, respectively. For glViewportIndexedfv, v contains the address of an array
        /// of floating point values specifying the left (x), bottom (y), width (w), and height (h) of each
        /// viewport, in that order. x and y give the location of the viewport's lower left corner, and w and h
        /// give the width and height of the viewport, respectively. The viewport specifies the affine
        /// transformation of x and y from normalized device coordinates to window coordinates. Let x nd y nd be
        /// normalized device coordinates. Then the window coordinates x w y w are computed as follows:.
        /// </para>
        /// </summary>
        /// <param name="index">
        /// Specify the first viewport to set.
        /// </param>
        /// <param name="v">
        /// For glViewportIndexedfv, specifies the address of an array containing the viewport parameters.
        /// </param>
        public static void ViewportIndexedfv(UInt32 index, Single[] v)
        {
            Delegates.glViewportIndexedfv(index, v);
        }

        /// <summary>
        /// Instruct the GL server to block until the specified sync object becomes signaled.
        /// <para>
        /// glWaitSync causes the GL server to block and wait until sync becomes signaled. sync is the name of
        /// an existing sync object upon which to wait. flags and timeout are currently not used and must be set
        /// to zero and the special value GL_TIMEOUT_IGNORED, respectively[1]. glWaitSync will always wait no
        /// longer than an implementation-dependent timeout. The duration of this timeout in nanoseconds may be
        /// queried by calling glGet with the parameter GL_MAX_SERVER_WAIT_TIMEOUT. There is currently no way to
        /// determine whether glWaitSync unblocked because the timeout expired or because the sync object being
        /// waited on was signaled.
        /// </para>
        /// </summary>
        /// <param name="sync">
        /// Specifies the sync object whose status to wait on.
        /// </param>
        /// <param name="flags">
        /// A bitfield controlling the command flushing behavior. flags may be zero.
        /// </param>
        /// <param name="timeout">
        /// Specifies the timeout that the server should wait before continuing. timeout must be
        /// GL_TIMEOUT_IGNORED.
        /// </param>
        public static void WaitSync(IntPtr sync, UInt32 flags, UInt64 timeout)
        {
            Delegates.glWaitSync(sync, flags, timeout);
        }

    }
}
