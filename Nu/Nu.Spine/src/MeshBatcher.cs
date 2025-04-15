/******************************************************************************
 * Spine Runtimes License Agreement
 * Last updated July 28, 2023. Replaces all prior versions.
 *
 * Copyright (c) 2013-2023, Esoteric Software LLC
 *
 * Integration of the Spine Runtimes into software or otherwise creating
 * derivative works of the Spine Runtimes is permitted under the terms and
 * conditions of Section 2 of the Spine Editor License Agreement:
 * http://esotericsoftware.com/spine-editor-license
 *
 * Otherwise, it is permitted to integrate the Spine Runtimes into software or
 * otherwise create derivative works of the Spine Runtimes (collectively,
 * "Products"), provided that each user of the Products must obtain their own
 * Spine Editor license and redistribution of the Products in any form must
 * include this license and copyright notice.
 *
 * THE SPINE RUNTIMES ARE PROVIDED BY ESOTERIC SOFTWARE LLC "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL ESOTERIC SOFTWARE LLC BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES,
 * BUSINESS INTERRUPTION, OR LOSS OF USE, DATA, OR PROFITS) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THE
 * SPINE RUNTIMES, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *****************************************************************************/

using OpenGL;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Numerics;
using static Spine.SkeletonBinary;

namespace Spine
{
    // #region License
    // /*
    // Microsoft Public License (Ms-PL)
    // MonoGame - Copyright � 2009 The MonoGame Team
    //
    // All rights reserved.
    //
    // This license governs use of the accompanying software. If you use the software, you accept this license. If you do not
    // accept the license, do not use the software.
    //
    // 1. Definitions
    // The terms "reproduce," "reproduction," "derivative works," and "distribution" have the same meaning here as under
    // U.S. copyright law.
    //
    // A "contribution" is the original software, or any additions or changes to the software.
    // A "contributor" is any person that distributes its contribution under this license.
    // "Licensed patents" are a contributor's patent claims that read directly on its contribution.
    //
    // 2. Grant of Rights
    // (A) Copyright Grant- Subject to the terms of this license, including the license conditions and limitations in section 3,
    // each contributor grants you a non-exclusive, worldwide, royalty-free copyright license to reproduce its contribution, prepare derivative works of its contribution, and distribute its contribution or any derivative works that you create.
    // (B) Patent Grant- Subject to the terms of this license, including the license conditions and limitations in section 3,
    // each contributor grants you a non-exclusive, worldwide, royalty-free license under its licensed patents to make, have made, use, sell, offer for sale, import, and/or otherwise dispose of its contribution in the software or derivative works of the contribution in the software.
    //
    // 3. Conditions and Limitations
    // (A) No Trademark License- This license does not grant you rights to use any contributors' name, logo, or trademarks.
    // (B) If you bring a patent claim against any contributor over patents that you claim are infringed by the software,
    // your patent license from such contributor to the software ends automatically.
    // (C) If you distribute any portion of the software, you must retain all copyright, patent, trademark, and attribution
    // notices that are present in the software.
    // (D) If you distribute any portion of the software in source code form, you may do so only under this license by including
    // a complete copy of this license with your distribution. If you distribute any portion of the software in compiled or object
    // code form, you may only do so under a license that complies with this license.
    // (E) The software is licensed "as-is." You bear the risk of using it. The contributors give no express warranties, guarantees
    // or conditions. You may have additional consumer rights under your local laws which this license cannot change. To the extent
    // permitted under your local laws, the contributors exclude the implied warranties of merchantability, fitness for a particular
    // purpose and non-infringement.
    // */
    // #endregion License
    //

    /// <summary>Draws batched meshes.</summary>
    public class MeshBatcher
    {
        private readonly List<MeshItem> items = new List<MeshItem>(256);
        private readonly Queue<MeshItem> freeItems = new Queue<MeshItem>(256);
        private readonly uint vao, vbo, ibo;
        private readonly uint shader;
        private readonly int uMatrix;
        private readonly int uTexture;
        private Vertex[] vertexArray = { };
        private ushort[] triangles = { };

        public unsafe MeshBatcher(Func<string, string, uint> createShaderFromStrings)
        {
            shader = createShaderFromStrings(VertexShader.vertexShader, VertexShader.fragmentShader);
            uMatrix = Gl.GetUniformLocation(shader, "uMatrix");
            uTexture = Gl.GetUniformLocation(shader, "uTexture");
            uint[] vaoArr = new uint[1];
            uint[] vboArr = new uint[1];
            uint[] iboArr = new uint[1];
            Gl.GenVertexArrays(vaoArr);
            Gl.GenBuffers(vboArr);
            Gl.GenBuffers(iboArr);
            vao = vaoArr[0];
            vbo = vboArr[0];
            ibo = iboArr[0];
            Gl.BindVertexArray(vao);
            Gl.BindBuffer(BufferTarget.ArrayBuffer, vbo);
            Gl.BindBuffer(BufferTarget.ElementArrayBuffer, ibo);
            Gl.VertexAttribPointer(0u, 2, VertexAttribPointerType.Float, false, sizeof(Vertex), (IntPtr)0);
            Gl.EnableVertexAttribArray(0u);
            Gl.VertexAttribPointer(1u, 4, VertexAttribPointerType.UnsignedByte, true, sizeof(Vertex), (IntPtr)8);
            Gl.EnableVertexAttribArray(1u);
            Gl.VertexAttribPointer(2u, 2, VertexAttribPointerType.Float, false, sizeof(Vertex), (IntPtr)12);
            Gl.EnableVertexAttribArray(2u);
            Gl.VertexAttribPointer(3u, 4, VertexAttribPointerType.UnsignedByte, true, sizeof(Vertex), (IntPtr)20);
            Gl.EnableVertexAttribArray(3u);
            Gl.BindVertexArray(0u);
            EnsureCapacity(256, 512);
        }

        public void Destroy()
        {
            Gl.DeleteBuffers(new uint[vbo]);
            Gl.DeleteBuffers(new uint[ibo]);
            Gl.DeleteVertexArrays(new uint[vao]);
        }

        /// <summary>Returns a pooled MeshItem.</summary>
        public MeshItem NextItem(int vertexCount, int triangleCount)
        {
            MeshItem item = freeItems.Count > 0 ? freeItems.Dequeue() : new MeshItem();
            if (item.vertices.Length < vertexCount) item.vertices = new Vertex[vertexCount];
            if (item.triangles.Length < triangleCount) item.triangles = new int[triangleCount];
            item.vertexCount = vertexCount;
            item.triangleCount = triangleCount;
            items.Add(item);
            return item;
        }

        public void Draw(BlendingFactor blendSrc, BlendingFactor blendDst, in Matrix4x4 matrix)
        {
            if (items.Count == 0) return;

            int itemCount = items.Count;
            int vertexCount = 0, triangleCount = 0;
            for (int i = 0; i < itemCount; i++)
            {
                MeshItem item = items[i];
                vertexCount += item.vertexCount;
                triangleCount += item.triangleCount;
            }
            EnsureCapacity(vertexCount, triangleCount);

            vertexCount = 0;
            triangleCount = 0;
            uint lastTexture = 0;
            for (int i = 0; i < itemCount; i++)
            {
                MeshItem item = items[i];
                int itemVertexCount = item.vertexCount;

                if (item.texture != lastTexture || vertexCount + itemVertexCount > short.MaxValue)
                {
                    FlushVertexArray(vertexCount, triangleCount, lastTexture, blendSrc, blendDst, matrix);
                    vertexCount = 0;
                    triangleCount = 0;
                    lastTexture = item.texture;

                    // TODO: restore Spine layers behavior.
                    //if (item.textureLayers != null)
                    //{
                    //    for (int layer = 1; layer < item.textureLayers.Length; ++layer)
                    //        device.Textures[layer] = item.textureLayers[layer];
                    //}
                }

                int[] itemTriangles = item.triangles;
                int itemTriangleCount = item.triangleCount;
                for (int ii = 0, t = triangleCount; ii < itemTriangleCount; ii++, t++)
                    triangles[t] = (ushort)(itemTriangles[ii] + vertexCount);
                triangleCount += itemTriangleCount;

                Array.Copy(item.vertices, 0, vertexArray, vertexCount, itemVertexCount);
                vertexCount += itemVertexCount;
            }

            FlushVertexArray(vertexCount, triangleCount, lastTexture, blendSrc, blendDst, matrix);

            // clean up cache
            for (int i = 0; i < items.Count; i++)
            {
                var item = items[i];
                item.texture = 0;
                freeItems.Enqueue(item);
            }
            items.Clear();
        }

        private void EnsureCapacity(int vertexCount, int triangleCount)
        {
            if (vertexArray.Length < vertexCount) vertexArray = new Vertex[vertexCount];
            if (triangles.Length < triangleCount) triangles = new ushort[triangleCount];
        }

        private unsafe void FlushVertexArray(
            int num_vertices,
            int num_indices,
            uint texture,
            BlendingFactor blendSrc,
            BlendingFactor blendDst,
            Matrix4x4 matrix)
        {
            if (num_vertices > 0 && num_indices > 0 && texture != 0u)
            {
                // setup state
                Gl.BlendEquation(BlendEquationMode.FuncAdd);
                Gl.BlendFunc(blendSrc, blendDst);
                Gl.Enable(EnableCap.Blend);

                // setup vao
                Gl.BindVertexArray(vao);

                // setup shader
                Gl.UseProgram(shader);
                Gl.UniformMatrix4(uMatrix, false, matrix.ToArray());
                Gl.Uniform1(uTexture, 0);

                // setup texture
                Gl.ActiveTexture(TextureUnit.Texture0);
                Gl.BindTexture(TextureTarget.Texture2d, texture);

                // setup geometry
                Gl.BindBuffer(BufferTarget.ArrayBuffer, vbo);
                using (var vertexArrayHnd = vertexArray.AsMemory().Pin())
                {
                    var vertexArrayPtr = (IntPtr)vertexArrayHnd.Pointer;
                    Gl.BufferData(BufferTarget.ArrayBuffer, (uint)(num_vertices * sizeof(Vertex)), vertexArrayPtr, BufferUsage.StaticDraw);
                }
                Gl.BindBuffer(BufferTarget.ElementArrayBuffer, ibo);
                using (var trianglesHnd = triangles.AsMemory().Pin())
                {
                    var trianglesPtr = (IntPtr)trianglesHnd.Pointer;
                    Gl.BufferData(BufferTarget.ElementArrayBuffer, (uint)(num_indices * sizeof(ushort)), trianglesPtr, BufferUsage.StaticDraw);
                }

                // draw geometry
                Gl.DrawElements(PrimitiveType.Triangles, num_indices, DrawElementsType.UnsignedShort, IntPtr.Zero);

                // teardown shader
                Gl.BindTexture(TextureTarget.Texture2d, 0u);
                Gl.UseProgram(0u);

                // teardown vao
                Gl.BindVertexArray(0u);

                // teardown state
                Gl.BlendEquation(BlendEquationMode.FuncAdd);
                Gl.BlendFunc(BlendingFactor.One, BlendingFactor.Zero);
                Gl.Disable(EnableCap.Blend);
            }
        }
    }

    public class MeshItem
    {
        public uint texture = 0;
        public uint[] textureLayers = null;
        public int vertexCount, triangleCount;
        public Vertex[] vertices = { };
        public int[] triangles = { };
    }
}