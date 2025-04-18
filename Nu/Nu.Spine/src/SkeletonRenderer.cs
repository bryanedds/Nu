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

namespace Spine
{
    /// <summary>Draws region and mesh attachments.</summary>
    public class SkeletonRenderer
    {
        private SkeletonClipping clipper = new SkeletonClipping();
        private MeshBatcher batcher;
        private float[] vertices = new float[8];
        private int[] quadTriangles = { 0, 1, 2, 2, 3, 0 };
        private bool premultipliedAlpha;
        private float zSpacing = 0.0f;
        private float z = 0.0f;

        /// <summary>Returns the <see cref="SkeletonClipping"/> used by this renderer for use with e.g.
        /// <see cref="Skeleton.GetBounds(out float, out float, out float, out float, ref float[], SkeletonClipping)"/>
        /// </summary>
        public SkeletonClipping SkeletonClipping { get { return clipper; } }

        public IVertexEffect VertexEffect { get; set; }

        public bool PremultipliedAlpha { get { return premultipliedAlpha; } set { premultipliedAlpha = value; } }

        /// <summary>Attachments are rendered back to front in the x/y plane by the SkeletonRenderer.
        /// Each attachment is offset by a customizable z-spacing value on the z-axis to avoid z-fighting
        /// in shaders with ZWrite enabled. Typical values lie in the range [-0.1, 0].</summary>
        public float ZSpacing { get { return zSpacing; } set { zSpacing = value; } }

        /// <summary>A Z position offset added at each vertex.</summary>
        public float Z { get { return z; } set { z = value; } }

        public SkeletonRenderer(Func<string, string, uint> createShaderFromStrings)
        {
            batcher = new MeshBatcher(createShaderFromStrings);
        }

        public void Destroy()
        {
            batcher.Destroy();
        }

        public void Draw(Func<object, uint> getTextureId, Skeleton skeleton, in Matrix4x4 matrix)
        {
            var drawOrder = skeleton.DrawOrder;
            var drawOrderItems = skeleton.DrawOrder.Items;
            float skeletonR = skeleton.R, skeletonG = skeleton.G, skeletonB = skeleton.B, skeletonA = skeleton.A;
            ColorPacked color = new ColorPacked();
            BlendingFactor blendSrc = BlendingFactor.SrcAlpha;
            BlendingFactor blendDst = BlendingFactor.OneMinusSrcAlpha;

            if (VertexEffect != null) VertexEffect.Begin(skeleton);

            for (int i = 0, n = drawOrder.Count; i < n; i++)
            {
                Slot slot = drawOrderItems[i];
                Attachment attachment = slot.Attachment;
                float attachmentZOffset = z + zSpacing * i; // NOTE: BGE: not utilized because the shader I found doesn't seem to support it.

                float attachmentColorR, attachmentColorG, attachmentColorB, attachmentColorA;
                object textureObject = null;
                int verticesCount = 0;
                float[] vertices = this.vertices;
                int indicesCount = 0;
                int[] indices = null;
                float[] uvs = null;

                if (attachment is RegionAttachment)
                {
                    RegionAttachment regionAttachment = (RegionAttachment)attachment;
                    attachmentColorR = regionAttachment.R; attachmentColorG = regionAttachment.G; attachmentColorB = regionAttachment.B; attachmentColorA = regionAttachment.A;
                    regionAttachment.ComputeWorldVertices(slot, vertices, 0, 2);
                    verticesCount = 4;
                    indicesCount = 6;
                    indices = quadTriangles;
                    uvs = regionAttachment.UVs;
                    AtlasRegion region = (AtlasRegion)regionAttachment.Region;
                    textureObject = region.page.rendererObject;
                }
                else if (attachment is MeshAttachment)
                {
                    MeshAttachment mesh = (MeshAttachment)attachment;
                    attachmentColorR = mesh.R; attachmentColorG = mesh.G; attachmentColorB = mesh.B; attachmentColorA = mesh.A;
                    int vertexCount = mesh.WorldVerticesLength;
                    if (vertices.Length < vertexCount) vertices = new float[vertexCount];
                    verticesCount = vertexCount >> 1;
                    mesh.ComputeWorldVertices(slot, vertices);
                    indicesCount = mesh.Triangles.Length;
                    indices = mesh.Triangles;
                    uvs = mesh.UVs;
                    AtlasRegion region = (AtlasRegion)mesh.Region;
                    textureObject = region.page.rendererObject;
                }
                else if (attachment is ClippingAttachment)
                {
                    ClippingAttachment clip = (ClippingAttachment)attachment;
                    clipper.ClipStart(slot, clip);
                    continue;
                }
                else continue;

                // update blend state
                BlendingFactor blendSrcLocal;
                BlendingFactor blendDstLocal;
                switch (slot.Data.BlendMode)
                {
                    case BlendMode.Normal:
                        blendSrcLocal = BlendingFactor.SrcAlpha;
                        blendDstLocal = BlendingFactor.OneMinusSrcAlpha;
                        break;
                    case BlendMode.Additive:
                        blendSrcLocal = BlendingFactor.SrcAlpha;
                        blendDstLocal = BlendingFactor.One;
                        break;
                    case BlendMode.Multiply:
                        blendSrcLocal = BlendingFactor.DstColor;
                        blendDstLocal = BlendingFactor.Zero;
                        break;
                    case BlendMode.Screen:
                        blendSrcLocal = BlendingFactor.OneMinusDstColor;
                        blendDstLocal = BlendingFactor.SrcColor;
                        break;
                    default:
                        blendSrcLocal = BlendingFactor.SrcAlpha;
                        blendDstLocal = BlendingFactor.OneMinusSrcAlpha;
                        break;
                }

                // finish batch if blend state changed
                if (blendSrcLocal != blendSrc || blendDstLocal != blendDst)
                {
                    batcher.Draw(blendSrc, blendDst, matrix);
                    blendSrc = blendSrcLocal;
                    blendDst = blendDstLocal;
                }

                // calculate color
                float a = skeletonA * slot.A * attachmentColorA;
                if (premultipliedAlpha)
                {
                    color = new ColorPacked(
                            skeletonR * slot.R * attachmentColorR * a,
                            skeletonG * slot.G * attachmentColorG * a,
                            skeletonB * slot.B * attachmentColorB * a, a);
                }
                else
                {
                    color = new ColorPacked(
                            skeletonR * slot.R * attachmentColorR,
                            skeletonG * slot.G * attachmentColorG,
                            skeletonB * slot.B * attachmentColorB, a);
                }

                ColorPacked darkColor = new ColorPacked();
                if (slot.HasSecondColor)
                {
                    if (premultipliedAlpha)
                    {
                        darkColor = new ColorPacked(slot.R2 * a, slot.G2 * a, slot.B2 * a, 0);
                    }
                    else
                    {
                        darkColor = new ColorPacked(slot.R2 * a, slot.G2 * a, slot.B2 * a, 0);
                    }
                }
                darkColor.A = premultipliedAlpha ? (byte)255 : (byte)0;

                // clip
                if (clipper.IsClipping)
                {
                    clipper.ClipTriangles(vertices, indices, indicesCount, uvs);
                    vertices = clipper.ClippedVertices.Items;
                    verticesCount = clipper.ClippedVertices.Count >> 1;
                    indices = clipper.ClippedTriangles.Items;
                    indicesCount = clipper.ClippedTriangles.Count;
                    uvs = clipper.ClippedUVs.Items;
                }

                if (verticesCount == 0 || indicesCount == 0) continue;

                // submit to batch
                // TODO: restore Spine layers functionality.
                MeshItem item = batcher.NextItem(verticesCount, indicesCount);
                item.texture = getTextureId(textureObject);

                for (int ii = 0, nn = indicesCount; ii < nn; ii++)
                {
                    item.triangles[ii] = indices[ii];
                }
                Vertex[] itemVertices = item.vertices;
                for (int ii = 0, v = 0, nn = verticesCount << 1; v < nn; ii++, v += 2)
                {
                    itemVertices[ii].Color = color.PackedValue;
                    itemVertices[ii].Color2 = darkColor.PackedValue;
                    itemVertices[ii].Position.X = vertices[v];
                    itemVertices[ii].Position.Y = vertices[v + 1];
                    //itemVertices[ii].Position.Z = attachmentZOffset; NOTE: BGE: not supported by the shader I found.
                    itemVertices[ii].TextureCoordinate.X = uvs[v];
                    itemVertices[ii].TextureCoordinate.Y = uvs[v + 1];
                    if (VertexEffect != null) VertexEffect.Transform(ref itemVertices[ii]);
                }

                clipper.ClipEnd(slot);
            }
            clipper.ClipEnd();
            if (VertexEffect != null) VertexEffect.End();
            batcher.Draw(blendSrc, blendDst, matrix);
        }
    }
}