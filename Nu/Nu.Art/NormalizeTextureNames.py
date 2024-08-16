# a blender script for normalizing texture names for consumption by Nu
import bpy
import os

print("Running texture normalization script...")

# normalize texture file names
texture_file_names = set()
for obj in bpy.context.scene.objects:
    if obj.data:
        for material_slot in obj.material_slots:
            if material_slot.material:
                for node in material_slot.material.node_tree.nodes:
                    if node.type == 'TEX_IMAGE':
                        try:
                            
                            # old file path
                            old_file_name = os.path.basename(node.image.filepath.replace("//", ""))
                            old_file_path = os.getcwd() + "\\textures\\" + old_file_name

                            # ambient occlusion
                            new_file_name = new_file_name.replace("AO.", "AmbientOcclusion.")
                            new_file_name = new_file_name.replace("AO_2k.", "AmbientOcclusion.")
                            new_file_name = new_file_name.replace("ao.", "AmbientOcclusion.")
                            new_file_name = new_file_name.replace("ao_2k.", "AmbientOcclusion.")
                            new_file_name = new_file_name.replace("ambientOcclusion.", "AmbientOcclusion.")

                            # albedo
                            new_file_name = old_file_name
                            new_file_name = new_file_name.replace("BaseColor.", "Albedo.")
                            new_file_name = new_file_name.replace("base_color.", "Albedo.")
                            new_file_name = new_file_name.replace("Color.", "Albedo.")
                            new_file_name = new_file_name.replace("color.", "Albedo.")
                            new_file_name = new_file_name.replace("Diffuse.", "Albedo.")
                            new_file_name = new_file_name.replace("diffuse.", "Albedo.")
                            new_file_name = new_file_name.replace("albedo.", "Albedo.")

                            # roughness
                            new_file_name = new_file_name.replace("Rough_2k.", "Roughness.")
                            new_file_name = new_file_name.replace("rough_2k.", "Roughness.")
                            new_file_name = new_file_name.replace("rough.", "Roughness.")
                            new_file_name = new_file_name.replace("Spec_2k.", "Roughness.")
                            new_file_name = new_file_name.replace("spec_2k.", "Roughness.")
                            new_file_name = new_file_name.replace("spec.", "Roughness.")
                            new_file_name = new_file_name.replace("roughness.", "Roughness.")

                            # metallic
                            new_file_name = new_file_name.replace("Metalness.", "Metallic.")
                            new_file_name = new_file_name.replace("metalness.", "Metallic.")
                            new_file_name = new_file_name.replace("Metalness2k.", "Metallic.")
                            new_file_name = new_file_name.replace("metalness_2k.", "Metallic.")
                            new_file_name = new_file_name.replace("metallic.", "Metallic.")

                            # normal
                            new_file_name = new_file_name.replace("NormalGL.", "Normal.")
                            new_file_name = new_file_name.replace("nor_gl.", "Normal.")
                            new_file_name = new_file_name.replace("NormalDX.", "Normal.")
                            new_file_name = new_file_name.replace("nor_dx.", "Normal.")
                            new_file_name = new_file_name.replace("Normal_2k.", "Normal.")
                            new_file_name = new_file_name.replace("nor_2k.", "Normal.")
                            new_file_name = new_file_name.replace("normal.", "Normal.")

                            # user-defined
                            new_file_name = new_file_name.replace("plant_22.", "PlantAlbedo.")
                            new_file_name = new_file_name.replace("SHC art 2.", "ShcArt2Albedo.")
                            new_file_name = new_file_name.replace("SHC art.", "ShcArtaAlbedo.")
                            new_file_name = new_file_name.replace("SHC Tree Leaves 4096px .", "ShcTreeLeavesAlbedo.")
                            new_file_name = new_file_name.replace("SHC-full-planet-edit-1.", "ShcFullPlanetAlbedo.")
                            new_file_name = new_file_name.replace("Ultrawide screen.", "UltrawideScreenAlbedo.")
                            new_file_name = new_file_name.replace("pc hardware.", "PcHardwareAlbedo.")
                            new_file_name = new_file_name.replace("cylces10.", "Cycles10Albedo.")

                            # new file path
                            new_file_path = os.getcwd() + "\\textures\\" + new_file_name

                            # rename the image if changed
                            if new_file_name != old_file_name:
                                
                                # file on disk if it hasn't been already
                                if new_file_name not in texture_file_names:
                                    print("Attemping to rename " + old_file_path + " to " + new_file_path)
                                    os.rename(old_file_path, new_file_path)
                                    texture_file_names.add(new_file_name)

                                # rename the image and change its filepath in scene
                                print("Renaming node image filepath to " + new_file_path)
                                node.image.filepath = new_file_path
                                node.image.name = new_file_name

                            # add the texture file name to the list of changed file names
                            texture_file_names.add(new_file_name)

                        # handle error                            
                        except Exception as exn:
                            print(exn)