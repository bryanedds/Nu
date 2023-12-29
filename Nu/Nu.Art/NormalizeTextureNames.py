# a blender script for normalizing texture names for consumption by Nu
import bpy
import os

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

                            # new file path
                            new_file_name = old_file_name
                            new_file_name = new_file_name.replace("BaseColor.png", "Albedo.png")
                            new_file_name = new_file_name.replace("BaseColor.jpg", "Albedo.jpg")
                            new_file_name = new_file_name.replace("Color.jpg", "Albedo.jpg")
                            new_file_name = new_file_name.replace("Diffuse.jpg", "Albedo.jpg")
                            new_file_name = new_file_name.replace("diff_2k.jpg", "Albedo.jpg")
                            new_file_name = new_file_name.replace("NormalGL.jpg", "Normal.jpg")
                            new_file_name = new_file_name.replace("NormalDX.jpg", "Normal.jpg")
                            new_file_name = new_file_name.replace("normal.jpg", "Normal.jpg")
                            new_file_name = new_file_name.replace("nor_2k.jpg", "Normal.jpg")
                            new_file_name = new_file_name.replace("nor_gl_2k.jpg", "Normal.jpg")
                            new_file_name = new_file_name.replace("roughness.jpg", "Roughness.jpg")
                            new_file_name = new_file_name.replace("rough_2k.jpg", "Roughness.jpg")
                            new_file_name = new_file_name.replace("spec_2k.jpg", "Roughness.jpg")
                            new_file_name = new_file_name.replace("Metalness.jpg", "Metallic.jpg")
                            new_file_name = new_file_name.replace("AO.png", "AmbientOcclusion.png")
                            new_file_name = new_file_name.replace("AO.jpg", "AmbientOcclusion.jpg")
                            new_file_name = new_file_name.replace("ao.jpg", "AmbientOcclusion.jpg")
                            new_file_name = new_file_name.replace("ao_2k.jpg", "AmbientOcclusion.jpg")
                            new_file_name = new_file_name.replace("plant_22.png", "PlantAlbedo.png")
                            new_file_name = new_file_name.replace("SHC art.jpg", "ShcArtaAlbedo.jpg")
                            new_file_name = new_file_name.replace("SHC art 2.jpg", "ShcArt2Albedo.jpg")
                            new_file_name = new_file_name.replace("SHC Tree Leaves 4096px .jpg", "ShcTreeLeavesAlbedo.jpg")
                            new_file_name = new_file_name.replace("SHC-full-planet-edit-1.jpg", "ShcFullPlanetAlbedo.jpg")
                            new_file_name = new_file_name.replace("Ultrawide screen.jpg", "UltrawideScreenAlbedo.jpg")
                            new_file_name = new_file_name.replace("pc hardware.jpg", "PcHardwareAlbedo.jpg")
                            new_file_name = new_file_name.replace("cylces10.jpg", "Cycles10Albedo.jpg")
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