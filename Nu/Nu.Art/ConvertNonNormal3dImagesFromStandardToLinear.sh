find . -type f \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.tga" -o -iname "*.tif" -o -iname "*.tiff" \) ! -iname "*Normal*" | while read file; do

    # Convert the image from sRGB to linear RGB
    magick "$file" -colorspace RGB "$file"

done