# configure Linux for Nu application
echo Configuring Linux for Nu application...

# configure sdl2 and components
sudo apt update
sudo apt install \
  libsdl2-dev libsdl2-2.28-0 \
  libjpeg-dev libwebp-dev libtiff5-dev libsdl2-image-dev libsdl2-image-2.8-0 \
  libmikmod-dev libfishsound1-dev libsmpeg-dev liboggz2-dev libflac-dev libfluidsynth-dev libsdl2-mixer-dev libsdl2-mixer-2.8-0 \
  libfreetype6-dev libsdl2-ttf-dev libsdl2-ttf-2.22-0

# configure .net
# if this reports that the package can't be found, try the advice here - https://dev.to/solrevdev/unable-to-locate-package-dotnet-sdk-3-1-4b6
sudo apt-get update
sudo apt-get install -y dotnet-runtime-8.0

# configure assimp
sudo apt-get install libassimp-dev
