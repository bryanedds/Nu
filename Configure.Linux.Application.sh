# configure Linux for Nu application
echo Configuring Linux for Nu application...

# configure apt
sudo apt update

#configure x11 (this seems to be needed when running just Wayland due to issue #810)
sudo apt-get install libx11-dev

# configure sdl2 and components
sudo apt install \
  libsdl2-dev libsdl2-2.0-0 \
  libjpeg-dev libwebp-dev libtiff5-dev libsdl2-image-dev libsdl2-image-2.0-0 \
  libmikmod-dev libfishsound1-dev libsmpeg-dev liboggz2-dev libflac-dev libfluidsynth-dev libsdl2-mixer-dev libsdl2-mixer-2.0-0 \
  libfreetype6-dev libsdl2-ttf-dev libsdl2-ttf-2.0-0

# configure .net
# if this reports that the package can't be found, try the advice here - https://dev.to/solrevdev/unable-to-locate-package-dotnet-sdk-3-1-4b6
sudo apt-get update
sudo apt-get install -y dotnet-runtime-8.0

# configure assimp
sudo apt-get install libassimp-dev
