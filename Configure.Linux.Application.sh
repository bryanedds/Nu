# configure Linux for Nu application
echo Configuring Linux for Nu application...

# configure apt and apt-get
sudo apt update
sudo apt-get update

# configure x11 (this seems to be needed when running just Wayland due to issue #810)
sudo apt-get install libx11-dev

# configure SDL3 and extension libraries
sudo apt install -y \
  libsdl3-dev \
  libsdl3-image-dev \
  libsdl3-mixer-dev \
  libsdl3-ttf-dev

# configure SDL3 image format dependencies
sudo apt install -y \
  libjpeg-dev \
  libwebp-dev \
  libtiff5-dev

# configure SDL3 audio format dependencies
sudo apt install -y \
  libogg-dev \
  libvorbis-dev \
  libflac-dev \
  libopus-dev \
  libmpg123-dev \
  libfluidsynth-dev

# configure .net
# NOTE: there might be a way to install dotnet for deployment only without the full SDK we could use instead.
sudo apt-get install -y dotnet-sdk-10.0

# configure assimp
sudo apt-get install libassimp-dev
