# configure Linux for Nu development
echo Configuring Linux for Nu development...

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

# configure SDL3 font dependencies
sudo apt install -y libfreetype6-dev

# configure .net
sudo apt-get install -y dotnet-sdk-10.0

# configure assimp
sudo apt-get install libassimp-dev

# configure F#
sudo apt install fsharp

# configure code
sudo snap install code --classic
