# configure Ubuntu for Nu development
echo Configuring Ubuntu for Nu development...

# configure sdl2 and components
sudo apt update
sudo apt install \
  libsdl2-dev libsdl2-2.0-0 \
  libjpeg-dev libwebp-dev libtiff5-dev libsdl2-image-dev libsdl2-image-2.0-0 \
  libmikmod-dev libfishsound1-dev libsmpeg-dev liboggz2-dev libflac-dev libfluidsynth-dev libsdl2-mixer-dev libsdl2-mixer-2.0-0 \
  libfreetype6-dev libsdl2-ttf-dev libsdl2-ttf-2.0-0

# configure mono
sudo apt install gnupg ca-certificates
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb https://download.mono-project.com/repo/ubuntu stable-focal main" | sudo tee /etc/apt/sources.list.d/mono-official-stable.list
sudo apt update
sudo apt install mono-devel

# configure win forms
sudo apt get canberra-gtk-module

# configure F#
sudo apt install fsharp

# configure code
sudo snap install code --classic
