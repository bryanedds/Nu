# configure Linux for Nu development
echo Configuring Linux for Nu development...

# configure apt and apt-get
sudo apt update
sudo apt-get update

# configure x11 (this seems to be needed when running just Wayland due to issue #810)
sudo apt-get install libx11-dev

# configure .net
sudo apt-get install -y dotnet-sdk-10.0

# configure assimp
sudo apt-get install libassimp-dev

# configure F#
sudo apt install fsharp

# configure code
sudo snap install code --classic
