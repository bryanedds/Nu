# configure Linux for Nu application
echo Configuring Linux for Nu application...

# configure apt and apt-get
sudo apt update
sudo apt-get update

# configure x11 (this seems to be needed when running just Wayland due to issue #810)
sudo apt-get install libx11-dev

# configure .net
# NOTE: there might be a way to install dotnet for deployment only without the full SDK we could use instead.
sudo apt-get install -y dotnet-sdk-10.0

# configure assimp
sudo apt-get install libassimp-dev
