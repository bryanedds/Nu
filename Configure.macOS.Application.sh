# configure macOS for Nu application
echo Configuring macOS for Nu application...

# install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
/opt/homebrew/bin/brew install SDL2 SDL2_image SDL2_mixer SDL2_ttf assimp