echo "Propagating default assets..."

for f in \
    Nu/Nu.Pipe \
    Nu/Nu.Template.ImSim.Empty \
    Nu/Nu.Template.ImSim.Game \
    Nu/Nu.Template.Mmcc.Empty \
    Nu/Nu.Template.Mmcc.Game \
    Nu/Nu.Tests \
    Projects/*; do
    if [ -d "$f/Assets" ]; then
        rm -rf "$f/Assets/Default/*"
        mkdir -p "$f/Assets/Default/"
        cp -rf Nu/Nu.Gaia/Assets/Default/. "$f/Assets/Default/"
    fi
done