echo "Propagating default assets..."

for f in \
    Nu/Nu.Pipe \
    Nu/Nu.Template.ImSim.Empty \
    Nu/Nu.Template.ImSim.Game \
    Nu/Nu.Template.Mmcc.Empty \
    Nu/Nu.Template.Mmcc.Game \
    Nu/Nu.Tests \
    Projects/*; do
    # Nonexistent directories will fail the command but will still proceed to the next one.
    rm -f "$f/Assets/Default/*"
    cp -f Nu/Nu.Gaia/Assets/Default/* "$f/Assets/Default/"
done