echo "Propagating default assets..."

rm -f Nu/Nu.Pipe/Assets/Default/*
cp -f Nu/Nu.Gaia/Assets/Default/* Nu/Nu.Pipe/Assets/Default/

rm -f Nu/Nu.Template.ImNui.Empty/Assets/Default/*
cp -f Nu/Nu.Gaia/Assets/Default/* Nu/Nu.Template.ImNui.Empty/Assets/Default/

rm -f Nu/Nu.Template.ImNui.Game/Assets/Default/*
cp -f Nu/Nu.Gaia/Assets/Default/* Nu/Nu.Template.ImNui.Game/Assets/Default/

rm -f Nu/Nu.Template.Mmcc.Empty/Assets/Default/*
cp -f Nu/Nu.Gaia/Assets/Default/* Nu/Nu.Template.Mmcc.Empty/Assets/Default/

rm -f Nu/Nu.Template.Mmcc.Game/Assets/Default/*
cp -f Nu/Nu.Gaia/Assets/Default/* Nu/Nu.Template.Mmcc.Game/Assets/Default/

rm -f Nu/Nu.Tests/Assets/Default/*
cp -f Nu/Nu.Gaia/Assets/Default/* Nu/Nu.Tests/Assets/Default/

rm -f "Projects/Blaze Vector ImNui/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Blaze Vector ImNui/Assets/Default/"

rm -f "Projects/Blaze Vector Mmcc/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Blaze Vector Mmcc/Assets/Default/"

rm -f "Projects/Breakout ImNui/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Breakout ImNui/Assets/Default/"

rm -f "Projects/Breakout Mmcc/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Breakout Mmcc/Assets/Default/"

rm -f "Projects/Metrics/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Metrics/Assets/Default/"

rm -f "Projects/Jump Box/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Jump Box/Assets/Default/"

rm -f "Projects/Nelmish/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Nelmish/Assets/Default/"

rm -f "Projects/Terra Firma/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Terra Firma/Assets/Default/"

rm -f "Projects/Twenty 48/Assets/Default/*"
cp -f Nu/Nu.Gaia/Assets/Default/* "Projects/Twenty 48/Assets/Default/"
