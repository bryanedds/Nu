@echo Propagating default assets...

for /D %%f in (
    Nu\Nu.Pipe
    Nu\Nu.Template.ImSim.Empty
    Nu\Nu.Template.ImSim.Game
    Nu\Nu.Template.Mmcc.Empty
    Nu\Nu.Template.Mmcc.Game
    Nu\Nu.Tests
    Projects\*
) do (
    if exist "%%f\Assets\" (
        del "%%f\Assets\Default\*" /Q 2>nul
        mkdir "%%f\Assets\Default\"
        copy Nu\Nu.Gaia\Assets\Default\* "%%f\Assets\Default\" /Y
    )
)

@pause