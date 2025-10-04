@echo Propagating default assets...

for /D %%f in (
    Nu\Nu.Pipe
    Nu\Nu.Template.ImSim.Empty
    Nu\Nu.Template.ImSim.Game
    Nu\Nu.Template.Mmcc.Empty
    Nu\Nu.Template.Mmcc.Game
    Nu\Nu.Tests
    Projects\*) do (
    @rem Nonexistent directories will fail the command but will still proceed to the next one.
    del "%%f\Assets\Default\*" /Q
    copy Nu\Nu.Gaia\Assets\Default\*, "%%f\Assets\Default" /Y
)

@pause