@echo Propagating default assets...

del Nu\Nu.Pipe\Assets\Default\* /Q
copy Nu\Nu.Gaia\Assets\Default\*, Nu\Nu.Pipe\Assets\Default /Y

del Nu\Nu.Template.ImNui.Empty\Assets\Default\* /Q
copy Nu\Nu.Gaia\Assets\Default\*, Nu\Nu.Template.ImNui.Empty\Assets\Default /Y

del Nu\Nu.Template.ImNui.Game\Assets\Default\* /Q
copy Nu\Nu.Gaia\Assets\Default\*, Nu\Nu.Template.ImNui.Game\Assets\Default /Y

del Nu\Nu.Template.Mmcc.Empty\Assets\Default\* /Q
copy Nu\Nu.Gaia\Assets\Default\*, Nu\Nu.Template.Mmcc.Empty\Assets\Default /Y

del Nu\Nu.Template.Mmcc.Game\Assets\Default\* /Q
copy Nu\Nu.Gaia\Assets\Default\*, Nu\Nu.Template.Mmcc.Game\Assets\Default /Y

del Nu\Nu.Tests\Assets\Default\* /Q
copy Nu\Nu.Gaia\Assets\Default\*, Nu\Nu.Tests\Assets\Default /Y

del "Projects\Blaze Vector ImNui\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Blaze Vector ImNui\Assets\Default" /Y

del "Projects\Blaze Vector Mmcc\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Blaze Vector Mmcc\Assets\Default" /Y

del "Projects\Breakout ImNui\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Breakout ImNui\Assets\Default" /Y

del "Projects\Breakout Mmcc\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Breakout Mmcc\Assets\Default" /Y

del "Projects\Jump Box\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Jump Box\Assets\Default" /Y

del "Projects\Metrics\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Metrics\Assets\Default" /Y

del "Projects\Nelmish\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Nelmish\Assets\Default" /Y

del "Projects\Terra Firma\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Terra Firma\Assets\Default" /Y

del "Projects\Twenty 48\Assets\Default\*" /Q
copy Nu\Nu.Gaia\Assets\Default\*, "Projects\Twenty 48\Assets\Default" /Y

@pause
