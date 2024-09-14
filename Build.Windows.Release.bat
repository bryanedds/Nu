@echo Attempting to restore dependencies...
"C:/Program Files/Microsoft Visual Studio/2022/Community/MSBuild/Current/Bin/MSBuild.exe" Nu.sln /t:restore

@echo Attempting to build...
"C:/Program Files/Microsoft Visual Studio/2022/Community/MSBuild/Current/Bin/MSBuild.exe" Nu.sln /t:build /property:Configuration=Release

@pause
