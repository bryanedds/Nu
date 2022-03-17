@echo Attempting to restore dependencies...
"C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/MSBuild/Current/Bin/MSBuild.exe" Nu.sln /t:restore

@echo Attempting to build...
"C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/MSBuild/Current/Bin/MSBuild.exe" Nu.sln /t:build /property:Configuration=Release

@pause