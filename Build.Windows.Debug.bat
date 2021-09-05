@echo Attempting to restore dependencies (this doesn't seem to work)...
"C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/MSBuild/Current/Bin/MSBuild.exe" Nu.sln /t:restore

@echo Attempting to build (will work if dependencies are restored)...
"C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/MSBuild/Current/Bin/MSBuild.exe" Nu.sln /t:build /property:Configuration=Debug

@pause