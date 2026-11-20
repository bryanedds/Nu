dotnet publish -c Release -r win-x64 -p PublishSingleFile=true -p IncludeNativeLibrariesForSelfExtract=true -p PublishDir=./bin/Release/net11.0/win-x64 --self-contained true
cd ./bin/Release/net11.0/win-x64
for %%f in (*.dll) do (
    if /i not "%%f"=="assimp.dll" (
    if /i not "%%f"=="zlib1.dll" (
    if /i not "%%f"=="libbulletc.dll" (
    del "%%f"))))
del *.pdb
del *.so
del *.xml
pause
