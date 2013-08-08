@echo off

echo Deleting distribution...
rd Aml /s /q
del Aml.zip /q

echo Copying documentation...
md Aml\Documentation
copy ..\Documentation, Aml\Documentation

echo Copying repl (debug)...
md Aml\Bin\AmlRepl\AmlReplDebug
copy ..\AmlRepl\AmlRepl\bin\Debug, Aml\Bin\AmlRepl\AmlReplDebug

echo Copying repl (release)...
md Aml\Bin\AmlRepl\AmlReplRelease
copy ..\AmlRepl\AmlRepl\bin\Release, Aml\Bin\AmlRepl\AmlReplRelease

echo Copying repl (optimized)...
md Aml\Bin\AmlRepl\AmlReplOptimized
copy ..\AmlRepl\AmlRepl\bin\Release, Aml\Bin\AmlRepl\AmlReplOptimized

echo Copying stdlib...
md Aml\Bin\Stdlib
copy ..\AmlRepl\AmlRepl\Stdlib, Aml\Bin\Stdlib

echo Copying plugins...
md Aml\Plugins\TextPad
copy ..\Plugins\TextPad, Aml\Plugins\TextPad

echo Finished.

pause