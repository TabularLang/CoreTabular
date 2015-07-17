@echo off
cls
REM unfotunately, there is no way to specify the "-ExcludeVersion" except by commandd line install
REM cf http://nuget.codeplex.com/workitem/1522
REM so this line has to be run once before compiling
REM This will install FAKE which is needed in 
REM ".nuget\NuGet.exe" "Install" "FAKE" "-OutputDirectory" "packages" "-ExcludeVersion"


REM This is mainly to update the AssemblyInfo's but other Fake task could be added.
REM 
"packages\FAKE\tools\Fake.exe" build.fsx
pause
