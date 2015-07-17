ECHO parameter=%1
CD %1
COPY "TabularExcelCLI.exe"  "notilmerged.exe"
REM ..\..\ILMerge.exe   /targetplatform:"v4,C:\Windows\Microsoft.NET\Framework\v4.0.30319"  /out:"TabularExcelCLImerged.exe" "notilmerged.exe" "nunit.framework.dll"  "TaskPaneScreen.dll" "TaskPaneModel.dll" "TaskPane.dll" "TabularExcelTaskPaneWindows.dll"
..\..\ILRepack.exe   /targetplatform:"v4"  /out:"TabularExcelCLI_packed.exe" "notilmerged.exe" "nunit.framework.dll"  "TaskPaneScreen.dll" "TaskPaneModel.dll" "TaskPane.dll" "TabularExcelTaskPaneWindows.dll"

