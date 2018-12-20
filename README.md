# Core Tabular
This is the source code for the Core Tabular command-line compiler, tc.exe.

## License

See file LICENSE.txt.

## Requirements:

 1. Infer.NET v2.6

 CoreTabular uses Infer.NET as a backend. 

 2. Visual Studio 2013 (Optional)

## Building:

In Visual Studio 2013:
  1. Open the Tabular.sln file in VS 2013.
  2. In Solution Explorer window, right-click on the solution and "Enable nuget package restore".
  3. Click Build.

Or just type msbuild from the command-line.

## Documentation:
 
tc.exe gives command line options.
tc.exe --help gives a summary of tabular syntax.

Models may be written as comma-separated .csv files or tab-separated .txt files.
The latter have the advantage that fields containing "," need not be wrapped in quotes ("...").

## Samples:

The project comes with two sample applications in folders Samples\TrueSkill and
Samples\Faithful.  Each contains a script run.bat that invokes the
Tabular compiler tc.exe to compile the model (from Tabular to
Infer.NET) and run inference.
  
## Building on Unix-like systems

Building under recent Ubuntu with Mono and optional MonoDevelop, goes something like this:

```
cd CoreTabular 
#get MS VB dll
sudo apt-get install libmono-microsoft-visualbasic10.0-cil 
sudo apt-get install nuget
nuget restore
chmod a+x packages/FsLexYacc.6.1.0/build/fslex.exe 
chmod a+x packages/FsLexYacc.6.1.0/build/fsyacc.exe 
```
From the command-line

```
xbuild /t:build /p:Configuration=Debug /p:Platform="Any CPU" 
```
Or use the MonoDevelop IDE.