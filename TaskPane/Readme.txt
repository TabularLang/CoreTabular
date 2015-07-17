Requirements :  Excel 2013, .NET Framework 4.5

Disclaimer: This is research software and is provided for evaluation purposes only.

Licensing: see file MSR-LA Software - Tabular.rtf

Tabular is built on Infer.NET (http://research.microsoft.com/infernet). 

Tabular includes one open-source software component. To allow users to comply with 
their owns policies for use of open source, we are providing information
about this component, which is provided in binary form in the distribution.

1.Excel-DNA v0.30 (licensed under a custom license (see below)) 
  (binaries bin\TabularTaskPaneDNA-AddIn64.xll and bin\TabularTaskPaneDNA-AddIn32.xll
   are copies of ExcelDNA binaries ExcelDna64.xll and ExcelDna.xll) 


Getting Started:
  Please read 'Getting Started.docx' in this folder.

Launching:

- double click either Tabular 32.bat or Tabular 64.bat depending on whether you have a 32 or 64-bit version of Excel 2013.
- open a sheet and model (see below)
- activate the Tabular task pane (Tabular ribbon -> Model icon)
- select the model you want to apply by activating the sheet named : 'Tabular_NameOfYourModel'. 
  (the 'Tabular_' prefix is required for Tabular to recognize the sheet as containing a model.)


Models: 

- you can open the example spreadsheets in 'Tabular Examples' folder
- 'CheatSheet.xlsx' is a not an example but a reference sheet listing the syntax of Tabular constructs

Data access:

When creating a model from scratch, you need to add your data into the new PowerPivot Data Model of Excel 2013
That involves :
- formatting your dataset as one or more tables (Home -> Format as table)
- adding the tables to the data model  (PowerPivot -> add to Data Model)
- setting up the links between the tables in PowerPivot

See the Getting Started guide for a walkthrough.


Links: 

Please report any bugs and suggestions to:
  tabular-discussions@microsoft.com
You may also subscribe to this distribution group.

Alternatively, contact Andy Gordon (adg@microsoft.com) or Claudio Russo (crusso@microsoft.com).



Excel-DNA Custom License
------------------------

 Copyright (C) 2005-2013 Govert van Drimmelen

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.


  Govert van Drimmelen
  govert@icon.co.za
  



