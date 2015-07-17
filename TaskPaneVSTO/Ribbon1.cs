using System;
using System.Collections.Generic;
using System.Xml.Linq;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using Microsoft.Office.Tools.Excel;
using Microsoft.FSharp.Core;
using Microsoft.Office.Tools.Ribbon;

namespace TaskPaneVSTO
{
   public partial class Ribbon1
   {
      
      
      private void Ribbon1_Load(object sender, RibbonUIEventArgs e)
      {
      }


      private void toggleButton1_Click(object sender, RibbonControlEventArgs e)
      {
         Globals.ThisAddIn.TaskPane.Visible = ((RibbonToggleButton)sender).Checked;
      }
   }
}
