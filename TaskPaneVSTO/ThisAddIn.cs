using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using Microsoft.Office.Tools.Excel;
using Microsoft.FSharp.Core;

namespace TaskPaneVSTO
{
    public partial class ThisAddIn
    {
       MicrosoftResearch.Infer.Tabular.TaskPane.MyUserControl taskPaneControl1 = null;
       Microsoft.Office.Tools.CustomTaskPane taskPaneValue = null;

        private void ThisAddIn_Startup(object sender, System.EventArgs e)
        {
           //var r = Globals.ThisAddIn.CustomTaskPanes.Add(new TaskPane.MyUserControl(new FSharpOption<Excel.Application>(Globals.ThisAddIn.Application)), "tabular");
           //r.Visible = true;
           taskPaneControl1 = new MicrosoftResearch.Infer.Tabular.TaskPane.MyUserControl(new FSharpOption<Excel.Application>(Globals.ThisAddIn.Application));
           taskPaneValue = Globals.ThisAddIn.CustomTaskPanes.Add(taskPaneControl1, "TabularVSTO");

           taskPaneValue.VisibleChanged += new EventHandler(taskPaneValue_VisibleChanged);
        }
        private void taskPaneValue_VisibleChanged(object sender, System.EventArgs e)
        {
           Globals.Ribbons.Ribbon1.toggleButton1.Checked = taskPaneValue.Visible;
        }
        public Microsoft.Office.Tools.CustomTaskPane TaskPane
        {
           get
           {
              return taskPaneValue;
           }
        }

        private void ThisAddIn_Shutdown(object sender, System.EventArgs e)
        {
        }

        #region VSTO generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InternalStartup()
        {
            this.Startup += new System.EventHandler(ThisAddIn_Startup);
            this.Shutdown += new System.EventHandler(ThisAddIn_Shutdown);
        }
        
        #endregion
    }
}
