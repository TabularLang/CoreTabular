using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration.Install;
using System.Linq;
using System.Threading.Tasks;
using System.Runtime.InteropServices;
using Microsoft.Win32;

namespace TaskPaneUDF
{
   [RunInstaller(true)]
   public partial class Installer : System.Configuration.Install.Installer
   {
      static string NAME = "TabularVSTOUDF";
      public Installer()
      {
         InitializeComponent();
      }

      public override void Install(System.Collections.IDictionary savedState)
      {
         base.Install(savedState);

         var assembly = typeof(TaskPaneUDF.Udf).Assembly;
         var registration = new RegistrationServices();



         bool success = registration.RegisterAssembly(assembly,AssemblyRegistrationFlags.SetCodeBase);

         try
         {

            var key = Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\15.0\Excel\Options", true);
            if (key != null)
            {
               int openCnt = 0;
               foreach (string optionKeyName in key.GetValueNames())
                  if (optionKeyName.StartsWith("OPEN"))
                     openCnt++;
               // Add the open key
               key.SetValue("OPEN" + (openCnt == 0 ? "" : openCnt.ToString()), "/A "+NAME);
            }
         }
         catch (Exception) { }

         if (!success)
            throw new InstallException("Failed to register for COM");

      }

      public override void Uninstall(System.Collections.IDictionary savedState)
      {
         //MessageBox.Show("1");
         var assembly = typeof(TaskPaneUDF.Udf).Assembly;
         var registration = new RegistrationServices();

         bool success = registration.UnregisterAssembly(assembly);
         //MessageBox.Show("2");


         var key = Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\15.0\Excel\Options", true);
         if (key != null) { 
         foreach (string valueName in key.GetValueNames())
         {
            if (valueName.StartsWith("OPEN"))
            {
               if (key.GetValue(valueName).ToString().Contains(NAME))
               {
                  try
                  {
                     key.DeleteValue(valueName);
                  }
                  catch { }
               }
            }
         }
         }
         if (!success)
            throw new InstallException("Failure to unregister COM");


         base.Uninstall(savedState);
      }
   }
}
