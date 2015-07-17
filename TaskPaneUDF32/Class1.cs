using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;
using Excel = Microsoft.Office.Interop.Excel;
using System.Windows.Forms;
using System.Reflection;
using Microsoft.Win32;



namespace TaskPaneUDF32
{

   #region interfaceandclass
   [Guid("CA0CE65E-4D6E-4011-A8CD-421A7CAA24DE")]
   [ComVisible(true)]
   public partial interface IUdf { 
      string   Echo(string s);
      double   Mean(string s);
      object   Method(string dist,string name) ;
      double   Variance(string dist);
      object   Field(string dist,string name);
      object   Property(string dist,string name);
      double   GetLogProb(string dist, int v);
      double[] GetProbs(string dist, int v);
      string   Reflect(string dist);       
      object   GetMode(string s);
      string   ToString(string s);
   }

   //1-verify that after regasm, we can create object in VBA
   //2-verify that after install, we can call functions on the sheet

   //the progid is the name with which we can CREATE AN OBJECT
   //Sub toto()
   //Dim fact
   //Set UDF = CreateObject("TabularVSTOUDF32")
   //r = UDF.GetMode("Discrete.PointMass(3)")
   //MsgBox (r)
   //End Sub
   //http://en.wikipedia.org/wiki/ProgID
   [Guid("0FE9B1F2-0FCA-43C7-BCAA-3279BAEED1E4")]
   [ClassInterface(ClassInterfaceType.None)]
   [ProgId("TabularVSTOUDF32")]
   [ComVisible(true)]
   public class Udf : IUdf, Extensibility.IDTExtensibility2
    {
      public  string Echo(string s)
       {
          return UDFs.Echo(s);
       }

      public  double Mean(string s)
       {
          return UDFs.Mean(s);
       }

      public  object Method(string dist, string name)
       {
          return UDFs.Method(dist,  name);
       }

      public  double Variance(string dist)
       {
          return UDFs.Variance(dist);
       }

      public  object Field(string dist, string name)
       {
          return UDFs.Field(dist, name);
       }

      public  object Property(string dist, string name)
       {
          return UDFs.Property(dist, name);
       }

      public  double GetLogProb(string dist, int v)
       {
          return UDFs.GetLogProb(dist, v);
       }

      public  double[] GetProbs(string dist, int v)
       {
          return UDFs.GetProbs(dist,v);
       }

      public  string Reflect(string dist)
       {
          return UDFs.Reflect(dist);
       }

      public  object GetMode(string s)
       {
          return UDFs.GetMode(s);
       }

      public  string ToString(string s)
       {
          return UDFs.ToString(s);
       }

   #endregion

   #region IDTExtensibility2
       private static Excel.Application Application;
       private static object ThisAddIn;
       private static bool fVstoRegister = false;
       static string NAME = "TabularVSTOUDF32";

       public void OnConnection(object application, Extensibility.ext_ConnectMode connectMode, object addInInst, ref System.Array custom)
       {
          // get a reference to the instance of the add-in
          Application = application as Excel.Application;
          ThisAddIn = addInInst;
       }

       /// <summary>
       /// When we disconnect - remove everything - clean up
       /// </summary>
       /// <param name="disconnectMode"></param>
       /// <param name="custom"></param>
       public void OnDisconnection(Extensibility.ext_DisconnectMode disconnectMode, ref System.Array custom)
       {
          // clean up
          Marshal.ReleaseComObject(Application);
          Application = null;
          ThisAddIn = null;
          GC.Collect();
          GC.Collect();
          GC.WaitForPendingFinalizers();
       }
       // the following functions are required to be defined, but not needed
       public void OnAddInsUpdate(ref System.Array custom) { }
       public void OnStartupComplete(ref System.Array custom) { }
       public void OnBeginShutdown(ref System.Array custom) { }


      ///// <summary>
      ///// Registers the COM Automation Add-in in the CURRENT USER context
      ///// and then registers it in all versions of Excel on the users system
      ///// without the need of administrator permissions
      ///// </summary>
      ///// <param name="type"></param>
      ////[ComRegisterFunctionAttribute]
      //public static void RegisterFunction(Type type)
      //{
      //   string PATH = System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase.Replace("\\", "/");
      //   string ASSM = Assembly.GetExecutingAssembly().FullName;
      //   int startPos = ASSM.ToLower().IndexOf("version=") + "version=".Length;
      //   int len = ASSM.ToLower().IndexOf(",", startPos) - startPos;
      //   string VER = ASSM.Substring(startPos, len);
      //   string GUID = "{" + type.GUID.ToString().ToUpper() + "}";
      //   //NAME = type.Namespace + "." + type.Name; // global
      //   string BASE = @"Classes\" + NAME;
      //   string CLSID = @"Classes\CLSID\" + GUID;

      //   // open the key
      //   RegistryKey CU = Registry.CurrentUser.OpenSubKey("Software", true);

      //   // is this version registred?
      //   RegistryKey key = CU.OpenSubKey(CLSID + @"\InprocServer32\" + VER);
      //   if (key == null)
      //   {
      //      // The version of this class currently being registered DOES NOT
      //      // exist in the registry - so we will now register it

      //      // BASE KEY
      //      // HKEY_CURRENT_USER\CLASSES\{NAME}
      //      key = CU.CreateSubKey(BASE);
      //      key.SetValue("", NAME);

      //      // HKEY_CURRENT_USER\CLASSES\{NAME}\CLSID}
      //      key = CU.CreateSubKey(BASE + @"\CLSID");
      //      key.SetValue("", GUID);

      //      // CLSID
      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}
      //      key = CU.CreateSubKey(CLSID);
      //      key.SetValue("", NAME);

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\Implemented Categories
      //      key = CU.CreateSubKey(CLSID + @"\Implemented Categories").CreateSubKey("{62C8FE65-4EBB-45e7-B440-6E39B2CDBF29}");

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\InProcServer32
      //      key = CU.CreateSubKey(CLSID + @"\InprocServer32");
      //      key.SetValue("", @"mscoree.dll");
      //      key.SetValue("ThreadingModel", "Both");
      //      key.SetValue("Class", NAME);
      //      key.SetValue("CodeBase", PATH);
      //      key.SetValue("Assembly", ASSM);
      //      key.SetValue("RuntimeVersion", "v4.0.30319");

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\InProcServer32\{VERSION}
      //      key = CU.CreateSubKey(CLSID + @"\InprocServer32\" + VER);
      //      key.SetValue("Class", NAME);
      //      key.SetValue("CodeBase", PATH);
      //      key.SetValue("Assembly", ASSM);
      //      key.SetValue("RuntimeVersion", "v4.0.30319");

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\ProgId
      //      key = CU.CreateSubKey(CLSID + @"\ProgId");
      //      key.SetValue("", NAME);

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\Progammable
      //      key = CU.CreateSubKey(CLSID + @"\Programmable");

      //      // now register the addin in the addins sub keys for each version of Office
      //      foreach (string keyName in Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\").GetSubKeyNames())
      //      {
      //         if (IsVersionNum(keyName))
      //         {
      //            // and now set it to a loaded state by adding it to the options key
      //            key = Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\" + keyName + @"\Excel\Options", true);
      //            if (key != null)
      //            {
      //               // loop though all the names and count how many have the name OPEN#
      //               int openCnt = 0;
      //               foreach (string optionKeyName in key.GetValueNames())
      //                  if (optionKeyName.StartsWith("OPEN"))
      //                     openCnt++;
      //               // Add the open key
      //               key.SetValue("OPEN" + (openCnt == 0 ? "" : openCnt.ToString()), "/A " + NAME);
      //            }
      //         }
      //      }
      //      if (!fVstoRegister)
      //      {
      //         // all done - this just helps to assure REGASM is complete
      //         // this is not needed, but is useful for troubleshooting
      //         MessageBox.Show("Registered " + NAME + ".");
      //      }
      //   }
      //}

      //public static void RegisterFunctionOriginal(Type type)
      //{
      //   string PATH = System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase.Replace("\\", "/");
      //   string ASSM = Assembly.GetExecutingAssembly().FullName;
      //   int startPos = ASSM.ToLower().IndexOf("version=") + "version=".Length;
      //   int len = ASSM.ToLower().IndexOf(",", startPos) - startPos;
      //   string VER = ASSM.Substring(startPos, len);
      //   string GUID = "{" + type.GUID.ToString().ToUpper() + "}";
      //   //NAME = type.Namespace + "." + type.Name; // global
      //   string BASE = @"Classes\" + NAME;
      //   string CLSID = @"Classes\CLSID\" + GUID;

      //   // open the key
      //   RegistryKey CU = Registry.CurrentUser.OpenSubKey("Software", true);

      //   // is this version registred?
      //   RegistryKey key = CU.OpenSubKey(CLSID + @"\InprocServer32\" + VER);
      //   if (key == null)
      //   {
      //      // The version of this class currently being registered DOES NOT
      //      // exist in the registry - so we will now register it

      //      // BASE KEY
      //      // HKEY_CURRENT_USER\CLASSES\{NAME}
      //      key = CU.CreateSubKey(BASE);
      //      key.SetValue("", NAME);

      //      // HKEY_CURRENT_USER\CLASSES\{NAME}\CLSID}
      //      key = CU.CreateSubKey(BASE + @"\CLSID");
      //      key.SetValue("", GUID);

      //      // CLSID
      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}
      //      key = CU.CreateSubKey(CLSID);
      //      key.SetValue("", NAME);

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\Implemented Categories
      //      key = CU.CreateSubKey(CLSID + @"\Implemented Categories").CreateSubKey("{62C8FE65-4EBB-45e7-B440-6E39B2CDBF29}");

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\InProcServer32
      //      key = CU.CreateSubKey(CLSID + @"\InprocServer32");
      //      key.SetValue("", @"c:\Windows\SysWow64\mscoree.dll");
      //      key.SetValue("ThreadingModel", "Both");
      //      key.SetValue("Class", NAME);
      //      key.SetValue("CodeBase", PATH);
      //      key.SetValue("Assembly", ASSM);
      //      key.SetValue("RuntimeVersion", "v4.0.30319");

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\InProcServer32\{VERSION}
      //      key = CU.CreateSubKey(CLSID + @"\InprocServer32\" + VER);
      //      key.SetValue("Class", NAME);
      //      key.SetValue("CodeBase", PATH);
      //      key.SetValue("Assembly", ASSM);
      //      key.SetValue("RuntimeVersion", "v4.0.30319");

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\ProgId
      //      key = CU.CreateSubKey(CLSID + @"\ProgId");
      //      key.SetValue("", NAME);

      //      // HKEY_CURRENT_USER\CLASSES\CLSID\{GUID}\Progammable
      //      key = CU.CreateSubKey(CLSID + @"\Programmable");

      //      // now register the addin in the addins sub keys for each version of Office
      //      foreach (string keyName in Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\").GetSubKeyNames())
      //      {
      //         if (IsVersionNum(keyName))
      //         {
      //            // and now set it to a loaded state by adding it to the options key
      //            key = Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\" + keyName + @"\Excel\Options", true);
      //            if (key != null)
      //            {
      //               // loop though all the names and count how many have the name OPEN#
      //               int openCnt = 0;
      //               foreach (string optionKeyName in key.GetValueNames())
      //                  if (optionKeyName.StartsWith("OPEN"))
      //                     openCnt++;
      //               // Add the open key
      //               key.SetValue("OPEN" + (openCnt == 0 ? "" : openCnt.ToString()), "/A " + NAME);
      //            }
      //         }
      //      }
      //      if (!fVstoRegister)
      //      {
      //         // all done - this just helps to assure REGASM is complete
      //         // this is not needed, but is useful for troubleshooting
      //         MessageBox.Show("Registered " + NAME + ".");
      //      }
      //   }
      //}

      ///// <summary>
      ///// Unregisters the add-in, by removing all the keys
      ///// </summary>
      ///// <param name="type"></param>
      ////[ComUnregisterFunctionAttribute]
      //public static void UnregisterFunction(Type type)
      //{
      //   string GUID = "{" + type.GUID.ToString().ToUpper() + "}";
      //   string NAME = type.Namespace + "." + type.Name;
      //   string BASE = @"Classes\" + NAME;
      //   string CLSID = @"Classes\CLSID\" + GUID;
      //   // open the key
      //   RegistryKey CU = Registry.CurrentUser.OpenSubKey("Software", true);
      //   // DELETE BASE KEY
      //   // HKEY_CURRENT_USER\CLASSES\{NAME}
      //   try
      //   {
      //      CU.DeleteSubKeyTree(BASE);
      //   }
      //   catch { }
      //   // HKEY_CURRENT_USER\CLASSES\{NAME}\CLSID}
      //   try
      //   {
      //      CU.DeleteSubKeyTree(CLSID);
      //   }
      //   catch { }
      //   // now un-register the addin in the addins sub keys for Office
      //   // here we just make sure to remove it from allversions of Office
      //   foreach (string keyName in Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\").GetSubKeyNames())
      //   {
      //      if (IsVersionNum(keyName))
      //      {
      //         RegistryKey key = Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\" + keyName + @"\Excel\Add-in Manager", true);
      //         if (key != null)
      //         {
      //            try
      //            {
      //               key.DeleteValue(NAME);
      //            }
      //            catch { }
      //         }
      //         key = Registry.CurrentUser.OpenSubKey(@"Software\Microsoft\Office\" + keyName + @"\Excel\Options", true);
      //         if (key == null)
      //            continue;
      //         foreach (string valueName in key.GetValueNames())
      //         {
      //            if (valueName.StartsWith("OPEN"))
      //            {
      //               if (key.GetValue(valueName).ToString().Contains(NAME))
      //               {
      //                  try
      //                  {
      //                     key.DeleteValue(valueName);
      //                  }
      //                  catch { }
      //               }
      //            }
      //         }
      //      }
      //   }
      //   MessageBox.Show("Unregistered " + NAME + "!");
      //}

      ///// <summary>
      ///// HELPER FUNCTION
      ///// This assists is in determining if the subkey string we are passed
      ///// is of the type like:
      /////     8.0
      /////     11.0
      /////     14.0
      /////     15.0
      ///// </summary>
      ///// <param name="s"></param>
      ///// <returns></returns>
      //public static bool IsVersionNum(string s)
      //{
      //   int idx = s.IndexOf(".");
      //   if (idx >= 0 && s.EndsWith("0") && int.Parse(s.Substring(0, idx)) > 0)
      //      return true;
      //   else
      //      return false;
      //}
      #endregion
    }
}
