namespace MicrosoftResearch.Infer.Tabular.DataLayer
open Microsoft.Office.Interop.Excel
open Microsoft.Office.Interop
open MicrosoftResearch.Infer.Tabular
open System.Linq
open System.Data.OleDb
open System
open ADODB
open System.Data.SqlClient


[<AutoOpen>]
module ExcelNames =
    
    let fold2D f firstVal arr = 
        let mutable accum = firstVal
        for i = (Array2D.base1 arr) to ((Array2D.base1 arr) + Array2D.length1 arr - 1) do
            for j = (Array2D.base2 arr) to ((Array2D.base2 arr) + Array2D.length2 arr - 1) do
                accum <- f accum arr.[i,j]
        accum
    let numberToLetter i = 
        if i < 1 then invalidArg "i" "cells count from 1"
        if i > 16384 then invalidArg "i" "max column is XFD=16384"
        let help num = string(char((int('A')+num)))
        let rec doConv i = 
            match i with 
            |_ when i >= 676 -> (help (i/676-1) + doConv (i%676))
            |_ when i >= 26 -> (help (i/26-1) + doConv (i%26))
            |_ -> help i
        doConv (i-1)
    /// convert Column,Row to Excel format
    let cte (col,row:int) = 
        numberToLetter col + string row

    // type extension
    type Range with
        /// select a sub-range from withing this range
        member rng.selectSubRng(?topcol, ?toprow, ?botcol, ?botrow) = 
            let topcol0 = defaultArg topcol 1
            let toprow0 = defaultArg toprow 1
            let botcol0 = defaultArg botcol rng.Columns.Count
            let botrow0 = defaultArg botrow rng.Rows.Count
            rng.Range(cte (topcol0,toprow0), cte (botcol0,botrow0))

        /// sequence of cells in range
        member rng.toSeq = 
            seq {
                for r in 1 .. rng.Rows.Count do
                    for c in 1 .. rng.Columns.Count do
                        yield rng.Item(r,c) :?> Range
            }

        /// does a Range contain no values?
        member rng.allBlank = Seq.forall (fun (r:Range) -> r.Value2 = null) (rng.toSeq)

    
    let outputConfusionMatrixAndGraph cm (ws:Worksheet) (labels0: _[]) threshold =
        let labels = Array.map (fun x -> x.ToString()) labels0
        let labellen = Array.length labels
        let cmDim = Array2D.length1 cm
        if labellen <> cmDim then invalidArg "labels" "should be the same length as cm"
        ws.Range(cte (1,cmDim+3), cte (1,cmDim+3)).Value2 <- "threshold"
        ws.Range(cte (2,cmDim+3), cte (2,cmDim+3)).Value2 <- threshold
        let allRng = ws.Range(cte (1,1), cte (cmDim+2,cmDim+2) )//.Offset(10,0)
        do
            let trueHeaderRng = allRng.selectSubRng(topcol=3, botrow=1)
            trueHeaderRng.MergeCells <- true
            trueHeaderRng.HorizontalAlignment <- Constants.xlCenter
            trueHeaderRng.Value2 <- "Truth"
        do
            let predictedHeaderRng = allRng.selectSubRng(toprow=3,botcol=1)
            predictedHeaderRng.MergeCells <- true
            predictedHeaderRng.HorizontalAlignment <- Constants.xlCenter
            predictedHeaderRng.WrapText <- true
            predictedHeaderRng.VerticalAlignment <- Constants.xlCenter
            predictedHeaderRng.Value2 <- "Predict"

        let tableRng = allRng.selectSubRng(toprow=2,topcol=2)
        //let labels = Array.init cmDim (fun i -> "l"+(string i)) // replace with actual label values later
        do
            let trueLabels = Array2D.init 1 cmDim (fun _ j -> labels.[j])
            let r = tableRng.selectSubRng(topcol=2, botrow=1)
//            for i = 1 to (cmDim-1) do
//                r.selectSubRng(topcol=i,botcol=i).Value2 <- labels.[i]
            r.Value2 <- trueLabels
        do
            let predictLabels = Array2D.init cmDim 1 (fun i _ -> labels.[i])
            let r = tableRng.selectSubRng(toprow=2,botcol=1)
            r.Value2 <- predictLabels

        let cmRng = tableRng.selectSubRng(toprow=2,topcol=2)
        cmRng.Value2 <- cm

        let cos = ws.ChartObjects() :?> ChartObjects
        let co = 
            let left = (tableRng.Left :?> float)+(tableRng.Width :?> float)
            let top = tableRng.Top :?> float
            cos.Add(left, top, 350.0, 300.0)
        //co.Delete()
        co.Chart.ChartWizard(Source=tableRng,
                                Gallery=XlChartType.xl3DColumn,
                                Format=7,
                                SeriesLabels=1,
                                CategoryLabels=1,
                                CategoryTitle="Predicted Label",
                                ValueTitle="Counts",
                                ExtraTitle="True Label",
                                Title="Confusion Matrix",
                                HasLegend=false
                                )
        ()


    
                

    let mutate f x = f x; x
    let start (tName:string) arg = tName.StartsWith(arg , StringComparison.CurrentCultureIgnoreCase)
    let integers = Seq.unfold(fun i -> Some(i, i+1)) 0
 
    let local (ws:Worksheet) name = ws.Name + "!" + name
 
    ///test wether a name exists on the sheet and erases it if it exists but is invalid
    let (|ValidNamedRangeExists|_|) (ws:Worksheet) sName  = 
       if (ws.Names.Cast<Name>() |> Seq.exists (fun s -> start s.Name (local ws sName))) then
          if (ws.Names.[sName].RefersTo :?> string).Contains("#REF!") then
             ws.Names.[sName].Delete()
             None
          else
             Some(ws.Names.[sName])
       else 
          None
 
    let eraseTag (ws:Worksheet)  sName  = 
       if (ws.Names.Cast<Name>() |> Seq.exists (fun s -> start s.Name (local ws sName))) then
          ws.Names.[sName].Delete()
 
    ///ensures the existence of a named range refering to a valid position
    let ensureTag (ws:Worksheet) (defaultPos:int*int) sName  = 
       match sName with 
       | ((ValidNamedRangeExists ws name)) -> name.RefersToRange
       | _ -> ws.Names.Add(sName, ws.Cells.[fst defaultPos, snd defaultPos]).RefersToRange
 
    let ensureTag2D (ws:Worksheet) (defaultPosUpperLeft:int*int,defaultPosLowerRight:int*int) sName  = 
       match sName with 
       | ((ValidNamedRangeExists ws name)) -> name.RefersToRange
       | _ -> ws.Names.Add(sName, ws.Range(ws.Cells.[fst defaultPosUpperLeft , snd defaultPosUpperLeft ], 
                                           ws.Cells.[fst defaultPosLowerRight, snd defaultPosLowerRight] )).RefersToRange
 
    let ensureSheet (wb:Workbook) (sheetName) = 
       match wb.Worksheets.Cast<Worksheet>() |> Seq.tryFind(fun e -> String.Compare(e.Name, sheetName, StringComparison.CurrentCultureIgnoreCase) = 0
                                                                   ||String.Compare(e.Name, sheetName.Replace(' ','_'), StringComparison.CurrentCultureIgnoreCase) = 0) with
       | Some ws -> ws
       | None ->  wb.Worksheets.Add() :?> Worksheet |> mutate (fun s -> s.Name <- (sheetName.Replace(' ','_')))
 
    let GetWorkbook(filePath) = Excel.ApplicationClass().Workbooks.Open(filePath)
 
    let ExcelTypeToClrType(arg : Excel.XlParameterDataType ) = 
       match arg with
             | Excel.XlParameterDataType.xlParamTypeBigInt -> typeof<Int32>
             | Excel.XlParameterDataType.xlParamTypeBinary -> typeof<Boolean>
                //case Excel.XlParameterDataType.xlParamTypeBit:return typeof ()
                //case Excel.XlParameterDataType.xlParamTypeChar:return typeof (Boolean)
             |  Excel.XlParameterDataType.xlParamTypeDate -> typeof<DateTime>
                //case Excel.XlParameterDataType.xlParamTypeDecimal:return typeof (Boolean);
             |  Excel.XlParameterDataType.xlParamTypeDouble -> typeof<double>
             |  Excel.XlParameterDataType.xlParamTypeFloat -> typeof<Single>
             |  Excel.XlParameterDataType.xlParamTypeInteger -> typeof<Int32>
                //case Excel.XlParameterDataType.xlParamTypeLongVarBinary:return typeof (Boolean);
                //case Excel.XlParameterDataType.xlParamTypeLongVarChar:return typeof (Boolean);
             |  Excel.XlParameterDataType.xlParamTypeNumeric -> typeof<double>
             |  Excel.XlParameterDataType.xlParamTypeReal -> typeof<double>
             |  Excel.XlParameterDataType.xlParamTypeSmallInt -> typeof<Int32>
             |  Excel.XlParameterDataType.xlParamTypeTime -> typeof<DateTime>
             |  Excel.XlParameterDataType.xlParamTypeTimestamp -> typeof<DateTime>
             |  Excel.XlParameterDataType.xlParamTypeTinyInt -> typeof<Int32>
                //case Excel.XlParameterDataType.xlParamTypeUnknown:return typeof (Boolean);
                //case Excel.XlParameterDataType.xlParamTypeVarBinary:return typeof (Boolean);
                //case Excel.XlParameterDataType.xlParamTypeVarChar:return typeof (Boolean);
             |  Excel.XlParameterDataType.xlParamTypeWChar -> typeof<string>
             | _ -> failwith ("not supported XlParameterDataType Enumeration " + arg.ToString())
 
    let ADOIntToXlParameterDataType(arg:DataTypeEnum) = 
       match arg with 
          //case  DataTypeEnum.adError           :  return
          |  DataTypeEnum.adBigInt          ->  Excel.XlParameterDataType.xlParamTypeBigInt
          |  DataTypeEnum.adBinary            -> Excel.XlParameterDataType.xlParamTypeBinary
          |  DataTypeEnum.adBoolean         -> Excel.XlParameterDataType.xlParamTypeBinary
          |  DataTypeEnum.adBSTR            -> Excel.XlParameterDataType.xlParamTypeWChar
          //case  DataTypeEnum.adChapter         :  return
          |  DataTypeEnum.adChar            -> Excel.XlParameterDataType.xlParamTypeWChar
          //case  DataTypeEnum.adCurrency        :  ->    
          |  DataTypeEnum.adDate            -> Excel.XlParameterDataType.xlParamTypeDate
          //case  DataTypeEnum.adDBDate          :  ->    
          //case  DataTypeEnum.adDBTime          :  ->    
          //case  DataTypeEnum.adDBTimeStamp     :  ->    
          |  DataTypeEnum.adDecimal         -> Excel.XlParameterDataType.xlParamTypeFloat
          |  DataTypeEnum.adDouble          -> Excel.XlParameterDataType.xlParamTypeFloat
          //case  DataTypeEnum.adEmpty           :  ->    
          //case  DataTypeEnum.adError           :  ->    
          |  DataTypeEnum.adFileTime        -> Excel.XlParameterDataType.xlParamTypeTime
          |  DataTypeEnum.adGUID            -> Excel.XlParameterDataType.xlParamTypeBigInt
          //| DataTypeEnum.adIDispatch       :  ->    
          |  DataTypeEnum.adInteger         -> Excel.XlParameterDataType.xlParamTypeBigInt
          //| DataTypeEnum.adIUnknown        :  ->    
          | DataTypeEnum.adLongVarBinary   -> Excel.XlParameterDataType.xlParamTypeBinary
          | DataTypeEnum.adLongVarChar     -> Excel.XlParameterDataType.xlParamTypeWChar
          | DataTypeEnum.adLongVarWChar    -> Excel.XlParameterDataType.xlParamTypeWChar
          | DataTypeEnum.adNumeric         -> Excel.XlParameterDataType.xlParamTypeWChar
          //| DataTypeEnum.adPropVariant     :  ->    
          | DataTypeEnum.adSingle          -> Excel.XlParameterDataType.xlParamTypeFloat    // decimal number
          | DataTypeEnum.adSmallInt        -> Excel.XlParameterDataType.xlParamTypeBigInt
          | DataTypeEnum.adTinyInt         -> Excel.XlParameterDataType.xlParamTypeBigInt
          | DataTypeEnum.adUnsignedBigInt  -> Excel.XlParameterDataType.xlParamTypeBigInt
          | DataTypeEnum.adUnsignedInt     -> Excel.XlParameterDataType.xlParamTypeBigInt
          | DataTypeEnum.adUnsignedSmallInt-> Excel.XlParameterDataType.xlParamTypeBigInt
          | DataTypeEnum.adUnsignedTinyInt -> Excel.XlParameterDataType.xlParamTypeBigInt
          //| DataTypeEnum.adUserDefined     :  ->    
          | DataTypeEnum.adVarBinary       -> Excel.XlParameterDataType.xlParamTypeBinary
          | DataTypeEnum.adVarChar         -> Excel.XlParameterDataType.xlParamTypeWChar
          //| DataTypeEnum.adVariant         :  ->    
          | DataTypeEnum.adVarNumeric      ->   Excel.XlParameterDataType.xlParamTypeFloat
          | DataTypeEnum.adVarWChar        -> Excel.XlParameterDataType.xlParamTypeWChar
          | DataTypeEnum.adWChar           -> Excel.XlParameterDataType.xlParamTypeWChar
          |  _ -> failwith("not supported DataTypeEnum Enumeration " + arg.ToString())



[<AutoOpen>]
module ExcelExtensions = 
    open Syntax
    open ExcelNames
    open System.Collections
    
    let ps = System.Data.Entity.Design.PluralizationServices.PluralizationService.CreateService(System.Globalization.CultureInfo.GetCultureInfo("en-us"));
    let openPPModel= 
         let memo = ref List<string>.Empty
         fun (excel:Application, wbName)  ->
            let addins = excel.COMAddIns   
            let mutable PPConnected = false       
            for i in 1..addins.Count do
              let mutable l = box i 
              let addin = addins.[&l] 
              if addin.Description.Contains("PowerPivot") && addin.Connect then PPConnected <- true
            if !memo |> List.exists((=) wbName) then ()
            else
               if PPConnected then ["%";"B";"M"] |> List.iter(fun k -> excel.SendKeys(k))
               memo  := wbName::!memo 

    let rtuple a b = b, a 
    let coerce (v:'a) = System.Convert.ChangeType(v, (typeof<'b>)) :?> 'b


    let getRelations(wb : Excel.Workbook) =
        let model = wb.Model; 
        model.ModelRelationships.Cast<Excel.ModelRelationship>().ToArray();

    let getTables(wb : Excel.Workbook) = 
         let tables = wb.Model.ModelTables.Cast<Excel.ModelTable>().ToArray();
         [| for e in tables -> 
             e.Name, e.ModelTableColumns.Cast<Excel.ModelTableColumn>().Select(fun (c:ModelTableColumn) -> c.Name, c.DataType) |]
    let getDBGraph wb = 
        //let r =  getRelations wb 
        let relations = [for e in getRelations wb do 
                          yield (e.ForeignKeyTable.Name, e.ForeignKeyColumn.Name), (e.PrimaryKeyTable.Name, e.PrimaryKeyColumn.Name) ]
        let tables =    [for (tName, tColumns) in getTables wb do 
                          yield tName, [for (fName, fIntType) in tColumns ->  fName, ExcelTypeToClrType (ADOIntToXlParameterDataType (box fIntType :?>_))]]
        ToDBEntityGraph(relations, tables)



    let getSchema wb (getID : string -> IdStrategy) fUniqueValues fHasNoEmptyValue =
        let DBGraph = getDBGraph wb
        let a = [ for (tableName, table)  in DBGraph.Vertices do
                    let idStrategy = getID tableName 
                    let oid = match idStrategy with | FromColumn n -> Some n | FromPosition -> None
                    yield Declaration(Table(tableName, Some idStrategy), table.GetNewTable(oid, fUniqueValues tableName, fHasNoEmptyValue tableName)) ] 
        a

