
#I @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Visual Studio Tools for Office\PIA\Office15\"
#r @"Microsoft.Office.Interop.Excel.dll"
open Microsoft.Office.Interop

#r "bin\Debug\QuickGraph.dll"
//#r "bin\Debug\QuickGraph.Data.dll"
//#r "bin\Debug\QuickGraph.Graphviz.dll"
//#r "bin\Debug\QuickGraph.Serialization.dll"
//#r "bin\Debug\System.Reactive.Core.dll"
//#r "bin\Debug\System.Reactive.Interfaces.dll"
#r "bin\Debug\Tabular.dll"
#r "bin\Debug\DataLayerModel.dll"
#r "bin\Debug\DataLayerExecution.dll"
//#r @"..\DataLayerModel\bin\Debug\DataLayerModel.dll"
#r "bin\Debug\TabularExecution.dll"


open QuickGraph
//open System.Diagnostics;
//open System.Collections.Generic
//open System.Collections.Specialized
//open System.Collections.ObjectModel
//open System.ComponentModel
open MicrosoftResearch.Infer.Tabular.DataLayer.ExcelExtensions
open MicrosoftResearch.Infer.Tabular.Service
open MicrosoftResearch.Infer.Tabular.DataLayer.NewIO
open MicrosoftResearch.Infer.Tabular.DataLayer.DataModelConnectors
open MicrosoftResearch.Infer.Tabular.DataLayer.DBSchema
open MicrosoftResearch.Infer.Tabular.Syntax
open System

let flip f x y = f y x

let excel = new Excel.ApplicationClass(Visible = true);
let wb = excel.Workbooks.Open(__SOURCE_DIRECTORY__  + @"\modelsearch_dsl_script_test.xlsx")

// start with a DBSchema, a graph where
//  node = Table in DataModel, with columns with a name and a Type 
//  edge = foreign key relationship from the primary key table to the foreign key table (ex/ T_Country -> classifier_table)
let dbgraph = getDBGraph wb
use loader = new DataModelLoader(wb :?> _, false) :> ILoader
let schema = loader.GetSchema()
let dicLog, dicIdStrategy, dicDatas, idToPos = readTable loader schema 

/// need enrichment
type ColumnDataType0 =
    | Bool
    | DateTime
    | Integer
    | Real
    | String
    | Link of TableName

/// convert a system type to mytype.  This function can use the actural table data, but it does not at the moment
/// the Type is one of Int32, Boolean, double, Single, DateTime, string
let convertToColumnDataType0 (sysType:Type) _colData = // we can include the whole dbtable if we need it
    match sysType with
    | x when x = typeof<Int32> -> Integer
    | x when x = typeof<DateTime> -> DateTime
    | x when x = typeof<Boolean> -> Bool
    | x when x = typeof<Single> 
          || x = typeof<double> -> Real
    | x when x = typeof<string> -> String
    | _ -> failwithf "unexpected type %A" sysType

/// The DBTables in topological order
let lTopo = getTablesTopological dbgraph

type ColumnMap = Map<ColumnName,int>
type ColumnTypeMap = Map<ColumnName,(ColumnDataType0*int)>
type TypedTable = ColumnTypeMap * (obj[] seq)
type NamedTypedTable = TableName * TypedTable
//type TypeDTO = Map<TableName, (ColumnTypeMap * (obj[] seq) )>

// Construct a list of the form (TableName * Map<ColumnName, (ColumnDataType0 * seq<obj>) ))
/// maintains topological order
// ((TableName*(ColumnTypeMap*(obj[] seq))) list)
let tableTypeDataListTopo :(NamedTypedTable list) = 
    lTopo 
    |> List.map (fun (tname, dbtable) ->
                    let tableData = dicDatas |> Map.find tname
                    //let colTypeDataMap =
                    let colTypeDataMap :ColumnTypeMap =
                        dbtable.fields.CastSeq() 
                        |> Seq.map (fun kv ->
                                    let columnName, column = kv.Key, kv.Value
                
                                    // first get the index of this column and use it to grab the actual column data for this column only
                                    let colIdx = tableData |> fst |> Map.find columnName
                                    let colData = tableData |> snd |> Seq.map (flip Array.get colIdx) |> Seq.cast<IComparable>

                                    let ty =    match column with
                                                | Local (typ, colName) -> 
                                                    assert(colName.Equals(columnName)) // redundant information
                                                    // convert system data type to my data type
                                                    let mytyp = convertToColumnDataType0 typ colData
                                                    mytyp
                                                    //(columnName:ColumnName), (mytyp, colData)

                                                | Remote pktname -> // actual data are integer row #s: e.g., {0; 0; 2; 0}
                                                    Link pktname
                                                    //(columnName:ColumnName), (Link pktname, colData)
                                    (columnName:ColumnName),(ty,colIdx)
                                   )
                        |> Map.ofSeq
                    
//                    let colmap =
//                        colTypeDataMap
//                        |> Seq.mapi (fun idx (colname, (mytyp,colData)) ->
//                                        (colname, (mytyp, idx))
//                                    )
//                        |> Map.ofSeq :ColumnTypeMap
//
//                    let 

                    //(tname:TableName), colTypeDataMap
                    (tname:TableName), (colTypeDataMap, snd tableData)
                )
    //|> Map.ofList


//type objcomp = {IComparable with this.CompareTo = (=)}

//let normalizeTables (originalTlist:NamedTypedTable list) =
//    
//    let getGoodDet (table:TypedTable) :(ColumnTypeMap*ColumnTypeMap*Map<IComparable [],IComparable []>) =
//        
//        Map.empty, Map.empty, Map.empty
//
//    let splitTableWithDet (detmap:ColumnTypeMap,rngmap:ColumnTypeMap,map:Map<IComparable [],IComparable []>) :(NamedTypedTable*NamedTypedTable) = 
//        failwith ""
//
//    let rec normalizeTablesRec (tablesRemain:NamedTypedTable list) =
//        match tablesRemain with 
//        | [] -> []
//        | table::tail ->
//            match table |> snd |> getGoodDet with
//            | (x,y,z) when Map.isEmpty x && Map.isEmpty y && Map.isEmpty z -> table :: normalizeTablesRec tail
//            | (detmap, rngmap, map) as det -> 
//                let t1, t2 = splitTableWithDet det
//                // do renaming of future tables here
//                t1::t2 :: normalizeTablesRec tail
//
//            
//
//
////    let res = 
////        List.scan
////            (fun accum table ->
////            
////                ()
////            ) () originalTlist
//
//
//
//    normalizeTablesRec originalTlist
//
//let normTlist = normalizeTables tableTypeDataListTopo






//let d = dicDatas |> Map.find "classifier_table"

//type ColumnDataType =
//    | Nominal
//    | Ordinal
//    | Numeric 
//    | Bool

/// function that takes (1) the type from the data model, (2) the actual column data, and returns the new type
//let convert





