module MainApp

open System
open System.Windows
open System.Windows.Controls
//open FSharpx
open MicrosoftResearch.Infer.Tabular.Syntax
open MicrosoftResearch.Infer.Tabular.TaskPane
open Microsoft.Office.Interop
open MicrosoftResearch.Infer
open MicrosoftResearch.Infer.Distributions

let isModelSheet (ws:Excel.Worksheet) =  ws.Name.StartsWith(TabularModelSheetNamePrefix,StringComparison.CurrentCultureIgnoreCase)
let delta x = x,x

type Excel.Workbook with
    member wb.ActivateNonTabularModelSheet () = 
        wb.Worksheets.Cast<Excel.Worksheet>() |> Seq.filter (isModelSheet >> not) 
                                                |> Seq.head |> (fun ws -> ws.Activate())
    member wb.TabularModelSheets () = 
        wb.Worksheets.Cast<Excel.Worksheet>() 
        |> Seq.filter isModelSheet

let getMvc (excel:Excel.Application) =
    let model      = TaskPaneModel()
    let view       = new TaskPaneView(TaskPaneIIScreen.TaskPaneControl(), Some excel)
    let controller = new TaskPaneController(Some excel, false)
    let mvc        = FSharp.Windows.Mvc(model, view, controller)
    mvc.Start(), view, controller
   
type ModelsToRun = | All | Sheets of string list

type UIBundle = IDisposable * TaskPaneView * TaskPaneController


open MicrosoftResearch.Infer.Tabular.DataLayer
open MicrosoftResearch.Infer.Tabular
open MicrosoftResearch.Infer.Tabular.DataLayer.NewIO

let loadWindow() =
   let excel = Microsoft.Office.Interop.Excel.ApplicationClass();
   let wb = excel.Workbooks.Open(__SOURCE_DIRECTORY__  + @"\..\TaskPane\data\modelwizard test 2.xlsx")
   //excel.Visible <- true
   // next lines not used
   let model = TaskPaneModel()
   let view = new TaskPaneView(TaskPaneIIScreen.TaskPaneControl(), Some (excel :> _))
   let controller = new TaskPaneController(Some (excel :> _), true)
   let mvc = FSharp.Windows.Mvc(model, view, controller)
   mvc.Start() |> ignore
   // --------------------------------------------------------------------------------------------------------
   
   // DEFINITIONS ------------------------------------
   let sClassifierTable = "classifier_table":TableName
   let getClassifierTableData (DTO(dto)) =
        match dto.TryFind sClassifierTable with
        | Some data -> data
        | None -> failwith "Sorry could not find \"classifier_table\""
   
   
   /// takes the rows of data and the index of the classifier column
   /// yields sequence of triples to use for cross-validation: testRowsCleared:obj[][] * testRowTruths:obj[] * trainRows:obj[][]
   let split (rowData: _ array seq) classcolIdx  = 
       if Seq.length rowData = 0 then failwith "no values to test or train on for validation"

       let randomGen = 
            let randomSeed = 3584134                     // for repeatability
            System.Random(randomSeed)

       // helper function used to partition a sequence
       let splitNumbering labelOp numbering = 
            let (g,h) = Array.partition (snd >> labelOp) numbering
            Array.map fst g, Array.map fst h
       
       /// scheme for doing 80-20 5-fold validation
       let numbering  = Array.init (rowData |> Seq.length)  (fun i -> i, randomGen.Next(0,5))
       let count01234 = Seq.init 5 id

       /// make one triple given the test set marker
       let makeWithIdxI i =
            let (testRowsIndex,trainRowsIndex) = 
                let (testRowsIndex00, trainRowsIndex00) =
                    match splitNumbering ((=) i) numbering with
                    // handle scenario where the test or training set is empty; put at least one value in there
                    | (testRowsIndex0,_) when Array.length testRowsIndex0 = 0 ->
                         let oneIdx = randomGen.Next(0,Seq.length rowData)
                         Array.create 1 oneIdx, Array.init ((Seq.length rowData)-1) (fun idx -> if idx < oneIdx then idx else idx+1)
                    | (_,trainRowsIndex0) when Array.length trainRowsIndex0 = 0 ->
                         let oneIdx = randomGen.Next(0,Seq.length rowData)
                         Array.init ((Seq.length rowData)-1) (fun idx -> if idx < oneIdx then idx else idx+1), Array.create 1 oneIdx
                    | res -> res
                Seq.ofArray testRowsIndex00, Seq.ofArray trainRowsIndex00

            // remove test classification data before inference
            let testRowTruths = testRowsIndex |> Seq.map(fun i -> (Seq.nth i rowData).[classcolIdx])
            let testRowsCleared = testRowsIndex |> Seq.map(fun i -> 
                                                                let arr = Array.copy <| Seq.nth i rowData
                                                                arr.SetValue(null,classcolIdx)
                                                                //(arr:obj array).[classcolIdx] <- null // BAD
                                                                arr
                                                                )
            let trainRows = trainRowsIndex |> Seq.map(fun i -> Seq.nth i rowData)
            testRowsCleared, testRowTruths, trainRows
       Seq.map makeWithIdxI count01234


   // repack the cleared test set + training set into a DTO to pass to Infer.NET
   // second returned value is the position of the first test row
   let createDTOForInference testRowsCleared (trainRows:obj array seq) (originalDTO:Map<TableName, dataNormalized>) = 
       let newRowSeq = Seq.append (trainRows) (testRowsCleared)
       Map.map (fun tableName ((colMapping,_) as dataNorm) -> if String.Equals(tableName,sClassifierTable) then (colMapping,newRowSeq) else dataNorm) originalDTO 
       |> DTO, Seq.length trainRows

   //Map<TableName, Map<ColumnName,int> * (obj array seq)> //row level DTO
   let doPrediction (inputDTO:DTO) (inputSchema:Schema) = //returnedSchema dicReturnedData =
       let (_, predictedPZ, _, _)  as res =
            ExcelCompiler.latentModel("CrossValidationTemp","CrossValidationTemp",inputSchema).performInferenceGeneric(inputDTO,Some(new VariationalMessagePassing():> IAlgorithm), None) |> Async.RunSynchronously
       predictedPZ : DistDTO

   
   let getClasscolFromDTO (dto:DTO) classcolIdx = 
       let data = dto |> getClassifierTableData |> snd
       Seq.map (fun objarr -> (objarr:obj array).[classcolIdx]) data 
   
   /// skip down to the test rows
   let getTrainRowsFromClasscol classcolRows trainRowIdx = 
       Seq.skip trainRowIdx classcolRows

   let convertDistDTOToDTO (DistDTO(ddto)) = DTO(ddto)

   let getproba_modeLoss (pred:Discrete) idx = if pred.GetMode() = idx then 1 else 0
   let multproba_regular = (*)
   let addproba_regular = (+)
   let getdimension_discrete (dist:Discrete) = dist.Dimension

   /// convert array of indexes into point masses, given the total dimension; used for converting the true values
   let makePointMasses vals dim = 
        Seq.map (fun x-> match (x:obj) with
                           | :? int as  i -> Discrete.PointMass(i, dim)
                           | _ -> failwith ("expected int but actually have "+(x.ToString()))
                   ) vals
   
   //let confusionMatrix (predictedvals: obj array) (truevals: obj array) : int array array = 
   let genConfusionMatrix (trueDs:'a seq)(predictedDs:'a seq) getdimension getproba multproba addproba = 
        let dimension = getdimension(trueDs |> Seq.head) 
        let index = Array.init dimension id
        let confusion = Array2D.zeroCreate dimension dimension 
        for (predD, trueD)  in Seq.zip predictedDs trueDs do
            for i in index do
                for j in index do
                    let p1 = getproba predD i
                    let p2 = getproba trueD j
                    let m = multproba(p1)(p2) |> float
                    confusion.[i,j] <- addproba confusion.[i,j]  m
        confusion :float[,] // enforce floating point number type

   


   

   //let testRowsCleared, testRowTruths, trainRows = allRowData |> Seq.toArray |> split 
                                                   //|> Seq.head // just taking first test and training set for now
   /// function to create confusion matrix given test set to predict, test truths, training set
   let doValidationCM testRowsCleared testRowTruths trainRows inputSchema classcolIdx originalData =
       let inputDTO, trainRowIdx = createDTOForInference testRowsCleared trainRows originalData
       let predictionDTO = doPrediction inputDTO inputSchema
       let classcolReturned = getClasscolFromDTO (convertDistDTOToDTO(predictionDTO)) classcolIdx
       let predictedVals = getTrainRowsFromClasscol classcolReturned trainRowIdx

       let predictDs = Seq.map (fun predictedVal ->
                                   match (predictedVal:obj) with
                                   | :? MicrosoftResearch.Infer.Distributions.Discrete as  d -> d
                                   | _ -> failwith ("expected discrte distribution but actually have "+(predictedVal.ToString()))
                                   ) predictedVals
       let dim = predictDs |> Seq.head |> getdimension_discrete
       let truthDs = makePointMasses testRowTruths dim
       genConfusionMatrix predictDs truthDs getdimension_discrete getproba_modeLoss multproba_regular addproba_regular
   
   let doValidationCM_allWays originalDataStruct allRowData schema classcolIdx =
       // If a row has a null value in the classcol, filter it out.  We cannot train on it, nor do we have ground truth on it.
       let dataToUse = Seq.filter (fun objarr -> (objarr: _ array).[classcolIdx] <> null) allRowData
       
       // helper funtions
       let flip f x y = f y x
       /// map2 function for Array2D
       let map2_array f a1 (a2:_[,]) =
           Array2D.initBased (Array2D.base1 a1) (Array2D.base2 a1) (Array2D.length1 a1) (Array2D.length2 a1) (fun i j -> f (a1.[i,j]) (a2.[i,j]))
       
       let CMseq = Seq.map (fun (a,b,c) -> doValidationCM a b c schema classcolIdx originalDataStruct) (split dataToUse classcolIdx)
       let CMtotal = Seq.reduce (map2_array (+)) CMseq
       let CMmean = Array2D.map (CMseq |> Seq.length |> float |> (flip (/))) CMtotal
       CMmean
   
   // OPERATIONAL ---------------------------------------

   use loader = new ExcelLoader(wb :?> _,false) :> ILoader     
   /// TEST SCHEMA                                                                                                                 
   let schema :Schema =                                                                                                                                                         
            [   Table("T_Occupation", ["Occupation",{Type=T_String; Markup=Input}]);
                Table("T_ShoeSize", ["ShoeSize",{Type=T_String; Markup=Input}]);
                Table("AgeTable", ["Age",{Type=T_String; Markup=Input}]);
                Table("classifier_table", 
                      [("Occupation",{Type=T_Det(B_Link "T_Occupation", D); Markup=Observable(MCall("CDiscrete",[("N",SizeOf("T_Occupation")) ]))});
                       ("Age",{Type=T_Det(B_Link "AgeTable", D); Markup=Observable(MIndexed(MCall("CDiscrete",[("N",SizeOf("AgeTable")) ]), Var "Occupation", Const (IntConst -1)))});
                       ("ShoeSize",{Type=T_Det(B_Link "T_ShoeSize", D); Markup=Observable(MIndexed(MCall("CDiscrete",[("N",SizeOf("T_ShoeSize")) ]), Var "Occupation", Const (IntConst -1)))});
                       ("isMale",{Type=T_Det(B_Bool, D); Markup=Observable(MIndexed(MCall("CBernoulli",[ ]), Var "Occupation", Const (IntConst -1)))});
                       ("Income",{Type=T_Det(B_Bool, D); Markup=Observable(MIndexed(MCall("CBernoulli",[ ]), Var "Occupation", Const (IntConst -1)))})]
                     )
            ]
   // dicDatas: Map "Occupation"->0,"Age"->1,... * ROWDATA IEnumerable<object[]> // TYPE DTO
   // idToPos:  Map "T_Occupation" -> (map "Cobbler" -> 3) 
   // let dicLog, dicIdStrategy, dicDatas, idToPos 
   let _, _, dicDatas, _ = readTable loader schema
   
   let allRowData, classcolIdx = 
       let classifierTableData = getClassifierTableData (DTO(dicDatas))
       let classifierTable = match schema |> List.tryFind  (fun decl -> decl.Name.Equals(sClassifierTable)) with
                                 | Some x -> x.getTable
                                 | None -> failwith "Sorry could not find \"classifier_table\""
       // the classifier column is the FIRST column in the table
       let (classcolName, _) = List.head classifierTable 
       let classcolIdx = (fst classifierTableData).[classcolName] // the position in one row of data of classcol
       classifierTableData |> snd, classcolIdx

   let CMmean = doValidationCM_allWays dicDatas allRowData schema classcolIdx
   



   

   // NEXT STEP: output cm to Excel










   // print for testing
   //let rowPrinter =  "%A " |> (testRowsCleared |> Seq.head |> Array.length |> String.replicate) |> printfn 
//   let funRowsToString = fun rows ->
//        let sb = Seq.fold (fun sb rowarr -> (Array.fold (fun sb o -> (sb:System.Text.StringBuilder).Append(sprintf "%A, " o)) sb rowarr ).AppendLine())
//                          (new System.Text.StringBuilder()) rows
//        in sb.ToString()
//   let trainS = funRowsToString trainRows
//   let testS = funRowsToString testRows
//   let testModS = funRowsToString testRowsCleared
//   let trainN = Seq.length trainRows
//   let testN = Seq.length testRows
//   let testModN = Seq.length testRowsCleared
   //printfn (Printf.TextWriterFormat<_>(rowarr.ToString())) ) testRowsCleared

   // Step 2 - 

   // --------------------------------------------------------------------------------------------------------
   let window = Window(Title = "wrapper instead of excel", Content = view.Control, Width = 800., Height = 800.)
   window.DataContext <- model
   window   



open TaskPane.TableNorm
open TableNorm.TableNormTest
[<STAThread>]
let bestFD = (GSTableDTOOrig.tables |> List.head).data |> getBestFD 

normalize GSTableDTOOrig
//(new Application()).Run(loadWindow()) |> ignore

