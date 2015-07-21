namespace MicrosoftResearch.Infer.Tabular

[<AutoOpen>]
module CrossValidation =

   

    let USE_PARALLELISM = false // ExcelCompiler needs to be threadsafe first

    open MicrosoftResearch.Infer.Tabular.Syntax
    open MicrosoftResearch.Infer
    open MicrosoftResearch.Infer.Distributions
    open MicrosoftResearch.Infer.Tabular
    open MicrosoftResearch.Infer.Tabular.TabularCompiler
   // open MicrosoftResearch.Infer.Tabular.Compiler
   
    let dbToKnowAndDist schema (db : TypedDTO.DataBase) = 
        let distDTO =  
         DistDTO ( 
          schema |> List.choose (function | (Declaration(Table(tn,_), table)) ->
                                                let cols = table |> List.filter (fun c -> level c > W) |> List.map fst
                                                let (tblsize, colValues, idRep, keytoPos)  = db.[tn]
                                                let c2i  =  cols |> List.mapi   (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                                let data = cols |> List.map (fun cn -> (colValues.[cn] :?> TypedDTO.Instance).get_NonNullValues ) |> List.toArray
                                                Some(tn,(c2i, [| for row in 0..tblsize-1 -> Array.init cols.Length (fun i -> data.[i].GetValue(row)) |] :> seq<obj[]>))
                                          | (Declaration(Fun(name), table)) ->  None)
                  |> Map.ofList
          )
        let knowDTO = 
          KnowDTO (
           schema |> List.choose (function | (Declaration(Table(tn,_), table)) ->
                                                         let cols = table |> List.filter (fun c -> level c = W) |> List.map fst
                                                         let (tblsize, colValues,idRep, keytoPos)  = db.[tn]
                                                         let c2i  =  cols |> List.mapi   (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                                         Some(tn,(c2i, [| for cn in cols -> (colValues.[cn]  :?>  TypedDTO.Static).Value()  |]  ))
                                                  | (Declaration(Fun(name), table)) ->  None)
                  |> Map.ofList                
           )
        distDTO, knowDTO
    

    // DEFINITIONS ------------------------------------
    let sClassifierTable = "classifier_table":TableName
    let dtoToClassifierTableData (DTO(dto)) =
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
        Map.map (fun tableName ((colMapping,_) as dataNorm) -> if System.String.Equals(tableName,sClassifierTable) then (colMapping,newRowSeq) else dataNorm) originalDTO 
        |> DTO, Seq.length trainRows
    
    //Map<TableName, Map<ColumnName,int> * (obj array seq)> //row level DTO
    let doPrediction (inputDTO:DTO) (typedCoreSchema:Schema) = //returnedSchema dicReturnedData =
        
             //let file = wb.Name
             //let name = match m.ActiveModelName with None -> "model" | Some s -> s
             //ExcelCompiler.latentModel("CrossValidationTemp","CrossValidationTemp",inputSchema,(*verbose*)false,(*collectStats*)false).performInferenceGeneric(inputDTO,Some(new VariationalMessagePassing():> IAlgorithm), None, None) |> Async.RunSynchronously
           //  let (_,_,(typedCoreSchema,_)) = Elaborator.elaborate(inputSchema)
             let dbin = DTOToTypedDTO.readFromDTO typedCoreSchema inputDTO
             let (schema,ev , dbout) = 
                  TabularCompiler.latentModelStraight( 
                                               (* file *) "CrossValidationTemp",
                                               (* name *) "CrossValidationTemp",
                                               (* schema *) typedCoreSchema,
                                               (* verbose *)  false,
                                               (* collectStats *) false,
                                               (* codefile *) null,
                                               (* database *)  dbin,
                                               (* algo *) Some (new VariationalMessagePassing():> IAlgorithm),
                                               (* breakSymmetry *) true,
                                               (* iterations *) 50,
                                               (* seed *) 123567,
                                               (* cstLongToken *) new System.Threading.CancellationToken())
             let (distDTO,knowDTO) = dbToKnowAndDist typedCoreSchema dbout
             distDTO

    let getClasscolFromDTO (dto:DTO) classcolIdx = 
        let data = dto |> dtoToClassifierTableData |> snd
        Seq.map (fun objarr -> (objarr:obj array).[classcolIdx]) data 
    
    /// skip down to the test rows
    let getTrainRowsFromClasscol classcolRows trainRowIdx = 
        Seq.skip trainRowIdx classcolRows
    
    let convertDistDTOToDTO (DistDTO(ddto)) = DTO(ddto)
    
    
    let getproba_modeLoss (pred:Discrete) idx = if pred.GetMode() = idx then 1 else 0 // GetMean()
 
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
    /// [i,j] entry means i is the predicted class; j is the true class
    let genConfusionMatrix (predictedDs:'a seq) (trueDs:'a seq) getdimension getproba multproba addproba = 
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

    //let confusionMatrix (predictedvals: obj array) (truevals: obj array) : int array array = 
    /// [i,j] entry means i is the predicted class; j is the true class
    let genConfusionMatrix_threshold (predictedDs:'a seq) (trueDs:'a seq) getdimension getproba multproba addproba threshold = 
         let dimension = getdimension(trueDs |> Seq.head) 
         let index = Array.init dimension id
         let confusion = Array2D.zeroCreate dimension dimension 
         for (predD, trueD)  in Seq.zip predictedDs trueDs do
             for i in index do
                 for j in index do
                     let p1 = //getproba predD i
                        //if (predD:Discrete).GetMean() > threshold then 1 else 0
                        if (predD:Discrete).GetMean() > threshold then i else 1-i
                     let p2 = getproba trueD j
                     let m = multproba(p1)(p2) |> float
                     confusion.[i,j] <- addproba confusion.[i,j]  m
         confusion :float[,] // enforce floating point number type
    
    
    let getGood (confusion:float[,]) =
        confusion.[0,0] + confusion.[1,1]
     
    
    
    
    
    
    //let testRowsCleared, testRowTruths, trainRows = allRowData |> Seq.toArray |> split 
                                                    //|> Seq.head // just taking first test and training set for now
    /// function to create confusion matrix given test set to predict, test truths, training set
    let doValidationCM testRowsCleared testRowTruths trainRows inputSchema classcolName originalData =
        let inputDTO, trainRowIdx = createDTOForInference testRowsCleared trainRows originalData
        let predictionDTO = doPrediction inputDTO inputSchema
        let classcolIdx = (predictionDTO) |> convertDistDTOToDTO |> dtoToClassifierTableData |> fst |> Map.find classcolName
        let classcolReturned = getClasscolFromDTO (convertDistDTOToDTO(predictionDTO)) classcolIdx
        let predictedVals = getTrainRowsFromClasscol classcolReturned trainRowIdx
    
        let predictDs = Seq.map (fun predictedVal ->
                                    match (predictedVal:obj) with
                                    | :? MicrosoftResearch.Infer.Distributions.Discrete as  d -> d
                                    | _ -> failwith ("expected discrete distribution but actually have "+(predictedVal.ToString()))
                                    ) predictedVals
        let dim = predictDs |> Seq.head |> getdimension_discrete
        let truthDs = makePointMasses testRowTruths dim
        genConfusionMatrix predictDs truthDs getdimension_discrete getproba_modeLoss multproba_regular addproba_regular
 
    /// return CM and best threshold that maximizes score on diagonal (somewhat locally)
    let doValidationCM_getThreshold testRowsCleared testRowTruths trainRows inputSchema classcolName originalData =
        let inputDTO, trainRowIdx = createDTOForInference testRowsCleared trainRows originalData
        let predictionDTO = doPrediction inputDTO inputSchema
        let classcolIdx = (predictionDTO) |> convertDistDTOToDTO |> dtoToClassifierTableData |> fst |> Map.find classcolName
        let classcolReturned = getClasscolFromDTO (convertDistDTOToDTO(predictionDTO)) classcolIdx
        let predictedVals = getTrainRowsFromClasscol classcolReturned trainRowIdx
    
        let predictDs = Seq.map (fun predictedVal ->
                                    match (predictedVal:obj) with
                                    | :? MicrosoftResearch.Infer.Distributions.Discrete as  d -> d
                                    | _ -> failwith ("expected discrete distribution but actually have "+(predictedVal.ToString()))
                                    ) predictedVals
        let dim = predictDs |> Seq.head |> getdimension_discrete
        let truthDs = makePointMasses testRowTruths dim
        let mutable oldt, newt = 0.5, 0.25
        let mutable oldCM, newCM = 
            genConfusionMatrix_threshold predictDs truthDs getdimension_discrete getproba_modeLoss multproba_regular addproba_regular oldt
            , genConfusionMatrix_threshold predictDs truthDs getdimension_discrete getproba_modeLoss multproba_regular addproba_regular newt
        let mutable oldgood, newgood = getGood oldCM, getGood newCM
        while oldt-newt > 0.001 do
            if newgood >= oldgood
            then oldt <- newt; newt <- newt * 0.5; oldgood <- newgood; oldCM <- newCM
            else newt <- newt + (oldt-newt) * 0.5
            newCM <- genConfusionMatrix_threshold predictDs truthDs getdimension_discrete getproba_modeLoss multproba_regular addproba_regular newt
            newgood <- getGood newCM
        oldCM, oldt

    let doValidationCM_fixThreshold testRowsCleared testRowTruths trainRows inputSchema classcolName originalData threshold =
        let inputDTO, trainRowIdx = createDTOForInference testRowsCleared trainRows originalData
        let predictionDTO = doPrediction inputDTO inputSchema
        let classcolIdx = (predictionDTO) |> convertDistDTOToDTO |> dtoToClassifierTableData |> fst |> Map.find classcolName
        let classcolReturned = getClasscolFromDTO (convertDistDTOToDTO(predictionDTO)) classcolIdx
        let predictedVals = getTrainRowsFromClasscol classcolReturned trainRowIdx
    
        let predictDs = Seq.map (fun predictedVal ->
                                    match (predictedVal:obj) with
                                    | :? MicrosoftResearch.Infer.Distributions.Discrete as  d -> d
                                    | _ -> failwith ("expected discrete distribution but actually have "+(predictedVal.ToString()))
                                    ) predictedVals
        let dim = predictDs |> Seq.head |> getdimension_discrete
        let truthDs = makePointMasses testRowTruths dim
        genConfusionMatrix_threshold predictDs truthDs getdimension_discrete getproba_modeLoss multproba_regular addproba_regular threshold



    //open FSharp.Collections.ParallelSeq
    // open System.Linq
    /// create a confusion matrix that is the average of the confusion matrices doing validation 5 times in 80-20 train-test splits
    let doValidationCM_allWays (DTO(originalDataStruct)) allRowData schema classcolIdx =
        // If a row has a null value in the classcol, filter it out.  We cannot train on it, nor do we have ground truth on it.
        let dataToUse = Seq.filter (fun objarr -> (objarr: _ array).[classcolIdx] <> null) allRowData
        
        // helper funtions
        let flip f x y = f y x
        /// map2 function for Array2D
        let map2_array f a1 (a2:_[,]) =
            Array2D.initBased (Array2D.base1 a1) (Array2D.base2 a1) (Array2D.length1 a1) (Array2D.length2 a1) (fun i j -> f (a1.[i,j]) (a2.[i,j]))
        
        //let CMseq = Seq.map (fun (a,b,c) -> doValidationCM a b c schema classcolIdx originalDataStruct) (split dataToUse classcolIdx)
        
        let classcolName = (DTO(originalDataStruct)) |> dtoToClassifierTableData |> fst |> Map.findKey (fun name idx -> idx = classcolIdx)
        let bigseq = split dataToUse classcolIdx
        let CMseq, threshold = 
            let a,b,c = Seq.head bigseq
            /// get best threshold for the first fold
            let CMseq1, threshold = doValidationCM_getThreshold a b c schema classcolName originalDataStruct
            /// use best threshold for subsequent (k-1)-folds
            Seq.append (Seq.singleton CMseq1)
                        (bigseq |> Seq.skip 1 |> Seq.map (fun (a,b,c) -> doValidationCM_fixThreshold a b c schema classcolName originalDataStruct threshold))
            , threshold


       // let CMseq = System.Linq.ParallelEnumerable.Select((split dataToUse classcolIdx).AsParallel(), 
        //                                                  new System.Func<_,_>(fun (a,b,c) -> doValidationCM a b c schema classcolName originalDataStruct))
  
(*
        let CMseq  = 
             if USE_PARALLELISM then 
                System.Linq.ParallelEnumerable.Select((split dataToUse classcolIdx).AsParallel(), 
                                                      new System.Func<_,_>(fun (a,b,c) -> doValidationCM a b c schema classcolName originalDataStruct))
                :> seq<float[,]>
             else   
               Seq.map (fun (a,b,c) -> doValidationCM a b c schema classcolName originalDataStruct) (split dataToUse classcolIdx)
*)

        let CMtotal = Seq.reduce (map2_array (+)) CMseq
        let CMmean = Array2D.map (CMseq |> Seq.length |> float |> (flip (/))) CMtotal
        CMmean,threshold

      

    
    

    // OPERATIONAL ---------------------------------------
    
    // Commented code constitutes a full example.
//    use loader = new ExcelLoader(wb :?> _,false) :> ILoader     
//    /// TEST SCHEMA                                                                                                                 
//    let schema :Schema =                                                                                                                                                         
//             [   Table("T_Occupation", ["Occupation",{Type=T_String; Markup=Input}]);
//                 Table("T_ShoeSize", ["ShoeSize",{Type=T_String; Markup=Input}]);
//                 Table("AgeTable", ["Age",{Type=T_String; Markup=Input}]);
//                 Table("classifier_table", 
//                       [("Occupation",{Type=T_Det(B_Link "T_Occupation", D); Markup=Observable(MCall("CDiscrete",[("N",SizeOf("T_Occupation")) ]))});
//                        ("Age",{Type=T_Det(B_Link "AgeTable", D); Markup=Observable(MIndexed(MCall("CDiscrete",[("N",SizeOf("AgeTable")) ]), Var "Occupation", Const (IntConst -1)))});
//                        ("ShoeSize",{Type=T_Det(B_Link "T_ShoeSize", D); Markup=Observable(MIndexed(MCall("CDiscrete",[("N",SizeOf("T_ShoeSize")) ]), Var "Occupation", Const (IntConst -1)))});
//                        ("isMale",{Type=T_Det(B_Bool, D); Markup=Observable(MIndexed(MCall("CBernoulli",[ ]), Var "Occupation", Const (IntConst -1)))});
//                        ("Income",{Type=T_Det(B_Bool, D); Markup=Observable(MIndexed(MCall("CBernoulli",[ ]), Var "Occupation", Const (IntConst -1)))})]
//                      )
//             ]
//    // dicDatas: Map "Occupation"->0,"Age"->1,... * ROWDATA IEnumerable<object[]> // TYPE DTO
//    // idToPos:  Map "T_Occupation" -> (map "Cobbler" -> 3) 
//    // let dicLog, dicIdStrategy, dicDatas, idToPos 
//    let _, _, dicDatas, _ = readTable loader schema
//    
    /// Get the row data of the "classifier_table" and the index of the classifier column (using first column) and the name of the foreignTable
    let parseDTOForClassifier (DTO(originalDataStruct)) (schema:Schema) = 
        let classifierTableData = dtoToClassifierTableData (DTO(originalDataStruct))
        let classifierTable = match schema |> List.tryFind  (fun decl -> decl.Name.Equals(sClassifierTable)) with
                                  | Some x -> x.getTable
                                  | None -> failwith "Sorry could not find \"classifier_table\""
        // the classifier column is the FIRST **OUTPUT** column in the table
        let (classcolName, col) = match classifierTable |> List.tryFind (fun (_,column) ->
                                                                        match column.Markup with
                                                                        | Observable _ -> true
                                                                        | _ -> false
                                                                        ) with
                                  | Some x -> x
                                  | None -> failwith "no output column in schema; what is the classifier column?"
        let foreignTableName = match col.Type with
                               | T_Det (B_Link ftn,_) -> ftn
                               | T_Det (B_Upto(TypedExp(SizeOf(ftn),_)),_) -> ftn
                               | _ -> failwithf ("expected classifier_table->%s to be a link type, but actually is %A") classcolName col.Type
        let classcolIdx = (fst classifierTableData).[classcolName] // the position in one row of data of classcol
        classifierTableData |> snd, classcolIdx, foreignTableName

    /// get classifier column labels
    let getForeignLabels (foreignTableName:TableName) (schema:Schema) (idToPos:Map<TableName,Map<System.IComparable,int>>) = 
        let posToId = idToPos |> Map.map (fun tname lIdToPos -> lIdToPos |> Seq.map (fun kv -> kv.Value, kv.Key) |> Map.ofSeq)
        let foreignTable:Table = match schema |> List.tryFind  (fun decl -> decl.Name.Equals(foreignTableName)) with
                                  | Some x -> x.getTable
                                  | None -> failwithf "Sorry could not find %A" foreignTableName
        let foriegnTableVals = posToId.Item(foreignTableName) |> Map.toArray |> (Array.map snd)
        foriegnTableVals

    //let allRowData, classcolIdx = getAllRowDataAndClassColIdx dicDatas schema
//    
//    let CMmean = doValidationCM_allWays dicDatas allRowData schema classcolIdx

    