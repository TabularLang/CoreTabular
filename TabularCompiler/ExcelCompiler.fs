namespace MicrosoftResearch.Infer.Tabular


open MicrosoftResearch.Infer
open MicrosoftResearch.Infer.Models
open MicrosoftResearch.Infer.Distributions
open MicrosoftResearch.Infer.Collections
//open MicrosoftResearch.Infer.Tabular.Service
open Syntax
module FArray = Microsoft.FSharp.Collections.Array

module ExcelCompiler =
 open Target
 open Compiler
 open System.Threading

 exception AbortException 
 exception CheckException of string

 let defaultRandomSeed = 123567

#if DEAD
 //  aglo = None -> sampling from prior
 let compile (verbose: bool, collectStats:bool, file:string,name:string,fullSchema:Schema, algo:IAlgorithm option, numberOfIterations : int option, cts: CancellationToken option) =
      let total = new System.Diagnostics.Stopwatch()
      do total.Start()
      let sw = new System.Diagnostics.Stopwatch()
      do sw.Start()
      let lazyWriteLine = if verbose then fun (s:Lazy<string>) -> System.Console.WriteLine(s.Force()) else fun _ -> ()  
      lazyWriteLine(lazy ("Infer.NET version: " + Translate.inferNetVersion.ToString()))   
      let (log,err,(typedCoreSchema,schemaType)) = Elaborator.elaborate(fullSchema)
      lazyWriteLine <| lazy Map.fold (fun s tb log -> 
                                      Map.fold (fun s col v -> 
                                                     match v with 
                                                     |  (Table.Err msg) ->
                                                      s+(sprintf "\n %A %A : %A" tb col msg)
                                                     | _ -> s) 
                                                     s log)
                            "" log
      if err then failwithf "type-checking error %A" log   
      lazyWriteLine <| lazy Pretty.schemaToStr typedCoreSchema 
      let (TI,TE,s) = Translate.trSchemaWithInfo(typedCoreSchema)
      lazyWriteLine <| lazy Pretty.StoCSoft "\n" s
      //lazyWriteLine <| lazy Pretty.StoString "\n" s 
      let code = lazy (try CodeDom.toCompileUnit "Tabular" name typedCoreSchema s with e -> sprintf "/*C# code generation failed %s */" (e.ToString()))
      lazyWriteLine <| code
      let tex = lazy (try Tex.schemaToStr typedCoreSchema with e -> sprintf "%%tex generation failed %s */" (e.ToString()))
      //lazyWriteLine <| tex
    
      let (evidence,(RE,VE,AE)) = interpM  s
      do sw.Stop()
      let compileTime = sw.ElapsedMilliseconds
      let ie = new InferenceEngine(defaultArg algo (new ExpectationPropagation() :> _))
      ie.ShowMsl <- verbose
      ie.ShowTimings <- verbose
      ie.ShowProgress <- verbose
      ie.Compiler.GenerateInMemory <- true
      ie.Compiler.WriteSourceFiles <- verbose
      ie.Compiler.IncludeDebugInformation <- false
      ie.Compiler.GeneratedSourceFolder <- System.IO.Path.GetTempPath() + @"Tabular\GeneratedSource"
      lazyWriteLine <| lazy (sprintf "Generated Source Folder: %s" (ie.Compiler.GeneratedSourceFolder))
      ie.NumberOfIterations <- defaultArg numberOfIterations 10
      let infer (DTO dto)  =

#if ANGLICAN
        let sexp = lazy (try Anglican.trSchem (DTO dto) typedCoreSchema with e -> sprintf "--Anglican code generation failed %s" (e.ToString()))
        lazyWriteLine sexp
#endif
        match algo with
        | None -> // Just sample
            let (distDTO,knowDTO) = QueryCompiler.Sample (DTO dto)  typedCoreSchema 
            (typedCoreSchema,0.0,(distDTO,knowDTO))
        | Some _->
        let rec trTables vsToInfer (TE:Map<TableName,int * Map<ColumnName,int>>) tables = 
          match tables with
          | [] -> (TE,vsToInfer)
          | (Declaration(Table(tn,_),table)::tables) ->
            let (colmap,data) = dto.[tn]
            let data = Seq.toArray(data)
            let length = data.Length
            let choosei fchoose ar =  ar |> FArray.mapi(fun i e -> (i,e))|> FArray.choose fchoose 
            let checkUpto      i size v      = if v >= 0 && v < size then v else           raise (CheckException (sprintf "row %A : expected value in [0,SizeOf(%A)=%A), found %A" i tn size v)) 
            let checkToInt     i     (v:obj) = try System.Convert.ToInt32   v with | _ ->  raise (CheckException (sprintf @"row %A : %A can't be converted to %A" i v (System.Int32.MinValue.GetType())))
            let checkToReal    i     (v:obj) = try System.Convert.ToDouble  v with | _ ->  raise (CheckException (sprintf @"row %A : %A can't be converted to %A" i v (System.Double.MinValue.GetType())))
            let checkToBool    i     (v:obj) = try System.Convert.ToBoolean v with | _ ->  raise (CheckException (sprintf @"row %A : %A can't be converted to %A" i v (true.GetType())))   
            let checkToString  i     (v:obj) = try System.Convert.ToString  v with | _ ->  raise (CheckException (sprintf @"row %A : %A can't be converted to %A" i v ("".GetType())))   
            let mkArray c cty  =
              let i = colmap.[c]
              
              let observedIndices = data |> choosei (fun (k,r) -> if (r.[i] = null) then None else Some k)
              observedIndices,
              //TODO: these checks with sprintf's are unsafe and need to be fixed
              match cty with 
               // note: unsafe conversions
               | T_Link tn 
               | T_Upto (TypedExp(SizeOf tn,_)) ->
                              try let check i = checkToInt i >> checkUpto i (fst (TE.[tn])) 
                                  ([| for irow in observedIndices -> check irow (data.[irow].[i]) |] :> System.Array)
                              with | CheckException m -> raise (System.ArgumentException((sprintf "in table %A, column %A :"  tn c ) + m))
               | T_Upto (TypedExp(Exp.Const (IntConst n),_)) ->   
                                 try let check i = checkToInt i >> checkUpto i n  
                                     ([| for irow in observedIndices ->       check   irow (System.Convert.ToInt32  (data.[irow].[i]))|] :> System.Array)
                                 with | CheckException m -> raise (System.ArgumentException((sprintf "in table %A, column %A :"  tn c ) + m))
               | T_Real     ->   try ([| for irow in observedIndices -> checkToReal   irow (data.[irow].[i])|] :> System.Array)
                                 with | CheckException m -> raise (System.ArgumentException((sprintf "in table %A, column %A :"  tn c ) + m))
               | T_Int      ->   try ([| for irow in observedIndices -> checkToInt    irow (data.[irow].[i])|] :> System.Array)
                                 with | CheckException m -> raise (System.ArgumentException((sprintf "in table %A, column %A :"  tn c ) + m))
               | T_Bool     ->   try  ([| for irow in observedIndices -> checkToBool  irow (data.[irow].[i])|] :> System.Array)
                                 with | CheckException m -> raise (System.ArgumentException((sprintf "in table %A, column %A :"  tn c ) + m))
               | T_String _ ->   try ([| for irow in observedIndices -> checkToString irow (data.[irow].[i])|] :> System.Array)
                                 with | CheckException m -> raise (System.ArgumentException((sprintf "in table %A, column %A :"  tn c ) + m))
               | T_Array(ety,e) -> failwith "NYI"
               | T_Record _  -> failwith "NYI"
               | T_Vector -> failwith "NYI"
  
            let s = size tn
            VE.[s].observeValue(Seq.length(snd(dto.[tn])))
            let rec trColumns vsToInfer (CE: Map<ColumnName,int>) columns   =
              match columns with
              |  [] -> 
                trTables vsToInfer (TE.Add(tn,(length,CE))) tables
              | (cn,{Type=ty;Markup=m})::rest ->
                if Types.det ty = Qry
                then trColumns vsToInfer CE rest // skip queries
                else
                let r = range(tn)
                match m with
                | Hyper _ -> 
                  trColumns vsToInfer CE rest
                | Param _ ->
                  let v = VE.[col(tn,cn)]
                 // let CE = CE.Add(cn, (Seq.length CE.Keys))
                  trColumns ((v:>IVariable)::vsToInfer) CE rest
                | Input ->
                  let av = AE.[col(tn,cn)]
                  let indices,vs = mkArray cn ty
                  assert (indices.Length = length)
                  assert (vs.Length = length )
                  av.observeValue(vs) //TODO: specialize to avoid computing fst
                  trColumns vsToInfer CE rest    
                | Latent _ ->
                  let CE = CE.Add(cn, (Seq.length CE.Keys))
                  let av = AE.[col(tn,cn)]
                  trColumns ((av:>IVariable)::vsToInfer) CE rest  
                | Observable _ -> 
                  let av = AE.[col(tn,cn)]
                  let size = VE.[subarraysize(tn,cn)]
                  let indices = AE.[subarrayindices(tn,cn)]
                  //let range = RE.[subarrayrange(tn,cn)]
                  let subarray = VE.[subarray(tn,cn)]
                  let is,vs = mkArray cn ty
                  size.observeValue(is.Length)
                  indices.observeValue(is)
                  subarray.observeValue(vs)
                  //av.observeValue(mkArray cn ty)
                  // let _ = use v = Variable.If(evidence) //TODO: review
                  //         av.observeValueMissing(mkArray cn ty)
                  let CE = CE.Add(cn, (Seq.length CE.Keys))
                  trColumns ((av:>IVariable)::vsToInfer) CE rest  
            trColumns vsToInfer Map.empty table

        

        let (TE,vsToInfer) = trTables [evidence:>IVariable] Map.empty typedCoreSchema

        let (rows,cells) = Map.fold (fun (sumrows,sumcells) t (size,cols:Map<_,_>) -> (sumrows + size, sumcells + size * (Seq.length cols.Keys))) (0,0) TE

        // breakSymmetries, now we have the table sizes (sizeMap) and variables in hand
        let () = let sizeMap = Map.map (fun tn (n,_) -> n) TE
                 SymmetryBreaking.breakSymmetries sizeMap (RE,VE,AE) typedCoreSchema

       // let vsToInfer = Map.fold (fun vs n (v:Variable) ->  if true ||(not(v.IsObserved)) then ((v:>IVariable)::vs) else vs) [evidence:>IVariable] VE
       // let vsToInfer = Map.fold (fun vs n (v:Variable) ->  if true || (not(v.IsObserved)) then ((v:>IVariable)::vs) else vs) vsToInfer AE
        ie.OptimiseForVariables <- List.toArray(vsToInfer)

        //ie.Compiler.GivePriorityTo(typeof<MicrosoftResearch.Infer.Factors.GaussianProductOp_SHG09>);


        do sw.Reset();
        do sw.Start();
        try
           let f = InferenceProgressEventHandler(fun _ ipea -> 
              System.Console.WriteLine("({0})",ipea.Iteration);
              if cts.IsSome && cts.Value.IsCancellationRequested then raise AbortException)
           ie.add_ProgressChanged(f)
           let eD = 
//              try 
                  ie.Infer<Bernoulli>(evidence)
//              with  :? AbortException -> System.Console.WriteLine "Inference aborted by user"
//                                         raise AbortException
//                   | e  -> raise e 
           //let eD = new Bernoulli(0.5)
         //  let VD,AD = Map.map(fun n (v:Variable) -> if (not(v.IsObserved)) then ie.Infer(v) else null ) VE, Map.map(fun n (v:Variable) -> if (not(v.IsObserved)) then ie.Infer(v) else null) AE
         //  let VD,AD = Map.fold(fun (VD:Map<v,obj>) n (v:Variable) -> if  true || (not(v.IsObserved)) then VD.Add(n,ie.Infer(v)) else VD ) Map.empty VE,
         //              Map.fold(fun (AD:Map<v,obj>) n (v:Variable) -> if  true || (not(v.IsObserved)) then AD.Add(n,ie.Infer(v)) else AD) Map.empty AE
            
           // REVIEW : do we still need this hack in Infer.NET 2.5?
           let distToArray size (dist:obj) =
               match dist with
               | :? ConvertibleToArray as a -> a.ToArray()
               // sometimes Infer.NET will infer a PointMass for an array, in which case we need to extract the elements of the array value
               // this is not ideal since we don't know which PointMass distribution (eg. Discrete.PointMass) to introduce
               | dist when   (let t = dist.GetType()  
                              t.IsGenericType 
                              && t.GetGenericTypeDefinition() = typedefof<PointMass<unit>>
                              && t.GetGenericArguments().[0].IsArray) ->
                 let t = dist.GetType()
                 let at = t.GetGenericArguments().[0]
                 let ety = at.GetElementType()
                 let pt = t.GetProperty("Point").GetValue(dist,null) :?> System.Array
                 assert (pt.Length = size)
                 [| for i in 0  .. pt.Length-1  ->
                    (System.Activator.CreateInstance(typedefof<PointMass<unit>>.MakeGenericType(ety),[| pt.GetValue(i)|])) |] :> System.Array
               | _ ->
                 [| for i in 0  .. size - 1 ->
                    "bogus"|] :> System.Array


           let distDTO = 
               DistDTO (  TE |> Map.map(fun tn (tblsize,colmap) -> 
                                         let rowsize = Seq.length colmap.Keys
                                         let dists = Microsoft.FSharp.Collections.Array.create<System.Array> rowsize null
                                         let _ = Map.iter (fun cn i -> 
                                                              dists.[i] <- 
                                                              let dists = try ie.Infer(AE.[col(tn,cn)])
                                                                          with :? AbortException as e -> raise e
                                                                             | e -> box (PointMass<string[]>(Array.create tblsize "bogus"))
                                                              distToArray tblsize dists) colmap
                                         let colmapInv = Map.fold(fun (inv:Map<_,_>) c i -> inv.Add(i,c)) Map.empty colmap
                                         let tbl = [| for row in 0..tblsize-1 -> Microsoft.FSharp.Collections.Array.init rowsize (fun i -> dists.[i].GetValue(row))  |]
                                         (colmap, tbl :> seq<obj[]>)))
           let knowDTO = KnowDTO (TI |> Map.map (fun tn colmap ->
                                                 let colmap = Map.filter (fun cn (colinfo:Translate.ColumnInfo) -> colinfo.B = W ) colmap
                                                 let cols = Seq.toList(colmap.Keys)
                                                 let c2i = cols |> List.mapi (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                                 let array = [| for cn in cols ->
                                                                try ie.Infer(VE.[col(tn,cn)])
                                                                with  :? AbortException as e -> raise e
                                                                    | e  -> box (PointMass("bogus"))
                                                             |]
                                                 (c2i,
                                                  array)))
           ie.remove_ProgressChanged(f)
       // printfn "evidence %A" eD
       // printfn "%A %A" VD AD
           do sw.Stop()
           let inferenceTime = sw.ElapsedMilliseconds 
        
           let (distDTO,knowDTO, interpretedQueryTime,compiledQueryTime) =
               if not (QueryCompiler.schemaHasQuery false typedCoreSchema)
               then 
               // sw.Stop()
                (distDTO,knowDTO,0L,0L)
               else
#if ESOP
               do sw.Reset()
               do sw.Start()
               let _ = Query.trTables (DTO dto) distDTO knowDTO Map.empty typedCoreSchema
               sw.Stop()
#endif
               let interpretedQueryTime = sw.ElapsedMilliseconds
               sw.Reset()
               sw.Start()
               let (distDTO,knowDTO) = QueryCompiler.Query (DTO dto) distDTO knowDTO typedCoreSchema
               sw.Stop()
               let compiledQueryTime = sw.ElapsedMilliseconds
               (distDTO,knowDTO,interpretedQueryTime,compiledQueryTime)
       
           do sw.Stop()
           do total.Stop()
           if collectStats then
             let totalTime = total.ElapsedMilliseconds - interpretedQueryTime
            // let queryTime = sw.ElapsedMilliseconds
             let file = System.IO.Path.GetFileNameWithoutExtension(file) // remove .xlsx 
             let header = "% name rows cells compileTime inferenceTime interpretedQueryTime compiledQueryTime algorithm iterations totalTime\n"
             let stats = sprintf "\\\\%s\n %s & %O & %O & %O & %O & %O & %O & %s & %O & %O \\\\ \n" file name rows cells compileTime inferenceTime interpretedQueryTime compiledQueryTime (ie.Algorithm.ShortName) (ie.NumberOfIterations) totalTime
             let row = header+stats 
             lazyWriteLine <| lazy row
             let statsfile = System.IO.Path.GetTempPath() + @"Tabular\allstats.txt"
             System.IO.File.AppendAllText(statsfile,row)
             let statfile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".txt"
             System.IO.File.AppendAllText(statfile,row)
           (typedCoreSchema,eD.LogOdds,(distDTO,knowDTO))

         finally
            if collectStats then 
             let (log,err,(typedFullSchema,_)) = Schema.typeSchema fullSchema
             let fulltex = Tex.schemaToStr (typedFullSchema)
             let texfile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".tex"
             System.IO.File.WriteAllText(texfile,sprintf "%s\n\n%s\n\n" fulltex (tex.Force()))
             let codefile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".cs"
             System.IO.File.WriteAllText(codefile,code.Force())
#if ANGLICAN
          //to generate Anglican code
          let sexpfile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".sx"
          System.IO.File.WriteAllText(sexpfile,sexp.Force())
#endif          
      infer
 open System.Threading
 let latentModel(file,name,db,verbose,collectStats) = 
   { new LatentModel() with 
      member x.TrainAndPredictWithLogEvidence(din:DTO,?algo:IAlgorithm, ?numberOfIterations:int, ?cts:CancellationToken) : Schema * float*(DistDTO * KnowDTO) = 

          
          // compile(verbose,collectStats,file,name,db,algo, numberOfIterations,cts)(din)
          
          let tcs = new Tasks.TaskCompletionSource<Schema * float*(DistDTO * KnowDTO)>()
          let f (state:obj) = 
                    try 
                         let res = compile(verbose,collectStats,file,name,db,algo, numberOfIterations,cts)(din)
                         tcs.SetResult(res)
                    with e -> tcs.SetException(e)
          let stackSizeInByes = if System.Environment.Is64BitProcess 
                                then
                                  512*1024*1024
                                else
                                  16*1024*1024
          let tid = new Thread(new ThreadStart(f),stackSizeInByes)
          do tid.Name <- "inference thread"
          let _ = tid.Start()
          try tcs.Task.Result 
          with  // unwrap any aggregate exception
          | :? System.AggregateException as e -> raise e.InnerException 
          | e -> raise e
 }
 #endif

 let rec filterQueries  (TE:Declaration list)(tables:Declaration list)= 
      match tables with
      | [] -> List.rev TE                                                        
      | (Declaration(tid,table)::tables) -> 
        let rec trColumns CE columns  =
          match columns with
          |  [] ->  filterQueries (Declaration(tid,List.rev CE)::TE) tables
          | ((cn,{Type=T;Markup=m}) as col)::rest ->
            if Types.det T = Qry 
            then trColumns       CE  rest
            else trColumns (col::CE) rest
        trColumns [] table




 open TypedDTO
 let compileNew (verbose: bool, collectStats:bool, extractCode:string, file:string,name:string,typedCoreSchema:Schema, dbin:DataBase, algo:IAlgorithm option, breakSymmetry, numberOfIterations : int option, randomSeed,cts: CancellationToken option) =
      MicrosoftResearch.Infer.Maths.Rand.Restart(randomSeed)
      let total = new System.Diagnostics.Stopwatch()
      do total.Start()
      let sw = new System.Diagnostics.Stopwatch()
      do sw.Start()
      let lazyWriteLine = if verbose then fun (s:Lazy<string>) -> System.Console.WriteLine(s.Force()) else fun _ -> ()  
      lazyWriteLine(lazy ("Infer.NET version: " + Translate.inferNetVersion.ToString()))   

      lazyWriteLine <| lazy Pretty.schemaToStr typedCoreSchema 
      let (TI,TE,s) = Translate.trSchemaWithInfo(typedCoreSchema)
      lazyWriteLine <| lazy Pretty.StoCSoft "\n" s
      lazyWriteLine <| lazy Pretty.StoString "\n" s 
    //  let code = lazy (try CodeDom.toCompileUnit dbin "Tabular" name typedCoreSchema s with e -> sprintf "/*C# code generation failed %s */" (e.ToString()))
     // lazyWriteLine <| code
      let tex = lazy (try Tex.schemaToStr typedCoreSchema with e -> sprintf "%%tex generation failed %s */" (e.ToString()))
      //lazyWriteLine <| tex
    
      let (evidence,(RE,VE,AE)) = interpM  s
      do sw.Stop()
      let compileTime = sw.ElapsedMilliseconds
      let ie = new InferenceEngine(defaultArg algo (new ExpectationPropagation() :> _))
      ie.ShowMsl <- verbose
      ie.ShowTimings <- verbose; ie.ShowProgress <- verbose;  ie.Compiler.GenerateInMemory <- true; ie.Compiler.WriteSourceFiles <- verbose; ie.Compiler.IncludeDebugInformation <- false
      ie.Compiler.GeneratedSourceFolder <- System.IO.Path.GetTempPath() + @"Tabular\GeneratedSource"
      lazyWriteLine <| lazy (sprintf "Generated Source Folder: %s" (ie.Compiler.GeneratedSourceFolder))
      ie.NumberOfIterations <- defaultArg numberOfIterations 10
      let infer (dbin: DataBase)  =
      
#if ANGLICAN
        let sexp = lazy (try Anglican.trSchem (DTO dto) typedCoreSchema with e -> sprintf "--Anglican code generation failed %s" (e.ToString()))
        lazyWriteLine sexp
#endif
   
        match algo with
        | None ->   let dbout = QueryCompiler.Sample dbin  typedCoreSchema 
                    let typedCoreSchema  = filterQueries [] typedCoreSchema 
                    (typedCoreSchema,0.0,dbout)
        | Some _->
        let rec trTables vsToInfer (TE:Map<TableName,int * Map<ColumnName,int (*order which column have been added*)>>) (db:DataBase) tables  = 
          match tables with
          | [] -> (TE,vsToInfer)
          | (Declaration(Table(tn,_),table)::tables) ->
            let (ntable, colmap, idRep, idToPos) = db.[tn]
            let s = size tn
            VE.[s].observeValue(ntable)
            let rec trColumns vsToInfer (CE: Map<ColumnName,int>) columns   =
              match columns with
              |  [] -> 
                trTables vsToInfer (TE.Add(tn,(ntable,CE))) db tables
              | (cn,{Type=ty;Markup=m})::rest ->
                if Types.det ty = Qry
                then trColumns vsToInfer CE rest // skip queries
                else
                let r = range(tn)
                match m with
                | Hyper _ -> 
                  trColumns vsToInfer CE rest
                | Param _ ->
                  let v = VE.[col(tn,cn)]
                  trColumns ((v:>IVariable)::vsToInfer) CE rest
                | Input ->
                  let av = AE.[col(tn,cn)]
                  let inputValue = colmap.[cn] :?> TypedDTO.Instance
                  let aInputValue = inputValue.get_NonNullValues
                  av.observeValue(aInputValue)
                  trColumns vsToInfer CE rest    
                | Latent _ ->
                  let CE = CE.Add(cn, (Seq.length CE.Keys))
                  let av = AE.[col(tn,cn)]
                  trColumns ((av:>IVariable)::vsToInfer) CE rest  
                | Observable _ -> 
                  let size, indices, subarray = VE.[subarraysize(tn,cn)], AE.[subarrayindices(tn,cn)], VE.[subarray(tn,cn)]
                  let       is     , vs       = let inputValue = colmap.[cn] :?> TypedDTO.Instance 
                                                inputValue.get_NonNullIndices, inputValue.get_NonNullValues
                  size.observeValue(is.Length)
                  indices.observeValue(is)
                  subarray.observeValue(vs)
                  let CE = CE.Add(cn, (Seq.length CE.Keys))
                  let av = AE.[col(tn,cn)]
                  trColumns ((av:>IVariable)::vsToInfer) CE rest  
            trColumns vsToInfer Map.empty table

        let (TE,vsToInfer) = trTables [evidence:>IVariable] Map.empty dbin typedCoreSchema
        let (rows,cells) = Map.fold (fun (sumrows,sumcells) t (size,cols:Map<_,_>) -> (sumrows + size, sumcells + size * (Seq.length cols.Keys))) (0,0) TE
        // breakSymmetries, now we have the table sizes (sizeMap) and variables in hand
        if breakSymmetry then
                 let sizeMap = Map.map (fun tn (n,_) -> n) TE
                 SymmetryBreaking.breakSymmetries sizeMap (RE,VE,AE) typedCoreSchema
       // let vsToInfer = Map.fold (fun vs n (v:Variable) ->  if true || (not(v.IsObserved)) then ((v:>IVariable)::vs) else vs) [evidence:>IVariable] VE
       // let vsToInfer = Map.fold (fun vs n (v:Variable) ->  if true || (not(v.IsObserved)) then ((v:>IVariable)::vs) else vs) vsToInfer AE
        ie.OptimiseForVariables <- List.toArray(vsToInfer)
        //ie.Compiler.GivePriorityTo(typeof<MicrosoftResearch.Infer.Factors.GaussianProductOp_SHG09>);
        do sw.Reset();
        do sw.Start();
        try
           let f = InferenceProgressEventHandler(fun _ ipea -> 
              System.Console.WriteLine("({0})",ipea.Iteration);
              if cts.IsSome && cts.Value.IsCancellationRequested then raise AbortException)
           ie.add_ProgressChanged(f)
           let eD = 
              try 
                  ie.Infer<Bernoulli>(evidence)
              with  :? AbortException -> System.Console.WriteLine "Inference aborted by user"
                                         raise AbortException
                   | e  -> raise e 

           // REVIEW : do we still need this hack in Infer.NET 2.5?
           let distToArray size (dist:obj) =
               match dist with
               | :? ConvertibleToArray as a -> a.ToArray() 
               // sometimes Infer.NET will infer a PointMass for an array, in which case we need to extract the elements of the array value
               // this is not ideal since we don't know which PointMass distribution (eg. Discrete.PointMass) to introduce
               | dist when   (let t = dist.GetType()  
                              t.IsGenericType 
                              && t.GetGenericTypeDefinition() = typedefof<PointMass<unit>>
                              && t.GetGenericArguments().[0].IsArray) ->
                 let t = dist.GetType()
                 let at = t.GetGenericArguments().[0]
                 let ety = at.GetElementType()
                 let pt = t.GetProperty("Point").GetValue(dist,null)  :?> System.Array
                 assert (pt.Length = size)
                 [| for i in 0  .. pt.Length-1  ->
                    (System.Activator.CreateInstance(typedefof<PointMass<unit>>.MakeGenericType(ety),[| pt.GetValue(i)|])) |] :> System.Array
               | _ ->
                 [| for i in 0  .. size - 1 ->
                       box "bogus" :?> _ |] :> System.Array

           let dbout = 
               let rec trTables (DB:DataBase) (idReps : Map<TableName,  Map<_,int>>) (tables :Declaration list) = 
                  match tables with
                  | [] -> DB
                  | (Declaration(Fun(tn),table)::tables) ->   trTables DB idReps tables
                  | (Declaration(Table(tn, _),table)::tables) ->
                       let (length, colValues,idRep, keyToPos ) = dbin.[tn]
                       let rec trColumns (TB: Map<ColumnName,ColValue>)
                                         (columns: Table) = 
                        match columns with
                        |  [] -> 
                           trTables (DB.Add(tn,(keyToPos.Count,TB,idRep,keyToPos))) (idReps.Add(tn,keyToPos)) tables
                        | (cn,({Type=ty;Markup=m}))::rest -> 
                           //TODO
                           let TB' = match m with //we only read input and observables
                                       | Input          ->
                                          TB.Add(cn, colValues.[cn])
                                       | Observable _   when Types.det ty <> Qry -> 
                                          let dists = try ie.Infer(AE.[col(tn,cn)])
                                                      with :? AbortException as e -> raise e
                                                           | e -> box (PointMass<string[]>(Array.create length "bogus"))
                                          let instance = new DistributionInstance<obj>(distToArray length dists)  :> Instance
                                          TB.Add(cn, instance)
                                       | Latent _  when Types.det ty <> Qry -> 
                                          let dists = try ie.Infer(AE.[col(tn,cn)])
                                                      with :? AbortException as e -> raise e
                                                           | e -> box (PointMass<string[]>(Array.create length "bogus"))
                                          let instance = new DistributionInstance<obj>(distToArray length dists)  :> Instance
                                          TB.Add(cn, instance)
                                       | Param _  when Types.det ty <> Qry ->
                                          let res = try ie.Infer(VE.[col(tn,cn)])
                                                    with :? AbortException as e -> raise e
                                                         | e -> box (PointMass<string[]>(Array.create length "bogus"))
                                          let instance = new Static<obj>(res)  :> Static
                                          TB.Add(cn, instance)
                                       | _ -> TB
                           trColumns TB'  rest
                       trColumns Map.empty table
               trTables  Map.empty  Map.empty typedCoreSchema

           //let distDTO, knowDTO = failwith ""
           ie.remove_ProgressChanged(f)
           do sw.Stop()
           let inferenceTime = sw.ElapsedMilliseconds 
           
           (*hack*)
           let code = lazy (try Extraction.toCompileUnit dbout "Tabular" name typedCoreSchema s with e -> sprintf "/*C# code generation failed %s */" (e.ToString()))
           lazyWriteLine <| code
           
               //let codefile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".cs"
           if (extractCode <> null) then 
               System.IO.File.WriteAllText(extractCode,code.Force())

           let (dbout, interpretedQueryTime,compiledQueryTime) =
               if not (QueryCompiler.schemaHasQuery false typedCoreSchema)
               then 
               // sw.Stop()
                (dbout,0L,0L)
               else
               let interpretedQueryTime = sw.ElapsedMilliseconds
               sw.Reset()
               sw.Start()
               let dbout' = 
                       QueryCompiler.Query dbout typedCoreSchema
               sw.Stop()
               let compiledQueryTime = sw.ElapsedMilliseconds
               (dbout',interpretedQueryTime,compiledQueryTime)
       
           do sw.Stop()
           do total.Stop()
           if collectStats then
             let totalTime = total.ElapsedMilliseconds - interpretedQueryTime
            // let queryTime = sw.ElapsedMilliseconds
             let file = System.IO.Path.GetFileNameWithoutExtension(file) // remove .xlsx 
             let header = "% name rows cells compileTime inferenceTime interpretedQueryTime compiledQueryTime algorithm iterations totalTime\n"
             let stats = sprintf "\\\\%s\n %s & %O & %O & %O & %O & %O & %O & %s & %O & %O \\\\ \n" file name rows cells compileTime inferenceTime interpretedQueryTime compiledQueryTime (ie.Algorithm.ShortName) (ie.NumberOfIterations) totalTime
             let row = header+stats 
             lazyWriteLine <| lazy row
             let statsfile = System.IO.Path.GetTempPath() + @"Tabular\allstats.txt"
             System.IO.File.AppendAllText(statsfile,row)
             let statfile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".txt"
             System.IO.File.AppendAllText(statfile,row)
             //let fulltex = Tex.schemaToStr (typedCoreSchema)
             //let texfile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".tex"
             //System.IO.File.WriteAllText(texfile,sprintf "%s\n\n%s\n\n" fulltex (tex.Force()))
           (typedCoreSchema,eD.LogOdds,dbout)

         finally
          ()
#if ANGLICAN
          //to generate Anglican code
          let sexpfile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".sx"
          System.IO.File.WriteAllText(sexpfile,sexp.Force())
#endif   
      infer dbin

 open System.Threading

 let dbToKnowAndDist schema (db : TypedDTO.DataBase) = 
   let distDTO =  
     DistDTO ( 
      schema |> List.choose (function | (Declaration(Table(tn,_), table)) ->
                                            let cols = table |> List.filter (fun c -> level c > W) |> List.map fst
                                            let (tblsize, colValues, idRep, keytoPos)  = db.[tn]
                                            let c2i  =  cols |> List.mapi   (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                            let data = cols |> List.map (fun cn -> (colValues.[cn] :?> Instance).get_NonNullValues ) |> List.toArray
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
                                                     Some(tn,(c2i, [| for cn in cols -> (colValues.[cn]  :?> Static).Value()  |]  ))
                                              | (Declaration(Fun(name), table)) ->  None)
              |> Map.ofList                
       )
   distDTO, knowDTO

 let latentModelStraight(file, name, schema, verbose, collectStats, codefile, dbin : DataBase, algo, breakSymmetry, numberOfIterations, randomSeed, ctsLongToken) = 
      let (schema, le, db) = 
         async {
            let ctx = SynchronizationContext.Current
            do! Async.SwitchToThreadPool()
            let! tok= Async.StartChild(async { 
                        let tcs = new Tasks.TaskCompletionSource<Schema * float* DataBase>()
                        let f (state:obj) = 
                                 try 
                                    let res = compileNew(verbose,collectStats, codefile, file,name,schema, dbin, algo, breakSymmetry, Some numberOfIterations, randomSeed, Some ctsLongToken)
                                    tcs.SetResult(res)
                                 with e -> tcs.SetException(e)
                        let stackSizeInByes = if System.Environment.Is64BitProcess 
                                              then 512*1024*1024
                                              else  16*1024*1024
                        let tid = new Thread(new ThreadStart(f),stackSizeInByes)
                        do tid.Name <- "inference thread"
                        let _ = tid.Start()
                        try 
                           let (typedCoreSchema,logOdds,db) as res = tcs.Task.Result 
                           //let (distDTO,knowDTO) = dbToKnowAndDist typedCoreSchema db
                           return (typedCoreSchema,logOdds, db)
                        with  // unwrap any aggregate exception
                        | :? System.AggregateException as e -> return raise e.InnerException 
                        | e -> return raise e               
            })
            let! schema, le, db = tok
            do! Async.SwitchToContext(ctx)
            return schema, le, db
         } |> Async.RunSynchronously
      schema,le, db

