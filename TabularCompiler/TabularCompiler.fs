namespace MicrosoftResearch.Infer.Tabular


open MicrosoftResearch.Infer
open MicrosoftResearch.Infer.Models
open MicrosoftResearch.Infer.Distributions
open MicrosoftResearch.Infer.Collections
open Syntax

module FArray = Microsoft.FSharp.Collections.Array

module TabularCompiler =
 open Target
 open Compiler
 open System.Threading

 exception AbortException 
 exception CheckException of string

 let defaultRandomSeed = 123567

 let rec filterQueries  (TE:Declaration list)(tables:Declaration list)= 
      match tables with
      | [] -> List.rev TE                                                        
      | (Declaration(tid,table)::tables) -> 
        let rec trColumns CE columns  =
          match columns with
          |  [] ->  filterQueries (Declaration(tid,List.rev CE)::TE) tables
          | ((cn,{Type=T;Markup=m}) as col)::rest ->
            if Types.det T = Qry 
            then trColumns CE rest
            else trColumns (col::CE) rest
        trColumns [] table




 open TypedDTO
 let compileNew (verbose: bool, collectStats:bool, extractCode:string, file:string,name:string,typedCoreSchema:Schema, dbin:DataBase, algo:IAlgorithm option, breakSymmetry, numberOfIterations : int option, randomSeed,cts: CancellationToken option) =
      MicrosoftResearch.Infer.Maths.Rand.Restart(randomSeed)
      let tmpDir= System.IO.Path.GetTempPath() + @"Tabular"
      if not(System.IO.Directory.Exists tmpDir)
      then  System.IO.Directory.CreateDirectory(tmpDir) |> ignore
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
      ie.Compiler.GeneratedSourceFolder <- System.IO.Path.Combine(tmpDir,"GeneratedSource")
      lazyWriteLine <| lazy (sprintf "Generated Source Folder: %s" (ie.Compiler.GeneratedSourceFolder))
      ie.NumberOfIterations <- defaultArg numberOfIterations 10
      let infer (dbin: DataBase)  = 
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

           // REVIEW : do we still need this workaround in Infer.NET 2.5?
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

           
           ie.remove_ProgressChanged(f)
           do sw.Stop()
           let inferenceTime = sw.ElapsedMilliseconds 
           
           
           let code = lazy (try Extraction.toCompileUnit dbout "Tabular" name typedCoreSchema s with e -> sprintf "/*C# code generation failed %s */" (e.ToString()))
           lazyWriteLine <| code
          
           if (extractCode <> null) then 
               System.IO.File.WriteAllText(extractCode,code.Force())

           let (dbout, interpretedQueryTime,compiledQueryTime) =
               if not (QueryCompiler.schemaHasQuery false typedCoreSchema)
               then 
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
             let file = System.IO.Path.GetFileNameWithoutExtension(file)
             let header = "% name rows cells compileTime inferenceTime interpretedQueryTime compiledQueryTime algorithm iterations totalTime\n"
             let stats = sprintf "\\\\%s\n %s & %O & %O & %O & %O & %O & %O & %s & %O & %O \\\\ \n" file name rows cells compileTime inferenceTime interpretedQueryTime compiledQueryTime (ie.Algorithm.ShortName) (ie.NumberOfIterations) totalTime
             let row = header+stats 
             lazyWriteLine <| lazy row
             let statsfile = System.IO.Path.Combine(tmpDir,"allstats.txt")
             System.IO.File.AppendAllText(statsfile,row)
             let statfile = System.IO.Path.Combine(tmpDir,name+".txt")
             System.IO.File.AppendAllText(statfile,row)
             //let fulltex = Tex.schemaToStr (typedCoreSchema)
             //let texfile = System.IO.Path.GetTempPath() + @"Tabular\"+name+".tex"
             //System.IO.File.WriteAllText(texfile,sprintf "%s\n\n%s\n\n" fulltex (tex.Force()))
           (typedCoreSchema,eD.LogOdds,dbout)

         finally
          () 
      infer dbin

 open System.Threading


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

