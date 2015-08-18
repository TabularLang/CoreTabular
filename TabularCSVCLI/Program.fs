namespace MicrosoftResearch.Infer.Tabular

module CLI = 
    open System
    open Printf
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open System.Runtime.InteropServices
    open ParserCLI
    open CSVTabular 


    type FSPath = Fresh of string | Reuse of string
    let mapfspath f v = match v with | Fresh s -> Fresh <| f s | Reuse s -> Reuse <| f s

    let TryGetValue (d : IDictionary<_,_>) key =
        match d.TryGetValue(key) with
        | (true, v) -> Some(v)
        | (false, _) -> None

    let writeStd (verbose:bool) (writer:IO.TextWriter) (msg:string) = 
      if verbose then 
         let tmp = Console.Out;
         Console.SetOut(writer);
         System.Console.WriteLine(msg)
         Console.SetOut(tmp);
      else 
         ()

    let pad n (msg:string) = 
      let trunc = String(msg |> Seq.truncate n  |> Seq.toArray)
      trunc.PadRight(n)

    let completePath def path = if System.IO.Path.IsPathRooted(path) then path else def + @"\" + path
    let ensureDir dirname = if not (System.IO.Directory.Exists(dirname)) then System.IO.Directory.CreateDirectory(dirname) |> ignore

    let runCLI executionDirectory oSeparator modelFileName dataDirectoryName sample oIterations oAlgo outputDirectoryName verbose descSaveCSharpCode breakSym saveTypedModels = 
        let  dataDirectoryName = dataDirectoryName |> completePath executionDirectory
        let  outputDirectoryName = mapfspath (completePath executionDirectory) outputDirectoryName 
        //phase 0 - Read schema
        let sep = if System.IO.Path.GetExtension(modelFileName) = ".txt" then "\t" else ","
        let (error, model, _, settings) = getSchema (defaultArg oSeparator sep ) (modelFileName |> completePath executionDirectory)
        //phase 1 - Check and elaborate
        let (Some(schema)) = model
        Schema.checkSchema schema 
        let (_,_,(typedCoreSchema,_)) = Elaborator.elaborate(schema)
        let (_,_,(typedFullSchema,_)) = Schema.typeSchema schema
        //phase 2 - Read data type directed
        let dbin = readCSVData typedCoreSchema dataDirectoryName
        //phase 3 - get options 
        let oAlgo, iterations, saveInput = 
            //from the model
            let modeloAlgo,modelIterations,modelSaveInput = 
                    let defIteration,defSaveInput = 60, false
                    match settings |> Option.map getAdHocOptions with 
                    | Some(oAlgo,oIterations,oSaveInput) -> (defaultArg oAlgo AvailableDefaultAlgo.[0]), defaultArg oIterations defIteration, defaultArg oSaveInput defSaveInput
                    | _ -> AvailableDefaultAlgo.[0], defIteration,defSaveInput
            //command line get priorities
            (if sample then None else Some(defaultArg oAlgo modeloAlgo)), defaultArg oIterations modelIterations, modelSaveInput
        // phase 4 - run the inference
        let outputDir = match outputDirectoryName with | Fresh s -> getFreshOutputDirName s  |Reuse s -> s
        let modelshortname = System.IO.Path.GetFileNameWithoutExtension(modelFileName)
        ensureDir outputDir

        let (typedCoreSchema,le, odb)  as res =
            TabularCompiler.compileNew(verbose,true,(if descSaveCSharpCode then outputDir + @"\" + modelshortname + ".cs" else null), 
                                        modelFileName,modelshortname,
                                        typedCoreSchema, dbin, oAlgo, breakSym, 
                                        Some iterations, TabularCompiler.defaultRandomSeed, None)
        // phase 5 - save the results
        if saveTypedModels then
            let modelName = modelshortname
            saveModeToCSV outputDir (modelName + "_typedFull.csv") typedFullSchema
            saveModeToCSV outputDir (modelName + "_typedCore.csv"     ) (Syntax.coreS (Library.prelude @ schema))

        TypedDTO.writeCSV   outputDir
                            typedCoreSchema
                            DistributionPrinter.distToString
                            odb
        //ignore <| Console.ReadLine()


    [<EntryPoint>]
    let main argv = 
        let realConsole = Console.Out
        let modelFileName      , descmodelFileName      = "model"        , "filename of model stored as CSV"  
        let dataDirectoryName  , descdataDirectoryName  = "data"         , "directory name of the input data. one CSV per table from the model. default to current directory"  
        let outputDirectoryName, descoutputDirectoryName= "output"       , "directory name to save the output. default to model name" 
        let sample             , descSample             = "sample"       , "samples the tables from the model's prior, instead of doing inference"
        let iterations         , descIterations         = "iterations"   , "number of iterations of algorithm"                                          
        let algo               , descAlgo               = "algo"         , sprintf "name of algorithm to use. infer.net supports %A " (AvailableDefaultAlgo |> List.map(fun a -> a.Name))
        let verbose            , descverbose            = "verbose"      , "verbose mode"                                          
        let breakSym           , descbreakSym           = "breaksymmetry", "break symmetries in the model"                                          
        let separator          , descSeparator          = "separator"    , """separator used in for parsing model's CSV file. default to "," """
        let saveTypedModels    , descSaveTypeModel      = "savemodels"   , "saves the intermediate core and full models"
        let saveCsharpCode     , descSaveCSharpCode     = "savecsharp"   , "saves the emitted csharp code"

        let defs = [
            {CLIArg.Command=modelFileName       ; Description= descmodelFileName        ; Required=true  ;NumberOfParameter = Fixed 1 }
            {CLIArg.Command=dataDirectoryName   ; Description= descdataDirectoryName    ; Required=false ;NumberOfParameter = Fixed 1 } 
            {CLIArg.Command=outputDirectoryName ; Description= descoutputDirectoryName  ; Required=false ;NumberOfParameter = Fixed 1 } 
            {CLIArg.Command=sample              ; Description= descSample               ; Required=false ;NumberOfParameter = Fixed 0 } 
            {CLIArg.Command=algo                ; Description= descAlgo                 ; Required=false ;NumberOfParameter = Fixed 1 } 
            {CLIArg.Command=iterations          ; Description= descIterations           ; Required=false ;NumberOfParameter = Fixed 1 } 
            {CLIArg.Command=verbose             ; Description= descverbose              ; Required=false ;NumberOfParameter = Fixed 0 } 
            {CLIArg.Command=breakSym            ; Description= descbreakSym             ; Required=false ;NumberOfParameter = Fixed 0 } 
            {CLIArg.Command=separator           ; Description= descSeparator            ; Required=false ;NumberOfParameter = Fixed 1 } 
            {CLIArg.Command=saveTypedModels     ; Description= descSaveTypeModel        ; Required=false ;NumberOfParameter = Fixed 0 } 
            {CLIArg.Command=saveCsharpCode      ; Description= descSaveCSharpCode       ; Required=false ;NumberOfParameter = Fixed 0 } 
            ]
        try
            let parsedArgs = ParserCLI.parseArgs defs argv
            let settingOrDirectory setting dir = match TryGetValue parsedArgs setting  with | Some(s) -> s.Head | _ -> dir
 
            let modelFileName       = parsedArgs.[modelFileName].Head 
            let dataDirectoryName   = settingOrDirectory dataDirectoryName  "."
            let outputDirectoryName = settingOrDirectory outputDirectoryName (System.IO.Path.GetFileNameWithoutExtension(modelFileName))
            let sample              = TryGetValue parsedArgs sample             |> Option.isSome
            let verbose             = TryGetValue parsedArgs verbose            |> Option.isSome
            let breakSym            = TryGetValue parsedArgs breakSym           |> Option.isSome
            let oIterations         = TryGetValue parsedArgs iterations         |> Option.map (fun v -> Convert.ToInt32(v.Head))
            let oSeparator          = TryGetValue parsedArgs separator          |> Option.map (fun v -> System.Text.RegularExpressions.Regex.Unescape(v.Head).Chars(0).ToString())
            let saveTypedModels     = TryGetValue parsedArgs saveTypedModels    |> Option.isSome
            let descSaveCSharpCode  = TryGetValue parsedArgs descSaveCSharpCode |> Option.isSome
            let oAlgo               = TryGetValue parsedArgs algo               |> Option.map (fun v -> tryFindAlgo v.Head)

            let writer left right = writeStd verbose realConsole (sprintf "%s:  %A" (pad 20 left) right)
            writer "modelFileName      "  modelFileName      
            writer "dataDirectoryName  "  dataDirectoryName  
            writer "outputDirectoryName"  outputDirectoryName
            writer "verbose            "  verbose            
            writer "breakSym           "  breakSym            
            writer "separator          "  oSeparator            
            writer "saveTypedModels    "  saveTypedModels            

            try 
                runCLI (System.IO.Directory.GetCurrentDirectory()) oSeparator modelFileName dataDirectoryName sample oIterations oAlgo (Fresh outputDirectoryName) verbose descSaveCSharpCode breakSym saveTypedModels 
            with | e -> 
                        Console.SetOut(realConsole);
                        System.Console.WriteLine(e.Message)
        with |e ->
            Console.SetOut(realConsole);
            System.Console.WriteLine(ParserCLI.printUsage defs)
        0
