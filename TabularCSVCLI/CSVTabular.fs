namespace MicrosoftResearch.Infer.Tabular

module CSVTabular =
    open Syntax
    open System.IO
    open System.Text.RegularExpressions
    open MicrosoftResearch.Infer

    let getFreshName fExists outname = 
        let rec go optNum =
            let name = outname + (match optNum with | Some i -> i.ToString() | _ -> "")
            match fExists (name) with
            | true -> go (optNum |>  Option.fold(fun _ i -> Some (i + 1)) (Some 1) ) 
            | false -> name
        go None
    let getFreshOutputDirName = getFreshName System.IO.Directory.Exists  
    let getFreshFileName      = getFreshName System.IO.File.Exists
    

    let getSchema separator (modelFilePath : string) =
        let clean (s:string)= s |> String.collect(fun c -> if c = '\"' then "" else c.ToString())
        let mclean (a,b,c,d,e) = clean a, clean b, clean c, clean d, e
        let lines = File.ReadAllLines(modelFilePath) 
                  |> Array.map  (fun   l -> Regex.Split(l, separator + """(?=(?:[^\"]|\"[^\"]*\")*$)""")) //balanced by \" split
                  |> Array.mapi (fun i l-> if l.Length <> 4 then failwith (sprintf "unexpected line %A" i) else mclean(l.[0],l.[1],l.[2],l.[3],None))
                  |> Array.toList
        SchemaParser.readSchema (List.append lines  ([("","","","",None)]))


    open System.Collections
    open System.Linq
    open System.Collections.Generic
    open SchemaConstants 
    
    let approxEq (s1:string) (s2:string) = 
      let eq1 (s1:string) (s2:string) = s1.ToLowerInvariant().Equals(s2.ToLowerInvariant()) 
      eq1 s1 s2 || eq1 (s1.Replace(" ", ""))  (s2.Replace(" ", ""))
    let coerce (v:'a) = System.Convert.ChangeType(v, (typeof<'b>)) :?> 'b

    let  AvailableDefaultAlgo   = [new ExpectationPropagation() :> IAlgorithm
                                   new VariationalMessagePassing():> IAlgorithm
                                   new GibbsSampling():> IAlgorithm]

    let  tryFindAlgo  algorithmLabel=  algorithmLabel |> (fun algo -> try AvailableDefaultAlgo |> List.find(fun a -> approxEq a.Name (algorithmLabel |> string))
                                                                      with |e -> failwith (sprintf "can not find algo '%A' specified in the settings" algorithmLabel))

    let getAdHocOptions (settings:Map<string,obj>) = 
        let settings = settings.AsEnumerable() 
        let  tryFind name =  settings |> Seq.tryFind(fun kv -> approxEq name  kv.Key) |> Option.map(fun kv -> kv.Value)
        let oAlgo = tryFind algorithmLabel
                    |> Option.map tryFindAlgo
        let oIterations = tryFind iterationsLabel
                          |> Option.map(fun iterations -> try  coerce iterations : int
                                                          with |e -> failwith (sprintf "can not convert the setting '%A' of value %A to int" iterationsLabel iterations))
        let oSaveInput = tryFind saveinpuLabel
                         |> Option.map (fun saveInput -> try coerce saveInput : bool
                                                         with |e -> failwith (sprintf "can not convert the setting '%A' of value %A to bool" saveinpuLabel saveInput))
        oAlgo, oIterations, oSaveInput


    open TypedDTO    
    let readCSVData typedCoreSchema (dirPath: string) = 
     read typedCoreSchema  (CSVConverter()) (CSVSource.read dirPath)

    let saveModeToCSV outputDir filename schema = 
        let res = Pretty.toPositional2DStr schema  |> List.toArray
        let ares : obj [,] =  Array2D.init (res.Length) 4 (fun i j -> let (a,b,c,d) = res.[i] in (match j with | 0 -> a | 1 -> b | 2 -> c | 3 -> d) |> box)
        TypedDTO.write2DArrayToCSV outputDir filename   ares
