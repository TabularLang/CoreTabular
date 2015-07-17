#if INTERACTIVE
#I @"bin\Debug" // wherever the .dll files are located
#r @"Infer.Runtime.dll";
#r @"Infer.Compiler.dll";
#r @"Infer.Fun.dll";
#r @"DataLayer.dll";
#r @"MarkupFunLayer.dll";
#else
module testsIO
#endif

//open Xunit
//open System.Collections.Generic
//open MicrosoftResearch.Infer.Tabular
//open MicrosoftResearch.Infer.Tabular.DataLayer
//open MicrosoftResearch.Infer.Tabular.OldService
//open MicrosoftResearch.Infer.Tabular.Tabular
//open MicrosoftResearch.Infer
//open MicrosoftResearch.Infer.Fun.Learner
//open MicrosoftResearch.Infer.Distributions
//open IO
//
//type D<'T> = IDistribution<'T>
//let integers = Seq.unfold(fun i -> Some(i, i+1)) 0
//module S = Fun.FSharp.Syntax
//module FArray = Microsoft.FSharp.Collections.Array
//
//
//[<Fact>]
//let testIdE1 () = 
//    // store >> read == identity
//
//    //Generate
//    let sizes = [("Students",15);("Answers",6*10);("Questions",10);("Responses",15*10);]|> Map.ofList
//    let dStudent   = sizes.["Students"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dQuestions = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dAnswers   = sizes.["Answers"]   |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let x = ((((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//                [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
//                [| for i in 1..sizes.["Questions"] -> 
//                        let answer =  (new Distributions.Discrete([| for i in 1..6 -> 1.0/(float) 6 |])).Sample()
//                        let offset =  (i-1) * 6
//                        ((((((( (ONE,"question text"), offset + answer),
//                                                       offset + 0),offset + 1), offset+2), offset+3), offset+4), offset+5)|]),
//                 [| for s in 1..sizes.["Students"] -> [| for q in 1..sizes.["Questions"] -> ((ONE,s-1),q-1) |] |]|> FArray.concat
//             )
//    let schema = DifficultyAbility.Schema
//    let latentModel = trDB schema sizes
//    
//    let dataXYZ = (latentModel.PackX x).merge (latentModel.Sample (x))  //we put X back in
//
//    //Store
//    let storer  = new DAOLoader(__SOURCE_DIRECTORY__ + @"\\testIDE1.accdb") :> IStorer
//    reCreate storer (DifficultyAbility.Schema |> Database.ConcreteSubset |> Database.ItemsTSNameTable) (DTO dataXYZ)
//    let (readData, pos2Id) = readTable (new DAOLoader(__SOURCE_DIRECTORY__ + @"\\testIDE1.accdb") :> ILoader) DifficultyAbility.Schema
//
//    let AinB (A:Dictionary<_,_>) (B:Dictionary<_,_>) = 
//        A.Keys |> Seq.iter(fun  e -> if B.ContainsKey e then B.[e] <- B.[e] - 1
//                                      else B.Add(e,-1))
//        A.Keys |> Seq.fold (fun s e -> s && B.[e] >= 0) true
//
//    //iterate over generated's table
//    //checks to see if it is contained in readData
//    let isdentical = 
//      [ for (tableName, table) in (DifficultyAbility.Schema |> Database.ConcreteSubset).Tables do
//          let (gColMap, gObjs) = dataXYZ.[tableName]
//          let (rColMap, rObjs) = readData.[tableName]
//
//          //we order the row's element according to the column order specified in the schema for both generated and read data
//          let lColNames = table.Columns |> List.map fst
//          let gObjsOrdered = gObjs |> Seq.map (fun genae  -> lColNames |> List.map (fun colName ->  genae.[gColMap.[colName]]) |> List.toArray)
//          let rObjsOrdered = rObjs |> Seq.map (fun readae -> lColNames |> List.map (fun colName -> readae.[rColMap.[colName]]) |> List.toArray)
//
//          let c = {new IEqualityComparer<obj []> with member x.Equals(a,b) = Array.zip a b |> Seq.forall (fun (a,b) -> a = b)
//                                                      member x.GetHashCode(a) = hash a}
//
//          //we count for each potentially equal element how many there are - emulates multiset
//          let generated = gObjsOrdered |> Seq.fold (fun (d:Dictionary<_,_>) e -> (if d.ContainsKey e then d.[e] <- d.[e] + 1 else d.Add(e,1));d) (new Dictionary<obj[], int>(c))
//          let read      = rObjsOrdered |> Seq.fold (fun (d:Dictionary<_,_>) e -> (if d.ContainsKey e then d.[e] <- d.[e] + 1 else d.Add(e,1));d) (new Dictionary<obj[], int>(c))
//          let one = AinB generated read
//
//          //this is destructive, we have to recreate dictionnary
//          let generated = gObjsOrdered |> Seq.fold (fun (d:Dictionary<_,_>) e -> (if d.ContainsKey e then d.[e] <- d.[e] + 1 else d.Add(e,1));d) (new Dictionary<obj[], int>(c))
//          let read      = rObjsOrdered |> Seq.fold (fun (d:Dictionary<_,_>) e -> (if d.ContainsKey e then d.[e] <- d.[e] + 1 else d.Add(e,1));d) (new Dictionary<obj[], int>(c))
//          let two = AinB generated read
//          yield one && two ] 
//      
//    Assert.True(true)