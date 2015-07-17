#if INTERACTIVE
#I @"bin\Debug" // wherever the .dll files are located
#r @"Infer.Runtime.dll";
#r @"Infer.Compiler.dll";
#r @"Infer.Fun.dll";
#r @"DataLayer.dll";
#r @"MarkupFunLayer.dll";
#else
module testsRecommender
#endif

//open Xunit
//open System.Collections.Generic
//open MicrosoftResearch.Infer.Fun
//open MicrosoftResearch.Infer.Fun.Learner
//open MicrosoftResearch.Infer.Fun.FSharp.Syntax
//open MicrosoftResearch.Infer
//open MicrosoftResearch.Infer.Fun.Learner
//open MicrosoftResearch.Infer.Distributions
//open MicrosoftResearch.Infer.Maths // Access to Vector and PositiveDefiniteMatrix, etc.
//open Microsoft.FSharp.Reflection
//open MicrosoftResearch.Infer.Tabular
//open MicrosoftResearch.Infer.Tabular.Tabular
//open MicrosoftResearch.Infer.Tabular.DataLayer
////open MicrosoftResearch.Infer.Tabular.OldService
////open IO
//
//
////You might need to increase stack size.
////"C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\Tools\vsvars32.bat" editbin /STACK:100000000  "C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE\CommonExtensions\Microsoft\TestWindow\vstest.executionengine.x86.exe"
//
//
//type D<'T> = IDistribution<'T>
//let TABLESIZE = 10
//let sizeMap tables = List.fold (fun (map:Map<string,int>) (name, _) -> map.Add(name,TABLESIZE)) (Map.empty) tables
//let sizeMap' tables = List.fold (fun (map:Map<string,int>) (name, _) -> map.Add(name,if name = "Ratings" then 500 else TABLESIZE)) (Map.empty) tables
//let integers = Seq.unfold(fun i -> Some(i, i+1)) 0
//module S = Fun.FSharp.Syntax
//module FArray = Microsoft.FSharp.Collections.Array
//
////[<Fact>]
////let testusermovieloadermodel () = 
////    let userTable = (new UserMovieLoader() :> ILoader).GetSchema()
////    let model = trDB userTable (sizeMap userTable.Tables)
////    Assert.True(model.Test())
//
//type userW  = ((UNIT*Vector)*(double[]))*(Vector[])
//type userX  =     UNIT[]
//type userY  =   ((UNIT*bool)*int)[]
//type userZ  =    (UNIT*int)[]
//type userDW = ((D<UNIT>*D<Vector>)*(D<double>[]))*(D<Vector>[])
//type userDY =  ((D<UNIT>*D<bool>)*D<int>)[]
//type userDZ =   (D<UNIT>*D<int>)[]
//
//[<Fact>]
//let testUserTable () = //does not work
//    let userTable = [(Recommender.Schema.Tables.Head)]
//    let latentModel = trDB {Name = "Users"; Tables = userTable} (sizeMap userTable)
//    printfn "%s" (latentModel.ToString())
//
//    let latentModelTyped = latentModel.LatentModel<UNIT*userW,UNIT*userX,UNIT*userY,UNIT*userZ, D<UNIT>*userDW, D<UNIT>*userDY, D<UNIT>*userDZ>()
//    let sampler = Sampler.FromModel(latentModelTyped.Model);
//    printfn "w_true: %A " sampler.Parameters
//
//    let x = (ONE,[| for i in 1..10 -> ONE |])
//
//    let (sy,sz) as sample = sampler.Sample x
//    printfn "%A" (sample)
//    
//    let learner = LearnerFromModel(latentModelTyped.Model); 
//    learner.Train(x,sample) 
//    let d = learner.Posterior()
//    printfn "%A" d
//    Assert.True(true)
//
//[<Fact>]
//let testUserTableLatent () = 
//    let userTable = [(Recommender.Schema.Tables.Head)]
//    let latentModel = trDB {Name = "Users"; Tables = userTable} (sizeMap userTable)
//    printfn "%s" (latentModel.ToString())
//
//    let latentModelTyped = latentModel.LatentModel<UNIT*userW,UNIT*userX,UNIT*userY,UNIT*userZ, D<UNIT>*userDW, D<UNIT>*userDY, D<UNIT>*userDZ>()
//    let sampler = Sampler.FromModel(latentModelTyped.Model);
//    printfn "w_true: %A " sampler.Parameters
//
//    let x = (ONE,[| for i in 1..100 -> ONE |])
//    let (sy,sz) as sample = sampler.Sample x
//
//    printfn "sy\n %A" sy
//    printfn "sz\n %A" sz
//    printfn "w_true: %A " sampler.Parameters
//    
//    let l:LatentLearner.ILatentLearner< D<UNIT>*userDW,_,_,_,D<UNIT>*userDY,D<UNIT>*userDZ> =
//         LatentLearner.LearnerFromModel(latentModelTyped.Model) 
//    let (dw,dz) = l.TrainPredict(x,sy) 
//    printfn "\nw_true = \n %A" sampler.Parameters
//    printfn "\ndw = \n %A" dw 
//    printfn "\ndz = \n%A" dz 
//    Assert.True(true)
//
//
//
//type movieX = UNIT[]
//type movieW = ((((UNIT*Vector)*(Vector[]))*(Vector[])))
//type movieY = ((UNIT*int)*int)[]
//type movieDY = ((D<UNIT>*D<int>)*D<int>)[]
//type movieZ = (UNIT*int)[]
//type movieDZ = (D<UNIT>*D<int>)[]
//type movieDW = (((D<UNIT>*D<Vector>)*(D<Vector>[]))*(D<Vector>[]))
//
//
//[<Fact>]
//let testMovieTableLatent () = 
//    let table = [Recommender.Schema.Tables.Tail.Head]
//    let latentModel = trDB {Name = "Movies"; Tables = table} (sizeMap table)
//    printfn "%s" (latentModel.ToString())
//
//    let latentModelTyped = latentModel.LatentModel<UNIT*movieW,UNIT*movieX,UNIT*movieY,UNIT*movieZ, D<UNIT>*movieDW, D<UNIT>*movieDY, D<UNIT>*movieDZ>()
//    let sampler = Sampler.FromModel(latentModelTyped.Model);
//    printfn "w_true: %A " sampler.Parameters
//
//    let x = (ONE,[| for i in 1..100 -> ONE |])
//    let (sy,sz) as sample = sampler.Sample x
//
//    printfn "sy\n %A" sy
//    printfn "sz\n %A" sz
//    printfn "w_true: %A " sampler.Parameters
//    
//    let l:LatentLearner.ILatentLearner< D<UNIT>*movieDW,_,_,_,D<UNIT>*movieDY,D<UNIT>*movieDZ> =
//         LatentLearner.LearnerFromModel(latentModelTyped.Model) 
//    let (dw,dz) = l.TrainPredict(x,sy) 
//    printfn "\nw_true = \n %A" sampler.Parameters
//    printfn "\ndw = \n %A" dw 
//    printfn "\ndz = \n%A" dz 
//    Assert.True(true)
//
//
//type userMovieW   = (((UNIT*userW)*movieW))
//type userMovieX   = (((UNIT*userX)*movieX))
//type userMovieY   = (((UNIT*userY)*movieY))
//type userMovieZ   = (((UNIT*userZ)*movieZ))
//type userMovieDW  = (((D<UNIT>*userDW)*movieDW))
//type userMovieDY  = (((D<UNIT>*userDY)*movieDY))
//type userMovieDZ  = (((D<UNIT>*userDZ)*movieDZ))
//
//[<Fact>]
//let testUserMoviesTables () = 
//    let tables = let userTable, moviesTable = Recommender.Schema.Tables.Head, Recommender.Schema.Tables.Tail.Head in [userTable;moviesTable]
//    let sizeMap = (sizeMap tables)
//    let latentModel = trDB {Name = "UsersMovies"; Tables = tables } sizeMap
//
//    let latentModelTyped = latentModel.LatentModel<userMovieW,userMovieX, userMovieY, userMovieZ, 
//                                                             userMovieDW,userMovieDY,userMovieDZ>()
//
//    let x = ((ONE, [| for i in 1..sizeMap.["Users"]  -> ONE |]),
//                   [| for i in 1..sizeMap.["Movies"] -> ONE |])
//
//    let sampler = Sampler.FromModel(latentModelTyped.Model);
//    let (sy,sz) as sample = sampler.Sample x
//
//    printfn "sy\n %A" sy
//    printfn "sz\n %A" sz
//    printfn "w_true: %A " sampler.Parameters
//    
//    let l:LatentLearner.ILatentLearner<userMovieDW,_,_,_,userMovieDY, userMovieDZ> =
//         LatentLearner.LearnerFromModel(latentModelTyped.Model) 
//    let (dw,dz) = l.TrainPredict(x,sy) 
//    printfn "\nw_true = \n %A" sampler.Parameters
//    printfn "\ndw = \n %A" dw 
//    printfn "\ndz = \n%A" dz 
//    Assert.True(true)
//
//
//
//type ratingsW =  ((UNIT * Vector) * Vector) * Vector[][]
//type ratingsX =    UNIT[]
//type ratingsY = (((UNIT * int) * int) * int)[]
//type ratingsZ =    UNIT[]
//type ratingsDW =  ((D<UNIT> * D<Vector>) * D<Vector>) * D<Vector>[][]
//type ratingsDY = (((D<UNIT>*D<int>)*D<int>)*D<int>)[]
//type ratingsDZ =    D<UNIT>[]
//type RecommenderX =  (((UNIT*userX)*movieX)*ratingsX)
//type RecommenderY  = (((UNIT*userY)*movieY)*ratingsY)
//type RecommenderZ  = (((UNIT*userZ)*movieZ)*ratingsZ)
//type RecommenderW  = (((UNIT*userW)*movieW)*ratingsW)
//type RecommenderDW =  (((D<UNIT>*userDW)*movieDW)*ratingsDW)
//type RecommenderDY =  (((D<UNIT>*userDY)*movieDY)*ratingsDY)
//type RecommenderDZ =  (((D<UNIT>*userDZ)*movieDZ)*ratingsDZ)
//
//let WRecommenderTotalContrast (sizeMap:Map<string,int>) = 
//    let userW : userW =   let nil = ONE 
//                          let vcl = Lib.VectorFromArray([|1.0;1.0;0.001;0.001|]) // female, male, either, either
//                          let vgen = [| 0.001;0.99; 0.5;0.5 |]
//                          let vage = [| Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> if i <  40 then 0.025 else 0.0|]) // young (female)
//                                        Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> if i >= 60 then 0.025 else 0.0|]) // old (male)
//                                        Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> 0.01 |])
//                                        Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> 0.01 |])
//                                     |]
//                          (((nil,vcl),vgen),vage)
//    let moviesW: movieW = let nil = ONE 
//                          let vcl = Lib.VectorFromArray([|1.0;1.0;0.001;0.001|]) // female, male, either, either
//                          let vgenre= [| Lib.VectorFromArray([| for i in 0 .. 6 - 1 -> if i <  2 then 0.5 else 0.0|]) // young (female)
//                                         Lib.VectorFromArray([| for i in 0 .. 6 - 1 -> if i >= 4 then 0.5 else 0.0|]) // old (male)
//                                         Lib.VectorFromArray([| for i in 0 .. 6 - 1 -> 1.0 / 6.0 |])
//                                         Lib.VectorFromArray([| for i in 0 .. 6 - 1 -> 1.0 / 6.0 |])
//                                     |]
//                          let vyear = [| Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> if i < 40 then 0.025 else 0.0|]) // young (female)
//                                         Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> if i > 60 then 0.025 else 0.0|]) // old (male)
//                                         Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> 0.01 |])
//                                         Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> 0.01 |])
//                                     |]
//                          (((nil,vcl),vgenre),vyear)                                
//    let ratingsW: ratingsW = 
//                          let nil = ONE 
//                          let wuserid =  Lib.VectorFromArray([| for i in 0 ..  sizeMap.["Users"]-1 -> 1.0 / (float) (sizeMap.["Users"]) |]) 
//                          let wmovieid=  Lib.VectorFromArray([| for i in 0 .. sizeMap.["Movies"]-1 -> 1.0 / (float) (sizeMap.["Movies"]) |]) 
//                          let wratings =
//                                   [| for m in 0 .. 3 -> 
//                                      [| for u in 0 .. 3 -> 
//                                         if m = u then
//                                            Lib.VectorFromArray([| 0.0; 0.0 ; 0.0 ; 0.0; 1.0|]) // young (female)
//                                         else 
//                                            Lib.VectorFromArray([| 1.0; 0.0 ; 0.0 ; 0.0; 0.0|])
//                                      |]
//                                   |]
//                          (((nil,wuserid),wmovieid),wratings)                                
//    (((ONE,userW),moviesW),ratingsW):RecommenderW
//
//let WRecommenderRatingContrast (sizeMap:Map<string,int>) = 
//    let userW : userW =   let nil = ONE 
//                          let vcl = Lib.VectorFromArray([|1.0;1.0;0.001;0.001|]) // female, male, either, either
//                          let vgen = [| 0.001;0.99; 0.5;0.5 |]
//                          let vage = [| Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> if i <  40 then 0.025 else 0.0|]) // young (female)
//                                        Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> if i >= 60 then 0.025 else 0.0|]) // old (male)
//                                        Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> 0.01 |])
//                                        Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> 0.01 |])
//                                     |]
//                          (((nil,vcl),vgen),vage)
//    let moviesW: movieW = let nil = ONE 
//                          let vcl = Lib.VectorFromArray([|1.0;1.0;0.001;0.001|]) // female, male, either, either
//                          let vgenre= [| Lib.VectorFromArray([| for i in 0 .. 6 - 1 -> if i <  2 then 0.5 else 0.0|]) // young (female)
//                                         Lib.VectorFromArray([| for i in 0 .. 6 - 1 -> if i >= 4 then 0.5 else 0.0|]) // old (male)
//                                         Lib.VectorFromArray([| for i in 0 .. 6 - 1 -> 1.0 / 6.0 |])
//                                         Lib.VectorFromArray([| for i in 0 .. 6 - 1 -> 1.0 / 6.0 |])
//                                     |]
//                          let vyear = [| Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> if i < 40 then 0.025 else 0.0|]) // young (female)
//                                         Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> if i > 60 then 0.025 else 0.0|]) // old (male)
//                                         Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> 0.01 |])
//                                         Lib.VectorFromArray([| for i in 0 .. 100 - 1 -> 0.01 |])
//                                     |]
//                          (((nil,vcl),vgenre),vyear)                                
//    let ratingsW: ratingsW = 
//                          let nil = ONE 
//                          let wuserid =  Lib.VectorFromArray([| for i in 0 ..  sizeMap.["Users"]-1 -> 1.0 / (float) (sizeMap.["Users"]) |]) 
//                          let wmovieid=  Lib.VectorFromArray([| for i in 0 .. sizeMap.["Movies"]-1 -> 1.0 / (float) (sizeMap.["Movies"]) |]) 
//                          let wratings =
//                                   [| for m in 0 .. 3 -> 
//                                      [| for u in 0 .. 3 -> 
//                                         if m = u then
//                                            Lib.VectorFromArray([| 0.0; 0.0 ; 0.0 ; 0.0; 1.0|]) // young like young, old like old
//                                         else 
//                                            Lib.VectorFromArray([| 1.0; 0.0 ; 0.0 ; 0.0; 0.0|]) // otherwise does not like
//                                      |]
//                                   |]
//                          (((nil,wuserid),wmovieid),wratings)                                
//    (((ONE,userW),moviesW),ratingsW):RecommenderW
//
//
//let inline normalize fa = let s = FArray.sum fa in FArray.map(fun e -> e / s) fa
//
//let WRecommenderDemo (sizeMap:Map<string,int>) = 
//    let rnd = System.Random()
//    let certainty = 10.
//    let userW : userW =   let nil = ONE 
//                          let vcl = Lib.VectorFromArray([|20.0;20.0;5.|]) // female, male, either, either
//                          let vgen = [| 0.2;0.8; 0.4 |]
//                          let vage = [| Lib.VectorFromArray(FArray.init 100 (fun i -> if i <  40 then 40. else 20.) |> normalize) 
//                                        Lib.VectorFromArray(FArray.init 100 (fun i -> if i >= 60 then 40. else 20.) |> normalize) 
//                                        Lib.VectorFromArray(FArray.init 100 (fun i -> 1. + rnd.NextDouble()) |> normalize) 
//                                     |]
//                          (((nil,vcl),vgen),vage)
//    let moviesW: movieW = let nil = ONE 
//                          let vcl = Lib.VectorFromArray([|1000.0;1000.0;5.1;5.1;5.1|]) // female, male, either, either
//                          let vgenre= [| Lib.VectorFromArray(FArray.init 6 (fun i -> if i <  2 then 40. else 20.) |> normalize) 
//                                         Lib.VectorFromArray(FArray.init 6 (fun i -> if i >= 4 then 40. else 20.) |> normalize) 
//                                         Lib.VectorFromArray(FArray.init 6 (fun i -> 1. + rnd.NextDouble()) |> normalize) 
//                                         Lib.VectorFromArray(FArray.init 6 (fun i -> 1. + rnd.NextDouble()) |> normalize) 
//                                         Lib.VectorFromArray(FArray.init 6 (fun i -> 1. + rnd.NextDouble()) |> normalize) 
//                                     |]
//                          let vyear = [| Lib.VectorFromArray(FArray.init 100 (fun i -> if i <  40 then 40. else 20.) |> normalize) 
//                                         Lib.VectorFromArray(FArray.init 100 (fun i -> if i >= 60 then 40. else 20.) |> normalize) 
//                                         Lib.VectorFromArray(FArray.init 100 (fun i -> 1. + rnd.NextDouble()) |> normalize) 
//                                         Lib.VectorFromArray(FArray.init 100 (fun i -> 1. + rnd.NextDouble()) |> normalize) 
//                                         Lib.VectorFromArray(FArray.init 100 (fun i -> 1. + rnd.NextDouble()) |> normalize) 
//                                     |]
//                          (((nil,vcl),vgenre),vyear)                                
//    let ratingsW: ratingsW = 
//                          let nil = ONE 
//                          let wuserid =  Lib.VectorFromArray([| for i in 0 ..  sizeMap.["Users"]-1 -> 1.0 / (float sizeMap.["Users"]) |]) 
//                          let wmovieid=  Lib.VectorFromArray([| for i in 0 .. sizeMap.["Movies"]-1 -> 1.0 / (float sizeMap.["Movies"]) |]) 
//                          let wratings =
//                                   [| for m in 0 .. 4 -> 
//                                      [| for u in 0 .. 2 -> 
//                                         if m = u then
//                                            Lib.VectorFromArray(FArray.init 5 (fun i -> (float i+1.) * rnd.NextDouble()) |> normalize)
//                                         else 
//                                            Lib.VectorFromArray([| 1.0; 1.0 ; 1.0 ; 1.0; 1.0|])
//                                      |]
//                                   |]
//                          (((nil,wuserid),wmovieid),wratings)                                
//    (((ONE,userW),moviesW),ratingsW):RecommenderW
//
//open QuickGraph.Serialization
//open System
//
//
//
//[<Fact>]
//let testUserTableLatentGen () = //we generate 2 distinct groups we should see them appears in the pseudo count of the posterior of dw
//    let userTable = [(Recommender.Schema.Tables.Head)]
//    let sizeMap  = sizeMap Recommender.Schema.Tables
//
//    let latentModel = trDB {Name="Users";Tables=userTable} sizeMap
//    printfn "%s" (latentModel.ToString())
//
//    let latentModelTyped = latentModel.LatentModel<UNIT*userW,UNIT*userX,UNIT*userY,UNIT*userZ, D<UNIT>*userDW, D<UNIT>*userDY, D<UNIT>*userDZ>()
//    let ((w_true,_),_) = WRecommenderTotalContrast sizeMap
//    let sampler = Sampler.FromParameter(latentModelTyped.Model,w_true);
//
//    let x = (ONE,[| for i in 1..100 -> ONE |])
//    let (sy,sz) as sample = sampler.Sample x
//
//    printfn "sy\n %A" sy
//    printfn "sz\n %A" sz
//    printfn "w_true: %A " sampler.Parameters
//    
//    let l:LatentLearner.ILatentLearner< D<UNIT>*userDW,_,_,_,D<UNIT>*userDY,D<UNIT>*userDZ> =
//         LatentLearner.LearnerFromModel(latentModelTyped.Model) 
//    let (dw,dz) = l.TrainPredict(x,sy) 
//    printfn "\nw_true = \n %A" sampler.Parameters
//    printfn "\ndw = \n %A" dw 
//    printfn "\ndz = \n%A" dz 
//    Assert.True(true)
//
//
//
//[<Fact>]
//let testRecommender () = 
//    let schema = Recommender.Schema
//    let tables = schema.Tables
//    let sizeMap  = sizeMap tables
//    let latentModel = trDB schema sizeMap
//
//    let latentModelTyped = latentModel.LatentModel<RecommenderW,RecommenderX,RecommenderY,RecommenderZ,RecommenderDW,RecommenderDY,RecommenderDZ>()
//    let w_true = WRecommenderTotalContrast sizeMap
//
//    let x = (((ONE,
//                [| for i in 1..sizeMap.["Users"] -> ONE |]),
//                [| for i in 1..sizeMap.["Movies"] -> ONE |]),
//                [| for i in 1..sizeMap.["Ratings"] -> ONE |])
//
//    let sampler = Sampler.FromParameter(latentModelTyped.Model,w_true);
//    let (sy,sz) as sample = sampler.Sample x
//
//
//    printfn "sy\n %A" sy
//    printfn "sz\n %A" sz
//    printfn "w_true: %A " sampler.Parameters
//    
//    let l:LatentLearner.ILatentLearner<RecommenderDW,_,_,_,RecommenderDY,RecommenderDZ> =
//         LatentLearner.LearnerFromModel(latentModelTyped.Model) 
//    let (dw,dz) = l.TrainPredict(x,sy) 
//    printfn "\nw_true = \n %A" w_true
//    printfn "\ndw = \n %A" dw 
//    printfn "\ndz = \n%A" dz 
//    
//    let g = latentModelTyped.GetVariableGraph()
//    let formatNode = fun (x:VariableNode)  (n:DirectedGraphML.DirectedGraphNode) -> n.Label <- x.Id+x.Dimension.ToString()
//    let formatLink = fun (x:IndexedByEdge) (e:DirectedGraphML.DirectedGraphLink) -> e.Label <- ""
//
//    g.ToDirectedGraphML(QuickGraph.Algorithms.AlgorithmExtensions.GetVertexIdentity(g),
//                        QuickGraph.Algorithms.AlgorithmExtensions.GetEdgeIdentity(g), 
//                        new Action<_,_>(formatNode), new Action<_,_>(formatLink)).WriteXml(__SOURCE_DIRECTORY__ + @"\" + "vargraph" + ".dgml")
//
//    Assert.True(true)
//
////
////[<Fact>]
////let createBDRecommender() = 
////    let schema = Recommender.Schema
////    let tables = schema.Tables
////    let sizeMap  = sizeMap tables
////    let latentModel = trDB schema sizeMap
////    let latentModelTyped = latentModel.LatentModel<RecommenderW,unit,RecommenderY,RecommenderZ,RecommenderDW,RecommenderDY,RecommenderDZ>()
////
////    let w_true  = WRecommenderTotalContrast sizeMap
////    let sampler = Sampler.FromParameter(latentModelTyped.Model,w_true);
////    let (sy,sz) as sample = sampler.Sample()
////
////    let storer  = new DAOLoader(__SOURCE_DIRECTORY__ + "\RecommendationsContrasted12.accdb") :> IStorer
////    let data = latentModelTyped.PackYZ (sy,sz)
////
////    reCreate storer (Recommender.Schema |> Database.OnlyTableWithConcrete).Tables data
////    let last (arr:_ array) =
////        let leng = arr.Length
////        arr.[leng-1]
////
////    Assert.True(true)
////
////[<Fact>]
////let createBDRecommenderDemo() = 
////    let tables = Recommender.Schema.Tables
////    let sizeMap  = sizeMap'  tables
////    let latentModel = trDB Recommender.Schema sizeMap
////    let latentModelTyped = latentModel.LatentModel<RecommenderW,unit,RecommenderY,RecommenderZ,RecommenderDW,RecommenderDY,RecommenderDZ>()
////
////    let w_true = WRecommenderDemo sizeMap
////    let sampler = Sampler.FromParameter(latentModelTyped.Model,w_true);
////    let (sy,sz) as sample = sampler.Sample()
////
////    let storer  = new DAOLoader(__SOURCE_DIRECTORY__ + "\Recommendations.accdb") :> IStorer
////    let data = latentModelTyped.PackYZ (sy,sz)
////
////
////    reCreate storer ((Recommender.Schema |> Database.OnlyTableWithConcrete).Tables) data
////    Assert.True(true)
////
////
////[<Fact>]
////let createBDRecommenderUntyped() = 
////    let schema = Recommender.Schema
////    let tables = schema.Tables
////    let sizeMap  = sizeMap tables
////    let latentModel = trDB schema sizeMap
////    
////    let latentModelTyped = latentModel.LatentModel<RecommenderW,unit,RecommenderY,RecommenderZ,RecommenderDW,RecommenderDY,RecommenderDZ>()
////    let data = latentModelTyped.Sample (WRecommenderTotalContrast sizeMap)
////
////    let storer  = new DAOLoader(__SOURCE_DIRECTORY__ + "\RecommendationsContrasted12.accdb") :> IStorer
////    reCreate storer  ((Recommender.Schema |> Database.OnlyTableWithConcrete).Tables) data
////    Assert.True(true)
//
//
// 
//
//[<Fact>]
//let testUserPack () = 
//    let userTable = [Recommender.Schema.Tables.Head]
//    let model = trDB {Name="User";Tables=userTable} (sizeMap userTable)
//    let userModel = model.LatentModel<(unit*(((unit*Vector)*(double[]))*(Vector[]))),
//                                       unit,
//                                       unit*((unit*bool)*int)[],
//                                       unit*((unit*int)[]),
//                                       (D<unit>*(((D<unit>*D<Vector>)*(D<double>[]))*(D<Vector>[]))),
//                                       (D<unit>* (   ((D<unit>*D<bool>)*D<int>          )[])), 
//                                       D<unit>*((D<unit>*D<int>)[])
//                                     >();
//
//    let sampler = Sampler.FromModel(userModel.Model);
//    printfn "w_true: %A " sampler.Parameters
//    let users = sampler.Sample();
//    let works = fst users = userModel.UnpackY((userModel.PackYZ(users)));
//    let (DTO dataout) = userModel.PackYZ(users);
//    Map.iter(fun tableName (fmap,os) -> 
//                    printf "%s \n %A" tableName (Seq.toArray os)) dataout  
//
//   (*
//  //  let users' = userModel.unpack(usery)
//  +		[1]	{MicrosoftResearch.Infer.Distributions.IDistribution<MicrosoftResearch.Infer.Maths.Vector>[4]}	object {MicrosoftResearch.Infer.Distributions.IDistribution<MicrosoftResearch.Infer.Maths.Vector>[]}
//  -		o	{Beta(1.031,1.044)[mean=0.4968]}	object {MicrosoftResearch.Infer.Distributions.Beta}
//  +		o	{Dirichlet(1.074 55.87 45.99 1.074)}	object {MicrosoftResearch.Infer.Distributions.Dirichlet}
//
//    printfn "%A" (users)
//
//    let learner = 
//         LearnerFromModel<_,_,_,_,
//                          IDistribution<unit>*
//                          (((IDistribution<unit>*IDistribution<Vector>)*(IDistribution<double>[]))*(IDistribution<Vector>[])),
//                          obj>(userModel.m); 
//    learner.Train((),users) 
//    let d = learner.Posterior()
//    printfn "%A" d
//    *)
//    Assert.True(true)
//
//
//[<Fact>]
//let selftestUsersTable () =    
//    let tables = [(Recommender.Schema.Tables.Head)]
//    let sizes = sizeMap tables
//    let model = trDB  {Name="User";Tables=tables} (sizeMap tables)
//    printfn "%s" (model.ToString())
//    let x = ((ONE,
//              [| for i in 1..sizes.["Users"] -> ONE |]))
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//[<Fact>]
//let selftestUsersMoviesTable () =    
//    let userTable = (Recommender.Schema.Tables.Head)
//    let moviesTable = (Recommender.Schema.Tables.Tail.Head)
//    let tables = [userTable;moviesTable]
//
//    let sizes = sizeMap tables
//    let model = trDB  {Name="UserMovies";Tables=tables} (sizeMap tables)
//    printfn "%s" (model.ToString())
//    let x = ((ONE,
//              [| for i in 1..sizes.["Users"] -> ONE |]),
//              [| for i in 1..sizes.["Movies"] -> ONE |])
//    let b = model.Test(x)
//    Assert.True(b)
//
//[<Fact>]
//let selftestRecommender () =    
//    let schema =  Recommender.Schema
//    let tables = schema.Tables
//
//    let sizes = sizeMap tables
//    let model = trDB schema (sizeMap tables)
//    printfn "%s" (model.ToString())
//    let x = (((ONE,
//                [| for i in 1..sizes.["Users"] -> ONE |]),
//                [| for i in 1..sizes.["Movies"] -> ONE |]),
//                [| for i in 1..sizes.["Ratings"] -> ONE |])
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//[<Fact>]
//let selftestPureRecommender () =    
//    let schema = PureRecommender.Schema
//    let tables = schema.Tables
//    let users = 20
//    let movies = 200
//
//    let sizes = [("Users",users);
//                 ("Movies",movies);
//                 ("Ratings",users*50);
//                ]|> Map.ofList
//    let sizes = sizeMap tables
//    let model = trDB schema (sizeMap tables)
//    printfn "%s" (model.ToString())
//    let users = sizes.["Users"]
//    let movies = sizes.["Movies"]
//    let dusers = new Distributions.Discrete([| for i in 0..users-1 -> 1.0/(float) users |])
//    let dmovies = new Distributions.Discrete([| for i in 0..movies-1 -> 1.0/(float) movies |])
//    let x = (((ONE,
//                [| for i in 1..sizes.["Users"] -> ONE |]),
//                [| for i in 1..sizes.["Movies"] -> ONE |]),
//                [| for i in 1..sizes.["Ratings"] -> ((ONE,dusers.Sample()),dmovies.Sample())|])
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//
//
//[<Fact>]
//let selftestRecommenderQuery () =    
//    let schema = RecommenderQuery.Schema
//    let tables = schema.Tables
//    let users = 20
//    let movies = 200
//
//    let sizes = [("Users",users);
//                 ("Movies",movies);
//                 ("Ratings",users*50);
//                 ("RatingsQuery",users)
//                ]|> Map.ofList
//    
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let users = sizes.["Users"]
//    let movies = sizes.["Movies"]
//    let dusers = new Distributions.Discrete([| for i in 0..users-1 -> 1.0/(float) users |])
//    let dmovies = new Distributions.Discrete([| for i in 0..movies-1 -> 1.0/(float) movies |])
//   // let dstars = new Distributions.Discrete([| for i in 0..5 -> 1.0/(float) movies |])
//    let x = ((((ONE,
//                [| for i in 1..sizes.["Users"] -> ONE |]),
//                [| for i in 1..sizes.["Movies"] -> ONE |]),
//                [| for i in 1..sizes.["Ratings"] -> ((ONE,dusers.Sample()),dmovies.Sample())|]),
//                [| for i in 1..sizes.["RatingsQuery"] -> ((ONE,i-1))|])
//
//
//    let b = model.Test(x)
//    Assert.True(b)