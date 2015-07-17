#if INTERACTIVE
#I @"bin\Debug" // wherever the .dll files are located
#r @"Tabular.dll";
#r @"TaskPane.dll";
#else
module ModelTests
#endif

open NUnit.Framework
open MicrosoftResearch.Infer.Tabular
open MicrosoftResearch.Infer.Tabular.Syntax

[<Test>]
let ModelLibraryDAConcreteWorks () =
   //Assert.Equal<Declaration seq> ([] :> Declaration seq, [])
   try
      //let da = NewModels.DA
      let dac = NewModels.DAConcrete()
      ()
   with |e ->  failwith e.Message

   Assert.AreSame (NewModels.DAConcrete() , NewModels.DAConcrete() )
   
[<Test>]
let ModelLibraryDAREConcreteWorks () =
   Assert.AreSame (NewModels.DAREConcrete() , NewModels.DAREConcrete()  )

   
open System
open System.IO
open System.Linq

//
//open System.Collections.Generic
//open DataLayer.IO
////
////[<Fact>]
////let testOleDB () = 
////    use a = new OleDBLoader(__SOURCE_DIRECTORY__ + "\Recommendations.accdb")  :> ILoader
////    let dbSchema = a.GetSchema()
////    let r = a.ReadEntities(dbSchema)
////    let colnames, data = r.["Users"]
////    let d = data |> Seq.toArray
////    Assert.NotEmpty(d)
//
//[<Fact>]
//let testDAORead () = 
//    use fake = new FakeLoader()  :> ILoader
//    use a = new DataLayer.AccessConnectors.DAOLoader(__SOURCE_DIRECTORY__ + "\Recommendations.accdb")  :> ILoader
//    let d = let data = a.ReadConcrete "Users" [ "UserCluster" ]
//            data |> Seq.toArray
//    Assert.NotEmpty(d)
//
//[<Fact>]
//let testFakeOK () = 
//    let a = new FakeLoader()  :> ILoader
//    let d = let data =a.ReadConcrete "Users" [ "UserCluster" ]
//            data |> Seq.toArray
//    Assert.NotEmpty(d)
//
//[<Fact>]
//let testDAOReadSchema () = 
//    use a = new DataLayer.AccessConnectors.DAOLoader(__SOURCE_DIRECTORY__ + "\Recommendations.accdb")  :> ILoader
//    let dbSchema = a.GetSchema()
//    let (tname, cols) = dbSchema.Tables.Head
//    let data = a.ReadConcrete(tname) (cols.Columns |> List.map fst )
//    let d = data |> Seq.toArray
//    Assert.NotEmpty(d)
//
//[<Fact>]
//let testDAOGetSchema () = 
//    use a = new DataLayer.AccessConnectors.DAOLoader(__SOURCE_DIRECTORY__ + "\Recommendations.accdb")  :> ILoader
//    let dbSchema = a.GetSchema()
//    printfn "%A" dbSchema
//    Assert.Equal(1,1)
