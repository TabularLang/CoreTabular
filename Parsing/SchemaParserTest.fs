#if INTERACTIVE
#I @"bin\Debug" // wherever the .dll files are located
#r @"Tabular.dll";
#r @"TaskPane.dll";
#else
module ParserTests
#endif

open NUnit.Framework
open MicrosoftResearch.Infer.Tabular.SchemaParser
open MicrosoftResearch.Infer.Tabular.Syntax

//[<Fact>]
//let should_fail () = Assert.Equal (1, 2)



[<Test>]
let ``Sheet parser test1`` () =
   let (err, table,_) = readTable [  ("tablename","","","" , None)
                                     ("colname1","int","Latent","CGaussian()" , None)
                                     ("colname2","int","Latent","CGaussian()" , None)
                                     ("colname3","int","Latent","CGaussian()" , None)
                                     ("colname4","int","Latent","CGaussian()" , None) 
                                 ]
   Assert.AreEqual(err, "")
   let (Declaration(Table(tname, _),table)) = table.Value
   Assert.AreEqual         (table |> List.length, 4)

[<Test>]
let ``Sheet parser test2`` () =
   let (err, schema,_,_) =
                        readSchema [ ("tablename","","",""  , None)
                                     ("colname1","int","Latent","CGaussian()"  , None)
                                     ("colname2","int","Latent","CGaussian()"  , None)
                                     ("colname3","int","Latent","CGaussian()"  , None)
                                     ("colname4","int","Latent","CGaussian()"  , None) 
                                     ("","","",""  , None)
                                     ("","","",""  , None)
                                     ("","","",""  , None)
                                     ("","","",""  , None)
                                 ]

   Assert.AreEqual (err, "")
   Assert.AreEqual         (schema.Value |> List.length, 1)
   schema.Value |> List.map (fun t ->  let (Declaration(Table(tname,_), table)) = t
                                       Assert.AreEqual         (table |> List.length, 4))

[<Test>]
let ``Sheet parser test3`` () =
   let (err, schema,_,_)= 
                        readSchema  [ ("tablename","","","" , None )
                                      ("colname1","int","Latent","CGaussian()" , None)
                                      ("colname2","int","Latent","CGaussian()" , None)
                                      ("colname3","int","Latent","CGaussian()" , None)
                                      ("colname4","int","Latent","CGaussian()" , None) 
                                      ("","","","" , None)
                                      ("","","","" , None)
                                      ("","","","" , None)
                                      ("","","","" , None)
                                      ("anothertablename","","","" , None )
                                      ("colname4","int","Latent","CGaussian()" , None)
                                      ("colname4","int","Latent","CGaussian()" , None)
                                      ("colname4","int","Latent","CGaussian()" , None)
                                      ("colname4","int","Latent","CGaussian()" , None)
                                      ("","","",""  , None)
                                      ("anothertablename with no column","","",""  , None)
                                      ("","","",""  , None)
                                 ]
   Assert.AreEqual (err, "")
   Assert.AreEqual         (schema.Value |> List.length, 3)
   schema.Value |> List.mapi (fun i t ->let (Declaration(Table(tname,_),table)) = t
                                        Assert.AreEqual(table |> List.length, if i < 2 then 4 else 0))


let other           =         [ ("Players","" ,"","" , None)
                                ("Skill", "real", "latent", "CGaussian()"   , None)   
                                ("","","","" , None)
                                ("Matches","","","" , None)
                                ("Player1", "link(Players)", "input","" , None)
                                ("Player2", "link(Players)", "input","" , None)
                            //    ("Perf1", "real", "latent", "CGaussian(mean=Players[Player1].Skill,variance=1.0)")
                            //    ("Perf2", "real", "latent", "CGaussian(mean=Players[Player2].Skill,variance=1.0)")
                                ("Perf1", "real", "latent", "CGaussian(mean=Player1.Skill,variance=1.0)" , None)
                                ("Perf2", "real", "latent", "CGaussian(mean=Player2.Skill,variance=1.0)" , None)
                                ("Win1", "bool", "output", "Perf1 > Perf2" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None) ]


[<Test>]
let ``Sheet parser test4`` () =
   let (err, schema,_,_) = readSchema other
   Assert.AreEqual (err, "")
   Assert.AreEqual         (schema.Value |> List.length, 2)
   schema.Value |> List.mapi (fun i t -> let (Declaration(Table(tname, _),table)) = t
                                         Assert.AreEqual         (table |> List.length, if i = 0 then 1 else 5))



let another         =         [ ("Players","" ,"","" , None)
                                ("Skill", "real", "latent", "GaussianFromMeanAndPrecision(25.0, 10.0)" , None  ) 
                                ("","","","" , None)
                                ("Matches","","","" , None)
                                ("Player1", "link(Players)", "input","" , None)
                                ("Player2", "link(Players)", "input","" , None)
                            //    ("Perf1", "real", "latent", "CGaussian(mean=Players[Player1].Skill,variance=1.0)")
                            //    ("Perf2", "real", "latent", "CGaussian(mean=Players[Player2].Skill,variance=1.0)")
                                ("Perf1", "real", "latent", "GaussianFromMeanAndPrecision(Player1.Skill,1.0)" , None)
                                ("Perf2", "real", "latent", "GaussianFromMeanAndPrecision(Player2.Skill,1.0)" , None)
                                ("Win1", "bool", "output", "Perf1 > Perf2" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None)
                                ("","","","" , None) ]
[<Test>]
let ``Sheet parser test5`` () =
   let (err, schema,_,_) = readSchema another
   Assert.AreEqual (err, "")
   Assert.AreEqual         (schema.Value |> List.length, 2)
   schema.Value |> List.mapi (fun i t -> let (Declaration(Table(tname, _),table)) = t
                                         Assert.AreEqual         (table |> List.length, if i = 0 then 1 else 5))

