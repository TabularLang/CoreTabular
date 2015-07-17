namespace MicrosoftResearch.Infer.Tabular

module Parsing =
  open Microsoft.FSharp.Text.Lexing
  open Lexer
  open Parser
  open System
  open Syntax
  open Microsoft.FSharp.Reflection
  open Microsoft.FSharp.Quotations.Patterns

  let parseFromString (f: _ -> _ -> 'T)(s: string) =
      f Lexer.tokenize (LexBuffer<char>.FromString(s))

  let ParseExp             s = parseFromString Parser.Exp               s 
  let ParseExpList         s = parseFromString Parser.ExpList           s 
  let ParseBindings        s = parseFromString Parser.Bindings          s 
  let ParseModel           s = parseFromString Parser.Model             s 
  let ParseColumnType      s = parseFromString Parser.ColumnType        s
  let ParseTableName       s = parseFromString Parser.TableId           s
  let ParseColumnName      s = parseFromString Parser.ColumnName        s
  let ParseMarkup          s = parseFromString Parser.Markup            s
  let ParseMarkupOf        s = parseFromString Parser.MarkupOf         s
  let ParseSimpleString    s = parseFromString Parser.SimpleString      s
  let ParseSettingsTableId s = parseFromString Parser.SettingsTableId   s
  let ParseIsEmpty         s = try let _ = parseFromString Parser.EmptyCell s in true with | _ -> false

  let init() =
      Pretty.escape :=
          fun (s:string) ->
           try match Lexer.tokenize (LexBuffer<char>.FromString(s)) with 
               | token.IDENT s' -> if s' = s then s else "#\"" + s + "\"" 
               | _ -> "#\"" + s  + "\""
           with _ -> "#\"" + s  + "\""


module Test =
  open Microsoft.FSharp.Text.Lexing
  open Lexer
  open Parser
  open System
  open Syntax
  open Microsoft.FSharp.Reflection
  open Microsoft.FSharp.Quotations.Patterns
  open Parsing

  let rec recUnionFieldToString (x:obj) =
    match FSharpValue.GetUnionFields(x, x.GetType()) with 
    | case, pis ->  if pis.Length = 0 then  case.Name
                    else
                      case.Name + "("  + (pis |> Array.map (fun pi -> 
                                                if FSharpType.IsUnion (pi.GetType()) then  
                                                  recUnionFieldToString pi
                                                else
                                                  pi.ToString() ) |> String.concat "," ) + ")"

  let tee f x = f x; x

  let testModel = [ "1", MExp(Const(IntConst(1)))]
  let testConst = [ "1", IntConst(1)
                    "1.", RealConst(1.)
                    "1", IntConst(1) ]
  let testExp  = [ "1", Const(IntConst(1))]
  let testExpList  = [ "1,2,3", [ Const(IntConst(1)); Const(IntConst(2)); Const(IntConst(3))]]
  let testBindings   = [ "q=1,a=2", [ "q", Const(IntConst(1)); "a", Const(IntConst(2))] ]

  let testExp2    = [ "Beta(2.,1.)"                                , Dist(Beta, [ Const(RealConst(2.)); Const(RealConst(1.))])
                      "[for x < 50 -> 2 ]"                         , ForLoop("x", Const(IntConst(50)), Const(IntConst(2)))
                      "[for x < 50 -> Beta(2.,1.)]"                , ForLoop("x", Const(IntConst(50)), Dist(Beta, [ Const(RealConst(2.)); Const(RealConst(1.))]))
                      "[1,2,3.]"                                   , Array([Const(IntConst(1)); Const(IntConst(2)); Const(RealConst(3.))])
                      "let x = 50 in let y = 50 in 5"              , Let("x",  Const(IntConst(50)), Let("y",  Const(IntConst(50)),  Const(IntConst(5))))
                      "let x = 50 in let y = 50 in SizeOf(users)"  , Let("x",  Const(IntConst(50)), Let("y",  Const(IntConst(50)),  SizeOf("users")))
                      "(let x = 50 in let y = 50 in SizeOf(users))", Let("x",  Const(IntConst(50)), Let("y",  Const(IntConst(50)),  SizeOf("users")))  ]
  let testLookup = [ "[1,2,3.][1]", Subscript(Array([Const(IntConst(1)); Const(IntConst(2)); Const(RealConst(3.))]), Const(IntConst(1)))           
                     "([1,2,3.][1])[2]", Subscript(Subscript(Array([Const(IntConst(1)); Const(IntConst(2)); Const(RealConst(3.))]), Const(IntConst(1))), Const(IntConst(2)))
                     "([1,2,3.][1]).two", DeRef(Subscript(Array([Const(IntConst(1)); Const(IntConst(2)); Const(RealConst(3.))]), Const(IntConst(1))),"", "two")
                     "user[1].toto", DeRef( Subscript(Var("user"),Const(IntConst(1))),"", "toto")
                     "(user[1])[2]", Subscript( Subscript(Var("user"),Const(IntConst(1))),Const(IntConst(2)))
                     "(user[userid])[cluster]", Subscript( Subscript(Var("user"),Var("userid")),Var("cluster"))
                     "((user)[userid])[cluster]", Subscript( Subscript(Var("user"),Var("userid")),Var("cluster"))
                       ]
  let testModel2 = [ "CDiscrete(dimension=4,a=2)", MCall("CDiscrete",["dimension",Const(IntConst(4));"a", Const(IntConst(2))])                      
                     "CDiscrete()", MCall("CDiscrete",[])                   ]

  let testModel3 = [ "CDiscrete(dimension=4)"              , MCall("CDiscrete",["dimension",Const(IntConst(4))])
                     "CBernoulli()[cluster<4]"             , MIndexed(MCall("CBernoulli",[]),Var("cluster"), Const(IntConst(4)))
                     "CDiscrete(dimension=100)[cluster<4]" , MIndexed(MCall("CDiscrete",["dimension",Const(IntConst(100))]),Var("cluster"), Const(IntConst(4)))
                     "CDiscrete(dimension=5)[users[userid]<4]" , MIndexed(MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Subscript(Var("users"), Var("userid")),
                                                                                        Const(IntConst(4))
                                                                                    )
                     "CDiscrete(dimension=5)[(users[userid])<4]" , MIndexed(MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Subscript(Var("users"), Var("userid")),
                                                                                        Const(IntConst(4))
                                                                                    )
                     "CDiscrete(dimension=5)[(users[userid])[cluster]<4]" , MIndexed(MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Subscript(Subscript(Var("users"), Var("userid")),Var("cluster")),
                                                                                        Const(IntConst(4))
                                                                                    ) 
                     "(CDiscrete(dimension=5)[cluster<4])[cluster<4]" , MIndexed(
                                                                                    MIndexed(
                                                                                        MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Var("cluster"),
                                                                                        Const(IntConst(4))
                                                                                    ),
                                                                                    Var("cluster"),
                                                                                    Const(IntConst(4))
                                                                                  )
                                                                                  
                     "(CDiscrete(dimension=5)[(users[userid])[cluster]<4])[(users[userid])[cluster]<4]" , MIndexed(
                                                                                    MIndexed(
                                                                                        MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Subscript(Subscript(Var("users"), Var("userid")),Var("cluster")),
                                                                                        Const(IntConst(4))
                                                                                    ),
                                                                                    Subscript(Subscript(Var("users"), Var("userid")),Var("cluster")),
                                                                                    Const(IntConst(4))
                                                                                  )

                     "(users[userid])[cluster]",  MExp(Subscript(Subscript(Var("users"), Var("userid")),Var("cluster")))
                     "(users[userid]).cluster",  MExp(DeRef(Subscript(Var("users"), Var("userid")),"","cluster"))
                     "users[userid].cluster",  MExp(DeRef(Subscript(Var("users"), Var("userid")),"",("cluster")))

                     "(CDiscrete(dimension=5)[(users[userid].cluster)<4])[(movies[movieid].cluster)<4])" , MIndexed(
                                                                                    MIndexed(
                                                                                        MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        DeRef(Subscript(Var "users", Var "userid"),"",("cluster")),
                                                                                        Const(IntConst(4))
                                                                                    ),
                                                                                    DeRef(Subscript(Var("movies"), Var("movieid")),"", ("cluster")),
                                                                                    Const(IntConst(4))
                                                                                  )

                                                                                  ]
  let fail = [ 
                                                                                  
                     "(CDiscrete(dimension=5)[users[userid].cluster<4])[(users[userid])[cluster]<4]" , MIndexed(
                                                                                    MIndexed(
                                                                                        MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Subscript(Subscript(Var("users"), Var("userid")),Var("cluster")),
                                                                                        Const(IntConst(4))
                                                                                    ),
                                                                                    Subscript(Subscript(Var("users"), Var("userid")),Var("cluster")),
                                                                                    Const(IntConst(4))
                                                                                  )

                     "(CDiscrete(dimension=5)[user.cluster<4])[cluster<4]" , MIndexed(
                                                                                    MIndexed(
                                                                                        MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Subscript(Var("user"), Var("cluster")),
                                                                                        Const(IntConst(4))
                                                                                    ),
                                                                                    Var("cluster"),
                                                                                    Const(IntConst(4))
                                                                                  )
                     "(CDiscrete(dimension=5)[cluster.toto<4])[cluster<4]" , MIndexed(
                                                                                    MIndexed(
                                                                                        MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Var("cluster"),
                                                                                        Const(IntConst(4))
                                                                                    ),
                                                                                    Var("cluster"),
                                                                                    Const(IntConst(4))
                                                                                  )
                     "(CDiscrete(dimension=5)[(users[userid])[cluster]<4])[(movies[movieid])[cluster]<4]" , MIndexed(
                                                                                    MIndexed(
                                                                                        MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                        Subscript(Subscript(Var("users"), Var("userid")),Var("cluster")),
                                                                                        Const(IntConst(4))
                                                                                    ),
                                                                                    Subscript(Subscript(Var("movies"), Var("movieid")),Var("cluster")),
                                                                                    Const(IntConst(4))
                                                                                  )
                     ]


  let userMovieInput =  
    [ Declaration(Table("Users",None), ["cluster" , { Type = T_Int; Markup = Latent(parseFromString Parser.Model  "CDiscrete(dimension=4)") } 
                                        "isMale"  , { Type = T_Bool; Markup = Latent(parseFromString Parser.Model "CBernoulli()[cluster<4]" ) } 
                                        "age"     , { Type = T_Bool; Markup = Latent(parseFromString Parser.Model "CDiscrete(dimension=100)[cluster<4]" ) } 
                                          ])
      Declaration(Table("Movies",None),["cluster" , { Type = T_Int; Markup = Latent(parseFromString Parser.Model  "CDiscrete(dimension=4)") } 
                                        "year"    , { Type = T_Bool; Markup = Latent(parseFromString Parser.Model   "CDiscrete(dimension=100)[cluster<4]" ) } 
                                        "category", { Type = T_Bool; Markup = Latent(parseFromString Parser.Model "CDiscrete(dimension=50)[cluster<4]" ) }
                                            ])

      Declaration(Table("Ratings",None),["rating" , { Type = T_Int; Markup = Latent(parseFromString Parser.Model  "(CDiscrete(dimension=5)[(users[userid].cluster)<4])[(movies[movieid].cluster)<4])") } 
                    ])
      ]

  let userMoviePased = 
    [ Declaration(Table("Users", None),["cluster" , { Type = T_Int; Markup = Latent(MCall("CDiscrete",["dimension",Const(IntConst(4))])) } 
                                        "isMale"  , { Type = T_Bool; Markup = Latent(MIndexed(MCall("CBernoulli",[]),Var("cluster"), Const(IntConst(4)))) } 
                                        "age"     , { Type = T_Bool; Markup = Latent(MIndexed(MCall("CDiscrete",["dimension",Const(IntConst(100))]),Var("cluster"), Const(IntConst(4)))) } 
                    ])
      Declaration(Table("Movies",None),["cluster" , { Type = T_Int; Markup = Latent(MCall("CDiscrete",["dimension",Const(IntConst(4))])) } 
                                        "year"    , { Type = T_Bool; Markup = Latent(MIndexed(MCall("CDiscrete",["dimension",Const(IntConst(100))]),Var("cluster"), Const(IntConst(4)))) } 
                                        "category", { Type = T_Bool; Markup = Latent(MIndexed(MCall("CDiscrete",["dimension",Const(IntConst(100))]),Var("cluster"), Const(IntConst(4)))) }
                    ])

      Declaration(Table("Ratings",None),["rating" , { Type = T_Int; Markup = Latent(MIndexed(
                                                                                          MIndexed(
                                                                                              MCall("CDiscrete",["dimension",Const(IntConst(5))]),
                                                                                              Subscript(Subscript(Var("users"), Var("userid")),Var("cluster")),
                                                                                              Const(IntConst(4))
                                                                                          ),
                                                                                          Subscript(Subscript(Var("movies"), Var("movieid")),Var("cluster")),
                                                                                          Const(IntConst(4))))} 
                    ])
      ]

  let runTest() =
      let a = (testModel  , Parser.Model)     |> (fun (tests,parser) -> tests |> List.choose(fun (s, res) -> let r = parseFromString parser s in if r =  res then None else Some ("failed :"+s+": "+recUnionFieldToString r))) |> List.map (tee (printfn "failed %A")) |> (fun l -> if l.Length = 0 then printfn "great success")
      let b = (testExp    , Parser.Exp)       |> (fun (tests,parser) -> tests |> List.choose(fun (s, res) -> let r = parseFromString parser s in if r =  res then None else Some ("failed :"+s+": "+recUnionFieldToString r))) |> List.map (tee (printfn "failed %A")) |> (fun l -> if l.Length = 0 then printfn "great success")
      let c = (testExpList, Parser.ExpList)   |> (fun (tests,parser) -> tests |> List.choose(fun (s, res) -> let r = parseFromString parser s in if r =  res then None else Some ("failed :"+s+": "+recUnionFieldToString r))) |> List.map (tee (printfn "failed %A")) |> (fun l -> if l.Length = 0 then printfn "great success")
      let d = (testBindings, Parser.Bindings) |> (fun (tests,parser) -> tests |> List.choose(fun (s, res) -> let r = parseFromString parser s in if r =  res then None else Some ("failed :"+s+": "+recUnionFieldToString r))) |> List.map (tee (printfn "failed %A")) |> (fun l -> if l.Length = 0 then printfn "great success")
      let e = (testExp2    , Parser.Exp)      |> (fun (tests,parser) -> tests |> List.choose(fun (s, res) -> let r = parseFromString parser s in if r =  res then None else Some ("failed :"+s+": "+recUnionFieldToString r))) |> List.map (tee (printfn "failed %A")) |> (fun l -> if l.Length = 0 then printfn "great success")
      let f = (testLookup  , Parser.Exp)      |> (fun (tests,parser) -> tests |> List.choose(fun (s, res) -> let r = parseFromString parser s in if r =  res then None else Some ("failed :"+s+": "+recUnionFieldToString r))) |> List.map (tee (printfn "failed %A")) |> (fun l -> if l.Length = 0 then printfn "great success")
      let g = (testModel2  , Parser.Model)    |> (fun (tests,parser) -> tests |> List.choose(fun (s, res) -> let r = parseFromString parser s in if r =  res then None else Some ("failed :"+s+": "+recUnionFieldToString r))) |> List.map (tee (printfn "failed %A")) |> (fun l -> if l.Length = 0 then printfn "great success")
      let g = (testModel3  , Parser.Model)    |> (fun (tests,parser) -> tests |> List.choose(fun (s, res) -> let r = parseFromString parser s in if r =  res then None else Some ("failed :"+s+": "+recUnionFieldToString r))) |> List.map (tee (printfn "failed %A")) |> (fun l -> if l.Length = 0 then printfn "great success")
      ()
      
  do
      let modelTests = runTest()
      Console.ReadKey() |> ignore
