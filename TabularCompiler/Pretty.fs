namespace MicrosoftResearch.Infer.Tabular

module Pretty =
  module T = Syntax
  open T
  open Target
       
  let rec EtoString e : string = 
      let EsToString (es:E list) = (System.String.Join (",",[| for e in es -> EtoString e |]))
      match e with
        | Var v -> v
        | Rng r -> r + "/*range*/"
        | Const (IntConst i) -> sprintf "Variable.Constant<int>(%O)" i
        | Const (RealConst r) ->  sprintf "Variable.Constant<double>(%O)" (r.ToString())
        | Const (BoolConst b) -> sprintf "Variable.Constant<bool>(%O)" (b.ToString())
        | Const (StringConst s) -> sprintf "Variable.Constant<string>(%O)" (sprintf "\"%A\"" (s.ToString()))
        | IndexRng (e1,e2) -> sprintf  "%O[ %O]" (EtoString e1) e2 
        | Index (e1,e2) -> sprintf "%O[ %O]" (EtoString e1) (EtoString e2) 
        | Prim(Prim.Gt,[e1;e2]) ->  sprintf "%O > %O" (EtoString e1) (EtoString e2) 
        | Prim(Prim.GtEq,[e1;e2]) ->  sprintf  "%O >= %O" (EtoString e1) (EtoString e2)   
        | Prim(Prim.Lt,[e1;e2]) ->  sprintf "%O < %O"   (EtoString e1) (EtoString e2) 
        | Prim(Prim.LtEq,[e1;e2]) ->  sprintf "%O <= %O" (EtoString e1) (EtoString e2)   
        | Prim(Prim.Eq,[e1;e2]) -> 
              sprintf  "(%O = %O)" (EtoString e1) (EtoString e2) 
        | Prim(Prim.Minus,[e1;e2])  -> 
              sprintf  "(%O - %O)" (EtoString e1) (EtoString e2) 
        | Prim(Prim.And, [e1;e2])  -> sprintf "(%O & %O)" (EtoString e1) (EtoString e2) 
        | Prim(Prim.Or, [e1;e2])  -> sprintf "(%O | %O)" (EtoString e1) (EtoString e2) 
        | Prim(Prim.Mult, [e1;e2])  -> sprintf   "(%O * %O)" (EtoString e1) (EtoString e2)
        | Prim(Prim.Factor (FactorName s), es) ->
              sprintf "Factor.%O(%O)" s (EsToString es)
        | Prim(p, es) ->
              sprintf "Prim.%A(%O)" p (EsToString es)
        | Dist(d,es) -> sprintf "Variable.%A(%O)" d (EsToString es)
        | _ -> sprintf "??%A??" e
  
  let rec tyToString  ty = 
        match ty with
        | T_Int -> "int"
        | T_Real -> "double"
        | T_Bool -> "bool"
        | T_String -> "string"
        | T_Array (ty,e) -> tyToString ty + sprintf "[/*%A*/]" (Pretty.exprToStr e)
        | T_Upto e -> sprintf "int /*upto(%A)*/" (Pretty.exprToStr e)
        | T_Link t ->  sprintf "int /*upto(SizeOf(%s))*/" t
        | T_Vector -> "Maths.Vector"
        | T_PositiveDefiniteMatrix -> "PositiveDefiniteMatrix"
        | T_Record flds -> sprintf "{%O}" (System.String.Join(",",[|for (n,ty) in flds -> sprintf "%O = %O" n (tyToString ty)|]))
        | t -> sprintf "??%A??" t

  let rec objToString ty (obj:obj) = 
        match ty with
        | T_Int 
        | T_Real 
        | T_Bool 
        | T_String
        | T_Upto _
        | T_Link _ 
        | T_Vector 
        | T_PositiveDefiniteMatrix -> obj.ToString() 
        | T_Array (ty,e) -> sprintf "new %O []{%O}" (tyToString ty) (System.String.Join(",", [| for o in (obj :?> System.Array) -> objToString ty  o |]))
        | t -> sprintf "??%A??" t

  let rec StoString tab s = 
       //let sprintf fmt k = "\n"+tab+(sprintf fmt k)
       match s with
       | CloneRng (s,r) -> tab+sprintf "var %O = %O.Clone();" s r 
       | LetRng (r,i) -> tab+sprintf "var %O = new Range(%O);" r i 
       | LetVar (v,e) -> tab+sprintf "var %O = %O;" v (EtoString e)
       | LetNew (v,t) -> tab+sprintf "var %O = Variable.New<%O>();" v (tyToString t)
       | LetArray (v,r,t) -> 
           tab+(sprintf "var %O = Variable.Array<%O>(%O);" v (tyToString t)  r)
       | ObserveValue(v,t,obj) ->
           tab+(sprintf "%O.ObservedValue=%O;" v (objToString t obj))
       | Assign (v,r,E) -> 
           tab+(sprintf "%O[%O] = %O;" v r (EtoString E))
       | AssignIndex (v,Ei,E) -> 
           tab+(sprintf "%O[%O] = %O;" v (EtoString Ei) (EtoString E))
       |  SetTo(v,E) ->
           tab+(sprintf "%O.SetTo(%O);" v (EtoString E)) 
       |  Seq (S1,S2) -> 
          (sprintf "%O%O" (StoString tab S1) (StoString tab S2))
       |  ForEach(r,S) ->
           tab + (sprintf "using(Variable.ForEach(%O)) {%O" r (StoString (tab+" ") S)) + tab + "}"
       |  ForLoop(r,x,S) ->
           tab + (sprintf "using(var %OBlock = Variable.ForEach(%O)) { var %O= %OBlock.Index; %O" r r x r   (StoString (tab+" ") S)) + tab + "}"
       |  IfNot(v,S) ->
            tab + (sprintf "using(Variable.IfNot(%O)) {%O"  v (StoString (tab+" ") S)) + tab + "}"
       |  If(v,S) ->
            tab + (sprintf "using(Variable.If(%O)) {%O" v (StoString (tab+" ") S)) + tab + "}"
       |  Skip -> ""
       |  Switch(v,S) ->
            tab + (sprintf "using(Variable.Switch(%O)) {%O" v (StoString (tab+" ") S)) + tab + "}"
       | SetValueRange(v,r) ->
            tab + (sprintf "%O.SetValueRange(%O);" v r)
       | LetCopy(v,E) -> 
            tab + (sprintf "var %O = Variable.Copy<?>(%O);" v (EtoString E))

  let rangeindex r = r+"_i"

  let rec EToCSoft e : string = 
      let EsToString (es:E list) = (System.String.Join (",",[| for e in es -> EToCSoft e |]))
      match e with
        | Var v -> v
        | Rng r -> r + "/*range*/"
        | Const (IntConst i) -> i.ToString()
        | Const (RealConst r) -> r.ToString()
        | Const (BoolConst b) -> b.ToString()
        | Const (StringConst b) -> sprintf "\"%A\"" (b.ToString())
        //| DiscreteConst (i,n) ->  i.ToString() // TODO set value range                                
       // | RealConst r ->  r.ToString()
       // | BoolConst b ->  b.ToString()
        | IndexRng (e1,r) -> sprintf  "%O[ %O]" (EToCSoft e1) (rangeindex r) 
        | Index (e1,e2) -> sprintf "%O[ %O]" (EToCSoft e1) (EToCSoft e2) 
        | Prim(Prim.Gt,[e1;e2]) ->  sprintf "%O > %O" (EToCSoft e1) (EToCSoft e2) 
        | Prim(Prim.GtEq,[e1;e2]) ->  sprintf  "%O >= %O" (EToCSoft e1) (EToCSoft e2)   
        | Prim(Prim.Lt,[e1;e2]) ->  sprintf "%O < %O"   (EToCSoft e1) (EToCSoft e2) 
        | Prim(Prim.LtEq,[e1;e2]) ->  sprintf "%O <= %O" (EToCSoft e1) (EToCSoft e2)   
        | Prim(Prim.Eq,[e1;e2]) -> 
              sprintf  "(%O = %O)" (EToCSoft e1) (EToCSoft e2) 
        | Prim(Prim.Minus,[e1;e2])  -> 
              sprintf  "(%O - %O)" (EToCSoft e1) (EToCSoft e2) 
        | Prim(Prim.And, [e1;e2])  -> sprintf "(%O & %O)" (EToCSoft e1) (EToCSoft e2) 
        | Prim(Prim.Or, [e1;e2])  -> sprintf "(%O | %O)" (EToCSoft e1) (EToCSoft e2) 
        | Prim(Prim.Mult, [e1;e2])  -> sprintf   "(%O * %O)" (EToCSoft e1) (EToCSoft e2)
        | Prim(Prim.Factor (FactorName s), es) ->
              sprintf "Factor.%O(%O)" s (EsToString es)
        | Prim(p, es) ->
              sprintf "Prim.%A(%O)" p (EsToString es)
        | Dist(d,es) -> sprintf "Variable.%A(%O)" d (EsToString es)
        | _ -> sprintf "??%A??" e
  
  let rec tyToCSoft  ty = 
        match ty with
        | T_Int -> "int"
        | T_Real -> "double"
        | T_Bool -> "bool"
        | T_String -> "string"
        | T_Array (ty,e) -> tyToCSoft ty + sprintf "[(*%A*)]" e
        | T_Upto e -> sprintf "int"
        | T_Link _ -> "int"
        | T_Vector -> "Maths.Vector"
        | T_PositiveDefiniteMatrix -> "PositiveDefiniteMatrix"
        | T_Record flds -> sprintf "{%O}" (System.String.Join(",",[|for (n,ty) in flds -> sprintf "%O = %O" n (tyToCSoft ty)|]))
        | t -> sprintf "??%A??" t

  let rec StoCSoft tab s : string = 
       //let sprintf fmt k = "\n"+tab+(sprintf fmt k)
       match s with
       | CloneRng (s,r) -> tab+sprintf "var %O = %O.Clone();" s r 
       | LetRng (r,i) -> tab+sprintf "var %O = %O;" r i 
       | LetVar (v,e) -> tab+sprintf "var %O = %O;" v (EToCSoft e)
       | LetNew (v,t) -> tab+sprintf "%O %O;" (tyToCSoft t) v
       | LetArray (v,r,t) -> 
           tab+(sprintf "var %O = new %O[%O];" v (tyToCSoft t) r  )
       | ObserveValue(v,t,obj) ->
           tab+(sprintf "%O.ObservedValue=%O;" v (objToString t obj))
       | Assign (v,r,E) -> 
           tab+(sprintf "%O[%O] = %O;" v (rangeindex r) (EToCSoft E))
       | AssignIndex (v,Ei,E) -> 
           tab+(sprintf "%O[%O] = %O;" v (EToCSoft Ei) (EToCSoft E))
       |  SetTo(v,E) ->
           tab+(sprintf "%O = %O;" v (EToCSoft E)) 
       |  Seq (S1,S2) -> 
           (sprintf "%O%O" (StoCSoft tab S1) (StoCSoft tab S2))
       | LetCopy(v,E) -> 
            tab + (sprintf "var %O = Copy(%O);" v (EToCSoft E))
       | SetValueRange(v,r) ->
            tab + (sprintf "/* %O.SetValueRange(%O); */" v r)
       |  ForEach(r,S) ->
           let ri = rangeindex r
           tab + (sprintf "for(int %O = 0; %O < %O; %O++) {%O" ri ri r ri (StoCSoft (tab+" ") S)) + tab + "}"
       |  ForLoop(r,x,S) ->
           let ri = x
           tab + (sprintf "for(int %O = 0; %O < %O; %O++) {%O" ri ri r ri (StoCSoft (tab+" ") S)) + tab + "}"
       |  IfNot(v,S) ->
            tab + (sprintf "if (!%O) {%O"  v (StoCSoft (tab+" ") S)) + tab + "}"
       |  If(v,S) ->
            tab + (sprintf "if (%O) {%O" v (StoCSoft (tab+" ") S)) + tab + "}"
       |  Skip -> ""
       |  Switch(v,S) ->
            tab + (sprintf "using(Variable.Switch(%O)) {%O" v (StoCSoft (tab+" ") S)) + tab + "}"
