namespace MicrosoftResearch.Infer.Tabular


module Anglican =
 
 module FArray = Microsoft.FSharp.Collections.Array

 module S = Syntax
 open S
 let size(tn) = tn+"-"+"size"
 let col(tn:string,cn) = tn+"-"+cn
 open SExp
 let uptoAsMod = ref true
 let vectorAsArray = ref (None:int option)

 let trSchem (data:DTO) coreschema =

 
  let detToStr d = 
      match d with 
      | Syntax.D -> "det"
      | Syntax.R -> "rnd"
      | Syntax.Qry -> "qry" 
      |> Atom

  let rec 
    modelToStr i  rho (m:S.Model) : Sexp =
    let exprToStr = exprToStr i rho
    match m with 
     | S.MExp e -> exprToStr e
     | S.MIndexed(m,e1,e2) -> 
        list "Indexed" [modelToStr i rho m;exprToStr e1; exprToStr e2]
     | S.MCall(f,args) -> 
        list "Indexed" (Atom f::(fldsToStr i rho args))
     | S.TypedModel(m,mt) -> modelToStr i rho m
  

 
  and exprToStr i (rho:Map<string, Sexp -> Sexp>) (e:S.Exp) : Sexp =
   let exprToStrAux = exprToStr i
   let exprToStr = exprToStr i rho
   match e with
   | S.Var v -> Map.find v rho i
   | S.Const (S.IntConst v) -> sprintf "%A" v |> Atom
   | S.Const (S.BoolConst v) -> sprintf "%A" v |> Atom
   | S.Const (S.RealConst v) -> sprintf "%A" v |> Atom
   | S.Const (S.StringConst v) -> sprintf "%A" v |> Atom
   | S.Prim (S.Factor(S.FactorName "BreakSymmetry"),[e]) -> exprToStr e
   | S.Prim (p,es) -> list (sprintf "%A" p) (List.map exprToStr es)
   //| S.Dist(S.GaussianFromMeanVariance,[e1,e2]) -> list (sprintf "%A" d) (List.map exprToStr es)
   | S.Dist(S.GaussianFromMeanAndPrecision,[e1;e2]) -> list "normal" [exprToStr e1;list "sqrt" [list "/" [Atom "1.0";exprToStr e2]]]
   | S.Dist(S.GammaFromShapeAndScale,[e1;e2]) -> list "gamma" [exprToStr e1;list "/" [Atom "1.0";exprToStr e2]]
   | S.Dist(S.Discrete,[e1;e2]) -> list "discrete" [exprToStr e2]
   | S.Dist(S.DirichletSymmetric,[e1;e2]) -> list "dirichlet" [list "map " [(list "lambda" [List [Atom "-"] ;exprToStr  e2]); 
                                                                                           (list "range" [exprToStr e1])]]
   | S.Dist(d,es) -> list (sprintf "%A" d) (List.map exprToStr es)
  // | S.Dist(d,es) -> list (sprintf "%A" d) (List.map exprToStr es)
   | S.SizeOf(tn) -> Atom (size(tn))
   | S.DeRef(e1,tn,cn) ->
          list "nth"  [Atom (col(tn,cn)); exprToStr e1]
   | S.Ref(tn,cn) ->
          Atom (col(tn,cn))
   | S.If(e1,e2,e3) -> list "if" [exprToStr e1; exprToStr e2; exprToStr e3]
   | S.ForLoop(x,e1,e2) -> list "map " [(list "lambda" [List [Atom x] ;(exprToStrAux (rho.Add(x,fun _ -> Atom x))  e2)]); 
                                        (list "range" [exprToStr e1])]
   | S.Array(es) -> list "vector" (List.map exprToStr es)
   | S.Subscript(e1,e2) -> list "nth"  [exprToStr e1; exprToStr e2]
   | S.Constraint(e1,t1) -> exprToStr e1
   | S.Let(x,e1,e2) -> list "For" [Atom x; exprToStr e1; exprToStr e2]
   | S.Scan(s,x,e1,e2,e3) -> list "Scan" [Atom s; Atom x; exprToStr e1; exprToStr e2;exprToStr e3]
   | S.Infer(d,es,x,e) -> list "Infer" [(sprintf "%A" d|> Atom); List (List.map exprToStr es); Atom x; exprToStr e]
   | S.TypedExp(e,ty) -> exprToStr e
   | _ -> Atom (sprintf "?%A" e)

  and fldsToStr i rho es = 
      match es with 
      | [] -> []
      | (f,e)::es -> (list f [exprToStr i rho e])::(fldsToStr i rho es)
  and expsToStr i rho es = 
      match es with 
      | [] -> []
      | e::es -> (exprToStr i rho e)::(expsToStr i rho es)
  and recordTyToStr i rho ts = 
      match ts with 
      | [] -> []
      | (f,t)::ts -> list f [columnTypeToStr i rho t] :: (recordTyToStr i rho ts)
  and columnTypeToStr i rho ty =
     let Base s = List [s;detToStr (S.det ty)] 
     match ty with
      | S.T_Real -> Base (Atom "Real")
      | S.T_Int -> Base (Atom "Int")
      | S.T_Bool -> Base (Atom "Bool")
      | S.T_String -> Base (Atom "String")
      | S.T_Link t -> Base (list "link" [Atom t])
      | S.T_Array (ty,e) -> list "Array" [columnTypeToStr i rho ty;exprToStr i rho e]
      | S.T_Upto e -> Base (list "Upto" [exprToStr i rho e])
      | S.T_Record flds ->  list "Record" (recordTyToStr i rho flds)
      | S.T_Vector  -> Base (Atom "Vector")
      | S.T_PositiveDefiniteMatrix -> Base (Atom "PositiveDefiniteMatrix")

  let noindex = List []
 
  let markupToStr rho tn cn ty (A:S.Markup)  =
    let dto = data
    let (colmap,data) = dto.[tn]
    let data = Seq.toArray(data)
    let length = data.Length
    let choosei fchoose ar =  ar |> FArray.mapi(fun i e -> (i,e))|> FArray.choose fchoose 
    
    let mkArray c cty  =
        let i = colmap.[c]
        let atom (o:obj) = Atom (o.ToString())
        let observedIndices = data |> choosei (fun (k,r) -> if (r.[i] = null) then None else Some k)
        observedIndices,
        match cty with 
        // note: unsafe conversions
        | T_Link tn ->  ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i]) |> atom|] )
        | T_Real ->     ([| for irow in observedIndices -> System.Convert.ToDouble (data.[irow].[i]) |> atom|] )
        | T_Int ->      ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i]) |> atom|] )
        | T_Bool ->     ([| for irow in observedIndices -> System.Convert.ToBoolean(data.[irow].[i]) |> atom|] )
        | T_Upto _ ->   ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i]) |> atom|] )
        | T_String _ -> ([| for irow in observedIndices -> System.Convert.ToString (data.[irow].[i]) |> atom|] )
        | T_Array(ety,e) -> failwith "NYI"
        | T_Record _  -> failwith "NYI"
        | T_Vector -> failwith "NYI"
    let index = col(tn,"index")
    let name = col(tn,cn)
    match A with
     | S.Hyper(e) -> 
     //[assume name e*]
       Map.add name (fun i -> Atom name) rho,
       [list "assume" [Atom name; exprToStr noindex rho e]]
     | S.Param(M) -> 
       // [assume name M*]
       Map.add cn (fun i -> Atom name) rho,
       [list "assume" [Atom name; modelToStr noindex rho M];
        list "predict" [Atom name]]

     | S.Input -> 
       let (is,data)= mkArray cn ty
       Map.add cn (fun i -> list "nth" [Atom name; i]) rho,
       [list "assume" [Atom name; list "vector" [for d in data -> d]]] //TBC
     | S.Latent(M) ->
       let assume = list "assume" [Atom name;  
                                   list "mem" [list "lambda" [List [Atom index] ; modelToStr (Atom index) rho M]]] //TBC
         //[assume name (mem (lambda (r) M*)]
       let predict = list "predict" [
                           list "map " [(list "lambda" [List [Atom index]; List [Atom name; Atom index] ]); 
                                        (list "range" [Atom (length.ToString())])]]
       Map.add cn (fun i -> List [Atom (col(tn,cn)); i]) rho,
       [assume;predict]    
     | S.Observable(M) -> 
       let (is,data)= mkArray cn ty
      // let assume = list "assume" [Atom name;  
       //                            list "mem" [list "lambda" [List [Atom index] ; modelToStr (Atom index) rho M]]] //TBC
         //[assume name (mem (lambda (r) M*)]
       let observes = Array.mapi (fun i v -> list "observe" [modelToStr v rho M; v])  data
       (Map.add cn (fun i -> List [Atom (col(tn,cn)); i]) rho),
       [for p in observes -> p]  
       
        //[assume name (mem (lambda (r) M*)]
        //for i in 0..data[t].size-1 
        //   match data[t][i] with 
        //   | None -> [predict (name {i}))]
        //   | Some v -> [observe (name {i}) v]

  let tableToStr rho tn (T:S.Table) = 
       let (_,data) = data.[tn]
       let data = Seq.toArray(data)
       let length = Atom (data.Length.ToString())
       let rho = Map.add (size(tn)) (fun (s:Sexp) -> Atom (length.ToString())) rho
       let dec = list "assume" [Atom (size(tn)); exprToStr noindex rho (S.SizeOf(tn))]

       let rho,decs = 
                      List.fold (fun (rho,decs) (cn,col:S.Column) -> 
                                    let rho', dec = markupToStr rho  tn cn col.Type col.Markup
                                    (rho',(List.rev dec)@decs)) 
                                 (Map.empty,[]) T
       List.rev decs 
  let declToStr ((S.Declaration (decl, T)):S.Declaration): Sexp list =
    match decl with
    | S.Table(nme,_) -> tableToStr (Map.empty) nme T
    | S.Fun(nme) ->  tableToStr (Map.empty) nme T

  let schemaToStr (S:S.Schema) = list "program" ((list "import" [Atom "'core"]) ::(List.concat (List.map declToStr S)))

  let schemaToString S = pretty "  " "" (schemaToStr S)

  schemaToString coreschema