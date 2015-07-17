namespace MicrosoftResearch.Infer.Tabular

open System.Diagnostics

open MicrosoftResearch.Infer
open MicrosoftResearch.Infer.Models
open MicrosoftResearch.Infer.Distributions
open MicrosoftResearch.Infer.Collections
open MicrosoftResearch.Infer.Transforms
//open MicrosoftResearch.Infer.Tabular.Service
open Syntax
module FArray = Microsoft.FSharp.Collections.Array

open System.Text.RegularExpressions


       
 


module R2Compiler =

 
  // a dummy algorithm for selection R2
  type R2Algorithm()=
     inherit GibbsSampling() 
     override this.Name = "R2"
            
       
 // let runR2 args = let p = Process.Start(@"...",args)
 //                  do p.WaitForExit()
 
  let runR2 (numSamples:int) model =
    //let workingFolder = @"D:\tfs\mlp\pp\R2\src\R2\out"
    //let myFile =        @"D:\tfs\mlp\pp\R2\src\R2\out\R2.exe";
      
    let myFile =        @"R2.exe";
    let workingFolder = System.IO.Path.GetTempPath()+"\R2"
    if not (System.IO.Directory.Exists(workingFolder)) then System.IO.Directory.CreateDirectory(workingFolder) |> ignore
    let pInfo = new System.Diagnostics.ProcessStartInfo();
    pInfo.FileName <- myFile;
    pInfo.WorkingDirectory <- workingFolder;
    pInfo.WindowStyle <- System.Diagnostics.ProcessWindowStyle.Hidden
    pInfo.CreateNoWindow <- false//true
    pInfo.Arguments <- //sprintf "-Tgif %s -O" (inputFolder + @"\" + filename);
                       sprintf "/numSamples:%A %s" numSamples model;
    pInfo.LoadUserProfile <- true;
    pInfo.UseShellExecute <- false;
    
    pInfo.RedirectStandardOutput <- true;
    let proc = System.Diagnostics.Process.Start(pInfo)
    let res = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    res

  //open MicrosoftResearch.Infer.Tabular.DataLayer
  open MicrosoftResearch.Infer.Tabular
  open Syntax
 
  module Tabular = Syntax
  open MicrosoftResearch.Infer
  open MicrosoftResearch.Infer.Factors
  open System.Threading


  let verbose = ref true
  let timing = ref true
  let time header f x =
    if (!timing || !verbose) then printfn "(Fun %s" header
    let s = new System.Diagnostics.Stopwatch()
    s.Start()
    try 
      f x 
    finally 
      s.Stop()
      if (!timing || !verbose) then printfn "Fun %s time was %o ms)" header s.ElapsedMilliseconds


  
  type r = string
  type v = string
  type C = C 
 

  type T = Tabular.ColumnType

  type E =
              | Var of v
              | Rng of r
              | IndexRng of E * r
              | Const of int
              | DiscreteConst of int * r    // bounded constant                                               
              | RealConst of real
              | BoolConst of bool
              | StringConst of string
              | RealArrayConst of real[]
              | Prim of (Tabular.Prim * E list) //TODO: restrict to v list
              | Dist of (Tabular.Dist * E list)
              | Arr of E [] // TBD
              | Index of E * E // array indexing
              // used for symmetry breaking
              | InitialiseTo of E * obj // obj must be a distribution 

  type S = 
       | CloneRng of r * r
       | LetRng of r * v
       | LetNew of v * T
       | LetVar of v * T * E
       | LetArray of v  * r * T  
       | LetConstArray of v * r * T * System.Array //unused?
       | ForEach of r * S
       | IfNot of v * S
       | If of v * S 
       | SetTo of v * E
       | AssignIndex of v * E * E
       | Assign of v * r * E

       | Seq of S * S
       | ObserveValue of v * C
       | Skip
       | Switch of v * S
       | SetValueRange of v * r
       | LetCopy of v * E
       | Observe of v * r * E
 
  let DOT = ""
  let size(tn) = tn+DOT+"size"
  let col(tn:string,cn) = tn+DOT+cn
  let colfield(tn:string,cn,fld) = tn+DOT+cn+DOT+fld
  let range(tn:string) = tn+DOT+"range"
  let mutable i = 0
  let fresh() = 
        i <- i +  1 
        "v"+i.ToString()
          
 
  module T = Syntax
  

  let rangeindex r = r+DOT+"i"
  

  let rangesInv = new System.Collections.Generic.Dictionary<r,int>();
  let ranges = new System.Collections.Generic.Dictionary<int,r>()
  let rangeCtxt = ref (fun S -> S)


  let rangeOf i =
        if ranges.ContainsKey(i) then ranges.[i] 
        else 
             let s = fresh()
             let r = fresh()
             rangeCtxt := 
              (let ctxt = !rangeCtxt 
               fun S ->
                    ctxt ( Seq(LetVar(s,T_Int,Const(i)), // TODO: omit for R2?
                               Seq(LetRng(r, s),S))))

             rangesInv.Add(r,i);
             ranges.Add(i,r);
             r
  
  // decRangesXXX - collect all constant ranges via rangeOf
  let rec decRangesExp e = 
     match e with
     | T.Var v -> ()
     | T.Const c -> ()
     | T.Prim (p,es) -> List.iter decRangesExp es
     | T.Dist(d,es) ->  List.iter decRangesExp es
     | T.SizeOf(t) -> ()
     | T.DeRef(e1,tn,cn) ->  decRangesExp e1
     | T.Ref(tn,cn) -> ()
     | T.If(e1,e2,e3) -> decRangesExp e1; decRangesExp e2; decRangesExp e3
     | T.ForLoop(x,e1,e2) -> decRangesExp e1; decRangesExp e2
     | T.Array(es) -> List.iter decRangesExp es
     | T.Subscript(e1,e2) -> decRangesExp e1; decRangesExp e2
     | T.Constraint(e1,t1) -> decRangesExp e1; decRangesColumnType t1
     | T.Let(x,e1,e2) -> decRangesExp e1; decRangesExp e2
     | T.Scan(s,x,e1,e2,e3) -> decRangesExp e1; decRangesExp e2; decRangesExp e3
     | T.Infer(d,es,x,y) ->  List.iter decRangesExp es // shouldn't really occur
     | T.TypedExp(e,t) -> decRangesExp e;decRangesColumnType t
    // | _ -> failwithf "%A not supported" e

  and  decRangesModel m = 
       match m with 
       | T.MExp e -> decRangesExp e
       | T.TypedModel (m,((t1,t2),t3)) -> decRangesModel m; decRangesColumnType t1; decRangesColumnType t2; decRangesColumnType t3

 (*
       | T.MIndexed(m,e1,e2) -> T.Array(decRangesModel m,[decRangesExp e1]) // e2 will be inferred...
       | T.MCall("CGaussian",[]) -> T.CGaussian
       | T.MCall("CBernoulli",[]) -> T.CBernoulli
       | T.MCall("CDiscrete",[("N",e1)]) -> T.CDiscreteWith(decRangesExp e1)
       | T.MCall("CDiscrete",[]) -> T.CDiscreteWith(T.Const(2))
  *)
       | _ -> failwithf "decRanges: unexpected non-core model" 
  and decRangesColumnType t = 
       match t with
       | T.T_Link _ 
       | T.T_Real 
       | T.T_Bool 
       | T.T_String
       | T.T_Int 
       | T.T_PositiveDefiniteMatrix-> ()
       | T.T_Upto (T.TypedExp (T.Const (T.IntConst i),_)) -> ignore(rangeOf i)
       | T.T_Upto _ -> ()
       | T.T_Array (ct,(T.TypedExp (T.Const (T.IntConst i),_))) -> 
           ignore(rangeOf i); decRangesColumnType ct;
       | T.T_Array (ct,_) -> 
           decRangesColumnType ct
       | T.T_Record flds -> List.iter (fun (v,ty) -> decRangesColumnType ty) flds
       | T.T_Vector -> ()
  and decRangesMarkup m =
       match m with 
         T.Input -> ()
       | T.Latent m -> decRangesModel m
       | T.Observable m -> decRangesModel m
       | T.Hyper e -> decRangesExp e
       | T.Param m -> decRangesModel m
  let rec decRangesColumns cs = List.iter (fun (cn,col:T.Column) -> decRangesColumnType col.Type;
                                                                    decRangesMarkup col.Markup) cs
  let rec decRangesTable cs = decRangesColumns cs
  let rec decRangesTables decs = 
           match decs with 
           | [] -> ()
           | (T.Declaration(Table(tn,_),tbl))::decs ->
             decRangesTable tbl;
             decRangesTables decs;
           | dec::decs' -> decRangesTables decs' // skip non-core functions if present
  let rec decRangesSchema decs = decRangesTables decs

  let rec typeExp (TE: Map<TableName,Map<ColumnName,ColumnType>>) (CE:Map<ColumnName,B*ColumnType>) e =
      match e with 
      | TypedExp(e,t) -> t
      | _ -> failwith "BUG: encountered untyped subexpression %A "e
  
  let isAtomic et = match et with
      | TypedExp(e,t) ->
        match e with 
        | T.Const _ -> true
        | T.SizeOf t-> true
        | T.Var _ -> true
        | _ -> false
      | _ -> failwith "atomic: missing type annotation"

  let rec trExp tn (TE: Map<TableName,Map<ColumnName,ColumnType>>) (CE:Map<ColumnName,B*ColumnType>)  et k =
      let compileExp = trExp tn TE CE 
      let compileApp f es k = 
          let rec compileAppAux vs es  =
                    match es with 
                    | [] -> k (f (List.rev vs))
                    | ((TypedExp (_,t)) as e)::es when isAtomic(e) -> 
                           compileExp e 
                                  (fun E -> compileAppAux (E::vs) es)
                    | ((TypedExp (_,t)) as e)::es -> 
                           compileExp e 
                                  (fun E -> let v = fresh()
                                            Seq (LetVar(v,t,E),
                                                  compileAppAux ((Var v)::vs) es))
          compileAppAux [] es
      match et with 
      | TypedExp(e,t) ->                 
        match e with
        //TBC
        | T.Const (T.IntConst i) -> k (Const i)
        | T.Const (T.RealConst r) ->  k (RealConst r)
        | T.Const (T.BoolConst b) -> k (BoolConst b)
        | T.Const (T.StringConst s) -> k (StringConst s)
        | T.Prim(p,es) -> compileApp (fun vs -> Prim(p,vs)) es k
        | T.SizeOf t-> k (Var(size t ))
        | T.Constraint(e1,t1) ->
            compileExp e1 k
        | T.Subscript((TypedExp(_,ta) as ea),(TypedExp(_,ti) as ei)) ->
            compileApp (fun [v1;v2] -> Index(v1,v2)) [ea;ei] k
        | T.Array es ->
            match t with 
            | T_Array(et,_) ->
                let r = rangeOf es.Length
                let av = fresh()
                compileApp (fun vs -> vs) es (fun vs ->
                 let sets = List.mapi (fun i v -> AssignIndex(av,Const i,v)) vs
                 let s = List.foldBack (fun s ss -> Seq(s,ss)) sets (k (Var av))
                 Seq(LetArray(av,r,et),s))
        // this special-casing is a hack until we figure out how to declare local arrays...
        |  T.ForLoop(x,TypedExp((T.Const (IntConst n)) as e1 ,_),
                      (TypedExp((T.Const (T.RealConst d)),t2) as e2 )) -> 
                      k (RealArrayConst [| for i in 1..n -> d |])
        | T.ForLoop(x,TypedExp((T.Const (IntConst _) | T.SizeOf _) as e1 ,_),
                      (TypedExp(_,t2) as et2)) ->
            // What is the right code for this?
            let r = match e1 with  
                    | T.Const (IntConst i) -> rangeOf i
                    | T.SizeOf tn -> range(tn)
                   // | _ -> failwith "impossible"
            let av = fresh()
            let s = fresh()
            Seq(CloneRng(s,r), 
                Seq(LetArray(av,s,t2),
                    Seq(ForEach(s,
                                compileExp et2 (fun E -> Assign(av,s,E))),
                        k (Var av))))
        | T.Var c -> 
           match CE.[c] with
           | (B.H,_) 
           | (B.W,_) -> k (Var(col(tn,c)))
           | (B.Y,_) -> k (IndexRng  (Var(col(tn,c)),range tn )) // will this work for let?
        | T.DeRef (TypedExp(_,t1) as e1,tn',d) -> 
            match t1 with 
            | T_Upto (TypedExp (SizeOf tn,_)) 
            | T_Link tn -> //note we could rely on T_Link tn being translated to T_Upto (SizeOf tn?)
              assert(tn=tn')
              let tn'd = col(tn',d)
              compileApp (fun [v1] -> Index(Var(tn'd),v1)) [e1] k
            | _ -> failwithf "BUG: compiling %A" e
        | T.If(e1,e2,e3) ->
           compileExp e1 
                  (fun E1 ->
                       let ret = fresh()
                       Seq (LetNew(ret,t),
                            let v1 = fresh()
                            Seq(LetVar(v1,T_Bool,E1),  
                                Seq(If(v1, 
                                       (compileExp e2 (fun E2 -> Seq(SetTo(ret,E2),Skip)))),
                                    Seq(IfNot(v1, 
                                              (compileExp e3 (fun E3 -> Seq(SetTo(ret,E3),Skip)))),
                                        k (Var ret))))))
        
        | T.Dist(d,es) -> 
           //TODO: use dep. type of d to drive range/exp interpretation of es
           match d,es with
           | Dirichlet, ((TypedExp(e0,_))::es) ->
             let r = match e0 with 
                     | T.Const(IntConst n) -> rangeOf n 
                     | T.SizeOf(tn) -> range(tn)
                     | _ -> failwithf "BUG: %A has non-constant argument %A" d e0 
             compileApp (fun vs -> Dist(d,(Rng r) ::vs)) es k 
           | Discrete, ((TypedExp(e0,_))::es) ->
             match e0 with 
             | T.Const(IntConst n) ->
               // add symmetry breaking
               let r = rangeOf n
               let v = Distributions.Dirichlet([| for i in 1 .. n -> 10.0 |]).Sample()
               let o = Distributions.Discrete(v)
               compileApp (fun vs -> InitialiseTo(Dist(d,(Rng r) ::vs),o)) es k
             | T.SizeOf(tn) -> 
               // don't know how to symmetry break here without knowing size of tn
               let r = range(tn)
               compileApp (fun vs -> Dist(d,(Rng r) ::vs)) es k   
             | _ -> failwithf "BUG: %A has non-constant argument %A" d e0 
           | _,_ ->
           compileApp (fun vs -> Dist(d,vs)) es k 
      | ue -> failwithf "BUG: encountered untyped expression %A " ue

 
  type Es = E list
  type KE = (E -> S) -> S
  type KEs = (Es -> S) -> S
  type M = {H:unit -> KEs;P: Es -> KEs ;G: (Es*E) -> KE}
  type MT = {TH: ColumnType list;TW: ColumnType list; TX: ColumnType; TY: ColumnType}
  let One = Const(0)

      

  
  let rec typeModel (TE: Map<TableName,Map<ColumnName,ColumnType>>) (CE:Map<ColumnName,B*ColumnType>) m =
      // I'm not sure this is correct in general, but it works for our examples
      let rec flatten ty = 
              match ty with
              | T_Record flds -> List.concat (List.map ( snd >> flatten ) flds)
              | T_Array (flds,e) -> List.map (fun ty -> T_Array (ty,e)) (flatten flds)
              | ty -> [ty]
      match m with 
      |  TypedModel(m',((w,z),t)) ->
         {TH = []
          TW = flatten w 
          TX = T_Record[]
          TY = t}


  let rec trModel tn TE CE  mt : M =
      match mt with 
      | TypedModel(m,t) ->
       match m with 
       | MExp(e) ->  {H = fun () -> (fun k -> k[]); 
                      P = fun h -> (fun k -> k[]);
                      G = fun ([],x) -> trExp tn TE CE e}
       | _ -> failwith "trModel: unexpected %A" m
   
  type ColumnInfo = {B:B; HS:v list; WS: v list}
  let rec trTables 
            (DTO dto as data)
            (TI: Map<TableName,Map<ColumnName,ColumnInfo>>)
            (TE: Map<TableName,Map<ColumnName,ColumnType>>)
            tables = 
      let trTables = trTables data
     
      match tables with
      | [] -> (TI,TE,Skip)
      | (Declaration(Table(tn,_),table)::tables) -> 
       // let (tn,table) = tntable
        let rn = range(tn)
        let (colmap,data) = dto.[tn]
        let data = Seq.toArray(data)
        let length = data.Length
        //ranges.Add(,n)
        rangesInv.Add(rn,length) //hack
        let choosei fchoose ar =  ar |> FArray.mapi(fun i e -> (i,e))|> FArray.choose fchoose 

        let mkArray c cty  =
              let i = colmap.[c]
              let observedIndices = FArray.map (fun (o:obj array) -> if o.[i] = null then false else true) data
              let convert dummy conv = (FArray.map (fun (o:obj array) -> let  oi  = o.[i] in if oi = null then dummy else conv oi) data) :> System.Array
              observedIndices,
              match cty with 
              // note: unsafe conversions
              | T_Link tn ->  convert 0 System.Convert.ToInt32 
              | T_Real ->     convert 0.0 System.Convert.ToDouble 
              | T_Int ->      convert 0 System.Convert.ToInt32 
              | T_Bool ->    convert false System.Convert.ToBoolean 
              | T_Upto _ ->  convert 0 System.Convert.ToInt32 
              | T_String _ -> convert "" System.Convert.ToString 
              | T_Array(ety,e) -> failwith "NYI"
              | T_Record _  -> failwith "NYI"
              | T_Vector -> failwith "NYI"
      (*
        let mkArray c cty  =
              let i = colmap.[c]
              let observedIndices = data |> choosei (fun (k,r) -> if (r.[i] = null) then None else Some k)
              observedIndices,
              match cty with 
              // note: unsafe conversions
              | T_Link tn ->  ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i])|] :> System.Array)
              | T_Real ->     ([| for irow in observedIndices -> System.Convert.ToDouble (data.[irow].[i])|] :> System.Array)
              | T_Int ->      ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i])|] :> System.Array)
              | T_Bool ->     ([| for irow in observedIndices -> System.Convert.ToBoolean(data.[irow].[i])|] :> System.Array)
              | T_Upto _ ->   ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i])|] :> System.Array)
              | T_String _ -> ([| for irow in observedIndices -> System.Convert.ToString (data.[irow].[i])|] :> System.Array)
              | T_Array(ety,e) -> failwith "NYI"
              | T_Record _  -> failwith "NYI"
              | T_Vector -> failwith "NYI"
        *)
        let s = size tn
        let rec trColumns CI CE columns =
          match columns with
          |  [] -> 
            trTables (TI.Add(tn,CI)) (TE.Add(tn,Map.map  (fun col (b,ty) -> ty)  CE)) tables
          | (cn,{Type=ty;Markup=mu})::rest ->
            let _ = match ty with T_Upto(TypedExp(Tabular.Const (IntConst i),_)) -> ignore(rangeOf i )  | _ -> () // HACK
            let cv =  col(tn,cn)
            let r = range(tn)
            match mu with
            | Hyper (TypedExp (_,t) as e) ->
               let CE' = CE.Add(cn,(H,ty))
               let (TI,TE,S) = trColumns CI CE' rest
               (TI,TE,
                trExp tn TE CE e (fun E -> Seq(LetVar(cv,t,E),S)))//TBD
            | Param m ->
              let CE' = CE.Add(cn,(W,ty))
              let mt = typeModel TE CE m
              let m = trModel tn TE CE m
              //todo: derive HVs and WVs from field names of record types mt.TH mt.TH
              let HVs = List.mapi (fun i t -> "H_"+i.ToString()) mt.TH //TBD
              let WVs = List.mapi (fun i t -> "W_"+i.ToString()) mt.TW
              let CI' = CI.Add(cn,{B=W;HS=HVs;WS =WVs})
              let (TI,TE,S) = trColumns CI' CE' rest
              let rec BHs i hs Hs =
                  match hs with 
                  | [] ->
                     m.P (List.rev Hs) 
                         (fun ws -> BWs 0 ws [])
                  |  (E::hs) ->
                     let h = colfield(tn,cn,List.nth HVs i)
                     (Seq (LetVar (h,List.nth mt.TH i, E), BHs (i+1) hs (Var h::Hs)))
              and BWs i ws WS =
                  match ws with
                  | [] ->
                    (m.G (List.rev WS, Var "") (fun E -> Seq(LetVar(cv, mt.TY,E),S)))
                  | (E::ws) -> 
                     let w = colfield(tn,cn,List.nth WVs i)
                     (Seq (LetVar (w,List.nth mt.TH i, E), BWs (i+1) ws (Var w::WS)))
              (TI,TE,m.H() (fun hs -> BHs 0 hs []))  
            | Input ->
               let CE' = CE.Add(cn,(Y,ty))
               let (TI,TE,S) = trColumns CI CE' rest
               (TI,TE,
                Seq(LetConstArray(cv,r,ty,snd(mkArray cn ty)), //TBC
                    S))
            | Observable m 
            | Latent m -> 
              let CE' = CE.Add(cn,(Y,ty))
              let mt = typeModel TE CE m
              let m = trModel tn TE CE m
              //todo: derive HVs and WVs from field names of record types mt.TH mt.TH
              let HVs = List.mapi (fun i t -> "H_"+i.ToString()) mt.TH //TBD
              let WVs = List.mapi (fun i t -> "W_"+i.ToString()) mt.TW
              let CI' = CI.Add(cn,({B=Y;HS=HVs;WS =WVs}))
              let (TI,TE,S) = trColumns CI' CE' rest
              let rec BHs i hs Hs =
                  match hs with 
                  | [] ->
                     m.P (List.rev Hs) 
                         (fun ws -> BWs 0 ws [])
                  |  (E::hs) ->
                     let h = colfield(tn,cn,List.nth HVs i)
                     (Seq (LetVar (h,List.nth mt.TH i,E), BHs (i+1) hs (Var h::Hs)))
              and BWs i ws WS =
                  match ws with
                  | [] ->
                    match mu with 
                    | Observable _ ->
                      let vb = col(tn,col(cn,"mask"))
                      let vbi = col(tn,col(cn,"i"))
                      let vi = col(tn,col(cn,"val"))
                      let (mask,data) = mkArray cn ty
                      (
                         Seq (LetConstArray(cv,r,ty,data),
                         Seq (LetConstArray(vb,r,T_Bool,mask),
                              Seq(ForEach(r,
                                          m.G (List.rev WS, Var "") (fun E ->
                                          Seq(LetVar(vbi,T_Bool,IndexRng(Var(vb),r)),
                                              Seq(LetVar(vi,ty,E),
                                                  Seq(If(vbi,Observe(cv,r,Var vi)),
                                                      IfNot(vbi,Assign(cv,r,Var vi))))))),

                                  S))))
                    | Latent _ ->
                         Seq (LetArray(cv,r,ty),
                              Seq(ForEach(r,
                                          m.G (List.rev WS, Var "") (fun E -> Assign(cv,r,E))),
                                  S))
                  | (E::ws) -> 
                     let w = colfield(tn,cn,List.nth WVs i)
                     (Seq (LetVar (w,List.nth mt.TH i,E), BWs (i+1) ws (Var w::WS)))
              (TI,TE,m.H() (fun hs -> BHs 0 hs []))
        let (TI,TE,S) = trColumns Map.empty Map.empty table
        (TI,TE,Seq(LetVar(s,T_Int,Const length), //to be observed
               Seq(LetRng(rn, s),
                   S)))


  
    
  and trSchemaWithInfo(data,schema:Schema) =
      // reset state
      i <- 0
      ranges.Clear()
      rangesInv.Clear()
      rangeCtxt := fun S -> S
      decRangesSchema schema
      let (TI,TE,S) =  trTables data Map.empty Map.empty schema
      (TI,TE,(!rangeCtxt) S)

(*
  and trSchema(schema:Schema) =
      let (TI,TE,S) = trSchemaWithInfo(schema:Schema) 
      S                 
      *)
 // let rangeindex r = r+"_i"
  let rec EToR2 e : string = 
      let EsToString (es:E list) = (System.String.Join (",",[| for e in es -> EToR2 e |]))
      match e with
        | Var v -> v
        | Rng r -> r + "/*range*/"
        | Const i -> i.ToString()
        | DiscreteConst (i,n) ->  i.ToString() // TODO set value range                                
        | RealConst r ->  r.ToString()
        | BoolConst b ->  if b then "true" else "false" 
        | RealArrayConst b -> sprintf "new double[]{%O}" (System.String.Join(",",b))
        | IndexRng (e1,r) -> sprintf  "%O[ %O]" (EToR2 e1) (rangeindex r) 
        | Index (e1,e2) -> sprintf "%O[ %O]" (EToR2 e1) (EToR2 e2) 
        | Prim(Prim.Gt,[e1;e2]) ->  sprintf "%O > %O" (EToR2 e1) (EToR2 e2) 
        | Prim(Prim.GtEq,[e1;e2]) ->  sprintf  "%O >= %O" (EToR2 e1) (EToR2 e2)   
        | Prim(Prim.Lt,[e1;e2]) ->  sprintf "%O < %O"   (EToR2 e1) (EToR2 e2) 
        | Prim(Prim.LtEq,[e1;e2]) ->  sprintf "%O <= %O" (EToR2 e1) (EToR2 e2)   
        | Prim(Prim.Eq,[e1;e2]) -> 
              sprintf  "(%O == %O)" (EToR2 e1) (EToR2 e2) 
        | Prim(Prim.Minus,[e1;e2])  -> 
              sprintf  "(%O - %O)" (EToR2 e1) (EToR2 e2) 
        | Prim(Prim.Plus,[e1;e2])  -> 
              sprintf  "(%O + %O)" (EToR2 e1) (EToR2 e2) 
        | Prim(Prim.Or, [e1;e2])  -> sprintf "(%O | %O)" (EToR2 e1) (EToR2 e2) 
        | Prim(Prim.Mult, [e1;e2])  -> sprintf   "(%O * %O)" (EToR2 e1) (EToR2 e2)
        | Prim(Prim.Factor (FactorName s), es) ->
              sprintf "Factor.%O(%O)" s (EsToString es)
        | Prim(p, es) ->
              sprintf "Prim.%A(%O)" p (EsToString es)
        | Arr es -> String.concat ","  (Microsoft.FSharp.Collections.Array.map EToR2 es) 
        | Dist(GaussianFromMeanAndPrecision,[e0;e1]) -> 
              sprintf "Normal.SampleWithMeanAndPrecision(%O,%O)" (EToR2 e0) (EToR2(e1)) 
        | Dist(GaussianFromMeanAndVariance,[e0;e1]) -> 
              sprintf "Normal.SampleWithMeanAndVariance(%O,%O)" (EToR2 e0) (EToR2(e1)) 
        | Dist(GammaFromShapeAndScale,[e0;e1]) ->
              sprintf "Gamma.SampleWithShapeAndScale(%O,%O)" (EToR2 e0) (EToR2(e1)) 
       // | Dist(GammaFromShapeAndRate,[e0;e1]) ->
       //       sprintf "Gamma.SampleFromShapeAndRate(%O,%O)" (EToR2 e0) (EToR2(e1)) 
        | Dist(Dist.Beta,[e0;e1]) ->
              sprintf "Beta.Sample(%O,%O)" (EToR2 e0) (EToR2(e1)) //?
        | Dist(Dist.Binomial,[e0;e1]) ->
              sprintf "Binomial.Sample(%O,%O)" (EToR2 e0) (EToR2(e1)) //?
        | Dist(Bernoulli,[e0]) -> 
              //BUG: treating precision as stddev
              sprintf "Bernoulli.Sample(%O)" (EToR2 e0)  //?
        | Dist(Discrete,[e0;e1]) -> 
              //BUG: treating precision as stddev
              sprintf "Categorical.Sample(%O)" (EToR2 e1)  //?
          //  sprintf "Normal.FromMeanAndPrecision(%O,%O).Sample()" (EToR2 e0) (EToR2(e1))
        | Dist(d,es) -> sprintf "Variable.%A(%O)" d (EsToString es)
        | _ -> sprintf "??%A??" e
  
  let rec tyToR2  ty = 
        match ty with
        | T_Int -> "int"
        | T_Real -> "double"
        | T_Bool -> "bool"
        | T_String -> "string"
        | T_Array (ty,e) -> tyToR2 ty + "[]"
        | T_Upto e -> sprintf "int"
        | T_Link _ -> "int"
        | T_Vector -> "Maths.Vector"
        | T_Record flds -> sprintf "{%O}" (System.String.Join(",",[|for (n,ty) in flds -> sprintf "%O = %O" n (tyToR2 ty)|]))
        | t -> sprintf "??%A??" t

  let rec StoR2 tab s : string = 
       //let sprintf fmt k = "\n"+tab+(sprintf fmt k)
       match s with
       | CloneRng (s,r) -> tab+sprintf "int %O = %O (*.Clone()*);" s r 
       | LetRng (r,i) -> tab+sprintf "int %O = %O;" r i 
       | LetVar (v,t,e) -> tab+sprintf "%O %O; %O = %O;" (tyToR2 t) v v (EToR2 e) // "t v; v = e" 
       | LetNew (v,t) -> tab+sprintf "%O %O;" (tyToR2 t) v
       | LetArray (v,r,t) -> 
           let c = try rangesInv.[r].ToString() with _ -> r
           tab+(sprintf "%O[] %O; %O = new %O[%O];" (tyToR2 t) v v (tyToR2 t) c  )
       | LetConstArray (v,r,t,cs) ->
          let r = rangesInv.[r]
          let toS t (o:obj) = match t with
                        | T_Bool -> let b = o :?> bool in if b then "true" else "false"
                        | T_Int | T_Link _ | T_Upto _ -> o.ToString()
                        | T_Real -> o.ToString()
                        | T_String -> "\""+o.ToString()+"\"" 
                        | _ -> failwith "LetConstArray: unexpected consant"
          //todo: print bools lowercase
          tab+(sprintf "%O[] %O; %O = new %O[%O]{%O};" (tyToR2 t) v v (tyToR2 t) r (System.String.Join(",",[| for i in 0 .. cs.Length-1 -> toS t (cs.GetValue(i)) |])))
       | ObserveValue(v,C) ->
           tab+(sprintf "%O.ObservedValue=%O;" v C)
       | Assign (v,r,E) -> 
           tab+(sprintf "%O[%O] = %O;" v (rangeindex r) (EToR2 E))
       | AssignIndex (v,Ei,E) -> 
           tab+(sprintf "%O[%O] = %O;" v (EToR2 Ei) (EToR2 E))
       |  SetTo(v,E) ->
           tab+(sprintf "%O = %O;" v (EToR2 E)) 
       |  Seq (S1,S2) -> 
           (sprintf "%O%O" (StoR2 tab S1) (StoR2 tab S2))
       | LetCopy(v,E) -> 
            tab + (sprintf "var %O = %O;" v (EToR2 E)) //TBC needs type
       | SetValueRange(v,r) ->
            tab + (sprintf "/* %O.SetValueRange(%O); */" v r)
    (*
       |  ForEach(r,S) ->
           let ri = rangeindex r
           tab + (sprintf "for(int %O = 0; %O < %O; %O++) {%O" ri ri r ri (StoR2 (tab+" ") S)) + tab + "}"
     *)
       |  ForEach(r,S) ->
           let ri = rangeindex r // assumed rangeindex r in scope
           let bound = try rangesInv.[r].ToString() with _ -> r
           tab + (sprintf "for(%O = 0; %O < %O; %O++) {%O" ri ri bound ri (StoR2 (tab+" ") S)) + tab + "}" 
       |  IfNot(v,S) ->
            tab + (sprintf "if (!(%O)) {%O"  v (StoR2 (tab+" ") S)) + tab + "} else {}"
       |  If(v,S) ->
            tab + (sprintf "if (%O) {%O" v (StoR2 (tab+" ") S)) + tab + "} else {}"
       |  Skip -> ""
       |  Switch(v,S) ->
            tab + (sprintf "(* using(Variable.Switch(%O)) *) {%O" v (StoR2 (tab+" ") S)) + tab + "}"
       |  Observe (v,r,E) -> 
           tab+(sprintf "Observer.Observe(%O[%O] == %O);" v (rangeindex r) (EToR2 E))
 
  let rec hoistDecs Decs Body S =
      match S with 
       | Skip -> (Decs,Body)
       | Seq(S1,S2) -> let (D1,B1) = hoistDecs Decs Body S1
                       hoistDecs D1 B1 S2
       | If(b,S1) -> 
         let (D1,B1) = hoistDecs Skip Skip S1
         (Seq(Decs,D1),Seq(Body,If(b,B1)))
       | IfNot(b,S1) ->
         let (D1,B1) = hoistDecs Skip Skip S1
         (Seq(Decs,D1),Seq(Body,IfNot(b,B1)))
       // these declare both the range and the indexing variable
       | CloneRng (s,r) -> Seq(Decs,Seq(LetNew (s,T_Int),LetNew (rangeindex s,T_Int))), Seq(Body,SetTo(s,Var r)) 
       | LetRng (r,v) -> Seq(Decs,Seq(LetNew (r,T_Int),LetNew (rangeindex r,T_Int))), Seq(Body,SetTo(r,Var v)) 
       | LetVar (v,t,e) ->  Seq(Decs,LetNew (v,t)), Seq(Body,SetTo(v,e)) 
       | LetNew (v,t) -> Seq(Decs,S),Body
       | LetArray (v,r,t) -> 
           Seq(Decs,S),Body
       | LetConstArray (v,r,t,cs) ->
           Seq(Decs,S),Body
       | ObserveValue(v,C) ->
           Decs,Seq(Body,S)
       | Assign (v,r,E) -> 
           Decs,Seq(Body,S)
       | AssignIndex (v,Ei,E) -> 
           Decs,Seq(Body,S)
       |  SetTo(v,E) ->
           Decs,Seq(Body,S)
       | LetCopy(v,E) ->  // needs type of v to hoist
            failwith "hoistDecs:LetCopy"
       | SetValueRange(v,r) ->
            Decs,Seq(Body,S)
       |  ForEach(r,S1) ->
          let (D1,B1) = hoistDecs Decs Skip S1
          (D1,Seq(Body,ForEach(r,B1)))
       |  Switch(v,S) ->
          failwith "hoistDecs:Switch"
       |  Observe (v,r,E) -> 
          Decs,Seq(Body,S)
      
  type R2Dist(mean:double,variance:double) =
       override this.ToString() = sprintf "Mean=%O,Variance=%O" mean variance
                       
  
  let compile (name,fullSchema:Schema, algo:IAlgorithm option, numberOfIterations : int option, cts : CancellationToken option) (DTO dto as data) =
      let (log,err,(typedCoreSchema,schemaType)) = Elaborator.elaborate(fullSchema)
      let errors = Map.fold (fun s tb log -> Map.fold (fun s col v -> 
                                                     match v with 
                                                     |  (Table.Err msg) ->
                                                      s+(sprintf "\n %A %A : %A" tb col msg)
                                                     | _ -> s) 
                                                     s log)

                          "" log
      System.Console.WriteLine(errors)
      if err then failwithf "type-checking error %A" log   
      System.Console.WriteLine(Pretty.schemaToStr typedCoreSchema)
      let (TI,TE,s) = trSchemaWithInfo(data,typedCoreSchema)
      let (decs,body) = hoistDecs Skip Skip s
      let s = Seq(decs,body)
      let cs = StoR2 "\n     " s
      // System.Console.WriteLine(cs)
       
  
      
      let getMean (line:string) = let [|_;mean|] = line.Split([|"Mean: ";System.Environment.NewLine|],System.StringSplitOptions.None) in System.Double.Parse(mean)
      let getVariance (line:string) = let [|_;var|] = line.Split([|"Variance: ";System.Environment.NewLine|],System.StringSplitOptions.None) in System.Double.Parse(var)

 
      let rec trTables vsToInfer tables = 
          match tables with
          | [] ->  List.rev vsToInfer
          | (Declaration(Table(tn,_),table)::tables) ->
            let rec trColumns vsToInfer columns   =
              match columns with
              |  [] -> 
                trTables  vsToInfer tables
              | (cn,{Type=ty;Markup=m})::rest ->
                if Types.det ty = Qry
                then trColumns vsToInfer rest // skip queries
                else
                let r = range(tn)
                match m with
                | Hyper _ -> 
                  trColumns  vsToInfer rest
                | Param _ ->
                  trColumns ((ty,col(tn,cn))::vsToInfer)  rest
                | Input ->
                  trColumns vsToInfer rest    
                | Latent _ ->
                  trColumns ((T_Array(ty,SizeOf(tn)),col(tn,cn))::vsToInfer) rest
                | Observable _ -> 
                  trColumns ((T_Array(ty,SizeOf(tn)),col(tn,cn))::vsToInfer)  rest  
            trColumns vsToInfer table

      let vsToInfer = trTables [] typedCoreSchema
      let tys = System.String.Join(",",[|for (ty,_) in vsToInfer -> tyToR2 ty |])
      let ids = System.String.Join(",",[|for (_,tncn) in vsToInfer -> tncn|])
      let cs = 
          sprintf "using MicrosoftResearch.R2Lib;
                   using MicrosoftResearch.R2Lib.Distributions;
                   class %O {
                     public Tuple<%O> Model() {
                      %O 
                      return new Tuple<%O>(%O);
                     }
                   }" name tys cs tys ids
      
      let model = @"C:\temp\r2model.cs"
      let () = System.IO.File.WriteAllText(model,cs)
      let csvname tn = @"C:\temp\"+tn + ".txt"
      System.Console.WriteLine(cs)
      let writeDTO = dto |> 
                       Map.iter (fun tn (colmap,os) -> 
                                 let headers = Seq.toArray(colmap.Keys)
                                 let is = Microsoft.FSharp.Collections.Array.map (fun h -> colmap.[h]) headers
                                 let lines = seq { yield System.String.Join(",",headers)
                                                   for o in os do 
                                                    yield System.String.Join(",",[| for i in is -> o.[i] |])  }             
                                 System.IO.File.WriteAllLines(csvname tn,lines))
      let files = System.String.Join(" ",Seq.map csvname dto.Keys)
      let res = runR2 (defaultArg numberOfIterations 1000) model
      let lines = res.Split([|System.Environment.NewLine|],System.StringSplitOptions.None) |> Array.filter (fun s -> s.StartsWith("<")) |> Array.toList
      let (distMap,_) = 
          List.fold (
            fun (map,lines) var ->
                  match var with
                  | (T_Array(ty,SizeOf(tn)),v)-> 
                    let (colmap,data) = dto.[tn]
                    let data = Seq.toArray(data)
                    let length = data.Length
                    let dist :obj [] = Array.create length null
                    let rec readLines n lines =
                          match (n,lines) with 
                          | n,_ when n = length -> lines
                          | n,  meanLine::varLine::lines -> 
                                dist.[n] <- new R2Dist(getMean meanLine,getVariance varLine) :> obj
                                readLines (n + 1) lines
                    let lines = readLines 0 lines
                    (Map.add v (dist:>obj) map),lines   
                  | (ty,v) ->
                    match lines with 
                    |  meanLine::varLine::lines -> 
                       let dist = new R2Dist(getMean meanLine,getVariance varLine)
                       (Map.add v (dist:>obj) map),lines) (Map.empty,lines) vsToInfer

      
      System.Console.WriteLine(res)
        
      let distDTO = 
            DistDTO (  TI |> Map.map(fun tn colmap-> 
                                      let colmap = Map.filter (fun cn (colinfo:ColumnInfo) -> colinfo.B = Y && List.exists (fun (ty,v) -> col(tn,cn) = v) vsToInfer) colmap
                                      let cols = Seq.toList(colmap.Keys)
                                      let c2i = cols |> List.mapi (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                      let tblsize = Seq.length(snd(dto.[tn]))
                                      let rowsize = Seq.length colmap.Keys
                                      let dists = Microsoft.FSharp.Collections.Array.create<System.Array> rowsize null
                                      let _ = List.iteri (fun i cn -> 
                                                           dists.[i] <- distMap.[col(tn,cn)]  :?> System.Array) cols
                                      let colmapInv = Map.fold(fun (inv:Map<_,_>) c i -> inv.Add(i,c)) Map.empty colmap
                                      let tbl = [| for row in 0..tblsize-1 -> Microsoft.FSharp.Collections.Array.init rowsize (fun i -> dists.[i].GetValue(row))  |]
                                      (c2i, tbl :> seq<obj[]>)))
      let knowDTO = KnowDTO (TI |> Map.map (fun tn colmap ->
                                              let colmap = Map.filter (fun cn (colinfo:ColumnInfo) -> colinfo.B = W  && List.exists (fun (ty,v) -> col(tn,cn) = v) vsToInfer ) colmap
                                              let cols = Seq.toList(colmap.Keys)
                                              let c2i = cols |> List.mapi (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                              let array = [| for cn in cols -> distMap.[col(tn,cn)] |]
                                              (c2i,
                                               array)))   

      (typedCoreSchema,0.0,(distDTO,knowDTO))
        (*                        
        
        let rec trTables (TE:Map<TableName,int * Map<ColumnName,int>>) tables = 
          match tables with
          | [] -> TE
          | (Table(tn,table)::tables) ->
            let (colmap,data) = dto.[tn]
            let data = Seq.toArray(data)
            let length = data.Length
            let mkArray c cty  =
              let i = colmap.[c]
              match cty with 
              // note: unsafe conversions
              | T_Link tn -> ([| for d in data -> System.Convert.ToInt32(d.[i])  |] :> System.Array)
              | T_Real -> ([| for d in data -> System.Convert.ToDouble(d.[i])  |] :> System.Array)
              | T_Int -> ([| for d in data -> System.Convert.ToInt32(d.[i]) |] :> System.Array)
              | T_Bool -> ([| for d in data -> System.Convert.ToBoolean(d.[i])|] :> System.Array)
              | T_Upto _ -> ([| for d in data -> System.Convert.ToInt32(d.[i]) |] :> System.Array)
              | T_String _ -> ([| for d in data -> System.Convert.ToString(d.[i])  |] :> System.Array)
              | T_Array(ety,e) -> failwith "NYI"
              | T_Record _  -> failwith "NYI"
              | T_Vector -> failwith "NYI"
  
            let s = size tn
            VE.[s].observeValue(Seq.length(snd(dto.[tn])))
            let rec trColumns (CE: Map<ColumnName,int>) columns   =
              match columns with
              |  [] -> 
                trTables (TE.Add(tn,(length,CE))) tables
              | (cn,{Type=ty;Markup=m})::rest ->  
                let r = range(tn)
                match m with
                | Hyper _ -> 
                  trColumns CE rest
                | Param _ ->
                 // let CE = CE.Add(cn, (Seq.length CE.Keys))
                  trColumns CE rest
                | Input ->
                  let av = AE.[col(tn,cn)]
                  av.observeValue(mkArray cn ty)
                  trColumns CE rest    
                | Latent _ ->
                  let CE = CE.Add(cn, (Seq.length CE.Keys))
                  trColumns CE rest  
                | Observable _ -> 
                  let av = AE.[col(tn,cn)]
                  av.observeValue(mkArray cn ty)   
                  trColumns CE rest  
            trColumns Map.empty table
        let TE = trTables Map.empty typedCoreSchema
        let vsToInfer = Map.fold (fun vs n (v:Variable) ->  if true ||(not(v.IsObserved)) then ((v:>IVariable)::vs) else vs) [evidence:>IVariable] VE
        let vsToInfer = Map.fold (fun vs n (v:Variable) ->  if true ||(not(v.IsObserved)) then ((v:>IVariable)::vs) else vs) vsToInfer AE
       // ie.OptimiseForVariables <- List.toArray(vsToInfer)
        let eD = ie.Infer<Bernoulli>(evidence)
      //  let VD,AD = Map.map(fun n (v:Variable) -> if (not(v.IsObserved)) then ie.Infer(v) else null ) VE, Map.map(fun n (v:Variable) -> if (not(v.IsObserved)) then ie.Infer(v) else null) AE
        let VD,AD = Map.fold(fun (VD:Map<v,obj>) n (v:Variable) -> if true || (not(v.IsObserved)) then VD.Add(n,ie.Infer(v)) else VD ) Map.empty VE,
                    Map.fold(fun (AD:Map<v,obj>) n (v:Variable) -> if true || (not(v.IsObserved)) then AD.Add(n,ie.Infer(v)) else AD) Map.empty AE
        let distDTO = 
            DistDTO (  TE |> Map.map(fun tn (tblsize,colmap) -> 
                                      let rowsize = Seq.length colmap.Keys
                                      let dists = Microsoft.FSharp.Collections.Array.create<System.Array> rowsize null
                                      let _ = Map.iter (fun cn i -> 
                                                           dists.[i] <- (ie.Infer(AE.[col(tn,cn)]):?> ConvertibleToArray).ToArray()) colmap
                                      let colmapInv = Map.fold(fun (inv:Map<_,_>) c i -> inv.Add(i,c)) Map.empty colmap
                                      let tbl = [| for row in 0..tblsize-1 -> Microsoft.FSharp.Collections.Array.init rowsize (fun i -> dists.[i].GetValue(row))  |]
                                      (colmap, tbl :> seq<obj[]>)))
   //     let knowDTO = KnowDTO (Map.empty) //TBC
        let knowDTO = KnowDTO (TI |> Map.map (fun tn colmap ->
                                              let colmap = Map.filter (fun cn colinfo -> not (List.isEmpty colinfo.WS) || colinfo.B = W ) colmap
                                              let cols = Seq.toList(colmap.Keys)
                                              let c2i = cols |> List.mapi (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                              let array = [| for cn in cols -> 
                                                                 let {HS=HS;WS=WS;B=B} = colmap.[cn]
                                                                 let dWs = 
                                                                  //   [ for w in WS -> ie.Infer(VE.[colfield(tn,cn,w)]) ]
                                                                     List.foldBack (fun w w'dWs -> (w,ie.Infer(VE.[colfield(tn,cn,w)])):> obj ::w'dWs) WS []
                                                                 let dWs = if (B = W) then ie.Infer(VE.[col(tn,cn)]) :: dWs else dWs
                                                                 match dWs with
                                                                   [] -> failwith "impossible"
                                                                 | [d] -> d
                                                                 | dists ->
                                                                   let tupleTy = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType([| for w in dWs -> typeof<obj>|])
                                                                   Microsoft.FSharp.Reflection.FSharpValue.MakeTuple(List.toArray(dists),tupleTy)
                                                          |]
                                              (c2i,
                                               array)))
                                                                 
                                              
                                              
                                     
       // printfn "evidence %A" eD
       // printfn "%A %A" VD AD
        (eD.LogOdds,(distDTO,knowDTO))
      *)

  let latentModel(name,db) = 
    { new LatentModel() with 
      member x.TrainAndPredictWithLogEvidence(din:DTO, ?algo:IAlgorithm, ?numberOfIterations:int, ?cts:CancellationToken) : Schema * float*(DistDTO * KnowDTO) = 
          compile(name,db, algo, numberOfIterations, cts)(din) }


