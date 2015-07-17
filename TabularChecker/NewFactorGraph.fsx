#r "bin\Debug\Tabular.dll";;
#r "bin\Debug\TabularChecker.exe";;

open MicrosoftResearch.Infer.Tabular
open MicrosoftResearch.Infer.Tabular.Syntax
open MicrosoftResearch.Infer.Tabular.Pretty
open MicrosoftResearch.Infer.Tabular.Plates

open MicrosoftResearch.Infer.Tabular.Types

module P = MicrosoftResearch.Infer.Tabular.Pretty

//print upto types as mod types

do P.uptoAsMod := true

type Type = Syntax.ColumnType
type Level = Table | Row
type Visibility = Input | Local | Output
type CoreModel = Epsilon | MExp of Syntax.Exp

type Column = (ColumnName * (Type * Level * Visibility * CoreModel))

type Table = Column list
type Schema = (ColumnName*Table) list

// translations from current Core Syntax to paper Core syntax
let fromColumn (cncol:ColumnName*Syntax.Column) : Column = 
    match cncol with
    | (cn,{Type=T;Markup=Syntax.Input}) ->
      (cn,(T,Row,Input,Epsilon))
    | (cn,{Type=T;Markup=Syntax.Latent (Syntax.MExp E)})
    | (cn,{Type=T;Markup=Syntax.Observable (Syntax.MExp E)}) ->
      (cn,(T,Row,Output,MExp E))  
    | (cn,{Type=T;Markup=Syntax.Hyper E}) ->
      (cn,(T,Table,Output,MExp E))     
    | (cn,{Type=T;Markup=Syntax.Param (Syntax.MExp E)}) ->
      (cn,(T,Table,Output,MExp E)) 
    | _ -> failwith "fromColumn: non-core input"

let fromDec dec = 
    match dec with
    | Syntax.Table (tn,cols) -> (tn,List.map fromColumn cols)
    | Syntax.Fun _ -> failwith "fromDec: non-core input"

let fromSchema S = 
    List.map fromDec S 

type Value = Exp // we use symbolic DB values, abusing the Exp type

// what are nodes really - "structured" variable names or mangled ones?
type Node =  ColumnName 
         // | Dot of (TableName * Node) // why Node, why not just ColumnName?
type Colour = Visibility // only used for rendering
type Edge = Var of Node * Type * Exp * colour:Colour
          | Let of Node * Type * Exp
          | Obs of Exp * Node
          | Plate of Syntax.Exp * Syntax.VarName * Edge List (* inlined Graph type *)
type Graph = Edge List


type TB = Map<ColumnName,Value>
type DB = Map<TableName,int * TB>

// hacks to represent finite map lookups - won't typecheck as Fun
//let DBLookup DB tn = Exp.Subscript(DB,Exp.Var tn)
//let TBLookup TB cn = Exp.Subscript(TB,Exp.Var cn)

let DBLookup (DB:DB) tn = DB.[tn]
let TBLookup (i,(TB:TB)) cn = 
    match cn with
//    | "#size" -> Const (IntConst i)
    | _ -> try TB.[cn]  with _ -> failwith (cn + " missing")
let TBSize (i,TB)  = i

// some (injective) maps from Tabular identifiers to node names. 
//let size t = t + "_#size"

let sep = "_"
let dot t c = System.String.Join(sep,[| (t:string); (c:string) |])
let index (t:string) = "i" // t.Substring(0,1)//dot t "ID"


let rec FS SS DB =
    match SS with 
      [] -> []
    | (t,TT)::SS ->
       (// we no longer bind #size for t since the typecheck won't know to declare it a hyper level. Instead, inline throughout.
        //Let(size t,T_Int,TBLookup (DBLookup DB t) "#size") ::
        FT t Map.empty TT DB @ 
        FS SS DB)

//todo: rename TB (table) to DB below

// translation of base type (including upto)
and FB t level TB b =
    match b with
    | B_Upto E  -> B_Upto (FE t level TB E)
    | B_Vector (* e *) -> B_Vector  (* (Subst exx) explicit case in case we decide to index B_Vector *)
    | b -> b 
// translation of types 
and FTy t level TB T =
    let FT = FTy t level TB 
    match T with 
    | T_Array (T,E) -> T_Array (FT  T, FE t level TB   E)
    | T_Record rt -> T_Record (List.map (fun (c,T) -> (c,FT T)) rt)
    | T_Det (b,d) -> T_Det (FB t level TB  b,d)
// translation of expressions affecting SizeOf, DeRef and Var
and FE t level TB (E: Exp) : Exp =
    let FEfull = FE
    let FE = FE t level TB 
    match E with
    | SizeOf t' -> //Syntax.Var(size t') 
                   Const(IntConst (TBSize (DBLookup TB t')))
    | DeRef(E,t',d) -> Subscript(Syntax.Var(dot t' d),FE E)
    | Syntax.Var c -> match Map.tryFind c level with
                | Some Table -> Syntax.Var (dot t c)
                | Some Row -> Subscript(Syntax.Var (dot t c), Syntax.Var(index t))
                | None -> Syntax.Var c // a let bound variable.
    // boring cases
    | Const(k) -> Const(k)
    | Prim(p,es) -> Prim(p,List.map FE es)
    | Dist(d,es) -> Dist(d,List.map FE es)
    | If(e1,e2,e3) -> If(FE e1,FE e2,FE e3)
    | ForLoop(x,e1,e2) -> // failwith "substE: forloop not implemented - needs occurs check"
        ForLoop(x,FE e1, FEfull t (level.Remove x) TB  e2) 
    | Array(es) -> Array(List.map FE es)
    | Subscript(e1,e2) -> Subscript(FE e1,FE e2)
    | Constraint(e1,t1) -> Constraint(FE e1,t1)
    | Syntax.Let(x,e1,e2) -> //failwith "substE: Let not implemented - needs occurs check" 
      Syntax.Let(x,FE e1, FEfull t (level.Remove x) TB  e2) 
    | Scan(_,_,_,_,_) -> failwith "NYI"
    | Infer(d,es,x,e) -> Infer(d,List.map FE es,x,FE e)
    | TypedExp(e,T) -> TypedExp(FE e, FTy t level TB T  )

// translation of tables, one column at a time. Introduces one plate per row level declaration.
and FT t level TT TB = 
    match TT with 
    | [] -> []
    // table level
    | (c,(T,Table,Input,Epsilon))::TT  ->
       [Var(dot t c,FTy t level TB T, TBLookup (DBLookup TB t) c, colour=Input)]
       @
       FT t (Map.add c Table level) TT TB
    | (c,(T,Table,Visibility.Local,MExp E))::TT  ->
       [Let(dot t c ,FTy t level TB T,FE t level TB E)] 
       @
       FT t (Map.add c Table level) TT TB
    | (c,(T,Table,Output,MExp E))::TT -> // do we allow table level output?
       let observed =  Map.containsKey c (snd (DBLookup TB t))
       [Var(dot t c,FTy t level TB T,FE t level TB E, colour=if observed then Output else Local)] @
        (if observed
         then [Obs(TBLookup (DBLookup TB t) c, dot t c)] 
         else []) @
        FT t (Map.add c Table level) TT TB
    // row level
    // declare inputs outside plate, as array
    | (c,(T,Row,Input,Epsilon))::TT ->
       // declare c with *array* type, outside the plate
       let TA = T_Array(FTy t level TB T,Const(IntConst (TBSize (DBLookup TB t)))) 
       [Var(dot t c,TA,TBLookup (DBLookup TB t) c,colour=Input)] @ // inputing a row is just inputting the entire row (no plate required)
       FT t (Map.add c Row level) TT TB
    (* 
    // alernatively, declare input inside plate, with indexing into inlined array value
    | (c,(T,Row,Input,Epsilon))::TT ->
       let TA = T_Array(FTy t level TB T,Const(IntConst (TBSize TB))) 
       //[Var(dot t c,TA,TBLookup TB c,colour=Local)] @
       [Plate(Const(IntConst (TBSize TB)),//TBLookup TB "#size",
              index t,
              [Var(dot t c,FTy t level TB T, FE t level TB (Subscript(TBLookup TB c,Syntax.Var (index t))),colour=Input)])]
       @
       FT t (Map.add c Row level) TT TB
    *)
    | (c,(T,Row,Visibility.Local,MExp E))::TT  ->
       [Plate(Const(IntConst (TBSize (DBLookup TB t))),//TBLookup TB "#size",
              index t,
              [Let(dot t c,FTy t level TB T, FE t level TB E)])]
       @
       FT t (Map.add c Row level) TT TB
    | (c,(T,Row,Output,MExp E))::TT ->
       let observed =  Map.containsKey c (snd (DBLookup TB t))
       [Plate(Const(IntConst (TBSize (DBLookup TB t))),//TBLookup TB "#size",
              index t,
              [Var(dot t c,FTy t level TB T, FE t level TB E,colour=if observed then Output else Local)])]
       @
       (if observed
        then [Obs(TBLookup (DBLookup TB t) c, dot t c)] 
        else [])@ // observing a row output is just observing the entire array outside the plate.
       FT t (Map.add c Row level) TT TB
  //  | _ -> failwith ("FC: illegal input")

         

let TCE g E T = 
   try match Checker.checkExpr B.W g E T with Syntax.TypedExp(E',T) -> T 
   with e -> failwithf  "%A %A \n %A" e g E 
let envInsertVar g x T = envInsertVar g x (T,B.W)
let rec concat g g' =
        match g' with 
        | G_Empty -> g
        | G_Var ((x,(T,B)),g') -> G_Var((x,(T,B)),concat g g')  
//let envInsertVar g x T = envInsertVar g x (T,B.H)
let rec TCG g G =
        match G with
        | [] -> G_Empty
        | Var(x, T, E,_)::G' ->
          let T = Checker.isWellFormed g T
          let T = TCE g E T
          let g' = TCG (envInsertVar g x T) G'
          envInsertVar g' x T
        | Let(x,T, E)::G' ->
          let T = Checker.isWellFormed g T
          let T = TCE g E T
          TCG (envInsertVar g x T) G'
        | Plate(n,x,G) :: G' ->
          let EN =  (TypedExp(n,T_Int))
          let gx = envInsertVar g x (T_Upto EN)
          let rec vecG g' = 
              match g' with 
              | G_Empty -> G_Empty
              | G_Var ((xi,(Ti,_)),g'') -> envInsertVar (vecG g'') xi (T_Array(Ti,EN))
              | _ -> failwith "impossible"
          let g' = TCG gx G
          let g' = vecG g'
          let g'' = TCG (concat g g') G'
          concat g' g''
        | Obs(v,x) :: G' ->
          let (T,_) = getType g x
          let U = TCE g v T
         // if not (Checker.areTypesEquivalent g T U) then failwith "bad observation"
          TCG g G'

         
 
          

open Syntax
let Players =
  Syntax.Table("Players",
         [ //"Name", {Type=T_String; Markup=Input}; // todo: Need string constants to reflect DB values
           "Skill", {Type=T_Det(B_Real,R);
                    Markup=Latent(MExp (Dist (GaussianFromMeanAndVariance, [Const(RealConst 0.0); Const(RealConst 100.0)])))}])
let Matches =
  Syntax.Table("Matches",
        ["Player1", {Type=T_Det(B_Upto(SizeOf("Players")), D); Markup=Input};
         "Player2", {Type=T_Det(B_Upto(SizeOf("Players")), D); Markup=Input};
         "Perf1", {Type=T_Det(B_Real, R);
                   Markup=Latent(MExp (Dist (GaussianFromMeanAndVariance, [DeRef(Var"Player1","Players","Skill"); Const(RealConst 100.0)])))};
         "Perf2", {Type=T_Det(B_Real, R);
                   Markup=Latent(MExp (Dist (GaussianFromMeanAndVariance, [DeRef(Var"Player2","Players","Skill"); Const(RealConst 100.0)])))};
         "Win1", {Type=T_Det(B_Bool,R); Markup=Observable(MExp(Prim(Gt,[Var "Perf1"; Var "Perf2"])))}])

(*
let Bets =
  Syntax.Table("Bets",
        ["Match", {Type=T_Det(B_Upto(SizeOf("Matches")), R); Markup=Input};
         "Odds1", {Type=T_Det(B_Real, R);  Markup=Input};
         "p", {Type=T_Det(B_Real, R);
                   Markup=Latent(MExp (Prim (Prim.Factor(FactorName "infer.Bernoulli.Bias"), [DeRef(Var"Match","Matches","Win1")])))};
         "PlaceBet1", {Type=T_Det(B_Bool,R); Markup=Observable(MExp(Prim(Gt,[Prim(Mult,[Var "Odds1"; Var "p"]); 
                                                                             Prim(Minus,[Const(RealConst 1.0);Var "p"])])))}
         ])
 *)
let Bets =
  Syntax.Table("Bets",
        ["Match", {Type=T_Det(B_Upto(SizeOf("Matches")), D); Markup=Input};
         "Odds1", {Type=T_Det(B_Real, D);  Markup=Input};
         "p", {Type=T_Det(B_Real, Qry);
                   Markup=Latent(MExp (Infer(Bernoulli,[],"Bias",DeRef(Var"Match","Matches","Win1"))))};
         "U", {Type=T_Array(T_Det(B_Real, Qry),Const (IntConst 3));
                   Markup=Latent(MExp (Array([Const(RealConst 1.0);Const(RealConst -1.0);Prim(Mult,[Var "Odds1"; Var "p"])])))};
         "EU", {Type=T_Array(T_Det(B_Real, Qry),Const (IntConst 2));
                   Markup=Latent(MExp (Array([Subscript(Var "U",Const(IntConst 1));
                                              Prim(Plus,[
                                                         Prim(Mult,[Prim(Minus,[Const(RealConst 1.0);Var "p"]);
                                                                    Subscript(Var "U",Const(IntConst 1))]);
                                                         Prim(Mult,[Var "p";
                                                                    Subscript(Var "U",Const(IntConst 2))])])])))
  
                                             };
         "PlaceBet1", {Type=T_Det(B_Upto(Const (IntConst 2)),Qry); Markup=Observable(MExp(Prim(Factor(FactorName "ArgMax"),[Var "EU"])))}
         ])
        
let BetaBernoulli = 
  Syntax.Table("BetaBernoulli",
       ["Bias",  {Type=T_Det(B_Real,R); Markup=Param (MExp (Dist (Beta, [Const (RealConst 1.0); Const (RealConst 1.0)])))};
        "Coin", {Type=T_Det(B_Bool,D); Markup=Observable (MExp (Dist (Bernoulli, [Var "Bias"])))}]) 

(*
let Coins = 
  Syntax.Table("Coins",
       ["V",  {Type=T_Det(B_Vector,R); Markup=Param (MExp (Dist (Dirichlet, [Const (IntConst 2); Array [Const (RealConst 1.0);Const (RealConst 1.0)]])))};
        "Flip",  {Type=T_Det(B_Upto(Const (IntConst 2)),R); Markup=Observable (MExp (Dist (Discrete, [Const (IntConst 2); Var "V" ])))};
        "iCounts",  {Type=T_Array(T_Det(B_Real,Qry),Const (IntConst 2)); Markup=Param (MExp (Infer(Dirichlet,[Const (IntConst 2)],"counts",Var "V")))};
        "alpha",  {Type=T_Det(B_Real,Qry); Markup=Param (MExp (Subscript(Var "iCounts",Const (IntConst 1))))};
        "beta",  {Type=T_Det(B_Real,Qry); Markup=Param (MExp (Subscript(Var "iCounts",Const (IntConst 0))))};
        "Mean",  {Type=T_Det(B_Real,Qry); Markup=Param (MExp (Prim(Prim.Div,[Var "alpha";Prim(Plus,[Var "alpha";Var "beta"])])))}])
*)
let Coins = 
  Syntax.Table("Coins",
       ["V",  {Type=T_Det(B_Vector,R); Markup=Param (MExp (Dist (Dirichlet, [Const (IntConst 2); Array [Const (RealConst 1.0);Const (RealConst 1.0)]])))};
        "Flip",  {Type=T_Det(B_Upto(Const (IntConst 2)),R); Markup=Observable (MExp (Dist (Discrete, [Const (IntConst 2); Var "V" ])))};
        "counts",  {Type=T_Array(T_Det(B_Real,Qry),Const (IntConst 2)); Markup=Param (MExp (Infer(Dirichlet,[Const (IntConst 2)],"counts",Var "V")))};
       // "alpha",  {Type=T_Det(B_Real,Qry); Markup=Param (MExp (Subscript(Var "iCounts",Const (IntConst 1))))};
        //"beta",  {Type=T_Det(B_Real,Qry); Markup=Param (MExp (Subscript(Var "iCounts",Const (IntConst 0))))};
        "Mean",  {Type=T_Det(B_Real,Qry); Markup=Param (MExp (Prim(Prim.Div,[Subscript(Var "counts",Const (IntConst 1));Prim(Plus,[Subscript(Var "counts",Const (IntConst 1));Subscript(Var "counts",Const (IntConst 0))])])))}])

let TrueSkillSS = fromSchema [Players;Matches];

let TrueSkillBetsSS = fromSchema [Players;Matches;Bets];

let BetaBernoulliSS = fromSchema [BetaBernoulli];

let CoinsSS = fromSchema [Coins];

let TrueSkillBetsDB = 
     Map.empty.Add("Players",(3,Map.empty))
              .Add("Matches",(3,Map.empty.Add("Player1",Syntax.Array [Const (IntConst 0);Const (IntConst 1);Const (IntConst 0)])
                                         .Add("Player2",Syntax.Array [Const (IntConst 1);Const (IntConst 2);Const (IntConst 2)])
                                         .Add("Win1",Syntax.Array [Const (BoolConst false);Const (BoolConst false);Var "?"])))
              .Add("Bets",(1,Map.empty.Add("Match",Syntax.Array [Const (IntConst 2)])
                                          .Add("Odds1",Syntax.Array [Const (RealConst 4.0)])))
                                
let BetaBernoulliDB = Map.empty.Add("BetaBernoulli",(3,Map.empty.Add("Coin", Syntax.Array [Const (BoolConst true);Const (BoolConst true);Const (BoolConst false)])))

let CoinsDB = Map.empty.Add("Coins",(3,Map.empty.Add("Flip", Syntax.Array [Const (IntConst 1);Const (IntConst 1);Const (IntConst 0)])))

let TrueSkillFG = FS TrueSkillSS TrueSkillBetsDB;

let TrueSkillBetsFG = FS TrueSkillBetsSS TrueSkillBetsDB;

let BetaBernoulliFG = FS BetaBernoulliSS BetaBernoulliDB;

let CoinsFG = FS CoinsSS CoinsDB;

//let g' = TCG G_Empty FG;
//let _ = TCG G_Empty   TrueSkillFG   

//let _ = TCG G_Empty   TrueSkillBetsFG 
let _ = TCG G_Empty   BetaBernoulliFG   
 
//let _ = TCG G_Empty   TrueSkillBetsFG 
let _ = TCG G_Empty  CoinsFG   


module P = MicrosoftResearch.Infer.Tabular.Plates

let split (v:string) = 
    let a = v.Split([|sep|],System.StringSplitOptions.None) 
    try (a.[0],a.[1]) with _ -> failwithf "couldn't split %s" v 

let ObsId tabnme nme = sprintf "Obs.%s.%s" tabnme nme


let deps (tabnme:TableName) factor rho e  =
  let rec f rho e =
   match e with
   | Var v -> if Set.contains(v) rho then [] else 
              let (tn,cn) = split v
              [Edge(VariableId tn cn,None,factor)]
   | Const (_) -> []
   | Prim(_,es) -> List.collect (f rho) es
   | Dist(_,es) -> List.collect (f rho) es
   | SizeOf(t) -> []
   | DeRef(e1,tn,cn) -> //let label = sprintf "%s.ID=%s" tn (exprToStr e1)
                        [Edge(VariableId tn cn,None,factor)] @ f rho e1
   | If(e1,e2,e3) -> List.concat [f rho e1; f rho e2; f rho e3]
   | ForLoop(x,e1,e2) -> let rho' = Set.add x rho in List.concat [f rho e1; f rho' e2]
   | Array(es) -> List.collect (f rho) es
   | Subscript(Var x,e1) -> //let label=sprintf "%s" (exprToStr e1)
                            (if Set.contains(x) rho 
                             then []
                             else [(let (tn,cn) = split x
                                    Edge(VariableId tn cn,None,factor))])
                             @ f rho e1
   | Subscript(e1,e2) -> List.concat [f rho e1; f rho e2] // TODO: do the general case
   | Constraint(e1,t1) -> f rho e1 
   | Let(x,e1,e2) -> let rho' = Set.add x rho in List.concat [f rho e1; f rho' e2]
   | Infer(d,es,x,e0) -> List.collect (f rho) (e0::es)
   | TypedExp(e,ty) -> f rho e
   | _ -> failwith (sprintf "deps: %s unexpected expression" (exprToStr e))

  in f rho e

let unqualify e = 
 let rec f  e =
   match e with
   | Var v -> if v.Contains(sep) 
              then let (tn,cn) = split v 
                   Var cn
              else e
   | Const (_) -> e
   | Prim(p,es) -> Prim(p, List.map f es)
   | Dist(d,es) -> Dist(d, List.map f es)
   | SizeOf _
   | DeRef _-> failwith "unexpected"
   | If(e1,e2,e3) -> If(f  e1, f  e2, f e3)
   | ForLoop(x,e1,e2) -> ForLoop(x,f e1 ,f e2) 
   | Array(es) -> Array (List.map f es)
  // | Subscript(e1,Var x) when false && Set.contains x rho -> f e1
   | Subscript(e1,e2) -> Subscript (f  e1, f  e2) 
   | Constraint(e1,t1) -> Constraint(f e1,t1)
   | Let(x,e1,e2) -> Let(x,f e1 ,f e2) 
   | Infer(d,es,x,e0) -> Infer(d,List.map f es, x, f e0)
   | TypedExp(e,ty) -> f e
   | _ -> failwith (sprintf "unqualify: %s unexpected expression" (exprToStr e))
 in f  e

let ExprNode0 (tabnme:TableName, nme:ColumnName, ty:ColumnType,  rho, subscript, e:Exp,style:string): List<Item> =
  let node = VariableId tabnme nme
  let label = TypedId ty (exprToStr (unqualify (subscript (Var nme))))
  let factor = FactorId tabnme nme
  let e' = unqualify  e
  [P.Node(factor, sprintf "label=\"%s\",shape=box,style=unfilled,fillcolor=black,height=0.1,width=0.1" (exprToStr e'));                 
   Edge(factor,None,node);
   P.Node(node, sprintf "label=\"%s\",%s" label style)]
  @ deps tabnme factor rho e

// deconstruct a possibly typed MExp
let rec private (|TypedForLoop|_|) M = match M with (TypedExp (TypedForLoop e,_)) -> Some e | ForLoop(x,e1,e2) -> Some (x,e1,e2) | _ -> None


let rec ExprNode (tabnme:TableName, nme:ColumnName, ty:ColumnType,  rho, subscript, e:Exp,style:string) =
  match e,ty with
  | TypedForLoop(x,e1,e2),T_Array(ty,_) -> [Cluster(sprintf "%s<%s" x (exprToStr e1),"style=unfilled;color=black;",
                                                    ExprNode(tabnme,nme,ty,(Set.add x rho),(fun e -> Subscript(subscript e,(Var x))), e2,style))]
  | _,_ -> ExprNode0 (tabnme,nme,ty,rho,subscript,e,style) 


let rec PofG rho subscript G = 
    match G with 
    | [] -> []
    | (Edge.Var(x,T,E,colour))::G ->
      let  tn,cn = split x
      let color = match colour with 
                  | Colour.Input -> P.office2013blue
                  | Colour.Output -> P.office2013orange
                  | Colour.Local -> P.office2013gray
      ExprNode(tn,cn,T,rho, subscript, E, "style=filled,color="+color)
      @ (PofG rho subscript G)
    | (Edge.Let(x,T,E))::G ->
      let  tn,cn = split x
      ExprNode(tn,cn,T,rho, subscript, E, "style=filled,color="+P.office2013gray)
      @ (PofG rho subscript G)
    | (Edge.Plate(n,x,G'))::G ->
      let label = sprintf "%s<%s" x (exprToStr n)
      [Cluster(label, "style=unfilled;color=black;", PofG (Set.add x rho) (fun e -> Syntax.Subscript(e,Var x))  G' )] 
      @ (PofG rho subscript G)
    | (Edge.Obs(v,x))::G -> 
      let (tn,cn) = split x
      let node = VariableId tn cn
      let obs = ObsId tn cn
      [P.Node(obs, sprintf "label=\"%s\",shape=box,style=unfilled,fillcolor=black,height=0.1,width=0.1" (exprToStr v));                 
       P.Edge(obs,None,node)] @
      (PofG rho subscript G)

let PofG0 G = 
    let text = sprintf "labeljust=l;nojustify=true;style=filled;color=gray95" 
   // let label = sprintf "%s<%s" x (exprToStr n)
    [Cluster("",text, PofG (Set.empty) (fun e -> e) G )]

let plates nme tmpPath outPath G = 
  let fileName = sprintf @"%s.dot" nme
  let nodes,edges = normal2 (PofG0 G)
  let x = System.IO.File.WriteAllText(tmpPath  + @"\"+  fileName, P.dot (nodes@edges))
  runDot  outPath  tmpPath  fileName
  nodes,edges


let platesCrusso nme  FG = 
   plates nme @"C:\Users\crusso\Desktop\" @"C:\Users\crusso\Desktop" FG

platesCrusso "TrueSkill" TrueSkillFG

platesCrusso "TrueSkillBets" TrueSkillBetsFG

platesCrusso "BetaBernoulli" BetaBernoulliFG

do Pretty.vectorAsArray := Some 2
platesCrusso "Coins" CoinsFG
do Pretty.vectorAsArray := None


let rec edgeToTex e = 
    match e with 
      | Edge.Var(x,t,e,cplour) ->
        sprintf "\\FVAR{%O}{%O}{%O}" x (Tex.TypeToStr t) (Tex.exprToStr e)
      | Edge.Let(x,t,e) ->
        sprintf "\\FVAR{%O}{%O}{%O}" x (Tex.TypeToStr t) (Tex.exprToStr e)
let rec toTex es = 
    match es with 
    | [] -> ""
    | e::es ->
      match e with 
      | Edge.Var(x,t,e,cplour) ->
        sprintf "\\FVAR{%O}{%O}{%O}\\\\\n%O" x (Tex.TypeToStr t) (Tex.exprToStr e) (toTex es)
      | Edge.Let(x,t,e) ->
        sprintf "\\FVAR{%O}{%O}{%O}\\\\\n%O" x (Tex.TypeToStr t) (Tex.exprToStr e) (toTex es)
      | Edge.Obs(v,x) ->
        sprintf "\\Fobserve{%O}{%O}\\\\\n%O" (Tex.exprToStr v) x (toTex es)
      | Edge.Plate(v,x,[e]) ->
        sprintf "\\Fplate[{%O}]{%O}{%O}\\\\\n%O" (Tex.exprToStr v) x (edgeToTex e)  (toTex es)
      
      
   
let s = toTex(TrueSkillBetsFG);;
