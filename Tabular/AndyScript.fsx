// Andy codes too, on Thursdays, in December

#r "bin\Debug\Tabular.dll";;

open MicrosoftResearch.Infer.Tabular.Syntax
open MicrosoftResearch.Infer.Tabular.Pretty
open MicrosoftResearch.Infer.Tabular.Plates
module P = MicrosoftResearch.Infer.Tabular.Pretty

let CBernoulli =
  Fun("CBernoulli",
      ["Alpha", {Type=T_Det(B_Int, D); Markup=Hyper (Const (IntConst 1))};
       "Beta",  {Type=T_Det(B_Int,D); Markup=Hyper (Const (IntConst 1))};
       "Bias",  {Type=T_Det(B_Real,R); Markup=Param (MExp (Dist (Beta, [Var "Alpha"; Var "Beta"])))};
       "CBernoulli", {Type=T_Det(B_Bool,D); Markup=Observable (MExp (Dist (Bernoulli, [Var "Bias"])))}] )

let CDiscrete =
  Fun("CDiscrete",
      ["N",     {Type=T_Det(B_Int, D); Markup=Hyper (Const (IntConst 2))};
       "Alpha", {Type=T_Det(B_Int,D); Markup=Hyper (Const (RealConst 1.0))};
       "V",     {Type=T_Det(B_Vector,R); Markup=Param (MExp (Dist (DirichletSymmetric, [Var "N"; Var "Alpha"])))};
       "CDiscrete", {Type=T_Det(B_Upto(Var "N"),D); Markup=Observable (MExp (Dist (Discrete, [Var "V"])))}] )

let CGaussian =
  Fun("CGaussian",
      ["Mu", {Type=T_Det(B_Real, D); Markup=Hyper (Const (RealConst 0.0))};
       "Tau",  {Type=T_Det(B_Real,D); Markup=Hyper (Const (RealConst 1.0))};
       "Kappa", {Type=T_Det(B_Real, D); Markup=Hyper (Const (RealConst 1.0))};
       "Theta",  {Type=T_Det(B_Real,D); Markup=Hyper (Const (RealConst 2.0))};
       "Mean",  {Type=T_Det(B_Real,R); Markup=Param (MExp (Dist (GaussianFromMeanAndPrecision, [Var "Mu"; Var "Tau"])))};
       "Prec",  {Type=T_Det(B_Real,R); Markup=Param (MExp (Dist (GammaFromShapeAndScale, [Var "Kappa"; Var "Theta"])))};
       "CGaussian", {Type=T_Det(B_Real,D); Markup=Observable (MExp (Dist (GaussianFromMeanAndPrecision, [Var "Mean"; Var "Prec"])))}] )

let Add =
  Fun("Add",
    ["X", {Type=T_Det(B_Int,R); Markup=Hyper (Const (IntConst 1))};
     "Y", {Type=T_Det(B_Int,R); Markup=Input};
     "Add", {Type=T_Det(B_Int,R); Markup=Observable (MExp (Prim (Plus, [Var "X"; Var "Y"])))}] )

let MyTable:Declaration =
  Table ("MyTable",
            ["b1", {Type=T_Det(B_Bool, R); Markup=Param(MCall("CBernoulli", []))};
             "theta1", {Type=T_Det(B_Real,R); Markup=Param(MExp (Dist (GaussianFromMeanAndPrecision, [Const(RealConst 0.0); Const(RealConst 1.0)])))};
             "theta2", {Type=T_Det(B_Real,R); Markup=Param(MExp (Dist (GaussianFromMeanAndPrecision, [Var "theta1"; Const(RealConst 1.0)])))};
             "a",  {Type=T_Det(B_Int, D);  Markup=Param(MExp(Const (IntConst 3)))};
             "b",  {Type=T_Det(B_Int, R);  Markup=Latent(MCall("Add", ["Y", Var "a"]))};
             "z",  {Type=T_Det(B_Upto(Const (IntConst 10)), R); Markup=Latent (MExp (Dist (DiscreteUniform, [Const (IntConst 10)])))};
             "b2", {Type=T_Det(B_Bool, R); Markup=Observable(MIndexed(MCall("CBernoulli", ["Alpha",Var "a"]),Var "z", Const(IntConst 10)))};
             "b3", {Type=T_Det(B_Bool, R); Markup=Observable(MCall("CBernoulli", ["Beta", Const(IntConst 2); "Alpha",Var "a"]))} ])



let SimpleTable:Declaration =
  Table ("SimpleTable",
            ["b1", {Type=T_Det(B_Bool, R); Markup=Param(MCall("CBernoulli", []))};
             "b2", {Type=T_Det(B_Bool, R); Markup=Observable(MCall("CBernoulli", []))} ])

// if S : Q and S'=normalize(S) then S':Q also

let S:Schema = [CBernoulli; SimpleTable]

let test (S:Schema) =
  let S' = coreS S
  printf "\nBefore normalization:\n%s\n\nAfter normalization:\n%s\n\n" (schemaToStr S) (schemaToStr S')
  ()

do test S

// POPL'14 examples

let LR:Declaration =
  Table("LinearRegression", ["muA" , {Type=T_Real           ; Markup=Hyper(Const(RealConst 0.0))};
                  "muB" , {Type=T_Real           ; Markup=Hyper(Const(RealConst 0.0))};
                  "A"   , {Type=T_Real           ; Markup=Param(MExp (Dist (GaussianFromMeanAndPrecision, [Var "muA"; Const(RealConst 1.0)])))};
                  "B"   , {Type=T_Real           ; Markup=Param(MExp (Dist (GaussianFromMeanAndPrecision, [Var "muB"; Const(RealConst 1.0)])))};
                  "X"   , {Type=T_Real           ; Markup=Input};
                  "Z"   , {Type=T_Real           ; Markup=Latent(MExp (Prim (Plus, [(Prim (Mult, [Var "X"; Var "A"])); Var "B"])))}; 
                  "Y"   , {Type=T_Real           ; Markup=Observable(MExp (Dist (GaussianFromMeanAndPrecision, [Var "Z"; Const(RealConst 1.0)])))};
                                        ])
                                        
let Players =
  Table("Players",
         ["skill", {Type=T_Det(B_Real,R);
                    Markup=Latent(MExp (Dist (GaussianFromMeanAndPrecision, [Const(RealConst 0.0); Const(RealConst 1.0)])))}])
let Matches =
  Table("Matches",
        ["player1", {Type=T_Det(B_Int, R); Markup=Input};
         "player2", {Type=T_Det(B_Int, R); Markup=Input};
         "perf1", {Type=T_Det(B_Real, R);
                   Markup=Latent(MExp (Dist (GaussianFromMeanAndPrecision, [DeRef(Var"player1","Players","skill"); Const(RealConst 1.0)])))};
         "perf2", {Type=T_Det(B_Real, R);
                   Markup=Latent(MExp (Dist (GaussianFromMeanAndPrecision, [DeRef(Var"player2","Players","skill"); Const(RealConst 1.0)])))};
         "outcome", {Type=T_Det(B_Real,R); Markup=Observable(MExp(Prim(Gt,[Var "perf1"; Var "perf2"])))}])
         
let MoG1 =
  Table("MoG1",
    ["z",    {Type=T_Det(B_Bool,R); Markup=Latent(MCall("CBernoulli",[]))};
     "g1",   {Type=T_Det(B_Real,R); Markup=Latent(MCall("CGaussian",[]))};
     "g2",   {Type=T_Det(B_Real,R); Markup=Latent(MCall("CGaussian",[]))};
     "y",    {Type=T_Det(B_Real,R); Markup=Observable(MExp(If(Var"z",Var"g1",Var"g2")))}])

let MoG2 =
  Table("MoG2",
    ["n",    {Type=T_Det(B_Int,D); Markup=Hyper(Const(IntConst 5))};
     "z",    {Type=T_Det(B_Bool,R); Markup=Latent(MCall("CDiscrete",["N",Var"n"]))};
     "y",    {Type=T_Det(B_Real,R); Markup=Observable(MIndexed(MCall("CGaussian",[]),Var"z",Var"n"))}])

let S2:Schema = [CBernoulli; CGaussian; MoG1]

let User =
  Table("User",
        ["Uclusters",    {Type=T_Det(B_Int,D); Markup=Hyper(Const(IntConst 4))};
         "Uz",       {Type=T_Det(B_Int,R); Markup=Latent(MCall("CDiscrete",["N",Var"Uclusters"]))};
         "Name",    {Type=T_Det(B_String, R); Markup=Input};
         "IsMale",  {Type=T_Det(B_Bool,R); Markup=Observable(MIndexed(MCall("CBernoulli",[]),Var"Uz",Var"Uclusters"))}
         "Age",     {Type=T_Det(B_Int,R); Markup=Observable(MIndexed(MCall("CDiscrete",["N",Const(IntConst 100)]),Var"Uz",Var"Uclusters"))}])

let Movie =
  Table("Movie",
        ["Mclusters",    {Type=T_Det(B_Int,D); Markup=Hyper(Const(IntConst 4))};
         "Mz",       {Type=T_Det(B_Int,R); Markup=Latent(MCall("CDiscrete",["N",Var"Mclusters"]))};
         "Title",    {Type=T_Det(B_String, R); Markup=Input};
         "Genre",    {Type=T_Det(B_Int,R); Markup=Observable(MIndexed(MCall("CDiscrete",["N",Const(IntConst 7)]),Var"Mz",Var"Mclusters"))}
         "Year",      {Type=T_Det(B_Int,R); Markup=Observable(MIndexed(MCall("CDiscrete",["N",Const(IntConst 100)]),Var"Mz",Var"Mclusters"))}])

let Rating =
  Table("Rating",
        ["u", {Type=T_Det(B_Int, R); Markup=Input};
         "m", {Type=T_Det(B_Int, R); Markup=Input};
         "Score", {Type=T_Det(B_Int,R); Markup=Observable(let M1=MCall("CDiscrete",["N",Const(IntConst 5)]) in
                                                          let M2=MIndexed(M1,DeRef(Var"u","User","Uz"),Var"Uclusters") in
                                                          MIndexed(M2,DeRef(Var"m","Movie","Mz"),Var"Mclusters"))}])

let LDA1 =
  let Words = Table("Words", ["Word", {Type=T_Det(B_String, R); Markup=Input}])
  let Docs = Table("Docs", ["Filename", {Type=T_Det(B_String, R); Markup=Input}])
  let Occs =
    Table("Occs",
          ["Doc", {Type=T_Det(B_Upto(SizeOf("Docs")), R); Markup=Input};
           "Position", {Type=T_Det(B_Int, R); Markup=Input};
           "NTopics", {Type=T_Det(B_Int,D); Markup=Hyper(Const(IntConst 10))};
           "Topic", {Type=T_Det(B_Int,R); Markup=Latent(let M1=MCall("CDiscrete",["N",Var"NTopics"; "Alpha",Const(RealConst 15.0)]) in
                                                            MIndexed(M1,Var"Doc",SizeOf"Docs"))};
           "Word", {Type=T_Det(B_Int,R); Markup=Observable(let M1=MCall("CDiscrete",["N",SizeOf("Words"); "Alpha",Const(RealConst 0.1)]) in
                                                            MIndexed(M1,Var"Topic",Var"NTopics"))}])
  [CDiscrete;Words;Docs;Occs]


let LDA2 =
  let Words = Table("Words", ["Word", {Type=T_Det(B_String, R); Markup=Input}])
  let Docs = Table("Docs", ["Filename", {Type=T_Det(B_String, R); Markup=Input};
                            "NTopics", {Type=T_Det(B_Int,D); Markup=Hyper(Const(IntConst 10))};
                            "Alpha", {Type=T_Det(B_Real,D); Markup=Hyper(Const(RealConst 15.0))};
                            "V", {Type=T_Det(B_Vector, R); Markup=Latent(MExp(Dist (DirichletSymmetric, [Var "NTopics"; Var "Alpha"])))}])
  let Occs =
    Table("Occs",
          ["Doc", {Type=T_Det(B_Int, R); Markup=Input};
           "Position", {Type=T_Det(B_Int, R); Markup=Input};
           "Topic", {Type=T_Det(B_Int,R); Markup=Latent(MExp(Dist (Discrete, [DeRef(Var"Doc","Docs","V")])))};
           "Word", {Type=T_Det(B_Int,R); Markup=Observable(let M1=MCall("CDiscrete",["N",SizeOf("Words"); "Alpha",Const(RealConst 0.1)]) in
                                                            MIndexed(M1,Var"Topic",Var"NTopics"))}])
  [CDiscrete;Words;Docs;Occs]


// temporal models

let SkillsTT =
  Table("Skills",
         ["N", {Type=T_Det(B_Int,D); Markup=Hyper(Const(IntConst 10))};
          "base", {Type=T_Array(T_Det(B_Int,D),Var"N"); Markup=Param(MExp( ForLoop("j",Var"N",Var"j") ))};
          "skill", {Type=T_Det(B_Real,R);
                    Markup=Latent(MExp( let e0 = Dist (GaussianFromMeanAndPrecision, [Const(RealConst 0.0); Const(RealConst 1.0)])
                                        let e1 = Dist (GaussianFromMeanAndPrecision, [Var "s"; Const(RealConst 1.0)])
                                        let e2 = Scan("s","i",e1,e0,Var"base")
                                        e0 ))} ])



let MatchesTT =
  Table("Matches",
        ["player1", {Type=T_Det(B_Int, R); Markup=Input};
         "player2", {Type=T_Det(B_Int, R); Markup=Input};
         "perf1", {Type=T_Det(B_Real, R);
                   Markup=Latent(MExp (Dist (GaussianFromMeanAndPrecision, [DeRef(Var"player1","Skills","skill"); Const(RealConst 1.0)])))};
         "perf2", {Type=T_Det(B_Real, R);
                   Markup=Latent(MExp (Dist (GaussianFromMeanAndPrecision, [DeRef(Var"player2","Skills","skill"); Const(RealConst 1.0)])))};
         "outcome", {Type=T_Det(B_Real,R); Markup=Observable(MExp(Prim(Gt,[Var "perf1"; Var "perf2"])))}])

// what are we trying to do?
// visualize the semantics of core Tabular programs as factor graphs with plates
// TODO
// eliminate edges to hyperparameter
// minimal mode
//
//let s1 = platesAdg "popl14" [CBernoulli; CGaussian; CDiscrete; Players; Matches]
//// let s2 = plates "InfernoDB" [CBernoulli; CGaussian; CDiscrete; User; Movie; Rating]
//// let lda1 = plates "LDA1" LDA1
//let lda2 = platesAdg "LDA2" LDA2

// let test = plates "TrueSkillTT" [SkillsTT]

//////////////////////////////////////////////////////////////////////////////////////////////////////////////

type TableName = string
type AttrName = string
type AttrType = Input | Output | Link of TableName | Text
type Table = List<AttrName * AttrType>
type Schema = List<TableName * Table>

// goal: construct a linear model given a relational schema
// hunch: using table instead of arrays, to better fit with relational tools (eg Power BI tools)
// v1: we ignore the links 

let Sales:Schema =
 ["States", ["ID", Text];
 "Retailers", ["ID", Text; "State", Link("States")];
  "Sales", ["Retailer", Link("Retailers"); "Price", Input; "Temp", Input; "Volume", Output]]

let residual y = sprintf "r_%s" y
let intercept y = sprintf "i_%s" y
let weight x y = sprintf "w_%s_%s" x y
let factor x y = sprintf "f_%s_%s" x y


let parms tb ls = List.fold (fun tb1 tb2 -> sprintf "%s_%s" tb1 tb2) (sprintf "params_%s" tb) ls

let sum ts = List.reduce (fun t1 t2 -> sprintf "(%s+%s)" t1 t2) ts
let product ts = List.reduce (fun t1 t2 -> sprintf "(%s*%s)" t1 t2) ts

let inputs T = List.collect (fun(nme,ty) -> if ty=Input then [nme] else []) T
let outputs T = List.collect (fun(nme,ty) -> if ty=Output then [nme] else []) T
let links T = List.collect (fun(nme,ty) -> match ty with Link(tab) -> [tab] | _ -> []) T

let model_input (tb:TableName) (nme:AttrName) =
  [tb, sprintf "%s,real,input" nme]

// define weight param for x wrt y
let model_io (tb:TableName) (ls:List<AttrName>) y x =
  [parms tb ls, sprintf "%s,real,latent,Gaussian()" (weight x y);
   tb, sprintf "%s,real,local,(%s*%s)" (factor x y) x (weight x y)]

let model_output (tb:TableName) (ls:List<AttrName>) xs y =
  let xws = sum (List.map (fun x -> factor x y) xs) in
  List.collect (model_io tb ls y) xs @
  [parms tb ls, sprintf "%s,real,latent,Gaussian()" (intercept y);
   tb, sprintf "%s,real,latent,Gaussian()" (residual y);
   tb, sprintf "%s,real,output,%s+%s+%s" y xws (intercept y) (residual y)]

let model_table (tb:TableName,T:Table) =
  let xs = inputs T
  let ys = outputs T
  let ls = links T
  List.collect (fun l -> [parms tb ls, sprintf "%s,top,input" l]) ls @
  List.collect (model_input tb) xs @ List.collect (model_output tb ls xs) ys
let model_schema (S) = List.collect model_table S
let csv xys = List.fold (fun csv (x,y) -> csv+x+","+y+"\n") "" xys
do System.IO.File.WriteAllText(@"C:\Users\adg\Desktop\linear.csv", "Table,Attribute,Type,Viz,Model1\n"+csv (model_schema Sales))

//

type c = string
type t = One | Index of c * c | Global of c 
type e = List<t>
type F = Formula of c * e

type ty = Number | Link of TableName
type S = List<string * ty>
let s = ["RETAILER", Link("RETAILER"); "CITY", Link("CITY"); "CHAIN", Link("CHAIN");
         "VOLUME", Number; "DISP", Number; "PRICE", Number]

let wellformed (S:S, F:F): bool = failwith "todo"
let search (S:S, response:string): F = failwith "todo"

type Model = List<string * string>  // list of tablename and column att|ty|viz|exp

let semantics_t (t:t): Model * string = failwith "todo"
let semantics_e (table:string) (e:e): Model = failwith "todo"
let semantics_F (table:string) (F:F): Model =failwith "todo"

let test1 = Formula("VOLUME", [])
let test2 = Formula("VOLUME", [One])
let test3 = Formula("VOLUME", [Global("PRICE")])
let test4 = Formula("VOLUME", [Index("PRICE","INDEX")])

let o = semantics_F "SALES" test1

// goal: generate actual Tabular sheets that work against the Marcin demo dataset