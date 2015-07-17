module MicrosoftResearch.Infer.Tabular.Library


open Syntax



let prelude : Schema =
    [
      Declaration(Fun "CDiscrete",
           ["N", {Type=T_Int;  Markup=Hyper(Const (IntConst 2))};
            "Alpha", {Type=T_Real;  Markup=Hyper(Const (RealConst 1.0))};
            "V", {Type=makeDet T_Vector R;
                  Markup=Param(MExp(Prim(Factor(FactorName "BreakSymmetry"),[Dist(DirichletSymmetric,[Var "N"; Var "Alpha"])])))};
            "ret", {Type=makeDet (T_Upto (Exp.Var "N")) R; 
                          Markup=Observable(MExp(Dist(Discrete,[Var "N"; Var "V"])))} ]) 
      Declaration(Fun "CBernoulli",
          [ "alpha", {Type=T_Real; Markup=Hyper (Const (RealConst 1.0))};
            "beta",  {Type=T_Real; Markup=Hyper (Const (RealConst 1.0))};
            "Bias",  {Type=makeDet T_Real R; Markup=Param (MExp (Prim(Factor(FactorName "BreakSymmetry"),[Dist (Beta, [Var "alpha"; Var "beta"])])))};
            "ret", {Type=makeDet T_Bool R; Markup=Observable (MExp (Dist (Bernoulli, [Var "Bias"])))}])

      Declaration(Fun "CGaussian",
          ["MeanMean",   {Type=T_Real;  Markup= Hyper(Const (RealConst 0.0))};
                       "MeanPrec",   {Type=T_Real;  Markup=Hyper(Const (RealConst 1.0))};
                       "Shape",  {Type=T_Real;  Markup=Hyper(Const (RealConst 1.0))};
                       "Scale",  {Type=T_Real;  Markup=Hyper(Const (RealConst 1.0))};
                       "Mean",    {Type=makeDet T_Real R;  Markup=Param(MExp(Dist(GaussianFromMeanAndPrecision,[Var "MeanMean"; Var "MeanPrec"])))};
                       "Prec",    {Type=makeDet T_Real R;  Markup=Param(MExp(Dist(GammaFromShapeAndScale,[Var "Shape"; Var "Scale"])))};
                       "ret", {Type=makeDet T_Real R;  Markup=Observable(MExp(Dist(GaussianFromMeanAndPrecision,[Var "Mean"; Var "Prec"])))} ]);
       
    ]

#if TBC
open NewTabular
open Types
module Tabular = NewTabular

open Checker
open Model
open Table
let Var = Exp.Var

let Entries : Declaration list =
    [ Fun ("CDiscrete",
           ["N", {Type=T_Int;  Markup=Hyper(Const (IntConst 2))};
            "V", {Type=T_Vector;
                  Markup=Param(MExp(Dist(Dirichlet,[Var "N"; ForLoop ("i", Var"N", Const (RealConst 1.0))])))};
            "D", {Type=T_Upto (Exp.Var "N"); 
                              Markup=Latent(MExp(Dist(Discrete,[Var "N"; Var "V"])))} ])]

    (*

    {CName = "CBernoulli";
     Table = {Columns=["hAlpha", {Type=T_Real; Markup=Latent(Hyper(RealConst 1.0))};
                       "hBeta",  {Type=T_Real; Markup=Latent(Hyper(RealConst 1.0))};
     
                       "Bias",   {Type=T_Real; Markup=Latent(Prior(Beta(Column "hAlpha", Column "hBeta")))};
                       "B",      {Type=T_Bool; Markup=Latent(Gen(Bernoulli(Column "Bias")))} ]};
     E = Column "B"
     };
     {CName = "CGaussian";
     Table = {Columns=["hMean",   {Type=T_Real;  Markup=Latent(Hyper(RealConst 0.0))};
                       "hPrec",   {Type=T_Real;  Markup=Latent(Hyper(RealConst 1.0))};
                       "hShape",  {Type=T_Real;  Markup=Latent(Hyper(RealConst 1.0))};
                       "hScale",  {Type=T_Real;  Markup=Latent(Hyper(RealConst 1.0))};
                       "Mean",    {Type=T_Real;  Markup=Latent(Prior(Gaussian(Column "hMean", Column "hPrec")))};
                       "Prec",    {Type=T_Real;  Markup=Latent(Prior(Gaussian(Column "hShape", Column "hScale")))};
                       "G",       {Type=T_Real;  Markup=Latent(Gen(Gaussian(Column "Mean", Column "Prec")))} ]};
     E = Column "G"
    }] *)

let getErrors (log:Table.Log) : string =
    let folder  = fun curr c value ->
                    match value with
                    Table.Err msg -> c + ":" + msg + "\n"
                    | _ -> curr
    Map.fold folder "" log

let rec typecheckEntry (entry:LibEntry<Markup>) : (Log * Error *LibEntryTyped * Level) =
    let (log,err,tb1) = synthTable G_Empty entry.Table
                                     
    if err then 
       //failwith (sprintf "synthesizing library model %O failed\nerror msg: %O" entry.CName (log.Item entry.CName))
       let EMT = (ERT,ERT,ERT,ERT)
       let ETT = (ERT,ERT,ERT,ERT,ERT)
       (log, err,({CName = entry.CName; Table=({Columns=[]},ETT);E=Types.Column "bogus", ERT},EMT) , bot)
    else
    let (_, t) = tb1
    let (h, w, _, _, z) = t
    let g4 = match z with //insert all entries into gamma
               Types.T_Record ls ->
                 let folder = fun g -> fun (x, t1) -> envInsertVar g x (t1,(R,Y))
                 List.fold folder G_Empty ls
             | _ -> failwith "error: output type of an array should be a record type"
    //TODO: check if all vars in entry.E are the columns of the table
    let (e',l) = synthExpr g4 entry.E
    let t' = extractType e'
    let outputLibElem:LibEntryUntyped = {CName = entry.CName; Table = tb1; E = e'}
    (log,false, (outputLibElem, (h, w, ERT, t')), l)

let rec buildLibraryRec (logStr:string) (err:Error) (entries:LibEntry<Markup> list) (g:Types.Env) : string * Error * Types.Env =
    match entries with
    | [] -> (logStr, err, g)
    | hd :: tl ->
        let (log', err', (elem, q),l) = typecheckEntry hd
        let newLogStr = logStr + (getErrors log')
        let g' = envInsertModel g hd.CName (q,l)
        buildLibraryRec (newLogStr) (err||err') tl g'

let buildLibrary () : string * Error * Types.Env =
    buildLibraryRec "" false Entries Types.G_Empty

#endif