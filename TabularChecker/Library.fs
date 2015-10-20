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
      Declaration(Fun "Error",
          ["Scale",   {Type=T_Real;  Markup= Hyper(Const (RealConst 10000.0))};
                       "Noise",    {Type=makeDet T_Real R;  Markup=Param(MExp(Dist(GammaFromShapeAndScale,[Const(RealConst 1.0); Var "Scale"])))};
                       "ret", {Type=makeDet T_Real R;  Markup=Observable(MExp(Dist(GaussianFromMeanAndPrecision,[Const(RealConst 0.0); Var "Noise"])))} ]);
       
    ]

