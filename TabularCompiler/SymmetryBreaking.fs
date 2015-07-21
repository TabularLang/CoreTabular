namespace MicrosoftResearch.Infer.Tabular

module SymmetryBreaking =

  module T = Syntax
  open Syntax
  open Target
  open Ranges
  open Translate
  open Compiler

  open MicrosoftResearch.Infer
  open MicrosoftResearch.Infer.Factors
  open MicrosoftResearch.Infer.Maths
  open MicrosoftResearch.Infer.Distributions
  open MicrosoftResearch.Infer.Models

  type IDist<'T> = IDistribution<'T>

  [<AbstractClass>]
  type AbsDist() =
       abstract member singleton: unit -> obj
       abstract member array: int -> AbsDist

  type MkStructDist<'T,'U when 
                     'T:> IDist<'U> and
                     'T:(new:unit->'T) and
                     'T:> System.ValueType and
                     'T:> Sampleable<'U> and
                     'T:struct and 'T:> SettableToProduct<'T> and 'T:> SettableToRatio<'T> and 'T:> SettableToPower<'T> and 'T:>SettableToWeightedSum<'T> and 'T:>CanGetLogAverageOf<'T> and 'T:>CanGetLogAverageOfPower<'T> and 'T:>CanGetAverageLog<'T>  >  (gen: unit -> 'T) =
       inherit AbsDist()
       override this.singleton()  = gen() :> obj
       override this.array(n:int) = MkRefDist(fun () ->  Distribution<'U>.Array<'T>( [| for i in 1..n-> gen() |]) :?> DistributionStructArray<'T,'U> ):> AbsDist
                                                      
  and MkRefDist<'T,'U when 
                     'T:> IDist<'U> and
                     'T:> Sampleable<'U> and
                     'T: not struct and
                     'T:> SettableTo<'T> and
                     'T:> SettableToProduct<'T> and 'T:> SettableToRatio<'T> and 'T:> SettableToPower<'T> and 'T:>SettableToWeightedSum<'T> and 'T:>CanGetLogAverageOf<'T> and 'T:>CanGetLogAverageOfPower<'T> and 'T:>CanGetAverageLog<'T>  >  (gen: unit -> 'T) =
       inherit AbsDist()
       override this.singleton()  = gen() :> obj
       override this.array(n:int) = MkRefDist(fun () ->  Distribution<'U>.Array<'T>( [| for i in 1..n-> gen() |]) :?> DistributionRefArray<'T,'U>) :> AbsDist

  let dirichletInit n  = 
        Distributions.Dirichlet([| for i in 1 .. n -> 1.0 + Rand.Double()*0.01|])

  let rec breakExpressionSymmetry (sizeOf:Map<TableName,int>) (TypedExp(e,t)) : AbsDist option = 
      match e with
      | T.ForLoop(v,TypedExp(e1,t1),e2) ->
          match e1 with 
          | T.Const(IntConst n)  ->
            match breakExpressionSymmetry sizeOf e2 with
               | Some d ->  Some (d.array n)
               | None -> None
          | T.SizeOf(tn) ->
            match breakExpressionSymmetry sizeOf e2 with
               | Some d -> Some (d.array (sizeOf.[tn]))
               | None -> None
          | _ -> None
      | T.Prim(T.Factor(T.FactorName("BreakSymmetry")),[T.TypedExp(T.Dist(d,es),_)]) ->
           match d,es with
           | (Bernoulli,[TypedExp(e0,_)]) ->
             match e0 with
             | T.Const(T.RealConst(p)) ->
               Some (MkStructDist (fun () -> 
                                       let b = MicrosoftResearch.Infer.Maths.Rand.Double() < p
                                       Distributions.Bernoulli.PointMass(b) 
                               ):>AbsDist)
             | T.Dist(Beta,[TypedExp(T.Const(T.RealConst(a)),_);TypedExp(T.Const(T.RealConst(b)),_)]) ->
               Some (MkStructDist(fun () ->
                            let p = a / (a+b)
                            let b = MicrosoftResearch.Infer.Maths.Rand.Double() < p
                            Distributions.Bernoulli.PointMass(b)) :> AbsDist)

             | _ -> None //TBC
           | (Dirichlet | DirichletUniform |DirichletSymmetric), ((TypedExp(e0,_))::es) ->
             match e0 with 
             | T.Const(IntConst n)  ->
                Some (MkRefDist<Distributions.Dirichlet,Vector>(fun () -> dirichletInit(n)):> AbsDist)
             | T.SizeOf(tn) ->
                let n = sizeOf.[tn]
                Some (MkRefDist<Distributions.Dirichlet,Vector>(fun () -> dirichletInit(n)) :> AbsDist)
             | _ -> None //TBC
           | Discrete, ((TypedExp(e0,_))::es) ->
             match e0 with 
             | T.Const(IntConst n) ->
                Some (MkRefDist(fun () -> Distributions.Discrete.PointMass(MicrosoftResearch.Infer.Maths.Rand.Int(n),n)):>AbsDist)
             | T.SizeOf(tn) ->
                let n = sizeOf.[tn]
                Some (MkRefDist(fun () ->Distributions.Discrete.PointMass(MicrosoftResearch.Infer.Maths.Rand.Int(n),n)):>AbsDist)
             | _ -> None 
           | _,_ ->
             None
      | _ -> None


  let breakModelSymmetry (sizeOf:Map<TableName,int>) (TypedModel(MExp e,_)) = breakExpressionSymmetry sizeOf e


  let breakSymmetries (sizeOf:Map<TableName,int>) (RE,VE:Map<v,Variable>,AE:Map<v,Variable>) (typedCoreSchema:(*typed core*) Schema) =
      let rec trTables  tables =
        match tables with
          | [] -> ()
          | (Declaration(Table(tn,_),table)::tables) ->
            let size = sizeOf.[tn]
          //  let s = size tn
            let rec trColumns columns   =
              match columns with
              |  [] -> 
                trTables tables
              | (cn,{Type=ty;Markup=m})::rest -> 
                if Types.det ty = Qry 
                then trColumns rest
                else 
                let r = range(tn)
                match m with
                | Hyper _ -> 
                  trColumns rest
                | Param m ->
                  let v = VE.[col(tn,cn)]
                  match breakModelSymmetry sizeOf m with
                  | Some f -> ignore( initializeTo (f.singleton()) v); trColumns rest
                  | None ->  trColumns rest
                | Input ->
                  trColumns  rest    
                | Latent m ->
                  let av = AE.[col(tn,cn)]
                  match breakModelSymmetry sizeOf m with
                  | Some f -> ignore( initializeTo (f.array(size).singleton()) av); trColumns rest
                  | None ->  trColumns rest
                | Observable m -> 
                  let av = AE.[col(tn,cn)]
                  match breakModelSymmetry sizeOf m with
                  | Some f -> ignore( initializeTo (f.array(size).singleton()) av); trColumns rest
                  | None ->  trColumns rest
            trColumns table
      trTables  typedCoreSchema

