namespace MicrosoftResearch.Infer.Fun.FunDBLayer
open MicrosoftResearch.Infer
open MicrosoftResearch.Infer.Models
open MicrosoftResearch.Infer.Distributions
open MicrosoftResearch.Infer.Collections
open MicrosoftResearch.Infer.Tabular
//open MicrosoftResearch.Infer.Tabular.DataLayer
module Tabular2IN =
(*
 open INCompiler
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
 let compile (db, algo:IAlgorithm option, numberOfIterations: int option) =
      let (TI,TE,s) = trDatabaseWithInfo(db)
      let cs = StoString "\n" s
      System.Console.WriteLine( cs)
      let (evidence,(RE,VE,AE)) = interpM  s
      let ie = new InferenceEngine(defaultArg algo (new ExpectationPropagation() :> _))
      ie.ShowMsl <- true
      ie.ShowTimings <- true
      ie.ShowProgress <- false
      ie.Compiler.GenerateInMemory <- true
      ie.NumberOfIterations <- (defaultArg numberOfIterations 30)
      let infer (DTO dto)  =
        let rec trTables (TE:Map<TableName,int * Map<ColumnName,int>>) tables = 
          match tables with
          | [] -> TE
          | (tntable::tables) ->
            let (tn,table) = tntable
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
            trColumns Map.empty table.Columns
        let TE = trTables Map.empty db.Tables
        let vsToInfer = Map.fold (fun vs n (v:Variable) ->  if (not(v.IsObserved)) then ((v:>IVariable)::vs) else vs) [evidence:>IVariable] VE
        let vsToInfer = Map.fold (fun vs n (v:Variable) ->  if (not(v.IsObserved)) then ((v:>IVariable)::vs) else vs) vsToInfer AE
        ie.OptimiseForVariables <- List.toArray(vsToInfer)
        let eD = ie.Infer<Bernoulli>(evidence)
      //  let VD,AD = Map.map(fun n (v:Variable) -> if (not(v.IsObserved)) then ie.Infer(v) else null ) VE, Map.map(fun n (v:Variable) -> if (not(v.IsObserved)) then ie.Infer(v) else null) AE
        let VD,AD = Map.fold(fun (VD:Map<v,obj>) n (v:Variable) -> if (not(v.IsObserved)) then VD.Add(n,ie.Infer(v)) else VD ) Map.empty VE,
                    Map.fold(fun (AD:Map<v,obj>) n (v:Variable) -> if (not(v.IsObserved)) then AD.Add(n,ie.Infer(v)) else AD) Map.empty AE
        let distDTO = 
            DistDTO (  TE |> Map.map(fun tn (tblsize,colmap) -> 
                                      let rowsize = Seq.length colmap.Keys
                                      let dists = Array.create<System.Array> rowsize null
                                      let _ = Map.iter (fun cn i -> 
                                                           dists.[i] <- (ie.Infer(AE.[col(tn,cn)]):?> ConvertibleToArray).ToArray()) colmap
                                      let colmapInv = Map.fold(fun (inv:Map<_,_>) c i -> inv.Add(i,c)) Map.empty colmap
                                      let tbl = [| for row in 0..tblsize-1 -> Array.init rowsize (fun i -> dists.[i].GetValue(row))  |]
                                      (colmap, tbl :> seq<obj[]>)))
   //     let knowDTO = KnowDTO (Map.empty) //TBC
        let knowDTO = KnowDTO (TI |> Map.map (fun tn colmap ->
                                              let colmap = Map.filter (fun cn colinfo -> not (List.isEmpty colinfo.WS)) colmap
                                              let cols = Seq.toList(colmap.Keys)
                                              let c2i = cols |> List.mapi (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                              let array = [| for cn in cols -> 
                                                                 let {HS=HS;WS=WS} = colmap.[cn]
                                                                 let dists = [| for w in WS -> ie.Infer(VE.[colfield(tn,cn,w)]) |]
                                                                 match dists with
                                                                   [||] -> failwith "impossible"
                                                                 | [|d|] -> d
                                                                 | dists ->
                                                                   let tupleTy = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType([| for w in WS -> typeof<obj>|])
                                                                   Microsoft.FSharp.Reflection.FSharpValue.MakeTuple(dists,tupleTy)
                                                          |]
                                              (c2i,
                                               array)))
                                                                 
                                              
                                              
                                     
       // printfn "evidence %A" eD
       // printfn "%A %A" VD AD
        (eD.LogOdds,(distDTO,knowDTO))
      infer
(*
namespace MicrosoftResearch.Infer.Fun.FunDBLayer
    type LatentModel() = 
        //we allow users to cast to what the know is the type
        abstract member LatentModel<'TW,'TX,'TY,'TZ, 'DistW_,'DistY_,'DistZ_  when 'TY : equality and 'TZ : equality> 
          : unit -> LatentModel<'TW,'TX,'TY,'TZ,'DistW_,'DistY_,'DistZ_  >
        abstract member Sample<'TX>  : 'TX -> DTO
        abstract member Sampler      : Map<string, int> -> DTO
        abstract member GetVariableGraph: unit -> VariableGraph
        abstract member TrainAndPredict               : DTO -> DistDTO * KnowDTO
        abstract member TrainAndPredictWithLogEvidence: DTO -> float*( DistDTO * KnowDTO)
        abstract member TrainAndPredictWithLEAndFit   : DTO * bool -> float*( DistDTO * KnowDTO * DistDTO)
        abstract member PackX<'TX> : 'TX -> DTO
        abstract member GSample  :  DTO -> DTO
        abstract member Test<'TX> : 'TX -> bool
        abstract member TestWithParameter<'TW,'TX> : 'TW*'TX -> bool  
       // read : RecordSet -> 'TY  // ich dont think so
       // write: 'TZ -> unit
*)

 type  Model(db) = 
               inherit Compiler.LatentModel()
               //we allow static typing by users to what the know is the type
               override this.LatentModel<'TW,'TX,'TY,'TZ, 'DistW_,'DistY_,'DistZ_  when 'TY : equality and 'TZ : equality> () =
                 failwith "NYI" : LatentModel<'TW,'TX,'TY,'TZ,'DistW_,'DistY_,'DistZ_  > 

               override this.Sample<'TX_> ( x:'TX_) : DTO = 
                 failwith "NYI"

               override this.Sampler inputSizes : DTO = 
                 failwith "NYI"

               override this.GSample(din:DTO) =
                 failwith "NYI"


               //we provide the generic function from the confine of our typed implementation
               override this.TrainAndPredict(din:DTO) : DistDTO * KnowDTO =
                 failwith "NYI"
                 (*
                      let ThisM = m :> obj :?> Model<unit,'TW,'TX,('TY*'TZ)> 
                      let learner:LatentLearner.ILatentLearner<'DistW,'TX,'TY,'TZ,'DistY,'DistZ> =
                          LatentLearner.LearnerFromModel(ThisM)
                      let x = unpackX(din)
                      let y = unpackY(din)
                      //MicrosoftResearch.Infer.Maths.Rand.Restart(12347);
                      let (dw,dz) = learner.TrainPredict(x,y)
                      dzpack dz, dwpack dw
                 *)

               override this.TrainAndPredictWithLogEvidence(din:DTO, ?algo:IAlgorithm, ?numberOfIterations: int) : float*(DistDTO * KnowDTO)=
                  compile(db,algo,numberOfIterations)(din)
                  
                  (*
                      let ThisM = m :> obj :?> Model<unit,'TW,'TX,('TY*'TZ)> 
                      let learner:LatentLearner.ILatentLearner<'DistW,'TX,'TY,'TZ,'DistY,'DistZ> =
                          LatentLearner.LearnerFromModel(ThisM)
                      let x = unpackX(din)
                      let y = unpackY(din)
                      //MicrosoftResearch.Infer.Maths.Rand.Restart(12347);
                      let e,(dw,dz) = learner.TrainPredictWithLogEvidence(x,y)
                      e,(dzpack dz, dwpack dw)
                   *)
                         
               override this.TrainAndPredictWithLEAndFit(din:DTO, display) : float*(DistDTO * KnowDTO * DistDTO )=
                  failwith "NYI"
                  (*
                      let ThisM = m :> obj :?> Model<unit,'TW,'TX,('TY*'TZ)> 
                      let learner:LatentLearner.ILatentLearner<'DistW,'TX,'TY,'TZ,'DistY,'DistZ> =
                          LatentLearner.LearnerFromModel(ThisM)
                      let x = unpackX(din)
                      let y = unpackY(din)
                      // MicrosoftResearch.Infer.Maths.Rand.Restart(12347);
                      let e,(dw,dz, dy) = learner.TrainPredictWithLEAndFit(x,y, display)
                      e,(dzpack dz, dwpack dw, dypack dy)
                   *)

               override this.GetVariableGraph () =  
                 failwith "NYI"              
               override this.ToString():string = 
                 failwith "NYI"
               override this.Test<'TX_>(x:'TX_) : bool = 
                 failwith "NYI"
                     
               override this.TestWithParameter<'TW_,'TX_>(w:'TW_,x:'TX_) : bool = 
                 failwith "NYI"
               override this.PackX<'TX_> (x:'TX_)   =
                 failwith "NYI"
             


 
 let maine argv =
//
//   ignore(INCompiler.dareProgressionEvaluation()) // do this twice to avoid counting jit times.
//   let _ =  INCompiler.dareProgressionEvaluation() 
//   
//   let _ =  INCompiler.testCSoftTrueSkill 100 20000 // works  
//  
//   let _ = INCompiler.testRecommender()
//  
// //  let _ =  INCompiler.testCSoftTrueSkill 10 1000 
//  // let _ =  INCompiler.testCSoftTrueSkillMultiple 10 100000 // works
// //  let _ = INCompiler.test()
//  // let _ = INCompiler.moocProgressionEvaluation()
   let _ = System.Console.ReadLine()
   0


 *)


 [<EntryPoint>]
 let main argv =
 (*
      let _ = Compiler.testTrueSkillWithConjugate 10 20
      
      let _ = Compiler.testTrueSkillWithHyperAndParam 10 20
      
  *)  
      //let _ = Compiler.testCSoftTrueSkill 100 20000
     // ignore(Compiler.dareProgressionEvaluation()) //
      //let _ = Compiler.dareProgressionEvaluation()
     
      //let _ = Compiler.testRecommender()
      
      let _ = System.Console.ReadLine()
     
      0
      