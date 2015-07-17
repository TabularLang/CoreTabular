namespace MicrosoftResearch.Infer.Tabular
 
 open System.Threading

 open MicrosoftResearch.Infer

 [<AbstractClass>]
 type LatentModel() = 
         abstract member TrainAndPredictWithLogEvidence: DTO  * IAlgorithm option * int option * CancellationToken option -> Syntax.Schema * float*( DistDTO * KnowDTO)
         member this.performInferenceGeneric (DTO dicDatas, algo, numberOfIterations, cts) =
            async {
              let! (schema, le, predictedPZ, knowledgeDW) = async {
                                                             let ctx = SynchronizationContext.Current
                                                             do! Async.SwitchToThreadPool()
                                                             let! tok= Async.StartChild(async { let schema,le,(predictedPZ, knowledgeDW) = this.TrainAndPredictWithLogEvidence(DTO dicDatas,algo, numberOfIterations, cts)
                                                                                                return schema, le,(predictedPZ, knowledgeDW) })
                                                             let! schema, le,(predictedPZ, knowledgeDW) = tok
                                                             do! Async.SwitchToContext(ctx)
                                                             return schema, le, predictedPZ, knowledgeDW
                                                           }
              return schema,le, None, predictedPZ, knowledgeDW, Option<DistDTO>.None
            }
