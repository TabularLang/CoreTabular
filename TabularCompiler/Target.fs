 namespace MicrosoftResearch.Infer.Tabular
 
      
 module Target =

  module Tabular = Syntax

  type r = string
  type v = string
  
  type T = Tabular.ColumnType

  type C = Tabular.Constant
  
  type E =
        | Var of v
        | Rng of r
        | IndexRng of E * r
        | Const of C
        | Prim of (Tabular.Prim * E list) //TODO: restrict to v list
        | Dist of (Tabular.Dist * E list)
        | Index of E * E // array indexing
        // used for symmetry breaking
        | InitialiseTo of E * obj // obj must be a distribution 

  type S = 
       | CloneRng of r * r
       | LetRng of r * v
       | LetNew of v * T
       | LetVar of v * E
       | LetArray of v  * r * T  
       | ForEach of r * S
       | ForLoop of r * v * S
       | IfNot of v * S
       | If of v * S 
       | SetTo of v * E
       | AssignIndex of v * E * E
       | Assign of v * r * E
       | Seq of S * S
       | ObserveValue of v * T * obj
       | Skip
       | Switch of v * S
       | SetValueRange of v * r
       | LetCopy of v * E

 

  let size(tn) = tn+"_"+"size"
  let col(tn:string,cn) = tn+"_"+cn
  
  let input(tn,cn) = col("in",col(tn,cn))
  let output(tn,cn) = col("out",col(tn,cn))  
  
  let subarraysize(tn:string,cn) = tn+"_"+cn+"_"+"size"
  let subarrayrange(tn:string,cn) = tn+"_"+cn+"_"+"range"
  let subarrayindices(tn:string,cn) = tn+"_"+cn+"_"+"indices"
  let subarray(tn:string,cn) = tn+"_"+cn+"_"+"subarray"
  let colfield(tn:string,cn,fld) = tn+"_"+cn+"_"+fld
  let range(tn:string) = tn+"_range"
  let mutable i = 0
  let fresh() = 
        i <- i +  1 
        "v"+i.ToString()
          
   