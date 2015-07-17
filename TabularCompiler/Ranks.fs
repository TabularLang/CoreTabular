namespace MicrosoftResearch.Infer.Tabular

// A TypeIndex representation of array types that (hopefully) makes it easy to generate Infer.NET's array declarations.
module Ranks =
    open Syntax
    open MicrosoftResearch.Infer.Models
    open MicrosoftResearch.Infer.Maths
   
  
  
    type IRankVisitor<'Result> =
          abstract CaseBase<'T> : Base<'T> -> 'Result
          abstract CaseArr<'V,'R  when 'V:>Variable
                                  and  'V:> System.ICloneable 
                                  and  'V:> SettableTo<'V>> : Arr<'V,'R> -> 'Result
     
    and [<AbstractClass>]
        Rank() = 
         inherit obj()
         abstract member Visit<'Result> : IRankVisitor<'Result> -> 'Result
         abstract member AddRange: Exp -> Rank
         abstract member ValueRange : Exp option
         abstract member NewAbstractVariable : (int -> Exp -> Range) -> Variable
         abstract member depth: int
  
    and [<AbstractClass>]  
        Rank<'R>() =
         inherit Rank()
         abstract member NewVariable : (int -> Exp -> Range) -> Variable<'R>
         override this.NewAbstractVariable(eToRange) = this.NewVariable(eToRange) :> Variable
   
    and [<AbstractClass>] 
        Rank<'V ,'R when 'V:>Variable
                     and 'V:> System.ICloneable 
                     and 'V:> SettableTo<'V>>() = 
         inherit Rank<'R>()   
        
         override this.NewVariable(eToRange) = this.NewVariableArray(eToRange) :> Variable<'R>      
         abstract member NewVariableArray: (int -> Exp -> Range) -> VariableArray<'V,'R>
         

    and Base<'T>(range:Exp,valueRangeOpt:Exp option) =
         inherit Rank<Variable<'T>,'T[]>()
         member this.Range = range
         override this.ValueRange = valueRangeOpt
         override this.Visit<'Result>(v : IRankVisitor<'Result>)  = v.CaseBase(this)
         
         override this.AddRange(range) =
                  let  arr = Arr(this,range) 
                  arr :> Rank
         override this.NewVariableArray(eToRange:int -> Exp -> Range) =
                 let a = Variable.Array<'T>(eToRange this.depth range) :>  VariableArray<Variable<'T>,'T[]>
                 if valueRangeOpt.IsSome then a.SetValueRange(eToRange 0 (valueRangeOpt.Value))
                 a
         override this.depth = 1

    and Arr<'V,'R when 'V:>Variable
                  and 'V:> System.ICloneable 
                  and 'V:> SettableTo<'V>>(rank:Rank<'V,'R>,range:Exp) =
         inherit Rank<VariableArray<'V,'R>,'R[]>()
         member this.Rank = rank
         member this.Range = range
         override this.ValueRange = rank.ValueRange
         override this.Visit<'Result>(v : IRankVisitor<'Result>)  = v.CaseArr(this)

         override this.AddRange(r) =
                  let arr = Arr(this:>Rank<VariableArray<'V,'R>,'R[]>,r) 
                  arr :> Rank
         override this.NewVariableArray(eToRange) =
                 // Variable.Array<VariableArray<'V,'R>,'R[]>(itemPrototype=r.NewVariableArray(),r=range)
                 let array = rank.NewVariableArray(eToRange)
                 let va = Variable.Array<'V,'R>(array=array,r=eToRange this.depth range)
                 let vr = array.GetValueRange(false)
                 if vr <> null then va.SetValueRange(vr)
                 va
         override this.depth = rank.depth + 1
   
    let test =
        let eToRange n e = match e with (Const (IntConst i)) -> new Range(i) 
        Arr(Base<int>(Const (IntConst 1),None), Const(IntConst 2)).NewVariableArray(eToRange) :> Variable<int[][]>
    
    open System.CodeDom
    
    
    
    let rec TToRank ty E =
       match ty with 
       | T_Int -> Base<int>(E,None) :> Rank
       | T_Bool -> Base<bool>(E,None) :> Rank
       | T_Real -> Base<double>(E,None) :> Rank
       | T_String -> Base<string>(E,None) :> Rank
       | T_Upto E' ->  Base<int>(E,Some E') :> Rank
       | T_Link t ->  Base<int>(E,Some (TypedExp(SizeOf(t),T_Int))) :> Rank
       | T_Vector -> Base<Vector>(E,None) :> Rank
       | T_PositiveDefiniteMatrix ->  Base<PositiveDefiniteMatrix>(E,None) :> Rank
       | T_Record _ -> failwithf "TToRank"
       | T_Array(ty,E') -> (TToRank ty E').AddRange( E)

