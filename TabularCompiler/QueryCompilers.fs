namespace MicrosoftResearch.Infer.Tabular


module QueryCompiler =

    open System.CodeDom
    open System.CodeDom.Compiler

    //open Microsoft.CSharp
    //open Microsoft.VisualBasic



    open Ranks

    open MicrosoftResearch.Infer
    open MicrosoftResearch.Infer.Models
    open MicrosoftResearch.Infer.Distributions
    open MicrosoftResearch.Infer.Collections
    open System.Linq.Expressions
    
    open TypedDTO
   
    open Syntax
    //open Target

    let input = Target.input
    let output = Target.output
    let col = Target.col
    let temp(tn,cn) = Target.col(tn,Target.col(cn,"tmp")) 

    
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns

    let rec private getMethodFromExpr : Expr -> System.Reflection.MethodInfo = function
    | Lambdas (_ , e') -> getMethodFromExpr e'
    | Microsoft.FSharp.Quotations.Patterns.Call(_, m, _) -> m
    | e -> failwith "cannot extract method info from " e
   
    module FArray = Microsoft.FSharp.Collections.Array

    type Helper =  Utilities
    
    type Sampler = 
        static member Bernoulli(bias:real) =
                      Distributions.Bernoulli(bias).Sample()
        static member Beta(alpha,beta) =
                      Distributions.Beta(alpha,beta).Sample()
        static member BetaFromMeanAndVariance(mean,variance) =
                      Distributions.Beta.FromMeanAndVariance(mean,variance).Sample()
        static member GaussianFromMeanAndPrecision(mean,prec) =
                      Distributions.Gaussian.FromMeanAndPrecision(mean,prec).Sample()
        static member GaussianFromMeanAndVariance(mean,var) =
                      Distributions.Gaussian.FromMeanAndVariance(mean,var).Sample()
   
        static member GammaFromShapeAndScale(shape,scale) =
                      Distributions.Gamma.FromShapeAndScale(shape,scale).Sample()
        static member GammaFromShapeAndRate(shape,rate) =
                      Distributions.Gamma.FromShapeAndRate(shape,rate).Sample()
        static member GammaFromMeanAndVariance(mean,var) =
                      Distributions.Gamma.FromMeanAndVariance(mean,var).Sample()             
        static member Binomial(trials,probSuccess) =
                      Distributions.Binomial(trials,probSuccess).Sample() 
        static member VectorGaussianFromMeanAndPrecision(mean:Maths.Vector,prec) =
                      Distributions.VectorGaussian(mean,prec).Sample()
        static member VectorGaussianFromMeanAndVariance(mean:Maths.Vector,var) =
                      Distributions.VectorGaussian.FromMeanAndVariance(mean,var).Sample()
        static member Discrete(count:int,probs:Maths.Vector) =
                      Distributions.Discrete(probs.ToArray()).Sample()
        static member DiscreteUniform(count:int) =
                      Distributions.Discrete.Uniform(count).Sample()
        static member Poisson(mean) =
                      Distributions.Poisson(mean:float).Sample()
        static member Dirichlet(counts:float[]) =
                      Distributions.Dirichlet(counts).Sample()
        static member DirichletUniform(count) =
                      Distributions.Dirichlet.Uniform(count).Sample() 
        static member DirichletSymmetric(count,alpha) =
                      Distributions.Dirichlet.Symmetric(count,alpha).Sample() 
        static member WishartFromShapeAndRate(shape,rate) =
                      Distributions.Wishart.FromShapeAndRate(shape,rate).Sample()
        static member WishartFromShapeAndScale(shape,rate) =
                      Distributions.Wishart.FromShapeAndScale(shape,rate).Sample()

     let rec schemaHasQuery
                    hasQuery
                    tables =
              match tables with
              | [] -> hasQuery
              | (Declaration(Fun tn,table)::tables) ->  schemaHasQuery hasQuery tables
              | (Declaration(Table(tn,_),table)::tables) ->
                let rec tableHasQuery hasQuery columns  =
                  match columns with
                  |  [] -> 
                    schemaHasQuery hasQuery tables
                  | (cn,{Type=T;Markup=m})::rest ->
                    tableHasQuery (hasQuery || Types.det T = Qry) rest
                tableHasQuery hasQuery table
  

      type XT = System.Linq.Expressions.ExpressionType
      
       
      [<AbstractClass>]
      type XExp<'E>(e:'E) = 
           abstract member MakeArray: unit -> XExp<'E> 
           abstract member e: 'E 
      type XExp<'E,'T>(e:'E) =
           inherit XExp<'E>(e)
           override this.MakeArray() = new XExp<'E,'T[]>(e) :> XExp<'E>
           override this.e = e
      

      type Dir = In  | Out | InOut 
        
      [<AbstractClass>]  
         
      type CodeGen<'S,'E,'D,'P,'Result>() =
         abstract member MakeMemberAccess : 'E * System.Reflection.MemberInfo -> 'E
         abstract member ConvertChecked : 'E * System.Type -> 'E
         abstract member CallVirt: System.Reflection.MethodInfo * 'E * 'E[]-> 'E
         abstract member Call: System.Reflection.MethodInfo * 'E[] -> 'E
         abstract member MakeBinary : XT * 'E * 'E -> 'E 
         abstract member MakeUnary : XT * 'E * System.Type -> 'E
         abstract member Constant<'T> : 'T -> 'E 
         abstract member Variable: System.Type * string -> 'D * 'E
         abstract member Condition: 'E * 'E * 'E * System.Type -> 'E
         abstract member Assign : 'E * 'E -> 'S
         abstract member Parameter: Dir * System.Type * string -> 'P * 'E 
         abstract member Block: 'D[] * 'S[] -> 'S
         abstract member ArrayIndex : 'E * 'E -> 'E
         abstract member NewArrayBounds: System.Type * 'E -> 'E
         abstract member NewArrayInit: System.Type * 'E[] -> 'E
         abstract member Loop: 'D * 'E * 'E * 'S * 'S[] -> 'S
         abstract member Let: ('D * 'E) * 'E * 'E -> 'E
         abstract member ForLoop: ('D * 'E) * 'E * 'E * System.Type-> 'E
         abstract member ArrayLength: 'E -> 'E
         abstract member ArrayAccess: 'E * 'E -> 'E
         abstract member DistProperty<'Dist,'T> : 'E * string -> XExp<'E,'T>
         abstract member DistMethod<'Dist,'T> : 'E * string ->  XExp<'E,'T>
         abstract member Finish:'P list * 'D list * 'S list * obj list * (obj[] -> DataBase) -> 'Result         
      
      type L = System.Linq.Expressions.Expression
      type LP = System.Linq.Expressions.ParameterExpression
      type LD =  System.Linq.Expressions.ParameterExpression
      type LS =  System.Linq.Expressions.Expression
      type LinqGen() =
         
         inherit CodeGen<System.Linq.Expressions.Expression,System.Linq.Expressions.Expression,System.Linq.Expressions.ParameterExpression,System.Linq.Expressions.ParameterExpression,DataBase>()
         override this. MakeMemberAccess(e,mr:System.Reflection.MemberInfo) = L.MakeMemberAccess(e,mr) :> L
         override this. ConvertChecked(e,t:System.Type) = L.ConvertChecked(e,t) :> L
         override this. CallVirt(mr:System.Reflection.MethodInfo,e,es) = L.Call(e,mr,es) :> L
         override this. Call(mr:System.Reflection.MethodInfo,es) = L.Call(mr,es)   :> L
         override this. MakeBinary(o,e1,e2) = L.MakeBinary(o,e1,e2) :> L
         override this. MakeUnary(o,e1,t) = L.MakeUnary(o,e1,t)  :> L
         override this. Constant(v) = L.Constant(v) :> L
         override this. Variable(t:System.Type,v) = let p = L.Variable(t,v) in (p,p:>L)  
         override this. Condition(b,e1,e2,t) = L.Condition(b,e1,e2,t)  :> L
         override this. Assign(e1,e2) = L.Assign(e1,e2) :> L
         override this. Parameter(dir,t:System.Type,n) = let t = match dir with 
                                                                 | In -> t 
                                                                 | Out| InOut -> t.MakeByRefType() 
                                                         let p =  L.Parameter(t,n) 
                                                         in (p,p:>L)
         override this. Block(decs,exps) = L.Block(decs,exps) :> L
         override this. ArrayIndex(e1,e2) =  L.ArrayIndex(e1,e2) :> L
         override this. NewArrayBounds(t:System.Type, e) = L.NewArrayBounds(t,e) :> L
         override this. NewArrayInit(t:System.Type, es) = L.NewArrayInit(t,es) :> L
         override this.Loop(init,expr,test,incr,body) = 
            let exit = L.Label(typeof<System.Void>)
            let cont = L.Label(typeof<System.Void>)
            L.Block([| init|],
                    [| L.Assign(init,expr) :> L ;
                       L.Loop(L.IfThenElse(test,
                                           L.Block(L.Block(body),
                                                   incr,
                                                   L.Continue(cont)),
                                           L.Break(exit))
                              ,exit,cont) :> L
                    |]
                 ) :> L
         override this.Let((xv,xr),e1,e2) =
                  L.Block([|xv|],
                          [|L.Assign(xr,e1) :> L;
                            e2
                          |]) :> L
         override this.ForLoop((xd,xv),e1,e2,t2) = 
            let exit = L.Label(typeof<System.Void>)
            let cont = L.Label(typeof<System.Void>)
            let a =  L.Variable(t2.MakeArrayType(),null)
            L.Block([| a;xd |],
                    [| L.Assign(xv,L.Constant(0)) :> L;
                       L.Assign(a,L.NewArrayBounds(t2,[|e1|])) :> L;
                       L.Loop(L.IfThenElse(
                                L.GreaterThanOrEqual(xv,L.ArrayLength(a)),
                                L.Break(exit),
                                L.Block(L.Assign(L.ArrayAccess(a,xv),
                                                 e2),
                                        L.PreIncrementAssign(xv),
                                        L.Continue(cont))
                                ),exit,cont) :> L;
                       a :> L
                    |]
                 ) :> L
         override this. ArrayLength e = L.ArrayLength(e) :> L
         override this. ArrayAccess (e1,e2)=  L.ArrayAccess(e1,e2) :> L
     
         override this. DistProperty<'Dist,'T>(dist,name) =  
               let mr = match (typeof<'Dist>).GetProperty(name,typeof<'T>) with
                        | null -> (typeof<'Dist>).GetField(name) :> System.Reflection.MemberInfo
                        | mr -> mr :> System.Reflection.MemberInfo
               new XExp<L,'T>(this.MakeMemberAccess(this.ConvertChecked(dist,typeof<'Dist>),mr))
          
         override this. DistMethod<'Dist,'T>(dist,name) =  
               let mr = (typeof<'Dist>).GetMethod(name,[||])
               new XExp<L,'T>(this.CallVirt(mr,this.ConvertChecked(dist,typeof<'Dist>),[||]) )

         override this.Finish(pars:LP list,decs:LP list,exps:L list,args:obj list,toDatabase: obj[] -> DataBase) =
                      let lam = L.Lambda(L.Block(decs,exps),pars)
                      let del = lam.Compile()
                      let args = List.toArray(args)
                      let res = del.DynamicInvoke(args)
                      toDatabase args                                                          
                      
         
      type CodeDomGen() =
         inherit CodeGen<CodeStatement,CodeExpression,CodeVariableDeclarationStatement,CodeParameterDeclarationExpression,CodeMemberMethod>()
         let splice e = 
                        use p = new Microsoft.CSharp.CSharpCodeProvider()
                        use tw = new System.IO.StringWriter()
                        p.GenerateCodeFromExpression(e,tw,new CodeGeneratorOptions() )
                        tw.ToString()
        
         let snippet fmt  = Printf.kprintf (fun s ->  new CodeSnippetExpression(s) :> CodeExpression) fmt
        
         let trXT xt = 
             match xt:XT with
             | XT.Equal -> CodeBinaryOperatorType.ValueEquality
             | XT.GreaterThan -> CodeBinaryOperatorType.GreaterThan
             | XT.GreaterThanOrEqual -> CodeBinaryOperatorType.GreaterThanOrEqual
             | XT.LessThan -> CodeBinaryOperatorType.LessThan
             | XT.LessThanOrEqual -> CodeBinaryOperatorType.LessThanOrEqual
             | XT.Add -> CodeBinaryOperatorType.Add
             | XT.Subtract -> CodeBinaryOperatorType.Subtract
             | XT.Multiply -> CodeBinaryOperatorType.Multiply 
             | XT.Divide -> CodeBinaryOperatorType.Divide 
             | XT.Modulo -> CodeBinaryOperatorType.Modulus
             | XT.AndAlso -> CodeBinaryOperatorType.BooleanAnd
             | XT.OrElse -> CodeBinaryOperatorType.BooleanOr
             | _ -> failwithf "trXT %A" xt
         override this. MakeMemberAccess(e,mr:System.Reflection.MemberInfo) = CodeMethodReferenceExpression(e,mr.Name) :> CodeExpression
         override this. ConvertChecked(e,t:System.Type) = CodeCastExpression(t,e) :> CodeExpression
         override this. CallVirt(mr:System.Reflection.MethodInfo,e,es) = CodeMethodInvokeExpression(e,mr.Name,es) :> CodeExpression
         override this. Call(mr:System.Reflection.MethodInfo,e:CodeExpression[]) = CodeMethodInvokeExpression(CodeTypeReferenceExpression(mr.DeclaringType),mr.Name,e)  :> CodeExpression///review
         override this. MakeBinary(o,e1,e2) =
             match o with 
             | XT.ArrayIndex -> CodeIndexerExpression(e1,[|e2|]) :> CodeExpression
             | XT.NotEqual ->  
                      CodeBinaryOperatorExpression(
                       CodeBinaryOperatorExpression(e1,CodeBinaryOperatorType.ValueEquality,e2) :> CodeExpression,
                       CodeBinaryOperatorType.ValueEquality,
                       CodePrimitiveExpression(box false):> CodeExpression)
                        :> CodeExpression
             | o -> CodeBinaryOperatorExpression(e1,trXT o,e2) :> CodeExpression
         override this. MakeUnary(o,e1,t) = 
             match o with 
             | XT.Not ->  CodeBinaryOperatorExpression(e1,CodeBinaryOperatorType.ValueEquality,CodePrimitiveExpression(box false))
                          :> CodeExpression
             | XT.Negate ->  CodeBinaryOperatorExpression(CodePrimitiveExpression(box 0.0),CodeBinaryOperatorType.Subtract,e1)
                             :> CodeExpression
         override this. Constant(v) = CodePrimitiveExpression(box v) :>  CodeExpression
         override this. Variable(t:System.Type,v) = (CodeVariableDeclarationStatement(t,v), 
                                                     CodeVariableReferenceExpression(v) :> CodeExpression)
 
         override this. Condition(b,e1:CodeExpression,e2:CodeExpression,t) =
                        snippet "((%s) ? (%s) : (%s))" (splice b) (splice e1) (splice e2)
                        
         override this. Assign(e1,e2) = CodeAssignStatement(e1,e2) :> CodeStatement
         override this. Parameter(dir,t:System.Type,n) = 
                        let p = CodeParameterDeclarationExpression(t,n) 
                        p.Direction <- match dir with 
                                       | In -> FieldDirection.In
                                       | Out -> FieldDirection.Out 
                                       | InOut -> FieldDirection.Ref          
                        (p,CodeVariableReferenceExpression(n):>CodeExpression) // TODO set direction
         override this. Block(decs,exps:CodeStatement[]) = CodeConditionStatement(CodePrimitiveExpression(box true) :> CodeExpression, // no real blocks in CodeDOm
                                                                                  Array.append (decs:>obj:?>(CodeStatement[])) exps,
                                                                                  ([||]:CodeStatement[])) :> CodeStatement // 
         override this. ArrayIndex(e1,e2) =  CodeIndexerExpression(e1,[|e2|]) :> CodeExpression
         override this. NewArrayBounds(t:System.Type, e:CodeExpression) = CodeArrayCreateExpression(CodeTypeReference(t.MakeArrayType()),e) :> CodeExpression
         override this. NewArrayInit(t:System.Type, es:CodeExpression[]) = CodeArrayCreateExpression(t.MakeArrayType(),es) :> CodeExpression
        
         override this. Let((xv,xr),e1,e2) = 
                        snippet "Utilities.Let(%s , %s => %s)" (splice e1)  (splice xr) (splice e2)
                        
         override this.ForLoop((xd,xr),e1,e2,t2) = 
                        snippet "Utilities.ForLoop(%s,  %s => %s)" (splice e1) (splice xr) (splice e2)
         override this. Loop(init,expr,test,incr,body) = 
                        let init = new CodeVariableDeclarationStatement(init.Type,init.Name,expr)
                        CodeIterationStatement(init,test,incr,body) :> CodeStatement
         override this. ArrayLength e = CodePropertyReferenceExpression(e,"Length") :> CodeExpression
         override this. ArrayAccess (e1,e2)=  CodeIndexerExpression(e1,[|e2|]) :> CodeExpression
         override this. DistProperty<'Dist,'T>(dist,name) =  
               let mr = match (typeof<'Dist>).GetProperty(name,typeof<'T>) with
                        | null -> (typeof<'Dist>).GetField(name) :> System.Reflection.MemberInfo
                        | mr -> mr :> System.Reflection.MemberInfo
               new XExp<CodeExpression,'T>(this.MakeMemberAccess(this.ConvertChecked(dist,typeof<'Dist>),mr))
         override this. DistMethod<'Dist,'T>(dist,name) =  
               let mr = (typeof<'Dist>).GetMethod(name,[||])
               new XExp<CodeExpression,'T>(this.CallVirt(mr,this.ConvertChecked(dist,typeof<'Dist>),[||]) ) 

         
         override this.Finish(pars:CodeParameterDeclarationExpression list,decs:CodeVariableDeclarationStatement list,exps: CodeStatement list,_:obj list,_: obj[] -> DataBase) : CodeMemberMethod =
                      let meth = CodeMemberMethod()
                      do meth.Name <- "Query"
                      for p in pars do 
                         meth.Parameters.Add(p) |> ignore
                      do meth.Attributes <- (MemberAttributes.Public |||  MemberAttributes.Static)
                      do meth.ReturnType <- CodeTypeReference(typeof<System.Void>)
                      for s in decs do 
                         meth.Statements.Add(s) |> ignore
                      for s in exps do 
                         meth.Statements.Add(s) |> ignore
                      meth 

       // the index into the lambda's parameter list, used to extract out arguments
      type index = int
      type mode = MSampler | MQuery

      let compile (X:CodeGen<'S,'E,'D,'P,'R>) =  
          
          let rec xexpOf T e  = 
               match T with
               | T_Upto _ 
               | T_Int -> new XExp<'E,int>(e) :> XExp<'E>
               | T_Bool -> new XExp<'E,bool>(e) :> XExp<'E>
               | T_Real ->  new XExp<'E,double>(e) :> XExp<'E>
               | T_String -> new XExp<'E,string>(e) :> XExp<'E>
               | T_PositiveDefiniteMatrix -> new XExp<'E,Maths.PositiveDefiniteMatrix>(e) :> XExp<'E>
               | T_Vector -> new XExp<'E,Maths.Vector>(e) :> XExp<'E>
               | T_Array(T,_) -> (xexpOf T e).MakeArray() // refactor
               | _ -> failwithf "xexpOfT %A" T

          let rec typeOf T : System.Type = 
               match T with
               | T_Upto _  
               | T_Int -> typeof<int>
               | T_Bool -> typeof<bool>
               | T_Real ->  typeof<double>
               | T_String -> typeof<string>
               | T_PositiveDefiniteMatrix -> typeof<Maths.PositiveDefiniteMatrix>
               | T_Vector -> typeof<Maths.Vector>
               | T_Array(T,_) -> (typeOf T).MakeArrayType()
               | _ -> failwithf "typeOf %A" T

      

         

          let interp_Gt (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.GreaterThan,v.e,w.e)) :> XExp<'E>
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.GreaterThan,v.e,w.e)) :> XExp<'E>
          let inline interp_GtEq (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.GreaterThanOrEqual,v.e,w.e)) :> XExp<'E>
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.GreaterThanOrEqual,v.e,w.e)) :> XExp<'E>

          let inline interp_Lt (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.LessThan,v.e,w.e)) :> XExp<'E>
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.LessThan,v.e,w.e)) :> XExp<'E>
          let inline interp_LtEq (v:XExp<'E>) (w:XExp<'E>) =  
              match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.LessThanOrEqual,v.e,w.e)) :> XExp<'E>
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.LessThanOrEqual,v.e,w.e)) :> XExp<'E>
          let inline interp_Plus (v:XExp<'E>) (w:XExp<'E>) = 
            match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
                new XExp<'E,int>(X.MakeBinary(XT.Add,v.e,w.e)) :> XExp<'E>
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
                new XExp<'E,double>(X.MakeBinary(XT.Add,v.e,w.e)) :> XExp<'E>
          let inline interp_Minus (v:XExp<'E>) (w:XExp<'E>) = 
            match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
                new XExp<'E,int>(X.MakeBinary(XT.Subtract,v.e,w.e)) :> XExp<'E>
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
                new XExp<'E,double>(X.MakeBinary(XT.Subtract,v.e,w.e)) :> XExp<'E>


          let inline interp_Eq (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
               new XExp<'E,bool>(X.MakeBinary(XT.Equal,v.e,w.e)) :> XExp<'E>
              | (:? XExp<'E,string> as v),(:? XExp<'E,string> as w) ->
               new XExp<'E,bool>( X.Call(getMethodFromExpr <@ fun (s1:string)(s2:string)-> System.String.Equals(s1,s2)@>,[|v.e;w.e|])) :> XExp<'E>      
              | (:? XExp<'E,bool> as v),(:? XExp<'E,bool> as w) ->
               new XExp<'E,bool>(X.MakeBinary(XT.Equal,v.e,w.e)) :> XExp<'E>
          let inline interp_Neq (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
               new XExp<'E,bool>(X.MakeBinary(XT.NotEqual,v.e,w.e)) :> XExp<'E>
              | (:? XExp<'E,bool> as v),(:? XExp<'E,bool> as w) ->
               new XExp<'E,bool>(X.MakeBinary(XT.NotEqual,v.e,w.e)) :> XExp<'E>
          let inline interp_And(v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,bool> as v),(:? XExp<'E,bool> as w) ->
                  new XExp<'E,bool>(X.MakeBinary(XT.AndAlso,v.e,w.e)):> XExp<'E>
          let inline interp_Or (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,bool> as v),(:? XExp<'E,bool> as w) ->
                new XExp<'E,bool>(X.MakeBinary(XT.OrElse,v.e,w.e)):> XExp<'E>
          let inline interp_Not (v:XExp<'E>) = 
              match v with
              | (:? XExp<'E,bool> as v)->
                new XExp<'E,bool>(X.MakeUnary(XT.Not,v.e,typeof<bool>)):> XExp<'E>
          let inline interp_Negate (v:XExp<'E>) = 
              match v with
              | (:? XExp<'E,double> as v)->
                 new XExp<'E,double>(X.MakeUnary(XT.Negate,v.e,typeof<double>)):> XExp<'E> 
          let inline interp_Mult (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
                 new XExp<'E,double>(X.MakeBinary(XT.Multiply,v.e,w.e)):> XExp<'E>      
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
                 new XExp<'E,int>(X.MakeBinary(XT.Multiply,v.e,w.e)):> XExp<'E> 

          let inline interp_Max (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
              new XExp<'E,double>( X.Call(getMethodFromExpr <@ fun (d1:double) (d2:double)-> System.Math.Max(d1,d2) @>
                                          ,[|v.e;w.e|])) :> XExp<'E>
          
          let inline interp_Div (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,double> as v),(:? XExp<'E,double> as w) ->
                     new XExp<'E,double>(X.MakeBinary(XT.Divide,v.e,w.e)):> XExp<'E>      
          let inline interp_Mod (v:XExp<'E>) (w:XExp<'E>) = 
              match v,w with
              | (:? XExp<'E,int> as v),(:? XExp<'E,int> as w) ->
               new XExp<'E,int>(X.MakeBinary(XT.Modulo,v.e,w.e)):> XExp<'E>   

          let interp_Logistic (v:XExp<'E>) = 
              match v with
              | (:? XExp<'E,real> as v) ->
                  new XExp<'E,double>( X.Call(getMethodFromExpr <@ fun d -> Maths.MMath.Logistic(d) @>,[|v.e|])) :> XExp<'E>
         
             

  
          let rec interpPrim p es : XExp<'E> = 
                match (p,es) with
                | Prim.Gt,[e1;e2] -> interp_Gt e1 e2
                | Prim.Lt,[e1;e2] -> interp_Lt e1 e2
                | Prim.GtEq,[e1;e2] -> interp_GtEq e1 e2
                | Prim.LtEq,[e1;e2] -> interp_LtEq e1 e2
                | Prim.Eq,[e1;e2]  -> 
                      interp_Eq e1 e2
                | Prim.Neq,[e1;e2]  -> 
                      interp_Neq e1 e2
                | Prim.Mult,[e1;e2]  -> 
                      interp_Mult e1 e2
                | Prim.Div,[e1;e2]  -> 
                      interp_Div e1 e2
                | Prim.Mod,[e1;e2]  -> 
                      interp_Mod e1 e2
                | Prim.Max,[e1;e2]  -> 
                      interp_Max e1 e2
                | Prim.Minus,[e1;e2]  -> 
                      interp_Minus e1 e2
                | Prim.Negate,[e1]  -> 
                      interp_Negate (e1)
                | Prim.Not,[e1]  -> 
                      interp_Not (e1)
                | Prim.Plus,[e1;e2]  -> 
                      interp_Plus e1 e2
                | Prim.Factor(FactorName"DampBackward"),[e1;e2] -> 
                   e1
                | Prim.Factor(FactorName"Logistic"),[e1] -> 
                   interp_Logistic e1
                | Prim.Factor(FactorName"Probit"),[e1;e2] ->  
                   failwithf "random Probit use at query level"
                | Prim.Factor(FactorName"Sum"),[e1] ->  
                   match (e1) with
                   |  (:? XExp<'E,double[]> as v1) ->
                       new XExp<'E,double>( X.Call(typeof<Helper>.GetMethod("Sum"),[|v1.e|]))
                        :> XExp<'E>
                | Prim.Factor(FactorName "Softmax"),[e1] ->  
                   match (e1) with
                   |  (:? XExp<'E,double[]> as v1) ->
                       new XExp<'E,Maths.Vector>( X.Call(getMethodFromExpr <@ fun l -> Maths.MMath.Softmax(l) @>,[|X.ConvertChecked(v1.e,typeof<System.Collections.Generic.IList<double>>)|])) :> XExp<'E>
                | Prim.Factor(FactorName"InnerProduct"),[e1;e2] ->  
                   match (e1,e2) with
                   |  (:? XExp<'E,Maths.Vector> as v1),
                      (:? XExp<'E,Maths.Vector> as v2) ->
                      new XExp<'E,double>(
                         X.Call(typeof<Maths.Vector>.GetMethod("InnerProduct"),[|v1.e;v2.e|]))
                      :> XExp<'E>
                | Prim.Factor(FactorName"VectorFromArray"),[e1] ->  
                   match e1 with
                   |  (:? XExp<'E,double[]> as v1) ->
                         new XExp<'E,Maths.Vector>(
                          X.Call(typeof<Maths.Vector>.GetMethod("FromArray",[|typeof<double[]>|]),[|v1.e|]))
                        :> XExp<'E>
                | Prim.Factor(FactorName"Exp"),[e1] ->  
                   match e1 with
                   |  (:? XExp<'E,double> as v1) ->
                        new XExp<'E,double>(
                          X.Call(typeof<System.Math>.GetMethod("Exp",[|typeof<double>|]),[|v1.e|]))
                        :> XExp<'E>
                | Prim.Factor(FactorName"Log"),[e1] ->  
                   match e1 with
                   |  (:? XExp<'E,double> as v1) ->
                        new XExp<'E,double>(
                          X.Call(typeof<System.Math>.GetMethod("Log",[|typeof<double>|]),[|v1.e|]))
                        :> XExp<'E>
                | Prim.Factor(FactorName "DiagonalPDMatrix"),[e1] ->  
                   match e1 with
                   |  (:? XExp<'E,real[]> as v1)   ->
                    new XExp<'E,Maths.PositiveDefiniteMatrix>(
                         X.Call(typeof<Helper>.GetMethod("DiagonalPDMatrix",[|typeof<real[]>|]),[|v1.e|])):> XExp<'E>
                
                | Prim.Factor(FactorName "IdentityScaledBy"),[e1;e2]->  
                    match (e1,e2) with
                    |  (:? XExp<'E,int> as v1),
                       (:? XExp<'E,double> as v2) ->
                       new XExp<'E,Maths.PositiveDefiniteMatrix>(
                          X.Call(typeof<Maths.PositiveDefiniteMatrix>.GetMethod("IdentityScaledBy",[|typeof<int>;typeof<double>|]),[|v1.e;v2.e|]))
                        :> XExp<'E>
            
                (*
                | Prim.Factor(FactorName "GetItems"),[e1;e2]->  // I doubt this will work...
                    let v = e1
                    let w = e2
                    let m = typeof<Variable>.GetGenericMethod("GetItems", [|  v.GetType().GetGenericArguments().[0]|])
                    let _ =  m.Invoke(v, [| i; w |])
                    ()
                | Prim.Factor(FactorName "SubArray"),[e1;e2]->  // I doubt this will work...
                    let v = e1
                    let w = e2
                    let m = typeof<Variable>.GetGenericMethod("GetItems", [|  v.GetType().GetGenericArguments().[0]|])
                    let _ =  m.Invoke(v, [| i; w |])
                    ()
                *)
 
                | Prim.Factor(FactorName "ArgMax"),[e1] ->
                  match (e1) with
                   |  (:? XExp<'E,double[]> as v1) ->
                        new XExp<'E,int>( X.Call(typeof<Helper>.GetMethod("ArgMax"),[|v1.e|]))
                        :> XExp<'E>
                | Prim.Factor(FactorName "ArgMin"),[e1] ->
                  match (e1) with
                   |  (:? XExp<'E,double[]> as v1) ->
                       new XExp<'E,int>( X.Call(typeof<Helper>.GetMethod("ArgMin"),[|v1.e|]))
                       :> XExp<'E>
                | Prim.Factor(FactorName "BreakSymmetry"),[e1] -> //erase
                    e1
                | Prim.Factor(FactorName "#Max"),[e1] ->
                   match (e1) with
                   |  (:? XExp<'E,double[]> as v1) ->
                       new XExp<'E,double>( X.Call(typeof<Helper>.GetMethod("HashMax"),[|v1.e|]))
                       :> XExp<'E>
                | Prim.Factor(FactorName "#Sqrt"),[e1] ->
                   match e1 with
                   |  (:? XExp<'E,double> as v1) ->
                        new XExp<'E,double>(
                          X.Call(typeof<System.Math>.GetMethod("Sqrt",[|typeof<double>|]),[|v1.e|]))
                        :> XExp<'E>
                | Prim.Factor(FactorName "#Round"),[e1] ->
                  match e1 with
                   |  (:? XExp<'E,double> as v1) ->
                        new XExp<'E,double>(
                          X.Call(typeof<System.Math>.GetMethod("Round",[|typeof<double>|]),[|v1.e|]))
                        :> XExp<'E>                  
                | Prim.Factor(FactorName f),es -> failwithf "interpPrim %A not yet implemented" f

          let tt = new XExp<'E,bool>(X.Constant(true)) :> XExp<'E>
          let ff = new XExp<'E,bool>(X.Constant(false)) :> XExp<'E>

         

          let rec
           trE mode (TE:Map<TableName,(XExp<'E>*Map<ColumnName, Syntax.B * Syntax.D * XExp<'E> * index>)>) (CE:Map<ColumnName, Syntax.B * Syntax.D * XExp<'E> * index>) (i:XExp<'E>) et  : XExp<'E> = 
              let trE = trE mode
              let (TypedExp(e,t)) = et 
              match e with 
              | Var x -> match CE.[x] with
                         | (H,_,v,_) -> v
                         | (W,_,v,_) -> v
                         | (Y,D.R,vs,_) when mode = MQuery -> 
                          new XExp<'E,obj>( X.ArrayIndex(vs.e,i.e)) :> XExp<'E>
                         | (Y,_,vs,_) -> 
                            xexpOf t (X.ArrayIndex(vs.e,i.e)) 
              | Const (IntConst v) -> new XExp<'E,int>( X.Constant(v)) :> XExp<'E>
              | Const (BoolConst v) ->new XExp<'E,bool>( X.Constant(v)) :> XExp<'E>
              | Const (RealConst v) -> new XExp<'E,double>( X.Constant(v)) :> XExp<'E>
              | Const (StringConst v) -> new XExp<'E,string>( X.Constant(v)) :> XExp<'E>
              | SizeOf(tn) -> fst TE.[tn] //size
              | Array es -> 
                   xexpOf t (X.NewArrayInit((typeOf t).GetElementType(), [| for e in es -> (trE TE CE i e).e |]))
            
              | Prim(Prim.And,[e1;e2])  -> 
                     interp_And (trE TE CE i e1) (trE TE CE i e2)
              | Prim(Prim.Or,[e1;e2])  -> 
                     interp_Or (trE TE CE i e1) (trE TE CE i e2)
              | Prim(p,es) -> interpPrim p (List.map (trE TE CE i) es)
              | Dist(d,es) -> 
                 let sampler = typeof<Sampler>.GetMethod(sprintf "%A" d)
                 assert not (sampler = null)
                 xexpOf t ( X.Call(sampler,[| for e in es -> (trE TE CE i e).e |]))
              | Let(x,(TypedExp(_,t1) as e1),e2) ->
                 let v1 = trE TE CE i e1 in
                 let (xv,xr) = X.Variable(typeOf t1, x)
                 let v2 = trE TE (CE.Add(x,(W,D,xexpOf t1 xr,-1))) i e2
                 xexpOf t (X.Let((xv,xr),v1.e,v2.e))
                 
              | DeRef(e,tn,cn) -> 
                 let v = trE TE CE i e in
                 match v, (snd(TE.[tn])).[cn] with
                 | :? XExp<'E,int> as k, (_,_,a,_) ->
                     xexpOf t (X.ArrayIndex(a.e,k.e))
              | Ref(tn,cn) -> 
                 match (snd(TE.[tn])).[cn] with
                 | (_,_,a,_) ->
                   a
              | If (e1,e2,e3) ->
                 let v1 = trE TE CE i e1 in
                 let v2 = trE TE CE i e2 in
                 let v3 = trE TE CE i e3 in
                 match v1 with
                 | :? XExp<'E,bool> as b ->
                   xexpOf t (X.Condition(v1.e,v2.e,v3.e,typeOf t))
              | ForLoop(x,e1,e2) ->
                 let (TypedExp(_,t2)) = e2
                 let v1 = trE TE CE i e1 
                 match v1 with
                 | :? XExp<'E,int> as b ->
                  let (xv,xr) = X.Variable(typeof<int>,x)
                  let v2 = trE TE  (CE.Add(x,(W,D,new XExp<'E,int>(xr):>XExp<'E>,-1))) i e2
                  xexpOf t (X.ForLoop((xv,xr),v1.e,v2.e,typeOf t2))
              | Subscript(e1,e2) ->
                 let v1 = trE TE CE i e1 in
                 let v2 = trE TE CE i e2 in
                 match v1 with
                 | :? XExp<'E,obj> -> // better be a distribution array
                    new XExp<'E,obj>( X.Call(typeof<Helper>.GetMethod("IndexDist"),[|v1.e;v2.e|])) :> XExp<'E>
                 | _ -> xexpOf t (X.MakeBinary(XT.ArrayIndex,v1.e,v2.e))
              | Constraint(e1,t1) ->
                 trE TE CE i e1 
              | Infer((Beta|BetaFromMeanAndVariance),[],("alpha"|"trueCount"),e1) ->
                 let v1 = trE TE CE i e1 
                 let b = Distributions.Beta()
                 X.DistProperty<Distributions.Beta,double>(v1.e,"TrueCount") :> XExp<'E>
              | Infer((Beta|BetaFromMeanAndVariance),[],("beta"|"falseCount"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistProperty<Distributions.Beta,double>(v1.e,"FalseCount") :> XExp<'E>
              | Infer((Beta|BetaFromMeanAndVariance),[],("Mean"|"mean"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistMethod<CanGetMean<double>,double>(v1.e,"GetMean") :> XExp<'E>
              | Infer((Beta|BetaFromMeanAndVariance),[],("Variance"|"variance"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistMethod<CanGetVariance<double>,double>(v1.e,"GetVariance") :> XExp<'E>
              | Infer(Bernoulli,[],("Bias"|"bias"|"Mean"|"mean"|"probTrue"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistMethod<CanGetMean<double>,double>(v1.e,"GetMean") :> XExp<'E>
              | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision),[],("Mean"|"mean"),e1) ->
                 let v1 = trE TE CE i e1
                 X.DistMethod<CanGetMean<double>,double>(v1.e,"GetMean") :> XExp<'E>
              | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision),[],("Variance"|"variance"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistMethod<CanGetVariance<double>,double>(v1.e,"GetVariance") :> XExp<'E>
              | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision),[],("Precision"|"precision"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistProperty<Distributions.Gaussian,double>(v1.e,"Precision") :> XExp<'E> 
              | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Mean"|"mean"),e1) ->
                 let v1 = trE TE CE i e1
                 X.DistMethod<CanGetMean<double>,double>(v1.e,"GetMean") :> XExp<'E>
              | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Variance"|"variance"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistMethod<CanGetVariance<double>,double>(v1.e,"GetVariance") :> XExp<'E>
              | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Rate"|"rate"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistProperty<Distributions.Gamma,double>(v1.e,"Rate") :> XExp<'E> 
              | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Shape"|"shape"),e1) ->
                 let v1 = trE TE CE i e1
                 X.DistProperty<Distributions.Gamma,double>(v1.e,"Shape") :> XExp<'E> 
              | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Scale"|"scale"),e1) ->
                 let v1 = trE TE CE i e1
                 X.DistMethod<Distributions.Gamma,double>(v1.e,"GetScale") :> XExp<'E> 
              | Infer((Dist.Dirichlet|Dist.DirichletSymmetric|DirichletUniform),[e0],("counts"|"Counts"),e1) -> 
                 let v1 = trE TE CE i e1 
                 new XExp<'E,double[]>( X.Call(typeof<Helper>.GetMethod("DirichletCounts"),[|v1.e|])) :> XExp<'E>
              | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Probs"|"probs"),e1) -> 
                 let v1 = trE TE CE i e1 
                 new XExp<'E,double[]>( X.Call(typeof<Helper>.GetMethod("DiscreteProbs"),[|v1.e|])) :> XExp<'E>
              | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Mode"|"mode"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistMethod<CanGetMode<int>,int>(v1.e,"GetMode") :> XExp<'E> //NB: not a constructor argument
              | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Median"|"median"),e1) ->
                 let v1 = trE TE CE i e1 
                 X.DistMethod<Distributions.Discrete,int>(v1.e,"GetMedian") :> XExp<'E> 
              | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Mean"|"mean"),e1) ->
                 let v1 = trE TE CE i e1
                 X.DistProperty<Distributions.Discrete,double[]>(v1.e,"GetMean") :> XExp<'E>  
              | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision|GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("StdDeviation"),e1) ->
                 let v1 = trE TE CE i e1
                 new XExp<'E,double>(X.Call(typeof<Helper>.GetMethod("StdDeviation"),[|v1.e|])) :> XExp<'E>
        

          let trModel mode TE CE i m = 
              match m with
              | TypedModel(MExp e,_) ->
                trE mode TE CE i e
              | _ -> failwith "non-core model"

       
          let rec trTables2 mode (db:DataBase) 
                    pars decs exps args
                    ((TE,LTE): Map<TableName,(XExp<'E> (* size *) *Map<ColumnName, Syntax.B * Syntax.D * XExp<'E> * index >)> * List<TableName * List<(ColumnName * (obj-> ColValue )) >>)
                    tables  = 
              let id2Rep = db |> Map.map(fun k (_,_,rep,keyToPos) -> rep) // better: build incrementally as we traverse tables
              let id2KeyToPos = db |> Map.map(fun k (_,_,_,keyToPos) -> keyToPos) // better: build incrementally as we traverse tables
              let trTables =  trTables2 mode (db:DataBase) 
             
              match tables with
              | [] -> 
                      let LTE = List.rev LTE
                      let pars = List.rev pars
                      let decs = List.rev decs
                      let exps = List.rev exps
                      let args = List.rev args
                      let toDataBase(args:obj[]) = LTE |> List.map (fun (tn, cl) ->  
                                                                   let (size_tn,colmap,idRep, keyToPos) = db.[tn]
                                                                   let colValues = cl |> List.map  (fun (cn, f) -> let (_,_,_,index) =  (snd TE.[tn]).[cn] in cn, f(args.[index]) )  
                                                                                      |> Map.ofList
                                                                   tn, (size_tn, colValues,idRep, keyToPos)
                                                                   ) 
                                                        |> Map.ofList
                           
                      X.Finish(pars,decs,exps,args,toDataBase)
                      
              | (Declaration(Table(tn,_),table)::tables) -> 
                let (length, colmap,idRep,idToPos) = db.[tn]
                let (sizev,sizer) = X.Parameter(In,typeof<int>,Target.input(tn,"size"))
                let pars = sizev::pars
                let args = (box length)::args
                let rec trColumns pars decs exps args (CE,LCE) columns  =
                  match columns with
                  |  [] -> 
                    trTables pars decs exps args (TE.Add(tn,(new XExp<'E,int>(sizer):>XExp<'E>,CE)), (tn,List.rev LCE)::LTE) tables
                  | (cn,{Type=T;Markup=m})::rest ->
                    if mode = MSampler && Types.det T = Qry 
                    then trColumns pars decs exps args (CE,LCE) rest
                    else
                    match m with
                    | Hyper e ->
                       let (pv,pr) = X.Variable(typeOf T,cn)
                       let exp = trE mode TE CE (XExp<'E,int>(X.Constant(-1)):>XExp<'E>) e
                       let CE' = CE.Add(cn,(H,D,xexpOf T pr,-1))
                       let LCE' = LCE
                       let decs = pv::decs
                       let exps = (X.Assign(pr,exp.e))::exps
                       trColumns pars decs exps args  (CE',LCE') rest

                    | Param m when Types.det T = R && mode=MQuery->
                      let (pv,pr) = X.Parameter(In,typeof<obj>,input(tn,cn))
                      let colValue = colmap.[cn] :?> Static
                      let v = colValue.Value()
                      let CE' = CE.Add(cn,(W,R,new XExp<'E,obj>(pr):>XExp<'E>,pars.Length)) //TBR
                      let LCE' = (cn, fun _ ->  colValue :> ColValue)::LCE
                      let args = v::args
                      let pars = pv::pars
                      trColumns pars decs exps args  (CE',LCE') rest

                    | Param m when Types.det T <> R || mode = MSampler -> 
                      
                      let (av,ar) = X.Variable(typeOf T,temp(tn,cn))
                      let (pv,pr) = X.Parameter(Out,typeOf T,output(tn,cn))
                      let exp = X.Block([||],[| X.Assign(ar,(trModel mode TE CE (XExp<'E,int>(X.Constant(-1)):>XExp<'E>) m).e)
                                                X.Assign(pr,ar) |])
                      let CE' = CE.Add(cn,(W,Types.det T,xexpOf T ar,List.length pars))
                      let rep = TypedDTO.rep (id2Rep, id2KeyToPos) T //TBR
                      let mkStatic (v:obj) : Static  =
                           rep.Open {new RepOpen<Static> with member this.Case<'T>(r:Rep<'T>) = (TypedDTO.Static<'T> (v:?>'T)):> TypedDTO.Static }
                      let LCE' = ( (cn, fun v -> mkStatic(v) :> ColValue))::LCE 
                      trColumns (pv::pars)  (av::decs) ((exp : 'S)::exps) ((null:>obj)::args)  (CE',LCE') rest

                    | Input ->
                      let inputValue = colmap.[cn] :?> TypedDTO.Instance
                      let vs = inputValue.get_NonNullValues
                      let (pv,pr) = X.Parameter(In,(typeOf T).MakeArrayType(),input(tn,cn))
                      let CE' = CE.Add(cn,(Y,D,(xexpOf T pr).MakeArray(),pars.Length))
                      let LCE' = ( (cn, fun _ -> inputValue :> ColValue))::LCE
                      trColumns (pv::pars) decs exps (vs:>obj::args) (CE',LCE') rest

                    | Observable m 
                    | Latent m when Types.det T = R && mode = MQuery->  
                      let vs = let a = colmap.[cn] :?> TypedDTO.Instance
                               [| for v in a.get_NonNullValues -> box v |]

                      let (av,ar) = X.Variable(typeof<obj[]>,temp(tn,cn))
                      let (pv,pr) =  X.Parameter(In,typeof<obj[]>,output(tn,cn))
                      let exp =  X.Assign(ar,pr) 
                      let CE' = CE.Add(cn,(Y,R,new XExp<'E,obj[]>(ar):>XExp<'E>,pars.Length))
                      let LCE' = ((cn, fun v -> let oa = box v :?> obj[]
                                                DistributionInstance<obj> (oa) :> Instance :> ColValue))::LCE
                      trColumns (pv::pars) (av::decs) (exp::exps) (vs:>obj::args)  (CE',LCE') rest

                    | Observable m 
                    | Latent m when Types.det T <> R || mode = MSampler -> 
                      let Ts = (typeOf T).MakeArrayType()
                      let (pv,pr) =  X.Parameter(Out,Ts,output(tn,cn))
                      let (av,ar) = X.Variable(Ts,temp(tn,cn))
                      let (xv,xr) = X.Variable(typeof<int>,cn+"_i")
                      let exp : 'S =
                         X.Block(
                                 [| |],
                                 [| X.Assign(ar,X.NewArrayBounds(typeOf T,sizer));
                                    X.Loop(xv,//assume initially zero
                                           X.Constant(0),
                                           X.MakeBinary(XT.LessThan,xr,X.ArrayLength(ar)),
                                           X.Assign(xr,X.MakeBinary(XT.Add,xr,X.Constant(box 1))),
                                           [|X.Assign(X.ArrayAccess(ar,xr),(trModel mode TE CE (new XExp<'E,int>(xr)) m).e)|]);
                                    X.Assign(pr,ar)|]
                         )
                      let CE' = CE.Add(cn,(Y,Types.det T,(xexpOf T ar).MakeArray(),pars.Length))
                      let rep = TypedDTO.rep (id2Rep, id2KeyToPos) T //TBR
                      let mkNonNullableInstance (vs:obj) : Instance  =
                           rep.Open {new RepOpen<Instance> with member this.Case<'T>(r:Rep<'T>) = NonNullableInstance<'T> (vs :?> 'T[] ):> Instance }
                      let LCE' = (cn, fun vs -> mkNonNullableInstance  vs:> ColValue)::LCE
                      trColumns (pv::pars) (av::decs) (exp::exps) ((null:>obj)::args)  (CE',LCE') rest
                trColumns pars decs exps args (Map.empty,[]) table
            
          let compile mode db tables = 
                    trTables2  mode db [] [] [] [] (Map.empty,[]) tables
          compile

       let Sample = compile (new LinqGen()) MSampler
       let Query = compile (new LinqGen()) MQuery
       let ExtractQuery = compile (new CodeDomGen()) MQuery



