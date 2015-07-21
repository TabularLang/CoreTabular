namespace MicrosoftResearch.Infer.Tabular

open System.CodeDom
open System.CodeDom.Compiler

open Microsoft.CSharp
//open Microsoft.VisualBasic
open System.IO

open MicrosoftResearch.Infer.Models
open MicrosoftResearch.Infer.Maths
open MicrosoftResearch.Infer.Distributions

open Ranks

// bare-bones Tabular to API file construction
// based on CodeDom, despite its limitations, this means we have no new dependencies.
// works for TrueSkill,DARE,faithfull, toy skype reports
//            refine types of inferred variables (the out parameters are all object for now - unfortunately out IDist<T>[] requires non-free conversion, make it optin using a typed wrapper?)
//            support partial observations with reference input types (nullable value types already work) 
//            support symmetry breaking (need to make explicit in IR)
//            consider introducing higher level view of data
//            inline atomic expressions for shorter code
//            find better workaround for missing using/blocks
//            what to do about query level?
//            what about non-numeric keys? 
//            we currently rely on C# specific snippets - once removed, the codedom code will hopefully be portable to say, VB (which Excel users might  actually prefer...)

module Extraction =
  
  open Syntax
  open Target
   
 // open CodeDomQueryCompiler.QueryCompiler

  let imports = [ "MicrosoftResearch.Infer"; "MicrosoftResearch.Infer.Models"; "MicrosoftResearch.Infer.Maths"; "MicrosoftResearch.Infer.Distributions"; "MicrosoftResearch.Infer.Collections" ] 
   
  
   
  let CodeTypeReferenceExpression(t:System.Type) = if List.exists (fun s-> t.Namespace = s) imports then CodeTypeReferenceExpression(t.Name) else CodeTypeReferenceExpression(t:System.Type)
  let CodeTypeReference(t:System.Type) = if List.exists (fun s-> t.Namespace = s) imports then CodeTypeReference(t.Name) else CodeTypeReference(t:System.Type)

  let rec tyToCode  ty = 
        match ty with
        | T_Int -> CodeTypeReference(typeof<int>)
        | T_Real -> CodeTypeReference(typeof<double>)
        | T_Bool ->   CodeTypeReference(typeof<bool>)
        | T_String ->  CodeTypeReference(typeof<string>)
        | T_Array (ty,e) -> System.CodeDom.CodeTypeReference(tyToCode ty,1)
        | T_Upto e -> CodeTypeReference(typeof<int>)
        | T_Link _ -> CodeTypeReference(typeof<int>)
        | T_Vector -> CodeTypeReference(typeof<MicrosoftResearch.Infer.Maths.Vector>)
        | T_PositiveDefiniteMatrix -> CodeTypeReference(typeof<MicrosoftResearch.Infer.Maths.PositiveDefiniteMatrix>)
        | t -> System.CodeDom.CodeTypeReference(sprintf "??%A??" t)

  let rec objToString ty (obj:obj) = 
        match ty with
        | T_Int 
        | T_Real 
        | T_Bool 
        | T_String
        | T_Upto _
        | T_Link _ 
        | T_Vector 
        | T_PositiveDefiniteMatrix -> obj.ToString() 
        | T_Array (ty,e) -> sprintf "new %O []{%O}" (tyToCode ty) (System.String.Join(",", [| for o in (obj :?> System.Array) -> objToString ty  o |]))
        | t -> sprintf "??%A??" t
  
  let rec valueRange t = 
      match t with
      | T_Upto E -> Some E
      | T_Vector -> None //case added to detect future change in T_Vector
      | T_Array(t,_) -> valueRange t
      | _ -> None


  let rec codeTypeReference (ty:System.Type) =
          if ty.IsGenericType then
             let code = CodeTypeReference(ty.GetGenericTypeDefinition())
             code.TypeArguments.AddRange([|for ty in ty.GetGenericArguments() -> codeTypeReference ty|])
             code
          else CodeTypeReference(ty)

  
  let rec rankToCodeDom classExp (eToRange:int -> Exp -> CodeExpression) (r:Rank) = 
            r.Visit 
             { new IRankVisitor<CodeExpression> with
                 member this.CaseBase<'T>(b:Base<'T>) = 
                        let rng = eToRange b.depth (b.Range) 
                        let ty = CodeTypeReference(typeof<'T>)
                        //let var = CodeTypeReference("var")
                        let array = CodeMethodInvokeExpression(
                                     CodeMethodReferenceExpression(CodeTypeReferenceExpression(typeof<MicrosoftResearch.Infer.Models.Variable>), "Array",[| ty|]),
                                                                   [|rng|]) :> CodeExpression
                        let array = match b.ValueRange with
                                    | Some E -> let valueRange = eToRange 0 E 
                                                CodeMethodInvokeExpression(CodeMethodReferenceExpression(classExp,"SetValueRange"),  [|array;valueRange|]) 
                                                :> CodeExpression
                                    | None -> array
                        array 
                         
                 member this.CaseArr<'V,'R when 'V:> Variable
                                           and  'V:> System.ICloneable 
                                           and  'V:> SettableTo<'V>>(a:Arr<'V,'R>) = 
                        let rng = eToRange a.depth (a.Range)
                        let array = rankToCodeDom classExp eToRange a.Rank
                        let V = codeTypeReference(typeof<'V>)
                        //V.TypeArguments.AddRange([|for ty in typeof<'V>.GetGenericArguments() ->  CodeTypeReference(ty)|])
                        let R = codeTypeReference(typeof<'R>)
                        let array = CodeMethodInvokeExpression(
                                     CodeMethodReferenceExpression(CodeTypeReferenceExpression(typeof<MicrosoftResearch.Infer.Models.Variable>), "Array",[| V;R|]),
                                                                   [|array;rng|]) :> CodeExpression
                        let array = match a.Rank.ValueRange with
                                    | Some E -> let valueRange = eToRange 0 E 
                                                CodeMethodInvokeExpression(CodeMethodReferenceExpression(classExp,"SetValueRange"),  [|array;valueRange|]) 
                                                :> CodeExpression
                                    | None -> array
                        array 
             }  

  let rec variableNew (classExp:CodeExpression) (eToRange:int -> Exp -> CodeExpression)  t =                                                      
          match t with
          | T_Array (u,E) ->
             rankToCodeDom classExp eToRange (TToRank u E)
          | t ->
              let tc =  tyToCode t 
              let ec = CodeMethodInvokeExpression(
                               CodeMethodReferenceExpression(CodeTypeReferenceExpression(typeof<MicrosoftResearch.Infer.Models.Variable>), "New",[| tc|]),
                                                      [||]) :> CodeExpression
              let ec = match valueRange t with 
                       | Some E ->
                          let valueRange = eToRange 0 E
                          CodeMethodInvokeExpression(CodeMethodReferenceExpression(classExp,"SetValueRange"),  [|ec;valueRange|]) 
                                                :> CodeExpression  
                       | None -> ec   
              ec

  let constant v  = 
        CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<MicrosoftResearch.Infer.Models.Variable>), "Constant",[|v|])  :> CodeExpression

  let CodeSetObservedValue e1 e2  = CodeAssignStatement(CodePropertyReferenceExpression(e1,"ObservedValue"),e2) :> CodeStatement
  let CodeSetNumberOfIterations e1 e2  = CodeAssignStatement(CodePropertyReferenceExpression(e1,"NumberOfIterations"),e2) :> CodeStatement
  let CodeSetOptimizeForVariables e1 vs = CodeAssignStatement(CodePropertyReferenceExpression(e1,"OptimiseForVariables"),
                                                               CodeArrayCreateExpression(CodeTypeReference(typeof<MicrosoftResearch.Infer.Models.IVariable>), [|for v in vs -> v:>CodeExpression|])):> CodeStatement
  let CodeVariableDot op es = CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<MicrosoftResearch.Infer.Models.Variable>), op, es) :> CodeExpression

  let rec namedOp op es = 
      CodeVariableDot op ([|for e in es -> EToCode e|]) 
  
  and primToCode p es : CodeExpression = 
        let dampBackward e1 e2 = CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<Variable<double>>), "Factor<double,double>",[|CodeSnippetExpression("MicrosoftResearch.Infer.Factors.Damp.Backward"):>CodeExpression;EToCode e1; EToCode e2|]) :> CodeExpression
        let binOp (op:CodeBinaryOperatorType) es = 
            match es with 
            | [e1;e2] -> CodeBinaryOperatorExpression( EToCode e1, op,EToCode e2) :> CodeExpression
            | _ -> failwith "binOp: expecting exactly two arguments"
        let interp = EToCode  
        //TBC with missing primitives
        match (p,es) with
        | Prim.Gt, _ -> binOp CodeBinaryOperatorType.GreaterThan es
        | Prim.Lt, _ -> binOp CodeBinaryOperatorType.LessThan es
        | Prim.GtEq, _ -> binOp CodeBinaryOperatorType.GreaterThanOrEqual es
        | Prim.LtEq, _ -> binOp CodeBinaryOperatorType.LessThanOrEqual es
        | Prim.Eq, _ -> binOp CodeBinaryOperatorType.ValueEquality es
        | Prim.Neq,[e1;e2]  -> // no unary operators in CodeDom
              namedOp "op_Inequality" es
        | Prim.Mult,_ -> 
              binOp CodeBinaryOperatorType.Multiply es
        | Prim.Div, _  -> 
              binOp CodeBinaryOperatorType.Divide es
        | Prim.Mod, _ -> 
              binOp CodeBinaryOperatorType.Modulus es 
        | Prim.Max,[e1;e2]  -> 
              let v = MicrosoftResearch.Infer.Models.Variable.Max
              namedOp "Max" es
        | Prim.Minus,[e1;e2]  -> 
              binOp CodeBinaryOperatorType.Subtract es 
        | Prim.Negate,[e1]  -> // no unary operators in CodeDom
              let v = MicrosoftResearch.Infer.Models.Variable.op_UnaryNegation
              namedOp "op_UnaryNegation" es
        | Prim.Not,[e1]  -> 
              let v = MicrosoftResearch.Infer.Models.Variable.op_LogicalNot
              namedOp "op_LogicalNot" es
        | Prim.Plus,[e1;e2]  -> 
             binOp CodeBinaryOperatorType.Add es
        | Prim.And,[e1;e2]  ->  
             let v = MicrosoftResearch.Infer.Models.Variable.op_BitwiseAnd
             binOp CodeBinaryOperatorType.BitwiseAnd es 
        | Prim.Or,[e1;e2]  ->
             let v = MicrosoftResearch.Infer.Models.Variable.op_BitwiseOr
             binOp CodeBinaryOperatorType.BitwiseOr es 
        | Prim.Factor(FactorName"DampBackward"),[e1;e2] -> 
           dampBackward e1 e2
        | Prim.Factor(FactorName"Logistic"),[e1] ->
           namedOp "Logistic" es
        | Prim.Factor(FactorName"Probit"),[e1;e2] ->
           EToCode(E.Prim(Prim.Gt, [E.Dist(GaussianFromMeanAndPrecision,[e1;e2]);E.Const(RealConst(0.0))]))
        | Prim.Factor(FactorName"Sum"),[e1] ->  
            namedOp "Sum" es
        | Prim.Factor(FactorName "Softmax"),[e1] ->  
            namedOp "Softmax" es
        | Prim.Factor(FactorName"InnerProduct"),[e1;e2] ->  
            namedOp "InnerProduct" es
        | Prim.Factor(FactorName"VectorFromArray"),[e1] ->  
            namedOp "Vector" es
        | Prim.Factor(FactorName"Exp"),[e1] ->  
            namedOp "Exp" es
        | Prim.Factor(FactorName"Log"),[e1] ->  
            namedOp "Log" es
        | Prim.Factor(FactorName"Subarray"),[e1;e2] ->  
            namedOp "Subarray" es
        | Prim.Factor(FactorName "DiagonalPDMatrix"),[e1]-> 
            let ovs = [| for e in es -> CodePropertyReferenceExpression(EToCode e,"ObservedValue") :> CodeExpression |]
            //have to figure out what code to spit out
            constant (CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<MicrosoftResearch.Infer.Maths.PositiveDefiniteMatrix>), "DiagonalMatrix",ovs))
        | Prim.Factor(FactorName "IdentityScaledBy"),[e1;e2]->  // I doubt this will work...
            let ovs = [| for e in es -> CodePropertyReferenceExpression(EToCode e,"ObservedValue") :> CodeExpression |]
            constant (CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<MicrosoftResearch.Infer.Maths.PositiveDefiniteMatrix>), "IdentityScaledBy",ovs))
           // Variable.Constant<Maths.PositiveDefiniteMatrix>(Maths.PositiveDefiniteMatrix.IdentityScaledBy(v1.ObservedValue,v2.ObservedValue)) :> Variable
        | Prim.Factor(FactorName f),es when f.StartsWith("#") ->   
            namedOp (f.Substring(1,f.Length-1)) es
       (*
        | Prim.Factor(FactorName "GetItems"),[e1;e2]->  // I doubt this will work...
            let v = interp e1
            let w = interp e2
            let m = typeof<Variable>.GetGenericMethod("GetItems", [|  v.GetType().GetGenericArguments().[0]|])
            let _ =  m.Invoke(v, [| i; w |])
            ()
        | Prim.Factor(FactorName "SubArray"),[e1;e2]->  // I doubt this will work...
            let v = interp e1
            let w = interp e2
            let m = typeof<Variable>.GetGenericMethod("GetItems", [|  v.GetType().GetGenericArguments().[0]|])
            let _ =  m.Invoke(v, [| i; w |])
            ()
        *)
        | Prim.Factor(FactorName f),es -> failwithf "primToCode %A not yet implemented" f

  and distToCode d es = 
      match d,es with
      | Discrete, [Rng r;e1] ->
          namedOp "Discrete" [e1] 
      | DirichletUniform, [Rng r as e1] ->
           // namedOp "DirichletUniform" [e1] fails to compile - see \mlp\pp\sandbox\DirichletUniformBug\
          namedOp "DirichletSymmetric" [e1;E.Const(RealConst 1.0)]
      | _,_ ->
          namedOp (sprintf "%A" d) es
       
  and EToCode e : CodeExpression = 
      let EsToCode (es:E list) = new CodeExpressionCollection([| for e in es -> EToCode e |])
      match e with
        | Var v -> CodeVariableReferenceExpression(v) :> CodeExpression
        | Rng r -> CodeVariableReferenceExpression(r) :> CodeExpression
        | Const (IntConst i) ->  CodePrimitiveExpression(box i) |> constant
        | Const (RealConst r) -> CodePrimitiveExpression(box r) |> constant
        | Const (BoolConst b) -> CodePrimitiveExpression(box b)  |> constant // are booleans supported?
        | Const (StringConst s) -> CodePrimitiveExpression(box s)  |> constant // are booleans supported?
        | IndexRng (e1,r) -> CodeIndexerExpression(EToCode e1,CodeVariableReferenceExpression(r)) :> CodeExpression
        | Index (e1,e2) -> CodeIndexerExpression(EToCode e1,EToCode e2) :> CodeExpression
        | Prim(p,es) -> primToCode p es
        | Dist(d,es) -> distToCode d es
        | _ -> CodeSnippetExpression(sprintf "??%A??" e) :> CodeExpression
  
  let rec VToCode t (o:obj) = //TBR 
      match (t,o) with
      | T_Array(ty,_),(:? System.Array as a) ->
         CodeArrayCreateExpression(a.GetType().GetElementType(),[| for i in 0..a.Length-1 -> VToCode ty (a.GetValue(i)) |]) :> CodeExpression
      | T_Vector, _ -> failwith "VToCode"
      | T_PositiveDefiniteMatrix, _ -> failwith "VToCode"
      | _,_ -> CodePrimitiveExpression(o) :> CodeExpression
      
  let rec eToRange depth E = 
             // let depth = failwith "eToRange:depth undefined" //FIXME
              match E with
              | Syntax.TypedExp(Syntax.Var r,_) -> CodeVariableReferenceExpression(r) :> CodeExpression
              | Syntax.TypedExp(Syntax.Const (IntConst i),_) -> CodeVariableReferenceExpression(Ranges.ranges.[(Ranges.RConst i,depth)]) :> CodeExpression
              | Syntax.TypedExp(Syntax.SizeOf t,_) -> CodeVariableReferenceExpression(Ranges.ranges.[(Ranges.RSizeOf t,depth)]) :> CodeExpression
              | _ -> failwithf "eToRange:%A" E

  let var = System.CodeDom.CodeTypeReference("var") //NB:use var as placeholder for inferred type
  let CodeUsingStatement(v,e,s) = CodeTryCatchFinallyStatement()
  let rec SToCode classExp  s : CodeStatement list = 
       let SToCode = SToCode classExp
       match s with
       | CloneRng (s,r) ->
         [CodeVariableDeclarationStatement(var,s,
                                          CodeMethodInvokeExpression(CodeVariableReferenceExpression(r),"Clone",[||])) 
          :> CodeStatement]
       | LetRng (r,i) -> 
         let ei = CodeObjectCreateExpression(CodeTypeReference(typeof<MicrosoftResearch.Infer.Models.Range>),CodeVariableReferenceExpression(i):>CodeExpression)
                                          :> CodeExpression
         [CodeVariableDeclarationStatement(var,r,ei)
          :> CodeStatement]
       | LetVar (v,e) -> 
         [CodeVariableDeclarationStatement(var,v,EToCode e) 
          :> CodeStatement]
       | LetNew (v,t) ->
         let New = variableNew classExp eToRange t
         [CodeVariableDeclarationStatement(var,v,New)  
         :> CodeStatement]
       | LetArray (v,r,t) -> 
          let rank = TToRank t (TypedExp(Syntax.Var r,T_Int))
          let rng = CodeVariableReferenceExpression(r) :> CodeExpression
          let array = rankToCodeDom classExp eToRange (TToRank t (Syntax.TypedExp(Syntax.Var(r),T_Int)))  //?
          [CodeVariableDeclarationStatement(var,v,array)  
           :> CodeStatement]
       | ObserveValue(v,t,obj) ->
           let e = VToCode t obj
           [ CodeSetObservedValue (CodeVariableReferenceExpression(v)) e]
       | Assign (v,r,E) -> 
           let lhs = CodeIndexerExpression(CodeVariableReferenceExpression(v),[|CodeVariableReferenceExpression( r):>CodeExpression|])
           [CodeAssignStatement(lhs,EToCode E)
            :> CodeStatement]
       | AssignIndex (v,Ei,E) -> 
           let lhs = CodeIndexerExpression(CodeVariableReferenceExpression(v),[|EToCode Ei|])
           [ CodeAssignStatement(lhs,EToCode E)
             :> CodeStatement]
       |  SetTo(v,E) ->
          [CodeExpressionStatement(CodeMethodInvokeExpression(CodeVariableReferenceExpression(v),"SetTo",[|EToCode E|]))
           :> CodeStatement]
       |  Seq (S1,S2) -> 
           (SToCode  S1) @ (SToCode  S2)
       | LetCopy(v,E) -> 
            let copy = CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<MicrosoftResearch.Infer.Models.Variable>), "Copy",[|(*ty;*)EToCode E|]) 
            [CodeVariableDeclarationStatement(var,v,copy)  
             :> CodeStatement]
       | SetValueRange(v,r) ->
            [CodeExpressionStatement(CodeMethodInvokeExpression(CodeVariableReferenceExpression(v),"SetValueRange",[|CodeVariableReferenceExpression(r):>CodeExpression|]))
             :> CodeStatement ]
       //yuck, no "using" in CodeDom...
       |  ForEach(r,S) -> 
           [CodeSnippetStatement("using(Variable.ForEach("+r+"))");
            CodeSnippetStatement("{") ] @
           (SToCode  S) @ 
           [CodeSnippetStatement("}")]
       |  ForLoop(r,x,S) ->
            let rBlock = r+"Block"
            [CodeSnippetStatement("using(var "+ rBlock + " = Variable.ForEach("+r+"))");
             CodeSnippetStatement("{ var "+ x + "=" + rBlock + ".Index;") ] @
            (SToCode  S) @ 
            [CodeSnippetStatement("}")]
       |  If(v,S) ->
           [CodeSnippetStatement("using(Variable.If("+v+"))");
            CodeSnippetStatement("{") ] @
           (SToCode  S) @ 
           [CodeSnippetStatement("}")]
       |  IfNot(v,S) ->
            [CodeSnippetStatement("using(Variable.IfNot("+v+"))");
             CodeSnippetStatement("{") ] @
            (SToCode  S) @ 
            [CodeSnippetStatement("}")]
       |  Skip -> []
       |  Switch(v,S) ->
           [CodeSnippetStatement("using(Variable.Switch("+v+"))");
            CodeSnippetStatement("{") ] @
           (SToCode  S) @ 
            [CodeSnippetStatement("}")]

  (*
    
  let toString code = 
    let compiler = new CSharpCodeProvider()
    let stringWriter = new StringWriter()
    let () = compiler.GenerateCodeFromStatement(code, stringWriter, null)
    stringWriter.ToString()


  let toMethodString S =
     let ss = SToCode  S
     let meth = CodeMemberMethod()
     do meth.Name <- "Model"
     do meth.Attributes <- (MemberAttributes.Public |||  MemberAttributes.Static)
     do meth.ReturnType <- CodeTypeReference(typeof<System.Void>)
     for s in ss do 
      let _ = meth.Statements.Add(s)
      ()
     let compiler = new CSharpCodeProvider()
     let stringWriter = new StringWriter()
     let () = compiler.GenerateCodeFromMember(meth, stringWriter, null)
     stringWriter.ToString()
*)
             
 

                
  let toCompileUnit db namespaceName className typedCoreSchema S =
     let classExp = System.CodeDom.CodeTypeReferenceExpression(System.CodeDom.CodeTypeReference("Utilities")) :> CodeExpression
     let classMethod m es = CodeMethodInvokeExpression(CodeMethodReferenceExpression(classExp,m),es)
     let algorithm = CodeParameterDeclarationExpression(typeof<MicrosoftResearch.Infer.IAlgorithm>,"_algorithm")
     let iterations = CodeParameterDeclarationExpression(typeof<int>,"_iterations")
     let dec_ie = CodeVariableDeclarationStatement(var,"_inferenceEngine",
                                                   CodeObjectCreateExpression(typeof<MicrosoftResearch.Infer.InferenceEngine>,
                                                    [|CodeVariableReferenceExpression algorithm.Name :> CodeExpression|])) 
     let ie = CodeVariableReferenceExpression(dec_ie.Name)
     let CodeSetInferredValue toArray e1 e2 = 
                if toArray 
                then CodeAssignStatement(e1,classMethod "AsArray" [|CodeMethodInvokeExpression(ie,"Infer",[|e2|])|]) :> CodeStatement 
                else CodeAssignStatement(e1,CodeMethodInvokeExpression(ie,"Infer",[|e2|])) :> CodeStatement
     let CodeInferValue e1 = CodeExpressionStatement(CodeMethodInvokeExpression(ie,"Infer",[|e1|])) :> CodeStatement 
                
     let CodeSetInferredValueGeneric e1 ty e2 = CodeAssignStatement(e1,CodeMethodInvokeExpression(CodeMethodReferenceExpression(ie,"Infer",[|ty|]),[|e2|])) :> CodeStatement
     let rec trTables vsToInfer (cps:CodeParameterDeclarationExpression list) (cos:CodeStatement list) (cis:CodeStatement list) tables = 
          match tables with
          | [] -> (List.rev vsToInfer, List.rev cps,List.rev cos,List.rev cis)
          | (Declaration(Table(tn,_),table)::tables) ->
            let is = input(tn,"size")
            let r = range(tn)
            let s = size tn
            let cps = CodeParameterDeclarationExpression(typeof<int>,is)::cps
            let cos = CodeSetObservedValue (CodeVariableReferenceExpression(s)) (CodeVariableReferenceExpression(is))::cos
            let rec trColumns vsToInfer cps cos cis columns   =
              match columns with
              |  [] -> 
                trTables vsToInfer cps cos cis tables
              | (cn,{Type=ty;Markup=m})::rest when Types.det ty = D ->
                match m with
                | Hyper _ -> 
                  trColumns vsToInfer cps cos cis rest
                | Param _ ->
                  let ocn = output(tn,cn)
                  let cp = CodeParameterDeclarationExpression(tyToCode ty,ocn)
                  cp.Direction <- FieldDirection.Out
                  let vi = CodeVariableReferenceExpression(col(tn,cn))
                  let vsToInfer = vi::vsToInfer
                  trColumns vsToInfer (cp::cps) cos cis rest
                | Input ->
                   let icn = input(tn,cn)
                   let cp = CodeParameterDeclarationExpression(System.CodeDom.CodeTypeReference(tyToCode ty,1),icn)
                   let co = CodeSetObservedValue (CodeVariableReferenceExpression(col(tn,cn))) (CodeVariableReferenceExpression(icn))
                   trColumns  vsToInfer (cp::cps) (co::cos) cis rest    
                | Latent _ ->
                  let ocn = output(tn,cn)
                  let cp = CodeParameterDeclarationExpression(System.CodeDom.CodeTypeReference(tyToCode ty,1),ocn)
                  cp.Direction <- FieldDirection.Out
                  let vi = CodeVariableReferenceExpression(col(tn,cn))
                  let ci = CodeInferValue  vi
                  trColumns (vi::vsToInfer) (cp::cps) cos (ci::cis) rest
                | Observable _ -> 
                  let ocn = output(tn,cn)
                  let vi = CodeVariableReferenceExpression(col(tn,cn))
                  let cp = CodeParameterDeclarationExpression(System.CodeDom.CodeTypeReference(tyToCode ty,1),ocn)
                  let ci = CodeInferValue  vi
                  trColumns (vi::vsToInfer) (cp::cps) cos (ci::cis) rest
              | (cn,{Type=ty;Markup=m})::rest when Types.det ty <> Qry ->
                match m with
                | Hyper _ -> 
                  trColumns vsToInfer cps cos cis rest
                | Param _ ->
                  let ocn = output(tn,cn)
                  let cp = CodeParameterDeclarationExpression(typeof<obj>,ocn) //TBR
                  cp.Direction <- FieldDirection.Out
                  let cps = cp::cps
                  let vi = CodeVariableReferenceExpression(col(tn,cn))
                  let ci = CodeSetInferredValue false (CodeVariableReferenceExpression(ocn)) vi 
                  let cis = ci::cis
                  let vsToInfer = vi::vsToInfer
                  trColumns vsToInfer cps cos cis rest
                | Input ->
                   let icn = input(tn,cn)
                   let cp = CodeParameterDeclarationExpression(System.CodeDom.CodeTypeReference(tyToCode ty,1),icn)
                   let co = CodeSetObservedValue (CodeVariableReferenceExpression(col(tn,cn))) (CodeVariableReferenceExpression(icn))
                   trColumns  vsToInfer (cp::cps) (co::cos) cis rest    
                | Latent _ ->
                  let ocn = output(tn,cn)
                  let cp = CodeParameterDeclarationExpression(typeof<obj[]>,ocn) //TBR
                  cp.Direction <- FieldDirection.Out
                  let vi = CodeVariableReferenceExpression(col(tn,cn))
                  let ci = CodeSetInferredValue true (CodeVariableReferenceExpression(ocn)) vi
                  trColumns (vi::vsToInfer) (cp::cps) cos (ci::cis) rest
                | Observable _ -> 
                  let icn = input(tn,cn)
                  let icp = CodeParameterDeclarationExpression(System.CodeDom.CodeTypeReference(System.CodeDom.CodeTypeReference("System.Nullable`1",[|tyToCode ty|]),1),icn) 
                  let ocn = output(tn,cn)
                  let ocp = CodeParameterDeclarationExpression(typeof<obj[]>,ocn) //TBR
                  ocp.Direction <- FieldDirection.Out
                  let cosize = CodeSetObservedValue (CodeVariableReferenceExpression(subarraysize(tn,cn))) 
                                                 (CodeMethodInvokeExpression(CodeMethodReferenceExpression(classExp,"ValueCount"),[|CodeVariableReferenceExpression(icn) :> CodeExpression|]))
                  let coindices = CodeSetObservedValue (CodeVariableReferenceExpression(subarrayindices(tn,cn))) 
                                                 (CodeMethodInvokeExpression(CodeMethodReferenceExpression(classExp,"Indices"),[|CodeVariableReferenceExpression(icn) :> CodeExpression|]))
                  let covalues =  CodeSetObservedValue (CodeVariableReferenceExpression(subarray(tn,cn))) 
                                                 (CodeMethodInvokeExpression(CodeMethodReferenceExpression(classExp,"Values"),[|CodeVariableReferenceExpression(icn) :> CodeExpression|]))
                  let vi = CodeVariableReferenceExpression(col(tn,cn))
                  let ci = CodeSetInferredValue true (CodeVariableReferenceExpression(ocn)) vi
                  trColumns (vi::vsToInfer) (ocp::icp::cps) (covalues::coindices::cosize::cos) (ci::cis) rest
              | (cn,{Type=ty;Markup=m})::rest when Types.det ty = Qry ->
                // just declare out parameters, to be set by the query level statemenets
                match m with
                | Hyper _ -> 
                  trColumns vsToInfer cps cos cis rest
                | Param _ ->
                  let ocn = output(tn,cn)
                  let cp = CodeParameterDeclarationExpression(tyToCode ty,ocn) //TBR
                  cp.Direction <- FieldDirection.Out
                  let cps = cp::cps
                  trColumns vsToInfer cps cos cis rest
                | Input ->
                   let icn = input(tn,cn)
                   let cp = CodeParameterDeclarationExpression(System.CodeDom.CodeTypeReference(tyToCode ty,1),icn)
                   let co = CodeSetObservedValue (CodeVariableReferenceExpression(col(tn,cn))) (CodeVariableReferenceExpression(icn))
                   trColumns  vsToInfer (cp::cps) (co::cos) cis rest    
                | Latent _ ->
                  let ocn = output(tn,cn)
                  let cp = CodeParameterDeclarationExpression(System.CodeDom.CodeTypeReference(tyToCode ty,1),ocn) //TBR
                  cp.Direction <- FieldDirection.Out
                  trColumns vsToInfer (cp::cps) cos cis rest
                | Observable _ -> //what does it mean to have a  qry level output
                  let ocn = output(tn,cn)
                  let ocp = CodeParameterDeclarationExpression(System.CodeDom.CodeTypeReference(tyToCode ty,1),ocn) //TBR
                  ocp.Direction <- FieldDirection.Out
                  trColumns vsToInfer (ocp::cps) cos cis rest
            trColumns vsToInfer cps cos cis table


     let cm = SToCode classExp S
     // wrap model code in one-armed evidence block
     let ev_dec =  CodeVariableDeclarationStatement(var,"_evidence",CodeVariableDot "Bernoulli" [|CodePrimitiveExpression(0.5)|])
     let ev = CodeVariableReferenceExpression(ev_dec.Name)
     let evBlock_dec = CodeVariableDeclarationStatement(var,"_evidenceBlock",CodeVariableDot "If" [|ev|])
     let evBlock = CodeVariableReferenceExpression(evBlock_dec.Name) 
     let cm = (ev_dec :> CodeStatement)::
              (evBlock_dec :> CodeStatement)::
              cm @
              [CodeExpressionStatement(CodeMethodInvokeExpression(evBlock,"CloseBlock",[||])):>CodeStatement];
     
     let oev= "out_evidence"
     let evidence = CodeParameterDeclarationExpression(CodeTypeReference(typeof<Bernoulli>),oev)
     evidence.Direction <- FieldDirection.Out

     let (vsToInfer,cps,cos,cis) = trTables [] [] [] [] typedCoreSchema
    
     let vsToInfer = ev::vsToInfer
     let cps = algorithm::iterations::evidence::cps
     let cis = 
               dec_ie :> CodeStatement::
               CodeSetNumberOfIterations (CodeVariableReferenceExpression dec_ie.Name) (CodeVariableReferenceExpression iterations.Name)::
               CodeSetOptimizeForVariables (CodeVariableReferenceExpression dec_ie.Name) vsToInfer:: // optimize for variables
               CodeSetInferredValueGeneric (CodeVariableReferenceExpression(oev)) (evidence.Type) ev :: // infer evidence
               cis
    
     let comment (text:string) ss = (CodeCommentStatement text :> CodeStatement)::ss
     let ss = comment "construct the model" cm@(comment "observe variables using [in]  parameters" cos)@ (comment "infer variables to set [out] parameters" cis)
     let cCompileUnit = new CodeCompileUnit()
     let cNamespace = new CodeNamespace(namespaceName)
     let _ = cNamespace.Imports.Add(CodeNamespaceImport("System"))

     let _ = cNamespace.Imports.AddRange([| for import in imports -> CodeNamespaceImport(import) |])
   
     let cclass = CodeTypeDeclaration(className)
     cclass.TypeAttributes <- System.Reflection.TypeAttributes.Public
     let meth = CodeMemberMethod()
     do meth.Name <- "Infer"
     for cp in cps do 
         meth.Parameters.Add(cp) |> ignore
     do meth.Attributes <- (MemberAttributes.Public |||  MemberAttributes.Static)
     do meth.ReturnType <- CodeTypeReference(typeof<System.Void>)
     for s in ss do 
       meth.Statements.Add(s) |> ignore

     // inline the query method body, relying on the names of parameters to match free variables.
     let QueryMethod = QueryCompiler.ExtractQuery db typedCoreSchema

     meth.Statements.AddRange(QueryMethod.Statements)
    
     let utils = CodeSnippetTypeMember("\
             public class Utilities {
                public static double StdDeviation(object dist) {
                    var v = ((CanGetVariance<double>)dist).GetVariance();
                    return System.Math.Sqrt(v);
                }

                public static double[] DiscreteProbs(object d) {
                    var counts = ((Discrete)d).GetProbs();
                    return counts.ToArray();
                }

                public static double[] DirichletCounts(object d) {
                    var counts = ((Dirichlet)d).PseudoCount;
                    return counts.ToArray();
                }

                public static double Sum(double[] a) {
                    double s = 0;
                    for (int i = 0; i < a.Length; i++) {
                        s += a[i];
                    }
                    return s;
                }

                public static PositiveDefiniteMatrix DiagonalPDMatrix(double[] v) {
                    var mat = PositiveDefiniteMatrix.Identity(v.Length);
                    mat.SetDiagonal(Vector.FromArray(v));
                    return mat;
                }

                public static object IndexDist(object dists, int i) {
                    var m = dists.GetType().GetMethod(\"get_Item\");
                    return m.Invoke(dists, new object[] { (object)i });
                }

                public static double HashMax(double[] a) {
                    double m = Double.NegativeInfinity;
                    for (int i = 0; i < a.Length; i++) {
                        m = System.Math.Max(a[i], m);
                    }
                    return m;
                }
                public static int ArgMax(double[] a) {
                    int m = 0;
                    for (int i = 0; i < a.Length; i++) {
                        if (a[i] > a[m])
                            m = i;
                    }
                    return m;
                }

                public static int ArgMin(double[] a) {
                    int m = 0;
                    for (int i = 0; i < a.Length; i++) {
                        if (a[i] < a[m])
                            m = i;
                    }
                    return m;
                }

                public static object[] AsArray(object o) {
                    var a = (o as ConvertibleToArray).ToArray();
                    var os = new object[a.GetLength(0)];
                    for (int i = 0; i < os.Length; i++)
                        os[i] = a.GetValue(i);
                    return os;
                }

                public static Range GetValueRange(Variable v) {
                    return v.GetValueRange(throwIfMissing: false);
                }
                public static T SetValueRange<T>(T v, Range r) where T : Variable {
                    if (r != null) v.SetValueRange(r);
                    return v;
                }

                public static int ValueCount<T>(T?[] vs) where T : struct {
                    var c = 0;
                    for (var i = 0; i < vs.Length; i++) {
                        if (vs[i].HasValue) c++;
                    }
                    return c;
                }
                public static T[] Values<T>(T?[] vs) where T : struct {
                    var ws = new T[ValueCount(vs)];
                    var h = 0;
                    for (var i = 0; i < vs.Length; i++) {
                        if (vs[i].HasValue) ws[h++] = vs[i].Value;
                    }
                    return ws;
                }
                public static int[] Indices<T>(T?[] vs) where T : struct {
                    var ws = new int[ValueCount(vs)];
                    var h = 0;
                    for (var i = 0; i < vs.Length; i++) {
                        if (vs[i].HasValue) ws[h++] = i;
                    }
                    return ws;
                }
                public static U Let<T, U>(T x, Func<T, U> f) {
                    return f(x);
                }
                public static T[] ForLoop<T>(int n, Func<int, T> f) {
                    var a = new T[n];
                    for (int i = 0; i < n; i++) {
                        a[i] = f(i);
                    }
                    return a;
                }

            }")

     let _ = cclass.Members.Add(meth)
     let _ = cclass.Members.Add(utils)
     let _ = cNamespace.Types.Add(cclass)
     let _ = cCompileUnit.Namespaces.Add(cNamespace)
     let compiler = new CSharpCodeProvider()
     let stringWriter = new StringWriter()
     let options = new CodeGeneratorOptions()
     let () = compiler.GenerateCodeFromCompileUnit(cCompileUnit, stringWriter, options)
     stringWriter.ToString()