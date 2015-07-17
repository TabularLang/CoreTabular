module MicrosoftResearch.Infer.Tabular.Plates

open MicrosoftResearch.Infer.Tabular.Syntax
open MicrosoftResearch.Infer.Tabular.Pretty
module P = MicrosoftResearch.Infer.Tabular.Pretty

// graphviz-2.34\release\bin\dot.exe -Tgif Tabular.dot -O
open System.IO


let getGraphvizLocation () = 
   let keys = System.Environment.GetEnvironmentVariables().Keys 
   let graphivzDotKey = keys |> Seq.cast<string> |> Seq.tryFind (fun s -> s.ToLower().Equals "graphvizdot")
   if graphivzDotKey.IsSome then
      Some <| System.Environment.GetEnvironmentVariable(graphivzDotKey.Value)
   else
      let path = System.Environment.GetEnvironmentVariable("Path");
      let potentialPlace = [ for folder in path.Split(';') -> Path.Combine(folder, "dot.exe") ] |> List.tryFind File.Exists      
      if potentialPlace.IsSome then 
         potentialPlace
      else 
         let s = System.Environment.GetEnvironmentVariable("programfiles(x86)") + @"\Graphviz2.34\bin\dot.exe"
         if File.Exists s then 
               Some s
         else None

let runDot dotexePath workingFolder inputFolder filename =

  let pInfo = new System.Diagnostics.ProcessStartInfo();
  pInfo.FileName <- dotexePath ;
  pInfo.WorkingDirectory <- workingFolder;
  pInfo.WindowStyle <- System.Diagnostics.ProcessWindowStyle.Hidden
  pInfo.CreateNoWindow <- true
  pInfo.Arguments <- sprintf "-Tgif %s -O" (inputFolder + @"\" + filename);
  pInfo.LoadUserProfile <- true;
  pInfo.UseShellExecute <- false;
  let proc = System.Diagnostics.Process.Start(pInfo)
  proc.WaitForExit()

// semantics of core Tabular

type ID = string
type Item =
  | Node of ID * string
  | Edge of ID * Option<string> * ID
  | Cluster of ID * string * List<Item>

// bring all edges to the top-level, so that the nodes alone constrain layout of graph
let rec normal2(items:List<Item>): List<Item>*List<Item> =
  match items with
  | [] -> [],[]
  | item::items' ->
    let nodes,edges = normal2 items'
    match item with
    | Node(id,label) -> item::nodes,edges
    | Edge(id1,opt,id2) -> nodes,item::edges
    | Cluster(id,style,items'') ->
      let nodes',edges' = normal2 items''
      Cluster(id,style,nodes')::nodes, edges'@edges

let gensym =
  let anon = ref 0
  fun() -> (anon := !anon+1; sprintf "%d" (!anon))

let rec render items =
  let rec f item =
    match item with
    | Node(id,label) -> sprintf "\"%s\" [%s];" id label
    | Edge(i1,None,i2) -> sprintf "\"%s\" -> \"%s\";" i1 i2
    | Edge(i1,Some(label),i2) -> sprintf "\"%s\" -> \"%s\" [label=\"%s\"];" i1 i2 label
    | Cluster("",style,items') -> sprintf "subgraph \"cluster%s\" { label=\"\"; %s %s }" (gensym()) style (render items')
    | Cluster(nme,style,items') -> sprintf "subgraph \"cluster%s\" { labeljust=\"l\"; label=\"%s\"; %s %s }" nme nme style (render items')

  String.concat "\n" (List.map f items)

let dot items = sprintf "strict digraph Tabular { %s }" (render items) // strict suppresses multiple edges
  
let TypedId ty nme = sprintf "%s %s" (columnTypeToStr ty)  nme
let VariableId tabnme nme = sprintf "Variable.%s.%s" tabnme nme
let FactorId tabnme nme = sprintf "Factor.%s.%s" tabnme nme

let deps (tabnme:TableName) factor e =
  let rec f rho e =
   match e with
   | Var v -> if Set.contains(v) rho then [] else [Edge(VariableId tabnme v,None,factor)]
   | Const (_) -> []
   | Prim(_,es) -> List.collect (f rho) es
   | Dist(_,es) -> List.collect (f rho) es
   | SizeOf(t) -> []
   | DeRef(e1,tn,cn) -> //let label = sprintf "%s.ID=%s" tn (exprToStr e1)
                        [Edge(VariableId tn cn,None,factor)] @ f rho e1
   | Ref(tn,cn) -> [Edge(VariableId tn cn,None,factor)] //TBR
   | If(e1,e2,e3) -> List.concat [f rho e1; f rho e2; f rho e3]
   | ForLoop(x,e1,e2) -> let rho' = Set.add x rho in List.concat [f rho e1; f rho' e2]
   | Array(es) -> List.collect (f rho) es
   | Subscript(Var x,e1) -> //let label=sprintf "%s" (exprToStr e1)
                            [Edge(VariableId tabnme x,None,factor)] @ f rho e1
   | Subscript(e1,e2) -> List.concat [f rho e1; f rho e2] // TODO: do the general case
   | Constraint(e1,t1) -> f rho e1
   | Let(x,e1,e2) -> let rho' = Set.add x rho in List.concat [f rho e1; f rho' e2]
   | Infer(d,es,x,e1) -> List.collect (f rho) (e1::es)
   | TypedExp(e,ty) -> f rho e
   | _ -> failwith (sprintf "deps: %s unexpected expression" (exprToStr e))

  in f (Set.empty) e

let ExprNode0 (tabnme:TableName, nme:ColumnName, ty:ColumnType, e:Exp,style:string): List<Item> =
  let node = VariableId tabnme nme
  let label = TypedId ty nme
  let factor = FactorId tabnme nme
  [Node(factor, sprintf "label=\"%s\",shape=box,style=unfilled,fillcolor=black,height=0.1,width=0.1" (exprToStr e));                 
   Edge(factor,None,node);
   Node(node, sprintf "label=\"%s\",%s" label style)]
  @ deps tabnme factor e

// deconstruct a possibly typed MExp
let rec private (|TypedForLoop|_|) M = match M with (TypedExp (TypedForLoop e,_)) -> Some e | ForLoop(x,e1,e2) -> Some (x,e1,e2) | _ -> None


let rec ExprNode (tabnme:TableName, nme:ColumnName, ty:ColumnType, e:Exp,style:string) =
  match e,ty with
  | TypedForLoop(x,e1,e2),T_Array(ty,_) -> [Cluster(sprintf "%s<%s" x (exprToStr e1),"style=unfilled;color=black;",ExprNode(tabnme,nme,ty,e2,style))]
  | _,_ -> ExprNode0 (tabnme,nme,ty,e,style) 

// deconstruct a possibly typed MExp
let rec private (|TypedMExp|_|) M = match M with (TypedModel (TypedMExp e,_)) -> Some e | MExp(e) -> Some e | _ -> None

let color red green blue = sprintf "\"#%02x%02x%02x\"" red green blue
let office2013blue = color 91 155 213
let office2013orange = color 237 125 49
let office2013gray = color 165 165 165

let Column1 (tabnme:TableName) (nme:ColumnName, col:Column): List<Item> =
  match col.Markup with
   | Hyper(e) -> [Node(VariableId tabnme nme, sprintf "style=unfilled,color=black,label=\"%s\"" (sprintf "%s=%s" (TypedId (col.Type) nme) (exprToStr e)))]
   | Param(TypedMExp(e)) -> ExprNode (tabnme,nme, col.Type, e, "style=filled,color="+office2013gray)
   | Input -> []
   | Latent(M) -> []
   | Observable(M) -> []
   | Param(_) -> failwith "Column1: not core Tabular"

let Column2 (tabnme:TableName) (nme:ColumnName, col:Column): List<Item> =
  match col.Markup with
   | Hyper(e) -> []
   | Param(M) -> []
   | Input -> [Node(VariableId tabnme nme, sprintf "label=\"%s\", style=filled,color=%s" (TypedId (col.Type) nme) office2013blue)]
   | Latent(TypedMExp(e)) -> ExprNode (tabnme, nme, col.Type, e, "style=filled,color="+office2013gray)
   | Observable(TypedMExp(e)) -> ExprNode (tabnme, nme, col.Type, e, "style=filled,color="+office2013orange)
   | _ -> failwith (sprintf "Column2: %s not core Tabular" (markupToStr col.Markup))

let platesDecl (S:Schema) ((Declaration (decl, T)):Declaration) =
  match decl with
  | Table(tabnme,oId) ->
      let text = sprintf "labeljust=l;nojustify=true;style=filled;color=gray95" // ;label=\"%s\" (P.declToStr decl)
      let T' = T //coreT S T
      let step1 = List.collect (Column1 tabnme) T'
      let step2 = List.collect (Column2 tabnme) T'
      let label = sprintf "ID<%s" (exprToStr (SizeOf(tabnme)))
      [Cluster("", text, [Cluster("","",step1)] @ [Cluster(label, "style=unfilled;color=black;", step2)])]
  | Fun(_) -> []


let plates nme tmpPath outPath (S:Schema) = 
  let S = coreS S
  let fileName = sprintf @"%s.dot" nme
  let nodes,edges = normal2 (List.collect (platesDecl S) S)
  let x = System.IO.File.WriteAllText(tmpPath  + @"\"+  fileName, dot (nodes@edges))
  runDot (Option.get <| getGraphvizLocation ()) outPath  tmpPath  fileName


let platesCrusso nme (S:Schema) = 
   plates  nme @"C:\Users\crusso\Desktop\" @"C:\Users\crusso\Desktop" S

let platesAdg  nme (S:Schema) = 
   plates  nme @"C:\Users\adg\Desktop\" @"C:\Users\adg\Desktop" S
