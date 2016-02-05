type id = string

type CDist
 = CNormal of u * u //mean and precision
 | CGamma of u * u  //shape and rate

and u
 = Scalar of double
 | Var of string
 | Interaction of u * u
 | Path of u * u
 | LinkLog of u
 | Dist of CDist
 | Noise

let cdist_args d =
  match d with
  | CNormal (mean,prec) -> [mean; prec]
  | CGamma (shape,rate) -> [shape; rate]

let rec is_det u = //is u deterministic?
  match u with
  | Noise -> false
  | Dist _ -> false
  | Path(u,v) -> is_det u && is_det v
  | Interaction(u,v) -> is_det u && is_det v
  | _ -> true

let rec is_noise u = //is u just noise?
  match u with
  | Noise -> true
  | _ -> false

type r
 = Immed of u
 | Coeff of u * id * r
 | Sum of r * r
 | Cond of r * u
 | Res of id * r

// pretty-printing

let s_large = Scalar(1e5)
let s_small = Scalar(1e-5)

let rec pretty_predictor u =
  match u with
  | Scalar s ->
      match s with
      | 0.0 -> "0"
      | 1.0 -> "1"
      | _ when Scalar s=s_large -> "large"
      | _ when Scalar s=s_small -> "small"
      | _ -> sprintf "%f" s
  | Var id -> id
  | Interaction(u,v) -> sprintf "%s:%s" (pretty_predictor u) (pretty_predictor v)
  | Path(u,v) -> sprintf "(%s).%s" (pretty_predictor u) (pretty_predictor v)
  | LinkLog u -> sprintf "Log(%s)" (pretty_predictor u)
  | Dist d -> pretty_cdist d //sprintf "%s(%s)" id (pretty_predictors us)
  | Noise -> "?"

and pretty_cdist d =
  match d with
  | CNormal (mean,precision) -> sprintf "Normal(%s, prec=%s)" (pretty_predictor mean) (pretty_predictor precision)
  | CGamma  (shape,rate)  -> sprintf "Gamma(%s, rate=%s)" (pretty_predictor shape) (pretty_predictor rate)

and pretty_predictors us = if us=[] then "" else List.reduce (fun s1 s2 -> s1+","+s2) (List.map pretty_predictor us)

let rec pretty_regression r =
  match r with
  | Immed(u) -> sprintf "^%s" (pretty_predictor u)
  | Coeff(u,alpha,r) -> sprintf "%s{%s~%s}" (pretty_predictor u) alpha (pretty_regression r)
  | Sum(r1,r2) -> sprintf "%s + %s" (pretty_regression r1) (pretty_regression r2)
  | Cond(r,u) -> sprintf "(%s | %s)" (pretty_regression r) (pretty_predictor u)
  | Res(alpha,r) -> sprintf "(nu %s)%s" alpha (pretty_regression r)

//modelling language
type stan_type
 = SInt
 | SReal

type SDist
 = SNormal of stan_exp * stan_exp //mean and standard deviation
 | SGamma of stan_exp * stan_exp //shape and scale

and stan_exp
 = Id of id
 | Index of stan_exp * List<stan_exp>
 | StanInt of int
 | StanReal of double
 | Plus of stan_exp * stan_exp
 | Times of stan_exp * stan_exp
 | Inv of stan_exp
 | Fun of id * List<stan_exp>

let rec mentions id s =
 match s with
 | Id i -> i = id
 | Index (s,is) -> mentions id s || List.exists (mentions id) is
 | Plus (s1,s2) -> mentions id s1 || mentions id s2
 | Times (s1,s2) -> mentions id s1 || mentions id s2
 | Fun (f,args) -> f = id || List.exists (mentions id) args
 | _ -> false

type stan_decl = Decl of stan_type * id * stan_exp option * stan_exp option * List<stan_exp>

let decl_name decl =
  match decl with
  | Decl(_,name,_,_,_) -> name

type stan_command
 = Assign of stan_exp * stan_exp
 | Draw of stan_exp * SDist
 | For of id * stan_exp * stan_exp * List<stan_command>

let rec modifies id cmd =
  match cmd with
  | Assign (lhs,_) -> mentions id lhs
  | Draw (lhs,_) -> mentions id lhs
  | For (_,_,_,cmds) -> List.exists (modifies id) cmds

type data
 = Const of id * Choice<int, double>
 | PosInt of id
 | Real of id * stan_exp option * stan_exp option
 | Vector of id * stan_exp * stan_exp option * stan_exp option

type data_block = List<stan_decl>

type transformed_data_block = List<stan_decl> * List<stan_command>

type parameters_block = List<stan_decl>

type transformed_parameters_block = List<stan_decl> * List<stan_command>

type model_block = List<stan_command>

type stan_model = data_block * parameters_block * transformed_parameters_block option * model_block

type model
 = Model of id * r


//type checking
type ctype
 = CReal
 | CPosReal
 | CNat
 | CMod of id
 | CArray of ctype * id

let rec unfold_ctype t =
  match t with
  | CArray (t,n) -> let (t', ns) = unfold_ctype t in (t', n :: ns)
  | t -> (t, [])

let make_array ctype is = List.foldBack (fun i t -> CArray (t,i)) is ctype

let rec strip_indices is t =
  if List.isEmpty is then Some t else
  match t with
  | CArray (t', i) -> if i = List.head is then strip_indices (List.tail is) t' else None
  | _ -> None

type env = List<id * ctype>

let well_formed (gamma : env) = List.length gamma = List.length (List.collect (fun x -> List.filter (fun (y,t) -> x = y) gamma) (List.map fst gamma))

let type_var gamma v = List.tryPick (fun (x,t) -> if x = v then Some t else None) gamma

type enclosure = List<int>

type penv = env * enclosure

let rec type_pred penv u =
  let gamma, e = penv in
    match u with
    | Scalar s -> if well_formed gamma then Some CReal else None 
    | Var v -> let t = type_var gamma v in Option.bind (strip_indices e) t
    | Interaction (u, v) -> if type_pred penv u = Some CReal && type_pred penv v = Some CReal then Some CReal else None
    | Path (u, v) -> match type_pred penv u with | Some (CMod f) -> type_pred (gamma, [f]) v | _ -> None 
    | LinkLog u -> type_pred penv u
    | Dist d -> if List.forall (fun x -> type_pred penv x = Some CReal) (cdist_args d) then Some CReal else None
    | Noise -> Some CReal

type renv = penv * enclosure

let rec type_reg renv r =
  let (penv, f) = renv
  let (gamma, e) = penv in
  match r with
  | Immed v -> if type_pred penv v = Some CReal then Some [] else None
  | Coeff (v, a, r) -> if type_pred penv v = Some CReal then let pi = type_reg ((gamma,f), []) r in Option.map (fun pi -> (a, make_array (if v = Noise then CPosReal else CReal) f) :: pi) pi else None
  | Sum (r1, r2) -> let pi = type_reg renv r1 in let pi' = Option.bind (fun g -> type_reg ((List.append g gamma, e), f) r2) pi in Option.bind (fun p1 -> Option.map (fun p2 -> List.append p1 p2) pi') pi
  | Cond (r,v) -> let s = type_pred penv v in match s with | Some (CMod n) -> type_reg (penv, (n :: f)) r | _ -> None 
  | Res (a,r) -> let pi = type_reg renv r in Option.map (List.filter (fun (b,_) -> not (a = b))) pi

//reduction to core regression (restrictions discarded)
type q = List<u * (id * r) option * List<u>> //predictor, coefficient, group

let rec to_core r =
  let rec f r cs = 
    match r with
    | Immed(u) -> [(u, None, [])]
    | Coeff(u,a,r) -> [(u, Some (a, r), cs)]
    | Sum(r1,r2) -> f r1 cs @ f r2 cs
    | Cond(r,u) -> f r (u::cs)
    | Res(alpha,r) -> f r cs
  in
    f r []

type reg
 = Deterministic of q  //a regression with no probabilistic components
 | Noisy of q * id * r * List<u> // a deterministic regression plus noise
 | Prior of CDist  //a single draw from a distribution
 | Other of q  //all other regressions

let decompose r =
  let q = to_core r
  let us = List.map (fun (u,_,_) -> u) q
  let n = List.length us
  let noiseless = List.filter ((fun (u,_,_) -> u) >> is_noise >> not) q
  let noises = List.filter ((fun (u,_,_) -> u) >> is_noise) q
  let vs = List.map (fun (u,_,_) -> u) noiseless
  let k = List.length vs in
    if List.forall is_det us then Deterministic q else
    if n - k = 1 && List.forall is_det vs then let (pi,r',es) = (fun (Noise,Some(pi,r'),es) -> (pi,r',es)) (List.head noises) in Noisy(noiseless, pi, r', es) else
    if n = 1 then match List.head us with | Dist d -> Prior d else
    Other q

    

//conversion to stan
let rec u2s index u =
  match u with
  | Scalar s -> StanReal s
  | Var id -> Index (Id id, index)
  | Interaction(u,v) -> Times (u2s index u, u2s index v)
  | Path(u,v) -> (u2s [u2s index u] v)
  | LinkLog u -> Fun ("log", [u2s index u])
  | _ -> raise (System.ArgumentException "attempted to convert a nondeterministic predictor to a stan expression")

and cdist2sdist index d =
  match d with
  | CNormal (mean,precision) -> SNormal(u2s index mean, Inv (Fun ("sqrt",[u2s index precision])))
  | CGamma (shape,rate) -> SGamma(u2s index shape, Inv (u2s index rate))

let get_path index cs = List.map (u2s index) cs

let rec size_of gamma (c : u) = match c with
  | Var v -> match (List.find (fun (a,t) -> a = v) gamma) with | (_,CArray (t,n)) -> n
  | Path (u,v) -> size_of gamma v
  | _ -> raise (System.ArgumentException "attempted to take size of a compound predictor")

let rec separate gamma (id : id) r (size : id list) =
  let is = List.map (sprintf "i%d") [1 .. (List.length size)]
  let index = List.map Id is
  let lhs = Index (Id id, index)
  let q = to_core r
  let to_stan_exp index (u,n,cs) =
    match n with
    | None -> u2s index u
    | Some(a,r) -> Times (u2s index u, u2s (get_path index cs) (Var a))
  let q2s = List.map (to_stan_exp index) >> List.reduce (fun e1 e2 -> Plus(e1,e2))
  let s =
    match decompose r with
    | Deterministic q -> Assign (lhs, q2s q)
    | Noisy (q,pi,r',cs) -> Draw (lhs, SNormal (q2s q, Inv (Fun ("sqrt",[u2s (get_path index cs) (Var pi)]))))
    | Prior d -> Draw(lhs, cdist2sdist index d)
    | Other q -> raise (System.NotImplementedException())
  let inner (_,n,cs) =
    Option.map (fun (a,r) -> let (t,ns) = unfold_ctype (Option.get (type_var gamma a)) in (a, r, ns)) n
  in
    (List.foldBack (fun (i,n) s -> For (i, StanInt 1, Id n, [s])) (List.zip is size) s, List.choose inner q)

let rec r2cmds gamma id r size =
  let (s, xs) = separate gamma id r size in
    List.concat (List.map ((fun f (x,y,z) -> f x y z) (r2cmds gamma)) xs) @ [s]

let model2cmds gamma m = match m with
  | Model (id,r) -> r2cmds gamma id r [size_of gamma (Var id)]

let rec is_det_cmd c = match c with
  | Assign(_,_) -> true
  | Draw(_,_) -> false
  | For(_,_,_,cs) -> List.forall is_det_cmd cs

let split_cmds = List.partition is_det_cmd

let param2decl (a,t) =
  let (t',ns) = unfold_ctype t 
  let cs = List.map Id ns in
    match t' with
    | CReal -> Decl(SReal, a, None, None, cs)
    | CPosReal -> Decl(SReal, a, Some (StanReal 0.0), None, cs)
    | CNat -> Decl(SInt, a, Some (StanInt 1), None, cs)
    | CMod k -> Decl(SInt, a, Some (StanInt 1), Some (Id k), cs)

let model2params gamma m =
  match m with
    | Model (y,r) -> List.map param2decl (Option.get (type_reg ((gamma,[size_of gamma (Var y)]), []) r))

let split_param (dets, probs) (ps : List<stan_decl>) = (List.filter (decl_name >> fun p -> List.exists (modifies p) dets) ps,
                                                        List.filter (decl_name >> fun p -> List.exists (modifies p) probs) ps)

let rec get_decl gamma (size : List<id>) u =
  let rec process_path size u =
    match u with
    | Var v -> ([size_of gamma (Var v)], [Decl(SInt, v, Some (StanInt 1), None, size)])
    | Path (u1,u2) -> let (size1, ds1) = process_path size u1 in let (size2, ds2) = process_path size u2 in (size2, ds1 @ ds2)
  in
    match u with
    | Scalar _ -> []
    | Var v -> [Decl(SReal, v, None, None, List.map Id size)]
    | Interaction (u1,u2) -> get_decl gamma size u1 @ get_decl gamma size u2
    | Path (u1,u2) -> let (size, ds1) = process_path (List.map Id size) u1 in ds1 @ get_decl gamma size u2
    | LinkLog u -> get_decl gamma size u
    | Dist d -> List.concat (List.map (get_decl gamma size) (cdist_args d))
    | Noise -> []


let rec get_data_decl gamma r (size : List<id>) =
  let rec f r size cs =
    match r with
    | Immed u -> get_decl gamma size u
    | Coeff (u,_,r') -> get_decl gamma size u @ get_data_decl gamma r' (List.map (size_of gamma) cs)
    | Sum (r1,r2) -> get_data_decl gamma r1 size @ get_data_decl gamma r2 size
    | Cond(r,c) -> get_decl gamma size c @ f r size (c::cs)
    | Res(_,r) -> f r size cs
  in
    f r size []

let model2data gamma m =
  match m with
  | Model (y,r) -> let size = [size_of gamma (Var y)] in Decl (SReal, y, None, None, List.map Id size) :: get_data_decl gamma r size

let model2stan gamma m =
  let pi = match (match m with Model (y,r) -> type_reg ((gamma,[size_of gamma (Var y)]),[]) r) with | Some x -> x | None -> raise(System.ArgumentException("regression is not well-typed"))
  let gamma' = List.append gamma pi
  let cmds = split_cmds (model2cmds gamma' m)
  let (dets, probs) = cmds
  let (ts,ps) = split_param cmds (model2params gamma m)
  let data_block = List.map param2decl gamma
  let param_block = ps
  let trans_param_block = (ts,dets)
  let model_block = probs
  in
    (data_block, param_block, trans_param_block, model_block)

//pretty printing
let pretty_stan_type t =
  match t with
  | SInt -> "int"
  | SReal -> "real"

let rec pretty_stan_exp s =
  match s with 
   | Id id -> id
   | Index (a,i) -> if List.isEmpty i then (pretty_stan_exp a) else sprintf "%s[%s]" (pretty_stan_exp a) (String.concat "," (List.map pretty_stan_exp i))
   | StanInt n -> sprintf "%d" n
   | StanReal x -> if x >= 0.0 then sprintf "%g" x else sprintf "(%g)" x
   | Plus (s1,s2) -> sprintf "%s + %s" (pretty_stan_exp s1) (pretty_stan_exp s2)
   | Times (s1,s2) -> sprintf "%s*%s" (pretty_stan_exp s1) (pretty_stan_exp s2)
   | Inv s -> sprintf "1.0 / (%s)" (pretty_stan_exp s)
   | Fun (f,ss) -> f + "(" + String.concat "," (List.map (pretty_stan_exp) ss) + ")"

let pretty_sdist d =
  match d with
  | SNormal (mean,stddev) -> sprintf "normal(%s,%s)" (pretty_stan_exp mean) (pretty_stan_exp stddev)
  | SGamma (shape,scale) -> sprintf "gamma(%s,%s)" (pretty_stan_exp shape) (pretty_stan_exp scale)

let rec pretty_stan_command s =
  match s with
   | Assign (x,s) -> sprintf "%s <- %s;" (pretty_stan_exp x) (pretty_stan_exp s)
   | Draw (x,d) -> sprintf "%s ~ %s;" (pretty_stan_exp x) (pretty_sdist d)
   | For (i,lower,upper,cmds) -> sprintf "for (%s in %s:%s) {\n%s\n}" i (pretty_stan_exp lower) (pretty_stan_exp upper) (String.concat ";\n" (List.map pretty_stan_command cmds))

let pretty_bounds b =
  match b with
  | (None, None) -> ""
  | (Some lower, None) -> sprintf "<lower=%s>" (pretty_stan_exp lower)
  | (None, Some upper) -> sprintf "<upper=%s>" (pretty_stan_exp upper)
  | (Some lower, Some upper) -> sprintf"<lower=%s,upper=%s>" (pretty_stan_exp lower) (pretty_stan_exp upper)

let pretty_stan_decl d =
  match d with
  | Decl (t,id,lower,upper,sizes) -> sprintf "%s%s %s;" (pretty_stan_type t) (pretty_bounds (lower,upper)) (pretty_stan_exp (Index (Id id,sizes)))


let pretty_data d =
   match d with
   | Const (id, (Choice1Of2 n)) -> sprintf "int %s;\n%s <- %d;\n" id id n
   | Const (id, (Choice2Of2 x)) -> sprintf "real %s\n%s <- %f;\n" id id x
   | PosInt id -> sprintf "int<lower=1> %s;\n" id
   | Real (id, lower, upper) -> sprintf "real%s %s;\n" (pretty_bounds (lower,upper)) id
   | Vector (id, size, lower, upper) -> sprintf "real%s %s[%s];\n" (pretty_bounds (lower,upper)) id (pretty_stan_exp size)

let pretty_data_block d = "data {\n" + String.concat "\n" (List.map pretty_stan_decl d) + "\n}\n"

let pretty_param_block p = "parameters {\n" + String.concat "\n" (List.map pretty_stan_decl p) + "\n}\n"

let pretty_trans_param_block (d,c) = "transformed parameters {\n" + String.concat "\n" (List.map pretty_stan_decl d) + "\n" 
                                                                  + String.concat "\n" (List.map pretty_stan_command c) + "\n}\n"

let pretty_model_block m = "model {\n" + String.concat "\n" (List.map pretty_stan_command m) + "\n}\n"

let pretty_stan s =
  match s with
  | (d,p,t,m) -> pretty_data_block d + "\n" + pretty_param_block p + "\n" + pretty_trans_param_block t + "\n" + pretty_model_block m

let pretty_model m =
  match m with
  | Model (id,r) -> id + " ~ " + pretty_regression r + ";\n"



//examples
let ex_data = [ Const ("N", Choice1Of2 10); PosInt "K"; Real ("alpha", Some (StanReal 0.0), None); Vector ("x", Id "K", None, None) ]
let ex_model = Model ("y", Sum(Immed (Scalar 1.0), Immed (Var "alpha")))

let def_r = Immed( Dist(CNormal(Scalar 0.0, Scalar 1e-5)) )
let def_noise = Immed( Dist(CGamma (Scalar 1.0, Scalar 1e5)) )

let radon_data = [("C", CNat); ("N", CNat); ("c", CArray (CMod "C", "N")); ("u", CArray (CReal, "C")); ("f", CArray (CReal, "N")); ("activity", CArray (CReal, "N"))]
let radon_inner_r = Sum(Coeff(Scalar 1.0, "a", def_r), Sum(Coeff(LinkLog (Var "u"), "b", def_r), Coeff(Noise, "eta", def_noise)))
let radon_outer_r = Sum(Cond(Coeff(Scalar 1.0, "alpha", radon_inner_r) , Var "c"),
                    Sum(Coeff(Var "f", "beta" , def_r), Coeff(Noise, "pi", def_noise)))
let radon = Model ("activity", radon_outer_r)

let insteval_data = [("N", CNat); ("score", CArray (CReal, "N")); ("S", CNat); ("student", CArray (CMod "S", "N")); ("P", CNat); ("professor", CArray (CMod "P", "N"));
                     ("D", CNat); ("department", CArray (CMod "D", "N")); ("R", CNat); ("service", CArray (CMod "R", "N"))]
let insteval_r = Sum(Cond(Coeff(Scalar 1.0, "a", def_r), Var "student"),
                 Sum(Cond(Coeff(Scalar 1.0, "b", def_r), Var "professor"),
                 Sum(Cond(Cond(Coeff(Scalar 1.0, "c", def_r), Var "department"), Var "service"),
                     Coeff(Noise, "pi", def_noise))))
let insteval = Model("score", insteval_r)

let trueskill_data = [("M", CNat); ("result", CArray (CReal, "M")); ("P", CNat); ("p1", CArray (CMod "P", "M")); ("p2", CArray (CMod "P", "M"))]
let trueskill_r = Sum(Cond(Coeff(Scalar 1.0, "skill", Immed(Dist(CNormal (Scalar 25.0, Scalar 10.0)))), Var "p1"),
                  Sum(Cond(Coeff(Scalar (- 1.0), "skill'", Immed(Var "skill")), Var "p2"),
                      Coeff(Noise, "pi", Immed(Scalar 100.0))))
let trueskill = Model("result", trueskill_r)



//file I/O
let to_file gamma model filename =
  System.IO.File.WriteAllText(filename, "// " + pretty_model model + "\n" + pretty_stan (model2stan gamma model))