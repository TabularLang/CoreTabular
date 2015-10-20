// July 2015

type id = string

type u
 = Scalar of double
 | Var of string
 | Interaction of u * u
 | Path of List<u> * u

type r
 = Immed of u
 | Coeff of u * id * r
 | Draw of id * List<u>
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
  | Path(us,v) -> sprintf "(%s).%s" (pretty_predictors us) (pretty_predictor v)

and pretty_predictors us = if us=[] then "" else List.reduce (fun s1 s2 -> s1+","+s2) (List.map pretty_predictor us)

let rec pretty_regression r =
  match r with
  | Immed(u) -> sprintf "^%s" (pretty_predictor u)
  | Draw(id,us) -> sprintf "%s(%s)" id (pretty_predictors us)
  | Coeff(u,alpha,r) -> sprintf "%s{%s~%s}" (pretty_predictor u) alpha (pretty_regression r)
  | Sum(r1,r2) -> sprintf "%s + %s" (pretty_regression r1) (pretty_regression r2)
  | Cond(r,u) -> sprintf "(%s | %s)" (pretty_regression r) (pretty_predictor u)
  | Res(alpha,r) -> sprintf "(nu %s)%s" alpha (pretty_regression r)

// simplification

let rec simplify u =
  match u with
  | Interaction(u1,u2) when u1=Scalar 0.0 -> Scalar 0.0
  | Interaction(u1,u2) when u2=Scalar 0.0 -> Scalar 0.0
  | Interaction(u1,u2) when u1=Scalar 1.0 -> simplify u2
  | Interaction(u1,u2) when u2=Scalar 1.0 -> simplify u1
  | Path(vs,Scalar s) -> Scalar s
  | _ -> u
 
// example

let fresh =
  let r = ref 0 in
  fun id -> (let n = !r in r := 1 + !r; sprintf "%s%d" id n)

let noise pi u = Sum(Coeff(Scalar 0.0, pi, u),Draw("GaussianMP", [Scalar 0.0; Var pi]))
let anon_noise() = let pi = fresh "pi" in Res(pi, noise pi (Draw("GammaSR", [Scalar 1.0; s_small])))

let uninf_Coeff u id = Coeff (u, id, Draw("Gaussian", [Scalar 0.0; s_large]))

let r1 = Coeff(Scalar 1.0, "alpha", Draw("Gaussian", [Scalar 0.0; s_large]))
let r2 = anon_noise()
let r3 = Sum(r1,anon_noise())
let r4 = Sum(r1,Sum(uninf_Coeff (Var "x") "beta",anon_noise()))
let r5 = Sum(Cond(r1,Var "s"), anon_noise())
let ralpha = Sum(uninf_Coeff (Scalar 1.0) "a",Sum(uninf_Coeff (Var "u") "b",anon_noise()))
let r6 = Sum(Cond(Coeff (Scalar 1.0, "alpha", ralpha), Var "s"),Sum(uninf_Coeff (Var "x") "beta",anon_noise()))

(*
LHS =
(  1{alpha~1{a~^Gaussian(0,large)} +
           u{b~^Gaussian(0,large)} +
           (nu pi10)0{pi10~^GammaSR(1,small)} +
           ^GaussianMP(0,pi10)
    } | s)
 + x{beta~^Gaussian(0,large)} +
(nu pi11)0{pi11~^GammaSR(1,small)}
+ ^GaussianMP(0,pi11)


RHS =
(nu pi10)(nu alpha12)(nu alpha13)(nu alpha14)(nu alpha15)(nu pi11)
  1{a~^Gaussian(0,large)} +
  (0{alpha12~^().a} | s) +
  (s).u{b~^Gaussian(0,large)} +
  (0{alpha13~^u:().b} | s) +
  0{pi10~^GammaSR(1,small)} +
  (0{alpha14~^0} | s) +
  (1{alpha15~^GaussianMP(0,pi10)} | s) +
  (0{alpha~^alpha12 + ^alpha13 + ^alpha14 + ^alpha15} | s) +
  x{beta~^Gaussian(0,large)} +
  0{pi11~^GammaSR(1,small)} +
  ^GaussianMP(0,pi11)

*)

type N = List<id * List<u>>
type Top =
    | Noise of N   
    | Det of vs:List<u>
type P =
 | Coeff_Cond of v:u * alpha:id * coeff:Top * us:List<u>
type R =
 | ResSum of List<id> * List<P> * N

let sum rs =
  match rs with
  | [] -> Immed(Scalar 0.0)
  | _ -> List.reduce (fun r1 r2 -> Sum(r1,r2)) rs

let cond r us = List.foldBack (fun u r -> Cond(r,u)) r us
let res alphas r = List.foldBack (fun alpha r -> Res(alpha,r)) alphas r

let emptyT t =
    match t with 
    | Noise [] -> true
    | Det [] -> true
    | _ -> false

let r_of_N ns:r = ns |> List.map Draw |> sum
let r_of_Top t :r =
     match t with
     | Noise n -> r_of_N n
     | Det vs -> vs |> List.map Immed |> sum

let r_of_P (Coeff_Cond (v1, alpha, t, us)) =
      let r = if emptyT t 
              then Immed(Scalar 0.0)
              else r_of_Top t
      cond us (Coeff(v1, alpha, r))

let cond_of_P v (Coeff_Cond (v1, alpha, vs, us)) = 
    Coeff_Cond (v1, alpha, vs, v :: us)

let r_of_R (ResSum(alphas, ps, ns)) = 
    (match (ps,ns) with
     | ([],_) -> [r_of_N ns]
     | (_,[]) -> List.map r_of_P ps
     | (_, _) -> (List.map r_of_P ps)@[r_of_N ns])
    |> sum |> res alphas 

let path_comp ws us = List.map (fun u -> Path(ws,u)) us

let norm_coeff ws v0 (Coeff_Cond (v1, beta, vs, us)) =
  // v0{alpha ~ (v1{beta ~ Sigma ^vs} | us)} | ws
  //   == v0:(ws.v1) {beta ~ Sigma ^vs} | ws.us)          
  //    + (0{alpha~^(v1:us.beta)} | ws)
        (Coeff_Cond(simplify(Interaction(v0,Path(ws,v1))),beta,vs,path_comp ws us),
         simplify(Interaction(v1,Path(us,Var beta))))

// assuming that restricted names are distinct throughout
let rec normalize ws r =
  match r with
  | Immed(v) ->
    let alpha = fresh "imm" in
      ResSum([alpha], [Coeff_Cond(v,alpha,Det([Scalar(1.0)]),[])],[])
  | Draw(alpha,vs) ->
      ResSum([], [],[(alpha,vs)])
  | Res(alpha,r) ->
      match normalize ws r with ResSum(alphas,Ps,N) -> ResSum(alpha::alphas,Ps,N)
  | Sum(r1,r2) ->
      match normalize ws r1, normalize ws r2 with
      | ResSum(alphas1,ps1,n1),ResSum(alphas2,ps2,n2) -> ResSum(alphas1@alphas2, ps1@ps2, n1@n2)
  | Cond(r0,w) ->
      normalize (ws @ [w]) r0
  | Coeff(v0,alpha,r0) ->
      match normalize [] r0 with
      | ResSum(alphas,[], N) -> // this case is an optimization, avoiding introducing a new name
        ResSum(alphas, [Coeff_Cond(v0, alpha, Noise N,ws)],[])
      | ResSum(alphas,ps,[]) -> // this case is an optimization, avoiding introducing a new name
        let pss,coeffs = ps |> List.map (norm_coeff ws v0) |> List.unzip
        let r' = Coeff_Cond(Scalar(0.0), alpha, Det coeffs,ws)
        in ResSum(alphas, pss @ [r'],[])
      | ResSum(alphas,ps,N) ->
        let pss,coeffs = ps |> List.map (norm_coeff ws v0) |> List.unzip
        let beta = fresh "noise"
        let n' = Coeff_Cond(v0, beta, Noise N,ws)
        let r' = Coeff_Cond(Scalar(0.0), alpha, Det(coeffs@[Var beta]),ws)
        in ResSum(alphas@[beta], pss @ [n';r'],[])
        
// testing

let test r1 =
  let r2 = r_of_R (normalize [] r1) in
  sprintf "\nBefore: %s\nAfter:  %s\n" (pretty_regression r1) (pretty_regression r2)

let r7 = res ["a";"b";"c"] (Sum (Immed (Scalar 1.0), Immed (Scalar 1.0)))
let r8 = cond [Var "f1"; Var "f2"; Var"f3"] (Immed (Scalar 1.0))

let r10 = Cond(Cond(Coeff (Scalar 1.0, "alpha", uninf_Coeff (Var "u") "b"), Var "s"), Var "t")

test r10

