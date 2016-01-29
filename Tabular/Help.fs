module Help

//TODO construct from code as far as possible
let help =
 """
Syntax:
----------------------------------------------
Identifiers (alphanumeric):
Attributes c,x,... 
Tables     t,...
Functions  f,...

(Escaped identifiers:  #"some string")

literals: true,false, 0.0, 0, "a string"
comments: (* a comment *)

Types:
T = int
  | bool
  | real
  | upto(n)           (* integers in [0,n) *)
  | mod(n)            (* same as upto(n) *)
  | vector            (* vector of reals *)
  | PositiveDefiniteMatrix 
                      (* a positive definite matrix *)
  | T[e]              (* T array of length e *)
  | link(t)           (* abbreviates upto(SizeOf(t)) *)
  | T ! spc           (* type in space spc *)

spc := det | rnd | qry  (* spaces *)

Primitives:
P := 
  | Logistic          (* (real) -> real *)
  | Probit            (* (real,real) -> real *)
  | Sum               (* (real[n]) -> real *)
  | Softmax           (* (real[n]) -> vector *)
  | DiagonalPDMatrix  (* (real[n]) -> PositiveDefiniteMatrix *)
  | IdentityScaledBy  (* (int,real[n]) -> PositiveDefiniteMatrix *)
  | InnerProduct      (* (vector,vector) -> real *)
  | Log               (* (real) -> real *)
  | VectorFromArray   (* (real[n]) -> vector *)
  | Exp               (* (real) -> real *)
  | ArgMin            (* (real[n]) -> upto(n) *)
  | ArgMax            (* (real[n]) -> upto(n) *)
  | BreakSymmetry     (* (T) -> T *)
  | DampBackward      (* (real,real) -> real *)
  | #ident            (* (T1,..,TN) -> T *)
                      (* #ident is an unchecked, trusted call to Infer.NET
                         Variable.ident() *)
Index Expressions:
e :=
  | c                 (* attribute (ie. variable) name *) 
  | i                 (* integer literal i > 0 *)
  | SizeOf(t)         (* SizeOf (previously declared) table t *)

Expressions:
E :=
  | e                    (* index expression e *)
  | l                    (* literal *)
  | t.c                  (* parameter c of table t)
  | E.c		             (* attribute of key E *)
  | [E1,...,EN]          (* array literal *)
  | [for c < e -> E]     (* array construction, c bound in E *) 
  | E1.[E2]              (* index array E1 by index E2 *)
  | if E then E1 else E2 (* conditional expression *)
  | E1 op E2             (* operation: op is +,-,*,/, mod, max *)
  | - E1                 (* unary minus *)
  | ! E1                 (* boolean negation *)
  | E1 rel E2            (* comparison: rel in  =,<>,<,<=,>,>= *)
  | D(e1,en,E1,...,EM)   (* random draw from distribution D *)
  | P(E1,...,EN)         (* primitive function *)
  | E : T                (* type constraint *)
  | let c = E1 in E2     (* local definition, c bound in E2)
  | infer.D.p(c)
  | infer.D[e1,...,en].p(c)
                         (* Property p of D / D[e1,...,en] distribution
                            inferred for attribute c 
                            eg. infer.Bernoulli.Bias(coin)
                            and inder.Discrete[6].Probs(die) *)
  | ( E )                (* parenthesized expression *)

Properties (of Distributions):
p := Mean|Variance|Precision|StdDeviation|Rate|Scale|Shape|alpha|beta|probTrue|Bias|trueCount|falseCount|Mode|Median|...


Models:
M :=
  | E	                 (* expression *)
  | M[E]                 (* indexed model with implicit bound *)
  | M[E < e]             (* indexed model with explicit bound *)
  | f(c1=E1,...,cn=En)   (* function call *)
  | ~ r                  (* regression *)

Predictors:
p :=
  | c                    (* attribute name  *)
  | l                    (* scalar literal, integer or float typed as real*)
  | p1 : p2              (* multiplicative interaction *)
  | ( p1,...,pn).p       (* path *)

Regressions:
r :=
  (alpha and pi additionally range over attribute names c denoting parameters *)
  | r1 + r2              (* sum of regressions *)
  | p                    (* sugar: predictor with implicit coefficent with default prior *)
  | p{alpha}             (* sugar: predictor with explicit coefficent named alpha with default prior *)
  | p{alpha~r}           (* predictor with explicit coefficent named alpha given by nested regression *)
  | 'p                   (* coefficent-less, immediate predictor *)
  | (r | p)              (* regression grouped by (discrete) predictor p *)
  | D(p1,...,pn)         (* draw / explicit noise *)
  | ? 	                 (* sugar: default noise *)
  | ?{pi}                (* sugar: named noise *)
  | ?{pi~r}              (* sugar: named noise with precision r *)
  | new pi . r           (* restriction *)
  | (r)                  (* parenthesized regression *)

Columns:
col := c T  input        (* concrete input or 
                            mandatory function parameter *)
    |  c T  hyper E      (* shared attribute or
                            optional function parameter 
                            with default, deterministic value E *)
    |  c T  param M      (* shared attribute of table M *)
    |  c T  latent M     (* latent attributes of table M *)
    |  c T  output M     (* observable attributes of table M *)

Tables:
Table  :=
       col1                     (* vertical columns *)
       ...
       coln

Declarations:
Declaration :=
      | t               (* table with infered key attribute *)
        Table
      | t[c]            (* table with specified key attribute c *)
        Table
      | function f      (* definition of function f *)
        Table

Schema :=
     Declaration1
     ...
     DeclarationN
"""


 