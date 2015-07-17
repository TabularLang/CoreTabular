(*Type of lexical symbols*)
type token = Key of string   | Id of string 

let explode (st:string) =   st.ToCharArray() |> List.ofArray
let explodes (st:string) =   explode st |> List.map (fun e -> e.ToString())

let scanner (alphas:string list,symbols) =
  let mem x xs = List.tryFind((=) x) xs |> Option.isSome
  let rev l = l |> List.rev

  let is_letter_or_digit c =
    'A'<=c &&  c<='Z' ||
    'a'<=c  && c<='z' ||
    '0'<=c  && c<='9';

  let specials =  explode ("!@#$%^&*()+-={}[]:\"|;'\\,./?`_~<>")

   (*scanning of an alphanumeric identifier or keyword*)
  let rec alphanum = function 
     |  (id, c::cs) ->  if is_letter_or_digit c then  alphanum (id+(c.ToString()), cs) else  (id, c::cs)
     |  (id, [])    ->  (id, []);

  let tokenof a = if  mem a alphas  then  Key(a.ToString())  else  Id(a.ToString());
  let rec symbolic  = function
  | (sy, c::cs) -> if mem sy symbols || not (mem c specials) then  (sy, c::cs) else  symbolic (sy+(c.ToString()), cs)
  | (sy, [])    -> (sy, []);

  let rec scanning  = function
     | (toks, []) -> rev toks    (*end of chars*)
     |  (toks, c::cs) -> if is_letter_or_digit c  then  let (id, cs2) = alphanum(c.ToString(), cs)
                                                        scanning (tokenof id :: toks, cs2)
                         else if mem c specials then 
                                   let  (sy, cs2) = symbolic(c.ToString(), cs)
                                   scanning (Key sy :: toks, cs2)
                              else
                                   scanning (toks, cs);

                        (*Scanning a list of characters into a list of tokens*)
  fun (a:string) -> scanning([], explode a)



(**** Parsing functionals ****)

//infix 5 --;
//infix 3 >>;
//infix 0 ||;


(*Phrase consisting of the keyword 'a' *)
let ($) a = function
  | (Key b :: toks) ->  if a=b then (a,toks) else failwith (a^" expected")
  | _  ->  failwith "Symbol expected";

(*Phrase consisting of an identifier*)
let id = function 
  | (Id a :: toks) -> (a,toks)
  |  toks ->  failwith "Identifier expected";

(*Application of f to the result of a phrase*)
let (>>) ph f toks = 
    let (x,toks2) = ph toks
    (f x, toks2)

(*Alternative phrases*)
let (||) ph1 ph2 toks = try ph1 toks 
                        with | e -> ph2 toks;

(*Consecutive phrases*)
let (--) ph1 ph2 toks = 
   let (x,toks2) = ph1 toks
   let (y,toks3) = ph2 toks2
   ((x,y), toks3)

(*The empty phrase!*)
let empty toks = ([],toks);

(*Zero or more phrases*)
let rec repeat ph toks = (   ph -- repeat ph >> (List.Cons) || empty   ) toks;

(*Check that no tokens remain*)
let finished ph toks = match ph toks with
                       | (x, []) -> x
                       | (_, _::_) -> failwith "Extra characters in phrase";

type prop  =  Atom of string
            | Neg of prop
            | Conj of prop * prop
            | Disj of prop * prop

(*The connectives & and | will associate to the LEFT.*)
let rec make_prop = function
  | (p, []) -> p
  | (p, (_,q)::pairs) -> make_prop(Disj(p,q), pairs);

let rec make_term = function
  | (p, []) -> p
  | (p, (_,q)::pairs) -> make_term(Conj(p,q), pairs);

(*Grammar:
    Prop    =  Term {"|" Term}
    Term    =  Factor {"&" Factor}
    Factor  =  Id  |  "~" Factor  |  "(" Prop ")"
*)
let rec prop toks =
    (   term -- repeat (($) @"|" -- term) >> make_prop) toks
and term toks =
    (   factor -- repeat (($)"&" -- factor) >> make_term) toks
and factor toks =
    (   id                   >> Atom
     || ($)"~" -- factor       >> (fun (_,p) -> Neg p)
     || ($)"(" -- prop -- ($)")" >> (fun ((_,p),_) -> p)
    ) toks;

(*Read a string as a proposition*)
let read = fun s -> finished prop ( (scanner([], explodes "()~&|") s));

let a = read"~a & b | ~(c & d) & (e | ~f)";


//let b = read  [  ("cell1","cell2","cell3","cell4" )
//                 ("cell1","cell2","cell3","cell4" )
//                 ("cell1","cell2","cell3","cell4" )
//                 ("cell1","cell2","cell3","cell4" ) ]