
(* parse.ML *)
(* Last modified on Tue Feb 28 15:11:43 1995 by Andy Gordon *)
(* "lists.ML" and "datatypes.ML" must be loaded. *)

(**** Lexical Analysis -- Scanning 
  All characters are covered except octal 0-41 (nul-space) 
  and 177 (del), which are ignored. ****)

(*Type of lexical symbols*)
datatype token = Key of string             (*keywords*)
               | Id of string;             (*identifiers*)

(*alphas = list of alphabetic keywords
  symbols = list of symbolic keywords*)
fun scanner (alphas,symbols) =
  let fun is_letter_or_digit c =
	  "A"<=c andalso c<="Z" orelse
	  "a"<=c andalso c<="z" orelse
	  "0"<=c andalso c<="9";

      val specials = explode"!@#$%^&*()+-={}[]:\"|;'\\,./?`_~<>";

      (*scanning of an alphanumeric identifier or keyword*)
      fun alphanum (id, c::cs) =
	    if is_letter_or_digit c then  alphanum (id^c, cs)
				    else  (id, c::cs)
	| alphanum (id, []) = (id, []);

      fun tokenof a = if a mem alphas  then  Key(a)  else  Id(a);

      (*scanning of a symbolic keyword*)
      fun symbolic (sy, c::cs) =
	    if sy mem symbols orelse not (c mem specials)
	    then  (sy, c::cs)
	    else  symbolic (sy^c, cs)
	| symbolic (sy, []) = (sy, []);

      fun scanning (toks, []) = rev toks    (*end of chars*)
	| scanning (toks, c::cs) =
	    if is_letter_or_digit c 
	    then (*identifier or keyword*)
		 let val (id, cs2) = alphanum(c, cs)
		 in  scanning (tokenof id :: toks, cs2)
		 end
	    else if c mem specials
	    then (*special symbol*)
		 let val (sy, cs2) = symbolic(c, cs)
		 in  scanning (Key sy :: toks, cs2)
		 end
	    else (*spaces, line breaks, strange characters are ignored*)
		 scanning (toks, cs);

      (*Scanning a list of characters into a list of tokens*)
      fun scan a = scanning([], explode a)
  in  scan  end;


(**** Parsing functionals ****)

infix 5 --;
infix 3 >>;
infix 0 ||;

exception SynError of string;

(*Phrase consisting of the keyword 'a' *)
fun $a (Key b :: toks) =
      if a=b then (a,toks) else raise SynError (a^" expected")
  | $a _ = raise SynError "Symbol expected";

(*Phrase consisting of an identifier*)
fun id (Id a :: toks) = (a,toks)
  | id toks = raise SynError "Identifier expected";

(*Application of f to the result of a phrase*)
fun (ph>>f) toks = 
    let val (x,toks2) = ph toks
    in  (f x, toks2)  end;

(*Alternative phrases*)
fun (ph1 || ph2) toks = ph1 toks 
			handle SynError _ => ph2 toks;

(*Consecutive phrases*)
fun (ph1 -- ph2) toks = 
    let val (x,toks2) = ph1 toks
	val (y,toks3) = ph2 toks2
    in  ((x,y), toks3)  end;

(*The empty phrase!*)
fun empty toks = ([],toks);

(*Zero or more phrases*)
fun repeat ph toks = (   ph -- repeat ph >> (op::)
		      || empty   ) toks;

(*Check that no tokens remain*)
fun finished ph toks = 
     (case ph toks of 
	  (x, []) => x
	| (_, _::_) => raise SynError "Extra characters in phrase");


(*The connectives & and | will associate to the LEFT.*)
fun make_prop (p, []) = p
  | make_prop (p, (_,q)::pairs) = make_prop(Disj(p,q), pairs);

fun make_term (p, []) = p
  | make_term (p, (_,q)::pairs) = make_term(Conj(p,q), pairs);

(*Grammar:
    Prop    =  Term {"|" Term}
    Term    =  Factor {"&" Factor}
    Factor  =  Id  |  "~" Factor  |  "(" Prop ")"
*)
fun prop toks =
    (   term -- repeat ($"|" -- term) >> make_prop) toks
and term toks =
    (   factor -- repeat ($"&" -- factor) >> make_term) toks
and factor toks =
    (   id                   >> Atom
     || $"~" -- factor       >> (fn (_,p) => Neg p)
     || $"(" -- prop -- $")" >> (fn ((_,p),_) => p)
    ) toks;

(*Read a string as a proposition*)
val read = finished prop o (scanner([], explode"()~&|"));

read"~a & b | ~(c & d) & (e | ~f)";
show it;