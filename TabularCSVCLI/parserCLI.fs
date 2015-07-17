namespace MicrosoftResearch.Infer.Tabular

module ParserCLI = 
    open System
    open Printf
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open System.Runtime.InteropServices

    type NumberOfParameters = | Variable | Fixed of int
    type CLIArg = { Command:string; Description:string; NumberOfParameter: NumberOfParameters; Required:bool }
    
    [<AutoOpen>]
    module Parsec =
        open System.Collections.Generic
        type token = Car of char
        type Word = token list 
        type ParseReturn<'a,'rest> = | Success of 'a *('rest list) | Failure of string

        let carScanner : char list -> Word list =
            let rec scanning  = function
                | (toks, cur, []) ->  (cur|> List.rev)::toks  |> List.rev
                | (toks, cur, ' '::cs) -> scanning ( (cur |> List.rev)  :: toks, [],  cs)
                | (toks, cur, c::cs) -> scanning (toks,(Car c)::cur, cs)
            fun (a:_ list)  -> scanning([], [], a) |> List.filter(fun word -> word.Length > 0)
     
        let scanner : #seq<string>  -> Word list =
            let rec scanning  = function
                | (toks, []) ->  toks  |> List.rev
                | (toks, s::ss) -> scanning ((s|> List.ofSeq |> List.map Car) :: toks, ss)
            fun (a: #seq<string> )  -> scanning([], a |> Seq.toList) |> List.filter(fun word -> word.Length > 0)
        let ($) (a) = function | (Car(b1):: toks) ->  if a=(b1) then Success(a,toks) else Failure (a.ToString()+" expected")
                               | _  ->   Failure "Symbol expected"
        let alpha   = function | (Car(b1):: toks) ->  if System.Char.IsLetterOrDigit(b1) then Success(b1,toks) else Failure ("LetterOrDigit expected") 
                               | _  ->   Failure "Symbol expected"
        let concat ltoks   = 
                ltoks |> List.fold (fun previous toks -> match toks with 
                                                            | (Success(x,toks2)) -> match previous with | Success(px,ptoks) -> Success(x::px,toks2::ptoks) 
                                                                                                        | Failure (m) -> Failure (m)
                                                            | Failure (m) -> Failure (m)) (Success([],[]))
        let (>>) ph f toks      = match ph  toks with | (Success(x,toks2)) -> Success(f x, toks2) | Failure (m) -> Failure (m)
        let (>>.) ph f toks     = ph toks |> f
        let (|-|) ph1 ph2 toks  = match ph1 toks with  | Success(_) as ret ->  ret | Failure(_) -> ph2 toks;
        let (|-|.) ph1 ph2 toks  = match ph1 toks with  | Success(_) as ret ->  ret | Failure(_) -> ph2 toks;
        let (!) ph toks         = match ph  toks with | Success(x,toks2) -> Failure (sprintf "not %A expected" x) | Failure(_) -> Success((), toks)
        let (.--)   ph1 ph2 toks  = match ph1 toks with | Success (x,toks2) -> (match ph2 toks2 with | Success (y,toks3) -> Success(x, toks3)     | Failure (m) -> Failure (m)) | Failure (m) -> Failure (m)
        let (--.)   ph1 ph2 toks  = match ph1 toks with | Success (_,toks2) -> (match ph2 toks2 with | Success (y,toks3) -> Success(y, toks3)     | Failure (m) -> Failure (m) )| Failure (m) -> Failure (m)
        let (.--.)  ph1 ph2 toks  = match ph1 toks with | Success (x,toks2) -> (match ph2 toks2 with | Success (y,toks3) -> Success((x,y), toks3) | Failure (m) -> Failure (m)) | Failure (m) -> Failure (m)
        let (.--..) ph1 ph2 toks  = match ph1 toks with | Success (x,toks2) -> (match ph2 toks2 with | Success (y,toks3) -> Success((x,y), toks3) | Failure (m) -> Failure (m)) | Failure (m) -> Failure (m)
        //dependant parser
        let (.---.)  ph1 fph2 toks = match ph1 toks with | Success (x,toks2) -> (match (fph2 x) toks2 with | Success (y,toks3) -> Success((x,y), toks3) | Failure (m) -> Failure (m)) | Failure (m) -> Failure (m)
        let empty toks = Success([],toks);
        let one  ph  toks = match ph toks with | Success (x,toks2) -> Success([x],toks2) | Failure (m) -> Failure (m);
        let rec repeat0plus ph toks = ((ph .--..  repeat0plus ph >> List.Cons) |-|.  empty ) toks;
        let rec repeat1plus ph toks = ((ph .--..  repeat0plus ph >> List.Cons) |-|. one ph ) toks;
        let rec repeatn  n  ph toks = if n = 0 then empty toks else ((ph .--..  repeatn (n-1) ph >> List.Cons)) toks;
        let finished ph toks = match ph toks with | Success(x, []) -> Success(x,[]) | Success(_, x::_) -> Failure (sprintf "could not parse : %A" x) | _  -> Failure "parsing failure" 
        let inSet (dic:Set<_>) toks = match toks with | (Success(x,toks2)) -> (if dic.Contains x then Success(x, toks2) else Failure (sprintf "%A not found in dictionary" x)) | Failure (m) -> Failure (m)
        let followedBy ph fph toks  = match toks with | (Success(x,toks2)) ->  match (fph x) toks2 with      | Failure (m) -> Failure (m)
        let tos (lc:char list) = System.String(lc |> List.toArray)
        let wtos (w:Word) = System.String(w |> List.map (fun (Car c) -> c) |> List.toArray)
        let wordMatch ph ltoks = let h,t = match ltoks  with | [] -> [],[] | (h::t) ->  h,t
                                 match finished ph h with
                                 | Success(x, empty) -> Success(x, t)
                                 | Failure(m) -> Failure(m)

    let parseArgs (specs:CLIArg list) = 
      let getSpec name = specs |> List.find(fun s -> s.Command = name)
      let inCommands = inSet (specs  |> List.map(fun e -> e.Command) |> Set.ofSeq)
      let makedict (keylvalues:(string * string list )  list ) = keylvalues |> Map.ofSeq
      //let rec commands    ltoks = ltoks  |> List.map commanddec |> concat |> (id>> makedict) //>> //(repeat0plus (commanddec) >> makedict) toks 
      let rec commands    (ltoks:Word list) = (repeat0plus commanddec >> makedict) ltoks 
      and     cmdDecpart1 (ltoks:Word list) = wordMatch(dashdash --. (command .-- repeat0plus(space))) ltoks 
      and     cmdDecpart2 cmd (ltoks:Word list) = match (getSpec cmd).NumberOfParameter with 
                                                    | Variable ->  repeat0plus wordspace ltoks  
                                                    | Fixed n  ->  try
                                                                      let args = ltoks |> Seq.take n |> Seq.toList |> List.map wtos
                                                                      Success(args, ltoks |> Seq.skip n |> Seq.toList)
                                                                   with | e ->
                                                                      Failure (sprintf "command %s expects %A arguments" cmd  ((getSpec cmd).NumberOfParameter))
                                                                       
      and     commanddec  (ltoks:Word list) = (cmdDecpart1 .---.  cmdDecpart2) ltoks 
      and     command     toks = (word >>. inCommands) toks 
      and     wordspace   toks = wordMatch(word .-- repeat0plus(space)) toks
      and     take  n     toks = failwith ""
      and     dashdash    toks = (($)('-') --. ($)('-') |-| ($)('-')) toks 
      and     word        toks = (repeat1plus(alpha |-| ($)('_') |-| ($)('-') |-| ($)('.') |-| ($)(':') |-| ($)('\\'))  >> tos) toks
      and     space       toks = (($)(' ') |-| ($)(':')|-| ($)('=')) toks
      fun (args: #seq<string> ) -> 
         match commands (scanner args) with 
         | Success(dic,_) -> dic
         | _ -> failwith ""
    let flip f a b = f b a
    //TODO rewrite help and parseArgs to deal with argument numbers
    let printUsage (specs:CLIArg list) = 
      let print2D (startTxt:string) (txt:string) size = 
            let words =  txt.Split([|' '|])
            let lines = words |> Array.fold(flip (fun e  -> function | []     -> (e::[])
                                                                     | (h::ss) -> let candidate = h + " " + e
                                                                                  if candidate.Length > size then
                                                                                       (e::h::ss)
                                                                                  else (candidate::ss)))
                                           []
                                |> List.rev
            startTxt + (lines |> List.reduce (fun a b  -> a + "\n" +
                                                          b.PadRight(size)  //
                                                           .PadLeft(startTxt.Length + size)))

      let usage = System.Text.StringBuilder()
      bprintf usage "\nUsage: %s --setting value1 .. valueN" AppDomain.CurrentDomain.FriendlyName
      bprintf usage "\n\n"
      bprintf usage "Available settings"
      bprintf usage "\n"

      let padcmd = specs |> List.map(fun e -> e.Command.Length) |> List.max
      for spec in specs do
         //let l = spec.Description.Length / (80-12-8-6)
         let cmd = 
            let start = sprintf "  %s :%s %s:" 
                                (spec.Command.PadRight(padcmd ))  
                                (if spec.Required then " req. " else "(opt.)") 
                                ((match spec.NumberOfParameter with 
                                    | Variable -> "args" 
                                    | Fixed n -> if n = 0 then ("     ".PadRight(8)) else n.ToString() + " param" + if n > 1 then "s" else "").PadRight(8))
            print2D start spec.Description (80-12-8-15)
         bprintf usage "%s \n" cmd
      usage.ToString()










