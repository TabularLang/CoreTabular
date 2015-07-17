namespace MicrosoftResearch.Infer.Tabular

module ParserCLI1 = 
   open System
   open Printf
   open System
   open System.Windows
   open System.Windows.Controls
   open System.Collections.Generic
   open MicrosoftResearch.Infer.Tabular.TaskPane
   open Microsoft.Office.Interop
   open System.Text.RegularExpressions
   open System.Runtime.InteropServices

   module Arguments = 
      type ArgInfo = { Command:string; Description:string; Required:bool }
      let DisplayHelp (defs:ArgInfo list) =
         match defs with
         | [] -> Console.WriteLine "No help text defined."
         | _ ->
               Console.WriteLine "Command Arguments:"
               defs
               |> List.iter (fun def ->
                  let helpText = sprintf "-%s (Required=%b) : %s" def.Command def.Required def.Description
                  Console.WriteLine helpText )
      let DisplayArgs (args:Dictionary<string, string>) =
         match args.Keys.Count with
         | 0 -> Console.WriteLine "No arguments found."
         | _ ->
               Console.WriteLine "Arguments Found:"
               for arg in args.Keys do
                  if String.IsNullOrEmpty(args.[arg]) then
                     Console.WriteLine (sprintf "-%s" arg)
                  else
                     Console.WriteLine (sprintf "-%s '%s'" arg args.[arg])

      // Parse the input arguments
      let ParseArgs (defs:ArgInfo list)  (args:string array)=
         let parsedArgs = new Dictionary<string, string>()
         let fullDefs = 
               if not (List.exists (fun def -> String.Equals(def.Command, "help")) defs) then
                  {ArgInfo.Command="help"; Description="Display Help Text"; Required=false } :: defs
               else
                  defs
         let reportError errorText =        
               DisplayArgs parsedArgs
               DisplayHelp fullDefs
               let errMessage = sprintf "Error occured: %A" errorText
               Console.Error.WriteLine errMessage
               Console.Error.Flush()
               Environment.Exit(1)
         let captureArg command value =
               match defs with
               | [] -> parsedArgs.Add(command, value)
               | _ ->                
                  if not (List.exists (fun def -> String.Equals(def.Command, command)) fullDefs) then
                     reportError (sprintf "Command '%s' Not in definition list." command)
                  else
                     parsedArgs.Add(command, value)            
         let (|IsCommand|_|) (command:string) =            
               let m = Regex.Match(command, "^(?:-{1,2}|\/)(?<command>.*)$", RegexOptions.IgnoreCase)
               if m.Success then Some(m.Groups.["command"].Value.ToLower()) else None

         let rec loop (argList:string list) =
               match argList with
               | [] -> ()
               | head::tail ->
                  match head with
                  | IsCommand command ->
                     match tail with
                     | [] -> captureArg command String.Empty
                     | iHead::iTail -> 
                           match iHead with
                           | IsCommand iCommand ->
                              captureArg command String.Empty
                              loop tail
                           | _ ->
                              captureArg command iHead
                              loop iTail
                  | _ -> reportError (sprintf "Expected a command but got '%s'" head)
         loop (Array.toList args)
         
         // Look to see if help has been requested if not check for required
         if (parsedArgs.ContainsKey("help")) then
               DisplayHelp defs
         else
               defs
               |> List.filter (fun def -> def.Required) 
               |> List.iter ( fun def ->
                  if not (parsedArgs.ContainsKey(def.Command)) then
                     reportError (sprintf "Command '%s' found but in argument list." def.Command))
         parsedArgs

   open Arguments

   let fileCommand = "file"
   let sheetCommand = "sheets"
   let outputCommand = "output"

   let defs = [
         {ArgInfo.Command=fileCommand  ; Description="Workbook to run tabular on"                      ; Required=true  }
         {ArgInfo.Command=sheetCommand ; Description="sheets containing models on which to run tabular"; Required=false } 
         {ArgInfo.Command=outputCommand; Description="output name"                                     ; Required=false }
         ]
   let parseArgs = ParseArgs defs  


module ParserCLI =
  type ArgInfo = { Command:string; Description:string; Required:bool }

  
  module Parsec =
     open System.Collections.Generic
     type token = Car of char
     type Word = token list 

     //not used
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


     type ParseReturn<'a,'rest> = | Success of 'a *('rest list) | Failure of string
     let ($) (a) = function | (Car(b1):: toks) ->  if a=(b1) then Success(a,toks) else Failure (a.ToString()+" expected")
                            | _  ->   Failure "Symbol expected"
     let alpha  = function | (Car(b1):: toks) ->  if System.Char.IsLetterOrDigit(b1) then Success(b1,toks) else Failure ("LetterOrDigit expected") 
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
     let (.--)   ph1 ph2 toks = match ph1 toks with | Success (x,toks2) -> (match ph2 toks2 with | Success (y,toks3) -> Success(x, toks3) | Failure (m) -> Failure (m)) | Failure (m) -> Failure (m)
     let (--.)   ph1 ph2 toks = match ph1 toks with | Success (_,toks2) -> (match ph2 toks2 with | Success (y,toks3) -> Success(y, toks3) | Failure (m) -> Failure (m) )| Failure (m) -> Failure (m)
     let (.--.)  ph1 ph2 toks = match ph1 toks with | Success (x,toks2) -> (match ph2 toks2 with | Success (y,toks3) -> Success((x,y), toks3) | Failure (m) -> Failure (m)) | Failure (m) -> Failure (m)
     let (.--..) ph1 ph2 toks = match ph1 toks with | Success (x,toks2) -> (match ph2 toks2 with | Success (y,toks3) -> Success((x,y), toks3) | Failure (m) -> Failure (m)) | Failure (m) -> Failure (m)
     let empty toks = Success([],toks);
     let one  ph  toks = match ph toks with | Success (x,toks2) -> Success([x],toks2) | Failure (m) -> Failure (m);
     let rec repeat0plus ph toks = ((ph .--..  repeat0plus ph >> List.Cons) |-|. empty ) toks;
     let rec repeat1plus ph toks = ((ph .--..  repeat0plus ph >> List.Cons) |-|. one ph ) toks;
     let finished ph toks = match ph toks with | Success(x, []) -> Success(x,[]) | Success(_, x::_) -> Failure (sprintf "could not parse : %A" x) | _  -> Failure "parsing failure" 
     let inSet (dic:Set<_>) toks = match toks with | (Success(x,toks2)) -> (if dic.Contains x then Success(x, toks2) else Failure (sprintf "%A not found in dictionary" x)) | Failure (m) -> Failure (m)
     let tos (lc:char list) = System.String(lc |> List.toArray)
     let wordMatch ph ltoks = let h,t = match ltoks  with | [] -> [],[] | (h::t) ->  h,t
                              match finished ph h with
                              | Success(x, empty) -> Success(x, t)
                              | Failure(m) -> Failure(m)


   open Parsec
   open Printf
   open System

   let parseArgs (specs:ArgInfo list) = 
      let inCommands = inSet (specs  |> List.map(fun e -> e.Command) |> Set.ofSeq)

      let makedict (keylvalues:(string * string list )  list ) = keylvalues |> Map.ofSeq
      //let rec commands    ltoks = ltoks  |> List.map commanddec |> concat |> (id>> makedict) //>> //(repeat0plus (commanddec) >> makedict) toks 
      let rec commands    (ltoks:Word list) = (repeat0plus (commanddec) >> makedict) ltoks 
      and     cmdDecpart1 (ltoks:Word list) = wordMatch(dashdash --. (command .-- repeat0plus(space))) ltoks 
      and     cmdDecpart2 (ltoks:Word list) = repeat0plus(wordMatch((word .-- repeat0plus(space))))  ltoks 
      and     commanddec  (ltoks:Word list) = (cmdDecpart1 .--.  cmdDecpart2) ltoks 
      and     command     toks = (word >>. inCommands) toks 
      and     dashdash    toks = (($)('-') --. ($)('-') |-| ($)('-')) toks 
      and     word        toks = (repeat1plus(alpha |-| ($)('_') |-| ($)('.') |-| ($)(':') |-| ($)('\\'))  >> tos) toks
      and     space       toks = (($)(' ') |-| ($)(':')|-| ($)('=')) toks

      fun (args: #seq<string> ) -> 
         match commands (scanner args  ) with 
         | Success(dic,_) -> dic
         | _ -> failwith ""

         

   let printUsage (specs:ArgInfo list) = 
      let usage = System.Text.StringBuilder()
      bprintf usage "\nUsage: %s --command value1 value2" AppDomain.CurrentDomain.FriendlyName
      bprintf usage "\n\n"
      for spec in specs do
         bprintf usage "  %s: %s%s \n" (spec.Command.PadRight(10))  spec.Description (if spec.Required then "(required)" else "(optional)")
      usage.ToString()










