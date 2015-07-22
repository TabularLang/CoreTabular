namespace MicrosoftResearch.Infer.Tabular

module SchemaConstants = 
  let saveinpuLabel  = "save input"
  let algorithmLabel  = "algorithm" 
  let iterationsLabel = "iterations"



module SchemaParser =
  open Syntax
  open Parsing
  exception ParseException of string
  exception LexParseException of string


  //for fslexx character level operations
  let parseCol p c s = try p s with | e -> raise (LexParseException (sprintf "column %A : could not parse  %A" c s))
  let parseOther p s = try p s with | e -> raise (LexParseException (sprintf "could not parse  %A" s))

  type token = Id of string * string * string * string * int option
  with override this.ToString() = match this with Id(s1,s2,s3,s4,_) -> sprintf "%s %s %s %s ..." s1 s2 s3 s4 
  let functionPrefix  = "function:"
  let settingsTableName =  "Settings"
  let tableIdLPar, tableIdRPar = @"[", @"]"

  let scanner () =
    let rev l = l |> List.rev
    let rec scanning  = function
       | (toks, []) -> rev toks  
       | (toks, c::cs) -> scanning (Id c :: toks, cs)
    fun (a:_ list) -> scanning([], a)

  type ParseReturn<'a,'rest> = | Success of 'a*('rest list) | Failure of string
  (*Phrase consisting of the keyword 'a' *)
  let ($) f = function
    | (b:: toks) ->  if f b then  Success(b,toks) else Failure (b.ToString()+" not satisfying f")
    | _  ->   Failure "Symbol expected"

  let (>>) ph f toks = 
      match ph toks with
      | (Success(x,toks2)) -> Success(f x, toks2)
      | Failure (m) -> Failure (m)

  (*Alternative phrases*)
  let (|-|) ph1 ph2 toks = match ph1 toks with | Success(_) as ret ->  ret | Failure(_) -> ph2 toks;
  let (!)       ph  toks = match ph toks with | Success(x,toks2) -> Failure (sprintf "not %A expected" x) | Failure(_) -> Success((), toks)
  let (.--) ph1 ph2 toks = 
     match ph1 toks with
     | Success (x,toks2) -> match ph2 toks2 with
                            | Success (y,toks3) -> Success(x, toks3)
                            | Failure (m) -> Failure (m)
     | Failure (m) -> Failure (m)
  let (--.) ph1 ph2 toks = 
     match ph1 toks with
     | Success (_,toks2) -> match ph2 toks2 with
                            | Success (y,toks3) -> Success(y, toks3)
                            | Failure (m) -> Failure (m)
     | Failure (m) -> Failure (m)
  (*Consecutive phrases*)
  let (.--.) ph1 ph2 toks = 
     match ph1 toks with
     | Success (x,toks2) -> match ph2 toks2 with
                            | Success (y,toks3) -> Success((x,y), toks3)
                            | Failure (m) -> Failure (m)
     | Failure (m) -> Failure (m)
  (*The empty phrase!*)
  let empty toks = Success([],toks);
  (*Zero or more phrases*)
  //(repeat ph) is either empty, returning a empty list of 'a, or ph followed by repeat ph, the successful parse of which is Consed
//  let rec repeat ph toks = ((ph .--.  repeat ph >> List.Cons) |-| empty ) toks;

  let rec repeat_aux l ph toks = 
     match ph toks with
     | Success (x,toks2) -> repeat_aux (x::l) ph toks2 
     | Failure (m) -> Success(List.rev l, toks)
  let repeat ph toks = repeat_aux [] ph toks

  (*Check that no tokens remain*)
  let finished ph toks = match ph toks with
                         | Success(x, []) -> Success(x,[])
                         | Success(_, x::_) -> Failure (sprintf "could not parse : %O" x)
                         | f  -> Failure "parsing failure some tokens remain"


  let parse1 p = function | (Id(b1,b2,b3,b4,rg):: toks) ->  try let r = p b1 in Success((b1,r,rg), toks) with | e -> Failure(e.Message) 
                          | _  ->   Failure "Symbol expected"
  let parse2 p = function | (Id(b1,b2,b3,b4,rg):: toks) ->  try let r = p b2 in Success((b2,r,rg), toks) with | e -> Failure(e.Message) 
                          | _  ->   Failure "Symbol expected"
  let parse3 p = function | (Id(b1,b2,b3,b4,rg):: toks) ->  try let r = p b3 in Success((b3,r,rg), toks) with | e -> Failure(e.Message) 
                          | _  ->   Failure "Symbol expected"
  let parse4 p = function | (Id(b1,b2,b3,b4,rg):: toks) ->  try let r = p b4 in Success((b4,r,rg), toks) with | e -> Failure(e.Message) 
                          | _  ->   Failure "Symbol expected"

  let pOption p arg = try Some (p arg) with | e -> None
  let pWorks p arg = match pOption p arg with | Some _ -> true | None -> false
  let pFails p arg = not (pWorks  p arg)

  let parseParallel p1 p2 p3 p4 joinSuccess toks = 
            match parse1 p1 toks, parse2 p2 toks, parse3 p3 toks, parse4 p4 toks with 
             |Success ((s1,r1,rg),rest), Success ((s2,r2,_),_), Success ((s3,r3,_),_), Success ((s4,r4,_),_)  -> 
                  let join = joinSuccess((s1,s2,s3,s4),(r1,r2,r3,r4))
                  if join |> Option.isSome then Success ((join.Value,rg),rest)
                  else Failure ("join condition not satisfied")
             | _ -> Failure("one match fails")

  let emptyline = parseParallel ParseIsEmpty ParseIsEmpty ParseIsEmpty ParseIsEmpty 
                                (fun (_,(a,b,c,d)) ->  if a&&b&&c&&d then Some () else None)
  let tableId   = parseParallel ParseIsEmpty ParseIsEmpty ParseIsEmpty ParseIsEmpty 
                                (fun ((tn,_,_,_),(a,b,c,d)) -> if (not a)&&b&&c&&d && (pFails ParseSettingsTableId tn) then Some tn else None)
  let columnId  = parseParallel ParseIsEmpty ParseIsEmpty ParseIsEmpty ParseIsEmpty 
                                (fun (col,(a,b,c,d)) -> if not a && not b && not c then Some col else None)
  let settingsTableIdent     = parse1 ParseSettingsTableId  

  let settingsAnyValue toks  = match toks with  | (Id(b1,b2,b3,b4,r) :: rest) when not(Parsing.ParseIsEmpty b1) -> Success (((b1,b2,b3,b4,r),rest))
                                                |  toks ->  Failure ("unknown exception")

  type ColData<'T> = string * 'T
      
  let declare_table (tableId, columns) =
    let tableDef  : Table = columns |> List.map fst
    let dictNameToPosition           = columns |> List.map snd |> List.choose id |> Map.ofList
    Declaration(tableId, tableDef), (tableId.Name  , dictNameToPosition)

  let declare_table_name (rawtableName:string,_) =
      let r = try Parsing.ParseTableName rawtableName with | e -> raise (LexParseException (sprintf "could not parse  %A %A" rawtableName e))
      r

  let  make_schema ((tables, settings) : List<_> * List<_>) : Schema * Map<string,Map<string,int>> * ((Map<string,obj>) option )= 
      let (declaration, colMapping2rg) = List.unzip tables
      declaration, colMapping2rg |> Map.ofList, (if settings.IsEmpty then None else Some(settings.Head))
      
        
  let declare_column ((colName:string, colTypeName, markupTypeName, modelString), range:int option) : ColData<_> * ColData<_> option= 
      try 
         let colType = parseCol Parsing.ParseColumnType colName colTypeName
        // let parsed =  parseCol Parsing.ParseMarkup     colName (markupTypeName + "(" +  modelString + ")" )
        // (parseCol Parsing.ParseColumnName colName  colName, {Type = colType; Markup = parsed }) , range |> Option.map (fun r -> colName, r)
         let markupOf =  parseCol Parsing.ParseMarkupOf colName   markupTypeName 
         let model = parseCol Parsing.ParseModel colName modelString
         let markup = markupOf model
         (parseCol Parsing.ParseColumnName colName  colName, {Type = colType; Markup = markup }) , range |> Option.map (fun r -> colName, r)
      with | :? LexParseException as e  -> raise e;
           | e -> failwithf "unknown exception in parsing %A" e

  let declareSettingsTable   (l : List<string * obj>) = l  |> Map.ofSeq 
  let declareSetting         (key:string, value, _, _, _) = key.ToLower(), box value

  let rec schema toks = ((emptyOrComments  --. 
                          (repeat(table .-- separator))   
                           .--.                                                             
                           (repeat(settingsTable .-- separator))
                         )
                         >> make_schema) toks
  and  emptyOrComments toks = (repeat emptyline) toks
  and  separator       toks = (emptyline .-- emptyOrComments) toks //an empty line, followed by optional lines
  and     table        toks = (( tableName .--. repeat (column)) >> declare_table) toks
  and     tableName    toks = ( tableId >> declare_table_name) toks
  and     column       toks = ( columnId >> declare_column) toks
  and  settingsTable   toks = (settingsTableIdent --. repeat settingsCol >> declareSettingsTable) toks
  and  settingsCol     toks = (settingsAnyValue >> declareSetting ) toks

  (*Read a string as a proposition*)
  let readSchema = fun s ->  try
                               match finished schema ( (scanner() s)) with 
                               | (Success((schema, colNameToInt,settings),_)) ->  ("", Some ( schema ), colNameToInt, settings);
                               | Failure(m) -> failwith m
                             with | :? LexParseException as e  -> failwith e.Data0;
                                  | e -> failwith (sprintf "unknown exception in parsing : %A" e.Message)
  let readTable  = fun s -> try 
                               match  finished table ( (scanner() s)) with 
                               | (Success((schema, colNameToInt),_)) -> ("", Some ( schema ), colNameToInt) 
                               | Failure(m) -> failwith m
                            with | e -> failwith "real exception"

  