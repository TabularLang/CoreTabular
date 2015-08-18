namespace MicrosoftResearch.Infer.Tabular

open System.Runtime.CompilerServices
open System.Collections
type Vector =  MicrosoftResearch.Infer.Maths.Vector
type Matrix =  MicrosoftResearch.Infer.Maths.Matrix

module Log = 
  type LogValue = | Info of string | Warning of string| Error of string
   with override x.ToString() = match x with | Info    (m) -> sprintf "Info %A" m  
                                             | Warning (m) -> sprintf "Warning %A" m  
                                             | Error   (m) -> sprintf "Error %A" m  
        member  x.map f = match x with 
                           | Info    (m) -> Info    (f m)
                           | Warning (m) -> Warning (f m)
                           | Error   (m) -> Error   (f m) 

[<AutoOpen>]
module StringToInferNet = 
    open Syntax
    open System

    type dataNormalized = Map<ColumnName,int> * (obj array) seq (*all the ids are positional, we don't need them to be explicit*)

    type DTO     = DTO of Map<TableName, dataNormalized>
    type KnowDTO = KnowDTO of Map<TableName, Map<ColumnName,int> * (obj array)>     //table level DTO for posterior
    type DistDTO = DistDTO of Map<TableName, Map<ColumnName,int> * (obj array seq)> //row level DTO

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DistDTO = 
      let getTableData name (DistDTO dto) = dto.[name]

    [<Extension>]
    type FunctionExtension() =
         [<Extension>]
         static member inline Item(DistDTO dto, name) = dto.[name]
         [<Extension>]
         static member inline Item(DTO dto, name) = dto.[name]


    let findIDHeuristic tableName columnSet (suppliedID: ID option) (potentialKeyFromRelation : string option) = 
      let ps = System.Data.Entity.Design.PluralizationServices.PluralizationService.CreateService(System.Globalization.CultureInfo.GetCultureInfo("en-us"));
      let unique = List.fold (fun l e -> if l |> List.tryFind(fun x -> x = e) |> Option.isNone then e::l else l)[]
      let potentialIds = 
           let singularName = ps.Singularize(tableName)
           [  Some "ID";
              Some "id";
              Some (singularName);
              potentialKeyFromRelation
              Some (singularName.ToLower())
              Some (singularName + " ID") ] 
           |> List.choose id |> unique

      let isPresent arg =  columnSet |> Array.tryFind(fun e -> e = arg) |> Option.isSome
      let candidates, validIds = 
         let candidates = if suppliedID.IsNone then potentialIds  else let (ColumnName n) = suppliedID.Value  in  [ n ] 
         candidates, candidates |> List.rev |> List.filter isPresent
      match validIds.Length, suppliedID with 
      | 0, Some v -> failwith (sprintf "in table %A, could not find the column %A" tableName (let (ColumnName n) = v in n))
      | 0, _      -> FromPosition            , [ Log.Info (sprintf "the id  = from position, candidates where %A, none detected" candidates)]  //defaulting to position
      | 1, _      -> FromColumn validIds.Head, [ Log.Info ("the id  = column " + validIds.Head) ] 
      | _, _      -> FromColumn validIds.Head, [ Log.Warning (sprintf "found conflicting candidates for IDs : %A \n id chosen : %A " candidates (candidates  |> List.head))]


module TypedDTO = 
  open MicrosoftResearch.Infer.Tabular.Syntax
  open System
  open System.Linq

  let foldi fold first = List.fold(fun (prev,i) c -> (fold i prev c, i + 1)) (first,0)

  type ColValue () = class end
  and [<AbstractClass>] Instance () = 
      inherit ColValue()
      abstract member get_Item:int -> obj
      abstract member get_NonNullIndices : int []
      abstract member get_NonNullValues : System.Array 
  and NonNullableInstance<'T> (v:'T[]) = 
      inherit Instance() 
      override this.get_Item i = box (v.[i])
      override this.get_NonNullIndices = [| v.GetLowerBound(0) .. v.GetUpperBound(0) |]
      override this.get_NonNullValues = v  :> System.Array
      //member this.v = v
  and NullableInstance<'T> (v:'T option[]) = 
      inherit Instance() 
      let _NonNullIndices, _NonNullValues = v |> Array.mapi(fun i e -> Option.map (fun v -> (i,v)) e ) |> Array.choose id |> Array.unzip

      override this.get_Item i = box (let ov = v.[i] in if ov.IsSome then ov.Value else Unchecked.defaultof<'T>)
      override this.get_NonNullIndices = _NonNullIndices
      override this.get_NonNullValues = _NonNullValues  :> System.Array
      member this.v = v
  and DistributionInstance<'T> (v:System.Array) = 
      inherit Instance() 
      override this.get_Item i = v.GetValue i
      override this.get_NonNullIndices = v.Cast() |>  Seq.mapi(fun i d -> if d = null then None else Some i) |> Seq.choose id |> Seq.toArray
      override this.get_NonNullValues =  v  //:> System.Array
      member this.v = v
  and [<AbstractClass>] Static () = 
      inherit ColValue()
      abstract member Value:unit -> obj
  and Static<'T> (v:'T)     = 
      inherit Static()
      override this.Value() = box v
      member this.v = v

 



  type ReadReturn<'x> = | Success of 'x * string list (*log*)
                        | Failure of string
  
 

  //IDs exist in 2 forms : as part of a table, and as part of a foreign reference


  [<AbstractClass>]
  type Converter<'Src> () = 
    abstract member convertReal      : 'Src -> real
    abstract member convertInt       : 'Src -> int
    abstract member convertBool      : 'Src -> bool
    abstract member convertString    : 'Src -> string
    abstract member convertOption    : ('Src -> 'T) ->  'Src -> 'T option
    member this.convertInstance<'T>  (f:('Src -> 'T)) (s:'Src[]) = Array.map f s
    abstract member convertArray<'T> :  ('Src -> 'T) -> 'Src -> 'T[]
  type ObjConverter ()=
       inherit Converter<obj>()
       override this.convertReal   o = System.Convert.ChangeType( o, typeof<real> ) :?> real
       override this.convertInt    o = System.Convert.ChangeType( o, typeof<int> ) :?> int
       override this.convertBool   o = System.Convert.ChangeType( o, typeof<bool> ) :?> bool
       override this.convertString o = System.Convert.ChangeType( o, typeof<string> ) :?> string
       override this.convertOption c o = if o = null then None else Some (c o)
       override this.convertArray<'T> f s = let es =  s :?> System.Array in 
                                            [| for e in es ->  System.Convert.ChangeType(e , typeof<'T> ) :?> 'T  |] 
  type CSVConverter ()=
       inherit Converter<string>()
       override this.convertReal  s     = System.Convert.ToDouble(s)
       override this.convertInt    s    = System.Convert.ToInt32(s)
       override this.convertBool    s   = System.Convert.ToBoolean(s)
       override this.convertString    s   = System.Convert.ToString(s)
       override this.convertOption c o = if o = null || o = "" then None else Some (c o)
       override this.convertArray  f ss = failwith "_" // need a parser
  type SexpConverter =
       inherit Converter<SExp.Sexp>
       override this.convertReal   arg    = match arg with | (SExp.Atom s)  -> System.Convert.ToDouble(s) | _ -> failwith ""
       override this.convertInt    arg    = match arg with | (SExp.Atom s)  -> System.Convert.ToInt32(s)  | _ -> failwith ""
       override this.convertBool    arg    = match arg with | (SExp.Atom s)  -> System.Convert.ToBoolean(s)  | _ -> failwith ""
       override this.convertString    arg    = match arg with | (SExp.Atom s)  -> System.Convert.ToString(s)  | _ -> failwith ""
       override this.convertOption c o = match o with | SExp.Atom "null" -> None | _  -> Some (c o) // maybe
       override this.convertArray  f arg  = match arg with | (SExp.List ss) -> [| for s in ss -> f s |]   | _ -> failwith ""

  
  type RepVisitor<'Result>  =
       abstract CaseIntRep: IntRep-> 'Result
       abstract CaseArrayRep<'T> : ArrayRep<'T> -> 'Result
       abstract CaseUpToRep: UptoRep->'Result
       abstract CaseUpToSizeRep: UptoSizeRep ->'Result
       abstract CaseBoolRep: BoolRep->'Result
       abstract CaseStringRep: StringRep->'Result
       abstract CaseRealRep: RealRep->'Result
       abstract CaseGenericIComparableRep: GenericIComparableRep ->'Result
       abstract CaseGenericToStringRep: GenericToStringRep ->'Result
       abstract CaseVectorRep: VectorRep ->'Result
       abstract CaseMatrixRep: MatrixRep ->'Result
  and RepOpen<'Result>  =
       abstract Case<'T> : Rep<'T> ->'Result

  //we have :
  // - the coltype in the schema, the underlying dotnet type (fed ultimately to infer.net)
  // - the 'Src type which is an input to the converter
  // - the Representation translates the coltype from the 'src type to the dotnet type 
  and [<AbstractClass>] Rep () = 
       abstract member Visit<'Result> : RepVisitor<'Result> -> 'Result  //uniform in return type
       abstract member Open<'Result> : RepOpen<'Result> -> 'Result  //uniforme in return type
       abstract member ConvertIComparable<'src> : Converter<'src> -> 'src -> IComparable 
       abstract member AddRank: int -> Rep
  //'T is the ultimate dotnet type
  and [<AbstractClass>] Rep<'T> () = 
        inherit Rep() 
        override this.Open<'Result> (ro:RepOpen<'Result>) = ro.Case<'T>(this) 
        abstract member Convert<'src> : Converter<'src> -> 'src -> 'T  //varying in return type
        member this.ConvertOption<'src>(c: Converter<'src>)(s:'src)  =
                c.convertOption (this.Convert c) s
        override this.ConvertIComparable c s = box ( this.Convert c s) :?> IComparable
        override this.AddRank n = new ArrayRep<'T>(this,n):>Rep<'T[]> :> Rep
  and IntRep() = 
      inherit Rep<int>()
      override this.Convert c s = c.convertInt s
      override this.Visit visitor = visitor.CaseIntRep this
  and ArrayRep<'T> (elementRep:Rep<'T>, n:int) =
      inherit Rep<'T[]>()
      member this.ElementRep = elementRep;
      override this.Convert c  s = c.convertArray (elementRep.Convert c ) s
      override this.Visit visitor = visitor.CaseArrayRep this
  and UptoRep (n) = // e is Const i or SizeOf tn
      inherit Rep<int>()
      override this.Convert c  s = let v = c.convertInt s
                                   if v < n then v else failwith (sprintf "%A : expected value in expected value in [0, %A]" v n)
      override this.Visit visitor = visitor.CaseUpToRep this
  and UptoSizeRep (concreteRep:Rep, map: Map<IComparable,int>) = //rep is the foreign id representation
      inherit Rep<int>()
      let inverse = Map.fold (fun (a:IComparable[]) k i -> a.[i] <- k;a ) (Array.create map.Count null) map in 
      member this.ConcreteRep = concreteRep
      member this.Inverse i = inverse.[i]
      override this.Convert c  s = let ic = concreteRep.ConvertIComparable c  s //we use the rep to convert to IComparable, then the dictionary to convert to int
                                   if map.ContainsKey ic then 
                                       map.[ic] 
                                   else failwith (sprintf "key %A not in the domain of %A" ic "")
      override this.Visit visitor = visitor.CaseUpToSizeRep this
  and RealRep () = 
      inherit Rep<real>()
      override this.Convert c s =  c.convertReal s
      override this.Visit visitor = visitor.CaseRealRep this
  and BoolRep () = 
      inherit Rep<bool>()
      override this.Convert c s = c.convertBool s
      override this.Visit visitor = visitor.CaseBoolRep this
  and StringRep () = 
      inherit Rep<string>()
      override this.Convert c s = c.convertString s
      override this.Visit visitor = visitor.CaseStringRep this
  
  and GenericIComparableRep ((*tableName:TableName*)) = 
      inherit Rep<IComparable>()
      override this.Convert c s = box s  :?> IComparable  //point 1 : we don't use the converter as we don't have any prefered type. we could call point 1
      override this.Visit visitor = visitor.CaseGenericIComparableRep this
  and GenericToStringRep () = 
      inherit Rep<IComparable> ()
      override this.Convert c s = box (s.ToString())  :?> IComparable  //point 1 : we dont use the converter as we dont have any prefered type. we could call point 1
      override this.Visit visitor = visitor.CaseGenericToStringRep this
  and VectorRep() =
      inherit Rep<Vector>()
      override this.Convert c s = raise (System.NotImplementedException("VectorRep"))
      override this.Visit visitor = visitor.CaseVectorRep this
  and MatrixRep() =
      inherit Rep<Matrix>()
      override this.Convert c s = raise (System.NotImplementedException("VectorRep"))
      override this.Visit visitor = visitor.CaseMatrixRep this
          
  type DataBase = Map<TableName, int * Map<ColumnName,ColValue> * Rep * Map<IComparable, int>>

  let mkStatic (converter: Converter<'Src>) (rep:Rep) (src:'Src) : Static = 
     (rep).Open { new RepOpen<Static> with member this.Case<'T>(r) = Static<'T> (r.Convert converter src) :> Static }
  let mkInstance (converter: Converter<'Src>) (rep:Rep)  (src:'Src[]) : Instance  = 
      (rep).Open { new RepOpen<Instance> with member this.Case<'T>(r) = NonNullableInstance<'T> (converter.convertInstance  (r.Convert converter) src) :> Instance }
 
  let mkNullableInstance (converter: Converter<'Src>) (rep:Rep)  (src:'Src []) : Instance  = 
      (rep).Open { new RepOpen<Instance> with member this.Case<'T>(r) = NullableInstance<'T> (converter.convertInstance  (r.ConvertOption converter) src) :> Instance }

  /// Across function call to get data, the same ID ordering has to be preserved....
  type DataSource<'Src> = TableName -> int *  (ColumnName -> 'Src[])

  let genericIComparableRep = GenericIComparableRep() :> Rep

  /// ColumnType -> env -> Rep 
  /// when refering to another table, we can use any encoding. This will neeed to be translated to an IComparable with the help of a rep (type directed translation)
  /// The rep itself stores the mapping;
  let rec rep (idReps : Map<TableName,  Rep>, idMap : Map<TableName, Map<IComparable,int>>) r = 
            match r with
            | T_Upto(TypedExp(Const(IntConst n),_)) -> new UptoRep(n):> Rep
            | T_Link(tn) 
            | T_Upto(TypedExp(SizeOf tn,_)) -> if idReps.ContainsKey tn then 
                                                   // ID as a foreign reference : 
                                                   //We always use an IComparable as the foreign representation
                                                   //this could be made smarter but how ? in the end the only universal thinkg we only know is that it will have to be IComparable..
                                                   //anything else has to come from either the schema (and here we have nothing) or from the source
                                                   //we can delegate to the converter some decision (cf point 1) but for what purpose ?
                                                 new UptoSizeRep(idReps.[tn], idMap.[tn] ):> Rep 
                                               else failwith (sprintf "unknown table %A" tn)
            | T_Upto(_ ) -> failwith "unknown case" 
            | T_Real -> new RealRep() :> Rep
            | T_Bool -> new BoolRep() :> Rep
            | T_Int  -> new IntRep() :> Rep
            | T_String -> new StringRep() :> Rep
            | T_Array(eTy,TypedExp(Const(IntConst n),_)) ->  let eRep:Rep = rep (idReps,idMap) eTy in eRep.AddRank(n)
            | T_Array(eTy,TypedExp(SizeOf(tn),_)) ->   let eRep:Rep = rep (idReps,idMap) eTy in  eRep.AddRank((idMap.[tn]).Count)
            | T_Vector -> new VectorRep() :> Rep
            | T_PositiveDefiniteMatrix -> new MatrixRep() :> Rep
            | _ -> failwith (sprintf "missing representation for type %A etc..." r)


  let read (typedCoreSchema : Declaration list)
           (converter       :  Converter<'Src> )
           (dataSource      : DataSource<'Src>)
           : DataBase = 
   let rec trTables (DB:DataBase) (idReps : Map<TableName,  Rep>, idMap: Map<TableName,  Map<IComparable,int>>) (tables :Declaration list) = 
      match tables with
      | [] -> DB
      | (Declaration(Fun(tn),table)::tables) ->   trTables DB (idReps,idMap)  tables
      | (Declaration(Table(tn, None),table)::tables) -> failwithf "Illegal schema: missing id strategy for table %A" tn
      | (Declaration(Table(tn, Some idStrategy),table)::tables) ->
           //ColumnType -> (s: Rep<T>) :> Rep  
           //let (ColumnName id) as ID = (match oIdColName with Some  ID -> ID | None ->  ColumnName "ID")
           let (size,TS) = dataSource(tn)
           // ID is part of a table, for others to refer to an element
           // many options are possible here : if we are given some common representation, we can replace it with a smarter one, or default to "ToString"
           // when the table is empty, we assume a "genericIComparableRep" 
           let inst, idRep  = 
                match idStrategy with 
                 | FromColumn cid -> let ids = TS cid
                                     let rep = if not (ids |> Seq.isEmpty) 
                                               then let v =  ids |> Seq.head  
                                                    match box v with
                                                     | :? string  -> StringRep()              :> Rep
                                                     | :? int     -> IntRep()                 :> Rep
                                                     | :? decimal -> RealRep()                :> Rep
                                                     | :? double  -> RealRep()                :> Rep
                                                     | _          -> GenericToStringRep()     :> Rep
                                               else GenericIComparableRep()  :> Rep
                                     mkInstance converter rep  (*(rep idReps idty)*)  ids,
                                     rep
                 |              _ -> new NonNullableInstance<int>([| for i in 0..size-1 -> i |])  :> Instance, //why IComparable and not just int?
                                     IntRep():>Rep
           let idReps' = idReps.Add(tn,idRep)
           let keyToPos = Map.ofArray [| for i in 0..(size-1) ->  (inst.get_Item i :?> IComparable),i |] 
           do match idStrategy with 
              | FromColumn cid ->
                if keyToPos.Count <> size then failwithf "Illegal data: table %A has a duplicate value in key column %A" tn cid
              | _ -> ()
           let idMap' = idMap.Add(tn,((*rep idReps  idty*) keyToPos))
           let rec trColumns (TB: Map<ColumnName,ColValue>)
                             (columns: Table) = 
            match columns with
            |  [] -> 
               trTables (DB.Add(tn,(keyToPos.Count,TB,idRep, keyToPos))) (idReps',idMap') tables
            | (cn,({Type=ty;Markup=m} as col))::rest -> 
               let TB' = match m with //we only read input and observables
                           | Input          ->
                              let src = TS cn
                              if src.Length <> size then failwithf "Illegal data: in table %A, expecting column %A of length %A, found column of length %A" tn cn size cn.Length
                              TB.Add(cn,mkInstance converter (rep (idReps',idMap') ty)  src)
                           | Observable _   -> 
                              let src = TS cn
                              if src.Length <> size then failwithf "Illegal data: in table %A, expecting column %A of length %A, found column of length %A" tn cn size cn.Length
                              TB.Add(cn,mkNullableInstance converter (rep (idReps',idMap') ty) src)
                           | _ -> TB
               trColumns TB'  rest
           trColumns Map.empty table
   trTables  Map.empty  (Map.empty,Map.empty) typedCoreSchema

  /// type directed write - the db can have more information than the schema
  let dbTo2DString (typedCoreSchema : Declaration list)
                    (writeTable :  TableName -> IdStrategy -> Table -> obj[,] -> obj[,] -> unit)
                    (distToString: Rep -> obj -> string)
                    (db:DataBase) : unit = 

         let rec ToObj (rep:Rep) (src:obj) : obj =
            (rep).Visit({ new RepVisitor<obj> with
                 member this.CaseIntRep                r  = match src with :? int -> src | _ -> distToString rep src :> obj
                 member this.CaseArrayRep<'T>         (r:ArrayRep<'T>)  = 
                                                             let ts = (src :?> System.Collections.IEnumerable) in  
                                                             "["+System.String.Join(",",[| for t in ts -> (ToObj (r.ElementRep) t).ToString() |])+"]" :> obj
                 member this.CaseUpToRep               r  =  match src with :? int -> src | _ -> distToString rep src :> obj
                 member this.CaseUpToSizeRep           r  =  match src with :? int as i -> (r.Inverse i).ToString() :> obj | _ -> distToString rep src :> obj
                                                           
                 member this.CaseBoolRep               r  =  match src with :? bool -> src | _ -> distToString rep src :> obj
                 member this.CaseStringRep             r  =  match src with :? string -> src | _ -> distToString rep src :> obj
                 member this.CaseRealRep               r  =  match src with :? real -> src | _ -> distToString rep src :> obj
                 member this.CaseGenericIComparableRep r  = src.ToString() :> obj // can this be a dist?
                 member this.CaseGenericToStringRep    r  = src.ToString() :> obj // can this be dist?
                 member this.CaseVectorRep r  =   match src with :?  Vector -> src.ToString() :> obj | _ -> distToString rep src :> obj
                 member this.CaseMatrixRep r  = match src with :?  Matrix -> src.ToString() :> obj | _ -> distToString rep src :> obj
                 })

         let id2Rep = db |> Map.map(fun k (_,_,rep,keyToPos) -> rep)         // build me up incrementally
         let id2KeyToPos = db |> Map.map(fun k (_,_,_,keyToPos) -> keyToPos) // build me up incrementally
         let rec trTables globalColCounter (tables :Declaration list) = 
            match tables with
            | [] -> ()
            | (Declaration(Fun(tn),table)::tables)                    -> trTables globalColCounter tables
            | (Declaration(Table(tn, None ),table)::tables)           -> failwith "please explicit a strategy for getting the id"
            | (Declaration(Table(tn, Some idStrategy),table)::tables) ->
                let (size, colMap, idRep, keyToPos) = db.[tn]//every table from schema must exist in database
                let cardStatic, cardInstances = 
                    let rec trColumns (cardStatic, cardInstances) (columns: Table) = 
                        match columns with
                        |  [] -> cardStatic, cardInstances
                        | ((cn,({Type=ty;Markup=(_,Local,_)})) as c)::rest -> 
                           (* skip locals *)
                           trColumns (cardStatic, cardInstances) rest
                        | ((cn,({Type=ty;Markup=m})) as c)::rest -> 
                            match level c with
                            | l when l > W  -> trColumns (cardStatic, cardInstances + 1) rest
                            | W             -> trColumns (cardStatic + 1, cardInstances) rest
                            | _             -> trColumns (cardStatic, cardInstances) rest

                    let extraCol = match idStrategy with | FromColumn  cid ->  if colMap.ContainsKey cid then 0 else 1
                                                         | _ -> 1
                    trColumns (0, extraCol) table

                let staticDataBuf, instanceDataBuf = 
                    let staticDataBuf   = Array2D.zeroCreate (1+cardStatic) 2        
                    let instanceDataBuf = Array2D.zeroCreate (1+size)       cardInstances
                    let rec trColumns (iStatic, iInstances) (columns: Table) = 
                        match columns with
                        |  [] -> staticDataBuf, instanceDataBuf
                        | ((cn,({Type=ty;Markup=(_,Local,_)})) as c)::rest ->
                           (* skip locals *)
                           trColumns (iStatic, iInstances) rest
                        | ((cn,({Type=ty;Markup=m})) as c)::rest ->
                            let rep = rep (id2Rep,id2KeyToPos) ty
                            let ToObj = ToObj rep 
                            match level c with
                            | l when l > W  -> 
                                instanceDataBuf.[0,iInstances] <- cn :> obj
                                let cv = (colMap.[cn] :?> Instance) //every column from schema must be in database
                                //other entries are Null. the backend, excel or CSV, should interpret those as None
                                for i in  cv.get_NonNullIndices do instanceDataBuf.[1+i,iInstances] <-  (ToObj (cv.get_Item i))
                                trColumns (iStatic, iInstances + 1) rest 
                            | W  -> 
                                staticDataBuf.[1+iStatic, 0]   <-  cn :> obj
                                staticDataBuf.[1+iStatic, 1] <- (ToObj ((colMap.[cn] :?> Static).Value()))
                                trColumns (iStatic + 1, iInstances) rest
                            | _             -> trColumns (iStatic, iInstances) rest
                    let extraCol = 
                        match idStrategy with | FromColumn  cid ->  if colMap.ContainsKey cid then 0 
                                                                    else let posToKey = keyToPos |> Seq.map (fun kv -> kv.Value, kv.Key) |> Map.ofSeq
                                                                         instanceDataBuf.[0, 0]  <- cid :> obj
                                                                         for i in  0 .. size - 1  do instanceDataBuf.[1+i, 0] <- (posToKey.[i]) :> obj //TBR
                                                                         1
                                              | _ -> instanceDataBuf.[0,0] <- "ID" :> obj;
                                                     for i in  0 .. size - 1  do instanceDataBuf.[1+i, 0] <- i :> obj
                                                     1
                    trColumns (0, extraCol) table

                let globalColCounter = 
                    if cardStatic > 0 then
                        staticDataBuf.[0,0] <- "Name"  :> obj;  staticDataBuf.[0,1] <- "Value" :> obj
                        globalColCounter + 3
                    else
                        globalColCounter 
            
                writeTable tn idStrategy table staticDataBuf  instanceDataBuf
       
                trTables  (globalColCounter + cardInstances + 1) tables
         trTables 0 typedCoreSchema

  /// Careful : erases existing CSVs. it ensures presence of dir
  /// representation : rows interpret nulls as None, and write an empty string to the file
  /// we interpret strings as objects to be persisted and add escape to it
  /// all this code should be driven by types
  let write2DArrayToCSV dir filename (rows:obj[,]) = 
        let separator = ","
        let quote (o:obj) = 
                    match o with 
                    | :? string as s -> "\""+s+"\"" :> obj
                    | o -> o
        let sw,name = 
         if not (System.IO.Directory.Exists(dir)) then System.IO.Directory.CreateDirectory(dir) |> ignore
         let name = dir + @"\" + filename 
         if System.IO.File.Exists(name) then System.IO.File.Delete(name) //careful
         let file = new System.IO.StreamWriter(name)
         file, name
        try 
           for r = rows.GetLowerBound(0) to rows.GetUpperBound(0) do
              sw.WriteLine(String.Join(separator,[| for c in rows.GetLowerBound(1) .. rows.GetUpperBound(1) -> if rows.[r,c] = null then "" |> box else quote (rows.[r,c]) |]))
        finally 
          sw.Close()

  let writeCSV  (dirName)
                (typedCoreSchema : Declaration list)
                (distToString: Rep -> obj -> string)
                (db:DataBase)  = 
        dbTo2DString typedCoreSchema
                    (fun tn _ _ staticDataBuf instanceDataBuf ->
                        let cardStatic      = Array2D.length1 staticDataBuf   - 1  //-1 for the headers
                        if cardStatic > 0 then
                            write2DArrayToCSV  dirName (tn+"static.csv") staticDataBuf
                        write2DArrayToCSV  dirName (tn+".csv")           instanceDataBuf
                    )
                    distToString
                    db
                
          
module DTOToTypedDTO = 
  open MicrosoftResearch.Infer.Tabular.Syntax
  open TypedDTO

  //type dataNormalized = Map<ColumnName,int> * (obj array) seq
  //type DTO     = DTO of Map<TableName, dataNormalized>

  /// this DTO translation to a Typed DTO does not exposes the actual ID but its encoding as an int, which is performed upon reading the DTO
  /// we could also expose the real id, and let the TypedDTO do the translation : every index is to be translated
  let dataSourceDTOInt ((DTO dto) : DTO) : DataSource<obj> = 
      fun tn -> 
                let (colMap,svalues) = dto.[tn]
                let values =  svalues |> Seq.toArray
                let l = values.Length
                l, fun cn -> values |> Array.map (fun e -> e.[colMap.[cn]]) 
  
  ///transforms a Schema and a DTO to a DataBase
  let readFromDTO (schema : Declaration list) (dto : DTO) : DataBase = 
            read schema (ObjConverter()) (dataSourceDTOInt dto)

//TODO:move elsewhere - perhaps to ExcelCompiler
module DistributionPrinter = 
   let rec distToString (rep:TypedDTO.Rep) (src:obj) : string =
        (rep).Visit({ new TypedDTO.RepVisitor<string> with
             member this.CaseIntRep                r  = src.ToString()
             member this.CaseArrayRep<'T>         (r:TypedDTO.ArrayRep<'T>)  = 
                                                               let ts = (src :?> IEnumerable) in  
                                                               "["+System.String.Join(",",[| for t in ts -> (distToString r.ElementRep t) |])+"]" 
             member this.CaseUpToRep               r  =  src.ToString()
                                                          
             member this.CaseUpToSizeRep           r  = match src with
                                                         | :? MicrosoftResearch.Infer.Distributions.Discrete as d ->
                                                           let v = d.GetProbs().ToArray()
                                                           "Discrete("
                                                           + System.String.Join(" ",Array.mapi (fun i Pi -> (r.Inverse(i).ToString()) + "=" + Pi.ToString()) v)
                                                           + ")"
                                                         | _ -> src.ToString()
             member this.CaseBoolRep               r  =  src.ToString()
             member this.CaseStringRep             r  =  src.ToString()
             member this.CaseRealRep               r  =  src.ToString()
             member this.CaseGenericIComparableRep r  = src.ToString() // can this be a dist?
             member this.CaseGenericToStringRep    r  = src.ToString() // can this be dist?
             member this.CaseVectorRep r  =  src.ToString() 
             member this.CaseMatrixRep r  = src.ToString()
        })

 module CSVSource =
     open Microsoft.VisualBasic.FileIO
     open System.Collections
     open Syntax
     open TypedDTO
     //type DataSource<'Src> = TableName * ID -> int * ColumnType * (ColumnName -> 'Src) (* * Map<IComparable, int>> *)
     let read dir : DataSource<string> = fun (tn (*,id*)) ->
         use tfp = new TextFieldParser(System.IO.Path.Combine([|dir;tn+".csv"|]))
         tfp.TextFieldType <- FieldType.Delimited
         tfp.Delimiters <- [| "," |]
         let headers = tfp.ReadFields()
         //let idx = Array.find (fun cn -> cn = id) headers
         let acc = new System.Collections.Generic.List<string[]>()
         while (not tfp.EndOfData) do
            acc.Add(tfp.ReadFields())
         tfp.Close()
         let size = acc.Count
         let get cn = 
             let col = Array.create<string> size null
             let ci = Array.findIndex (fun cn' -> cn = cn') headers
             for i in 0 .. size - 1 do
                col.[i] <- acc.[i].[ci]
             col
         (size,(*StringRep() :> _,*)get)
    


