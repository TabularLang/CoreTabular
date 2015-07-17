namespace MicrosoftResearch.Infer.Tabular


[<AutoOpen>]
module StringToInferNet = 
    open System
    open System.Threading;
    open System.IO;
    open MicrosoftResearch.Infer.Tabular.DataLayer
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Json
    open MicrosoftResearch.Infer.Utils
    open MicrosoftResearch.Infer.Distributions
    open Syntax

    let integers = Seq.unfold(fun i -> Some(i, i+1)) 0
    let someIntegers = integers |> Seq.map Some
    let rtuple a b = b, a 
    let curry f a b = f (a,b)
    let rev f a b = f b a
    let coerce (v:'a) = System.Convert.ChangeType(v, (typeof<'b>)) :?> 'b

        
    module String = 
      let ConcatBrace sOpen sep sClose sStr=
        sOpen +  String.concat sep sStr + sClose 
     
    let serializeJSON dist = 
        let rec types l dist =           
            match dist with
            | a when a.GetType().IsArray -> 
                                        let ad= box dist :?> obj[]
                                        if ad.Length > 0 then
                                          dist.GetType()::types l ad.[0]
                                        else                    
                                          ad.GetType()::l
            | a when Reflection.FSharpType.IsTuple (a.GetType()) -> Array.fold (rev (curry List.Cons)) l (Reflection.FSharpValue.GetTupleFields a |> Array.map (fun i -> i.GetType()))
            | _  ->  dist.GetType()::l

        let t = types List.empty dist |> List.toArray
        let serializer = DataContractJsonSerializer(dist.GetType(), t, Int32.MaxValue, false, new DataContractSurrogate(), true);
        use stream = new MemoryStream()
        //let writer = System.Xml.XmlDictionaryWriter.CreateTextWriter(stream);
        //serializer.WriteObject(writer, dist);
        serializer.WriteObject(stream, dist);
        //writer.Flush(); 
        stream.Flush(); stream.Position <- 0L;
        use sr = new StreamReader(stream,Text.UTF8Encoding.UTF8)
        sr.ReadToEnd()


    let serializeXML dist = 
        let rec types l dist =           
            match dist with
            | a when a.GetType().IsArray -> 
                                        let ad= box dist :?> obj[]
                                        if ad.Length > 0 then
                                          dist.GetType()::types l ad.[0]
                                        else                    
                                          ad.GetType()::l
            | a when Reflection.FSharpType.IsTuple (a.GetType()) -> Array.fold (rev (curry List.Cons)) l (Reflection.FSharpValue.GetTupleFields a |> Array.map (fun i -> i.GetType()))
            | _  ->  dist.GetType()::l

        let t = types List.empty dist |> List.toArray
        let serializer = DataContractSerializer(dist.GetType(), t, Int32.MaxValue, false, true, new DataContractSurrogate());
        use stream = new MemoryStream()
        let writer = System.Xml.XmlDictionaryWriter.CreateTextWriter(stream);
        serializer.WriteObject(writer, dist);
        writer.Flush(); stream.Flush(); stream.Position <- 0L;
        use sr = new StreamReader(stream,Text.UTF8Encoding.UTF8)
        sr.ReadToEnd()


    type DTO
        with member this.merge ((DTO other):DTO) = 
                      let (DTO this) = this
                      let tables = Set.union (seq { for ithis in this  -> ithis.Key } |> Set.ofSeq) (seq {for iother in other -> iother.Key } |> Set.ofSeq)
                      tables |> Seq.map(fun tn -> let map1, data1 = if this.ContainsKey tn then this.[tn] else Map.empty, Seq.empty
                                                  let map2, data2 = if other.ContainsKey tn then other.[tn] else Map.empty, Seq.empty
                                                  tn, (map1 |> Map.fold(fun (s:Map<_,_>) k v -> s.Add(k, v + map2.Count)) map2, Seq.zip data1 data2 |> Seq.map(fun (a1,a2) -> Array.concat([a2;a1])))
                                        ) |> Map.ofSeq

    let private mapi f (a:System.Array) =  Array.init (a.Length)  (fun (i:int) -> f i (a.GetValue(i)))
    let rec print (acc:int list) (o:obj) =
      match o with
       | :? System.Array as a ->  a |> mapi (fun i e -> print (i::acc) e) |> String.concat "\n" //explodes the dimensions (=cases i of parent distrib)
       | _ -> (if acc.IsEmpty then "" else (acc |> Seq.map string |> String.ConcatBrace "" "," ",")) + o.ToString()        //write the distribution

    // let rec print (acc:int list) (o:obj) = 
    //  match o with
    //   | a when a.GetType().IsArray -> box a :?> obj[] |> Array.mapi (fun i e -> print (i::acc) e) |> String.concat "\n" //explodes the dimensions (=cases i of parent distrib)
    //   | _ -> (if acc.IsEmpty then "" else (acc |> Seq.map string |> String.ConcatBrace "" "," ",")) + o.ToString()        //write the distribution

    let flip f x y = f y x

    let translateInferDistributionToOutputString (tables:(TableName*Table)list) (posToId:Map<TableName,Map<int,IComparable>>) (tname:TableName) (cmap:Map<ColumnName,int>) (colidx:int) (obj:obj) :string = 
        match obj with 
        | :? MicrosoftResearch.Infer.Distributions.Discrete as d -> //d.GetProbs().Item(0) 
            let colname = Map.fold (fun nameResult nameKey nameIdx -> if nameIdx = colidx then nameKey else nameResult) "" cmap
            let column = List.find (fun (tn,_) -> tn.Equals(tname)) tables 
                        |> snd 
                        |> List.find (fun (cn,_) -> cn.Equals(colname))
                        |> snd
            match column.Type with
            | T_Det(B_Link foreignTableName, _) 
            | T_Det(B_Upto (TypedExp(SizeOf foreignTableName,_)),_)->  // link to foreign table; bring over the foreign table's labels
                let foriegnTableVals = posToId.Item(foreignTableName) |> Map.toArray |> (Array.map snd)
                //Array.fold2 (fun str name prob -> str+name.ToString()+"="+prob.ToString()+" " ) "Discrete( " foriegnTableVals (d.GetProbs().ToArray())
                //+ ")"
                // No sorting here.  Alternatively we can sort the values in descending order
                (*let nameProbPairs = *) 
                let nameProbPairs = foriegnTableVals |> (Array.zip (d.GetProbs().ToArray()) )
                                    //|> Array.filter (fst >> (<>) 0.0)  // remove labels that have probability 0
                                    |> Array.sortBy fst 
                do Array.Reverse nameProbPairs // comment this line and the previous for no sorting
                "Discrete("+String.Join(" ",Array.map (fun (prob,name) -> name.ToString() + "=" + prob.ToString() ) nameProbPairs)+")"
            | T_Det(B_Upto N, _) -> // regular discrete distribution
                let nameProbPairs = Array.mapi (fun idx prob -> (prob, idx))  (d.GetProbs().ToArray())
                                    //|> Array.filter (fst >> (<>) 0.0)  // remove labels that have probability 0
                                    |> Array.sortBy fst 
                do Array.Reverse nameProbPairs // comment this line and the previous for no sorting
                "Discrete("+String.Join(" ",Array.map (fun (prob,name) -> name.ToString() + "=" + prob.ToString() ) nameProbPairs)+")"
            | _ -> print List.empty d
        | _ -> print List.empty obj


    type DistDTO 
        with  member x.Visit(f) : DTO=  
                        let (DistDTO m) = x
                        m |> Map.map(fun tname (cmap, saobj) -> 
                                            let saobj' = saobj |> Seq.map (fun aobj -> 
                                                                                aobj |> Array.map (fun o -> o |> f |> box))
                                            (cmap,  saobj')) |> DTO
              member x.VisitWithContext(f) :DTO = 
                        let (DistDTO m) = x
                        m |> Map.map(fun tname (cmap, saobj) -> 
                                            let saobj' = saobj |> Seq.map (fun aobj -> 
                                                                                aobj |> Array.mapi (fun colidx o -> o |> (f tname cmap colidx) |> box))
                                            (cmap,  saobj')) |> DTO
              member x.AsXml()    = x.Visit(serializeXML) 
              member x.AsJSON()   = x.Visit(serializeJSON)

              //old behaviour using .NET toString()
              //member x.AsString(posToId:Map<TableName,Map<int,IComparable>>,tables:(TableName*Table)list) = x.Visit(print List.empty)
              member x.AsString(posToId:Map<TableName,Map<int,IComparable>>,tables:(TableName*Table)list) = x.VisitWithContext(translateInferDistributionToOutputString tables posToId)
              member x.GetMode()  = 
                                      x.Visit(fun o -> 
                                              match o with
                                                | :? MicrosoftResearch.Infer.Distributions.Discrete as  d -> d.GetMode()
                                                | _ -> -1
                                                (*
                                                // these cases broken...
                                                | :? MicrosoftResearch.Infer.Distributions.Bernoulli as  d -> d.GetMean() :> obj 
                                                | :? MicrosoftResearch.Infer.Distributions.Gaussian as  g -> g.GetMean() :> obj
                                                | :? MicrosoftResearch.Infer.Distributions.Dirichlet as  d -> d.GetMean() :> obj
                                                | :? MicrosoftResearch.Infer.Distributions.Gamma as  g -> g.GetMean() :> obj
                                                | :? MicrosoftResearch.Infer.Distributions.Beta as  g -> g.GetMean() :> obj
                                                | _ -> "no mode" :> obj
                                                *))



    type Microsoft.FSharp.Collections.Map<'K, 'V when 'K : comparison > with 
        member t.Keys = t |> Seq.map(fun e -> e.Key)

