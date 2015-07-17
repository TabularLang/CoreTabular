module TableType

// Todo: create type aliases row index, col index for int

type TableName = string
type ColumnName = string

/// From Excel
type OldColumnType =
    | OBool
    | ODateTime
    | OInteger
    | OReal
    | OString
    | OLink of (TableName * ColumnName)

/// 
type ColumnType =
    | Bool
    | DateTime
    | Integer
    | Real
    | Discrete of int // # unique values
    | String
    | Link of (TableName * ColumnName) with
    override this.ToString() =
        match this with
        | Bool -> "Bool"
        | DateTime -> "DateTime"
        | Integer  -> "Integer"
        | Real     -> "Real"
        | Discrete d -> "Discrete("+string(d)+")"
        | String   -> "String"
        | Link (tname,cname) -> "Link("+tname+","+cname+")"
    // use regular expressions
//    member this.FromString str =
//        String.

// Nominal 
// Ordinal 


// # of unique values
// (total number of values is the length of the data set)
//type ColumnStats = {numUniqueVals:int}

/// later might want to store more information with each column 
type ColumnInfo = {colname:ColumnName; coltype:ColumnType(*; colstats:ColumnStats*)}
type TableData = seq<System.IComparable[]>

/// Relationships
type Relation = {primary:(TableName*ColumnName); foreign:(TableName*ColumnName)}


type Table = {name:TableName; colinfos:ColumnInfo[]; data:TableData} 
/// try to keep in topological order
type TableDTO = {tables:Table list; relations:(Set<Relation>) } 


/// Computation expression to stop computation early when a None is bound / done by let! / do!
type MaybeExpression() =
    member this.Bind(x, f) =
        match x with
        | Some v -> f v
        | None -> None
    member this.Return(x) =
        Some x
    member this.ReturnFrom(x) =
        x
let MAYBE = MaybeExpression()


///////////////////////////////////////////////////////////// ColumnType
let convertToNewType coldata oldtype =
    match oldtype with
    | OBool -> Bool
    | OLink bla -> Link bla
    | _ -> 
        let numdata = Seq.length coldata
        let uniqs = Seq.length <| Seq.distinct coldata
        let percentUniq = (float uniqs) / (float numdata)
        match oldtype with
        | OInteger ->   if uniqs < 15 || percentUniq < 0.1 then Discrete(uniqs) else Integer
        | OReal ->      if uniqs < 15 || percentUniq < 0.1 then Discrete(uniqs) else Real
        | ODateTime ->  if uniqs < 15 || percentUniq < 0.1 then Discrete(uniqs) else DateTime
        | OString ->    if uniqs < 15 || percentUniq < 0.1 then Discrete(uniqs) else String
        | _ -> failwith "impossible"



///////////////////////////////////////////////////////////// Table
let getColIdx table (cname:ColumnName) =
    table.colinfos
    |> Array.tryFindIndex (fun colinfo -> colinfo.colname = cname )

/// Supply a function to calculate the derived value given other values in the row
let addDerivedCol table f newcolinfo =
    let newcolinfos = Array.append table.colinfos [| newcolinfo |]
    let newdata = table.data |> Seq.map (fun objarr -> 
                                            let newo = f objarr
                                            Array.append objarr [| newo |]
                                        )
    {table with colinfos=newcolinfos; data=newdata}

let deleteCol table idx =
    let newcolinfos = Array.append table.colinfos.[..idx-1] table.colinfos.[idx+1..]
    let newdata = table.data |> Seq.map (fun objarr -> Array.append objarr.[..idx-1] objarr.[idx+1..])
    {table with colinfos=newcolinfos; data=newdata}

let getRowIdx table cname v =
    MAYBE {
        let! colidx = getColIdx table cname
        return! table.data
                |> Seq.tryFindIndex (fun objarr -> objarr.[colidx] = v)
    }

type Table with
    member this.getColIdx = getColIdx this
    member this.addDerivedCol = addDerivedCol this
    member this.deleteCol = deleteCol this
    member this.getRowIdx = getRowIdx this

    
///////////////////////////////////////////////////////////// TableDTO
let getTable tableDTO tname =
    tableDTO |> List.tryFind (fun tab -> tab.name = tname)

let rec getConcreteData tableDTO tname cname =
    MAYBE {
        let! table = tname |> getTable tableDTO
        let! colidx = cname |> getColIdx table
        match table.colinfos.[colidx].coltype with
        | Link (tnRemote,cnRemote) ->
            return! getConcreteData tableDTO tnRemote cnRemote
        | _ -> 
            return table.data |> Seq.map (fun objarr -> [|objarr.[colidx]|])
    }


type TableDTO with
    member this.getTable = getTable this.tables
    member this.getConcreteData = getConcreteData this.tables



