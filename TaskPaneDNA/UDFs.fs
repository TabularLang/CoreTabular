module DNAUDFs
  open ExcelDna.Integration 
  open UDFs

  [<ExcelFunction(Category="Tabular")>]
  let public Echo(s:string):string                = Echo s 
  [<ExcelFunction(Category="Tabular")>]
  let public Mean(s:string):double                = Mean s
  [<ExcelFunction(Category="Tabular")>]
  let public Method(dist:string,name:string):obj  = Method(dist,name)
  [<ExcelFunction(Category="Tabular")>]
  let public Variance(dist:string):double         = Variance(dist)
  [<ExcelFunction(Category="Tabular")>]
  let public Field(dist:string,name:string):obj   = Field(dist,name)
  [<ExcelFunction(Category="Tabular")>]
  let public Property(dist:string,name:string):obj= Property(dist,name)
  [<ExcelFunction(Category="Tabular")>]
  let public GetLogProb(dist:string,v:int):float  = GetLogProb(dist,v)
  [<ExcelFunction(Category="Tabular")>]
  let public GetProbs(dist:string,v:int):float[]  = GetProbs(dist,v)
  [<ExcelFunction(Category="Tabular")>]
  let public Reflect(dist:string):string          = Reflect(dist)
  [<ExcelFunction(Category="Tabular")>]
  let public GetMode(s:string): obj               = GetMode(s)
  [<ExcelFunction(Category="Tabular")>]
  let public ToString(s:string):string            = ToString(s)