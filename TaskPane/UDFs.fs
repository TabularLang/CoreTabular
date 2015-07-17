module UDFs 
  open MicrosoftResearch.Infer.Distributions
  open MicrosoftResearch.Infer.Maths

  let parseDist (s:string) = match s with
    | "Gaussian.Uniform" ->
        new Gaussian():>obj
    | "Beta.Uniform" ->
        new Beta():>obj
    | "Bernoulli.Uniform" ->
        new Bernoulli():>obj
    | "Beta.Uniform" ->
        new Beta():>obj
    | s when s.StartsWith("Gaussian.PointMass") ->
        let a = s.Split('(',')',',')
        let mutable  v = 0.0
        let true = System.Double.TryParse(a.[1],& v)
        Gaussian.PointMass(v):> obj
    | s when s.StartsWith("Beta.PointMass") ->
        let a = s.Split('(',')',',')
        let mutable  v = 0.0
        let true = System.Double.TryParse(a.[1],& v)
        Beta.PointMass(v):> obj
    | s when s.StartsWith("Bernoulli.PointMass") ->
        let a = s.Split('(',')',',')
        let mutable v = false
        let true = System.Boolean.TryParse(a.[1],& v)
        Bernoulli.PointMass(v):> obj
    | s when s.StartsWith("Gaussian.PointMass") ->
        let a =s.Split('(',')',',')
        let mutable  v = 0.0
        let true = System.Double.TryParse(a.[1],& v)
        Gaussian.PointMass(v):> obj
    | s when s.StartsWith("Dirichlet.PointMass") ->
        let a = s.Split('(',')',' ')
        let ds = [| for i in 1..a.Length-2 ->
                    let mutable  d = 0.0
                    let true = System.Double.TryParse(a.[i],& d)
                    if true then d else raise (System.ArgumentException())
                    |]
        Dirichlet.PointMass(Vector.FromArray(ds)):> obj
    | s when s.StartsWith("Discrete.PointMass") ->
        let a = s.Split('(',')',' ')
        let mutable v = 1
        let true = System.Int32.TryParse(a.[1],& v)
        let d = Discrete.PointMass(v,v+1)
        d :> obj // numValues = v+1 is arbitrary but the string representation doesn't reveal numValues
    | s when s.StartsWith("Gaussian(") ->
        let a = s.Split('(',')',',')
        let mutable  mean = 0.0
        let true = System.Double.TryParse(a.[1],& mean)
        let mutable variance = 0.0
        let true = System.Double.TryParse(a.[2],& variance)
        new Gaussian(mean,variance):> obj
    | s when s.StartsWith("Beta(") ->
        let a = s.Split('(',')',',')
        let mutable  tc = 0.0
        let true = System.Double.TryParse(a.[1],& tc)
        let mutable fc = 0.0
        let true = System.Double.TryParse(a.[2],& fc)
        new Beta(tc,fc):> obj
     | s when s.StartsWith("Gamma(") ->
        let a = s.Split('(',')',',')
        let mutable  shape = 0.0
        let true = System.Double.TryParse(a.[1],& shape)
        let mutable scale = 0.0
        let true = System.Double.TryParse(a.[2],& scale)
        new Gamma(shape,scale):> obj
    | s when s.StartsWith("Bernoulli(") ->
        let a = s.Split('(',')',',')
        let mutable  bias = 0.0
        let true = System.Double.TryParse(a.[1],& bias)
        new Bernoulli(bias):> obj
    | s when s.StartsWith("Poisson(") ->
        let a = s.Split('(',')',',')
        let mutable  bias = 0.0
        let true = System.Double.TryParse(a.[1],& bias)
        new Poisson(bias):> obj
    | s when s.StartsWith("Dirichlet(") -> //  lossy?
        let a = s.Split('(',')',' ')
        let ds = [| for i in 1..a.Length-2 ->
                    let mutable  d = 0.0
                    let ok = System.Double.TryParse(a.[i],& d)
                    if ok then d else raise (System.ArgumentException())
                    |]
        new Dirichlet(Vector.FromArray(ds)):> obj
    | s when s.StartsWith("Discrete(") -> //  lossy?
        let a = s.Split('(',')',' ')
        let mutable map = Map.empty
        do for i in 1..a.Length-2 do
                    let [|k;v|] = a.[i].Split('=')
                    let mutable  j = 1
                    let mutable  d = 0.0
                    let key : System.IComparable =
                              if System.Int32.TryParse(k, &j) 
                              then box j :?> System.IComparable
                              else box k  :?> System.IComparable
                    let ok = System.Double.TryParse(v,& d)
                    if ok then
                       map <- map.Add(key,d)
                    else raise (System.ArgumentException())
        let map = map
        new Discrete(Vector.FromArray([| for kv in map  -> kv.Value|] )):> obj

  let public Echo(s:string):string = s 

  let public Mean(s:string):double =
      let o = parseDist s :?> CanGetMean<double>
      o.GetMean()

  let public Method(dist:string,name:string):obj =
      let o = parseDist dist
      let ty = o.GetType()
      let m = ty.GetMethod(name)
      m.Invoke(o,[||])
     

  let public Variance(dist:string):double =
      let o = parseDist dist :?> CanGetVariance<double>
      o.GetVariance()

  let public Field(dist:string,name:string):obj =
      let o = parseDist dist 
      let ty = o.GetType()
      let f = ty.GetField(name)
      f.GetValue(o)
  
  let public Property(dist:string,name:string):obj =
      let o = parseDist dist 
      let ty = o.GetType()
      let pi = ty.GetProperty(name)
      pi.GetValue(o)

  let public GetLogProb(dist:string,v:int):float =
      let o = parseDist dist :?> Discrete
      o.GetLogProb(v)

  let public GetProbs(dist:string,v:int):float[] =
      let o = parseDist dist :?> Discrete
      o.GetProbs().ToArray()

  let public Reflect(dist:string):string =
      let o = parseDist dist
      let ty = o.GetType()
      let fis = ty.GetMembers()
      sprintf "%A" fis
(*
not available in IN 2.5
  let public Mode(s:string):double =
      let o = parseDist s :?> Bernoulli 
      o.Mode()
*)

  // Mode clashes with Excel Mode, hence GetMode
  let public GetMode(s:string): obj =
      Method(s,"GetMode")

  let public ToString(s:string):string =
      let o = parseDist s
      o.ToString()