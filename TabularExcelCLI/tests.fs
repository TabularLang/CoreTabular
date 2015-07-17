namespace MicrosoftResearch.Infer.Tabular


module Tests =
   open Xunit
   open ParserCLI

   let (+.) args = (fun c -> c + args)
   let fileCommand = "file"
   let sheetCommand = "sheets"
   let outputCommand = "output"


   [<Fact>] 
   let ``CLI parser retrieves an empty valued command`` () =
      let filename = "toto"
      let defs = [
            {ArgInfo.Command=fileCommand  ; Description="Workbook to run tabular on"                      ; Required=true  };
            {ArgInfo.Command=outputCommand; Description="output name"                                     ; Required=false };
            {ArgInfo.Command=sheetCommand ; Description="sheets containing models on which to run tabular"; Required=false } ]
      let s = fileCommand 
      let d = ParserCLI.parseArgs defs  (["--" + fileCommand])
      Assert.True(d.ContainsKey(fileCommand))


   [<Fact>] 
   let ``CLI parser retrieves a single valued command`` () =
      let filename = "toto"
      let defs = [
            {ArgInfo.Command=fileCommand  ; Description="Workbook to run tabular on"                      ; Required=true  };
            {ArgInfo.Command=outputCommand; Description="output name"                                     ; Required=false };
            {ArgInfo.Command=sheetCommand ; Description="sheets containing models on which to run tabular"; Required=false } ]
      let s = fileCommand 
      let d = ParserCLI.parseArgs defs  (["--" + fileCommand; filename])
      Assert.True(d.ContainsKey(fileCommand) && 
                 (d.[fileCommand] |> List.tryFind((=) filename) |> Option.isSome))


   [<Fact>] 
   let ``CLI parser retrieves a multiple valued command`` () =
      let filename1, filename2 = "toto1", "toto2"
      let command1 = fileCommand
      let defs = [
            {ArgInfo.Command=fileCommand  ; Description="Workbook to run tabular on"                      ; Required=true  };
            {ArgInfo.Command=outputCommand; Description="output name"                                     ; Required=false };
            {ArgInfo.Command=sheetCommand ; Description="sheets containing models on which to run tabular"; Required=false } ]

      //let args = [filename1; filename2] |> List.map ((+) " ")  |> List.fold (+) ""
      //let args = [command1] |> List.map ((+) " --" >> (+.) args)  |> List.fold (+) ""
      let args = [filename1; filename2] 
      let args = [command1] |> List.map ((+) "--") |> List.map (fun e -> [[e];args]|> List.concat) |> List.concat
      let d = ParserCLI.parseArgs defs args

      Assert.True(d.ContainsKey(fileCommand) && 
                 (d.[fileCommand] |> List.tryFind((=) filename1) |> Option.isSome) &&
                 (d.[fileCommand] |> List.tryFind((=) filename2) |> Option.isSome))

   [<Fact>] 
   let ``CLI parser retrieves two empty valued commands`` () =
      let filename = "toto"
      let command1, command2  = fileCommand, outputCommand
      let defs = [
            {ArgInfo.Command=fileCommand  ; Description="Workbook to run tabular on"                      ; Required=true  };
            {ArgInfo.Command=outputCommand; Description="output name"                                     ; Required=false };
            {ArgInfo.Command=sheetCommand ; Description="sheets containing models on which to run tabular"; Required=false } ]

//      let args = [] |> List.map ((+) " ")  |> List.fold (+) ""
//      let args = [command1;command2] |> List.map ((+) " --" >> (+.) args)  |> List.fold (+) ""  
      let args = [] 
      let args = [command1;command2] |> List.map ((+) "--") |> List.map (fun e -> [[e];args]|> List.concat) |> List.concat
      let d = ParserCLI.parseArgs defs args

      Assert.True(d.ContainsKey(command1) &&
                  d.ContainsKey(command2))

   [<Fact>] 
   let ``CLI parser retrieves two multiple valued commands`` () =
      let filename1, filename2 = "toto1", "toto2"
      let command1, command2  = fileCommand, outputCommand

      let defs = [
            {ArgInfo.Command=fileCommand  ; Description="Workbook to run tabular on"                      ; Required=true  };
            {ArgInfo.Command=outputCommand; Description="output name"                                     ; Required=false };
            {ArgInfo.Command=sheetCommand ; Description="sheets containing models on which to run tabular"; Required=false } ]

//      let args = [filename1; filename2] |> List.map ((+) " ")  |> List.fold (+) ""
//      let args = [command1;command2] |> List.map ((+) " --" >> (+.) args)  |> List.fold (+) "" 
      let args = [filename1; filename2] 
      let args = [command1;command2] |> List.map ((+) "--") |> List.map (fun e -> [[e];args]|> List.concat) |> List.concat
      let d = ParserCLI.parseArgs defs args


      Assert.True(d.ContainsKey(command1) &&
                 (d.[command1] |> List.tryFind((=) filename1) |> Option.isSome) &&
                 (d.[command1] |> List.tryFind((=) filename2) |> Option.isSome) &&
                  d.ContainsKey(command2) &&
                 (d.[command2] |> List.tryFind((=) filename1) |> Option.isSome) &&
                 (d.[command2] |> List.tryFind((=) filename2) |> Option.isSome))


   [<Fact>] 
   let ``CLI parser some real life command`` () =
      let command1, command2  = fileCommand, outputCommand
      let verboseCommand = "verbose"


      let defs = [
            {ArgInfo.Command=fileCommand  ; Description="Workbook to run tabular on"                      ; Required=true  };
            {ArgInfo.Command=outputCommand; Description="output name"                                     ; Required=false };
            {ArgInfo.Command=sheetCommand ; Description="sheets containing models on which to run tabular"; Required=false } 
            {ArgInfo.Command=verboseCommand    ; Description="verbose mode"                               ; Required=false } ]

      let d = ParserCLI.parseArgs defs ["--file";"PCA.xlsx";"--sheets";"tabular_pca";"--verbose"]

      Assert.True(d.ContainsKey(fileCommand) &&
                 (d.[fileCommand] |> List.tryFind((=) "PCA.xlsx") |> Option.isSome) &&
                  d.ContainsKey(sheetCommand) &&
                 (d.[sheetCommand] |> List.tryFind((=) "tabular_pca") |> Option.isSome) &&
                  d.ContainsKey(verboseCommand))





