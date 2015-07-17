namespace MicrosoftResearch.Infer.Tabular


module SchemaGraph =
  
   let ItemsTS getTableByName getDependencies projection x = 
      let visited = ref Set.empty
      let rec dfs (tableName:string) (visited:Set<_> ref) = [
           if not ((!visited).Contains tableName) then
             visited := (!visited).Add tableName
             let table = getTableByName tableName
             for t in getDependencies(table) do
              yield! dfs t visited
             yield projection tableName  ]

      x |> List.fold(fun (visited, previousseq) (tname) -> (visited, (dfs tname visited)@previousseq)) (visited, List.empty) |> snd |> List.rev

