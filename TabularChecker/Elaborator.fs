module MicrosoftResearch.Infer.Tabular.Elaborator

open Syntax
open Types
open Checker
open Model
open Table
open System.Collections.Generic
type Log = Map<TableName,Table.Log>


let logToString log =
                  Map.fold (fun s tb log -> 
                            Map.fold (fun s col v -> 
                                      match v with 
                                      |  (Table.Err msg) -> s+(sprintf "\nTable %A, column %A:\n %A" (Pretty.ident tb) (Pretty.ident col) msg) 
                                      | _ -> s) s log) "" log

let elaborate(fullSchema:Schema) =
      // reset fresh variable counter
      Syntax.counter <- 0
      // type Schema, adding Prelude, annotating derefs and bounds of indexed models
      let (log,err,(typedFullSchema,schemaType)) = Schema.typeSchema fullSchema
      if err 
      then (log,err,(typedFullSchema,schemaType))
      else 
           // erase type annotations
           let fullSchema = Erase.schema typedFullSchema
           // reduce 
           // System.Console.WriteLine(Pretty.schemaToStr fullSchema)
           let coreSchema = coreS fullSchema
           //System.Console.WriteLine("-----------")
           //System.Console.WriteLine(Pretty.schemaToStr coreSchema)
           // retypecheck
           Schema.synthSchema Types.G_Empty coreSchema
           

            
           
     