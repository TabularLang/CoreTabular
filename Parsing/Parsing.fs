namespace MicrosoftResearch.Infer.Tabular

module Parsing =
  open Microsoft.FSharp.Text.Lexing
  open Lexer
  open Parser
  open System
  open Syntax
  open Microsoft.FSharp.Reflection
  open Microsoft.FSharp.Quotations.Patterns

  let parseFromString (f: _ -> _ -> 'T)(s: string) =
      f Lexer.tokenize (LexBuffer<char>.FromString(s))

  let ParseExp             s = parseFromString Parser.Exp               s 
  let ParseExpList         s = parseFromString Parser.ExpList           s 
  let ParseBindings        s = parseFromString Parser.Bindings          s 
  let ParseModel           s = parseFromString Parser.Model             s 
  let ParseColumnType      s = parseFromString Parser.ColumnType        s
  let ParseTableName       s = parseFromString Parser.TableId           s
  let ParseColumnName      s = parseFromString Parser.ColumnName        s
  let ParseMarkup          s = parseFromString Parser.Markup            s
  let ParseMarkupOf        s = parseFromString Parser.MarkupOf         s
  let ParseSimpleString    s = parseFromString Parser.SimpleString      s
  let ParseSettingsTableId s = parseFromString Parser.SettingsTableId   s
  let ParseIsEmpty         s = try let _ = parseFromString Parser.EmptyCell s in true with | _ -> false

  let init() =
      Pretty.escape :=
          fun (s:string) ->
           try match Lexer.tokenize (LexBuffer<char>.FromString(s)) with 
               | token.IDENT s' -> if s' = s then s else "#\"" + s + "\"" 
               | _ -> "#\"" + s  + "\""
           with _ -> "#\"" + s  + "\""


