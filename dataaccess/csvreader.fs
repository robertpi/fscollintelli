#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools.DataAccess
open System
open System.IO
open System.Collections
open System.Reflection
open Microsoft.FSharp.Reflection
open Strangelights.DataTools.Extensions

type CsvReader<'a>(s: Stream, ?skipRows: int, ?dateFormat: string) =
    do if not (FSharpType.IsTuple(typeof<'a>)) then
         failwith "Type parameter must be a tuple"
    let elements = FSharpType.GetTupleElements(typeof<'a>)
    let getParseFunc t =
        match t with
        | _ when t = typeof<string> ->
            fun x -> x :> obj
        | _ when t = typeof<DateTime> ->
            let parser =
                match dateFormat with
                | Some format -> fun s -> DateTime.ParseExact(s, format, null)
                | None -> DateTime.Parse
            fun x -> parser x :> obj
        | _  ->
            let parse = t.GetMethod("Parse", BindingFlags.Static ||| BindingFlags.Public, null, [| typeof<string> |], null)
            fun (s: string) ->
                parse.Invoke(null, [| box s |]) 
    let funcs = Seq.cmap getParseFunc elements
    let stream = new StreamReader(s)
    let lines = 
        seq { while not stream.EndOfStream do
               yield stream.ReadLine() }
    let lines = 
        match skipRows with
        | Some x -> Seq.skip x lines
        | None -> lines
    let parseRow row = 
        let items =
            Seq.zip (List.of_array row) funcs
            |> Seq.map (fun (ele, parser) -> parser ele)
        FSharpValue.MakeTuple(Array.of_seq items, typeof<'a>)
    let items = 
        lines 
        |> Seq.map (fun x -> (parseRow (x.Split([|','|]))) :?> 'a)
        |> Seq.to_list 
    interface seq<'a> with
        member x.GetEnumerator() = 
           let seq = Seq.of_list items
           seq.GetEnumerator()
    interface IEnumerable with
        member x.GetEnumerator() = 
           let seq = Seq.of_list items
           seq.GetEnumerator() :> IEnumerator
