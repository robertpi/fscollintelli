#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

module Strangelights.DataTools.DataAccess.YahooFinance
open System
open Strangelights.DataTools.DataAccess

let baseUrl symb sMonth sDay sYear fMonth fDay fYear = Printf.sprintf "http://ichart.finance.yahoo.com/table.csv?s=%s&a=%i&b=%i&c=%i&d=%i&e=%i&f=%i&g=d&ignore=.csv" symb sMonth sDay sYear fMonth fDay fYear 

let getStockInfo progress symbol (start: DateTime) (finish: DateTime) =
    let parseStream _ stream =
        new CsvReader<DateTime * float * float * float * float * int * float>(stream, 1, "yyyy-MM-dd") :> seq<_>
    HttpXml.getContents progress (baseUrl symbol (finish.Month - 1) finish.Day finish.Year (start.Month - 1) start.Day start.Year) parseStream Seq.empty
