#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

module Strangelights.DataTools.DataAccess.YahooFinance
open System
open Strangelights.DataTools.DataAccess

let baseUrl = Printf.sprintf "http://ichart.finance.yahoo.com/table.csv?s=%s"

let getStockInfo progress symbol =
    let parseStream _ stream =
        new CsvReader<DateTime * float * float * float * float * int * float>(stream, 1, "yyyy-MM-dd") :> seq<_>
    HttpXml.getContents progress (baseUrl symbol) parseStream Seq.empty
