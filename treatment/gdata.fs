// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/
module Strangelights.DataTools.Treatment.GData

open System
open Strangelights.DataTools.DataAccess
open Strangelights.DataTools.Extensions
open Strangelights.DataTools.Clustering

type Location =
    { Country: string;
      NameValuesList: seq<string * option<float>> }

let createLocation names row  =
    let country = Seq.head row
    let row = Seq.skip 1 row
    let tryParse s =
        let success,res = Double.TryParse s
        if success then Some res else None
    let values = Seq.map tryParse row
    { Country = country;
      NameValuesList = Seq.zip names values }

let getData progress makeUrl urls = 
    let getData (key, colNames) = 
        async { let url = makeUrl key
                let cols = Seq.map fst colNames
                let names = Seq.skip 1 (Seq.map snd colNames)
                let! data = HttpXml.getGoogleSpreadSheet progress url cols
                return Seq.map (createLocation names) data  }
    Async.RunSynchronously (Async.Parallel (List.map getData urls))

let processData progress makeUrls urls =
    let data = Seq.concat (getData progress makeUrls urls)
    let countries = Seq.fold (fun acc { Country = c } -> Set.add c acc) Set.empty data
    let creatMasterLocation country = 
        let allEntries = Seq.filter (fun { Country = c } -> c = country) data
        let allValues = 
            Seq.map (fun { NameValuesList = vals } -> vals) allEntries
            |> Seq.concat
        { Country = country;
          NameValuesList = allValues }
    Seq.map creatMasterLocation countries
    |> Seq.filter (fun { NameValuesList = vals } -> not (Seq.exists (fun (_, value) -> Option.isNone value) vals))
    //|> Seq.take 10 // uncomment to work with a smaller set and spead things up
    |> Seq.map (fun loc ->
        let counts = Seq.map (fun (name,value) -> name, Option.get value) loc.NameValuesList
        { NameValueParis = Map.ofSeq counts; NodeDetails = Leaf loc.Country; })

let reverseMatrix initalNodes =
    let node = Seq.head initalNodes
    let values = Seq.map fst (Map.toSeq node.NameValueParis)
    let getName node =
       match node.NodeDetails with
       | Leaf s -> s
       | Node _ -> failwith "must be leaft"
    let reverse value =
        let counts =
            initalNodes |> Seq.map (fun node -> getName node, node.NameValueParis.[value])
        { NameValueParis = Map.ofSeq counts; NodeDetails = Leaf value; }
    Seq.map reverse values
