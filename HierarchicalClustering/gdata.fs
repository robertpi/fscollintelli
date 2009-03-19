#light
open System
open Strangelights.Extensions
open Strangelights.HierarchicalClustering

type Location =
    { Country: string;
      NameValuesList: seq<string * option<float>> }

let createLocation names row  =
    let country = Seq.hd row
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
                let! data = DataAccess.getGoogleSpreadSheet progress url cols
                return Seq.map (createLocation names) data  }
    Async.Run (Async.Parallel (List.map getData urls))

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
    |> Seq.filter (fun { NameValuesList = vals } -> not (Seq.exists (fun (_, value) -> Option.is_none value) vals))
    //|> Seq.take 10
    |> Seq.map (fun loc ->
        let counts = Seq.map (fun (name,value) -> name, Option.get value) loc.NameValuesList
        { NameValueParis = Map.of_seq counts; NodeDetails = Leaf loc.Country; })

let reverseMatrix initalNodes =
    let node = Seq.hd initalNodes
    let values = Seq.map fst (Map.to_seq node.NameValueParis)
    let getName node =
       match node.NodeDetails with
       | Leaf s -> s
       | Node _ -> failwith "must be leaft"
    let reverse value =
        let counts =
            initalNodes |> Seq.map (fun node -> getName node, node.NameValueParis.[value])
        { NameValueParis = Map.of_seq counts; NodeDetails = Leaf value; }
    Seq.map reverse values
