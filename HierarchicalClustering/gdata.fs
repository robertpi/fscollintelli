#light
open System
open Strangelights.Extensions
open Strangelights.HierarchicalClustering
let progress _ = ()
let makeUrl = Printf.sprintf "http://spreadsheets.google.com/feeds/list/%s/od6/public/values"
let urls =
    [ "phNtm3LmDZEP61UU2eSN1YA",
        [ "gsx:location", "";
          "gsx:hospitalbedsper10000population", "";
          "gsx:numberofdoctors", "";
          "gsx:numberofdentistrypersonnel", "";
          "gsx:numberofnursingandmidwiferypersonnel", "";
          "gsx:nursingandmidwiferypersonneldensityper10000population", "" ];
      "phNtm3LmDZEPVW3ee5eyISA",
        [ "gsx:location", ""; ] ]

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

let getData urls = 
    let getData (key, colNames) = 
        async { let url = makeUrl key
                let cols = Seq.map fst colNames
                let names = Seq.skip 1 (Seq.map snd colNames)
                let! data = DataAccess.getGoogleSpreadSheet progress url cols
                return Seq.map (createLocation names) data  }
    Async.Run (Async.Parallel (List.map getData urls))

let processData() =
    let data = Seq.concat (getData urls)
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
    |> Seq.map (fun loc ->
        let counts = Seq.map (fun (name,value) -> name, Option.get value) loc.NameValuesList
        { NameValueParis = Map.of_seq counts; NodeDetails = Leaf loc.Country; })

//let buildCluster items =
//    let counts = 
//        [ "beds", float (Seq.nth 1 items);
//          "nurse", float (Seq.nth 5 items) ]
//    let x =
//        Leaf { Name = Seq.nth 0 items;
//               Url = "";
//               OriginalWordCount = Map.of_list counts }
//    { WordCount = Map.of_list counts;
//      NodeDetails = x; }
//
//let clusters = Seq.map buildCluster rawData
//Clustering.buildClusterTree progress clusters
//
