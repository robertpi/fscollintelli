#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools.Treatment
#if INTERACTIVE
#I @"C:\Program Files\Reference Assemblies\Microsoft\Framework\v3.5"
#r "System.Threading"
#r "System.Core"
#r "FSharp.PowerPack"
#r "PresentationCore";;
#r "PresentationFramework";;
#r "WindowsBase";;
#load "../misc/extensions.fs"
#load "../clustering/multidscaling.fs"
#load "../dataaccess/httpxml.fs"
#endif
open System
open System.IO
open System.Net
open System.Text
open System.Xml
open System.Xml.XPath
open System.Globalization
open Strangelights.DataTools.Extensions
open Strangelights.DataTools.DataAccess
open Strangelights.DataTools.Clustering
open Strangelights.DataTools.Optimization

type Tweeter =
    { Id: int;
      Name: string;
      ScreenName: string;
      PictureUrl: string;
      FriendsCount: int } 

module Twitter =
    let progress = printfn "%s"

    let treatTweeter prefix progress (stream: Stream) =
        let xdoc = new XPathDocument(stream)
        let nav = xdoc.CreateNavigator()
        let xpath = nav.Compile(prefix + "user")
        let iter = nav.Select(xpath)
        [ for x in iter -> 
            let x  = x :?> XPathNavigator
            let getValue (nodename: string) =
                let node = x.SelectSingleNode(nodename)
                node.Value
            { Id = Int32.Parse(getValue "id");
              Name = getValue "name";
              ScreenName = getValue "screen_name";
              PictureUrl = getValue "profile_image_url";
              FriendsCount = Int32.Parse(getValue "friends_count") } ]

    let treatIds id progress (stream: Stream) =
//        let reader = new StreamReader(stream)
//        File.WriteAllText((sprintf @"C:\code\Strangelights.DataTools\examples\%i.xml" id), reader.ReadToEnd())
        let xdoc = new XPathDocument(stream)
        let nav = xdoc.CreateNavigator()
        let xpath = nav.Compile("ids/id")
        let iter = nav.Select(xpath)
        let items =
            [ for x in iter do
                yield Int32.Parse(x.ToString()) ]
        id, items

    let friendsUrl = sprintf "http://twitter.com/statuses/friends/%s.xml?page=%i"
    let showUserUrl = sprintf "http://twitter.com/users/show/%s.xml"
    let friendsIdUrl = sprintf "http://twitter.com/friends/ids.xml?user_id=%i"
    
    let getAllFriends progress name =
        let [ tweeter ] = Async.RunSynchronously (HttpXml.getContents progress (showUserUrl name) (treatTweeter "") []) 
        printfn "count: %i" tweeter.FriendsCount
        let urls = List.map (friendsUrl name) [ 0 .. (tweeter.FriendsCount / 100) + 1 ]
        let friends =
            Async.RunSynchronously (Async.Parallel (List.map (fun url -> HttpXml.getContents progress url (treatTweeter "users/") []) urls))
            |> Seq.toList |> List.concat
        tweeter, friends

    let getAllFof id (ids: list<int>) = 
        let initIdsSet = Set.ofList (id :: ids)
        let friendsOfFriendsWorkflows = Seq.map (fun sn -> HttpXml.getContents progress (friendsIdUrl sn) (treatIds sn) (0, [])) ids
        let res = Async.RunSynchronously (Async.Parallel friendsOfFriendsWorkflows)
        let allIds = List.map (fun (id, ids) -> id, List.filter (fun id -> Set.contains id initIdsSet) ids) (List.ofArray res)
        let prepClusterNodes (id, ids) =
            let idSet = Set.ofList ids 
            id, Seq.map (fun id -> id, if Set.contains id idSet then 1. else 0.) initIdsSet |> List.ofSeq
        let hcids = List.map prepClusterNodes allIds
//        let initScaleNodes (id,ids) = { DataName = string id; Vector = List.map snd ids }
//        let mdScaleNodes = List.map initScaleNodes hcids
//        let coords = MultiD.scaleDown progress 2 mdScaleNodes 0.01 |> List.map (fun { Location = [x; y] } -> x, y)
        let initClusterNodes allIds = 
            let idMap ids = 
                Seq.map (fun (id, connected) -> string id, connected) ids 
                |> Map.ofSeq
            let convert (id, ids) = 
                { NodeDetails = Leaf id; NameValueParis = idMap ids }
            List.map convert allIds
        let clusterTree =  Clustering.buildClusterTree progress (initClusterNodes hcids)
//        let rec getHeight t =
//            match t with
//            | Node { Left = l; Right = r } -> 
//                (getHeight l.NodeDetails) + (getHeight r.NodeDetails)
//            | Leaf x -> 1.
//        let rec getNodePos t (step: float) x y acc =
//            match t with
//            | Node { Left = l; Right = r } -> 
//                let acc = (getNodePos l.NodeDetails step (x + step) (y - step) acc) 
//                (getNodePos r.NodeDetails step (x - step) (y + step) acc)
//            | Leaf _ -> (x,y) :: acc
//        let height = getHeight clusterTree.NodeDetails
//        let coords = getNodePos clusterTree.NodeDetails (1. / height) 0.5 0.5 [] 
        let rec getNodes t acc =
            match t with
            | Node { Left = l; Right = r } -> 
                let acc = (getNodes l.NodeDetails acc) 
                (getNodes r.NodeDetails acc)
            | Leaf x -> x :: acc
        let nodes = getNodes clusterTree.NodeDetails []
        let theta = (Math.PI * 2.0) / double (List.length nodes) 
        // X = R cos(Theta) + Xo, Y = R sin(Theta) + Yo
        let nodeCoord = 
            nodes 
            |> List.mapi (fun step id ->
                let r = (double ((step % 3) + 1) * 0.1)
                let step = double step
                id, ((r * cos(step * theta)) + 0.5, (r * sin(step * theta)) + 0.5))
            |> Map.ofList
        let initSol = 
            allIds
            |> List.map (fun (id,ids) -> 
                let x, y = nodeCoord.[id]
                [ x; y ] )
            |> List.concat
        let rec pairsOfFloats list =
            match list with
            | x :: y :: list -> (x,y) :: pairsOfFloats list
            | [] -> []
            | _ -> failwith "uneven list"
        let costFun points =
            let cords = List.zip allIds (pairsOfFloats (List.ofSeq points))
            let map = 
                List.map (fun ((id, _), cord) -> id, cord) cords
                |> Map.ofList 
            let connections = 
               seq { for (_, (x1, y1)) in cords do
                         yield (x1, y1), (0.5, 0.5)
                     for ((id, ids), (x1, y1)) in cords do
                        for id' in ids do
                            if map.ContainsKey id' then
                                let x2, y2 = map.[id']
                                yield (x1,y1), (x2,y2) }
            Seq.combinations2 (Set.ofSeq connections)
            |> Seq.fold (fun acc (((x1,y1), (x2,y2)), ((x3,y3), (x4,y4))) ->
                let den = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
                if den = 0. then
                    acc
                else
                    let ua = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / den
                    let ub = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) / den
                    if 0. < ua && ub < 1. && 0. < ub && ub < 1. then acc + 1.
                    else acc) 0. 
        let res = hillclimb initSol [ for _ in 1 .. 2 * (Seq.length allIds) do yield 0., 1. ] costFun 
        let coords = pairsOfFloats (List.ofSeq res)
        ((id, ids), (0.5, 0.5)) :: List.zip allIds coords 
        