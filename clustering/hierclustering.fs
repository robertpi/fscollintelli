#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools.Clustering
open Strangelights.DataTools.Extensions

type BiculsterNodeDetails<'a> =
    | Node of NodeDetails<'a>
    | Leaf of 'a
    
and NodeDetails<'a> =
    { Left: BiculsterNode<'a>;
      Right: BiculsterNode<'a>;
      Distance: float; }
// TODO the NameValueParis field seems a bit wrong we only really need a vector here 
// (but we rely on having the name went outputing the results)
and BiculsterNode<'a> =
    { NameValueParis: Map<string,float>;
      NodeDetails: BiculsterNodeDetails<'a>; }

module Clustering =
    /// turns a list of cluster nodes into a hierarchal cluster tree
    let buildClusterTree progress clusters =
        let keys m = Map.to_seq m |> Seq.cmap snd
        let compareNodes { NameValueParis = c1 } { NameValueParis = c2 } =
            let wc1, wc2 = keys c1, keys c2
            1. - abs (Measures.pearson wc1 wc2)
        let initComparisons = 
            progress (Printf.sprintf "Building initial comparison set ...")
            let clusters = Set.to_seq (Set.of_seq clusters)
            let clusterParis = Seq.combinations2 clusters
            //Seq.iter (fun (x,y) -> if x = y then System.Windows.MessageBox.Show ("found pair " + (any_to_string x) + (any_to_string y)) |> ignore) toto
            clusterParis
            |> Seq.cmap (fun (c1, c2) -> compareNodes c1 c2, (c1, c2))
            |> Map.of_seq
        let averageWordMap wc1 wc2 =
            Seq.map2 (fun (word, v1) (_, v2) -> word, (v1 + v2) / 2.) (Map.to_list wc2) (Map.to_list wc1)
            |> Map.of_seq
        let rec innerBuildTree comparisons clusters =
            let first = Map.first (fun dist culst ->  Some(dist, culst)) comparisons
            let (dist, (c1, c2)) = Option.get first
            progress (Printf.sprintf "clusters: %i comparisons: %i" (Seq.length clusters) (Seq.length comparisons))
            //if c1 = c2 then System.Windows.MessageBox.Show (Printf.sprintf "clusters: %i comparisons: %i" (Seq.length clusters) (Seq.length comparisons)) |> ignore
            let restComps = Map.filter (fun _ (c1', c2') -> not (c1 = c1' || c2 = c2' || c1 = c2' || c1' = c2)) comparisons
            let node = { NameValueParis = averageWordMap c1.NameValueParis c2.NameValueParis;
                         NodeDetails = Node { Left = c1;
                                              Right = c2;
                                              Distance = dist; } }
            let restClusters = Seq.filter (fun x -> not (x = c1 || x = c2)) clusters
            let newComps = Seq.cmap (fun c -> compareNodes node c, (node, c)) restClusters
            let comparisons = Seq.fold (fun acc (dist, comps) -> Map.add dist comps acc) restComps newComps
            if Seq.length restClusters = 0 then
                node
            else
                let clusters = Seq.append [ node ] restClusters
                innerBuildTree comparisons clusters
        innerBuildTree initComparisons clusters
