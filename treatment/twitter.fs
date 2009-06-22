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
            |> Seq.to_list |> List.concat
        tweeter, friends

    let getAllFof id (ids: list<int>) = 
        let friendsOfFriendsWorkflows = Seq.map (fun sn -> HttpXml.getContents progress (friendsIdUrl sn) (treatIds sn) (0, [])) ids
        let res = Async.RunSynchronously (Async.Parallel friendsOfFriendsWorkflows)
        let allIds = List.of_array res
        let rec pairsOfFloats list =
            match list with
            | x :: y :: list -> (x,y) :: pairsOfFloats list
            | [] -> []
            | _ -> failwith "uneven list"
        let costFun points =
            0.
        let res = annealing [ for _ in 1 .. 2 * (Seq.length allIds) do yield 0., 1. ] costFun 
        let coords = pairsOfFloats (List.of_seq res)
        ((id, ids), (0.5, 0.5)) :: List.zip allIds coords