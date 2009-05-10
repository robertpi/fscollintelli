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
      PictureUrl: string; } 

module Twitter =
    let progress = printfn "%s"

    let treatTweeter progress (stream: Stream) =
        let xdoc = new XPathDocument(stream)
        let nav = xdoc.CreateNavigator()
        let xpath = nav.Compile("users/user")
        let iter = nav.Select(xpath)
        [ for x in iter -> 
            let x  = x :?> XPathNavigator
            let getValue (nodename: string) =
                let node = x.SelectSingleNode(nodename)
                node.Value
            { Id = Int32.Parse(getValue "id");
              Name = getValue "name";
              ScreenName = getValue "screen_name";
              PictureUrl = getValue "profile_image_url"; } ]

    let getFriendsCount progress (stream: Stream) =
        let xdoc = new XPathDocument(stream)
        let nav = xdoc.CreateNavigator()
        let iter = nav.Select("user/friends_count")
        let items =
            [ for x in iter do
                yield x.ToString() ]
        match items with
        | [ followers ] -> Int32.Parse followers
        | _ -> failwith "assert false"

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
        let count = Async.Run (HttpXml.getContents progress (showUserUrl name) getFriendsCount 0) 
        printfn "count: %i" count
        let urls = List.map (friendsUrl name) [ 0 .. (count / 100) + 1 ] 
        Async.Run (Async.Parallel (List.map (fun url -> HttpXml.getContents progress url treatTweeter []) urls))
        |> Seq.to_list |> List.concat

    let getAllFof id (ids: list<int>) = 
        let friendsOfFriendsWorkflows = Seq.map (fun sn -> HttpXml.getContents progress (friendsIdUrl sn) (treatIds sn) (0, [])) ids
        let res = Async.Run (Async.Parallel friendsOfFriendsWorkflows)
        let allIds = (id, ids) :: List.of_array res
        let rec pairsOfFloats list =
            match list with
            | x :: y :: list -> (x,y) :: pairsOfFloats list
            | [] -> []
            | _ -> failwith "uneven list"
        let costFun points =
            0.
        let res = annealing [ for _ in 1 .. (Seq.length allIds) do yield 400., 400. ] costFun 
        pairsOfFloats (List.of_seq res)
        