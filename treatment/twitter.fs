#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

module Strangelights.DataTools.Treatment.Twitter
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

let progress = printfn "%s"

type Tweeter =
    { Id: int;
      Name: string;
      ScreenName: string;
      PictureUrl: string; } 

let treatTweeter name progress (stream: Stream) =
    progress (Printf.sprintf "Getting: %s" name)
    let xdoc = new XPathDocument(stream)
    let nav = xdoc.CreateNavigator()
    let xpath = nav.Compile("users/user")
    let iter = nav.Select(xpath)
    let items =
        [ for x in iter -> 
            let x  = x :?> XPathNavigator
            let getValue (nodename: string) =
                let node = x.SelectSingleNode(nodename)
                node.Value
            { Id = Int32.Parse(getValue "id");
              Name = getValue "name";
              ScreenName = getValue "screen_name";
              PictureUrl = getValue "profile_image_url"; } ]
    name, items
//    ((items
//    |> Seq.take (min (List.length items) 20)
//    |> Seq.cache) :> seq<_>)

let friendsUrl = Printf.sprintf "http://twitter.com/statuses/friends/%s.xml"

let getAllFriendsOfFriends name = 
    let url = friendsUrl name
    let name, friends = Async.Run (HttpXml.getContents progress url (treatTweeter name) ("", []))
    let friendsScreenName = Seq.map (fun { ScreenName = sn } -> sn) friends
    let friendsOfFriendsWorkflows = Seq.map (fun sn -> HttpXml.getContents progress (friendsUrl sn) (treatTweeter sn) ("", [])) friendsScreenName
    friendsScreenName, Async.Run (Async.Parallel friendsOfFriendsWorkflows)
    

let getFof firstScreenName = 
    let friendsScreenName, fof = getAllFriendsOfFriends firstScreenName
    Seq.map (fun (name, friends) -> name, Seq.map (fun fsn  -> if Seq.exists (fun { ScreenName = sn }-> fsn = sn) friends then 2. else 1.) friendsScreenName) fof
    |> Seq.map (fun (name, fvect) -> { DataName = name; Vector = fvect })

//Seq.length fofMatrix

let scaleDownRes fofMatrix =
    MultiD.scaleDown progress 2 fofMatrix 0.01
