#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/
#if INTERACTIVE
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

open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Media3D

type MutliDScaling2DViewer(data: List<MultiDResult>) as x =
    inherit FrameworkElement()
    override x.OnRender(dc: DrawingContext) =
        let width, height =  x.ActualWidth, x.ActualHeight
        for { DataName = label; Location = loc } in data do
            let x, y, color =
                match loc with
                | [ x; y ] -> x, y, None
                | [ x; y; color ] -> x, y, Some color
                | _ -> failwith (Printf.sprintf "unsupported dims: %i" (List.length loc))
            let x, y = x * width, y * height
            let brush = 
                match color with
                | None -> Brushes.Black
                | Some x ->
                    let red = byte (x * 255.)
                    let blue = byte ((1. - x) * 255.)
                    new SolidColorBrush(Color.FromRgb(red, byte 0, blue))
            let text = new FormattedText(label, CultureInfo.GetCultureInfo("en-us"),
                                         FlowDirection.LeftToRight,
                                         new Typeface("Verdana"),
                                         10., brush)
            dc.DrawText(text, new Point(x + 5., y - 7.))

let progress = printfn "%s"

type Tweeter =
    { Id: int;
      Name: string;
      ScreenName: string;
      PictureUrl: string; } 

let treatTweeter name progress (stream: Stream) =
    let xdoc = new XPathDocument(stream)
    let nav = xdoc.CreateNavigator()
    let xpath = nav.Compile("users/user")
    let iter = nav.Select(xpath)
    name,
    seq { for x in iter -> 
            let x  = x :?> XPathNavigator
            let getValue (nodename: string) =
                let node = x.SelectSingleNode(nodename)
                node.Value
            { Id = Int32.Parse(getValue "id");
              Name = getValue "name";
              ScreenName = getValue "screen_name";
              PictureUrl = getValue "profile_image_url"; } }
//    let ids = HttpXml.queryXdoc xdoc "users/user/id"
//    let names = HttpXml.queryXdoc xdoc "users/user/name" 
//    let screenName = HttpXml.queryXdoc xdoc "users/user/screen_name" 
//    let imageUrl = HttpXml.queryXdoc xdoc "users/user/profile_image_url" 
//    let seq1 = Seq.zip ids names
//    let seq2 = Seq.zip screenName imageUrl
//    name,
//    Seq.zip seq1 seq2
//    |> Seq.map (fun ((id, name), (sn, url)) -> 
//        { Id = Int32.Parse(id);
//          Name = name;
//          ScreenName = sn;
//          PictureUrl = url; })

let friendsUrl = Printf.sprintf "http://twitter.com/statuses/friends/%s.xml"

let getAllFriendsOfFriends name = 
    let url = friendsUrl name
    let name, friends = Async.Run (HttpXml.getContents (fun _ -> ()) url (treatTweeter name) ("", Seq.empty))
    let friendsScreenName = Seq.map (fun { ScreenName = sn } -> sn) friends
    let friendsOfFriendsWorkflows = Seq.map (fun sn -> HttpXml.getContents (fun _ -> ()) (friendsUrl sn) (treatTweeter sn) ("", Seq.empty)) friendsScreenName
    friendsScreenName, Async.Run (Async.Parallel friendsOfFriendsWorkflows)
    

let fof = 
    let friendsScreenName, fof = getAllFriendsOfFriends "robertpi"
    let input =
        Seq.map (fun (name, friends) -> name, Seq.map (fun fsn  -> if Seq.exists (fun { ScreenName = sn }-> fsn = sn) friends then 1. else 0.) friendsScreenName) fof
        |> Seq.map (fun (name, fvect) -> { DataName = name; Vector = fvect })
    MultiD.scaleDown progress 2 input 0.01


let viewer = MutliDScaling2DViewer(fof)

let window = new Window(Content = viewer)

let app = new Application()

do app.Run(window) |> ignore


//let getContents progress (url: string) username password (parameters: string) action errRes =
//  async { try
//            progress (Printf.sprintf "url: %s" url)
//            //let user = Convert.ToBase64String(Encoding.UTF8.GetBytes(username + ":" + password))
//            //let bytes = Encoding.ASCII.GetBytes(parameters)
//            let req = WebRequest.Create(url, 
//                                        Method = "POST",
//                                        ContentLength = Convert.ToInt64(bytes.Length),
//                                        ContentType = "application/x-www-form-urlencoded") :?> HttpWebRequest
//            req.ServicePoint.Expect100Continue <- false
//            //req.Headers.Add("Authorization", "Basic " + user)
//            use! resp = req.AsyncGetResponse()
//            use stream = resp.GetResponseStream()
//            return action progress stream
//          with ex ->
//            progress (Printf.sprintf "error for: %s" url)
//            return errRes }
