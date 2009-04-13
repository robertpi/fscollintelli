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
#load "httpxml.fs"
#endif
open System
open System.IO
open System.Net
open System.Text
open System.Xml
open System.Xml.XPath
open Strangelights.DataTools.DataAccess

let postTweet username password tweet =
    let user = Convert.ToBase64String(Encoding.UTF8.GetBytes(username + ":" + password))
    // determine what we want to upload as a status
    let bytes = Encoding.ASCII.GetBytes("status=" + tweet)
    // connect with the update page
    let request = WebRequest.Create("http://twitter.com/statuses/update.xml", 
                                    Method = "POST",
                                    ContentLength = Convert.ToInt64(bytes.Length),
                                    ContentType = "application/x-www-form-urlencoded") :?> HttpWebRequest
    request.ServicePoint.Expect100Continue <- false
    // set the authorisation levels
    request.Headers.Add("Authorization", "Basic " + user)
   
    // set up the stream
    use reqStream = request.GetRequestStream()
    reqStream.Write(bytes, 0, bytes.Length)
    reqStream.Close()

type Tweeter =
    { Id: int;
      Name: string;
      ScreenName: string;
      PictureUrl: string; } 

let treatTweeter name progress (stream: Stream) =
    let xdoc = new XPathDocument(stream)
    let ids = HttpXml.queryXdoc xdoc "users/user/id"
    let names = HttpXml.queryXdoc xdoc "users/user/name" 
    let screenName = HttpXml.queryXdoc xdoc "users/user/screen_name" 
    let imageUrl = HttpXml.queryXdoc xdoc "users/user/profile_image_url" 
    let seq1 = Seq.zip ids names
    let seq2 = Seq.zip screenName imageUrl
    name,
    Seq.zip seq1 seq2
    |> Seq.map (fun ((id, name), (sn, url)) -> 
        { Id = Int32.Parse(id);
          Name = name;
          ScreenName = sn;
          PictureUrl = url; })

let friendsUrl = Printf.sprintf "http://twitter.com/statuses/friends/%s.xml"

let getAllFriendsOfFriends name = 
    let url = friendsUrl name
    let name, friends = Async.Run (HttpXml.getContents (fun _ -> ()) url (treatTweeter name) ("", Seq.empty))
    let friends = Seq.take 5 friends
    let friendsOfFriends = Seq.map (fun { ScreenName = sn } -> HttpXml.getContents (fun _ -> ()) (friendsUrl sn) (treatTweeter sn) ("", Seq.empty)) friends
    Async.Run (Async.Parallel friendsOfFriends)

getAllFriendsOfFriends "robertpi"

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
