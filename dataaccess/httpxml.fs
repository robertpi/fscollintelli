#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

module Strangelights.DataTools.DataAccess.HttpXml
open System
open System.IO
open System.Net
open System.Xml
open System.Xml.XPath
open Microsoft.FSharp.Control.WebExtensions
open Strangelights.DataTools.Extensions

/// builds a generic worflow that returns the result of action or a
/// default result in the case of an error
let getContents progress (url: string) action errRes =
  async { try
            progress (Printf.sprintf "url: %s" url)
            let req = WebRequest.Create(url)
            use! resp = req.AsyncGetResponse()
            use stream = resp.GetResponseStream()
            return action progress stream
          with ex ->
            printfn "%s" url
            printfn "%O" ex
            progress (Printf.sprintf "error for: %s" url)
            return errRes }
            
let getContentsLocal progress (url: string) action errRes =
  async { try
            progress (Printf.sprintf "url: %s" url)
            let stream = File.OpenRead(url)
            return action progress stream
          with _ ->
            progress (Printf.sprintf "error for: %s" url)
            return errRes }
            
/// query an XPathDocument and return the results as list of strings
let queryXdoc (xdoc: XPathDocument) xpath =
    let navTitle = xdoc.CreateNavigator()
    let xpathTitle = navTitle.Compile(xpath)
    let iterTitle = navTitle.Select(xpathTitle)
    seq { for x in iterTitle -> x.ToString() }

let namespaces =
    [ "at", "http://www.w3.org/2005/Atom";
      "openSearch", "http://a9.com/-/spec/opensearchrss/1.0/";
      "gsx", "http://schemas.google.com/spreadsheets/2006/extended" ]

let queryGoogleSpreadSheet (xdoc: XPathDocument) xpath columnNames =
    let nav = xdoc.CreateNavigator()
    let mngr = new XmlNamespaceManager(new NameTable())
    do List.iter (fun (prefix, url) -> mngr.AddNamespace(prefix, url)) namespaces
    let xpath = nav.Compile(xpath)
    do xpath.SetContext(mngr)
    let iter = nav.Select(xpath)
    seq { for x in iter -> 
            let x  = x :?> XPathNavigator
            let getValue nodename =
                let node = x.SelectSingleNode(nodename, mngr)
                node.Value
            Seq.cmap getValue columnNames }

let getGoogleSpreadSheet progress (url: string) columnNames =
  async { progress (Printf.sprintf "url: %s" url)
          let req = WebRequest.Create(url)
          use! resp = req.AsyncGetResponse()
          use stream = resp.GetResponseStream()
          let xdoc = new XPathDocument(stream)
          let titles = queryGoogleSpreadSheet xdoc "/at:feed/at:entry" columnNames
          return titles }
