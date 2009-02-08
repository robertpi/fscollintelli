#light
module Strangelights.HierarchicalClustering.DataAccess
open System
open System.Net
open System.Xml.XPath
open Strangelights.Extensions.Net

/// builds a generic worflow that returns the result of action or a
/// default result in the case of an error
let getContents progress (url: string) action errRes =
  async { try
            progress (Printf.sprintf "url: %s" url)
            let req = WebRequest.Create(url)
            use! resp = req.GetResponseAsync()
            use stream = resp.GetResponseStream()
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

