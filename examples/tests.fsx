#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

#r "FSharp.PowerPack";;
#r "PresentationCore";;
#r "PresentationFramework";;
#r "System.Core";;
#r "System.Threading";;
#r "System.Xml";;
#r "WindowsBase";;

#load "../misc/extensions.fs";;
#load "../dataaccess/csvreader.fs";;
#load "../dataaccess/httpxml.fs";;
#load "../dataaccess/yfinance.fs";;
#load "../clustering/hierclustering.fs";;
#load "../clustering/multidscaling.fs";;
#load "../treatment/gdata.fs";;
#load "../treatment/blogs.fs";;
#load "../presentation/dendrogram.fs";;
#load "../presentation/tagcloud.fs";;

open Strangelights.DataTools.Extensions
open Strangelights.DataTools.Clustering
open Strangelights.DataTools.Treatment

// how to report progress
let progress = printfn "%s"

// input / parameters
let url = "http://www.currybet.net/download/opml/top100ukblogs.xml"
let limit = 20
let timeout = 30000
let lowerLimit = 0.003
let upperLimit = 0.2

// get the URLs and titles from the "OPML" file
let urls = Async.Run(HttpXml.getContents progress url BlogTreatment.treatOpml (Seq.of_list []))
let urls' = Seq.take limit urls

// download the URLs 
let masterList, blogs = BlogTreatment.titleUlrsToWordCountMap progress timeout urls'

let clusterTree, chosen =
    Seq.filter (fun { BlogWordCount = wc } -> not (Map.is_empty wc)) blogs
    |> BlogTreatment.clusterWordCounts progress lowerLimit upperLimit masterList

let too = BlogTreatment.processOpmlToMdScaling progress false url lowerLimit upperLimit limit timeout
let mds = new MutliDScaling2DViewer(List.of_seq too)

let window = new Window(Content = mds)
window.Show()

open System.IO

DataAccess.getContents progress url (fun _ stream -> 
            let sr = new StreamReader(stream)
            File.WriteAllText(@"D:\code\github\HierarchicalClustering\data\blogs\top100ukblogs.opml", sr.ReadToEnd()))
            ()
            
Async.Run it

let safeName (s: string) =
    let chars = Path.GetInvalidFileNameChars()
    chars |>
    Seq.fold (fun (s: string) c ->
        s.Replace(c.ToString(), "")) s

let getBlog (name, url) =
    DataAccess.getContents progress url (fun _ stream -> 
                let sr = new StreamReader(stream)
                File.WriteAllText(Path.Combine(@"D:\code\github\HierarchicalClustering\data\blogs", (safeName name) + ".xml"), sr.ReadToEnd()))
                ()

Async.Run(Async.Parallel(Seq.map getBlog urls))