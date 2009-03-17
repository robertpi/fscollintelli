#light

#r "PresentationCore";;
#r "PresentationFramework";;
#r "System.Core";;
#r "System.Threading";;
#r "System.Xml";;
#r "WindowsBase";;

#load "extensions.fs";;
#load "dataAccess.fs";;
#load "clustering.fs";;
#load "gdata.fs";;
#load "algo.fs";;

open Strangelights.Extensions
open Strangelights.HierarchicalClustering

// how to report progress
let progress = printfn "%s"

// input / parameters
let url = "http://www.currybet.net/download/opml/top100ukblogs.xml"
let limit = 20
let timeout = 30000
let lowerLimit = 0.003
let upperLimit = 0.2

// get the URLs and titles from the "OPML" file
let urls = Async.Run(DataAccess.getContents progress url BlogTreatment.treatOpml (Seq.of_list []))
let urls' = Seq.take limit urls

// download the URLs 
let masterList, blogs = BlogTreatment.titleUlrsToWordCountMap progress timeout urls'

let clusterTree, chosen =
    Seq.filter (fun { BlogWordCount = wc } -> not (Map.is_empty wc)) blogs
    |> BlogTreatment.clusterWordCounts progress lowerLimit upperLimit masterList

Clustering.buildClusterTree progress (Gdata.processData())