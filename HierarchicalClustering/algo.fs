#light
namespace Strangelights.HierarchicalClustering
open System
open System.Net
open System.IO
open System.Xml.XPath
open System.Text.RegularExpressions
open System.Xml
open Strangelights.Extensions
open Strangelights.HierarchicalClustering

type Blog = 
    { Title: string;
      Url: string;
      Wordcount: Map<string,float>; }

/// The internal type of messages for the agent
type Message = 
    | NewBlog of Blog 
    | Fetch of AsyncReplyChannel<Map<string,float> * seq<Blog>>
 
type ReceiveBlogAgent() =
    let counter = MailboxProcessor.Start(fun inbox ->
             // The states of the message processing state machine...
             let rec loop((masterList: Map<string,float>), (blogs: list<Blog>)) =
                async { let! msg = inbox.Receive()
                        match msg with
                        | NewBlog ({ Wordcount = wc } as blog) ->
                            let masterList = MapOps.mergeFloatMap masterList wc
                            return! loop( masterList, blog :: blogs)
                               
                        | Fetch  replyChannel  ->
                            // post response to reply channel and continue
                            do replyChannel.Reply(masterList, Seq.of_list blogs)
                            return () }
 
             // The initial state of the message processing state machine...
             loop(Map.empty, []))
    member a.AddBlog(n) = counter.Post(NewBlog(n))
    member a.Fetch() = counter.PostAndReply(fun replyChannel -> Fetch replyChannel )

/// Type thtat will represent the cluster tree
type BiculsterNode =
    { Name: option<string>;
      Url: option<string>;
      Left: option<BiculsterNode>;
      Right: option<BiculsterNode>;
      Distance: option<float>;
      Cluster: Map<string,float>;
      OriginalCluster: option<Map<string,float>> }

type Result = 
    { BiculsterTree: BiculsterNode;
      MasterWordList: Map<string, float>;
      ChosenWords: seq<string>;
      ProcessedBlogs: int }

module Algorithm =
    /// action that turns the Opml stream into a list of 
    /// pairs of titles and URL
    let treatOpml progress (stream: Stream) =
        let xdoc = new XPathDocument(stream)
        let titles = DataAccess.queryXdoc xdoc "opml/body/outline/outline/@title"
        let urls = DataAccess.queryXdoc xdoc "opml/body/outline/outline/@xmlUrl" 
        progress (Printf.sprintf "urls: %i" (Seq.length urls))
        progress (Printf.sprintf "titles: %i" (Seq.length titles))
        Seq.zip titles urls 

    /// regualar expressions of processing the HTML    
    let stripHtml = new Regex("<[^>]+>", RegexOptions.Compiled)
    let splitChars = new Regex(@"\s|\n|\.|,|\!|\?|“|”|…|\||\(|\)|\[|\]|\-", RegexOptions.Compiled)

    /// action that turns an RSS stream into a map of word/count pairs
    let treatRss title url (receiver: ReceiveBlogAgent) progress (stream: Stream) =
        progress (Printf.sprintf "title: %s" title)
        let xdoc = new XPathDocument(stream)
        let treatText (html: string) =
            let text = stripHtml.Replace(html, "")
            let words = splitChars.Split(text)
            seq { for word in words do
                    if word <> "" then yield word.ToLower() }
        let countWords acc word =
            if Map.mem word acc then
                let count = (Map.find word acc) + 1.
                Map.add word count acc
            else
                Map.add word 1. acc
        let treatPostList posts =
            let words = Seq.concat (Seq.map (fun post -> treatText post) posts)
            PSeq.fold countWords Map.empty words
                
        let posts = DataAccess.queryXdoc xdoc "rss/channel/item/description"
        progress (Printf.sprintf "items: %i" (Seq.length posts))
        let wordMap = treatPostList posts
        let blog = { Title = title; Url = url; Wordcount = wordMap }
        receiver.AddBlog blog


    /// download opml stream then turn into title/url paris
    let opmlFileToTitleUlrs progress url limit = 
        let urlsWorkflow = DataAccess.getContents progress url treatOpml (Seq.of_list [])
        let urls = Async.Run urlsWorkflow
        Seq.take limit urls

    /// download rss files and turn into word/count maps     
    let titleUlrsToWordCountMap progress urls = 
        let receiver = new ReceiveBlogAgent()
        let flows = 
            urls 
            |> Seq.map (fun (title, url) -> 
                DataAccess.getContents progress url (treatRss title url receiver) ())
        try
            Async.Run (Async.Parallel flows, timeout = 30000) |> ignore
        with :? TimeoutException -> progress "Request timed out"
        receiver.Fetch()

    /// turns a list of cluster nodes into a hierarchal cluster tree
    let buildClusterTree progress clusters =
        let keys m = Map.to_seq m |> PSeq.map snd
        let compareNodes { Cluster = c1 } { Cluster = c2 } =
            let wc1, wc2 = keys c1, keys c2
            Distributions.pearson wc1 wc2
        let initComparisons = 
            progress (Printf.sprintf "Building initial comparison set ...")
            SeqenceOps.combinations2 clusters
            |> PSeq.map (fun (c1, c2) -> compareNodes c1 c2, (c1, c2))
            |> Map.of_seq
        let averageWordMap wc1 wc2 =
            Seq.map2 (fun (word, v1) (_, v2) -> word, (v1 + v2) / 2.) (Map.to_list wc2) (Map.to_list wc1)
            |> Map.of_seq
        let rec innerBuildTree comparisons clusters =
            let first = Map.first (fun dist culst ->  Some(dist, culst)) comparisons
            let (dist, (c1, c2)) = Option.get first
            progress (Printf.sprintf "clusters: %i comparisons: %i" (Seq.length clusters) (Seq.length comparisons))
            let restComps = Map.filter (fun _ (c1', c2') -> not (c1 = c1' || c2 = c2' || c1 = c2' || c1' = c2)) comparisons
            let node = { Name = None;
                         Url = None
                         Left = Some c1;
                         Right = Some c2;
                         Distance = Some dist;
                         Cluster = averageWordMap c1.Cluster c2.Cluster;
                         OriginalCluster = None }
            let restClusters = Seq.filter (fun x -> not (x = c1 || x = c2)) clusters
            let newComps = PSeq.map (fun c -> compareNodes node c, (node, c)) restClusters
            let comparisons = PSeq.fold (fun acc (dist, comps) -> Map.add dist comps acc) restComps newComps
            if Seq.length restClusters = 0 then
                node
            else
                let clusters = Seq.append [ node ] restClusters
                innerBuildTree comparisons clusters
        innerBuildTree initComparisons clusters


    /// turn word count list into a hierarical cluster
    let clusterWordCounts progress lowerBound upperBound masterList blogs =
        // remove words that are uncommon or too common
        let remove (wordCount: Map<_, _>) =
            let count = (float wordCount.Count)
            wordCount |> Map.filter (fun word wc -> 
                let frac = wc / count
                //printfn "%s: %i / %i = %f" word wc count frac
                lowerBound < frac && frac < upperBound)

        // build a master word list containing all the words we interested in
        let masterWordList =
            masterList
            |> remove
            |> Map.to_seq
            |> Seq.map fst
        
        // ensure wordcount only contains words from master list    
        let addjustToMasterList wordCount =
            masterWordList 
            |> Seq.map (fun word -> 
                match Map.tryfind word wordCount with
                | Some count -> word, count
                | None -> word, 0.)
            |> Map.of_seq

        // build inital cluster list
        let clustersList =
            blogs
            |> Seq.sort_by (fun { Title = title } -> title)
            |> PSeq.map (fun { Title = title; Url = url; Wordcount = wordmap } -> 
                { Name = Some title;
                  Url = Some url;
                  Left = None;
                  Right = None;
                  Distance = None;
                  Cluster = addjustToMasterList wordmap;
                  OriginalCluster = Some wordmap})

        buildClusterTree progress clustersList, masterWordList

    /// process a word count into a hierarical cluster.
    let processOpml progress url lowerLimit upperLimit limit =
        let masterList, blogs =
            opmlFileToTitleUlrs progress url limit
            |> titleUlrsToWordCountMap progress
        let clusterTree, chosen =
            Seq.filter (fun { Wordcount = wc } -> not (Map.is_empty wc)) blogs
            |> clusterWordCounts progress lowerLimit upperLimit masterList
        { BiculsterTree = clusterTree; MasterWordList = masterList;
          ChosenWords = chosen; ProcessedBlogs = Seq.length blogs}