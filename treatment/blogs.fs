#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools.Treatment
open System
open System.Net
open System.IO
open System.Xml.XPath
open System.Text.RegularExpressions
open System.Xml
open Strangelights.DataTools.DataAccess
open Strangelights.DataTools.Extensions
open Strangelights.DataTools.Clustering

type Blog = 
    { Title: string;
      Url: string;
      BlogWordCount: Map<string,float>; }

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
                        | NewBlog ({ BlogWordCount = wc } as blog) ->
                            let masterList = MapOps.mergeFloatMap masterList wc
                            return! loop( masterList, blog :: blogs)
                               
                        | Fetch  replyChannel  ->
                            // post response to reply channel and end
                            do replyChannel.Reply(masterList, Seq.of_list blogs)
                            return () }
 
             // The initial state of the message processing state machine...
             loop(Map.empty, []))
    member a.AddBlog(n) = counter.Post(NewBlog(n))
    member a.Fetch() = counter.PostAndReply(fun replyChannel -> Fetch replyChannel )

type BlogLeafDetails =
    { Name: string;
      Url: string;
      OriginalWordCount: Map<string,float> }
      
type Result = 
    { BiculsterTree: BiculsterNode<BlogLeafDetails>;
      MulitDScaling: seq<MultiDResult>;
      MasterWordList: Map<string, float>;
      ChosenWords: seq<string>;
      ProcessedBlogs: int }

module BlogTreatment =
    /// action that turns the Opml stream into a list of 
    /// pairs of titles and URL
    let treatOpml progress (stream: Stream) =
        let xdoc = new XPathDocument(stream)
        let titles = HttpXml.queryXdoc xdoc "opml/body/outline/outline/@title"
        let urls = HttpXml.queryXdoc xdoc "opml/body/outline/outline/@xmlUrl" 
        progress (Printf.sprintf "urls: %i" (Seq.length urls))
        progress (Printf.sprintf "titles: %i" (Seq.length titles))
        Seq.zip titles urls 

    /// regualar expressions of processing the HTML    
    let stripHtml = new Regex("<[^>]+>", RegexOptions.Compiled)
    let splitChars = new Regex(@"\s|\n|\.|,|\!|\?|“|”|""|…|\||\(|\)|\[|\]|\-|\#|\&|;|\+|\*|\s'|'\s", RegexOptions.Compiled)
    let replaceList = [ "&#8217;", "'";
                        "&#160;", "";
                        "&#039;", "'";
                        "&#8216;", "'";
                        "&#8220;", "\"";
                        "&#8230;", "";
                        "&mdash;", "-";
                        "&lt;", "";
                        "&gt;", "";
                        "&pound;", "£" ]

    /// action that turns an RSS stream into a map of word/count pairs
    let treatRss title url (receiver: ReceiveBlogAgent) progress (stream: Stream) =
        progress (Printf.sprintf "title: %s" title)
        let xdoc = new XPathDocument(stream)
        let treatText (html: string) =
            let text = stripHtml.Replace(html, "")
            let text = replaceList |> Seq.fold (fun (acc: string) (token, replacer) -> acc.Replace(token,replacer)) text
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
            let words = Seq.concat (Seq.cmap (fun post -> treatText post) posts)
            Seq.fold countWords Map.empty words
                
        let posts = HttpXml.queryXdoc xdoc "rss/channel/item/description"
        progress (Printf.sprintf "items: %i" (Seq.length posts))
        let wordMap = treatPostList posts
        let blog = { Title = title; Url = url; BlogWordCount = wordMap }
        receiver.AddBlog blog


    /// download opml stream then turn into title/url paris
    let opmlFileToTitleUlrs progress url limit = 
        let urlsWorkflow = HttpXml.getContents progress url treatOpml (Seq.of_list [])
        let urls = Async.Run urlsWorkflow
        Seq.take (min limit (Seq.length urls)) urls

    /// download rss files and turn into word/count maps     
    let titleUlrsToWordCountMap local progress timeout urls = 
        let receiver = new ReceiveBlogAgent()
        let flows = 
            urls 
            |> Seq.cmap (fun (title, url) ->
                if local then
                    HttpXml.getContents progress url (treatRss title url receiver) ()
                else
                    HttpXml.getContentsLocal progress url (treatRss title url receiver) ())
        try
            Async.Run (Async.Parallel flows, timeout = timeout) |> ignore
        with :? TimeoutException -> progress "Request timed out"
        receiver.Fetch()



    /// turn word count list into a hierarical cluster
    let clusterWordCounts progress lowerBound upperBound masterList blogs =
        // remove words that are uncommon or too common
        let remove (wordCount: Map<_, _>) =
            let count = Map.fold_left(fun total _ count -> total + count) 0. wordCount
            wordCount |> Map.filter (fun word wc -> 
                let frac = wc / count
                //printfn "%s: %i / %i = %f" word wc count frac
                lowerBound < frac && frac < upperBound)

        // build a master word list containing all the words we interested in
        let masterWordList =
            masterList
            |> remove
            |> Map.to_seq
            |> Seq.cmap fst
        
        // ensure wordcount only contains words from master list    
        let addjustToMasterList wordCount =
            masterWordList 
            |> Seq.cmap (fun word -> 
                match Map.tryfind word wordCount with
                | Some count -> word, count
                | None -> word, 0.)
            |> Map.of_seq

        // build inital cluster list
        let clustersList =
            blogs
            |> Seq.sort_by (fun { Title = title } -> title)
            |> Seq.cmap (fun { Title = title; Url = url; BlogWordCount = wordmap } ->
                    { NameValueParis = addjustToMasterList wordmap;
                      NodeDetails = Leaf { Name = title;
                                           Url = url;
                                           OriginalWordCount = wordmap; }; })
            |> Seq.cache

        clustersList, masterWordList

    let processOpmlToInitClusters progress local url lowerLimit upperLimit limit timeout =
        let masterList, blogs =
            if local then
                Directory.GetFiles(url)
                |> Seq.cmap (fun path -> Path.GetFileNameWithoutExtension(path), path)
                |> Seq.take limit
                |> titleUlrsToWordCountMap local progress timeout
            else
                opmlFileToTitleUlrs progress url limit
                |> titleUlrsToWordCountMap local progress timeout
        let clustersList, chosen =
            Seq.filter (fun { BlogWordCount = wc } -> not (Map.is_empty wc)) blogs
            //|> (fun blogs -> Set.to_seq (Set.of_seq blogs))
            |> clusterWordCounts progress lowerLimit upperLimit masterList
        masterList, blogs, clustersList, chosen

    let mdScaling progress clustersList =
        let getName node =
            match node with
            | Leaf { Name = n } -> n
            | Node _ -> ""
        let namesVectors = 
            Seq.cmap (fun { NameValueParis = wc; NodeDetails = nd } -> { DataName = getName nd; Vector = List.map snd (Map.to_list wc) }) clustersList
            |> Seq.cache
        MultiD.scaleDown progress 3 namesVectors 0.01

    /// process a word count into a hierarical cluster.
    let processOpmlAll progress local url lowerLimit upperLimit limit timeout =
        let masterList, blogs, clustersList, chosen =
            processOpmlToInitClusters progress local url lowerLimit upperLimit limit timeout
        { BiculsterTree = Clustering.buildClusterTree progress clustersList; 
          MulitDScaling = mdScaling progress clustersList;
          MasterWordList = masterList;
          ChosenWords = chosen; ProcessedBlogs = Seq.length blogs }

    let processOpmlToMdScaling progress local url lowerLimit upperLimit limit timeout =
        let masterList, blogs, clustersList, chosen =
            processOpmlToInitClusters progress local url lowerLimit upperLimit limit timeout
        mdScaling progress clustersList