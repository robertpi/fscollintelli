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

/// Type thtat will represent the cluster tree
type BiculsterNode =
    { Name: option<string>;
      Left: option<BiculsterNode>;
      Right: option<BiculsterNode>;
      Distance: option<float>;
      Cluster: Map<string,float>; }

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
    let treatRss title progress (stream: Stream) =
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
        title, wordMap


    /// pearson conversion - measures the distance between two list of floats
    /// TODO - check maths!
    let pearson (wc1: seq<float>) (wc2: seq<float>) =
        let sum = PSeq.reduce (+)
        let sum1 = sum wc1
        let sum2 = sum wc2
        
        let sumSq1 = sum (Seq.map (fun x -> x * x) wc1)
        let sumSq2 = sum (Seq.map (fun x -> x * x) wc2)
        
        let pSum = sum (Seq.map2 (fun x y -> x * y) wc1 wc2)
        
        let len = float (Seq.length wc1)
        let num = pSum - ((sum1 * sum2) / len)
        let den = sqrt (((sumSq1 - (sum1 * sum1)) / len) * ((sumSq2 - (sum2 * sum2)) / len))
        if den = 0. then 0. else 1. - (num/den)

    /// combine every item with every other item
    let rec combinations2 items =
      let head = Seq.hd items
      let items' = Seq.skip 1 items
      seq { for el in items' do
                yield head, el
            if Seq.length items' > 1 then
                yield! combinations2 items' }

    /// download opml stream then turn into title/url paris
    let opmlFileToTitleUlrs progress url limit = 
        let urlsWorkflow = DataAccess.getContents progress url treatOpml (Seq.of_list [])
        let urls = Async.Run urlsWorkflow
        Seq.take limit urls

    /// download rss files and turn into word/count maps     
    let titleUlrsToWordCountMap progress urls = 
        let flows = 
            urls 
            |> Seq.map (fun (title, url) -> 
                DataAccess.getContents progress url (treatRss title) ("", Map.empty))
        Async.Run (Async.Parallel flows)

    /// turns a list of cluster nodes into a hierarchal cluster tree
    let buildClusterTree progress clusters =
        let keys m = Map.to_seq m |> PSeq.map snd
        let compareNodes { Cluster = c1 } { Cluster = c2 } =
            let wc1, wc2 = keys c1, keys c2
            pearson wc1 wc2
        let initComparisons = 
            progress (Printf.sprintf "Building initial comparison set ...")
            combinations2 clusters
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
                         Left = Some c1;
                         Right = Some c2;
                         Distance = Some dist;
                         Cluster = averageWordMap c1.Cluster c2.Cluster; }
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
    let clusterWordCounts progress lowerBound upperBound titleWordMap =
        // remove words that are uncommon or too common
        let remove (wordCount: Map<_,_>) =
            let count = (float wordCount.Count)
            wordCount |> Map.filter (fun word wc -> 
                let frac = wc / count
                //printfn "%s: %i / %i = %f" word wc count frac
                lowerBound < frac && frac < upperBound)

        // merge two word count lists
        let mergeWordCounts wc1 wc2 =
            let merge acc (word, count) =
                match Map.tryfind word acc with
                | Some newCount -> Map.add word (count + newCount) acc
                | None -> Map.add word count acc
            Seq.fold merge wc1 (Map.to_seq wc2)

        // build a master word list containing all the words we interested in
        let masterWordList =
            Seq.reduce mergeWordCounts (Seq.map snd titleWordMap)
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
            titleWordMap
            |> PSeq.map (fun (title, wordmap) -> 
                { Name = Some title;
                  Left = None;
                  Right = None;
                  Distance = None;
                  Cluster = addjustToMasterList wordmap; })

        buildClusterTree progress clustersList

    /// process a word count into a hierarical cluster.
    let processOpml progress url lowerLimit upperLimit limit =
        opmlFileToTitleUlrs progress url limit
        |> titleUlrsToWordCountMap progress
        |> Seq.filter (fun (_, posts) -> not (Map.is_empty posts))
        |> clusterWordCounts progress lowerLimit upperLimit
