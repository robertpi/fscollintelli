#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

open System
open System.IO
open System.Globalization
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Media3D
open System.Xml
open System.Windows.Threading
open System.Threading
open System.ComponentModel
open System.Diagnostics
open Strangelights.DataTools.Extensions
open Strangelights.DataTools.Treatment
open Strangelights.DataTools.Clustering
open Strangelights.DataTools.UI
    
let processWords words =
    words
    |> Seq.filter (fun (word, count) -> count > 0.)
    |> Seq.sortBy (fun (_, count) -> count)
    |> Seq.toList
    |> List.rev

type Invokee = delegate of unit -> unit

/// creates the window and loads given the Xaml file into it
let createXamlWindow (file: string) = 
    use stream = XmlReader.Create(file)
    XamlReader.Load(stream) :?> Window

// create our window
let window = createXamlWindow "Window1.xaml"


let startButton = window.FindName("startButton") :?> Button
let opmlUrl = window.FindName("opmlUrl") :?> TextBox
let urlLimit = window.FindName("urlLimit") :?> TextBox
let outputDetail = window.FindName("outputDetail") :?> TextBox
let allWords = window.FindName("allWords") :?> TextBox
let masterWordList = window.FindName("masterWordList") :?> TextBox
let choosenWords = window.FindName("choosenWords") :?> TextBox
let choosenWordsAlph = window.FindName("choosenWordsAlph") :?> TextBox
let treeView = window.FindName("outputTree") :?> TreeView
let upperBounds = window.FindName("upperBounds") :?> TextBox
let lowerBounds = window.FindName("lowerBounds") :?> TextBox
let timeout = window.FindName("timeout") :?> TextBox
let usedWords = window.FindName("usedWords") :?> TextBox
let statusBar = window.FindName("statusBar") :?> TextBox
let currentPost = window.FindName("currentPost") :?> TabItem
let currentUrl = window.FindName("currentUrl") :?> TextBox
let postBrowser = window.FindName("postBrowser") :?> WebBrowser
let tagCloudContainer = window.FindName("tagCloud") :?> TabItem
let dendrogramContainer = window.FindName("dendrogram") :?> TabItem
let multidscaleContainer = window.FindName("multidscale") :?> TabItem

let progress s =
    statusBar.Dispatcher.Invoke(DispatcherPriority.Normal, new Invokee(fun _ -> statusBar.Text <- s)) |> ignore

let enable b =
    startButton.IsEnabled <- b
    opmlUrl.IsEnabled <- b
    urlLimit.IsEnabled <- b
    lowerBounds.IsEnabled <- b
    upperBounds.IsEnabled <- b
    timeout.IsEnabled <- b

let nodeSelectChanged node =
    outputDetail.Text <- ""
    allWords.Text <- ""
    usedWords.Text <- ""
    let wc = processWords (Map.toSeq node.NameValueParis)
    wc |> Seq.iter (fun (word, count) ->  usedWords.AppendText(Printf.sprintf "%s: %f\n" word count))
    match node.NodeDetails with
    | Leaf { Name = name; Url = url; OriginalWordCount = wordmap } ->
        currentUrl.Text <- url
        let owc = processWords (Map.toSeq wordmap)
        outputDetail.Text <- (Printf.sprintf "%s\n%s\nUsed Total Words: %i\nOrginal Total Words: %i" name url wc.Length owc.Length)
        owc |> Seq.iter (fun (word, count) ->  allWords.AppendText(Printf.sprintf "%s: %f\n" word count))
    | Node { Distance = dist } ->
        currentUrl.Text <- ""
        outputDetail.Text <- (Printf.sprintf "Distance: %f\nUsed Total Words: %i" dist wc.Length)


let rec drawTree (parent: ItemCollection) node =
    let item = TreeViewItem(IsExpanded = true, Tag = node)
    parent.Add(item) |> ignore
    match node.NodeDetails with
    | Node { Left = left; Right = right; Distance = dist } ->
        item.Header <- "---"
        drawTree item.Items left
        drawTree item.Items right
    | Leaf { Name = name } -> 
        item.Header <- name

    
let start() =
    enable false
    let bgwkr = new BackgroundWorker()
    let url, limit = opmlUrl.Text, Int32.Parse(urlLimit.Text)
    let uri = new Uri(url)
    let local = uri.IsFile
    let upperBounds = (float upperBounds.Text) / 100. 
    let lowerBounds = (float lowerBounds.Text) / 100. 
    let timeout = (int timeout.Text) * 1000
    treeView.Items.Clear()
    let stopwatch = new Stopwatch()
    bgwkr.DoWork.Add(fun ea ->
        stopwatch.Start()
        try
            ea.Result <- BlogTreatment.processOpmlAll progress local url lowerBounds upperBounds limit timeout
        with
        | Failure(msg) -> MessageBox.Show msg |> ignore)

    let showResult result =
        drawTree treeView.Items result.BiculsterTree
        let total = Map.fold(fun total _ count -> total + count) 0. result.MasterWordList
        processWords (Map.toSeq result.MasterWordList)
        |> Seq.iter (fun (word, count) ->  masterWordList.AppendText(Printf.sprintf "%s: %f = %f%%\n" word count ((count / total) * 100.)))
        let chosenWords = Seq.cmap (fun word -> word, result.MasterWordList.[word]) result.ChosenWords
        chosenWords
        |> Seq.iter (fun (word, count) ->  choosenWordsAlph.AppendText(Printf.sprintf "%s: %f = %f%%\n" word count ((count / total) * 100.)))
        processWords chosenWords
        |> Seq.iter (fun (word, count) ->  choosenWords.AppendText(Printf.sprintf "%s: %f = %f%%\n" word count ((count / total) * 100.)))

        let rec accNodes acc node =
            match node.NodeDetails with
            | Node { Left = left; Right = right; Distance = dist } ->
                let acc = accNodes acc left
                accNodes acc right
            | Leaf node -> node :: acc
        
//        let tagCloud = new TagCloud(chosenWords, accNodes [] result.BiculsterTree)
//        tagCloud.BlogClicked.Add(fun { Name = name; Url = url } ->
//            currentUrl.Text <- url
//            postBrowser.Navigate(new Uri(url))
//            currentPost.IsSelected <- true)
//        tagCloudContainer.Content <- tagCloud

        let rec mapNodes node =
            match node.NodeDetails with
            | Leaf { Name = name } -> { NodeDetails = Leaf name; NameValueParis = node.NameValueParis }
            | Node { Left = left; Right = right; Distance = dist } ->
                { NodeDetails = Node { Distance = dist; Left = mapNodes left; Right = mapNodes right; }
                  NameValueParis = node.NameValueParis }
        let node = mapNodes result.BiculsterTree
        dendrogramContainer.Content <- new Dendrogram(node.NodeDetails)
        multidscaleContainer.Content <- new MutliDScaling2DViewer(List.ofSeq result.MulitDScaling)
        enable true

        progress (Printf.sprintf "Done - Processed: %i in %O" result.ProcessedBlogs stopwatch.Elapsed)
        
    bgwkr.RunWorkerCompleted.Add(fun ea ->
        statusBar.Text <- "Updating UI ..."
        stopwatch.Stop()
        if ea.Result <> null then
            showResult (ea.Result :?> Result))
    bgwkr.RunWorkerAsync()
                
/// create the UI and wire up
let main() =
    opmlUrl.Text <- sprintf @"%s\combined.opml" (Directory.GetCurrentDirectory())
    postBrowser.IsVisibleChanged.Add(fun ea ->
        if postBrowser.IsVisible && currentUrl.Text <> "" then
            postBrowser.Navigate(new Uri(currentUrl.Text)) )
    treeView.SelectedItemChanged.Add(fun ea -> 
        if ea.NewValue <> null then
            let tvi = ea.NewValue :?> TreeViewItem
            nodeSelectChanged (tvi.Tag :?> BiculsterNode<BlogLeafDetails>))
    startButton.Click.Add(fun _ -> start())
    let app = new Application() in 
    app.Run(window) |> ignore
 
[<STAThread>]
do main()