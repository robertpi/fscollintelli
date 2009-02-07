#light

open System
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
open Strangelights.Extensions
open Strangelights.HierarchicalClustering
    
let processWords words =
    words
    |> Seq.filter (fun (word, count) -> count > 0.)
    |> Seq.sort_by (fun (_, count) -> count)
    |> Seq.to_list
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
    let wc = processWords (Map.to_seq node.WordCount)
    wc |> Seq.iter (fun (word, count) ->  usedWords.AppendText(Printf.sprintf "%s: %f\n" word count))
    match node.NodeDetails with
    | Leaf { Name = name; Url = url; OriginalWordCount = wordmap } ->
        currentUrl.Text <- url
        let owc = processWords (Map.to_seq wordmap)
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

let showResult result =
    drawTree treeView.Items result.BiculsterTree
    let total = float result.MasterWordList.Count
    processWords (Map.to_seq result.MasterWordList)
    |> Seq.iter (fun (word, count) ->  masterWordList.AppendText(Printf.sprintf "%s: %f = %f%%\n" word count ((count / total) * 100.)))
    let chosenWords = Seq.map (fun word -> word, result.MasterWordList.[word]) result.ChosenWords
    chosenWords
    |> Seq.iter (fun (word, count) ->  choosenWordsAlph.AppendText(Printf.sprintf "%s: %f = %f%%\n" word count ((count / total) * 100.)))
    processWords chosenWords
    |> Seq.iter (fun (word, count) ->  choosenWords.AppendText(Printf.sprintf "%s: %f = %f%%\n" word count ((count / total) * 100.)))
    enable true
    progress (Printf.sprintf "Done - Processed: %i in %O" result.ProcessedBlogs stopwatch.Elapsed)
let start() =
    enable false
    let bgwkr = new BackgroundWorker()
    let url, limit = opmlUrl.Text, Int32.Parse(urlLimit.Text)
    let upperBounds = Double.Parse(upperBounds.Text) / 100. 
    let lowerBounds = Double.Parse(lowerBounds.Text) / 100. 
    let timeout = Int32.Parse(timeout.Text) * 1000
    treeView.Items.Clear()
    let stopwatch = new Stopwatch()
    bgwkr.DoWork.Add(fun ea ->
        stopwatch.Start()
        ea.Result <- Algorithm.processOpml progress url lowerBounds upperBounds limit timeout)
    bgwkr.RunWorkerCompleted.Add(fun ea ->
        statusBar.Text <- "Updating UI ..."
        stopwatch.Stop()
        showResult (ea.Result :?> Result))
    bgwkr.RunWorkerAsync()
                
/// create the UI and wire up
let main() =
    postBrowser.IsVisibleChanged.Add(fun ea ->
        if postBrowser.IsVisible && currentUrl.Text <> "" then
            postBrowser.Navigate(new Uri(currentUrl.Text)) )
    treeView.SelectedItemChanged.Add(fun ea -> 
        if ea.NewValue <> null then
            let tvi = ea.NewValue :?> TreeViewItem
            nodeSelectChanged (tvi.Tag :?> BiculsterNode))
    startButton.Click.Add(fun _ -> start())
    
    let app = new Application() in 
    app.Run(window) |> ignore
 
[<STAThread>]
do main()