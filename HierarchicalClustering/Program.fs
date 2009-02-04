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
    
type Invokee = delegate of unit -> unit

/// creates the window and loads given the Xaml file into it
let createXamlWindow (file: string) = 
    use stream = XmlReader.Create(file)
    XamlReader.Load(stream) :?> Window

/// create the UI and wire up
let main() =
    // create our window
    let window = createXamlWindow "Window1.xaml"
    let startButton = window.FindName("startButton") :?> Button
    let opmlUrl = window.FindName("opmlUrl") :?> TextBox
    let urlLimit = window.FindName("urlLimit") :?> TextBox
    let outputDetail = window.FindName("outputDetail") :?> TextBox
    let allWords = window.FindName("allWords") :?> TextBox
    let masterWordList = window.FindName("masterWordList") :?> TextBox
    let choosenWords = window.FindName("choosenWords") :?> TextBox
    let treeView = window.FindName("outputTree") :?> TreeView
    let upperBounds = window.FindName("upperBounds") :?> TextBox
    let lowerBounds = window.FindName("lowerBounds") :?> TextBox
    let processWords words =
        words
        |> Seq.filter (fun (word, count) -> count > 0.)
        |> Seq.sort_by (fun (_, count) -> count)
        |> Seq.to_list
        |> List.rev
    treeView.SelectedItemChanged.Add(fun ea -> 
        if ea.NewValue <> null then
            let tvi = ea.NewValue :?> TreeViewItem
            let node = tvi.Tag :?> BiculsterNode
            let usedWords = processWords (Map.to_seq node.Cluster)
            let orginalWords = 
                match node.OriginalCluster with
                | Some x -> processWords (Map.to_seq x)
                | None -> []
            outputDetail.Text <- ""
            allWords.Text <- ""
            (match node.Distance with
            | Some dist -> outputDetail.AppendText(Printf.sprintf "Distance: %f\n\n" dist)
            | None ->())
            Seq.iter (fun (word, count) ->  outputDetail.AppendText(Printf.sprintf "%s: %f\n" word count)) usedWords
            Seq.iter (fun (word, count) ->  allWords.AppendText(Printf.sprintf "%s: %f\n" word count)) orginalWords)
    let statusBar = window.FindName("statusBar") :?> TextBox
    let progress s =
        statusBar.Dispatcher.Invoke(DispatcherPriority.Normal, new Invokee(fun _ -> statusBar.Text <- s)) |> ignore
    let rec drawTree (parent: ItemCollection) node =
        let item = TreeViewItem(IsExpanded = true, Tag = node)
        (match node.Name with
        | Some x -> item.Header <- x
        | None -> item.Header <- "---");
        (match node.Left with
        | Some x -> drawTree item.Items x
        | None -> ());
        (match node.Right with
        | Some x -> drawTree item.Items x
        | None -> ())
        parent.Add(item) |> ignore
    let start() =
        let enable b =
            startButton.IsEnabled <- b
            opmlUrl.IsEnabled <- b
            urlLimit.IsEnabled <- b
        enable false
        let bgwkr = new BackgroundWorker()
        let url, limit = opmlUrl.Text, Int32.Parse(urlLimit.Text)
        let upperBounds, lowerBounds = Double.Parse(upperBounds.Text), Double.Parse(lowerBounds.Text) 
        treeView.Items.Clear()
        let stopwatch = new Stopwatch()
        bgwkr.DoWork.Add(fun ea ->
            stopwatch.Start()
            ea.Result <- Algorithm.processOpml progress url lowerBounds upperBounds limit)
        bgwkr.RunWorkerCompleted.Add(fun ea -> 
            stopwatch.Stop()
            let result = ea.Result :?> Result
            drawTree treeView.Items result.BiculsterTree
            let total = float result.MasterWordList.Count
            Seq.iter (fun (word, count) ->  masterWordList.AppendText(Printf.sprintf "%s: %f = %f%%\n" word count (count / total))) (processWords (Map.to_seq result.MasterWordList))
            Seq.iter (fun word ->  choosenWords.AppendText(Printf.sprintf "%s\n" word)) result.ChosenWords
            enable true
            progress (Printf.sprintf "Done - Processed: %i in %O" result.ProcessedBlogs stopwatch.Elapsed))
        bgwkr.RunWorkerAsync()
    startButton.Click.Add(fun _ -> start())
    let app = new Application() in 
    app.Run(window) |> ignore
 
[<STAThread>]
do main()