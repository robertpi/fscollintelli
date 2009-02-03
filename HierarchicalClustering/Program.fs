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
    let treeView = window.FindName("outputTree") :?> TreeView
    treeView.SelectedItemChanged.Add(fun ea -> 
        let tvi = ea.NewValue :?> TreeViewItem
        let node = tvi.Tag :?> BiculsterNode
        let words = Map.to_seq node.Cluster
        let words = 
            words
            |> Seq.filter (fun (word, count) -> count > 0.)
            |> Seq.sort_by (fun (_, count) -> count)
            |> Seq.to_list
            |> List.rev
        outputDetail.Text <- ""
        (match node.Distance with
        | Some dist -> outputDetail.AppendText(Printf.sprintf "Distance: %f\n\n" dist)
        | None ->())
        Seq.iter (fun (word, count) ->  outputDetail.AppendText(Printf.sprintf "%s: %f\n" word count)) words)
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
        treeView.Items.Clear()
        bgwkr.DoWork.Add(fun ea -> 
            ea.Result <- Algorithm.processOpml progress url 0.003 0.02 limit)
        bgwkr.RunWorkerCompleted.Add(fun ea -> 
            drawTree treeView.Items (ea.Result :?> BiculsterNode)
            enable true
            progress "Done")
        bgwkr.RunWorkerAsync()
    startButton.Click.Add(fun _ -> start())
    let app = new Application() in 
    app.Run(window) |> ignore
 
[<STAThread>]
do main()