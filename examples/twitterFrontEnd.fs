#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

#if INTERACTIVE
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
#load "dendrogram.fs";;
#endif

open System
open System.Collections
open System.IO
open System.ComponentModel
open System.Windows
open System.Windows.Threading
open System.Windows.Controls
open System.Windows.Media.Imaging
open System.Windows.Media

open Strangelights.DataTools
open Strangelights.DataTools.DataAccess
open Strangelights.DataTools.UI
open Strangelights.DataTools.Extensions
open Strangelights.DataTools.Clustering
open Strangelights.DataTools.Treatment

//let fofMatrix = Twitter.getFof "robertpi"


//let defaultImage =
//    let decoder = BitmapDecoder.Create(new Uri(@"file:///C:\code\fscollintelli\examples\twits.jpg"), BitmapCreateOptions.None, BitmapCacheOption.Default)
//    decoder.Frames.[0]

type InvokeThing = delegate of unit -> unit

let addChildren (panel: Panel) children =
    Seq.iter (fun x -> panel.Children.Add(x) |> ignore) children
let dc x = x :> FrameworkElement    


let statusBar = new TextBox(Text = "Status: doing nothing", IsReadOnly = true, Width = Double.NaN)
let updateStatus text =
    let update =
        new InvokeThing(fun _ -> statusBar.Text <- text)
    statusBar.Dispatcher.Invoke(DispatcherPriority.Normal, update) |> ignore
    
type FriendViewer(twit: Tweeter) as x =
    inherit StackPanel()
    do updateStatus (sprintf "Creating friend: %s" twit.ScreenName)
    let image = new Image(Width = 100., Height = 100.)
    let finishedTrigger, finished = Event.create()
    let checkBox = new CheckBox(Content = twit.ScreenName)
    do addChildren x [ dc checkBox;  dc image; ]
    member x.ImageSource
        with get() = image.Source
        and set img = image.Source <- img
    member x.IsSelected
        with get() = if not checkBox.IsChecked.HasValue then false else checkBox.IsChecked.Value
        and set value = checkBox.IsChecked <- Nullable value
    member x.Tweeter = twit
        


let tweeterViewer = new Image(Width = 100., Height = 100.)
let listView = new ListView(Width = 200.)
let iterAllItems func =
    for item in (listView.Items :> IEnumerable) do
        match item with
        | :? FriendViewer as fv -> func fv
        | _ -> ()

let getAllItems () =
    seq { for item in (listView.Items :> IEnumerable) do
            match item with
            | :? FriendViewer as fv -> yield fv
            | _ -> () }


let listPanel =
    let buttonPanel = new StackPanel(Orientation = Orientation.Horizontal)
    let selectAll = new Button(Content = "Select All", Width = 100.)
    selectAll.Click.Add(fun _ -> iterAllItems (fun fv -> fv.IsSelected <- true))
    let deselectAll = new Button(Content = "Deselect All", Width = 100.)
    deselectAll.Click.Add(fun _ -> iterAllItems (fun fv -> fv.IsSelected <- false))
    addChildren buttonPanel [ dc selectAll; dc deselectAll ]

    let topPanel = new DockPanel()
    DockPanel.SetDock(tweeterViewer, Dock.Top)
    DockPanel.SetDock(buttonPanel, Dock.Top)
    addChildren topPanel [ dc tweeterViewer; dc buttonPanel; dc listView ]
    topPanel

let mainPanel = new Border()

let topControls = 
    let screenName = new TextBox(Text = "robertpi", Width = 400.)
    let getFriends = new Button(Content = "Get Friends", Width = 100.)
    let draw = new Button(Content = "Map Friends", Width = 100.)
    getFriends.Click.Add(fun _ ->
        listView.Items.Clear()
        let bckWrk = new BackgroundWorker()
        let username =  screenName.Text
        updateStatus (sprintf "Getting things for: %s" username)
        bckWrk.DoWork.Add(fun ea ->
            ea.Result <- Twitter.getAllFriends updateStatus username)
        bckWrk.RunWorkerCompleted.Add(fun ea ->
            updateStatus (sprintf "Finished getting things for: %s" username)
            let tweeter, friends =  ea.Result :?> (Tweeter * list<Tweeter>)
            let finishImage progress (stream: Stream) =
                let updateDelegate =
                    new InvokeThing(fun _ -> 
                        let decoder = BitmapDecoder.Create(stream, BitmapCreateOptions.None, BitmapCacheOption.Default)
                        tweeterViewer.Source <- decoder.Frames.[0]
                        tweeterViewer.InvalidateVisual()
                        progress (sprintf "Finshed: %s" tweeter.ScreenName))
                tweeterViewer.Dispatcher.Invoke(DispatcherPriority.Normal, updateDelegate) |> ignore
            let imageWF = HttpXml.getContents updateStatus tweeter.PictureUrl finishImage ()
            Async.Start imageWF
            tweeterViewer.Tag <- tweeter
            let processFriend x =
                let fv = new FriendViewer(x)
                let finishImage progress (stream: Stream) =
                    let updateDelegate =
                        new InvokeThing(fun _ -> 
                            let decoder = BitmapDecoder.Create(stream, BitmapCreateOptions.None, BitmapCacheOption.Default)
                            fv.ImageSource <- decoder.Frames.[0]
                            printfn "Finshed: %s" x.ScreenName
                            progress (sprintf "Finshed: %s" x.ScreenName)
                            listView.InvalidateVisual() )
                    listView.Dispatcher.Invoke(DispatcherPriority.Normal, updateDelegate) |> ignore
                let imageWF = HttpXml.getContents updateStatus x.PictureUrl finishImage ()
                fv, imageWF
            let fvImageWFs = List.map processFriend friends
            let fvs, imageWFs = List.unzip fvImageWFs
            let rec startLimited wfs max =
                let sem = new System.Threading.Semaphore(max, max)
                let wrapWF wf =
                    async { use _ = { new IDisposable with member x.Dispose() = sem.Release() |> ignore }
                            let! _ = sem.AsyncWaitOne()
                            do Async.Start wf }
                let wfs' = List.map wrapWF wfs
                Async.Start (Async.Ignore (Async.Parallel wfs')) 
//                printfn "Starting image batch ..."
//                let takeNo = (min 20 (Seq.length images))
//                let toProcess = Seq.take takeNo images
//                Async.RunSynchronously(Async.Parallel toProcess) |> ignore
//                if takeNo = 20 then
//                    processImages (Seq.skip takeNo images)
//            System.Threading.ThreadPool.QueueUserWorkItem(fun _ -> processImages imageWFs) |> ignore
            startLimited imageWFs 10
            Seq.iter (fun x -> listView.Items.Add(x) |> ignore) fvs)
        bckWrk.RunWorkerAsync())
    draw.Click.Add(fun _ ->
        let bckWrk = new BackgroundWorker()
        let tweeter = tweeterViewer.Tag :?> Tweeter
        let friends = 
            getAllItems ()
            |> Seq.filter (fun twit -> twit.IsSelected)
            |> Seq.map (fun twit -> twit.Tweeter, twit.ImageSource)
            |> Seq.to_list
        let friendIds =
            friends
            |> List.map (fun (twit, _) -> twit.Id)
        let twitIdMap =
            (tweeter, tweeterViewer.Source) :: friends
            |> List.map (fun ((twit, _) as x) -> twit.Id, x)
            |> Map.of_list
        bckWrk.DoWork.Add(fun ea ->
            updateStatus (sprintf "Get matrix for selected friends")
            let fofMatrix = 
                Twitter.getAllFof tweeter.Id friendIds
                |> List.map (fun ((id, conns), loc) -> 
                    let twit, pic = twitIdMap.[id]
                    { Id = id;
                      Text = twit.Name;
                      Picture = Some pic;
                      Location = loc;
                      Connections = conns }) 
            ea.Result <- fofMatrix)
        bckWrk.RunWorkerCompleted.Add(fun ea ->
            updateStatus "Finished grouping friends"
            let res =  ea.Result :?> List<GraphNode>
            let mdres = new GraphViewer(res)
            printfn "%A" mdres
            mainPanel.Child <- mdres)
        bckWrk.RunWorkerAsync())
    let sp = new StackPanel(Orientation = Orientation.Horizontal)
    addChildren sp [ dc screenName; dc getFriends; dc draw ]
    sp

let mainDock = 
    DockPanel.SetDock(statusBar, Dock.Bottom)
    DockPanel.SetDock(topControls, Dock.Top)
    DockPanel.SetDock(listPanel, Dock.Left)
    let dp = new DockPanel()
    addChildren dp [ dc statusBar; dc topControls; 
                     dc listPanel; dc mainPanel ]
    dp

let window = new Window(Title = "Robert Pickering's \"the twits\"", 
                        Content = mainDock)

let app = new Application()

[<STAThread>]
do app.Run(window) |> ignore