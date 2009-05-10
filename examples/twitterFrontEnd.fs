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
    let finishImage progress (stream: Stream) =
        let updateDelegate =
            new InvokeThing(fun _ -> 
                let decoder = BitmapDecoder.Create(stream, BitmapCreateOptions.None, BitmapCacheOption.Default)
                image.Source <- decoder.Frames.[0]
                progress (sprintf "Finshed: %s" twit.ScreenName)
                finishedTrigger decoder.Frames.[0])
        image.Dispatcher.Invoke(DispatcherPriority.Normal, updateDelegate) |> ignore
    let imageWF = HttpXml.getContents updateStatus twit.PictureUrl finishImage ()
    let checkBox = new CheckBox(Content = twit.ScreenName)
    do addChildren x [ dc checkBox;  dc image; ]
    do Async.Spawn(imageWF)
    member x.FinishedImage = finished
    member x.IsSelected
        with get() = if not checkBox.IsChecked.HasValue then false else checkBox.IsChecked.Value
        and set x = checkBox.IsChecked <- Nullable x
    member x.Tweeter = twit
        



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
    DockPanel.SetDock(buttonPanel, Dock.Top)
    addChildren topPanel [ dc buttonPanel; dc listView ]
    topPanel

let mainPanel = new Border()

let topControls = 
    let screenName = new TextBox(Text = "robertpi", Width = 400.)
    let getFriends = new Button(Content = "Get Friends", Width = 100.)
    let draw = new Button(Content = "Draw Friends Map", Width = 100.)
    getFriends.Click.Add(fun _ ->
        let bckWrk = new BackgroundWorker()
        let username =  screenName.Text
        updateStatus (sprintf "Getting things for: %s" username)
        bckWrk.DoWork.Add(fun ea ->
            let friends = Twitter.getAllFriends updateStatus username
            ea.Result <- friends)
        bckWrk.RunWorkerCompleted.Add(fun ea ->
            updateStatus (sprintf "Finished getting things for: %s" username)
            let friends =  ea.Result :?> list<Tweeter>
            let fvs = List.map (fun x -> new FriendViewer(x)) friends
            Seq.iter (fun x -> listView.Items.Add(x) |> ignore) fvs
            fvs |> Seq.iter (fun x -> x.FinishedImage.Add(fun _ -> listView.InvalidateVisual())))
        bckWrk.RunWorkerAsync())
    draw.Click.Add(fun _ ->
        let bckWrk = new BackgroundWorker()
        let friends = 
            getAllItems ()
            |> Seq.filter (fun twit -> twit.IsSelected)
            |> Seq.map (fun twit -> twit.Tweeter.ScreenName)
            |> Seq.to_list 
// TODO fix this bit need to know how one should render results of thingy
//        bckWrk.DoWork.Add(fun ea ->
//            updateStatus (sprintf "Get matrix for: %A" friends)
//            let fofMatrix = Twitter.getFofMatrixFromList friends
//            ea.Result <- MultiD.scaleDown updateStatus 2 fofMatrix 0.01)
//        bckWrk.RunWorkerCompleted.Add(fun ea ->
//            updateStatus "Finished grouping friends"
//            let res =  ea.Result :?> list<MultiDResult>
//            let mdres = new MutliDScaling2DViewer(res)
//            printfn "%A" mdres
//            mainPanel.Child <- mdres)
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