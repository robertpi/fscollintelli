#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, Please see the Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools.UI
open System
open System.Globalization
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open Strangelights.DataTools.Clustering
open Strangelights.DataTools.Treatment

type TagCloud(words: seq<string * float>, blogs: seq<BlogLeafDetails>) as x =
    inherit WrapPanel()
    let max = Seq.fold (fun acc (_,count) -> max acc count) 0. words
    let min = Seq.fold (fun acc (_,count) -> min acc count) 0. words
    let dist = max - min
    let threshold = (dist * 0.1) + min
    let event = new Event<BlogLeafDetails>()
    let showList word =
        let blogs = 
            blogs |>
            Seq.filter (fun { OriginalWordCount = wc } -> 
                Map.exists (fun word' _ -> word = word') wc)
        let cm = new ContextMenu()
        blogs |> Seq.iter (fun blog ->
                let mi = new MenuItem(Header = blog.Name)
                mi.Click.Add(fun _ ->
                    event.Trigger blog)
                cm.Items.Add(mi) |> ignore)
        cm.Visibility <- Visibility.Visible
        cm.IsOpen <- true
    let words = 
        words
        |> Seq.filter (fun (word, count) ->  threshold < count) 
        |> Seq.map (fun (word, count) ->
            //printfn "Word: %s Height: %f" word count
            word, (((count - min) / dist) * 40.) + 25.)
        |> Seq.map (fun (word, height) -> 
            let button = new Button(Content = word, Height=height, FontSize = height - 13.)
            button.Click.Add(fun _ -> showList word)
            button)
    do Seq.iter (fun b -> x.Children.Add(b) |> ignore) words
    member x.BlogClicked = event.Publish