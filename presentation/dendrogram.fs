#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, Please see the Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools.Clustering
open System
open System.Globalization
open System.Windows
open System.Windows.Media
open Strangelights.DataTools.Clustering

type Dendrogram(t) as x =
    inherit FrameworkElement()
    let rec getHeight t =
        match t with
        | Node { Left = l; Right = r } -> 
            (getHeight l.NodeDetails) + (getHeight r.NodeDetails)
        | Leaf x -> 1.
    let getDepth t =
        let rec getDepthInner t d =
            match t with
            | Node { Left = l; Right = r } ->
                max
                    (getDepthInner l.NodeDetails d + 1.0)
                    (getDepthInner r.NodeDetails d + 1.0)
            | Leaf x -> d
        getDepthInner t 0.0
    let pen = new Pen(Brushes.Black, 1.0)
    let height = getHeight t * 20.
    let depth = getDepth t
    
    do base.Height <- height
    //do base.Width <- width

    override x.OnRender(dc: DrawingContext) =
        let scaling = x.ActualWidth / depth 
        
        let rec drawNode t x y =
            match t with
            | Node t ->
                let h1 = (getHeight t.Left.NodeDetails) * 20.
                let h2 = (getHeight t.Right.NodeDetails) * 20.
                let top = y - (h1 + h2) / 2.
                let bottom = y + (h1 + h2) / 2.
                
                let ll = t.Distance * scaling
                dc.DrawLine(pen, new Point(x, top + h1 / 2.), new Point(x, bottom - h2 / 2.))

                // horizontal line
//                let text = new FormattedText(string t.Distance, CultureInfo.GetCultureInfo("en-us"),
//                                             FlowDirection.LeftToRight,
//                                             new Typeface("Verdana"),
//                                             10., Brushes.Black)
//                dc.DrawText(text, new Point(x + 5., y - 7.))
                dc.DrawLine(pen, new Point(x, top + h1 / 2.), new Point(x + ll, top + h1 / 2.))
                
                dc.DrawLine(pen, new Point(x, bottom - h2 / 2.), new Point(x + ll, bottom - h2 / 2.))
                
                drawNode t.Left.NodeDetails (x + ll) (top + h1 / 2.)
                drawNode t.Right.NodeDetails (x + ll) (bottom - h2 / 2.)
           | Leaf v ->
                let text = new FormattedText(v, CultureInfo.GetCultureInfo("en-us"),
                                             FlowDirection.LeftToRight,
                                             new Typeface("Verdana"),
                                             10., Brushes.Black)
                dc.DrawText(text, new Point(x + 5., y - 7.))
        drawNode t 10. (height / 2.)

