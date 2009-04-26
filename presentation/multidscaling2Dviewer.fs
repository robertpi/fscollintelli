#light
namespace Strangelights.DataTools.UI

open System.Globalization
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Media3D

open Strangelights.DataTools.Clustering

type MutliDScaling2DViewer(data: List<MultiDResult>) as x =
    inherit FrameworkElement()
    override x.OnRender(dc: DrawingContext) =
        let width, height =  x.ActualWidth, x.ActualHeight
        for { DataName = label; Location = loc } in data do
            let x, y, color =
                match loc with
                | [ x; y ] -> x, y, None
                | [ x; y; color ] -> x, y, Some color
                | _ -> failwith (Printf.sprintf "unsupported dims: %i" (List.length loc))
            let x, y = x * width, y * height
            let brush = 
                match color with
                | None -> Brushes.Black
                | Some x ->
                    let red = byte (x * 255.)
                    let blue = byte ((1. - x) * 255.)
                    new SolidColorBrush(Color.FromRgb(red, byte 0, blue))
            let text = new FormattedText(label, CultureInfo.GetCultureInfo("en-us"),
                                         FlowDirection.LeftToRight,
                                         new Typeface("Verdana"),
                                         10., brush)
            dc.DrawText(text, new Point(x + 5., y - 7.))

