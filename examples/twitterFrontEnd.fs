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
open System.Windows

open Strangelights.DataTools
open Strangelights.DataTools.UI
open Strangelights.DataTools.Extensions
open Strangelights.DataTools.Clustering
open Strangelights.DataTools.Treatment

let fofMatrix = Twitter.getFof "robertpi"

let viewer = new MutliDScaling2DViewer(Twitter.scaleDownRes fofMatrix)

let window = new Window(Content = viewer)

let app = new Application()

[<STAThread>]
do app.Run(window) |> ignore