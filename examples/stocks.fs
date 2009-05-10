#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/
#if INTERACTIVE
#I @"C:\Program Files\Reference Assemblies\Microsoft\Framework\v3.5"
#r "FSharp.PowerPack";;
#r "PresentationCore";;
#r "PresentationFramework";;
#r "System.Core";;
#r "System.Threading";;
#r "System.Xml";;
#r "WindowsBase";;

#load "../misc/extensions.fs";;
#load "../dataaccess/csvreader.fs";;
#load "../dataaccess/httpxml.fs";;
#load "../dataaccess/yfinance.fs";;
#load "../dataaccess/yfinance.fs";;
#load "../clustering/multidscaling.fs";;
#load "../presentation/multidscaling2Dviewer.fs";;
#endif

open System
open System.ComponentModel
open System.Windows
open System.Windows.Data
open System.Windows.Controls

open Strangelights.DataTools.Extensions
open Strangelights.DataTools.DataAccess
open Strangelights.DataTools.DataAccess
open Strangelights.DataTools.Clustering
open Strangelights.DataTools.UI
open Visifire.Charts


let progress = printfn "%s"

// Source: http://en.wikipedia.org/wiki/CAC_40
let cac40 =
      [ "Accor", "hotels", "AC", 0.91;
        "Air France-KLM", "airlines", "AF", 0.53;
        "Air Liquide", "commodity chemicals", "AI", 2.90;
        "Alcatel-Lucent", "telecommunications equipment", "ALU", 0.75;
        "Alstom", "industrial machinery", "ALO", 1.53;
        "ArcelorMittal", "steel", "MTP", 4.16;
        "AXA", "full line insurance", "CS", 5.14;
        "BNP Paribas", "banks", "BNP", 7.44;
        "Bouygues", "heavy construction", "EN", 1.31;
        "Capgemini", "computer services", "CAP", 0.62;
        "Carrefour", "food retailers and wholesalers", "CA", 2.73;
        "Crédit Agricole", "banks", "ACA", 2.02;
        "Dexia", "banks", "DX", 0.77;
        "EADS", "aerospace", "EAD", 0.67;
        "EDF", "electricity", "EDF", 2.34;
        "Essilor", "medical supplies", "EI", 0.98;
        "France Télécom", "fixed line telecommunications", "FTE", 5.07;
        "GDF Suez", "gas distribution", "GSZ", 6.08;
        "Groupe Danone", "food products", "BN", 3.03;
        "L'Oréal", "personal products", "OR", 2.60;
        "Lafarge", "building materials and fixtures", "LG", 1.45;
        "Lagardère", "publishing", "MMB", 0.46;
        "LVMH", "clothing and accessories", "MC", 2.32;
        "Michelin", "tires", "ML", 0.76;
        "Pernod Ricard", "distillers and vintners", "RI", 1.39;
        "PSA Peugeot Citroën", "automobiles", "UG", 0.70;
        "PPR", "broadline retailers", "PP", 0.71;
        "Renault", "automobiles", "RNO", 1.29;
        "Saint-Gobain", "building materials and fixtures", "SGO", 1.53;
        "Sanofi-Aventis", "pharmaceuticals", "SAN", 6.32;
        "Schneider Electric", "electrical components and equipment", "SU", 1.91;
        "Société Générale", "banks", "GLE", 5.00;
        "STMicroelectronics", "semiconductors", "STM", 0.64;
        "Suez Environnement", "water", "SEV", 0.70;
        "Total", "integrated oil and gas", "FP", 13.47;
        "Unibail-Rodamco", "real estate investment trusts", "UL", 1.58;
        "Vallourec", "industrial machinery", "VK", 1.10;
        "Veolia Environnement", "water", "VIE", 1.47;
        "Vinci", "heavy construction", "DG", 2.05;
        "Vivendi", "broadcasting and entertainment", "VIV", 3.61; ]

let djia =
    [ "3M", "MMM", "Diversified industrials"
      "Alcoa", "AA", "Aluminum"
      "American Express", "AXP", "Consumer finance"
      "AT&T", "T", "Telecommunication"
      "Bank of America", "BAC", "Institutional and retail banking"
      "Boeing", "BA", "Aerospace & defense"
      "Caterpillar", "CAT", "Construction and mining equipment"
      "Chevron Corporation", "CVX", "Oil and gas"
      "Citigroup", "C", "Banking"
      "Coca-Cola", "KO", "Beverages"
      "DuPont", "DD", "Commodity chemicals"
      "ExxonMobil", "XOM", "Integrated oil & gas"
      "General Electric", "GE", "Conglomerate"
      "General Motors", "GM", "Automobiles"
      "Hewlett-Packard", "HPQ", "Diversified computer systems"
      "Home Depot", "HD", "Home improvement retailers"
      "Intel", "INTC", "Semiconductors"
      "IBM", "IBM", "Computer services"
      "Johnson & Johnson", "JNJ", "Pharmaceuticals"
      "JPMorgan Chase", "JPM", "Banking"
      "Kraft Foods", "KFT", "Food processing"
      "McDonald's", "MCD", "Restaurants & bars"
      "Merck", "MRK", "Pharmaceuticals"
      "Microsoft", "MSFT", "Software"
      "Pfizer", "PFE", "Pharmaceuticals"
      "Procter & Gamble", "PG", "Non-durable household products"
      "United Technologies Corporation", "UTX", "Aerospace, heating/cooling, elevators"
      "Verizon Communications", "VZ", "Telecommunication"
      "Walmart", "WMT", "Broadline retailers"
      "Walt Disney", "DIS", "Broadcasting & entertainment" ]

type StockValueHistory =
    { StockName: String;
      DateValues: seq<DateTime * float> }

let getStocks desc =
    let getNamedStockInfo name ticker =
        async { let! res = YahooFinance.getStockInfo progress ticker DateTime.Now (DateTime.Now.AddMonths(-3))
                return name, res }

    let workflows =
        desc |> Seq.map (fun (name, ticker, _) -> getNamedStockInfo name ticker)
    
    let res =
        Async.Run(Async.Parallel workflows)
        |> Seq.filter (fun (_, l) ->  not (Seq.is_empty l))

    let dates = 
        Seq.map (fun (_,res) -> Set.of_seq (Seq.map (fun (d, _, _, _, _, _, _) -> d) res)) res
        |> Seq.reduce Set.intersect |> Set.to_seq
    let datePriceMap prices = Seq.map (fun (d, p, _, _, _, _, _) -> (any_to_string d), p) prices |> Map.of_seq
    res
    |> Seq.map (fun (name, prices) -> name, Seq.filter (fun (d, _, _, _, _, _, _) -> Seq.exists (fun d' -> d' = d) dates) prices)
    |> Seq.map (fun (name, prices) -> { StockName = name; DateValues = Seq.map (fun (d, p, _, _, _, _, _) -> d, p) prices |> Seq.cache })
    |> Seq.cache

let stocks = getStocks djia

let initClusterNodes stocks = 
    let datePriceMap prices = 
        Seq.map (fun (d, p) -> string d, p) prices 
        |> Map.of_seq
    let convertStockHist { StockName = name; DateValues = prices } = 
        { NodeDetails = Leaf name; NameValueParis = datePriceMap prices }
    Seq.cmap convertStockHist stocks :> seq<_>
    
let clusterTree =  Clustering.buildClusterTree progress (initClusterNodes stocks)

let viewer' = new Dendrogram(clusterTree.NodeDetails)

let nbl<'a when 'a:(new:unit->'a) and 'a: struct and 'a :> ValueType> x = new Nullable<'a>(x)

let calcPerformance stocks =
    let perf points =
        let _, first = Seq.hd points
        Seq.map (fun (x, y) -> x, (y / first - 1.) * 100.) points
    Seq.map (fun ({ DateValues = points } as stock) -> { stock with DateValues = perf points }) stocks

let dataSeries stocks =
    let series name points =
        let dataSeries = new DataSeries(RenderAs = RenderAs.Line, LegendText = name,
                                        LineThickness = nbl 0.5)
        points |>
        Seq.iter (fun (_, y) -> dataSeries.DataPoints.Add(new DataPoint(YValue = y)))
        dataSeries
    Seq.map (fun { StockName = name; DateValues = points } -> series name points) stocks
    |> Seq.cache

type ChartCheckbox(title, dataSeries: seq<DataSeries>) =
    inherit DockPanel()
    let dataSeries = List.of_seq dataSeries
    //do printfn "Seq.length dataSeries: %i" (Seq.length dataSeries)

    let chart =
        let chart = new Chart()
        chart.Height <- 500.
        chart.Titles.Add(new Title(Text = title))
        chart

    let chkBoxes =
        dataSeries |> List.map (fun serie -> 
            let chkbx = new CheckBox(Content = serie.LegendText)
            chkbx.Checked.Add(fun _ ->
                let series = dataSeries |> Seq.find (fun x -> x.LegendText = serie.LegendText)
                chart.Series.Add(series)
                chart.InvalidateVisual())
            chkbx.Unchecked.Add(fun _ ->
                let series = dataSeries |> Seq.find (fun x -> x.LegendText = serie.LegendText)
                chart.Series.Remove(series) |> ignore
                chart.InvalidateVisual())
            chkbx)
    let listBox =
        let listBox = new ListBox()
        chkBoxes |> Seq.iter (fun chkbx -> listBox.Items.Add(chkbx) |> ignore)
        listBox
    let selectButton text select =
        let button = new Button(Content = text)
        button.Click.Add(fun _ -> 
            chkBoxes |> Seq.iter (fun chkbx -> 
                chkbx.IsChecked <- nbl select))
        button
    let stackPanel =
        let buttonsPanel = new StackPanel(Orientation = Orientation.Horizontal)
        buttonsPanel.Children.Add(selectButton "Select All" true) |> ignore
        buttonsPanel.Children.Add(selectButton "Unselect All" false) |> ignore
        let stackPanel = new StackPanel()
        stackPanel.Children.Add(buttonsPanel) |> ignore
        stackPanel.Children.Add(listBox) |> ignore
        new ScrollViewer(Content = stackPanel)
    do DockPanel.SetDock(chart, Dock.Top)
    do base.Children.Add(chart) |> ignore
    do DockPanel.SetDock(stackPanel, Dock.Bottom)
    do base.Children.Add(stackPanel) |> ignore

let valuesTab = new TabItem(Content = ChartCheckbox("Raw Values", dataSeries stocks), 
                            Header = "Raw Values Chart") 
let perfTab = new TabItem(Content = ChartCheckbox("Performance", dataSeries (calcPerformance stocks)), 
                          Header = "Performance Chart") 
let tab3 = new TabItem(Content = viewer', Header = "Dendogram") 

let tabs = 
    let tabs = new TabControl()
    tabs.Items.Add(tab3) |> ignore
    tabs.Items.Add(valuesTab) |> ignore
    tabs.Items.Add(perfTab) |> ignore
    tabs

let window = new Window(Content = tabs)

let app = new Application()

[<STAThread>]
do app.Run(window) |> ignore