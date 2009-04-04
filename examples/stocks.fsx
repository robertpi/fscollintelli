#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

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

open Strangelights.DataTools.DataAccess

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



let  s = Async.Run(YahooFinance.getStockInfo progress "MSFT")
