#light

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
open Strangelights.Extensions
open Strangelights.HierarchicalClustering

let progress = printfn "%s"

let makeUrl = Printf.sprintf "http://spreadsheets.google.com/feeds/list/%s/od6/public/values"
let urlInfos =
    [ "phNtm3LmDZEP61UU2eSN1YA",
        [ "gsx:location", "";
          "gsx:hospitalbedsper10000population", "Hospital beds per 1000";
          "gsx:nursingandmidwiferypersonneldensityper10000population", "Nursing and Midwifery Personnel per 1000" ];
      "phNtm3LmDZEPVW3ee5eyISA",
        [ "gsx:location", "";
          "gsx:one-year-oldsimmunisedwiththreedosesofdiphtheriatetanustoxoidandpertussisdtp3", "One-year-olds Immunised with diphtheriatetanustoxoidandpertussisdtp3";
          "gsx:one-year-oldsimmunisedwiththreedosesofhepatitisbhepb3", "One-year-olds Immunised with hepatitisbhepb3";
          "gsx:one-year-oldsimmunisedwiththreedosesofhibhib3vaccine", "One-year-olds Immunised with hibhib3vaccine"; ]
      "phNtm3LmDZENjh68qYwzgVQ",
        [ "gsx:location", "";
          "gsx:adolescentfertilityratenumberofbirthstowomenaged15-19asaofwomeninthatagegroup", "Adolescent fertility rate (%)";
          "gsx:birthsattendedbyskilledhealthpersonnel", "Births attended by skilled health personnel (%)";
          "gsx:infantmortalityrateper1000livebirthsbothsexes", "Infant mortality rate (per 1 000 live births) both sexes"; 
          "gsx:maternalmortalityratioper100000livebirths", "Maternal mortality ratio (per 100 000 live births)"; 
          "gsx:neonatalmortalityrateper1000livebirths", "Neonatal mortality rate (per 1 000 live births)"; ];
      "phNtm3LmDZENaPYm1PLIDWw",
        [ "gsx:location", "";
          "gsx:lifeexpectancyatbirthyearsbothsexes", "Life expectancy at birth (years) both sexes";
          "gsx:lifeexpectancyatbirthyearsfemale", "Life expectancy at birth (years) female";
          "gsx:lifeexpectancyatbirthyearsmale", "Life expectancy at birth (years) male"; ]
//      "phNtm3LmDZEPgXnvwJ-SsHQ",
//        [ "gsx:location", "";
//          "gsx:contraceptiveprevalence", "Contraceptive prevalence (%)";]
      "phNtm3LmDZEMImPrpQFRGaQ",
        [ "gsx:location", "";
          "gsx:deathsamongchildrenunderfiveyearsofageduetohivaids", "Deaths among children under five years of age due to HIV/AIDS (%)"; ]
//      "phNtm3LmDZEMw9QQjI4qkHA",
//        [ "gsx:location", "";
//          "gsx:childrenunderfiveyearsofageoverweightforage", "Children under five years of age overweight for age (%)"; ]
      "phNtm3LmDZEOEqWqH8S4WKg",
        [ "gsx:location", "";
          "gsx:percapitarecordedalcoholconsumptionlitresofpurealcoholamongadultsolderthan15years", "Per capita recorded alcohol consumption (litres of pure alcohol) among adults"; ]
      "phNtm3LmDZENVv7vfgmjRMA",
        [ "gsx:location", "";
          "gsx:populationwithsustainableaccesstoimproveddrinkingwatersourcestotal", "Population with sustainable access to improved drinking water sources (%) total"; 
          "gsx:populationwithsustainableaccesstoimprovedsanitationtotal", "Population with sustainable access to improved sanitation (%) total"; ] ]

let data = Gdata.processData progress makeUrl urlInfos
let tree = Clustering.buildClusterTree progress data
let control = new Dendrogram(tree.NodeDetails)
let window = new System.Windows.Window(Content = control)

let data' = Gdata.reverseMatrix data
let tree' = Clustering.buildClusterTree progress data'
let control' = new Dendrogram(tree'.NodeDetails)
let window' = new System.Windows.Window(Content = control')

let main() =
#if COMPILED
    let app = new System.Windows.Application()
    app.Run(window) |> ignore
    app.Run(window') |> ignore
#else
    window.Show()
    window'.Show()
#endif

#if INERACTIVE
UIHelpers.savePNG "reversed.png" control'
#endif

[<STAThread>]
do main()