#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools.Clustering
open System
open System.Globalization
open System.Windows
open System.Windows.Media
open Strangelights.DataTools.Extensions

type MultiDResult =
    { DataName: string;
      Location: list<float> }

type MultiDInput =
    { DataName: string;
      Vector: seq<float> }

module MultiD =
//    let scaleDown data rate =
//        let data = Seq.to_array data
//        let n = data.Length
//        let realdist = Array2.init n n (fun x y -> 
//            Measures.pearson data.[x] data.[y])
//        let rand = new Random()
//        let loc = Array.init n (fun _ -> [ rand.NextDouble(); rand.NextDouble() ])
//        let fakedist = Array2.init n n (fun _ _ -> 0.)
//        let rec mainLoop iteration lastError =
//            for x in [ 0 .. n - 1 ] do
//                for y in [ 0 .. n - 1 ] do
//                    fakedist.[x,y] <- Measures.euclidean loc.[x] loc.[y]
//                    
//            let grad = Array.init n (fun _ -> [0.; 0.])
//            let mutable totalError = 0.
//            
//            for x in [ 0 .. n - 1 ] do
//                for y in [ 0 .. n - 1 ] do
//                    if x <> y then
//                        let errorterm = (fakedist.[x,y] - realdist.[x,y]) / realdist.[x,y]
//                        let l = Seq.zip3 grad.[x] loc.[x] loc.[y]
//                        let l = List.map (fun (grad, l1, l2) ->  grad + ((l1 - l2) / fakedist.[x,y]) * errorterm) (List.of_seq l)
//                        grad.[x] <- l
//                        totalError <- totalError + (abs errorterm)
//            
//            let cont =
//                iteration < 1000 &&
//                match lastError with
//                | Some lastError -> totalError < lastError
//                | None -> true
//            if cont then
//                for i in [ 0 .. n - 1 ] do
//                    let l = List.of_seq (Seq.zip loc.[i] grad.[i])
//                    loc.[i] <- List.map (fun (loc, diff) ->  loc + (diff * rate)) l
//                mainLoop (iteration + 1) (Some totalError)
//        mainLoop 0 None
//        loc

    let scaleDown progress n data rate =
        progress "Starting multi dimension scaling ..."
        let comparisons = Seq.combinations2 data
//        comparisons
//        |> Seq.iter (fun ({ DataName = n1 }, { DataName = n2 }) -> printfn "%A" (n1, n2))
        let realdistMap = Map.of_seq (Seq.map (fun ({ DataName = n1; Vector = v1 }, { DataName = n2; Vector = v2 }) -> (n1, n2), Measures.pearson v1 v2) comparisons)
        let rand = new Random()
        let input = data |> Seq.cmap (fun d -> d.DataName, List.init n (fun _ -> rand.NextDouble()), List.init n (fun _ -> 0.), 0.) 

        let rec mainLoop iteration lastError dataLocGradErr =
                    
            let dataLocGradErr =
                dataLocGradErr |> 
                Seq.cmap (fun (d1, loc1, grad, err) ->
                    let newGrad, newError =
                        dataLocGradErr |>
                        Seq.fold (fun (accGrad, accErr) (d2, loc2, _, _) ->
                            if d1 <> d2 then
                                let fakedist = Measures.euclidean loc1 loc2
                                let realdist = 
//                                    printfn "d1: %A d2: %A" d1.DataName d2.DataName
                                    match Map.tryfind (d1, d2) realdistMap with
                                    | Some x -> x
                                    | None -> realdistMap.[(d2,d1)]
                                let errorterm = (fakedist - realdist) / realdist
                                let l = Seq.zip3 accGrad loc1 loc2
                                let l = List.map (fun (grad, l1, l2) ->  grad + (((l1 - l2) / fakedist) * errorterm)) (List.of_seq l)
                                l, errorterm
                            else 
                                accGrad, accErr) 
                            (List.init n (fun _ -> 0.), 0.)
                    d1, loc1, newGrad, newError)

            let totalError = Seq.fold (fun acc (_, _, _, err) -> acc + (abs err)) 0. dataLocGradErr
            printfn "Iteration: %i Total Error: %f Last Error: %A" iteration totalError lastError
            progress (Printf.sprintf "Iteration: %i Total Error: %f" iteration totalError)
            let cont =
                iteration < 1000 &&
                match lastError with
                | Some lastError -> totalError < lastError
                | None -> true
            if cont then
                let dataLocGradErr = 
                    dataLocGradErr |>
                    Seq.cmap (fun (d,loc,grad,err) -> 
                        let l = List.of_seq (Seq.zip loc grad)
                        d, List.map (fun (loc, diff) ->  loc - (diff * rate)) l,grad,err)
                mainLoop (iteration + 1) (Some totalError) dataLocGradErr
            else
                dataLocGradErr
        let dataLocGradErr = mainLoop 0 None input
        List.of_seq dataLocGradErr |> List.map (fun (name,loc,_,_) -> { DataName = name; Location = loc }) 

