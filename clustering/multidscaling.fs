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
    let scaleDown progress n data rate =
        progress "Starting multi dimension scaling ..."
        let comparisons = Seq.combinations2 data
        let realdistMap = Map.of_seq (Seq.map (fun ({ DataName = n1; Vector = v1 }, { DataName = n2; Vector = v2 }) -> (n1, n2), Measures.pearson v1 v2) comparisons)
        let rand = new Random()

        let calculateError dataLocGradErr =
            dataLocGradErr |> 
            Seq.cmap (fun (d1, loc1, grad, err) ->
                let newGrad, newError =
                    dataLocGradErr |>
                    Seq.fold (fun (accGrad, accErr) (d2, loc2, _, _) ->
                        if d1 <> d2 then
                            let fakedist = Measures.euclidean loc1 loc2
                            let realdist = 
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
                d1, loc1, newGrad, newError) :> seq<_>
                
        let calTotalError dataLocGradErr = Seq.fold (fun acc (_, _, _, err) -> acc + (abs err)) 0. dataLocGradErr

        let rec mainLoop iteration temp lastError (dataLocGradErr: seq<_>) =
            let dataLocGradErr = calculateError dataLocGradErr
            let totalError = calTotalError dataLocGradErr
            
            let totalError = Seq.fold (fun acc (_, _, _, err) -> acc + (abs err)) 0. dataLocGradErr
            progress (Printf.sprintf "Iteration: %i Total Error: %f Last Error: %A" iteration totalError lastError)
            let cont =
                iteration < 1000 &&
                match lastError with
                | Some lastError -> totalError < lastError
// annealing type effect is no good here ...
//                    let p = System.Math.E ** ((-totalError - lastError) / temp)
//                    totalError < lastError || rand.NextDouble() < p

                | None -> true
            if cont then
                let dataLocGradErr = 
                    dataLocGradErr |>
                    Seq.cmap (fun (d,loc,grad,err) -> 
                        let l = List.of_seq (Seq.zip loc grad)
                        d, List.map (fun (loc, diff) ->  loc - (diff * rate)) l,grad,err)
                mainLoop (iteration + 1) (temp * 0.95) (Some totalError) dataLocGradErr
            else
                dataLocGradErr, totalError

        let init() = data |> Seq.cmap (fun d -> d.DataName, List.init n (fun _ -> rand.NextDouble()), List.init n (fun _ -> 0.), 0.)
        let inputs = Seq.init_finite 100 (fun _ -> init())
        let calcAndCompare ((_, best) as x) inputData =
            let res = calculateError inputData
            let curr = calTotalError res
            //progress (Printf.sprintf "Best: %f current: %f " best curr)
            if curr < best then res, curr
            else x
        let startPoint, initErr = inputs |> Seq.fold calcAndCompare (Seq.empty, Double.MaxValue)
        let dataLocGradErr, _ = mainLoop 0 10000. None startPoint
        List.of_seq (dataLocGradErr) |> List.map (fun (name,loc,_,_) -> { DataName = name; Location = loc }) 

