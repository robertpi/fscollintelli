#light
namespace Strangelights.Extensions
open System
open System.Linq

module PSeq =
    // Import some stuff from PLink
    let asParallel list: IParallelEnumerable<_> = ParallelQuery.AsParallel(list)
    let map f list = ParallelEnumerable.Select(asParallel list, new Func<_, _>(f))
    let reduce f list = ParallelEnumerable.Aggregate(asParallel list, new Func<_, _, _>(f));
    let fold f acc list = ParallelEnumerable.Aggregate(asParallel list, acc, new Func<_, _, _>(f));

module Net =
    /// Add the GetResponseAsync to the WebRequest class so it can
    /// be used in async workflows. 
    type System.Net.WebRequest with
        member x.GetResponseAsync() =
            Async.BuildPrimitive(x.BeginGetResponse, x.EndGetResponse)
            
module SeqenceOps =
    /// combine every item with every other item
    let rec combinations2 items =
      let head = Seq.hd items
      let items' = Seq.skip 1 items
      seq { for el in items' do
                yield head, el
            if Seq.length items' > 1 then
                yield! combinations2 items' }
module MapOps =
    // merge two word count lists
    let mergeFloatMap wc1 wc2 =
        let merge acc (word, count) =
            match Map.tryfind word acc with
            | Some (newCount: float) -> Map.add word (count + newCount) acc
            | None -> Map.add word count acc
        Seq.fold merge wc1 (Map.to_seq wc2)

module Correlations =
    /// pearson conversion - measures the distance between two list of floats
    let pearson (wc1: seq<float>) (wc2: seq<float>) =
        let sum = PSeq.reduce (+)
        let sum1 = sum wc1
        let sum2 = sum wc2
        
        let sumSq1 = sum (Seq.map (fun x -> x * x) wc1)
        let sumSq2 = sum (Seq.map (fun x -> x * x) wc2)
        
        let pSum = sum (Seq.map2 (fun x y -> x * y) wc1 wc2)
        
        let len = float (Seq.length wc1)
        let num = pSum - ((sum1 * sum2) / len)
        let den = sqrt ((sumSq1 - (sum1 * sum1) / len) 
                        * (sumSq2 - (sum2 * sum2) / len))
        if den = 0. then 0. else num / den

