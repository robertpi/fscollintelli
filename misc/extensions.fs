#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools.Extensions
open System
open System.Linq

module PSeq =
    // Import some stuff from PLink
    let asParallel list: IParallelEnumerable<_> = ParallelQuery.AsParallel(list)
    let map f list = ParallelEnumerable.Select(asParallel list, new Func<_, _>(f))
    let reduce f list = ParallelEnumerable.Aggregate(asParallel list, new Func<_, _, _>(f))
    let fold f acc list = ParallelEnumerable.Aggregate(asParallel list, acc, new Func<_, _, _>(f))
            
module Seq =
    let cmap f s = Seq.map f s |> Seq.cache
    let rec combinations n items =
        let rec comb n curn items combs =
            cmap (fun item ->
                        match curn with
                        | 1 -> Seq.concat (comb n 2 (Seq.filter (fun x -> x <> item) items) (seq { yield seq { yield item } }))
                        | _ when n = curn ->
                            seq { for comb in combs do yield Seq.cons item comb }
                        | _ ->
                            Seq.concat (comb n (curn + 1) (Seq.filter (fun x -> x <> item) items) (seq { for comb in combs do yield Seq.cons item comb })) )
                        items
        Seq.concat (comb n 1 items (Seq.of_list []))

    /// combine every item with every other item
    let rec combinations2 items =
        let combs = combinations 2 items
        cmap (fun l -> Seq.nth 0 l, Seq.nth 1 l) combs
//    let rec combinations2 items =
//      let head = Seq.hd items
//      let items' = Seq.skip 1 items
//      seq { for el in items' do
//                yield head, el
//            if Seq.length items' > 1 then
//                yield! combinations2 items' }
module MapOps =
    // merge two word count lists
    let mergeFloatMap wc1 wc2 =
        let merge acc (word, count) =
            match Map.tryfind word acc with
            | Some (newCount: float) -> Map.add word (count + newCount) acc
            | None -> Map.add word count acc
        Seq.fold merge wc1 (Map.to_seq wc2)

module Measures =
    let euclidean (wc1: seq<float>) (wc2: seq<float>) =
        let vectors = Seq.zip wc1 wc2
        let total = vectors |> Seq.fold (fun acc (p, q) -> acc + ((p - q) ** 2.)) 0.
        sqrt total
            
    /// pearson conversion - measures the distance between two list of floats
    let pearson (wc1: seq<float>) (wc2: seq<float>) =
        let sum = Seq.reduce (+)
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

module UIHelpers =
    open System.Windows.Media.Imaging
    open System.Windows
    open System.IO
    open System.Windows.Media

    let savePNG path (window: System.Windows.FrameworkElement) =
        // Get the size of canvas
        let size = new Size(window.ActualWidth, window.ActualHeight)
        printfn "%A %A" (window.Width, window.Height) size
        // Measure and arrange the surface
        // VERY IMPORTANT
        window.Measure(size)
        window.Arrange(new Rect(size))

        // Create a render bitmap and push the surface to it
        let renderBitmap = 
            new RenderTargetBitmap(
              int size.Width, 
              int size.Height, 
              96., 
              96., 
              PixelFormats.Pbgra32)
        renderBitmap.Render(window)

        // Create a file stream for saving image
        use outStream = new FileStream(path, FileMode.Create)
        // Use png encoder for our data
        let encoder = new PngBitmapEncoder()
        // push the rendered bitmap to it
        encoder.Frames.Add(BitmapFrame.Create(renderBitmap))
        // save the data to the stream
        encoder.Save(outStream)
