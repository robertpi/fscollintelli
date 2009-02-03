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

