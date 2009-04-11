#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/
module Strangelights.DataTools.Optimization

open System

let randomSolution domain = 
    let rand = new Random()
    seq { for (max, min) in domain -> (rand.NextDouble() * (max - min))  + min }

let random domain cost =
    let best = Double.MaxValue
    
    let rec loop i best ccost =
        if i < 1000 then
            let sol = randomSolution domain
            let cost = cost sol
            let best, ccost =
                if cost < ccost then
                    sol, cost
                else
                    best, ccost
            loop (i + 1) best ccost
        else
            best
    loop 0 (randomSolution domain) best
    
let hillclimb domain cost =
    let len = Seq.length domain
    let rec loop best ccost =
        let generateNeigbours dir i (max, min) =
            Seq.mapi (fun i' x -> if i = i' && x > min then x + dir else x) best
        let lowers = Seq.mapi (generateNeigbours -1.) domain
        let heights = Seq.mapi (generateNeigbours 1.) domain
        let findBest (best, ccost) sol =
            let cost = cost sol
            if cost < ccost then
                sol, cost
            else
                best, ccost
        
        let (best', ccost') = Seq.fold findBest (best, ccost) lowers
        let (best', ccost') = Seq.fold findBest (best', ccost') heights
        if ccost' = ccost then
            best
        else
            loop best' ccost'

    let sol = randomSolution domain
    loop sol (cost sol)
    
let annealing domain cost =
    let rand = new Random()
    let len = Seq.length domain

    let rec loop t best =
        let i = rand.Next(len)
        let min, max = Seq.nth i domain
        let step = float (rand.Next(-1, 1))
        let sol = Seq.mapi (fun i' x -> 
                let prop = x - step
                if i = i' && min < prop && prop < max then prop else x) best
        let ea = cost sol
        let eb = cost best
        let p = System.Math.E ** ((-ea - eb) / t)
        let best =
            if ea < eb || rand.NextDouble() < p then
                sol
            else
                best
        if t > 0.1 then
            loop (t * 0.95) best
        else
            best

    loop 10000. (randomSolution domain)