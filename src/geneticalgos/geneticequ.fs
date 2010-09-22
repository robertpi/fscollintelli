// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

namespace Strangelights.DataTools
open System
open System.Numerics
open Microsoft.FSharp.Math

/// Untyped expression tree
type UntypedExpression =
    | Multiply of UntypedExpression * UntypedExpression
    | Add of UntypedExpression * UntypedExpression
    | Subtract of UntypedExpression * UntypedExpression
    | GreaterThan of UntypedExpression * UntypedExpression
    | If of UntypedExpression * UntypedExpression * UntypedExpression
    | Constant of int
    | Parameter of int

module GenericExpressions =

    /// Given a list of parameters evaluate the tree
    let evaluateExpression parameters =
        let rec innerEval tree =
            match tree with
            | Multiply (x, y) -> innerEval x * innerEval y
            | Add (x, y) -> innerEval x + innerEval y
            | Subtract (x, y) -> innerEval x - innerEval y
            | GreaterThan (x, y) -> if innerEval x > innerEval y then 0 else 1
            | If (pred, tval, fval) -> if innerEval pred = 0 then innerEval fval else innerEval tval
            | Constant value -> value
            | Parameter pos -> List.nth parameters pos
        innerEval

    let simplifyExpression =
        let rec innerEval tree =
            match tree with
            | Multiply (Constant 0, _) -> Constant (0)
            | Multiply (_, Constant 0) -> Constant (0)
            | Multiply (Constant x, Constant y) -> Constant (x * y)
            | Add (Constant x, Constant y) -> Constant (x + y)
            | Subtract (Constant x, Constant y) -> Constant (x - y)
            | GreaterThan (Constant x, Constant y) -> if x > y then Constant 0 else Constant 1
            | If (Constant pred, tval, fval) -> if pred = 0 then fval else tval
            | x -> x
        let rec loop tree =
            let tree' = innerEval tree
            if tree' = tree then
                tree
            else
                loop tree
        loop

    /// print the expression to the console
    let printExpression =
        let rec innerPrint ppf tree =
            match tree with
            | Multiply (x, y) -> Printf.fprintf ppf "(%a * %a)" innerPrint x innerPrint y
            | Add (x, y) -> Printf.fprintf ppf "(%a + %a)" innerPrint x innerPrint y
            | Subtract (x, y) -> Printf.fprintf ppf "(%a - %a)" innerPrint x innerPrint y
            | GreaterThan (x, y) -> Printf.fprintf ppf "(%a > %a)" innerPrint x innerPrint y
            | If (pred, tval, fval) -> Printf.fprintf ppf "(if %a then %a else %a)" innerPrint pred innerPrint fval innerPrint tval
            | Constant value -> Printf.fprintf ppf "%i" value
            | Parameter pos -> Printf.fprintf ppf "p%i" pos
        innerPrint System.Console.Out

    let rand = new Random()

    /// build a random expression with limited depth, a maximum constants value,
    /// and a limited number of parameters
    let buildRandomExpression maxDepth maxConst noParams =
        let rec innerBuild curDepth =
            if curDepth < maxDepth then
                let nextDepth = curDepth + 1
                match rand.Next(7) with
                | 0 -> Multiply (innerBuild nextDepth, innerBuild nextDepth)
                | 1 -> Add (innerBuild nextDepth, innerBuild nextDepth)
                | 2 -> Subtract (innerBuild nextDepth, innerBuild nextDepth) 
                | 3 -> GreaterThan (innerBuild nextDepth, innerBuild nextDepth)
                | 4 -> If (innerBuild nextDepth, innerBuild nextDepth, innerBuild nextDepth) 
                | 5 -> Constant (rand.Next(maxConst))
                | 6 -> Parameter (rand.Next(noParams))
                | _ -> failwith "assert false"
            else 
                match rand.Next(2) with
                | 0 -> Constant (rand.Next(maxConst))
                | 1 -> Parameter (rand.Next(noParams))
                | _ -> failwith "assert false"
        innerBuild 0

    /// make a change to an existing tree by replace a node
    /// with a randomly generated tree
    let mutateExpression maxConst maxParam rate =
        let rec innerMutate currDepth tree =
            let mutate node = 
                let newNode =
                    if rand.NextDouble() < rate then 
                        buildRandomExpression maxConst maxParam (currDepth + 1) 
                    else node
                innerMutate (currDepth + 1) node
            match tree with
            | Multiply (x, y) -> Multiply (mutate x, mutate y)
            | Add (x, y) -> Add(mutate  x, mutate  y)
            | Subtract (x, y) -> Subtract (mutate  x, mutate  y)
            | GreaterThan (x, y) -> GreaterThan (mutate  x, mutate  y)
            | If (pred, tval, fval) -> If (mutate  pred, mutate  fval, mutate  tval)
            | Constant value -> Constant( value )
            | Parameter pos -> Parameter ( pos )
        innerMutate 0


    let (|Binary|Nullary|) = function 
        | Add(x,y) -> Binary((fun(x,y) -> Add(x,y)),x,y)
        | Subtract(x,y) -> Binary((fun(x,y) -> Subtract(x,y)),x,y)
        | Multiply(x,y) -> Binary((fun(x,y) -> Multiply(x,y)),x,y)
        | GreaterThan(x,y) -> Binary(((fun (x,y) -> GreaterThan(x,y))),x,y)
        | If(pred,tval,fval) -> Binary((fun (x,y) -> If (pred,x,y)),tval,fval)
        | x -> Nullary(x)

    type HoleTree =
      | LeftHole of (UntypedExpression * UntypedExpression -> UntypedExpression) * HoleTree * UntypedExpression
      | RightHole of (UntypedExpression * UntypedExpression -> UntypedExpression) * UntypedExpression * HoleTree
      | Hole

    let rec plug = function
      | LeftHole(con,h,r),t -> con(plug(h,t), r)
      | RightHole(con,l,h),t -> con(l, plug(h,t))
      | Hole,t -> t


    let rec descendTree top p = function
      | Nullary(x) -> Hole, x
      | t when not top && rand.NextDouble() < p -> Hole, t
      | Binary(con,l,r) -> 
          if rand.NextDouble() < 0.5 then
            let h,t = descendTree false p l
            LeftHole(con,h,r),t
          else
            let h,t = descendTree false p r
            RightHole(con,l,h),t

    let crossOverExpressions p t1 t2 =
        let h,_ = descendTree true p t1
        let _,t = descendTree true p t2
        plug(h,t)

    let evolve scoreFunction mutRate crossRate breedChance pop maxGen maxDepth maxConst noParams =

        let initPop = List.init pop (fun _ -> buildRandomExpression maxDepth maxConst noParams)

        // the inner loop which will handle each generation 
        let rec innerGenEvolve currPop currGen =
        
            // calculate score sort list to find the winner
            let res =
                [ for expr in currPop ->
                    scoreFunction expr, expr ]
            let res = List.sortBy (fun (score,_) -> score) res
            let score,winner = List.head res
            
            // print the winner ... just for info
            printfn "\nGen:%i score:%A" currGen score
            printExpression winner
            
            // if we've found winner or reached the maxium gens return
            if score = 0I || currGen = maxGen then
                winner
            else
                // get rid of scores, no longer needed
                let res = List.map snd res
                
                // always keep winner and second
                let winner, second = 
                    match res with 
                    | winner :: second :: _ -> winner, second 
                    | _ -> failwith "assert false"
                let newpop = winner :: second :: []
                
                // select an expression probably towards to top of the list
                let selectExpr() = List.nth res (int(log(rand.NextDouble()) / log(breedChance)))
                
                // loop to calculate the new population
                let rec addExpress acc=
                    if  List.length acc = pop then
                        acc
                    else
                        // cross two expressions then mutate
                        let crossExpress = (crossOverExpressions crossRate (selectExpr()) (selectExpr()))
                        let newExp = mutateExpression maxConst noParams mutRate crossExpress
                        addExpress (newExp :: acc)
                        
                let newpop = addExpress newpop
                // loop recursively
                innerGenEvolve newpop (currGen + 1)
        // start the loop
        innerGenEvolve initPop 0

    // define a secret funtion we're trying to find
    let secertFunction x y = (x * x) + (2 * y) + (3 * x + 5)

    // calculate some data from the secret function
    let data = [ for x in [0 .. 200] -> 
                    let x = rand.Next(40)
                    let y = rand.Next(40)
                    (x,y), secertFunction x y ]

    // evaluate the an expression to see how close to the secret function it is
    let scoreFunction expr =
        let results =
            [ for (x,y),res in data ->
                res - evaluateExpression [x;y] expr ]
        results |> List.fold (fun acc x -> BigInteger.Abs (BigInteger x) + acc) 0I

#if INTERACTIVE
// call the evolve function
GenericExpressions.evolve GenericExpressions.scoreFunction 0.1 0.7 0.7 500 500 4 10 2

printExpression it

let exp = buildRandomExpression 4 10 2
scoreFunction (buildRandomExpression 4 10 2)
printExpression exp
let exp' = mutateExpression 10 2 0.1 exp
printExpression exp'
let exp'' = crossOverExpressions 0.5 exp exp'
printExpression exp''
evaluateExpression [8; 6] exp''

let expr =
     Multiply
      (Add
        (Parameter 0,
         Add
          (Subtract (Constant 9,Parameter 0),GreaterThan (Parameter 0,Constant 1))),
       Multiply
        (Subtract (Parameter 1,If (Parameter 0,Constant 3,Constant 4)),
         Multiply (Constant 2,Multiply (Parameter 1,Constant 8))))

scoreFunction expr

let results =
    [ for (x,y),res in data ->
        evaluateExpression [x;y] expr ]
        
List.fold (fun acc x -> (abs x) + (abs acc)) 0 results
#endif