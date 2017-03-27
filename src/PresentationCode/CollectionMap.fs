module Coletto.TypeClassish.Collections

open System

module Base = 
    type WillMap = WillMap with
        static member inline (?<-) (x:array<_>, _Blank:WillMap,_:array<'b>) = fun f -> Array.map f x 
        static member inline (?<-) (x:list<_>, _Blank:WillMap,_:list<'b>) = fun f -> List.map f x 
        static member inline (?<-) (x:option<_>, _Blank:WillMap,_:option<'b>) = fun f -> Option.map f x

    let inline map (f:'a->'b) x :^M = (x ? (WillMap) <- Unchecked.defaultof< ^M>) f

    // map ((+) 1) [|1..3|]
    // map ((+) 1) [1..3]
    // map ((+) 1) (Some 2)

                 
module Test =
    
    type Tree<'t> =
    | Tree of 't * Tree<'t> * Tree<'t>
    | Leaf of 't

    type Base.WillMap with
        static member inline (?<-) (x:Tree<'a>, _Blank:Base.WillMap,_:Tree<'b>) = fun f -> 
                                                                                 let rec loop f = function
                                                                                     | Leaf x -> Leaf (f x)
                                                                                     | Tree (x, t1, t2) -> Tree (f x, loop f t1, loop f t2)
                                                                                 loop f x       
    