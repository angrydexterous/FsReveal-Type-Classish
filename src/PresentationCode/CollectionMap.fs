module Coletto.TypeClassish.Collections

open System

module Base = 
    type WillMap = WillMap with
        static member inline (?<-) (x:array<_>, _Blank:WillMap,_:array<'b>) = fun f -> Array.map f x 
        static member inline (?<-) (x:list<_>, _Blank:WillMap,_:list<'b>) = fun f -> List.map f x 
        static member inline (?<-) (x:option<_>, _Blank:WillMap,_:option<'b>) = fun f -> Option.map f x

    let inline map (f:'a->'b) x :^M = (x ? (WillMap) <- Unchecked.defaultof< ^M>) f

    