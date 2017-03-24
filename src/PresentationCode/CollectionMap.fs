module Coletto.TypeClassish.Collections

open System

type Fmap = Fmap with
    static member ($) (Fmap, x:array<_>) = fun f -> Array.map f x
    static member ($) (Fmap, x:list<_>  ) = fun f -> List.map f x
    static member ($) (Fmap, x:seq<_>  ) = fun f -> Seq.map f x
let inline map f x = Fmap $ x <| f

