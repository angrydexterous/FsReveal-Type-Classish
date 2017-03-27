module Coletto.TypeClassish.Collections

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.FSharp.Quotations

module Base = 
    type WillMap = WillMap with
        static member inline (?<-) (x:array<_>, _Blank:WillMap,_:array<'b>) = fun f -> Array.map f x 
        static member inline (?<-) (x:list<_>, _Blank:WillMap,_:list<'b>) = fun f -> List.map f x 
        static member inline (?<-) (x:option<_>, _Blank:WillMap,_:option<'b>) = fun f -> Option.map f x

    let inline map (f:'a->'b) x :^M = (x ? (WillMap) <- Unchecked.defaultof< ^M>) f



module Mapping = 

    type Default5 = class end
    type Default4 = class inherit Default5 end
    type Default3 = class inherit Default4 end
    type Default2 = class inherit Default3 end
    type Default1 = class inherit Default2 end


    // Functor class ----------------------------------------------------------

    [<Extension;Sealed>]
    type Map =
        inherit Default1

        static member inline Invoke (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` = 
            let inline call (mthd : ^M, source : ^I, _output : ^R) = ((^M or ^I or ^R) : (static member Map: _*_*_ -> _) source, mapping, mthd)
            call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

        [<Extension>]static member Map (x : seq<_>              , f : 'T->'U, [<Optional>]_impl:Default2) = Seq.map f x              : seq<'U>
        [<Extension>]static member Map (x : option<_>      , f : 'T->'U, [<Optional>]_mthd : Map) = Option.map  f x
        [<Extension>]static member Map (x : list<_>        , f : 'T->'U, [<Optional>]_mthd : Map) = List.map f x                        : list<'U>
        [<Extension>]static member Map (x : _ []           , f : 'T->'U, [<Optional>]_mthd : Map) = Array.map   f x
        

        // Restricted
        [<Extension>]static member Map (x : Expr<'T>       , f : 'T->'U, [<Optional>]_mthd : Map) = Expr.Cast<'U>(Expr.Application(Expr.Value(f),x))
        [<Extension>]static member Map (x : Dictionary<_,_>, f : 'T->'U, [<Optional>]_mthd : Map) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d: Dictionary<'Key,'U>     



    let inline map    (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x
