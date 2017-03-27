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

    type Default2 = class end
    type Default1 = class inherit Default2 end

    [<Extension;Sealed>]
    type Map =
        inherit Default1

        static member inline Invoke (mapping :'T->'U) (source : 'A) : 'B = 
            let inline call (mthd : ^M, source : ^I, _output : ^R) = ((^M or ^I or ^R) : (static member Map: _*_*_ -> _) source, mapping, mthd)
            call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'B>)

        static member inline InvokeOnInstance (mapping :'T->'U) (source : 'A) : 'B = 
            (^A : (static member Map: _ * _ -> _) source, mapping)

        static member inline       Map (x : 'A             , f : 'T->'U, _impl:Default1) = Map.InvokeOnInstance f x : 'B
        [<Extension>]static member Map (x : seq<_>         , f : 'T->'U, _impl:Default2) = Seq.map f x              : seq<'U>
        [<Extension>]static member Map (x : option<_>      , f : 'T->'U, _mthd : Map) = Option.map  f x
        [<Extension>]static member Map (x : list<_>        , f : 'T->'U, _mthd : Map) = List.map f x                : list<'U>
        [<Extension>]static member Map (x : _ []           , f : 'T->'U, _mthd : Map) = Array.map   f x

        // Restricted -- needed for seq
        [<Extension>]static member Map (x : Dictionary<_,_>, f : 'T->'U, _mthd : Map) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d: Dictionary<'Key,'U>
        [<Extension>]static member Map (x : Expr<'T>       , f : 'T->'U, _mthd : Map) = Expr.Cast<'U>(Expr.Application(Expr.Value(f),x))
    
    let inline fpmap (f:'T->'U) (x:'A) :'B = Map.Invoke f x
