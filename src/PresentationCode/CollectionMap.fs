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



    // Monad class ------------------------------------------------------------

    [<Extension;Sealed>]
    type Bind =
        [<Extension>]static member Bind (source : Lazy<'T>    , f : 'T -> Lazy<'U>    ) = lazy (f source.Value).Value                                   : Lazy<'U>
        [<Extension>]static member Bind (source               , f : 'T -> _           ) = Option.bind   f source                                        : option<'U>
        [<Extension>]static member Bind (source               , f : 'T -> _           ) = List.collect  f source                                        : list<'U>  
        [<Extension>]static member Bind (source               , f : 'T -> _           ) = Array.collect f source                                        : 'U []     
        [<Extension>]static member Bind (source               , k : 'T -> _           ) = (fun r -> k (source r) r)                                     : 'R->'U    
        [<Extension>]static member Bind (source               , f : 'T -> _           ) = async.Bind(source, f)                                         : Async<'U>

        [<Extension>]static member Bind (source : Map<'Key,'T>, f : 'T -> Map<'Key,'U>) = Map (seq {
                                                                                                        for KeyValue(k, v) in source do
                                                                                                            match Map.tryFind k (f v) with
                                                                                                            | Some v -> yield k, v
                                                                                                            | _      -> () })

        [<Extension>]static member Bind (source : Dictionary<'Key,'T>, f : 'T -> Dictionary<'Key,'U>) = 
                                                                                                        let d = Dictionary()
                                                                                                        for KeyValue(k, v) in source do
                                                                                                            match (f v).TryGetValue(k)  with
                                                                                                            | true, v -> d.Add(k, v)
                                                                                                            | _       -> ()
                                                                                                        d


        static member inline Invoke (source : '``Monad<'T>``) (binder : 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
            let inline call (_mthd : 'M, input : 'I, _output : 'R, f) = ((^M or ^I or ^R) : (static member Bind: _*_ -> _) input, f)
            call (Unchecked.defaultof<Bind>, source, Unchecked.defaultof<'``Monad<'U>``>, binder)

        static member inline InvokeOnInstance (source : '``Monad<'T>``) (binder : 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
            ((^``Monad<'T>`` or ^``Monad<'U>``) : (static member Bind: _*_ -> _) source, binder)


    [<Extension;Sealed>]
    type Join =
        inherit Default1
        static member inline       Join (x : '``Monad<'Monad<'T>>``, [<Optional>]_output : '``Monad<'T>``  , [<Optional>]_impl : Default2) = Bind.InvokeOnInstance x id: '``Monad<'T>``
        static member inline       Join (x : '``Monad<'Monad<'T>>``, [<Optional>]_output : '``Monad<'T>``  , [<Optional>]_impl : Default1) = ((^``Monad<'Monad<'T>>`` or  ^``Monad<'T>``) : (static member Join: _ -> _) x) : '``Monad<'T>``
        [<Extension>]static member Join (x : Lazy<Lazy<_>>         , [<Optional>]_output : Lazy<'T>        , [<Optional>]_impl : Join    ) = lazy x.Value.Value        : Lazy<'T>
        [<Extension>]static member Join (x                         , [<Optional>]_output : option<'T>      , [<Optional>]_impl : Join    ) = Option.bind   id x        : option<'T>
        [<Extension>]static member Join (x                         , [<Optional>]_output : list<'T>        , [<Optional>]_impl : Join    ) = List.collect  id x        : list<'T>  
        [<Extension>]static member Join (x                         , [<Optional>]_output : 'T []           , [<Optional>]_impl : Join    ) = Array.collect id x        : 'T []     
        [<Extension>]static member Join (g                         , [<Optional>]_output : 'R->'T          , [<Optional>]_impl : Join    ) = (fun r -> (g r) r)        : 'R->'T    
        [<Extension>]static member Join (x                         , [<Optional>]_output : Async<'T>       , [<Optional>]_impl : Join    ) = async.Bind(x, id)         : Async<'T>

        [<Extension>]static member Join (x : Map<_,_>                     , [<Optional>]_output : Map<'Key,'Value>, [<Optional>]_impl : Join    )                             : Map<'Key,'Value> =
                        Map (seq {
                            for KeyValue(k, v) in x do
                                match Map.tryFind k v with
                                | Some v -> yield k, v
                                | _      -> () })

        [<Extension>]static member Join (x : Dictionary<_,Dictionary<_,_>>, [<Optional>]_output : Dictionary<'Key,'Value>, [<Optional>]_impl:Join)                            : Dictionary<'Key,'Value> =
                        let d = Dictionary()
                        for KeyValue(k, v) in x do
                            match v.TryGetValue(k)  with
                            | true, v -> d.Add(k, v)
                            | _       -> ()
                        d


        static member inline Invoke (source : '``Monad<Monad<'T>>``) : '``Monad<'T>`` =
            let inline call (mthd : 'M, input : 'I, output : 'R) = ((^M or ^I or ^R) : (static member Join: _*_*_ -> _) input, output, mthd)
            call (Unchecked.defaultof<Join>, source, Unchecked.defaultof<'``Monad<'T>``>)


    type Return =
        inherit Default1

        static member inline Invoke (x:'T) : '``Applicative<'T>`` =
            let inline call (mthd : ^M, output : ^R) = ((^M or ^R) : (static member Return: _*_ -> _) output, mthd)
            call (Unchecked.defaultof<Return>, Unchecked.defaultof<'``Applicative<'T>``>) x
    
        static member inline InvokeOnInstance (x:'T) = (^``Applicative<'T>`` : (static member Return: ^T -> ^``Applicative<'T>``) x)

        static member        Return (_:seq<'a> , _:Default2) = fun  x     -> Seq.singleton x :seq<'a>
        static member inline Return (_:'R      , _:Default1) = fun (x:'T) -> Return.InvokeOnInstance x :'R

        static member        Return (_:option<'a>    , _:Return) = fun x -> Some x      :option<'a>
        static member        Return (_:list<'a>      , _:Return) = fun x -> [ x ]       :list<'a>
        static member        Return (_:'a []         , _:Return) = fun x -> [|x|]       :'a []
        static member        Return (_:'a Async      , _:Return) = fun (x:'a) -> async.Return x
        static member        Return (_:Choice<'a,'e> , _:Return) = fun x -> Choice1Of2 x :Choice<'a,'e>
        static member        Return (_:Expr<'a>      , _:Return) = fun x -> Expr.Cast<'a>(Expr.Value(x:'a))
        static member        Return (_:'a ResizeArray, _:Return) = fun x -> ResizeArray<'a>(Seq.singleton x)

        //Restricted
        static member Return (_:string       , _:Return) = fun (x:char) -> string x : string
        static member Return (_:StringBuilder, _:Return) = fun (x:char) -> new StringBuilder(string x):StringBuilder
        static member Return (_:'a Set       , _:Return) = fun (x:'a  ) -> Set.singleton x

    type Apply =
        inherit Default1
        
        static member inline ``<*>`` (f:'``Monad<'T->'U>``  , x:'``Monad<'T>``  , [<Optional>]_output:'``Monad<'U>``  , [<Optional>]_impl:Default2) : '``Monad<'U>``   = Bind.InvokeOnInstance f (fun (x1:'T->'U) -> Bind.InvokeOnInstance x (fun x2 -> Return.Invoke(x1 x2)))
        static member inline ``<*>`` (f:'``Applicative<'T->'U>``, x:'``Applicative<'T>``, [<Optional>]_output:'``Applicative<'U>``, [<Optional>]_impl:Default1) : '``Applicative<'U>`` = ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>): _*_ -> _) f, x)

        static member        ``<*>`` (f:_ []        , x:'T []          , [<Optional>]_output:'U []        , [<Optional>]_impl:Apply) = Array.collect (fun x1 -> Array.collect (fun x2 -> [|x1 x2|]) x) f :'U []
        static member        ``<*>`` (f:'r -> _     , g: _ -> 'T       , [<Optional>]_output: 'r -> 'U    , [<Optional>]_impl:Apply) = fun x -> f x (g x) :'U
        static member        ``<*>`` (f:Async<_>    , x:Async<'T>      , [<Optional>]_output:Async<'U>    , [<Optional>]_impl:Apply) = async.Bind (f, fun x1 -> async.Bind (x, fun x2 -> async {return x1 x2})) :Async<'U>

        static member        ``<*>`` (f:Map<'Key,_>      , x:Map<'Key,'T>        , [<Optional>]_output:Map<'Key,'U>, [<Optional>]_impl:Apply) :Map<'Key,'U> = Map (seq {
                                                                                                                                                                        for KeyValue(k, vf) in f do
                                                                                                                                                                            match Map.tryFind k x with
                                                                                                                                                                            | Some vx -> yield k, vf vx
                                                                                                                                                                            | _       -> () })

        static member        ``<*>`` (f:Dictionary<'Key,_>, x:Dictionary<'Key,'T>, [<Optional>]_output:Dictionary<'Key,'U>, [<Optional>]_impl:Apply) :Dictionary<'Key,'U> =
                                                                                                                                                                            let d = Dictionary()
                                                                                                                                                                            for KeyValue(k, vf) in f do
                                                                                                                                                                                match x.TryGetValue k with
                                                                                                                                                                                | true, vx -> d.Add(k, vf vx)
                                                                                                                                                                                | _        -> ()
                                                                                                                                                                            d
        
        static member        ``<*>`` (f:Expr<'T->'U>, x:Expr<'T>, [<Optional>]_output:Expr<'U>, [<Optional>]_impl:Apply) = Expr.Cast<'U>(Expr.Application(f,x))

        static member        ``<*>`` (f:('T->'U) ResizeArray, x:'T ResizeArray, [<Optional>]_output:'U ResizeArray, [<Optional>]_impl:Apply) =
            ResizeArray(Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) :'U ResizeArray

        static member inline Invoke (f:'``Applicative<'T -> 'U>``) (x:'``Applicative<'T>``) : '``Applicative<'U>`` =
            let inline call (mthd : ^M, input1 : ^I1, input2 : ^I2, output : ^R) =                                                          
                ((^M or ^I1 or ^I2 or ^R) : (static member ``<*>`` : _*_*_*_ -> _) input1, input2, output, mthd)
            call(Unchecked.defaultof<Apply>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)

        static member inline InvokeOnInstance (f:'``Applicative<'T->'U>``) (x:'``Applicative<'T>``) : '``Applicative<'U>`` =
            ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>): _*_ -> _) (f, x))

    // Functor class ----------------------------------------------------------

    [<Extension;Sealed>]
    type Iterate =
        [<Extension>]static member Iterate (x:Lazy<'T>  , action) = action x.Value :unit
        [<Extension>]static member Iterate (x:seq<'T>   , action) = Seq.iter action x
        [<Extension>]static member Iterate (x:option<'T>, action) = match x with Some x -> action x | _ -> ()
        [<Extension>]static member Iterate (x:list<'T>  , action) = List.iter action x
        [<Extension>]static member Iterate ((_:'W, a:'T), action) = action a :unit
        [<Extension>]static member Iterate (x:'T []     , action) = Array.iter   action x
        [<Extension>]static member Iterate (x:'T [,]    , action) = Array2D.iter action x
        [<Extension>]static member Iterate (x:'T [,,]   , action) = Array3D.iter action x
        [<Extension>]static member Iterate (x:'T [,,,]  , action) =
                        for i = 0 to Array4D.length1 x - 1 do
                            for j = 0 to Array4D.length2 x - 1 do
                                for k = 0 to Array4D.length3 x - 1 do
                                    for l = 0 to Array4D.length4 x - 1 do
                                        action x.[i,j,k,l]
        [<Extension>]static member Iterate (x:Async<'T>           , action) = action (Async.RunSynchronously x) : unit
        [<Extension>]static member Iterate (x:Choice<'T,'E>       , action) = match x with Choice1Of2 x -> action x | _ -> ()
        [<Extension>]static member Iterate (KeyValue(_:'Key, x:'T), action) = action x :unit
        [<Extension>]static member Iterate (x:Dictionary<'Key,'T> , action) = Seq.iter action x.Values
        [<Extension>]static member Iterate (x:_ ResizeArray       , action) = Seq.iter action x

        // Restricted
        [<Extension>]static member Iterate (x:string         , action) = String.iter action x
        [<Extension>]static member Iterate (x:StringBuilder  , action) = String.iter action (x.ToString())
        [<Extension>]static member Iterate (x:Set<'T>        , action) = Set.iter action x        

        static member inline Invoke (action : 'T->unit) (source : '``Functor<'T>``) : unit =
            let inline call (_ : ^M, source : ^I) =  ((^M or ^I) : (static member Iterate: _*_ -> _) source, action)
            call (Unchecked.defaultof<Iterate>, source)

    [<Extension;Sealed>]
    type Map =
        inherit Default1

        static member inline Invoke (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` = 
            let inline call (mthd : ^M, source : ^I, _output : ^R) = ((^M or ^I or ^R) : (static member Map: _*_*_ -> _) source, mapping, mthd)
            call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

        static member inline InvokeOnInstance (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` = 
            (^``Functor<'T>`` : (static member Map: _ * _ -> _) source, mapping)

        static member inline       Map (x : '``Monad<'T>``      , f : 'T->'U, [<Optional>]_impl:Default4) = Bind.InvokeOnInstance x (f >> Return.InvokeOnInstance) : '``Monad<'U>``
        static member inline       Map (x : '``Applicative<'T>``, f : 'T->'U, [<Optional>]_impl:Default3) = Apply.InvokeOnInstance (Return.InvokeOnInstance f) x : '``Applicative<'U>``
        [<Extension>]static member Map (x : seq<_>              , f : 'T->'U, [<Optional>]_impl:Default2) = Seq.map f x              : seq<'U>
        [<Extension>]static member Map (x : IDictionary<_,_>    , f : 'T->'U, [<Optional>]_impl:Default2) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d :> IDictionary<'Key,'U>
        [<Extension>]static member Map (x : IObservable<'T>     , f : 'T->'U, [<Optional>]_impl:Default2) = Observable.map f x       : IObservable<'U>
        static member inline       Map (x : '``Functor<'T>``    , f : 'T->'U, [<Optional>]_impl:Default1) = Map.InvokeOnInstance f x : '``Functor<'U>``

        [<Extension>]static member Map (x : option<_>      , f : 'T->'U, [<Optional>]_mthd : Map) = Option.map  f x
        [<Extension>]static member Map (x : list<_>        , f : 'T->'U, [<Optional>]_mthd : Map) = List.map f x                        : list<'U>
        [<Extension>]static member Map (g : 'R->'T         , f : 'T->'U, [<Optional>]_mthd : Map) = (>>) g f
        [<Extension>]static member Map (g : Func<'R, 'T>   , f : 'T->'U, [<Optional>]_mthd : Map) = Func<'R, 'U>(g.Invoke >> f)
        [<Extension>]static member Map ((m : 'Monoid, a)   , f : 'T->'U, [<Optional>]_mthd : Map) = (m, f a)
        [<Extension>]static member Map (x : _ []           , f : 'T->'U, [<Optional>]_mthd : Map) = Array.map   f x
        [<Extension>]static member Map (x : _ [,]          , f : 'T->'U, [<Optional>]_mthd : Map) = Array2D.map f x
        [<Extension>]static member Map (x : _ [,,]         , f : 'T->'U, [<Optional>]_mthd : Map) = Array3D.map f x
        [<Extension>]static member Map (x : _ [,,,]        , f : 'T->'U, [<Optional>]_mthd : Map) = Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
        [<Extension>]static member Map (x : Async<_>       , f : 'T->'U, [<Optional>]_mthd : Map) = async.Bind(x, async.Return << f)
        [<Extension>]static member Map (KeyValue(k, x)     , f : 'T->'U, [<Optional>]_mthd : Map) = KeyValuePair(k, f x)
        [<Extension>]static member Map (x : Dictionary<_,_>, f : 'T->'U, [<Optional>]_mthd : Map) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d: Dictionary<'Key,'U>
        [<Extension>]static member Map (x : Expr<'T>       , f : 'T->'U, [<Optional>]_mthd : Map) = Expr.Cast<'U>(Expr.Application(Expr.Value(f),x))
        [<Extension>]static member Map (x : ResizeArray<'T>, f : 'T->'U, [<Optional>]_mthd : Map) = ResizeArray(Seq.map f x) : ResizeArray<'U>

        // Restricted
        [<Extension>]static member Map (x : string         , f, [<Optional>]_mthd : Map) = String.map f x
        [<Extension>]static member Map (x : StringBuilder  , f, [<Optional>]_mthd : Map) = new StringBuilder(String.map f (x.ToString()))
        [<Extension>]static member Map (x : Set<_>         , f, [<Optional>]_mthd : Map) = Set.map f x
            



    let inline map    (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x
