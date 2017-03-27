(**
- title : Type Classes'ish in FSharp
- description : Type Class-like programmin in FSharp
- author : William Coletto
- theme : night
- transition : default

***

<br />

# Type Classes in FSharp

<img style="border: none; height:3em; padding-left:5em" src="images/fsharp.png" alt="FSharp logo" />
<img style="border: none; height:3em; padding-left:5em" src="images/Prolucid-Logo-White.png" alt="Prolucid logo" />
<br />

* William Coletto
* @willisbueller
* github/angrydexterous

***

### What are type classes?
#### Ad-Hoc Polymorphism
* Operator or function overloading with constraints
 - "This function will run on Type X if Type X implements foo"
* Contrasts with Parametric Polymorphism
 - Generics, where type is parameterized by caller

***
### Parametric Polymorphism (Not Type Classes)

> <p>Parametric polymorphism (...), allows a single piece of code to be typed “generically,” using variables in place of actual types, and then instantiated with particular types as needed. Parametric definitions are uniform: all of their instances behave the same. (...) </p>TAPL , §23.2:

--- 
### Parametric Polymorphism (Not Type Classes)
#### Example [rosettacode](https://rosettacode.org/wiki/Parametric_polymorphism#F.23)
*)
type BinaryTree<'T> = 
    | Element of 'T 
    | Tree of 'T * BinaryTree<'T> * BinaryTree<'T>
    member this.Map(f) =
    match this with
    | Element(x) -> Element(f x)
    | Tree(x,left,right) -> Tree((f x), left.Map(f), right.Map(f))

let t1 = Tree(2, Element(1), Tree(4,Element(3),Element(5)) )
let t2 = t1.Map(fun x -> x * 10)
(**
***
### Ad-Hoc Polymorphism (Type Classes)

TAPL
> <p>Ad-hoc polymorphism, by contrast, allows a polymorphic value to exhibit different behaviors when “viewed” at different types. The most common example of ad-hoc polymorphism is overloading, which associates a single function symbol with many implementations; the compiler (or the runtime system, depending on whether overloading resolution is static or dynamic) chooses an appropriate implementation for each application of the function, based on the types of the arguments.  </p>TAPL , §23.2:

---
### Ad-Hoc Polymorphism (Type Classes)
#### Example Operator Overloading [Gustavo Leon](http://nut-cracker.azurewebsites.net/blog/category/f/)

*)
type Vector2D<'a> = Vector2D of 'a * 'a with
    static member inline (~-.) (Vector2D(a1,a2))    = 
        Vector2D (-a1 , -a2)
 
type Vector3D<'a> = Vector3D of 'a * 'a * 'a with
    static member inline (~-.) (Vector3D(a1,a2,a3)) = 
        Vector3D (-a1, -a2, -a3)
let a =   -. (Vector2D (1.0,2.0))
let b =   -. (Vector3D (1.0,2.0,3.0))
(*** include-value: a ***)
(*** include-value: b ***)

(**
***
### Extending this to mimic Haskell Type Classes

*)
#r "../src/build/PresentationCode.dll"
open Coletto.TypeClassish.Collections
let amap = map id [1..3]
let bmap = map id [|1..3|]
let cmap = map id {1..3} 
(*** include-value: amap ***)
(*** include-value: bmap ***)
(*** include-value: cmap ***)

(** 
***
### What???
#### How'd that work? <img style="border: none; height:1em;padding-bottom:0em !important;margin:0 !important" src="images/open_mouth.png" />
*)
type Fmap = Fmap with
    static member ($) (Fmap, x:array<_>) = fun f -> Array.map f x
    static member ($) (Fmap, x:list<_>  ) = fun f -> List.map f x
    static member ($) (Fmap, x:seq<_>  ) = fun f -> Seq.map f x

let inline map f x = (Fmap $ x) f
(**
***
### How that works
#### Operators
> They have a particular behavior, at operator resolution the compiler looks in every operand class, so for example if we have a binary operator $ : (‘a,’b) -> ‘c it looks first into class ‘a then into class ‘b for the operator definition.
So the trick is we will use an intermediary class with an operator with overloads for the second parameter.
[Gustavo Leon](http://nut-cracker.azurewebsites.net/blog/category/f/)

***
### Inline Methods with Operator Overloading
#### Introduction [Anton Tayanovskyy](http://t0yv0.blogspot.com/2011/12/hacking-type-classes-in-f.html)
> To cut the long story short, before compiling to .NET F# expands methods declared inline and does overload resolution. This was intended to support flexible operator overloading, but opens up the door for interesting hacks. Even code that generalizes over higher kinds and therefore cannot exist at .NET level can with these hacks exist at the inline F# level.

---
### Inline Example
*)
let exampleFunction a b = a + b
let x = exampleFunction 1 2
let y = exampleFunction 1.0 2.0 // Error
(**
val exampleFunction : a:int -> b:int -> int //Determined by first call  
> severity: 'Error'
> message: 'This expression was expected to have type
>     'int'
> but here has type
>     'float'

---
### Corrected with inline
*)
let inline exampleFunction' a b = a + b
let x' = exampleFunction' 1 2
let y' = exampleFunction' 1.0 2.0 
(*** include-value: x' ***)
(*** include-value: y' ***)
(**
Statically resolved:  
val inline exampleFunction' :
  a: ^a -> b: ^b ->  ^c
    when ( ^a or  ^b) : (static member ( + ) :  ^a *  ^b ->  ^c)

***
### So back to type classes
*)
type Fmap = Fmap with
    static member ($) (Fmap, x:array<_>) = fun f -> Array.map f x
    static member ($) (Fmap, x:list<_>  ) = fun f -> List.map f x
    static member ($) (Fmap, x:seq<_>  ) = fun f -> Seq.map f x

let inline map f x = (Fmap $ x) f
(** 
Type classes allow you to define a set of functionality a type must provide so that a function can be run on the type

***
### Going Deeper
Say we want to be able to match only on output types? Use 3 params and overload the ternary operator
*)
type Tree<'t> =
    | Tree of 't * Tree<'t> * Tree<'t>
    | Leaf of 't
type ThreeMap = ThreeMap with
    static member inline (?<-) (x:array<'a>, _Blank:ThreeMap,_:array<'b>) = 
        fun f -> Array.map f x : 'b array
    static member inline (?<-) (x:list<'a>, _Blank:ThreeMap,_:list<'b>) = 
        fun f -> List.map f x : 'b list
    static member inline (?<-) (x:option<'a>, _Blank:ThreeMap,_:option<'b>) = 
        fun f -> Option.map f x : 'b option
    static member inline (?<-) (x:Tree<'a>, _Blank:ThreeMap,_:Tree<'b>) = 
        fun (f:'a->'b) -> 
            let rec loop f = function
                | Leaf x -> Leaf (f x)
                | Tree (x, t1, t2) -> Tree (f x, loop f t1, loop f t2)
            loop f x      
let inline threemap (f:'a->'b) x :^M = (x ? (ThreeMap) <- Unchecked.defaultof< ^M>) f
(**
---
### Applying with the new map function
*)
let ma = threemap ((*) 10) [1..3]
let mb = threemap ((*) 10) [|1..3|]
let mc = threemap ((*) 10) (Some 3) 
let mt = threemap ((*) 10) (Tree(6, Tree(2, Leaf 1, Leaf 3), Leaf 9))
(*** include-value: ma ***)
(*** include-value: mb ***)
(*** include-value: mc ***)
(*** include-value: mt ***)
(**
***
### But what if we want to add our own new types?
You can't add new operator overloads through extension types  
So we must go deeeper

***
### Going Deeper
#### Looking at FsharpPlus by Gustavo Leon


*)
