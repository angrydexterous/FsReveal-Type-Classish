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

*** 
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

***
### Ad-Hoc Polymorphism (Type Classes)
#### Example Operator Overloading [Microsoft](https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/operator-overloading)

*)
type Vector(x: float, y : float) =
   member this.x = x
   member this.y = y
   static member (~-) (v : Vector) =
     Vector(-1.0 * v.x, -1.0 * v.y)
   static member (*) (v : Vector, a) =
     Vector(a * v.x, a * v.y)
   static member (*) (a, v: Vector) =
     Vector(a * v.x, a * v.y)
   override this.ToString() =
     this.x.ToString() + " " + this.y.ToString()

let v1 = Vector(1.0, 2.0)

let v2 = v1 * 2.0
let v3 = 2.0 * v1
(**
***
### Extending this to mimic Haskell Type Classes

*)
#r "../src/build/PresentationCode.dll"
open Coletto.TypeClassish.Collections
let a = map id [1..3]
let b = map id [|1..3|]
let c = map id {1..3}
(*** include-value: a ***)
(*** include-value: b ***)
(*** include-value: c ***)
