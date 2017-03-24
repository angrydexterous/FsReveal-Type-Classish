

(**
- title : Type Classes'ish in FSharp
- description : Type Class-like programmin in FSharp
- author : William Coletto
- theme : black
- transition : default

***

### What are typeclasses?


*)
#r "../src/build/PresentationCode.dll"
open Coletto.TypeClassish.Collections
let a = map id [1..3]
let b = map id [|1..3|]
let c = map id {1..3}
(*** include-value: a ***)
(*** include-value: b ***)
(*** include-value: c ***)