#r "../src/build/PresentationCode.dll"
open Coletto.TypeClassish.Collections.Mapping

map ((+) 10) (Some 3)
map ((+) 10) [|1..3|]
map ((+) 10) {1..3} 


type TestType<'a> = TestType of 'a*'a with
    static member Map (x:TestType<'a>, f : 'T->'U) = 
        x : TestType<'a>

map ((*) 10) (TestType(5.,5.))