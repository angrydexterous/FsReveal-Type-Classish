#r "../src/build/PresentationCode.dll"
open Coletto.TypeClassish.Collections.Mapping

map ((+) 10) (Some 3)
map ((+) 10) [|1..3|]
map ((+) 10) {1..3} 


type TestType<'a> = TestType of 'a*'a with
    static member Map (x:TestType<_>, f : 'T->'U) = 
        let (TestType(a,b)) = x
        TestType(f a,f b)

map ((*) 10.0) (TestType(5.,5.))
map ((*) 10) (TestType(5,5))