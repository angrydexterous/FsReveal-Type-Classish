#r "../src/build/PresentationCode.dll"
open Coletto.TypeClassish.Collections.Mapping

map ((+) 10) (Some 3)
map ((+) 10) [|1..3|]
map ((+) 10) {1..3} 


type TestType<'a> = TestType of 'a*'a with
    static member Map (x:TestType<_>, f : 'T->'U) = 
        let (TestType(a,b)) = x
        TestType(f a,f b)

fpmap ((*) 10.0) (TestType(5.,5.))
fpmap ((*) 10) (TestType(5,5))


type Tree<'t> =
    | Tree of 't * Tree<'t> * Tree<'t>
    | Leaf of 't
    static member Map (x:Tree<'a>, f) = 
        let rec loop f = function
            | Leaf x -> Leaf (f x)
            | Tree (x, t1, t2) -> Tree (f x, loop f t1, loop f t2)
        loop f x

fpmap ((*) 10) (Tree(6, Tree(2, Leaf 1, Leaf 3), Leaf 9))