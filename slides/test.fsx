#r "../src/build/PresentationCode.dll"
open Coletto.TypeClassish.Collections.Mapping

map ((+) 10) (Some 3)
map ((+) 10) [|1..3|]
map ((+) 10) {1..3} 
