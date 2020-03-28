module Moon.Tests.Tree

open Moon
open Swensen.Unquote
open Xunit

[<Fact>]
let ``unfold, flatten, +, drawTree``() =
    let f x =
        if 2 * x + 1 > 7 then
            (x, [])
        else
            (x,
             [ 2 * x
               2 * x + 1 ])

    let tree = Tree.unfold f 1

    let p = Tree.drawTree tree

    let xs = Tree.flatten tree
    let b = tree + tree
    let c = tree + 199

    let e = Tree.create 0 []

    test <@ tree <> Tree.singleton 1 @>

[<Fact>]
let ``replace``() =
    let f x =
        if 2 * x + 1 > 7 then
            (x, [])
        else
            (x,
             [ 2 * x
               2 * x + 1 ])

    let tree = Tree.unfold f 1

    test <@ (Tree.replace tree.children.[0] 999 tree).children.[0].root = 999 @>

    test <@ (Tree.replace tree.children.[0].children.[0] 444 tree).children.[0].children.[0].root = 444 @>

    test <@ (Tree.replace tree.children.[0].children.[1] 666 tree).children.[0].children.[1].root = 666 @>
