namespace Moon

open System
open System.Linq

[<StructuredFormatDisplay("{show}")>]
[<CustomEquality; NoComparison>]
type Tree<'T> =
    { root: 'T
      children: Tree<'T> list }

    override x.Equals y =
        match y with
        | :? (Tree<'T>) as y ->
            (x :> _ IEquatable).Equals y
        | _ -> false

    override x.GetHashCode() = 391 + (box x.root).GetHashCode() * 23 + x.children.GetHashCode()

    interface IEquatable<Tree<'T>> with
        member x.Equals y = obj.Equals(x.root, y.root) && (x.children :> _ seq).SequenceEqual y.children

[<RequireQualifiedAccess>]
module Tree =
    open FSharpx.Collections

    let inline create root children =
        { root = root
          children = children }

    let inline singleton x = create x List.empty

    let rec map f (x: _ Tree) =
        { Tree.root = f x.root
          children = List.map (map f) x.children }

    let rec ap x f =
        { Tree.root = f.root x.root
          children =
              let a = List.map (map f.root) x.children
              let b = List.map (fun c -> ap x c) f.children
              List.append a b }

    let inline lift2 f a b =
        singleton f
        |> ap a
        |> ap b

    let rec bind f x =
        let a = f x.root
        { root = a.root
          children = List.append a.children (List.map (bind f) x.children) }

    let rec unfold f seed =
        let root, bs = f seed
        create root (unfoldForest f bs)

    and unfoldForest f =
        List.map (unfold f)

    let rec mapFold f state tree =
        let nstate, root = f state tree.root
        let nstate, children = List.mapAccum (mapFold f) nstate tree.children
        nstate, create root children

    let rec dfsPre (x: _ Tree) =
        seq {
            yield x.root
            yield! Seq.collect dfsPre x.children
        }

    let rec dfsPost (x: _ Tree) =
        seq {
            yield! Seq.collect dfsPost x.children
            yield x.root
        }

    let rec fold f state tree =
        Seq.fold f state (dfsPost tree)

    let foldWithParent f state tree =
        let mapChildRoots t = List.map (fun c -> c.root) t.children
        let f1 p s c = f s p c
        let rec foldr s t = List.fold foldr (List.fold (f1 t.root) s (mapChildRoots t)) t.children
        foldr state tree

    let flatten tree =
        let rec squish tree xs =
            tree.root :: List.foldBack squish tree.children xs
        squish tree []

    let replace treeNode root tip =
        let rec f x =
            match x with
            | t when t = treeNode -> create root t.children
            | t -> create t.root (List.map f t.children)
        f tip

    let rec visit (state: _ list) dispatchMap keyResolver tree =
        let visitor = Map.tryFind (keyResolver tree) dispatchMap

        let currentErrors = (visitor.map (fun it -> it state tree) @? state)

        let folder stateAccumulator childTree =
            let childErrors = visit [] dispatchMap keyResolver childTree
            (stateAccumulator @ childErrors)

        let cumulativeChildErrors = List.fold folder [] tree.children
        currentErrors @ cumulativeChildErrors

    let showRoot tree = show tree.root

    let rec showTree tree =
        showRoot tree + (if tree.children = []
                         then ""
                         else " [ " + String.concat " " (List.map showTree tree.children) + " ] ")

    let prefMid =
        seq {
            yield "├─"
            while true do
                yield "│ "
        }

    let prefEnd =
        seq {
            yield "└─"
            while true do
                yield "  "
        }

    let prefNone =
        seq {
            while true do
                yield ""
        }

    let rec visualize (tree: Tree<'T>) (pre: seq<string>) =
        seq {
            yield (Seq.head pre) + showRoot tree
            if tree.children <> [] then
                let preRest = Seq.skip 1 pre
                let last = List.last tree.children
                for e in tree.children do
                    if e = last
                    then yield! visualize e (Seq.map2 (+) preRest prefEnd)
                    else yield! visualize e (Seq.map2 (+) preRest prefMid)
        }

    let drawTree tree =
        visualize tree prefNone
        |> List.ofSeq
        |> String.concat "\n"

type Tree<'T> with
    static member Map(a: Tree<'a>, f) = Tree.map f a
    static member (+) (t1: Tree<'a>, t2: Tree<'a>) = Tree.create t1.root (t1.children @ [ t2 ])
    static member (+) (t: Tree<'a>, a: 'a) = Tree.create t.root ((Tree.create a []) :: t.children)
    static member (<<<<) (t: Tree<'a>, a: 'a) = Tree.create a t.children
    member self.show = Tree.showTree self
    member self.draw = Tree.drawTree self
