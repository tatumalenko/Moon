namespace global

open FSharpPlus
open System.Collections.Generic

[<AutoOpen>]
module Extensions =
    open System.Linq

    let NEWLINE = "\n"

    let INDENT = "      "

    let inline (@?) x v =
        match x with
        | Some y -> y
        | None -> v

    let inline (@!) x m =
        match x with
        | Some v -> v
        | None -> failwith m

    let inline (<<?) x f =
        match x with
        | Some v -> Some(f v)
        | None -> None

    let inline (>>?) f x =
        match f with
        | Some y -> Some(y x)
        | None -> None

    type Option<'a> with
        member inline x.map (f: _ -> _) = Option.map f x
        member inline x.flatten (a, b) =
            match a, b with
            | Some a, Some b -> Some(a, b)
            | _ -> None

    let (|Int|_|) (str: string) =
        match System.Int32.TryParse str with
        | true, int -> Some int
        | _ -> None

    let (|Double|_|) (str: string) =
        match System.Double.TryParse str with
        | true, float -> Some float
        | _ -> None

    module Seq =
        let safeSkip (num: int) (source: seq<'a>): seq<'a> =
            seq {
                use e = source.GetEnumerator()
                let idx = ref 0
                let loop = ref true
                while !idx < num && !loop do
                    if not (e.MoveNext()) then loop := false
                    idx := !idx + 1

                while e.MoveNext() do
                    yield e.Current
            }

    module List =
        let flatten xss =
            let rec folder state xs =
                state @ xs

            List.fold folder [] xss

        let flatMap f xs =
            List.map f xs |> flatten

        let flatMap2 f xs1 xs2 =
            List.map2 f xs1 xs2 |> flatten

        let pop<'E> (xs: 'E list byref) =
            let last = List.tryLast xs
            match last with
            | Some e ->
                xs <- List.truncate (List.length xs - 1) xs
                Some e
            | None ->
                None

        let insert (item: 'E) (idx: int) (items: 'E list byref) =
            let mutable newList = List.empty

            if List.isEmpty items then
                if idx = 0
                then newList <- [ item ]
                else failwith "Tried to insert in empty list at index <> 0"
            else if idx = List.length items then
                newList <- items @ [ item ]
            else
                for (i, e) in List.indexed items do
                    if i = idx then
                        newList <- newList @ [ item ]
                        newList <- newList @ [ e ]
                    else
                        newList <- newList @ [ e ]

            items <- newList

        let remove (idx: int) (items: 'E list byref) =
            let mutable newList = List.empty

            if List.isEmpty items then
                failwith "Tried to remove item from empty list"
            else
                for (i, e) in List.indexed items do
                    if i <> idx then newList <- newList @ [ e ]

            items <- newList

    type Iter<'T> =
        { mutable items: 'T list
          mutable item: 'T option
          mutable idx: int }

        member m.next() =
            m.idx <- m.idx + 1
            m.item <- List.tryItem m.idx m.items
            m.item

        member m.get() = m.item

        member m.rest() =
            if m.idx < List.length m.items then m.items.[m.idx + 1..] else List.empty

        member m.peek() = List.tryItem (m.idx + 1) m.items

        static member make (items: 'T list) =
            { items = items
              item = None
              idx = -1 }

    let equalityComparer (comparer: _ -> _ -> bool) (hasher: (_ -> int) option) =
        { new IEqualityComparer<'T> with
            member x.Equals(a, b) = comparer a b
            member x.GetHashCode(a) = (hasher >>? a) @? 42 }

    let inline distinct (comparer: _ -> _ -> bool) (xs: _ list) = List.ofSeq (xs.Distinct(equalityComparer comparer None))

    let mapCompare (comparer: 'a -> 'a -> 'b option) (xs: 'a list): 'b list =
        let mutable bs: 'b list = []
        let ixs = [|0..xs.Length-1|]
        for i in ixs do
            for j in ixs.Where (fun it -> it > i) do
                match comparer xs.[i] xs.[j] with
                | Some v -> bs <- v :: bs
                | None -> ()

        bs

    let groupBy (comparer: 'a -> 'a -> bool) (xs: 'a list): 'a list list =
        let mutable ys: 'a list list = []
        let mutable d = System.Collections.Concurrent.ConcurrentDictionary<'a, 'a list>()
        let ixs = [|0..xs.Length-1|]

        for i in ixs do
            for j in ixs.Where (fun it -> it > i) do
                match comparer xs.[i] xs.[j] with
                | true ->
                    let keys = d.Keys.Where (fun k -> comparer k xs.[i] && (k <> xs.[i] && k <> xs.[j]))
                    if keys.Count() = 0
                    then
                        d.GetOrAdd(xs.[i], xs.[i] :: xs.[j] :: []) |> ignore
                    else
                        let k = keys.First()
                        let vs = d.GetOrAdd(k, [])
                        let nvs = List.filter (fun v -> v <> xs.[i] && v <> xs.[j]) vs
                        d.AddOrUpdate(k, nvs, fun _ v -> xs.[i] :: xs.[j] :: nvs) |> ignore
                | false -> ()

        for kv in d.ToList() do
            ys <- ys @ [d.GetValueOrDefault(kv.Key)]
            ()

        ys

    let inline (+=) (a: 'a byref) (b: 'b) =
        a <- a + b

    let appendn a b =
        a + NEWLINE + b

[<AutoOpen>]
[<RequireQualifiedAccess>]
module String =
    let isBlankOrEmpty s =
        String.trimWhiteSpaces s = ""

    let replaceWith (oldValues: string list) (newValues: string list) (source: string) =
        List.fold2 (fun s o n -> String.replace o n s) source oldValues newValues

[<AutoOpen>]
module Show =
    let memoize fn =
        let cache = System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>()
        (fun x -> cache.GetOrAdd(x, (fun _ -> fn x)))

    let show = memoize string
