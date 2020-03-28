namespace global

open FSharpPlus

[<AutoOpen>]
module Extensions =
    let inline (|||) (a: 'a option) b =
        if Option.isSome a then Option.get a else b

    type Option<'E> with
        member x.map f = Option.map f x

    type Seq =
        static member safeSkip (num: int) (source: seq<'a>): seq<'a> =
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
