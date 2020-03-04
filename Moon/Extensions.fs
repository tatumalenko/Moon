module Moon.Extensions

type Seq =
    static member safeSkip (num: int) (source: seq<'a>) : seq<'a> =
        seq {
            use e = source.GetEnumerator()
            let idx = ref 0
            let loop = ref true
            while !idx < num && !loop do
                if not(e.MoveNext()) then
                    loop := false
                idx := !idx + 1

            while e.MoveNext() do
                yield e.Current 
        }
        
module List =
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
            if idx = 0 then
                newList <- [ item ]
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
                if i <> idx then
                    newList <- newList @ [ e ]
                
        items <- newList
        
type Iter<'T> =
    {
        mutable items: 'T list
        mutable item: 'T option
        mutable idx: int
    }
    member m.next() =
        m.idx <- m.idx + 1
        m.item <- List.tryItem m.idx m.items
        m.item
        
    member m.get() =
        m.item
        
    member m.rest() =
        if m.idx < List.length m.items then
            m.items.[m.idx+1..]
        else
            List.empty
    
    member m.peek() =
        List.tryItem (m.idx+1) m.items
            
    static member make(items: 'T list) =
        {
            items = items
            item = None
            idx = -1
        }