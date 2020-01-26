namespace Moon

type Vertex =
    | E
    | C of char

module Nfa =
    type Transition = Vertex * int

    let transition vertex id = Transition(vertex, id)

    let extractStateFromTransition (transition: Transition) =
        let (_, state) = transition
        state
        
    let epsilonClosureMatcher (transition: Transition): bool =
        match transition with
        | E, _ -> true
        | _, _ -> false

    let alphaClosureMatcher (t: Transition) =
        not (epsilonClosureMatcher t)

    let rec alphaClosure (table: Map<int, Set<Transition>>) (state: int) (result: byref<Set<int>>): Set<int> =
        let transitions = table.[state]
        let mutable keepLooking = true

        while keepLooking = true do
            result <- Set.union result (set [state])
            for tr in transitions do
                if (epsilonClosureMatcher tr) then
                    result <- Set.union result (set [state; (extractStateFromTransition tr)])
                    result <- Set.union result (alphaClosure table (extractStateFromTransition tr) &result)
                else
                    result <- Set.union result (set [state])

            keepLooking <- false

        result

    let epsilonClosure (table: Map<int, Set<Transition>>) (state: int) =
        let transitions = table.[state] |> Set.filter epsilonClosureMatcher
        let mutable result = set [ state ]
        for t in transitions do
            result <- alphaClosure table (extractStateFromTransition t) &result

        result |> Set.union (set [state])

    let rec nonEpsilonState (table: Map<int, Set<Transition>>) (state: int) (alpha: Vertex): Set<int> =
        let transitions = table.[state]

        let matchTransition =
            fun t ->
                match t with
                | vertex, state when vertex = alpha -> Some state
                | _, _ -> None

        Set.map matchTransition transitions
        |> Seq.choose id
        |> Set.ofSeq

    let rec states (table: Map<int, Set<Transition>>) (state: int) (alpha: Vertex): Set<int> =
        let transitions = table.[state]

        let matchTransition =
            fun t ->
                match t with
                | vertex, state when vertex = alpha -> Some state
                | _, _ -> None

        Set.map matchTransition transitions
        |> Seq.choose id
        |> Set.ofSeq

    let isState (s: int) (t: Transition) =
        let (_, state) = t
        state = s
        
    let isVertex (vertex: Vertex) (t: Transition) =
        let (v, _) = t
        v = vertex
    
    let moveNfa (table: Map<int, Set<Transition>>) (state: Set<int>) (vertex: Vertex) =
        let mutable result = Set.empty
        for s in state do
            let transitions = table.[s] |> Set.filter (isVertex vertex)
            result <- Set.union result (transitions |> Set.map extractStateFromTransition)
            
        result
            