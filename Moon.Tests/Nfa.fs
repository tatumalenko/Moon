module Moon.Tests.Nfa

open Xunit
open FsUnit.Xunit
open Moon
open Moon.Tests
open Moon.Nfa

[<Theory>]
[<MemberData("validEpsilonClosureCases1")>]
let ``Given Nfa table1, valid epsilon closure is found`` table stateFrom statesTo =
    epsilonClosure table stateFrom
    |> List.ofSeq
    |> should matchList (List.ofSeq statesTo)

[<Theory>]
[<MemberData("validEpsilonClosureCases2")>]
let ``Given Nfa table2, valid epsilon closure is found`` table stateFrom statesTo =
    epsilonClosure table stateFrom
    |> List.ofSeq
    |> should matchList (List.ofSeq statesTo)
    
[<Theory>]
[<MemberData("moveNfaProvider1")>]
let ``Given table and `` table vertex state expected =
    moveNfa table state vertex |> List.ofSeq |> should matchList (expected |> List.ofSeq)

let table1 =
    Map.ofSeq
        [ (0,
           set
               [ transition (C 'l') 1
                 transition (C 'd') 2
                 transition E 1 ])
          (1,
           set
               [ transition E 2
                 transition (C 'd') 2 ])
          (2,
           set
               [ transition (C 'l') 3
                 transition E 3 ])
          (3, set [ transition E 4 ])
          (4,
           set
               [ transition (C 'd') 5
                 transition E 5 ])
          (5,
           set
               [ transition (C 'l') 6
                 transition (C 'd') 7
                 transition (C 'l') 8
                 transition E 6 ])
          (6, set [ transition (C 'l') 0 ]) ]

let table2 =
    Map.ofSeq
        [ (0, set [ transition (C 'l') 1 ])
          (1,
           set
               [ transition E 2
                 transition E 4
                 transition E 7 ])
          (2, set [ transition (C 'l') 3 ])
          (3, set [ transition E 6 ])
          (4, set [ transition (C 'd') 5 ])
          (5, set [ transition E 6 ])
          (6,
           set
               [ transition E 7
                 transition E 1 ])
          (7, set []) ]

let validEpsilonClosureCases1 =
    Utils.asTestArguments
        [ [ table1
            0
            set [ 0; 1; 2; 3; 4; 5; 6 ] ]
          [ table1
            1
            set [ 1; 2; 3; 4; 5; 6 ] ]
          [ table1
            2
            set [ 2; 3; 4; 5; 6 ] ]
          [ table1
            3
            set [ 3; 4; 5; 6 ] ]
          [ table1
            4
            set [ 4; 5; 6 ] ]
          [ table1
            5
            set [ 5; 6 ] ]
          [ table1
            6
            set [ 6 ] ] ]

let validEpsilonClosureCases2 =
    Utils.asTestArguments
        [ [ table2
            0
            set [ 0 ] ]
          [ table2
            1
            set [ 1; 2; 4; 7 ] ]
          [ table2
            2
            set [ 2 ] ]
          [ table2
            3
            set [ 1; 2; 3; 4; 6; 7 ] ]
          [ table2
            4
            set [ 4 ] ]
          [ table2
            5
            set [ 1; 2; 4; 5; 6; 7 ] ]
          [ table2
            6
            set [ 1; 2; 4; 6; 7 ] ]
          [ table2
            7
            set [ 7 ] ] ]

let moveNfaProvider1 =
        Utils.asTestArguments [[table2; C 'l'; set [1;2;4;5;6;7]; set [3]]]

