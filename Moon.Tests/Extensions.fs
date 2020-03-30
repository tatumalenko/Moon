module Moon.Tests.Extensions

open Swensen.Unquote
open Xunit

[<Fact>]
let ``Option-coalescing and raise operators`` () =
    test <@ Some 1 @? 3 = 1 @>
    test <@ Some 1 @? 3 = 1 @>
    test <@ None @? 3 = 3 @>
    test <@ Some 1 @! "Was None" = 1 @>
    raisesWith<System.Exception> <@ None @! "Option was None" @> (fun e -> <@ e.Message = "Option was None" @>)

    let dummyStruct = {| a = Some ["1"; "2"; "3"]; b = 3; c = None |}

    test <@ dummyStruct.a @? [] = ["1"; "2"; "3"] @>
    test <@ dummyStruct.c @? [] = [] @>

    test <@ dummyStruct.a @! "dummyStrut: Tried to get `a` but was None" = ["1"; "2"; "3"] @>
    raisesWith<System.Exception> <@ dummyStruct.c @! "dummyStrut: Tried to get `c` but was None" @> (fun e -> <@ e.Message = "dummyStrut: Tried to get `c` but was None" @>)
