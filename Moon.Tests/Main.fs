module Moon.Tests.Main

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Given valid command line arguments, the main function returns 0``() =
    let result = Moon.Main.main [| "lex"; "--path"; Utils.makePath "resources/lexer/in/positivegrading.src"; "--outdir"; Utils.makePath "resources/lexer/out" |]
    result |> should equal 0
    
[<Fact>]
let ``Given invalud command line arguments, the main function should raise error``() =
    (fun () -> Moon.Main.main [| "lex"; "--poop" |] |> ignore) |> should throw typeof<Argu.ArguParseException> 