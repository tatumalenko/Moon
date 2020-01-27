module Moon.Tests.Main

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Given valid command line arguments, the main function returns 0``() =
    let result =
        Moon.Main.main
            [| "lex"
               "--path"
               Utils.makePath "resources/lexer/in/positivegrading.src"
               "--outdir"
               Utils.makePath "resources/lexer/out" |]
    result |> should equal 0

[<Fact>]
let ``Given invalid cli arguments for lex subcommand, the main function should raise error``() =
    (fun () -> Moon.Main.main [| "lex"; "--poop" |] |> ignore) |> should throw typeof<Argu.ArguParseException>

[<Fact>]
let ``Given missing path cli argument for lex subcommand, the main function should raise error``() =
    (fun () ->
    Moon.Main.main
        [| "lex"
           "--outdir"
           Utils.makePath "resources/lexer/out" |]
    |> ignore)
    |> should (throwWithMessage "Missing command line argument --path") typeof<System.Exception>

[<Fact>]
let ``Given missing outdir cli argument for lex subcommand, the main function should raise error``() =
    (fun () ->
    Moon.Main.main
        [| "lex"
           "--path"
           Utils.makePath "resources/lexer/in/positivegrading.src" |]
    |> ignore)
    |> should (throwWithMessage "Missing command line argument --outDir") typeof<System.Exception>

[<Fact>]
let ``Given no cli argument for lex subcommand, the main function should raise error``() =
    (fun () -> Moon.Main.main [| |] |> ignore)
    |> should (throwWithMessage "Missing subcommand argument") typeof<System.Exception>

[<Fact>]
let ``Given text cli argument for lex subcommand, the main function should raise error``() =
    Moon.Main.main [| "lex"; "--text"; "123.014e-24" |] |> should equal 0
