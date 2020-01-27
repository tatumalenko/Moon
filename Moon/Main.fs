module Moon.Main

open System
open Moon
open Moon.Lexer
open Argu

type LexerArgs =
    | Path of string
    | OutDir of string
    | Text of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Path _ -> "path of the file to tokenize"
            | OutDir _ -> "directory for the output tokenized files"
            | Text _ -> "text (string literal) to tokenize"

and CommandArgs =
    | [<CliPrefix(CliPrefix.None)>] Lex of ParseResults<LexerArgs>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Lex _ -> "runs the tokenization of a given source file"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CommandArgs>(programName = "moon")

    let results = parser.Parse argv
    let args = results.GetAllResults()

    let mutable inPath = ""
    let mutable outDir = ""
    let mutable text = ""

    if Seq.length args = 0 then failwith "Missing subcommand argument"

    let command =
        match args.[0] with
        | Lex lexer -> lexer

    for arg in command.GetAllResults() do
        match arg with
        | Path path -> inPath <- path
        | OutDir dir -> outDir <- dir
        | Text txt -> text <- txt

    if text = "" then
        if inPath = "" then
            Console.WriteLine(parser.PrintUsage())
            failwith "Missing command line argument --path"

        if outDir = "" then
            Console.WriteLine(parser.PrintUsage())
            failwith "Missing command line argument --outDir"

    let outcomes =
        if text = "" then
            tokenize (FilePath inPath)
        else
            tokenize
                (InputType.Text
                    (text.Split "\n"
                     |> List.ofSeq))

    if text = "" then
        let fileName = Utils.Path.fileName inPath
        writeTokens outcomes (Utils.Path.join outDir (fileName + ".outlextokens"))
        writeErrors outcomes (Utils.Path.join outDir (fileName + ".outlexerrors"))
    else
        printfn ""
        printfn "OUTLEXTOKENS:"
        printfn "%s" (display outcomes)
        printfn ""
        printfn "OUTLEXERRORS:"
        printfn "%s"
            (lexicalErrorsFromOutcomes outcomes
             |> List.map (fun e -> e.displayDetailed)
             |> List.fold (fun state e ->
                 if state = "" then e
                 else state + "\n" + e) "")

    0
