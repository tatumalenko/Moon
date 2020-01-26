module Moon.Main

open System
open Moon
open Moon.Lexer
open Argu
open Argu

type LexerArgs =
    | Path of string
    | OutDir of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Path _ -> "path of the file to tokenize"
            | OutDir _ -> "directory for the output tokenized files"
and CommandArgs =
    | [<CliPrefix(CliPrefix.None)>] Lex of ParseResults<LexerArgs>
with
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
    
    let command =
        match args.[0] with
        | Lex lexer -> lexer
    
    for arg in command.GetAllResults() do
        match arg with
        | Path path -> inPath <- path
        | OutDir dir -> outDir <- dir
    
    if inPath = "" then
        Console.WriteLine(parser.PrintUsage())
        failwith "Missing command line argument --path"
    
    if outDir = "" then
        Console.WriteLine(parser.PrintUsage())
        failwith "Missing command line argument --outDir"
    
    let fileName = Utils.Path.fileName inPath
    let outcomes = tokenize (FilePath inPath)
    writeTokens outcomes (Some (Utils.Path.join outDir (fileName + ".outlextokens")))
    writeErrors outcomes (Some (Utils.Path.join outDir (fileName + ".outlexerrors")))
    
    0

