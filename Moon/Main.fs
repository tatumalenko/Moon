module Moon.Main

open System
open System.Reflection
open System.Text.RegularExpressions
open System.Collections.Generic
open Moon
open Moon.Lexer
open Argu
open Moon.Grammar

type LexArgs =
    | Path of string
    | OutDir of string
    | Text of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Path _ -> "path of the file to tokenize"
            | OutDir _ -> "directory for the output tokenized files"
            | Text _ -> "text (string literal) to tokenize"

type ParseArgs =
    | Path of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Path _ -> "path of the file to parse"
            

and CommandArgs =
    | [<CliPrefix(CliPrefix.None)>] Lex of ParseResults<LexArgs>
    | [<CliPrefix(CliPrefix.None)>] Parse of ParseResults<ParseArgs>
    | [<CliPrefix(CliPrefix.None)>] CompileRegex
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Lex _ -> "runs the tokenization of a given source file"
            | Parse _ -> "parses the contents of a given source file"
            | CompileRegex -> "compile regex to dll"

let processLexCmd (command: ParseResults<LexArgs>) (parser: ArgumentParser<CommandArgs>) (args: CommandArgs list) =
    let mutable inPath = ""
    let mutable outDir = ""
    let mutable text = ""

    if Seq.length args = 0 then failwith "Missing subcommand argument"

    for arg in command.GetAllResults() do
        match arg with
        | LexArgs.Path path -> inPath <- path
        | LexArgs.OutDir dir -> outDir <- dir
        | LexArgs.Text txt -> text <- txt

    if text = "" then
        if inPath = "" then
            Console.WriteLine(parser.PrintUsage())
            failwith "Missing command line argument --path"

        if outDir = "" then
            Console.WriteLine(parser.PrintUsage())
            failwith "Missing command line argument --outDir"

    let outcomes = tokenize (if text = "" then FilePath inPath else InputType.Text text)

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
            (outcomes
             |> List.map string
             |> List.fold (fun state e ->
                 if state = "" then e else state + "\n" + e) "")

let processParseCmd (command: ParseResults<ParseArgs>) (parser: ArgumentParser<CommandArgs>) (args: CommandArgs list) =
    let mutable inPath = ""

    if Seq.length args = 0 then failwith "Missing subcommand argument"

    for arg in command.GetAllResults() do
        match arg with
        | ParseArgs.Path path -> inPath <- path

    if inPath = "" then
        Console.WriteLine(parser.PrintUsage())
        failwith "Missing command line argument --path"

    let fileName = Utils.Path.fileName inPath

    let grammar =
        Utils.read (Utils.Path.makePath "grammar.grm")
        |> ContextFreeGrammar.from

    match grammar with
    | Ok cfg ->
        let firstSets = Grammar.makeFirstSets cfg
        let followSets = Grammar.makeFollowSets cfg firstSets

        let outcomes = Lexer.tokenize (InputType.FilePath inPath)

        let mutable tokens = List.empty

        for outcome in outcomes do
            match outcome with
            | token when token.tokenType.isValid -> tokens <- tokens @ [ token ]
            | _ -> ignore()

        let table = Grammar.makeTable cfg firstSets followSets
        match table with
        | Ok parserTable ->
            let parserTableAsString = Parser.parserTableAsString parserTable
            Utils.write parserTableAsString (Utils.Path.makePath "parseTable.grm")

            let parser =
                { Parser.grammar = cfg
                  Parser.table = parserTable
                  Parser.firstSets = firstSets
                  Parser.followSets = followSets }

            match parser.sanitizeTokensAndParse tokens with
            | Ok (ast, derivationTable, syntaxErrors) ->
                Utils.write (Parser.astAsString ast) (Utils.Path.makePath "ast.grm")
                Utils.write
                    (ast.errors
                     |> List.map (fun e -> e.ToString())
                     |> String.concat "\n") (Utils.Path.makePath "astErrors.grm")
                Utils.write (AST.asGraphViz ast) (Utils.Path.makePath "ast.dot")
                Utils.write (Parser.derivationTableAsString derivationTable)
                    (Utils.Path.makePath "derivationTable.grm")
                Utils.write (Parser.syntaxErrorsAsString syntaxErrors) (Utils.Path.makePath "syntaxErrors.grm")
                ()
            | _ ->
                printfn "Oops. Something went wrong"

                ()

        | _ -> ()
    | Error _ -> ()
        
let processCompileRegexCmd (parser: ArgumentParser<CommandArgs>) =
    let tokenTypeCaseNames = Utils.unionCaseNames<TokenType>
    let tokenTypes = tokenTypeCaseNames |> Set.map Utils.makeUnionCase<TokenType>
    let mapTokenTypeToPattern (tokenType: TokenType) = (tokenType, tokenType.pattern)
    let tokenTypesAndPatterns = Set.map mapTokenTypeToPattern tokenTypes |> Array.ofSeq
    let regexes =
        tokenTypesAndPatterns
        |> Array.map (fun (tokenType, pattern) ->
            RegexCompilationInfo(
                pattern,
                RegexOptions.Compiled ||| RegexOptions.ExplicitCapture, 
                Utils.unionCaseName tokenType, 
                "Moon.DFA", 
                true))
        
    Regex.CompileToAssembly(regexes, AssemblyName("Moon.DFA, Version=1.0.0.1001, Culture=neutral, PublicKeyToken=null"))

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CommandArgs>(programName = "moon")

    let results = parser.Parse argv
    let args = results.GetAllResults()

    match args.[0] with
    | Lex lexer -> processLexCmd lexer parser args
    | Parse parse -> processParseCmd parse parser args
    | CompileRegex -> processCompileRegexCmd parser

    0
