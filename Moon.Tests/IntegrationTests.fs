module Moon.Tests.Grammar

open Moon
open Moon.Tests
open Swensen.Unquote
open Xunit

type SourceContext =
    { path: string
      code: string option
      line: int option }

let parse (sourceContext: SourceContext) (grammarPath: string) =
    let trimStartSlash (path: string) =
        if path.[0] = '/' then path.[1..] else path

    let sourcePath = sourceContext.path

    let sourceRelativePath = "resources/grammar/in/" + (trimStartSlash sourcePath) // e.g. sourcePath = 'polynomial/polynomial.src' from 'resources/grammar/in/'
    let grammarRelativePath = "resources/grammar/" + (trimStartSlash grammarPath) // e.g. grammarPath = 'grammar.grm' from 'resources/grammar/'
    let grammarFileText = Utils.read (Utils.makePath grammarRelativePath)
    let fileName = trimStartSlash (Utils.fileName sourcePath)
    let directoryName = (trimStartSlash (Utils.directoryName sourcePath)) // e.g. 'polynomial' from 'resources/grammar/[in|out|expected]/'
    let inRelativePath = "resources/grammar/in/" + directoryName + "/"
    let outRelativePath = "resources/grammar/out/" + directoryName + "/"
    let expectedRelativePath = "resources/grammar/expected/" + directoryName + "/"

    let sourceInPath = Utils.makePath sourceRelativePath

    let grammarOutPath = Utils.makePath (outRelativePath + fileName + ".parse.grammar")
    let firstSetsOutPath = Utils.makePath (outRelativePath + fileName + ".parse.first")
    let followSetsOutPath = Utils.makePath (outRelativePath + fileName + ".parse.follow")
    let parseTableOutPath = Utils.makePath (outRelativePath + fileName + ".parse.table")
    let tokensOutPath = Utils.makePath (outRelativePath + fileName + ".lexer.tokens")
    let validTokensOutPath = Utils.makePath (outRelativePath + fileName + ".lexer.tokens.valid")
    let invalidTokensOutPath = Utils.makePath (outRelativePath + fileName + ".lexer.tokens.invalid")
    let sanitizedTokensOutPath = Utils.makePath (outRelativePath + fileName + ".lexer.tokens.sanitized")
    let astOutPath = Utils.makePath (outRelativePath + fileName + ".parse.ast")
    let astErrorsOutPath = Utils.makePath (outRelativePath + fileName + ".parse.ast.errors")
    let syntaxTreeDotFileName = fileName + ".parse.ast.dot"
    let symbolTreeDotFileName = fileName + ".parse.symbolTree.dot"
    let syntaxTreeDotOutPath = Utils.makePath (outRelativePath + syntaxTreeDotFileName)
    let symbolTreeDotOutPath = Utils.makePath (outRelativePath + symbolTreeDotFileName)
    let derivationOutPath = Utils.makePath (outRelativePath + fileName + ".parse.derivation")
    let parseErrorsOutPath = Utils.makePath (outRelativePath + fileName + ".parse.errors")
    let symbolTableOutPath = Utils.makePath (outRelativePath + fileName + ".parse.symbols")

    let mutable sourceCode =
        match sourceContext.code, sourceContext.line with
        | Some code, Some line ->
            Utils.read sourceInPath |> Utils.appendTextAtLine code line
        | _ -> Utils.read sourceInPath

    let tokens = Lexer.tokenize (InputType.Text sourceCode)
    Utils.write (Lexer.drawTokens tokens) tokensOutPath
    Utils.write (Lexer.drawValidTokens tokens) validTokensOutPath
    Utils.write (Lexer.drawInvalidTokens tokens) invalidTokensOutPath
    Utils.write (Lexer.drawSanitizedTokens tokens) sanitizedTokensOutPath

    match Grammar.from grammarFileText with
    | Ok cfg ->
        Utils.write (show cfg) grammarOutPath

        let firstSets = Grammar.makeFirstSets cfg
        let followSets = Grammar.makeFollowSets cfg firstSets
        Utils.write (Parser.drawFirstSets firstSets) firstSetsOutPath
        Utils.write (Parser.drawFollowSets followSets) followSetsOutPath

        let table = Grammar.makeParseTable cfg firstSets followSets
        Utils.write (Grammar.displayParseTable cfg table) parseTableOutPath

        match Grammar.makeParseTable cfg firstSets followSets with
        | Ok parserTable ->
            let parser =
                { Parser.grammar = cfg
                  Parser.table = parserTable
                  Parser.firstSets = firstSets
                  Parser.followSets = followSets }

            let (ast, derivationTable, syntaxErrors) =
                match parser.sanitizeTokensAndParse tokens with
                | Ok success -> success
                | Error failure -> failwith failure

            Utils.write (Parser.drawDerivationTable derivationTable) derivationOutPath

            Utils.write (Parser.drawSyntaxErrors syntaxErrors) parseErrorsOutPath

            Utils.write (Parser.drawIndexedAst ast) astOutPath

            Utils.write (Parser.drawAstErrors ast.errors) astErrorsOutPath

            let syntaxElementTree = Ast.makeTree ast

            Utils.write (Ast.makeGraphViz syntaxElementTree) syntaxTreeDotOutPath

            let symbolTable, symbolTree = SymbolTable.makeSymbolTableAndTree syntaxElementTree

            let symbolTableAsString = SymbolTable.drawSymbolTable symbolTable

            Utils.write (symbolTableAsString) symbolTableOutPath

            Utils.write (Ast.makeGraphViz symbolTree) symbolTreeDotOutPath

            let semanticErrors = Semanter.check symbolTable

            let outputs, errors =
                Utils.CommandLineRunner.run (Utils.makePath outRelativePath) "/usr/local/bin/dot"
                    ("-Tpdf " + syntaxTreeDotFileName + " -o " + syntaxTreeDotFileName + ".pdf")

            let outputs, errors =
                Utils.CommandLineRunner.run (Utils.makePath outRelativePath) "/usr/local/bin/dot"
                    ("-Tpdf " + symbolTreeDotFileName + " -o " + symbolTreeDotFileName + ".pdf")

            test <@ derivationTable <> List.empty @>

            semanticErrors
        | Error failure ->
            match failure with
            | CollisionsInTable failure ->
                failwith
                    (failure
                     |> String.concat "\n")
            | InvalidVariable failure -> failwith failure
            | InvalidSymbol failure -> failwith failure
    | Error failure ->
        match failure with
        | CollisionsInTable failure ->
            failwith
                (failure
                 |> String.concat "\n")
        | InvalidVariable failure -> failwith failure
        | InvalidSymbol failure -> failwith failure

[<Fact>]
let ``Given polynomial source, then out matches expected``() =
    parse
        { path = "polynomial/polynomial.src"
          code = None
          line = None } "grammar.grm"

[<Fact>]
let ``Given bubblesort source, then out matches expected``() =
    parse
        { path = "bubblesort/bubblesort.src"
          code = None
          line = None } "grammar.grm"

[<Fact>]
let ``bubblesort/bubblesort.src, `arr[7] = unknownVar;`, UnknownIdentifier``() =
    let semanticErrors =
        parse
            { path = "bubblesort/bubblesort.src"
              code = Some "arr[7] = unknownVar;"
              line = Some 60 } "grammar.grm"
    test <@ semanticErrors = [ UnknownIdentifier ] @>

[<Fact>]
let ``bubblesort/bubblesort.src, `arr = 1;`, TypeMismatch``() =
    let semanticErrors =
        parse
            { path = "bubblesort/bubblesort.src"
              code = Some "arr = 1;"
              line = Some 60 } "grammar.grm"
    test <@ semanticErrors = [ TypeMismatch ] @>

[<Fact>]
let ``bubblesort/bubblesort.src, `arr[1] = arr;`, TypeMismatch``() =
    let semanticErrors =
        parse
            { path = "bubblesort/bubblesort.src"
              code = Some "arr[1] = arr;"
              line = Some 54 } "grammar.grm"
    test <@ semanticErrors = [ TypeMismatch ] @>

[<Fact>]
let ``Given minprog source, then out matches expected``() =
    parse
        { path = "minprog/minProg.src"
          code = None
          line = None } "grammar.grm"

[<Fact>]
let ``Given conditionals source, then out matches expected``() =
    parse
        { path = "conditionals/conditionals.src"
          code = None
          line = None } "grammar.grm"

[<Fact>]
let ``Given multrelexpr source, then out matches expected``() =
    parse
        { path = "multrelexpr/multRelexpr.src"
          code = None
          line = None } "grammar.grm"
