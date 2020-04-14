module Moon.Tests.IntegrationTests

open Moon
open Moon.Tests
open FSharpPlus
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
    let codeGenerationOutPath = Utils.makePath (outRelativePath + fileName + ".codegen.m")
    let moonDirectoryPath = Utils.makePath "resources/moon"
    let moonExecutablePath = Utils.makePath "resources/moon/moon"
    let moonLibRelativePath = "./samples/lib.m"
    let moonCodeGenerationInputRelativePath = "../grammar/out/" + directoryName + "/" + fileName + ".codegen.m"

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

            let semanticErrors, symbolTree = Semanter.check syntaxElementTree

            let codeFactory = Semanter.CodeGenerationVisitor.visit symbolTree

            let symbolTableAsString =
                SymbolTable.drawSymbolTable
                    (symbolTree.root.symbolEntry @! "IntegrationTests.parse: Tried to get `symbolTree.root.symbolEntry` but was None")

            Utils.write (symbolTableAsString) symbolTableOutPath

            Utils.write (Ast.makeGraphViz symbolTree) symbolTreeDotOutPath

            Utils.write (show codeFactory) codeGenerationOutPath

            Utils.CommandLineRunner.run (Utils.makePath outRelativePath) "/usr/local/bin/dot"
                ("-Tpdf " + syntaxTreeDotFileName + " -o " + syntaxTreeDotFileName + ".pdf") |> ignore

            Utils.CommandLineRunner.run (Utils.makePath outRelativePath) "/usr/local/bin/dot"
                ("-Tpdf " + symbolTreeDotFileName + " -o " + symbolTreeDotFileName + ".pdf") |> ignore

            let outputs, errors =
                Utils.CommandLineRunner.run moonDirectoryPath moonExecutablePath (moonLibRelativePath + " " + moonCodeGenerationInputRelativePath)

            Utils.write (String.concat "\n" (Seq.filter (fun e -> e <> null) outputs))
                (Utils.makePath (Utils.makePath (codeGenerationOutPath + ".out")))

            semanticErrors, String.trimWhiteSpaces (Seq.tryItem 2 outputs @? "")
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


module Demo =
    [<Fact>]
    let ``Given some demo source, then various semantic errors expected`` () =
        let semanticErrors, _ =
            parse
                { path = "demo/polynomial_mix.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ semanticErrors = [] @>

module CodeGeneration =
        [<Fact>]
        let intArithExpr () =
            let semanticErrors, output =
                parse
                    { path = "codegen/intarithexpr.src"
                      code = None
                      line = None } "grammar.grm"
            test <@ semanticErrors = [] @>
            test <@ output = "38 36 38 7 38 0 2 38 9" @>

        [<Fact>]
        let logArithExpr () =
            let semanticErrors, output =
                parse
                    { path = "codegen/logarithexpr.src"
                      code = None
                      line = None } "grammar.grm"
            test <@ semanticErrors = [] @>
            test <@ output = "-3 145 -145 1 0 0 0 1 1 1 1 0 0 0 0 1 1 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 -1 3 -145" @>

        [<Fact>]
        let relExpr () =
            let semanticErrors, output =
                parse
                    { path = "codegen/relexpr.src"
                      code = None
                      line = None } "grammar.grm"
            test <@ semanticErrors = [] @>
            test <@ output = "" @>

module Polynomial =
    [<Fact>]
    let ``Given polynomial source, then no semantic errors expected`` () =
        let semanticErrors, _ =
            parse
                { path = "polynomial/polynomial.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ semanticErrors = [] @>

    [<Fact>]
    let ```localFloatVar = undeclaredVar;`, [UndeclaredLocalVariable]`` () =
        let semanticErrors, _ =
            parse
                { path = "polynomial/polynomial.src"
                  code = Some "result = c;"
                  line = Some 33 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "UndeclaredLocalVariable" ] @>

    [<Fact>]
    let ```memberFloatVar = undeclaredVar;`, [UndeclaredLocalVariable]`` () =
        let semanticErrors, _ =
            parse
                { path = "polynomial/polynomial.src"
                  code = Some "a = c;"
                  line = Some 33 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "UndeclaredLocalVariable" ] @>

    [<Fact>]
    let ```memberFloatVar = integerLiteral;`, []`` () =
        let semanticErrors, _ =
            parse
                { path = "polynomial/polynomial.src"
                  code = Some "a = 1;"
                  line = Some 33 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [] @>

    [<Fact>]
    let ```localClassVar = localIntegerVar;`, [TypeMismatch]`` () =
        let semanticErrors, _ =
            parse
                { path = "polynomial/polynomial.src"
                  code = Some "f1 = counter;"
                  line = Some 75 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "TypeMismatch" ] @>

    [<Fact>]
    let ```localClassVar = floatLiteral;`, [TypeMismatch]`` () =
        let semanticErrors, _ =
            parse
                { path = "polynomial/polynomial.src"
                  code = Some "f1 = 2.0;"
                  line = Some 75 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "TypeMismatch" ] @>

    [<Fact>]
    let ```localFloatVar = undeclaredVar; undeclaredVar = memberFloatVar;`, [UndeclaredLocalVariable; UndeclaredLocalVariable]`` () =
        let semanticErrors, _ =
            parse
                { path = "polynomial/polynomial.src"
                  code = Some "result = c; c = a;"
                  line = Some 33 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "UndeclaredLocalVariable"; "UndeclaredLocalVariable" ] @>

    [<Fact>]
    let ```Given polynomial/polynomial_deluxe.src, expect 24 semantic errors`` () =
        let semanticErrors, _ =
            parse
                { path = "polynomial/polynomial_deluxe.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ semanticErrors = [] @>

module MinProg =
    [<Fact>]
    let ``Given minprog source, then no semantic errors expected`` () =
        let semanticErrors, _ =
            parse
                { path = "minprog/minProg.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ semanticErrors = [] @>

module Conditionals =
    [<Fact>]
    let ``Given conditionals source, then no semantic errors expected`` () =
        let semanticErrors, _ =
            parse
                { path = "conditionals/conditionals.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ semanticErrors = [] @>

module MultRelExpr =
    [<Fact>]
    let ``Given multrelexpr source, then no semantic errors expected`` () =
        let semanticErrors, _ =
            parse
                { path = "multrelexpr/multRelexpr.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ semanticErrors = [] @>

module BubbleSort =
    [<Fact>]
    let ``Given bubblesort source, then out matches expected`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ semanticErrors = [] @>

    [<Fact>]
    let ```arr[7] = unknownVar;`, [UnknownIdentifier]`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[7] = unknownVar;"
                  line = Some 60 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "UndeclaredLocalVariable" ] @>

    [<Fact>]
    let ```arr = 1;`, [ArrayDimensionMismatch]`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr = 1;"
                  line = Some 60 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1] = arr;`, [ArrayDimensionMismatch]`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1] = arr;"
                  line = Some 54 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1] = arr[1][1];`, [ArrayDimensionMismatch]`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1] = arr[1][1];"
                  line = Some 54 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1][1] = arr[1];`, [ArrayDimensionMismatch]`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1][1] = arr[1];"
                  line = Some 54 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1][1] = arr;`, [ArrayDimensionMismatch]`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1][1] = arr;"
                  line = Some 54 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1] = arr[1];` then []`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1] = arr[1];"
                  line = Some 54 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [] @>

    [<Fact>]
    let ```arr[1][1][5] = arr; arr = unknownVar;` then [ArrayDimensionMismatch; UndeclaredLocalVariable]`` () =
        let semanticErrors, _ =
            parse
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1][1][5] = arr; arr = unknownVar;"
                  line = Some 54 } "grammar.grm"
        test <@ List.map (fun it -> Utils.unionCaseName it) semanticErrors = [ "ArrayDimensionMismatch"; "UndeclaredLocalVariable" ] @>

module SymbolTable =
    [<Fact>]
    let Comparer () =
        let xs = Semanter.SymbolCheckVisitor.checkMultiplyDefined
        test <@ xs <> [] @>
