module Moon.Main

open Argu
open FSharpPlus
open System

let writeln (x: 'a) = Console.WriteLine(x)

type CompilerOutput =
    { validTokens: Token list
      invalidTokens: Token list
      showInvalidTokens: string
      firstSets: Map<NonTerminal, FirstSet>
      followSets: Map<NonTerminal, FollowSet>
      ll1Table: Result<ParseTable, GrammarError>
      derivationTable: DerivationTable
      syntaxErrors: SyntaxError list
      showSyntaxErrors: string
      syntaxTree: Tree<SyntaxElement>
      astErrors: ASTError list
      showAstErrors: string
      symbolTree: Tree<SymbolElement>
      symbolTable: SymbolTable
      semanticErrors: SemanticError list
      showSemanticErrors: string
      codeFactory: Semanter.CodeFactory
      moonCodeOutput: string
      codeGenErrors: string
      outFilePaths: string }

type ParseArgs =
    | Path of string
    | OutDir of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Path _ -> "path of the file to parse"
            | OutDir _ -> "directory for the output files"

let compile (sourcePath: string) (outDir: string) =
    let trimStartSlash (path: string) =
        if path.[0] = '/' then path.[1..] else path

    let sourceDir = __SOURCE_DIRECTORY__

    let grammarPath = Utils.makePath (sourceDir + "/resources/grammar.grm")

    let grammarFileText = Utils.read grammarPath
    let fileName = trimStartSlash (Utils.fileName sourcePath)
    let outRelativePath = outDir

    let sourceInPath = Utils.makePath sourcePath

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
    let semanticErrorsOutPath = Utils.makePath (outRelativePath + fileName + ".parse.semanticErrors")
    let symbolTableOutPath = Utils.makePath (outRelativePath + fileName + ".parse.symbols")
    let codeGenerationOutPath = Utils.makePath (outRelativePath + fileName + ".codegen.m")
    let codeGenerationOutTempPath = Utils.makePath (sourceDir + "/resources/moon/temp.m")
    let moonDirectoryPath = Utils.makePath (sourceDir + "/resources/moon")
    let moonExecutablePath = Utils.makePath (sourceDir + "/resources/moon/moon")
    let moonLibRelativePath = "./samples/lib.m"
    let moonCodeGenerationInputRelativePath = "./temp.m"
    let moonCodeGenerationOutputPath = (Utils.makePath (Utils.makePath (codeGenerationOutPath + ".out")))

    let mutable sourceCode = Utils.read sourceInPath

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

            Utils.write (String.concat "\n" (Seq.ofList (List.map show semanticErrors))) semanticErrorsOutPath

            let codeFactory = Semanter.CodeGenerationVisitor.visit symbolTree

            let symbolTableAsString =
                SymbolTable.drawSymbolTable (symbolTree.root.symbolEntry @? SymbolTable.empty)

            Utils.write (symbolTableAsString) symbolTableOutPath

            Utils.write (Ast.makeGraphViz symbolTree) symbolTreeDotOutPath

            Utils.write (show codeFactory) codeGenerationOutPath

            Utils.write (show codeFactory) codeGenerationOutTempPath

            Utils.CommandLineRunner.run (Utils.makePath outRelativePath) "/usr/local/bin/dot"
                ("-Tpdf " + syntaxTreeDotFileName + " -o " + syntaxTreeDotFileName + ".pdf") |> ignore

            Utils.CommandLineRunner.run (Utils.makePath outRelativePath) "/usr/local/bin/dot"
                ("-Tpdf " + symbolTreeDotFileName + " -o " + symbolTreeDotFileName + ".pdf") |> ignore

            let outputs, errors =
                Utils.CommandLineRunner.run moonDirectoryPath moonExecutablePath (moonLibRelativePath + " " + moonCodeGenerationInputRelativePath)

            Utils.write (String.concat "\n" (Seq.filter (fun e -> e <> null) outputs)) moonCodeGenerationOutputPath

            let outPaths =
                [ tokensOutPath
                  validTokensOutPath
                  invalidTokensOutPath
                  sanitizedTokensOutPath
                  grammarOutPath
                  firstSetsOutPath
                  followSetsOutPath
                  parseTableOutPath
                  derivationOutPath
                  parseErrorsOutPath
                  astOutPath
                  astErrorsOutPath
                  syntaxTreeDotOutPath
                  syntaxTreeDotOutPath + ".pdf"
                  semanticErrorsOutPath
                  symbolTableOutPath
                  symbolTreeDotOutPath
                  symbolTreeDotOutPath + ".pdf"
                  codeGenerationOutPath
                  moonCodeGenerationOutputPath ]

            let compilerOutput =
                { validTokens = Lexer.validTokens tokens
                  invalidTokens = Lexer.invalidTokens tokens
                  showInvalidTokens = Lexer.drawInvalidTokens tokens
                  firstSets = firstSets
                  followSets = followSets
                  ll1Table = table
                  derivationTable = derivationTable
                  syntaxErrors = syntaxErrors
                  showSyntaxErrors = Parser.drawSyntaxErrors syntaxErrors
                  syntaxTree = syntaxElementTree
                  astErrors = ast.errors
                  showAstErrors = Parser.drawAstErrors ast.errors
                  symbolTree = symbolTree
                  symbolTable = symbolTree.root.symbolEntry @? SymbolTable.empty
                  semanticErrors = semanticErrors
                  showSemanticErrors =
                      (String.concat "\n" (Seq.ofList (List.map show semanticErrors)))
                  codeFactory = codeFactory
                  moonCodeOutput = String.trimWhiteSpaces (Seq.tryItem 2 outputs @? "")
                  codeGenErrors =
                      String.concat "\n" errors
                  outFilePaths =
                      String.concat "\n" (Seq.ofList outPaths) }

            compilerOutput
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

let processCmd (command: ParseResults<ParseArgs>) (parser: ArgumentParser<ParseArgs>) (args: ParseArgs list) =
    let mutable inPath = ""
    let mutable outDir = ""

    for arg in command.GetAllResults() do
        match arg with
        | ParseArgs.Path path -> inPath <- path
        | ParseArgs.OutDir path -> outDir <- path

    if inPath = "" then
        writeln (parser.PrintUsage())
        failwith "Missing command line argument --path"

    writeln "-------------------------------------"
    writeln "        COMPILATION STARTED"
    writeln "-------------------------------------"
    writeln ("source path: " + inPath)
    writeln ("out dir path: " + outDir)
    writeln ""

    let output = compile inPath outDir

    writeln "-------------------------------------"
    writeln "          INVALID TOKENS"
    writeln "-------------------------------------"
    writeln output.showInvalidTokens
    writeln "-------------------------------------"
    writeln "           SYNTAX ERRORS"
    writeln "-------------------------------------"
    writeln output.showSyntaxErrors
    writeln "-------------------------------------"
    writeln "            AST ERRORS"
    writeln "-------------------------------------"
    writeln output.showAstErrors
    writeln "-------------------------------------"
    writeln "          SEMANTIC ERRORS"
    writeln "-------------------------------------"
    writeln output.showSemanticErrors
    writeln "-------------------------------------"
    writeln "          CODE GEN ERRORS"
    writeln "-------------------------------------"
    writeln output.codeGenErrors

    writeln "-------------------------------------"
    writeln "        COMPILATION FINISHED"
    writeln "-------------------------------------"
    writeln "File written:"
    writeln output.outFilePaths
    writeln ""
    ()


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<ParseArgs>(programName = "moon")

    let results = parser.Parse argv
    let args = results.GetAllResults()

    processCmd results parser args

    0
