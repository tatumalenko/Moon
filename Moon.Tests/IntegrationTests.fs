module Moon.Tests.IntegrationTests

open FSharpPlus
open Moon
open Moon.Tests
open Swensen.Unquote
open Xunit

type SourceContext =
    { path: string
      code: string option
      line: int option }

type CompilerOutput =
    { validTokens: Token list
      invalidTokens: Token list
      firstSets: Map<NonTerminal, FirstSet>
      followSets: Map<NonTerminal, FollowSet>
      ll1Table: Result<ParseTable, GrammarError>
      derivationTable: DerivationTable
      syntaxErrors: SyntaxError list
      syntaxTree: Tree<SyntaxElement>
      astErrors: ASTError list
      symbolTree: Tree<SymbolElement>
      symbolTable: SymbolTable
      semanticErrors: SemanticError list
      codeFactory: Semanter.CodeFactory
      moonCodeOutput: string }

let compile (sourceContext: SourceContext) (grammarPath: string) =
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
                SymbolTable.drawSymbolTable (symbolTree.root.symbolEntry @? SymbolTable.empty)

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

            let compilerOutput =
                { validTokens = Lexer.validTokens tokens
                  invalidTokens = Lexer.invalidTokens tokens
                  firstSets = firstSets
                  followSets = followSets
                  ll1Table = table
                  derivationTable = derivationTable
                  syntaxErrors = syntaxErrors
                  syntaxTree = syntaxElementTree
                  astErrors = ast.errors
                  symbolTree = symbolTree
                  symbolTable = symbolTree.root.symbolEntry @? SymbolTable.empty
                  semanticErrors = semanticErrors
                  codeFactory = codeFactory
                  moonCodeOutput = String.trimWhiteSpaces (Seq.tryItem 2 outputs @? "") }

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

module Lexer =
    [<Fact>]
    let positiveGrading () =
        let compilerOutput =
            compile
                { path = "lexer/positivegrading.src"
                  code = None
                  line = None } "grammar.grm"

        let invalidTokens = compilerOutput.invalidTokens
        let validTokens = compilerOutput.validTokens
        test <@ invalidTokens = [] @>

    [<Fact>]
    let negativeGrading () =
        let compilerOutput =
            compile
                { path = "lexer/negativegrading.src"
                  code = None
                  line = None } "grammar.grm"

        let invalidTokens = compilerOutput.invalidTokens
        let validTokens = compilerOutput.validTokens
        test <@ not (invalidTokens = []) @>

    [<Fact>]
    let testCase1 () =
        let compilerOutput =
            compile
                { path = "lexer/testcase1.src"
                  code = None
                  line = None } "grammar.grm"

        let invalidTokens = compilerOutput.invalidTokens
        let validTokens = compilerOutput.validTokens
        test <@ not (invalidTokens = []) @>

    [<Fact>]
    let testCase2 () =
        let compilerOutput =
            compile
                { path = "lexer/testcase2.src"
                  code = None
                  line = None } "grammar.grm"

        let invalidTokens = compilerOutput.invalidTokens
        let validTokens = compilerOutput.validTokens
        test <@ not (invalidTokens = []) @>

module Syntax =
    [<Fact>]
    let syntax () =
        let compilerOutput =
            compile
                { path = "syntax/syntax.src"
                  code = None
                  line = None } "grammar.grm"

        let syntaxErrors = compilerOutput.syntaxErrors
        test <@ syntaxErrors = [] @>

module SemanticAnalysis =
    [<Fact>]
    let invalidOperatorsOnObjects () =
        // 4.2.2 operators not allowed on objects
        let compilerOutput =
            compile
                { path = "semantics/4.2.2.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test
            <@ Utils.unionCaseNames semanticErrors = [ "AddTypeInvalid"; "MultTypeInvalid"; "AddTypeInvalid"; "MultTypeInvalid"; "RelTypeInvalid" ] @>

    [<Fact>]
    let invalidFreeFunctionParameterType () =
        // 4.2.3 checking of [[type]] and number of parameters upon a function call ([[free functions]] and member functions)
        let compilerOutput =
            compile
                { path = "semantics/4.2.3(a).src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [ "FunctionParamTypeMismatch" ] @>

    [<Fact>]
    let invalidFreeFunctionParameterNumber () =
        // 4.2.3 checking of type and [[number]] of parameters upon a function call ([[free functions]] and member functions)
        let compilerOutput =
            compile
                { path = "semantics/4.2.3(b).src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [ "FunctionArityMismatch" ] @>

    [<Fact>]
    let invalidMemberFunctionParameterType () =
        // 4.2.3 checking of [[type]] and number of parameters upon a function call (free functions and [[member functions]])
        let compilerOutput =
            compile
                { path = "semantics/4.2.3(c).src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [ "FunctionParamTypeMismatch" ] @>

    [<Fact>]
    let invalidMemberFunctionParameterNumber () =
        // 4.2.3 checking of type and [[number]] of parameters upon a function call (free functions and [[member functions]])
        let compilerOutput =
            compile
                { path = "semantics/4.2.3(d).src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [ "FunctionArityMismatch" ] @>

    [<Fact>]
    let invalidIndexedArrayDimension () =
        // 4.2.4 use of an array variable made using the same number of dimensions as declared in the variable declaration
        let compilerOutput =
            compile
                { path = "semantics/4.2.4.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [ "ArrayDimensionMismatch"; "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let nonIntegerExpressionUsedAsArrayIndex () =
        // 4.2.5 expressions used as an index must be of integer type
        let compilerOutput =
            compile
                { path = "semantics/4.2.5.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [ "ArrayIndexNonInteger"; "ArrayIndexNonInteger"; "ArrayIndexNonInteger" ] @>

    [<Fact>]
    let circularClassDependency () =
        // 4.2.6 circular class dependencies (through data members or inheritance) are detected and not allowed
        let compilerOutput =
            compile
                { path = "semantics/4.2.6.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [] @>

    [<Fact>]
    let invalidDotOperatorOnNonMemberVariable () =
        // 4.2.7 the “.” operator used only on variables of a class type (maybe same as undeclared member)
        let compilerOutput =
            compile
                { path = "semantics/4.2.7.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [ "UndeclaredLocalVariable"; "UndeclaredMemberVariable"; "UndeclaredMemberVariable" ] @>

    [<Fact>]
    let undeclaredFunction () =
        // 4.2.9 undeclared function: definition or call to a function that is not declared (free function or member function)
        let compilerOutput =
            compile
                { path = "semantics/4.2.9.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [] @>

    [<Fact>]
    let undefinedFunction () =
        // 4.2.10 undefined function: declaring a member function that does not have a corresponding function definition
        let compilerOutput =
            compile
                { path = "semantics/4.2.10.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [] @>

    [<Fact>]
    let undefinedClass () =
        // 4.2.11 undefined class
        let compilerOutput =
            compile
                { path = "semantics/4.2.11.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [] @>

    [<Fact>]
    let missingReturnStatement () =
        // 4.2.12 missing return statement
        let compilerOutput =
            compile
                { path = "semantics/4.2.12.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [] @>

    [<Fact>]
    let memberFunctionDeclAndDefMismatch () =
        // 4.2.13 mismatch between member function declaration and definition
        let compilerOutput =
            compile
                { path = "semantics/4.2.13.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [] @>

    [<Fact>]
    let shadowedInheritanceWarning () =
        // 4.2.14 warning for shadowed data members upon inheritance
        let compilerOutput =
            compile
                { path = "semantics/4.2.14.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [] @>

    [<Fact>]
    let unknownClassMemberFunctionDefinition () =
        // 4.2.15 member function defined as part of non-existing class
        let compilerOutput =
            compile
                { path = "semantics/4.2.15.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [] @>

    [<Fact>]
    let undeclaredLocalVariable () =
        // 4.2.16 undeclared variable: use of a local variable name for which there is no declaration
        let compilerOutput =
            compile
                { path = "semantics/4.2.16.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [] @>

    [<Fact>]
    let undeclaredDataMember () =
        // 4.2.17 undeclared data member: reference to a data member that is not declared (including in superclasses or deeply nested)
        let compilerOutput =
            compile
                { path = "semantics/4.2.17.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [] @>

    [<Fact>]
    let multiplyDeclaredVariable () =
        // 4.2.18 multiply declared variable: an identifier cannot be declared twice in the same scope
        let compilerOutput =
            compile
                { path = "semantics/4.2.18.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [] @>

    [<Fact>]
    let semantics () =
        let compilerOutput =
            compile
                { path = "semantics/semantics.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        let showSemanticErrors = List.map show semanticErrors
        test <@ Utils.unionCaseNames semanticErrors = [] @>

module CodeGeneration =
    [<Fact>]
    let intArithExpr () =
        let compilerOutput =
            compile
                { path = "codegen/intarithexpr.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ compilerOutput.semanticErrors = [] @>
        test <@ compilerOutput.moonCodeOutput = "38 36 38 7 38 0 2 38 9" @>

    [<Fact>]
    let logArithExpr () =
        let compilerOutput =
            compile
                { path = "codegen/logarithexpr.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ compilerOutput.semanticErrors = [] @>
        test <@ compilerOutput.moonCodeOutput = "-3 145 -145 1 0 0 0 1 1 1 1 0 0 0 0 1 1 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 -1 3 -145" @>

    [<Fact>]
    let relExpr () =
        let compilerOutput =
            compile
                { path = "codegen/relexpr.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ compilerOutput.semanticErrors = [] @>
        test <@ compilerOutput.moonCodeOutput = "1 1 0 0 0 1 0 0 1 1 1 1 0 0 0 1 1 0" @>

module Polynomial =
    [<Fact>]
    let ``Given polynomial source, then no semantic errors expected`` () =
        let compilerOutput =
            compile
                { path = "polynomial/polynomial.src"
                  code = None
                  line = None } "grammar.grm"

        let semanticErrors = compilerOutput.semanticErrors
        test <@ semanticErrors = [] @>

    [<Fact>]
    let ```localFloatVar = undeclaredVar;`, [UndeclaredLocalVariable]`` () =
        let compilerOutput =
            compile
                { path = "polynomial/polynomial.src"
                  code = Some "result = c;"
                  line = Some 33 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "UndeclaredLocalVariable" ] @>

    [<Fact>]
    let ```memberFloatVar = undeclaredVar;`, [UndeclaredLocalVariable]`` () =
        let compilerOutput =
            compile
                { path = "polynomial/polynomial.src"
                  code = Some "a = c;"
                  line = Some 33 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "UndeclaredLocalVariable" ] @>

    [<Fact>]
    let ```memberFloatVar = integerLiteral;`, []`` () =
        let compilerOutput =
            compile
                { path = "polynomial/polynomial.src"
                  code = Some "a = 1;"
                  line = Some 33 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [] @>

    [<Fact>]
    let ```localClassVar = localIntegerVar;`, [TypeMismatch]`` () =
        let compilerOutput =
            compile
                { path = "polynomial/polynomial.src"
                  code = Some "f1 = counter;"
                  line = Some 75 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "TypeMismatch" ] @>

    [<Fact>]
    let ```localClassVar = floatLiteral;`, [TypeMismatch]`` () =
        let compilerOutput =
            compile
                { path = "polynomial/polynomial.src"
                  code = Some "f1 = 2.0;"
                  line = Some 75 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "TypeMismatch" ] @>

    [<Fact>]
    let ```localFloatVar = undeclaredVar; undeclaredVar = memberFloatVar;`, [UndeclaredLocalVariable; UndeclaredLocalVariable]`` () =
        let compilerOutput =
            compile
                { path = "polynomial/polynomial.src"
                  code = Some "result = c; c = a;"
                  line = Some 33 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "UndeclaredLocalVariable"; "UndeclaredLocalVariable" ] @>

    [<Fact>]
    let ```Given polynomial/polynomial_deluxe.src, expect 24 semantic errors`` () =
        let compilerOutput =
            compile
                { path = "polynomial/polynomial_deluxe.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ compilerOutput.semanticErrors = [] @>

module MinProg =
    [<Fact>]
    let ``Given minprog source, then no semantic errors expected`` () =
        let compilerOutput =
            compile
                { path = "minprog/minProg.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ compilerOutput.semanticErrors = [] @>

module Conditionals =
    [<Fact>]
    let ``Given conditionals source, then no semantic errors expected`` () =
        let compilerOutput =
            compile
                { path = "conditionals/conditionals.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ compilerOutput.semanticErrors = [] @>

module MultRelExpr =
    [<Fact>]
    let ``Given multrelexpr source, then no semantic errors expected`` () =
        let compilerOutput =
            compile
                { path = "multrelexpr/multRelexpr.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ compilerOutput.semanticErrors = [] @>

module BubbleSort =
    [<Fact>]
    let ``Given bubblesort source, then out matches expected`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = None
                  line = None } "grammar.grm"
        test <@ compilerOutput.semanticErrors = [] @>

    [<Fact>]
    let ```arr[7] = unknownVar;`, [UnknownIdentifier]`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[7] = unknownVar;"
                  line = Some 60 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "UndeclaredLocalVariable" ] @>

    [<Fact>]
    let ```arr = 1;`, [ArrayDimensionMismatch]`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr = 1;"
                  line = Some 60 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1] = arr;`, [ArrayDimensionMismatch]`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1] = arr;"
                  line = Some 54 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1] = arr[1][1];`, [ArrayDimensionMismatch]`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1] = arr[1][1];"
                  line = Some 54 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1][1] = arr[1];`, [ArrayDimensionMismatch]`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1][1] = arr[1];"
                  line = Some 54 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1][1] = arr;`, [ArrayDimensionMismatch]`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1][1] = arr;"
                  line = Some 54 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "ArrayDimensionMismatch" ] @>

    [<Fact>]
    let ```arr[1] = arr[1];` then []`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1] = arr[1];"
                  line = Some 54 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [] @>

    [<Fact>]
    let ```arr[1][1][5] = arr; arr = unknownVar;` then [ArrayDimensionMismatch; UndeclaredLocalVariable]`` () =
        let compilerOutput =
            compile
                { path = "bubblesort/bubblesort.src"
                  code = Some "arr[1][1][5] = arr; arr = unknownVar;"
                  line = Some 54 } "grammar.grm"
        test <@ Utils.unionCaseNames compilerOutput.semanticErrors = [ "ArrayDimensionMismatch"; "UndeclaredLocalVariable" ] @>

module SymbolTable =
    [<Fact>]
    let Comparer () =
        let xs = Semanter.SymbolCheckVisitor.checkMultiplyDefined
        test <@ xs <> [] @>
