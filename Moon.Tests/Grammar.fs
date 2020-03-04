module Moon.Tests.Grammar

open Xunit
open FsUnit.Xunit
open Moon
open Moon.Grammar
open Moon.Language
open Moon.Lexer
open Moon.Tests
open Swensen.Unquote

let parse (sourcePath: string) (grammarPath: string) =
    let trimStartSlash (path: string) = if path.[0] = '/' then path.[1..] else path
    
    let sourceRelativePath = "resources/grammar/in/" + (trimStartSlash sourcePath) // e.g. sourcePath = 'polynomial/polynomial.src' from 'resources/grammar/in/'
    let grammarRelativePath = "resources/grammar/" + (trimStartSlash grammarPath) // e.g. grammarPath = 'grammar.grm' from 'resources/grammar/'
    let fileName = trimStartSlash (Utils.fileName sourcePath)
    let directoryName = (trimStartSlash (Utils.directoryName sourcePath)) // e.g. 'polynomial' from 'resources/grammar/[in|out|expected]/'
    let inRelativePath = "resources/grammar/in/" + directoryName + "/"
    let outRelativePath = "resources/grammar/out/" + directoryName + "/"
    let expectedRelativePath = "resources/grammar/expected/" + directoryName + "/"
    
    let grammarFileText = Utils.read (Utils.makePath grammarRelativePath)
    let grammar = ContextFreeGrammar.from grammarFileText

    match grammar with
    | Ok cfg ->
        Utils.write (string cfg) (Utils.makePath (outRelativePath + fileName + ".parse.grammar"))
        
        let firstSets = Grammar.makeFirstSets cfg
        let followSets = Grammar.makeFollowSets cfg firstSets
        Utils.write (Parser.firstSetsAsString firstSets) (Utils.makePath (outRelativePath + fileName + ".parse.first"))
        Utils.write (Parser.followSetsAsString followSets) (Utils.makePath (outRelativePath + fileName + ".parse.follow"))

        let tokens = Lexer.tokenize (InputType.FilePath (Utils.makePath sourceRelativePath))
        
        Utils.write (Lexer.displayTokens tokens) (Utils.makePath (outRelativePath + fileName + ".lexer.tokens"))
        Utils.write (Lexer.displayValidTokens tokens) (Utils.makePath (outRelativePath + fileName + ".lexer.tokens.valid"))
        Utils.write (Lexer.displayInvalidTokens tokens) (Utils.makePath (outRelativePath + fileName + ".lexer.tokens.invalid"))
        Utils.write (Lexer.displaySanitizedTokens tokens) (Utils.makePath (outRelativePath + fileName + ".lexer.tokens.sanitized"))

        let table = Grammar.makeTable cfg firstSets followSets
        match table with
        | Ok parserTable ->
            let parserTableAsString = Grammar.displayParseTable cfg table
            Utils.write parserTableAsString (Utils.makePath (outRelativePath + fileName + ".parse.table"))

            let parser =
                { Parser.grammar = cfg
                  Parser.table = parserTable
                  Parser.firstSets = firstSets
                  Parser.followSets = followSets }

            let (ast, derivationTable, syntaxErrors) =
                match parser.sanitizeTokensAndParse tokens with
                | Ok success -> success
                | Error failure -> failwith failure

            Utils.write (Parser.astAsString ast) (Utils.makePath (outRelativePath + fileName + ".parse.ast"))
            Utils.write
                (ast.errors
                 |> List.map (fun e -> e.ToString())
                 |> String.concat "\n") (Utils.makePath (outRelativePath + fileName + ".parse.ast.errors"))
            Utils.write (AST.asGraphViz ast) (Utils.makePath (outRelativePath + fileName + ".parse.ast.dot"))
            Utils.write (Parser.derivationTableAsString derivationTable) (Utils.makePath (outRelativePath + fileName + ".parse.derivation"))
            Utils.write (Parser.syntaxErrorsAsString syntaxErrors) (Utils.makePath (outRelativePath + fileName + ".parse.errors"))

            test <@ derivationTable <> List.empty @>
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

//[<Fact>]
//let ``Given a grammar source, then context free grammar properly parsed``() =
//    let variables = Set.ofList [ E; T; X; Y ]
//
//    let terminals =
//        Set.ofList
//            [ OpenBracket
//              ClosedBracket
//              IntegerLiteral ""
//              Plus
//              Asterisk ]
//
//    let productions =
//        [ { lhs = E
//            rhs =
//                [ Variable T
//                  Variable X ] }
//          { lhs = T
//            rhs =
//                [ Terminal OpenBracket
//                  Variable E
//                  Terminal ClosedBracket ] }
//          { lhs = T
//            rhs =
//                [ Terminal(IntegerLiteral "")
//                  Variable Y ] }
//          { lhs = X
//            rhs =
//                [ Terminal Plus
//                  Variable E ] }
//          { lhs = X
//            rhs = [ Epsilon ] }
//          { lhs = Y
//            rhs =
//                [ Terminal Asterisk
//                  Variable T ] }
//          { lhs = Y
//            rhs = [ Epsilon ] } ]
//    let cfg =
//        { variables = variables
//          terminals = terminals
//          start = E
//          productions = productions }
//
//    let firstSets = Grammar.makeFirstSets cfg
//
//    let followSets = Grammar.makeFollowSets cfg firstSets
//
//    test
//        <@ firstSets
//           |> Utils.shouldEqual
//               (Map.ofList
//                   [ (E,
//                      Set.ofList
//                          [ FirstType.Terminal(IntegerLiteral "")
//                            FirstType.Terminal OpenBracket ])
//                     (T,
//                      Set.ofList
//                          [ FirstType.Terminal(IntegerLiteral "")
//                            FirstType.Terminal OpenBracket ])
//                     (X,
//                      Set.ofList
//                          [ FirstType.Terminal Plus
//                            FirstType.Epsilon ])
//                     (Y,
//                      Set.ofList
//                          [ FirstType.Terminal Asterisk
//                            FirstType.Epsilon ]) ]) @>
//
//    test
//        <@ followSets
//           |> Utils.shouldEqual
//               (Map.ofList
//                   [ (E,
//                      Set.ofList
//                          [ FollowType.Terminal ClosedBracket
//                            FollowType.DollarSign ])
//                     (T,
//                      Set.ofList
//                          [ FollowType.Terminal Plus
//                            FollowType.Terminal ClosedBracket
//                            FollowType.DollarSign ])
//                     (X,
//                      Set.ofList
//                          [ FollowType.Terminal ClosedBracket
//                            FollowType.DollarSign ])
//                     (Y,
//                      Set.ofList
//                          [ FollowType.Terminal Plus
//                            FollowType.Terminal ClosedBracket
//                            FollowType.DollarSign ]) ]) @>
//
//    let outcomes = Lexer.tokenize (InputType.Text "(1 * 3) + 6")
//    let mutable tokens = List.empty
//
//    for outcome in outcomes do
//        match outcome with
//        | token when outcome.tokenType.isValid ->
//            tokens <- tokens @ [ token ]
//        | _ -> ignore()
//
//    let table = Grammar.makeTable cfg firstSets followSets
//    match table with
//    | Ok parserTable ->
//        let parserTableAsString = Parser.parserTableAsString parserTable
//        Utils.write parserTableAsString (Utils.makePath "resources/grammar/parseTable.grm")
//
//        let parser =
//            { Parser.grammar = cfg
//              Parser.table = parserTable
//              Parser.firstSets = firstSets
//              Parser.followSets = followSets }
//
//        let (ast, derivationTable, syntaxErrors) =
//            match parser.sanitizeTokensAndParse tokens with
//            | Ok ok -> ok
//            | Error failure -> failwith failure
//        //Utils.write (Parser.firstSetsAsString firstSets) (Utils.makePath "resources/grammar/ast.grm")
//        Utils.write (Parser.derivationTableAsString derivationTable) (Utils.makePath "resources/grammar/derivationTable.grm")
//        Utils.write (Parser.syntaxErrorsAsString syntaxErrors) (Utils.makePath "resources/grammar/errors.grm")
//
//        test <@ derivationTable = [] @>
//    | _ -> ()
//
//    test <@ firstSets <> Map.empty @>
//
//[<Fact>]
//let ``Given a grammar source, then context free grammar properly parsed2``() =
//    let variables = Set.ofList [ X; Y; Z ]
//    let terminals = Set.ofList [ A; B; C; D ]
//
//    let productions =
//        [ { lhs = Z
//            rhs = [ Terminal D ] }
//          { lhs = Z
//            rhs =
//                [ Variable X
//                  Variable Y
//                  Variable Z ] }
//          { lhs = Y
//            rhs = [ Terminal C ] }
//          { lhs = Y
//            rhs = [ Epsilon ] }
//          { lhs = X
//            rhs = [ Variable Y ] }
//          { lhs = X
//            rhs = [ Terminal A ] } ]
//    let cfg =
//        { variables = variables
//          terminals = terminals
//          start = Z
//          productions = productions }
//
//    let firstSets = Grammar.makeFirstSets cfg
//
//    let followSets = Grammar.makeFollowSets cfg firstSets
//
//    firstSets
//    |> should equal
//           (Map.ofList
//               [ (X,
//                  Set.ofList
//                      [ FirstType.Terminal A
//                        FirstType.Terminal C
//                        FirstType.Epsilon ])
//                 (Y,
//                  Set.ofList
//                      [ FirstType.Terminal C
//                        FirstType.Epsilon ])
//                 (Z,
//                  Set.ofList
//                      [ FirstType.Terminal A
//                        FirstType.Terminal C
//                        FirstType.Terminal D ]) ])
//
//    test <@
//    followSets
//    |> Utils.shouldEqual
//           (Map.ofList
//               [ (X,
//                  Set.ofList
//                      [ FollowType.Terminal A
//                        FollowType.Terminal C
//                        FollowType.Terminal D ])
//                 (Y,
//                  Set.ofList
//                      [ FollowType.Terminal A
//                        FollowType.Terminal C
//                        FollowType.Terminal D ])
//                 (Z, Set.ofList [ FollowType.DollarSign ]) ])
//    @>
//
//    let table = Grammar.makeTable cfg firstSets followSets
//
//    match table with
//    | Ok success ->
//        test <@ success <> Map.empty @>
//    | Error failure ->
//        match failure with
//        | CollisionsInTable failure ->
//            failwith
//                (failure
//                 |> String.concat "\n")
//        | InvalidVariable failure -> failwith failure
//        | InvalidSymbol failure -> failwith failure
//
//[<Fact>]
//let ``Given grammar file, then correct first and follow sets found``() =
//    //    let grammar = Utils.read (Utils.makePath "resources/grammar/grammar.grm")
//    let grammarFileText = Utils.read (Utils.makePath "resources/grammar/grammar_with_actions2.grm")
//    let grammar = grammarFileText
//                  |> ContextFreeGrammar.from
//
//    match grammar with
//    | Ok cfg ->
//        let firstSets = Grammar.makeFirstSets cfg
//        let followSets = Grammar.makeFollowSets cfg firstSets
//        Utils.write (Parser.firstSetsAsString firstSets) (Utils.makePath "resources/grammar/firstsets.grm")
//        Utils.write (Parser.followSetsAsString followSets) (Utils.makePath "resources/grammar/followsets.grm")
//
//        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
//        
////        let outcomes = Lexer.tokenize (InputType.Text "class POLYNOMIAL inherits POOP, CRAP { public evaluate(float x) : float; }; POLYNOMIAL::evaluate(float x) : float do return (0); end main local linear f1; quadratic f2; integer counter; do f1 = f1.build(2, 3.5); end")
//        //        let outcomes = Lexer.tokenize (InputType.FilePath (Utils.makePath "resources/grammar/input/bubblesort_without_comments.src"))
//        //        let outcomes = Lexer.tokenize (InputType.FilePath (Utils.makePath "resources/grammar/input/print_arr.src"))
//        let tokens = Lexer.tokenize (InputType.FilePath(Utils.makePath "resources/grammar/input/polynomial.src"))
//        //        let outcomes = Lexer.tokenize (InputType.FilePath (Utils.makePath "resources/grammar/input/simple_error_check.src"))
//        //        let outcomes = Lexer.tokenize (InputType.FilePath (Utils.makePath "resources/grammar/input/main.src"))
//        //        let outcomes = Lexer.tokenize (InputType.FilePath (Utils.makePath "resources/grammar/input/main_do.src"))
//        
//        stopWatch.Stop()
//        let elapsedTotalMs = stopWatch.Elapsed.TotalMilliseconds
//        printfn "%f" elapsedTotalMs
//        
//        Utils.write (Lexer.displayValidTokens tokens) (Utils.makePath "resources/grammar/validTokens.grm")
//        Utils.write (Lexer.displayInvalidTokens tokens) (Utils.makePath "resources/grammar/invalidTokens.grm")
//        Utils.write (Lexer.displaySanitizedTokens tokens) (Utils.makePath "resources/grammar/sanitizedTokens.grm")
//
//        let table = Grammar.makeTable cfg firstSets followSets
//        match table with
//        | Ok parserTable ->
//            let parserTableAsString = Parser.parserTableAsString parserTable
//            Utils.write parserTableAsString (Utils.makePath "resources/grammar/parseTable.grm")
//
//            let parser =
//                { Parser.grammar = cfg
//                  Parser.table = parserTable
//                  Parser.firstSets = firstSets
//                  Parser.followSets = followSets }
//
//            let (ast, derivationTable, syntaxErrors) =
//                match parser.sanitizeTokensAndParse tokens with
//                | Ok success -> success
//                | Error failure -> failwith failure
//
//            Utils.write (Parser.astAsString ast) (Utils.makePath "resources/grammar/ast.grm")
//            Utils.write
//                (ast.errors
//                 |> List.map (fun e -> e.ToString())
//                 |> String.concat "\n") (Utils.makePath "resources/grammar/astErrors.grm")
//            Utils.write (AST.asGraphViz ast) (Utils.makePath "resources/grammar/ast.dot")
//            Utils.write (Parser.derivationTableAsString derivationTable) (Utils.makePath "resources/grammar/derivationTable.grm")
//            Utils.write (Parser.syntaxErrorsAsString syntaxErrors) (Utils.makePath "resources/grammar/errors.grm")
//
//            test <@ derivationTable <> List.empty @>
//        | Error failure ->
//            match failure with
//            | CollisionsInTable failure ->
//                failwith
//                    (failure
//                     |> String.concat "\n")
//            | InvalidVariable failure -> failwith failure
//            | InvalidSymbol failure -> failwith failure
//    | Error failure ->
//        match failure with
//        | CollisionsInTable failure ->
//            failwith
//                (failure
//                 |> String.concat "\n")
//        | InvalidVariable failure -> failwith failure
//        | InvalidSymbol failure -> failwith failure

[<Fact>]
let ``Given polynomial source, then out matches expected`` () =
    parse "polynomial/polynomial.src" "grammar_with_actions2.grm"

[<Fact>]
let ``Given bubblesort source, then out matches expected`` () =
    parse "bubblesort/bubblesort.src" "grammar.grm"
    
[<Fact>]
let ``Given minprog source, then out matches expected`` () =
    parse "minprog/minProg.src" "grammar_with_actions2.grm"
    
[<Fact>]
let ``Given conditionals source, then out matches expected`` () =
    parse "conditionals/conditionals.src" "grammar.grm"
    
[<Fact>]
let ``Given multrelexpr source, then out matches expected`` () =
    parse "multrelexpr/multRelexpr.src" "grammar_with_actions2.grm"