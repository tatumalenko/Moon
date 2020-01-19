namespace Moon.Lexer.Tests

module Utils =
    let a = 1

    let asTestArguments (args: obj list list): seq<obj []> =
        seq {
            for arg in args do
                yield Array.ofList arg
        }
