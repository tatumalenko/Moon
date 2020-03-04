module Moon.Tests.Utils

open System.IO

/// <summary>Converts a list of object lists into a sequence of object arrays.</summary>
/// <param name="args">The list of object lists to convert.</param>
/// <returns>The sequence of object arrays.</returns>
let asTestArguments (args: obj list list): seq<obj array> =
    seq {
        for arg in args do
            yield Array.ofList arg
    }

let makePath (path: string) =
    Path.Combine(__SOURCE_DIRECTORY__, path)
    
let fileName (path: string) =
    System.IO.Path.GetFileNameWithoutExtension (path)
    
let directoryName (path: string) =
    System.IO.Path.GetDirectoryName (path)
    
let shouldEqual<'T when 'T: equality> (arg1: 'T) (arg2: 'T) = arg1 = arg2