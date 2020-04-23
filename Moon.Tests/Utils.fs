module Moon.Tests.Utils

open FSharpPlus
open Moon
open System.IO

/// <summary>Converts a list of object lists into a sequence of object arrays.</summary>
/// <param name="args">The list of object lists to convert.</param>
/// <returns>The sequence of object arrays.</returns>
let asTestArguments (args: obj list list): seq<obj array> =
    seq {
        for arg in args do
            yield Array.ofList arg
    }

let unionCaseNames discriminatedUnionCases =
    List.map Utils.unionCaseName discriminatedUnionCases

let makePath (path: string) =
    Path.Combine(__SOURCE_DIRECTORY__, path)

let fileName (path: string) =
    System.IO.Path.GetFileNameWithoutExtension(path)

let directoryName (path: string) =
    System.IO.Path.GetDirectoryName(path)

let shouldEqual<'T when 'T: equality> (arg1: 'T) (arg2: 'T) = arg1 = arg2

let appendTextAtLine (textToAdd: string) (line: int) (text: string) =
    String.split [ "\n" ] text
    |> List.ofSeq
    |> List.splitAt (line - 1)
    ||> (fun list1 list2 -> list1 @ [ textToAdd ] @ list2)
    |> String.concat "\n"
