module Moon.Utils

open System
open System.Diagnostics
open System.IO
open Microsoft.FSharp.Reflection

let unionCases<'T> =
    typeof<'T>
    |> FSharpType.GetUnionCases
    |> List.ofSeq

let makeUnionCase<'T> (name: string) =
    let case =
        unionCases<'T>
        |> List.filter (fun e -> e.Name = name)
        |> List.head
    FSharpValue.MakeUnion(case, [||]) :?> 'T

let unionCaseName (x: 'U) =
    match FSharpValue.GetUnionFields(x, typeof<'U>) with
    | case, _ -> case.Name

let unionCaseNames<'T> =
    typeof<'T>
    |> FSharpType.GetUnionCases
    |> Array.map (fun e -> e.Name)
    |> Set.ofSeq

let fromString<'a> (s: string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
    | _ -> None

let read (path: string): string =
    System.IO.File.ReadAllText path

let write (text: string) (path: string) =
    if not (Directory.Exists(Path.GetDirectoryName path)) then Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore

    File.WriteAllText(path, text)

module Path =
    let join partialPath1 partialPath2 =
        Path.Combine(partialPath1, partialPath2)

    let fileName (path: string) =
        Path.GetFileName path

    let makePath (path: string) =
        Path.Combine(__SOURCE_DIRECTORY__, path)

[<RequireQualifiedAccess>]
module CommandLineRunner =
    let run (workingDirectory: string) (executablePath: string) (args: string) =
        let processStartInfo =
            ProcessStartInfo
                (RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false, FileName = executablePath, Arguments = args,
                 WorkingDirectory = workingDirectory)
        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let outputHandler f (_sender: obj) (args: DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = processStartInfo)
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler(outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler(outputHandler errors.Add))
        let started =
            try
                p.Start()
            with ex ->
                ex.Data.Add("filename", executablePath)
                reraise()
        if not started then failwithf "Failed to start process %s" executablePath
        printfn "Started %s with pid %i" p.ProcessName p.Id
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        printfn "Finished %s" executablePath
        let cleanOut l = l |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)
        cleanOut outputs, cleanOut errors
