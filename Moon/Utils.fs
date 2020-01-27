namespace Moon

module Utils =
    open System.IO

    let read (path: string): string list =
        seq { yield! System.IO.File.ReadLines path } |> List.ofSeq

    let write (text: string) (path: string) =
        if not (Directory.Exists(Path.GetDirectoryName path)) then
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore

        File.WriteAllText(path, text)

    module Path =
        let join partialPath1 partialPath2 =
            Path.Combine(partialPath1, partialPath2)

        let fileName (path: string) =
            Path.GetFileName path
