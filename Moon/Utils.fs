namespace Moon

module Utils =
    open System.IO

    let read (path: string): string list =
        seq { yield! System.IO.File.ReadLines path } |> List.ofSeq

    let write (text: string) (path: string option) =
        let fullPath =
            match path with
            | Some s -> s
            | None -> Path.Combine(__SOURCE_DIRECTORY__, "output.txt")

        File.WriteAllText(fullPath, text)
