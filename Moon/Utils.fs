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
            
        if not (Directory.Exists (Path.GetDirectoryName fullPath)) then
            Directory.CreateDirectory (Path.GetDirectoryName fullPath) |> ignore

        File.WriteAllText(fullPath, text)
        
    module Path =
        let join partialPath1 partialPath2 =
            Path.Combine(partialPath1, partialPath2)
            
        let fileName (path: string) =
            Path.GetFileName path
