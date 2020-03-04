    module Moon.Utils
    
    open System.IO
    open Microsoft.FSharp.Reflection
    
    let unionCases<'T> =
        typeof<'T> |> FSharpType.GetUnionCases |> List.ofSeq
        
    let makeUnionCase<'T> (name: string) =
        let case = unionCases<'T> |> List.filter (fun e -> e.Name = name) |> List.head
        FSharpValue.MakeUnion(case, [||]) :?> 'T
        
    let unionCaseName (x: 'U) =
        match FSharpValue.GetUnionFields(x, typeof<'U>) with
        | case, _ -> case.Name
     
    let unionCaseNames<'T> =
        typeof<'T> |> FSharpType.GetUnionCases |> Array.map (fun e -> e.Name) |> Set.ofSeq
        
    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None
    
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
    