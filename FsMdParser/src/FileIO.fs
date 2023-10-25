namespace FsMdParser

open System

/// Example
///
/// task {
///     let! content = FileIO.ReadTextFile @"{filePath}"
///         Array.iter (fun s -> printfn $"{s}") content
///         
///         let contentToWrite = content
///                              |> Array.reduce (fun acc row -> $"{acc}\r\n{row}")
///         do! FileIO.WriteTextFile "test.txt" contentToWrite
///     }
module FileIO =
    open System.IO
     
    let ReadTextFile = File.ReadAllLinesAsync
        
    let WriteTextFile path (content: string) =
        task {
            use file = File.Create(path)
            let bytes = System.Text.Encoding.UTF8.GetBytes content
            do! file.WriteAsync(ReadOnlyMemory bytes)
        }