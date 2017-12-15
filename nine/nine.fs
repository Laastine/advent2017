open System

let explode (s: string): List<char> = [for c in s -> c]

let inputFileToList(fileName: string) =
  System.IO.File.ReadAllText(fileName)
    |> explode

[<EntryPoint>]
let main argv =
    printfn "%A" (inputFileToList("./nine.txt"))
    0 // return an integer exit code
