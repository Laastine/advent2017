open System

let calcIndex(idx: int, len: int): int =
  int (System.Math.Round((((float idx) / (float len)) % 1.0) * (float len)))

let inputFileToList(fileName: string): List<int> =
  System.IO.File.ReadAllText(fileName).Split([|','|])
    |> Array.toList
    |> List.map int

[<EntryPoint>]
let main argv =
  printfn "%A" (inputFileToList "./ten2.txt")
  0
