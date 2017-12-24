open System

let parseLine(input: string): int*int[] =
  let array = input.Trim().Split([|"<->"|], StringSplitOptions.None)
  let values = array.[1].Split([|", "|], StringSplitOptions.None) |> Array.map int
  ((int array.[0]), values)

let inputFileToList(fileName: string) =
  System.IO.File.ReadLines(fileName)
    |> Seq.toList
    |> List.map parseLine

[<EntryPoint>]
let main argv =
    printfn "%A" (inputFileToList "./twelve2.txt")
    0
