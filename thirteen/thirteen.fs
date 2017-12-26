open System

type Layer = {
  depth: int;
  range: int
}

let parseLine(input: string): Layer =
  let array = input.Trim().Split([|':'|])
  printfn "%A" array
  {depth = (int array.[0]); range = (int array.[1])}

let inputFileToList(fileName: string): Layer[] =
  System.IO.File.ReadLines(fileName)
    |> Seq.toArray
    |> Array.map parseLine


[<EntryPoint>]
let main argv =
    printfn "%A" (inputFileToList "./thirteen2.txt")
    0
