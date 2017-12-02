module two

open System

let inputFileToList(fileName: string): List<string> =
  System.IO.File.ReadLines(fileName) |> Seq.toList

let calc =
  inputFileToList "./input.txt"
    |> List.map (fun x -> x.Split('\t') |> Seq.map System.Int32.Parse |> Seq.toList)
    |> List.map (fun x -> (x |> List.max, x |> List.min))
    |> List.sumBy (fun x -> fst x - snd x)

[<EntryPoint>]
let main argv =
  printfn "%A" calc
  0
