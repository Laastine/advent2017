open System

let inputFileToList(fileName: string): List<string[]> =
  System.IO.File.ReadAllLines(fileName)
    |> Seq.toList
    |> List.map (fun x -> x.Split [|' '|])

let containsDuplicate(input: seq<string*int>): bool =
  input
    |> Seq.toList
    |> List.map (fun x -> (snd x))
    |> List.forall ((=) 1)

let uniques =
  inputFileToList "./input.txt"
    |> List.map (Seq.countBy id)
    |> List.filter (fun x -> (containsDuplicate(x)))
    |> List.length

[<EntryPoint>]
let main argv =
    printfn "%A" uniques
    0
