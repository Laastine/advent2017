open System

let explode (s: string): List<char> = [for c in s -> c]

let inputFileToList(fileName: string): List<List<string>> =
  System.IO.File.ReadAllLines(fileName)
    |> Seq.toList
    |> List.map (fun x -> x.Split [|' '|])
    |> List.map List.ofArray

let containsDuplicate(input: List<string>): bool =
  input
    |> Seq.countBy id
    |> Seq.toList
    |> List.map snd
    |> List.forall ((=) 1)

let containsNoAnagrams(input: seq<string>) =
  input
    |> Seq.toList
    |> List.map (fun x -> ((explode x)))
    |> List.map List.sort
    |> List.map (fun (x: List<char>) -> System.String.Concat(Array.ofList(x)))

let uniques() =
  inputFileToList "./input.txt"
    |> List.filter containsDuplicate
    |> List.map containsNoAnagrams
    |> List.filter containsDuplicate
    |> List.length

[<EntryPoint>]
let main argv =
    printfn "%A" (uniques())
    0
