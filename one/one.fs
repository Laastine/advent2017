open System

let explode (s: string): List<int> = [for c in s -> c |> string |> int]

let inputFileToList(fileName: string): List<string> =
  System.IO.File.ReadLines(fileName)
  |> Seq.toList

let calculateValue(inputList: List<int>): int =
  let first = inputList.Head
  let rec recur(list: List<int>, acc: int) =
    match list with
    | [] -> acc
    | head::tail ->
                      // printfn "B: %A::%A %A, first: %A" head tail acc first
                      if list.Length = 1 && first = head then recur(tail, acc+first)
                      elif list.Length > 1 && head = tail.Head then recur(tail, acc+head)
                      else recur(tail, acc)
  recur(inputList, 0)

let calc: int =
  inputFileToList "./input.txt"
  // ["1";"1";"2";"2"]
  // ["1";"1";"1";"1"]
  // ["1";"2";"3";"4"]
  // ["9";"1";"2";"1";"2";"1";"2";"9"]
  |> List.collect explode
  |> calculateValue

[<EntryPoint>]
let main argv =
    printfn "%A" calc
    0