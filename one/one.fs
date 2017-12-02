module One

open System

let explode (s: string): List<int> = [for c in s -> c |> string |> int]

let inputFileToList(fileName: string): List<string> =
  System.IO.File.ReadLines(fileName)
  |> Seq.toList


let calcIndex(idx: int, len: int): int =
  int (System.Math.Round((((float idx) / (float len)) % 1.0) * (float len)))

let calculateValueA(inputList: List<int>): int =
  let first = inputList.Head
  let rec recur(list: List<int>, acc: int) =
    match list with
    | [] -> acc
    | head::tail ->
        if list.Length = 1 && first = head then recur(tail, acc+first)
        elif list.Length > 1 && head = tail.Head then recur(tail, acc+head)
        else recur(tail, acc)
  recur(inputList, 0)

let calculateValueB(inputList: List<int>): int =
  let div = inputList.Length / 2
  let rec recur(list: List<int>, acc: int) =
    match list with
    | [] -> acc
    | head::tail ->
          let index = inputList.Length - tail.Length - 1
          let idx =
            if tail.Length = 0 then
              div - 1
            else calcIndex(index+div, inputList.Length)
          printfn "IDX=%A, idx+%A=%A | head:%A = val:%A => %A" index div idx head inputList.[idx] acc
          if head = inputList.[idx] then
            // printfn "A: %A = %A" head inputList.[idx]
            recur(tail, acc+head)
          else recur(tail, acc)
  recur(inputList, 0)

let calc: int =
  inputFileToList "./input.txt"
  // ["1";"2";"3";"1";"2";"3"]
  // ["1";"2";"1";"2"]
  // ["1";"1";"1";"1"]
  // ["1";"2";"3";"4"]
  // ["1";"2";"1";"3";"1";"4";"1";"5";"1";"1"]
    |> List.collect explode
    |> calculateValueB
//986-967=19
[<EntryPoint>]
let main argv =

    // assert (["1";"2";"2";"1"] |> List.collect explode |> calculateValueB = 0)
    // assert (["1";"2";"2";"4";"2";"5"] |> List.collect explode |> calculateValueB = 4)
    // assert (["1";"2";"3";"1";"2";"3"] |> List.collect explode |> calculateValueB = 12)
    // assert (["1";"2";"1";"3";"1";"4";"1";"5"] |> List.collect explode |> calculateValueB = 4)


    printfn "%A" calc
    0