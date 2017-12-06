open System

let inputFileToList(fileName: string): int[] =
  System.IO.File.ReadAllLines(fileName)
    |> Seq.toList
    |> List.map int
    |> Array.ofList

let isOutOfBounds(idx: int, arr: int[]): bool = idx < 0 || idx >= arr.Length

let calcA(inputList: int[]): int =
  let rec recur(input: int[], currIdx: int, acc: int): int =
    let newIdx = if input.[currIdx] <> 0 then input.[currIdx] + currIdx
                  else currIdx
    let newInputList = input
                        |> Array.mapi (fun i x ->
                              if i = currIdx then x+1
                              else x)
    if isOutOfBounds(newIdx, input) then acc+1
    else
      recur(newInputList, newIdx, acc + 1)
  recur(inputList, 0, 0)

let calcB(inputList: int[]): int =
  let rec recur(input: int[], currIdx: int, acc: int): int =
    let newIdx = if input.[currIdx] <> 0 then input.[currIdx] + currIdx
                  else currIdx

    let newInputList = input
                        |> Array.mapi (fun i x ->
                              if i = currIdx && x > 2 then x-1
                              elif i = currIdx then x+1
                              else x)
    if isOutOfBounds(newIdx, input) then acc+1
    else recur(newInputList, newIdx, acc + 1)
  recur(inputList, 0, 0)

[<EntryPoint>]
let main argv =
    printfn "%A " (calcA(inputFileToList "./input.txt"))
    printfn "%A " (calcB(inputFileToList "./input.txt"))
    0
