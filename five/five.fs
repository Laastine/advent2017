open System

let inputFileToList(fileName: string): List<int> =
  System.IO.File.ReadAllLines(fileName)
    |> Seq.toList
    |> List.map int

let isOutOfBounds(idx: int, arr: List<int>): bool = idx < 0 || idx >= arr.Length

let calc(inputList: List<int>): int =
  let rec recur(input: List<int>, currIdx: int, acc: int): int =
    let newIdx = if input.[currIdx] <> 0 then input.[currIdx] + currIdx
                  else currIdx
    if isOutOfBounds(newIdx, input) then acc+1
    else
      let newInputList = input
                          |> List.mapi (fun i x ->
                                if i = currIdx then x+1
                                else x)
      recur(newInputList, newIdx, acc + 1)
  recur(inputList, 0, 0)

[<EntryPoint>]
let main argv =
    printfn "%A " (calc(inputFileToList "./input.txt"))
    0
