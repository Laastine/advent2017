open System

let calcIndex(idx: int, len: int): int =
  int (System.Math.Round((((float idx) / (float len)) % 1.0) * (float len)))

let constructListByIndex(subSeries: List<int>, originalList: List<int>): List<int> =
  let revList = subSeries |> List.rev
  let indexes = subSeries |> List.map (fun e -> (originalList |> List.findIndex ((=) e)))
  let newList = List.zip indexes revList
  List.mapi (fun idx x ->
            if (List.exists (fun e -> e = idx) indexes) then
                newList |> List.find (fun e -> (idx = (fst e))) |> snd
            else x) originalList

let constructSublist(startPoint: int, endPoint: int, list: List<int>): List<int> =
  [startPoint..endPoint] |> List.map (fun e -> list.[calcIndex(e, list.Length)])

let calcA(series: List<int>, inputList: List<int>): List<int> =
  let rec recur(currIdx: int, skipSize: int, acc: List<int>): List<int> =
    if skipSize = series.Length then acc
    else
      let index = calcIndex(currIdx, inputList.Length)
      let windowSize = series.[skipSize]
      let subList = constructSublist(index, index+windowSize-1, acc)
      let newAcc = constructListByIndex(subList, acc)
      recur(currIdx + subList.Length + skipSize, skipSize + 1, newAcc)
  recur(0, 0, inputList)

let inputFileToList(fileName: string): List<int> =
  System.IO.File.ReadAllText(fileName).Split([|','|])
    |> Array.toList
    |> List.map int

[<EntryPoint>]
let main argv =
  let inputList = inputFileToList "./ten.txt"
  let series = [0..255]
  let res = calcA(inputList, series)
  printfn "%A" (res.[0] * res.[1])
  0
