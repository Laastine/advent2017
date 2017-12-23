module Ten

open System

let calcIndex(idx: int, len: int): int = int (System.Math.Round((((float idx) / (float len)) % 1.0) * (float len)))

let explode(s: string): char[] = [ for c in s -> c ] |> List.toArray

let constructListByIndex(subSeries: int[], originalList: int[]): int[] =
  let revList = subSeries |> Array.rev
  let indexes = subSeries |> Array.map (fun e -> (originalList |> Array.findIndex ((=) e)))
  let newList = Array.zip indexes revList
  Array.mapi (fun idx x ->
    if (Array.exists (fun e -> e = idx) indexes) then
      newList |> Array.find (fun e -> (idx = (fst e))) |> snd
    else x) originalList

let constructSublist(startPoint: int, endPoint: int, list: int[]): int[] =
  [ startPoint..endPoint ] 
    |> List.map (fun e -> list.[calcIndex(e, list.Length)]) 
    |> List.toArray

let hash(series: int[], inputList: int[], index: int, skipSize: int): int[] * int * int =
  let rec recur(currIdx: int, skipSize: int, acc: int[]): int[] * int * int =
    if skipSize = series.Length then (acc, index, skipSize)
    else
      let index = calcIndex(currIdx, acc.Length)
      let windowSize = series.[skipSize]
      let subList = constructSublist(index, index + windowSize - 1, acc)
      let newAcc = constructListByIndex(subList, acc)
      recur(currIdx + subList.Length + skipSize, skipSize+1, newAcc)
  recur(index, skipSize, inputList)
    
let reverseSublist(nums: int[], start: int, endPoint: int) = 
  let digits = Array.copy nums
  for i in [0..endPoint-1] do
    nums.[(start + i) % nums.Length] <- digits.[(start + endPoint - i - 1) % nums.Length] 

let mutableHash (times: int) (input: seq<int>) =
  let series = [|0..255|]
  let mutable index = 0
  let mutable skipSize = 0
  for _ in [1..times] do
    for digit in input do
      reverseSublist(series, index, digit)
      index <- index + digit + skipSize
      skipSize <- skipSize + 1 
  series

let inputFileToListA(fileName: string): int[] =
  System.IO.File.ReadAllText(fileName).Trim().Split([|','|]) |> Array.map int

let inputFileToListB(fileName: string): string = System.IO.File.ReadAllText(fileName).Trim()

let calcB(input: string) =
  let suffix = [|17; 31; 73; 47; 23|] |> Array.map byte
  
  input.ToCharArray()
    |> Array.map (fun e -> Convert.ToByte(e))
    |> (fun e -> Array.append e suffix)
    |> Array.map int
    |> mutableHash 64
    |> Array.chunkBySize 16
    |> Array.map (Array.reduce (^^^))
    |> Array.fold (fun acc digit -> acc + sprintf "%02x" digit) ""    

let calcA(series: int[], inputList: int[]): int =
  let (list, _, _) = hash(series, inputList, 0, 0)
  list 
    |> Array.take 2 
    |> Array.reduce (*)

[<EntryPoint>]
let main argv =
  printfn "%A" (calcA(inputFileToListA "./ten.txt", [|0..255|]))
  printfn "%A" (calcB(inputFileToListB "./ten.txt"))
  0
