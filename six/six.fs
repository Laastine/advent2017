open System

let inputFileToList(fileName: string): List<int> =
  System.IO.File.ReadAllText(fileName).Split('\t')
    |> Array.map int
    |> List.ofArray

let areArraysEqual(a: List<int>, b: List<int>) = a.Length = b.Length && List.forall2 (=) a b

let calcIndex(idx: int, len: int): int =
  int (System.Math.Round((((float idx) / (float len)) % 1.0) * (float len)))

let distributeRemainder(remainder: int, currIdx: int, list: List<int>): List<int> =
                          let remainders: List<int*int> = [0..remainder-1]
                                                        |> List.map (fun x -> calcIndex(x+currIdx+1, list.Length))
                                                        |> Seq.countBy id
                                                        |> Seq.sortBy (fun x -> (fst x))
                                                        |> Seq.toList
                          List.mapi (fun i x ->
                                                let remList = remainders |> List.tryFind (fun e-> (fst e) = i)
                                                let (idx, v) = if remList.IsSome then remList.Value else (i, 0)
                                                if idx = i then x+v else x) list

let distribtuteVal(value: int, currIdx: int, list: List<int>): List<int> =
  let vals: List<int*int> = [0..list.Length-1]
                              |> List.map (fun x -> calcIndex(x+currIdx+1, list.Length))
                              |> Seq.countBy id
                              |> Seq.sortBy (fun x -> (fst x))
                              |> Seq.toList
  List.mapi (fun i x ->
                      let valList = vals |> List.tryFind (fun e -> (fst e) = i)
                      let (idx, v) = if valList.IsSome then valList.Value else (i, 0)
                      if idx = i then x+v else x) list

let calcDistribution(currIdx: int, input: List<int>): List<int> =
  let listLen = input.Length
  let currVal = input.[currIdx]
  let (value, remainder) = (currVal / listLen, currVal % listLen)
  let zeroed = input |> List.mapi (fun i x -> if i = currIdx then 0 else x)
  if value > 0 then
                  let vals = distribtuteVal(value, currIdx, zeroed)
                  if remainder > 0 then distributeRemainder(remainder, currIdx, vals) else vals
  elif remainder > 0 && value = 0 then distributeRemainder(remainder, currIdx, zeroed)
  else input

let calcA(inputList: List<int>): int =
  let maxValIdx(list: List<int>): int = list |> List.findIndex ((=)(List.max list))
  let rec recur(input: List<int>, currIdx: int, acc: List<List<int>>) =
    // printfn "\n%A\n" (input |> List.mapi (fun i x -> if i = currIdx then (sprintf "(%A)" x) else (sprintf "%A" x)))
    let newInput = calcDistribution(currIdx, input)
    let newAcc = newInput::acc
    if (acc |> List.tryFind (fun x -> areArraysEqual(newInput, x))).IsSome then newAcc.Length
    else recur(newInput, maxValIdx(newInput), newAcc)
  recur(inputList, maxValIdx(inputList), [])

let calcB(inputList: List<int>): int =
  let maxValIdx(list: List<int>): int = list |> List.findIndex ((=)(List.max list))
  let rec recur(input: List<int>, currIdx: int, acc: List<List<int>>) =
    // printfn "\n%A\n" (input |> List.mapi (fun i x -> if i = currIdx then (sprintf "(%A)" x) else (sprintf "%A" x)))
    let newInput = calcDistribution(currIdx, input)
    let newAcc = newInput::acc
    if (acc |> List.tryFind (fun x -> areArraysEqual(newInput, x))).IsSome then
      ((acc |> List.findIndex (fun e -> areArraysEqual(e, newInput))) + 1)
    else recur(newInput, maxValIdx(newInput), newAcc)
  recur(inputList, maxValIdx(inputList), [])

[<EntryPoint>]
let main argv =
    printfn "%A" (calcB(inputFileToList "./input.txt"))
    0
