open System

type Layer = {
  depth: int;
  range: int
}

let parseLine(input: string): Layer =
  let array = input.Trim().Split([|':'|])
  {depth = (int array.[0]); range = (int array.[1])}

let inputFileToList(fileName: string): Layer[] =
  System.IO.File.ReadLines(fileName)
    |> Seq.toArray
    |> Array.map parseLine

let isMovingUp(tick: int, range: int): bool =
  (tick/range) % 2 <> 0

let calcScannerPos(tick: int, r: int): int*int =
  let range = r-1
  let index = tick % range
  let isMovingUp = isMovingUp(tick, range)
  if isMovingUp then
    (Math.Abs(range-index),range)
  else (index, range)

let calcA(inputArray: Layer[]): int =
  inputArray
  |> Array.map (fun e ->
                  let pos = fst (calcScannerPos(e.depth, e.range))
                  let fatality = e.depth * e.range
                  if pos = 0 then
                      fatality
                  else 0)
  |> Array.sum

[<EntryPoint>]
let main argv =
  (inputFileToList "./thirteen.txt" |> calcA |> printfn "%A")
  0
