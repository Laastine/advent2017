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

let calcScannerPos(tick: int, r: int): int =
  let range = r-1
  let index = tick % range
  let isMovingUp = isMovingUp(tick, range)
  if isMovingUp then Math.Abs(range-index)
  else index

let isSafe tick (depth: int, scannerPos: int): bool =
  (tick + depth) % scannerPos <> 0

let calcA(inputArray: Layer[]): int =
  inputArray
  |> Array.map (fun e ->
                  let pos = calcScannerPos(e.depth, e.range)
                  if pos = 0 then e.depth * e.range
                  else 0)
  |> Array.sum

let calcB(inputArray: Layer[]): int =
  let detectors = Array.map (fun e -> (e.depth, 2 * (e.range - 1))) inputArray

  let rec recur(x: int) =
    let ticks = Array.forall (isSafe x) detectors
    if ticks then x else recur(x+1)
  recur(0)

[<EntryPoint>]
let main argv =
  (inputFileToList "./thirteen.txt" |> calcA |> printfn "%A")
  (inputFileToList "./thirteen.txt" |> calcB |> printfn "%A")
  0
