module Eleven

open System

let updatePosition(movement: string): int*int*int =
  match movement with
    | "ne" -> 1,0,-1
    | "se" -> 1,-1,0
    | "sw" -> -1,0,1
    | "nw" -> -1,1,0
    | "n" -> 0,1,-1
    | "s" -> 0,-1,1
    | _ -> failwith "Unkown movement direction"

let sumTuples(a: int*int*int, b: int*int*int): int*int*int =
  let x1,y1,z1 = a
  let x2,y2,z2 = b
  (x1+x2, y1+y2, z1+z2)

let calcDistance(inputArray: string[]): int*int*int =
  let startPosition = 0,0,0
  let rec recur(input: List<string>, acc: int*int*int) =
    match input with
    | [] -> acc
    | head::tail ->
        let movement = updatePosition(head)
        let newAcc = sumTuples(acc, movement)
        recur(tail, newAcc)
  recur(inputArray |> Array.toList, startPosition)

let calcA(movement: int*int*int): int =
  let x,y,z = movement
  (Math.Abs x + Math.Abs y + Math.Abs z) / 2

let inputFileToListB (fileName : string) : string[] =
  System.IO.File.ReadAllText(fileName).Trim().Split([|','|])

[<EntryPoint>]
let main argv =
  printfn "%A" (inputFileToListB "./eleven.txt" |> calcDistance |> calcA)
  0
