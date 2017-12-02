module two

open System
open System.Data

let inputFileToList(fileName: string): List<string> =
  System.IO.File.ReadLines(fileName) |> Seq.toList

let calcIndex(idx: int, len: int): int =
  int (System.Math.Round((((float idx) / (float len)) % 1.0) * (float len)))

let calcA =
  inputFileToList "./input.txt"
    |> List.map (fun x -> x.Split('\t') |> Seq.map System.Int32.Parse |> Seq.toList)
    |> List.map (fun x -> (x |> List.max, x |> List.min))
    |> List.sumBy (fun x -> fst x - snd x)

let isDivisible(x: int, y: int): int*int =
  if (((float x) / (float y)) % 1.0) = 0.0 then (x, y)
  else (0,0)

let findDivisable(input: List<int>): int*int =
  let rec recur(list: List<int>, acc: int*int): int*int =
    match list with
    | [] -> acc
    | head::tail ->
          if tail.Length = 0 then recur(tail, acc)
          else
                let foo = tail
                            |> List.map (fun tailHead ->
                                    if head > 0 && tailHead > head then isDivisible(tailHead, head)
                                    elif tailHead > 0 && tailHead < head then isDivisible(head, tailHead)
                                    else (0,0))
                            |> List.filter (fun x -> (fst x) > 0 && (snd x) > 0)
                if foo.IsEmpty then recur(tail, acc) else recur(tail, foo.Head)
  recur(input, (0,0))

let calcB =
  inputFileToList "./input.txt"
    |> List.map (fun x -> x.Split('\t') |> Seq.map System.Int32.Parse |> Seq.toList)
    |> List.map findDivisable
    |> List.sumBy (fun x -> fst x / snd x)

[<EntryPoint>]
let main argv =
  printfn "%A" calcB
  0
