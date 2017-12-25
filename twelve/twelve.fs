open System

type Leaf = {
  node: int;
  leafs: int[]
}

let parseLine(input: string): Leaf =
  let array = input.Trim().Split([|"<->"|], StringSplitOptions.None)
  let values = array.[1].Split([|", "|], StringSplitOptions.None) |> Array.map int
  {node = (int array.[0]); leafs = values}

let inputFileToList(fileName: string) =
  System.IO.File.ReadLines(fileName)
    |> Seq.toList
    |> List.map parseLine

let findRoutes(inputList: List<Leaf>): int[] =
  let zero: int[] = inputList |> List.filter (fun e -> e.node = 0) |> List.map (fun e -> e.leafs) |> Array.concat
  let rec recur(input: List<Leaf>, acc: int[]): int[] =
    let newAcc = input
                    |> List.filter (fun e -> Array.exists ((=) (e.node)) acc)
                    |> List.map (fun e -> e.leafs)
                    |> List.reduce (Array.append)
                    |> Array.append acc
                    |> Array.distinct
    let newList = input |> List.filter (fun e -> Array.exists ((<>) (e.node)) newAcc)
    if acc.Length = newAcc.Length then acc
    else recur(newList, newAcc)
  recur(inputList, zero)

let calcA(input: int[]): int =
  input |> Array.length

[<EntryPoint>]
let main argv =
    printfn "%A" (inputFileToList "./twelve.txt" |> findRoutes |> calcA)
    0
