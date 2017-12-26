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

let findRoutes(inputList: List<Leaf>, nodeToFind: int): int[] =
  let zero: int[] = inputList |> List.filter (fun e -> e.node = nodeToFind)
                              |> List.map (fun e -> e.leafs)
                              |> Array.concat
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

let findGroups(inputList: List<Leaf>): int[] =
  let rec recur(nodeToInspect: int, subArray: int[], acc: int[]): int[] =
    let newInput = inputList |> List.filter (fun e -> not (Array.exists ((=) e.node) subArray))
    if newInput.IsEmpty then acc
    else
      let newNode = newInput.Head
      let newNodes = Array.append acc [|newNode.node|]
      let newSubArray = findRoutes(inputList, newNode.node) |> Array.append subArray |> Array.distinct
      recur(newNode.node, newSubArray, newNodes)
  recur(0, [||], [||])

let calcA(input: int[]): int =
  input |> Array.length

let calcB(input: List<Leaf>) = findGroups(input) |> Array.length

[<EntryPoint>]
let main argv =
    printfn "%A" (inputFileToList "./twelve.txt" |> (fun e -> findRoutes(e, 0)) |> calcA)
    printfn "%A" (inputFileToList "./twelve.txt" |> calcB)
    0
