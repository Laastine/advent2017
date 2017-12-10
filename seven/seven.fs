open System

let replace(input: string, stripchars: string) =
  Seq.fold (fun (str: string) chr -> str.Replace(chr, ' ').Trim()) input stripchars

let findLeaf(leaf: string, list: List<string*int*List<string>>): string*int*List<string> =
  let leaf = list |> List.tryFind (fun e ->
                          let (node, _, _) = e
                          leaf = node)
  if leaf.IsSome then leaf.Value else failwith "Didn't find leaf"


let calcBrancWeight(leaf, list: List<string*int*List<string>>): int =
  let (_, _, b) = findLeaf(leaf, list)
  b |> List.sumBy (fun e ->
                            let (_, vb, _) = (findLeaf(e, list))
                            vb)

let calcWeight(leafName: string, inputList: List<string*int*List<string>>): int =
  let rec recur(leaf: string*int*List<string>, depth: int, acc: int): int =
    let (_, _, branches) = leaf
    if branches.IsEmpty then acc
    else
      acc+(branches |> List.sumBy (fun e ->
                                      let fl = findLeaf(e, inputList)
                                      let (l,v,b) = fl
                                      let foo =  b
                                                    |> List.map (fun e -> calcBrancWeight(e, inputList))
                                                    |> Seq.groupBy id
                                      if b.IsEmpty then v else recur(fl, depth+1, v)))
  let lf = findLeaf(leafName, inputList)
  let (_, acc, _) = lf
  recur(lf, 1, acc)

let parseLine(input: string): string*int*List<string> =
  let arr = input.Split([|' '|])
  let node = arr |> Array.head
  let value = replace(arr.[1], "()") |> int
  let splitted = input.Split([|'>'|])
  let leaves =
    if splitted.Length > 1 then
      splitted.[1].Split([|','|])
        |> Array.map (fun e -> e.Trim())
        |> Array.toList
    else []
  (node,value,leaves)

let inputFileToList(fileName: string): List<string*int*List<string>> =
  System.IO.File.ReadLines(fileName)
    |> Seq.toList
    |> List.map parseLine

let calcA(input: List<string*int*List<string>>): string =
  input
        |> Seq.toList
        |> List.collect (fun e ->
                              let (fst, _, trd) = e
                              fst::trd)
        |> Seq.countBy id
        |> Seq.sortBy (fun e -> (snd e))
        |> Seq.map (fun e -> (fst e))
        |> Seq.head

let calcB(root: string, inputList: List<string*int*List<string>>): List<int> =
  let (_, _, branches) = findLeaf(root, inputList)
  branches |> List.map (fun e -> calcWeight(e, inputList))

[<EntryPoint>]
let main argv =
  let inputList = inputFileToList("./seven.txt")
  let root = calcA(inputList)
  let (_,_,rootBranches) = findLeaf(root, inputList)
  let inBalanced = rootBranches
                                |> List.map (fun e ->
                                                        let (s, _, br) = findLeaf(e, inputList)
                                                        (s, br |> List.map (fun b -> (calcB(b, inputList)) |> List.distinct)))
                                |> List.filter (fun e -> (snd e) |> List.exists (fun x -> x.Length <> 1))
                                |> List.head
  printfn "inBalanced %A" (inBalanced) // 1381 1386
  printfn "roots2 %A" (findLeaf("gozhrsf", inputList))  //leaf 1386 is "gozhrsf" and its value is 762
  printfn "%A" (762-(1386-1381))
  0
