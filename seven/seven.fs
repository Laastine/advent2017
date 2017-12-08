open System

let replace(input: string, stripchars: string) =
  Seq.fold (fun (str: string) chr -> str.Replace(chr, ' ').Trim()) input stripchars

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

let calcA(input: List<string*int*List<string>>) =
  input
        |> Seq.toList
        |> List.collect (fun e ->
                              let (fst, snd, trd) = e
                              fst::trd)
        |> Seq.countBy id
        |> Seq.sortBy (fun e -> (snd e))
        |> Seq.map (fun e -> (fst e))
        |> Seq.head

[<EntryPoint>]
let main argv =
  printfn "%A" (calcA(inputFileToList "./seven.txt"))
  0
