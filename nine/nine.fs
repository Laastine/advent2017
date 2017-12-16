open System

let explode (s: string): List<char> = [for c in s -> c]

let inputFileToList(fileName: string) =
  System.IO.File.ReadAllText(fileName)
    |> explode

let calc(inputList: List<char>): int*int =
  let rec recur(input: List<char>, isGarbage: bool, garbage: int, depth: int, acc: int): int*int =
    match input with
    | [] -> garbage,acc
    | head::tail ->
                    if head = '!' then recur(tail.Tail, isGarbage, garbage, depth, acc)
                    elif not isGarbage && head = '{' then recur(tail, isGarbage, garbage, depth+1, acc+depth+1)
                    elif not isGarbage && head = '}' then recur(tail, isGarbage, garbage, depth-1, acc)
                    elif isGarbage && head <> '>' then recur(tail, isGarbage, garbage+1, depth, acc)
                    elif not isGarbage &&  head = '<' then recur(tail, true, garbage, depth, acc)
                    elif isGarbage && head = '>' then recur(tail, false, garbage, depth, acc)
                    else recur(tail, isGarbage, garbage, depth, acc)
  recur(inputList, false, 0, 0, 0)

[<EntryPoint>]
let main argv =
    printfn "%A" (inputFileToList("./nine.txt") |> calc)
    0
