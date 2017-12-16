open System

let explode (s: string): List<char> = [for c in s -> c]

let inputFileToList(fileName: string) =
  System.IO.File.ReadAllText(fileName)
    |> explode

let calcA(inputList: List<char>): int =
  let rec recur(input: List<char>, isGarbage: bool, depth: int, acc: int): int =
    match input with
    | [] -> acc
    | head::tail ->
                    // printfn "acc %A" acc
                    if head = '!' then recur(tail.Tail, isGarbage, depth, acc)
                    elif not isGarbage && head = '{' then recur(tail, isGarbage, depth+1, acc+depth+1)
                    elif not isGarbage && head = '}' then recur(tail, isGarbage, depth-1, acc)
                    elif isGarbage && head <> '>' then recur(tail, isGarbage, depth, acc)
                    elif not isGarbage &&  head = '<' then recur(tail, true, depth, acc)
                    elif isGarbage && head = '>' then recur(tail, false, depth, acc)
                    else recur(tail, isGarbage, depth, acc)
  recur(inputList, false, 0, 0)

[<EntryPoint>]
let main argv =
    printfn "%A" (inputFileToList("./nine.txt") |> calcA)
    0
