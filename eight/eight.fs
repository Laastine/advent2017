open System


type Element = string*string*string*string*string*string

let parseLine(input: string) =
  let arr = input.Split([|"if "|], StringSplitOptions.None)
  let firstParts = arr.[0].Split([|' '|])
  let lastPart = arr.[1].Split([|' '|])
  let (variable, operation, value) = (firstParts.[0], firstParts.[1], firstParts.[2])
  let (varA, sign, varB) = (lastPart.[0], lastPart.[1], lastPart.[2])
  (variable, operation, value, varA, sign, varB)

let evaluateExpression(varA: int, varB: int, sign: string): bool =
  match sign with
  | "==" -> varA = varB
  | "!=" -> varA <> varB
  | ">" -> varA > varB
  | "<" -> varA < varB
  | ">=" -> varA >= varB
  | "<=" -> varA <= varB
  | _ -> failwith "Didn't recognize sign"

let evaluateOperation(variable: int, value: int, operation: string): int =
  if operation = "inc" then variable + value
  elif operation = "dec" then variable - value
  else failwith "Didn't recognize operation"

let getVariable(el: Element): string =
  let (variable, _, _, _, _, _) = el
  variable

let isVariableInList(variable: string, list: List<Element>): bool =
  list |> List.exists (fun x ->
                                let listVariable = getVariable(x)
                                variable = listVariable)

let insertVariableToList(inputList: List<Element>): List<List<Element>> =
  let rec recur(list: List<Element>, acc) =
    match list with
    | [] -> acc
    | head::tail ->
                    let headVariable = getVariable(head)
                    let newAcc =
                            if acc |> List.exists (fun e -> isVariableInList(headVariable, e)) then
                                acc |> List.map (fun x ->
                                                    if x.IsEmpty then x
                                                    else
                                                      if getVariable(x.Head) = headVariable then head::x
                                                      else x)
                            else [head]::acc
                    recur(tail, newAcc)
  recur(inputList, [])

let inputFileToList(fileName: string) =
  System.IO.File.ReadLines(fileName)
    |> Seq.toList
    |> List.map parseLine
    |> insertVariableToList

[<EntryPoint>]
let main argv =
    printfn "%A" ((inputFileToList "./eight2.txt"))
    0


// b inc 5 if a > 1
// a inc 1 if b < 5
// c dec -10 if a >= 1
// c inc -20 if c == 10
