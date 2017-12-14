open System

type Element = string*string*string*string*string*string

let parseLine(input: string): Element =
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

let explode (s: string): List<char> = [for c in s -> c]

let removeMinusLetter(input: string): string =
  let firstLetter = explode(input) |> List.head |> String.Concat
  if firstLetter = "-" then explode(input) |> List.tail |> Array.ofList |> String.Concat
  else input

// For some reason "TryParse" won't work with negative numbers
let isNumber(el: string): bool =
  fst (System.Int32.TryParse(removeMinusLetter(el)))

let getValue(acc: List<string*int>, el: string): int =
  if isNumber(el) then (int el)
  else
    let someFound = acc |> List.tryFind (fun x -> el = (fst x))
    if someFound.IsSome then (snd someFound.Value) else 0


let setRegisterValue(acc: List<string*int>, el: string, value: int): List<string*int> =
  let someFound = acc |> List.tryFind (fun x -> el = (fst x))
  if someFound.IsSome then
    List.map (fun x -> if el = (fst x) then (el, value) else x) acc
  else (el, value)::acc

let isVariableInList(variable: string, list: List<string*int>): bool =
  List.exists (fun x -> variable = (fst x)) list

let insertVariableToList(inputList: List<Element>): List<string*int> =
  let rec recur(list: List<Element>, acc: List<string*int>) =
    match list with
    | [] -> acc
    | head::tail ->
          let (headVariable, operation, value, varA, sign, varB) = head
          let res = evaluateExpression(getValue(acc, varA), getValue(acc, varB), sign)
          let newAcc =
                  if res then
                    let newRegVal = evaluateOperation(getValue(acc, headVariable), (int value), operation)
                    setRegisterValue(acc, headVariable, newRegVal)
                  else acc
          recur(tail, newAcc)
  recur(inputList, [])

let inputFileToList(fileName: string) =
  System.IO.File.ReadLines(fileName)
    |> Seq.toList
    |> List.map parseLine


let calcA(inputList: List<Element>) =
  inputList
    |> insertVariableToList
    |> List.maxBy snd

[<EntryPoint>]
let main argv =
    printfn "%A" (calcA(inputFileToList "./eight.txt"))
    0
