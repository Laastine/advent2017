open System

let nth = 603
let range = [(nth-1)..(-1)..0]

let calcNeighbours(x: int, y: int, arr: int[,]): int =
  let getN(a: int, b: int): int =
    if a >= 0 && a < Array2D.length1 arr && b >= 0 && b < Array2D.length2 arr then arr.[a,b]
    else 0
  (getN(x-1,y+1)) + (getN(x,y+1)) + (getN(x+1,y+1)) + (getN(x-1,y)) + (getN(x+1,y)) + (getN(x-1,y-1)) + (getN(x,y-1)) + (getN(x+1,y-1))

let squared(x: int): int = x * x

let calcIndex(idx: int, len: int): int =
  int (System.Math.Round((((float idx) / (float len)) % 1.0) * (float len)))

let removeFirstIfFirstRound list =  list |> List.tail

let Spiral n =
  let square = Array2D.create n n 0
  let nCur = ref (n*n)
  let nextNumber() = nCur := (!nCur-1) ; !nCur
  let Frame num =
    let rangeF = [(n - num - 2)..(-1)..num]
    let rangeB = [(num + 1)..(n - num - 1)]
    rangeF |> Seq.iter (fun i -> square.[n-num-1,i] <- nextNumber()) //bottom
    rangeF |> Seq.iter (fun i -> square.[i,num] <- nextNumber())     //left
    rangeB |> Seq.iter (fun i -> square.[num,i] <- nextNumber())     //top
    rangeB |> Seq.iter (fun i -> square.[i,n-num-1] <- nextNumber()) //right
  [0..(n/2 - 1)] |> Seq.iter Frame
  let rangeC = [n/2..n-1]
  let rangeCval = List.append [0] [3..(+2)..n]
  rangeC |> Seq.iteri (fun idx x -> square.[x,x] <- (rangeCval.[idx]) * (rangeCval.[idx]))
  square

let SpiralB(n: int) =
  let square = Array2D.create n n 0
  let nCur = ref 0
  let nextNumber() = nCur := (!nCur+1) ; !nCur
  let rangeC = [n/2..n-1]
  square.[n/2, n/2] <- 1
  let Frame num =
    let rangeF = [num..(n - num - 2)]
    let rangeB = [(n - num - 1)..(-1)..(num + 1)]
    rangeB |> removeFirstIfFirstRound
            |> Seq.iter (fun i -> square.[i,n-num-1] <- calcNeighbours(i,n-num-1, square))  //right
    rangeB |> Seq.iter (fun i -> square.[num,i] <- calcNeighbours(num,i, square))          //top
    rangeF |> Seq.iter (fun i -> square.[i,num] <- calcNeighbours(i,num, square))          //left
    rangeF |> Seq.iter (fun i -> square.[n-num-1,i] <- calcNeighbours(n-num-1,i, square))  //bottom
    let idx = calcIndex(Math.Abs(num-n/2), n/2)
    let i = rangeC.[idx]
    square.[i, i] <- calcNeighbours(i, i, square)        // corner

  [(n/2 - 1)..(-1)..0] |> Seq.iter Frame
  square

let spiral = Spiral nth

for x in range do
  for y in range do
    if spiral.[x,y] = 361527 then printfn "found: %A,%A => %A" x y (Math.Abs((nth-1)/2-x) + Math.Abs((nth-1)/2-y))

[<EntryPoint>]
let main argv =
  printfn "%A" (SpiralB 9 )
  0 // return an integer exit code


