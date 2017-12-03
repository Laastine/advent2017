open System

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

// let indexes = Spiral 603 |>

[<EntryPoint>]
let main argv =
  printfn "%A" (Spiral 9)
  0 // return an integer exit code
