// Learn more about F# at http://fsharp.org

open System
open System.Text

let cartesian xs ys = 
    xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))

type Cell =
| Live
| Dead

// type Transistion =
//     | Underpopulation
//     | Survive
//     | Overpopulation
//     | Reproduced

let (|Underpopulation|Survive|Overpopulation|Reproduce|RemainDead|) (neighbors : Cell list, cell : Cell)  =
    let liveCellsCount = neighbors |> List.filter((=)Live) |> List.length
    match cell with
    | Live when liveCellsCount < 2-> Underpopulation
    | Live when liveCellsCount = 2 || liveCellsCount = 3 -> Survive
    | Live -> Overpopulation
    | Dead when liveCellsCount = 3 -> Reproduce
    | Dead -> RemainDead

let tryGet x y (grid : Cell [,]) =
    try
        Some grid.[x,y]
    with e ->
        None

let getNeighbors x y (grid : Cell [,]) =
    let neighborPositions = 
        cartesian [-1; 0; 1] [-1; 0; 1]
        |> List.filter(fun t -> t <> (0,0))
    neighborPositions
    |> List.map(fun (nx, ny) -> (x + nx, y + ny) )
    |> List.choose(fun (x,y) -> tryGet x y grid)

let transition (neighbors : Cell list) (cell : Cell) =
    match (neighbors, cell) with
    | Underpopulation -> ()
    | _ -> failwithf "Unknown transition"

let step (current : Cell [,]) =
    let next = Array2D.init (Array2D.length1 current) (Array2D.length2 current) (fun x y -> Live)
    for x = 0 to (Array2D.length1 current - 1) do
        for y = 0 to (Array2D.length2 current - 1) do
            try
                let neighbors =  getNeighbors x y current 
                let cell = current.[x,y]
                let newCell =
                    match (neighbors, cell) with
                    | Underpopulation -> Dead
                    | Survive -> Live
                    | Overpopulation -> Dead
                    | Reproduce -> Live
                    | RemainDead -> Dead
                next.[x,y] <- newCell
            with e ->
                printfn "x : %i, y : %i" x y
                reraise()
            
    next

let printGrid iteration (grid : Cell [,]) = 
    let output = sprintf "\u001b[29DIteration %i" iteration
    // stdout.Write()
    let sb = StringBuilder()
    
    for x = 0 to (Array2D.length1 grid - 1) do
        for y = 0 to (Array2D.length2 grid - 1) do 
            match grid.[x,y] with
            | Live -> sb.Append "#" |> ignore
            | Dead -> sb.Append "." |> ignore
        sb.AppendLine() |> ignore
    stdout.Write(output + "\n" + sb.ToString())
    // stdout.Write "                             \u001b[29D"

[<EntryPoint>]
let main argv = 
    let initGrid = Array2D.init 10 10 (fun x y -> Live)
    let rec run (i : int) grid =
        Threading.Thread.Sleep(1000)
        printGrid i grid
        let nextGrid = step grid
        if i > 100 then
            ()
        else 
            run (i + 1) nextGrid
    run 0 initGrid

    0 // return an integer exit code
