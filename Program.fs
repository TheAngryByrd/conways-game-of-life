// Learn more about F# at http://fsharp.org

open System
open System.Text

let cartesian xs ys = 
    xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))

type Cell =
| Live
| Dead

let (|Underpopulation|Survive|Overpopulation|Reproduce|RemainDead|) (neighbors : Cell list, cell : Cell)  =
    let liveCellsCount = neighbors |> List.filter((=)Live) |> List.length
    match cell with
    | Live when liveCellsCount < 2 -> Underpopulation
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
    current 
    |> Array2D.iteri(
        fun x y cell ->
            let neighbors =  getNeighbors x y current 
            let newCell =
                match (neighbors, cell) with
                | Underpopulation -> Dead
                | Survive -> Live
                | Overpopulation -> Dead
                | Reproduce -> Live
                | RemainDead -> Dead
            next.[x,y] <- newCell
    )
    next


let printGrid iteration (grid : Cell [,]) = 
    let output = sprintf "Iteration %i" iteration
    let sb = StringBuilder()
    sb.AppendLine(output) |> ignore

    for x = 0 to (Array2D.length1 grid - 1) do
        for y = 0 to (Array2D.length2 grid - 1) do 
            match grid.[x,y] with
            | Live -> sb.Append "#" |> ignore
            | Dead -> sb.Append "." |> ignore
        sb.AppendLine() |> ignore

    stdout.Write(sb.ToString())


let readGrid (inputFile : IO.FileInfo) =
    let input = inputFile.FullName |> IO.File.ReadAllText
    let lines = input.Split("\n",StringSplitOptions.RemoveEmptyEntries)
    lines
    |> Seq.map(fun l ->
        l 
        |> Seq.choose(fun s ->
            match s with
            | '#' -> Some Live
            | '.' -> Some Dead
            | s -> None
        )
    )
    |> array2D

let exampleGrids = IO.Path.Combine(__SOURCE_DIRECTORY__, "grids" )

let readGridFile gridFile = 
    IO.Path.Combine(exampleGrids, gridFile)
    |> IO.FileInfo

let grid1 = 
    readGridFile "grid1.txt"
    |> readGrid 


let grid2 = 
    readGridFile "grid2.txt"
    |> readGrid 

let random = Random()

let randomGrid = 
    Array2D.init 60 280 (fun x y -> 
        if random.Next(0,2) % 2 = 0 then
            Live
        else 
            Dead
        )

[<EntryPoint>]
let main argv = 
    // let initGrid = Array2D.init 10 10 (fun x y -> Live)
    let rec run (i : int) grid =
        Threading.Thread.Sleep(1000)
        printGrid i grid
        let nextGrid = step grid
        if i > 1000 then
            ()
        else 
            run (i + 1) nextGrid
    run 0 randomGrid

    0 // return an integer exit code
