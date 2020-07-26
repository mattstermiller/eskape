open System

type MazeConfig = {
    Width: int
    Height: int
}
with
    member this.RowWalls = this.Width*2 + 1
    member this.TotalWalls = this.RowWalls*this.Height + this.Width

let render (maze: MazeConfig) walls =
    walls
    |> Array.mapi (fun i isWall ->
        let wall = '█'
        let cell = if isWall then wall else ' '
        let i = i % maze.RowWalls
        let isVertical = i < maze.Width
        let isEol = i = maze.Width - 1 || i = maze.RowWalls - 1
        [|
            if isVertical then
                wall
                cell
                if isEol then
                    wall
                    '\n'
            else
                cell
                if isEol then
                    '\n'
                else
                    ' '
        |]
    )
    |> Array.collect id
    |> String |> string

[<EntryPoint>]
let main args =
    let maze = {
        Width = 40
        Height = 14
    }
    let walls = Array.init maze.TotalWalls (fun _ -> true)
    let screen = render maze walls
    Console.Write screen
    Console.ReadKey() |> ignore
    0
