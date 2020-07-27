open System

let rand = Random()

let shuffle items =
    let arr = items |> Seq.toArray
    for i in [0..arr.Length-1] do
        let pick = i + rand.Next(arr.Length-i)
        let swap = arr.[pick]
        arr.[pick] <- arr.[i]
        arr.[i] <- swap
    arr

type UnionFindNode() =
    member val private Parent: UnionFindNode option = None with get, set
    member val private Rank = 0 with get, set

    member this.Find () = this.Parent |> Option.map (fun n -> n.Find()) |> Option.defaultValue this

    member this.Union (other: UnionFindNode) =
        let root1 = this.Find()
        let root2 = other.Find()
        if root1 = root2 then
            false
        else
            if root1.Rank < root2.Rank then
                root1.Parent <- Some root2
            else
                root2.Parent <- Some root1
                if root1.Rank = root2.Rank then
                    root1.Rank <- root1.Rank + 1
            true

type Direction = Up | Down | Left | Right

type MazeConfig = {
    Width: int
    Height: int
}
with
    member this.RowWalls = this.Width*2 + 1
    member this.TotalWalls = this.RowWalls*this.Height + this.Width

    /// Returns whether the given wall index is an outer wall
    member this.IsOuterWall wall =
        wall < this.Width || wall >= this.RowWalls*this.Height ||
        wall % this.RowWalls = this.Width || wall % this.RowWalls = this.RowWalls - 1

    /// Returns the room indexes next to the given wall index 
    member this.Rooms wall =
        let i = wall % this.RowWalls
        let row = wall / this.RowWalls
        if i < this.Width then
            // north/south
            let room = wall - (this.Width+1)*row - this.Width
            [
                if row > 0 then
                    room
                if row < this.Height then
                    room+this.Width
            ]
        else
            // east/west
            let room = wall - (this.Width+1)*(row+1)
            [
                if i > this.Width then
                    room
                if i < this.RowWalls - 1 then
                    room+1
            ]

    /// Returns a new room index given the walls, a movement direction, and a current room index.
    /// Returns the same room when the given movement is not valid.
    member this.Move (walls: bool array) dir room =
        let row = room / this.Width
        let i = room % this.Width
        let offset =
            match dir with
            | Up -> 0
            | Down -> this.RowWalls
            | Left -> this.Width
            | Right -> this.Width + 1
        let wall = walls.[row*this.RowWalls + i + offset]
        let move =
            if wall || dir = Left && i = 0 then 0
            else
                match dir with
                | Right -> 1
                | Left -> -1
                | Down -> this.Width
                | Up -> -this.Width
        room + move

let render (maze: MazeConfig) walls player =
    walls
    |> Array.mapi (fun i isWall ->
        let wall = '█'
        let cell = if isWall then wall else ' '
        let rowI = i % maze.RowWalls
        let isVertical = rowI < maze.Width
        let isEol = rowI = maze.Width - 1 || rowI = maze.RowWalls - 1
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
                    let room = maze.Rooms i |> List.last
                    if room = player then '*' else ' '
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
    let wallsToRemove = [|0..maze.TotalWalls-1|] |> Array.filter (not << maze.IsOuterWall) |> shuffle
    let rooms = Array.init (maze.Width*maze.Height) (fun _ -> UnionFindNode())
    // remove inner walls
    for i in wallsToRemove do
        match maze.Rooms i with
        | [a; b] ->
            if rooms.[a].Union(rooms.[b]) then
                walls.[i] <- false
        | _ -> failwith "Tried to remove outer wall"
    // remove outer E/W walls
    let startWall = maze.Width + rand.Next(maze.Height)*maze.RowWalls
    let finishWall = maze.Width*2 + rand.Next(maze.Height)*maze.RowWalls
    walls.[startWall] <- false
    walls.[finishWall] <- false
    let start = maze.Rooms startWall |> List.exactlyOne
    let finish = maze.Rooms finishWall |> List.exactlyOne
    let mutable player = start
    let mutable won = false

    let eraseCursor () =
        Console.CursorLeft <- Console.CursorLeft - 1
        Console.Write " "
        Console.CursorLeft <- Console.CursorLeft - 1
    let render () =
        Console.SetCursorPosition(0, 0)
        render maze walls player |> Console.Write
        let quitMsg = "Press [Escape] to quit."
        Console.Write (String(' ', 80))
        Console.CursorLeft <- 0
        Console.Write (
            if won then "You Won!!!  " + quitMsg
            else "Use the arrow keys or H J K L to move to the exit.  " + quitMsg
        )
    let move dir =
        player <- maze.Move walls dir player
        if player = finish then
            won <- true
        render ()

    Console.CursorVisible <- false
    render ()
    let mutable run = true
    while run do
        match Console.ReadKey().Key with
        | ConsoleKey.Escape -> run <- false
        | _ when won -> eraseCursor ()
        | ConsoleKey.LeftArrow | ConsoleKey.H -> move Left
        | ConsoleKey.DownArrow | ConsoleKey.J -> move Down
        | ConsoleKey.UpArrow | ConsoleKey.K -> move Up
        | ConsoleKey.RightArrow | ConsoleKey.L -> move Right
        | _ -> eraseCursor ()
    0
