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

    /// Returns the room indexes separated by the given wall index 
    member this.Rooms wall =
        if this.IsOuterWall wall then
            None
        else
            let i = wall % this.RowWalls
            let row = wall / this.RowWalls
            if i < this.Width then
                // north/south
                let room = wall - (this.Width+1)*row - this.Width
                Some (room, room+this.Width)
            else
                // east/west
                let room = wall - (this.Width+1)*(row+1)
                Some (room, room+1)

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
    let wallsToRemove = [|0..maze.TotalWalls-1|] |> Array.filter (not << maze.IsOuterWall) |> shuffle
    let rooms = Array.init (maze.Width*maze.Height) (fun _ -> UnionFindNode())
    let render () =
        Console.SetCursorPosition(0, 0)
        render maze walls |> Console.Write
        System.Threading.Thread.Sleep(10)
    // remove inner walls
    for i in wallsToRemove do
        match maze.Rooms i with
        | Some (a, b) ->
            if rooms.[a].Union(rooms.[b]) then
                walls.[i] <- false
                render ()
        | None -> failwith "Tried to remove outer wall"
    // remove outer E/W walls
    walls.[maze.Width + rand.Next(maze.Height)*maze.RowWalls] <- false
    walls.[maze.Width*2 + rand.Next(maze.Height)*maze.RowWalls] <- false
    render ()
    Console.ReadKey() |> ignore
    0
