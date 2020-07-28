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

module Seq =
    let collecti f =
        let mutable i = 0
        Seq.collect (fun a ->
            let b = f i a
            i <- i + 1
            b
        )

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

type Room = {
    // Whether this room has a wall to the North (Up)
    N: bool
    // Whether this room has a wall to the West (Left)
    W: bool
}

type RoomWall = N | W

type Game = {
    Width: int
    Height: int
    Rooms: Room array
    Start: int
    Finish: int
    Player: int
}
with
    member this.Won = this.Player = this.Finish

let newGame width height =
    let rooms = Array.init (width*height) (fun _ -> { N = true; W = true })
    let wallsToRemove =
        [|
            for room in Seq.init (width*height) id do
                if room >= width then
                    (room, N)
                if room % width > 0 then
                    (room, W)
        |]
    let roomNodes = Array.init (width*height) (fun _ -> UnionFindNode())
    for (room, wall) in shuffle wallsToRemove do
        let otherRoom =
            match wall with
            | N -> room - width
            | W -> room - 1
        if roomNodes.[room].Union(roomNodes.[otherRoom]) then
            rooms.[room] <-
                match wall with
                | N -> { rooms.[room] with N = false }
                | W -> { rooms.[room] with W = false }
    let start = rand.Next(height)*width
    let finish = rand.Next(height)*width + width - 1
    {
        Width = width
        Height = height
        Rooms = rooms
        Start = start
        Finish = finish
        Player = start
    }

let move game dir =
    let row = game.Player / game.Width
    let col = game.Player % game.Width
    match dir with
    | Up ->
        if row > 0 && not game.Rooms.[game.Player].N then
            { game with Player = game.Player - game.Width }
        else game
    | Down ->
        if row < game.Height-1 && not game.Rooms.[game.Player+game.Width].N then
            { game with Player = game.Player + game.Width }
        else game
    | Left ->
        if col > 0 && not game.Rooms.[game.Player].W then
            { game with Player = game.Player - 1 }
        else game
    | Right ->
        if col < game.Width-1 && not game.Rooms.[game.Player+1].W then
            { game with Player = game.Player + 1 }
        else game

let render game =
    let wall = '█'
    let playerChar = '*'
    let space = ' '
    let toWall b = if b then wall else space
    let line roomRow f = roomRow |> Seq.collecti f |> Seq.toArray |> String
    game.Rooms
    |> Array.chunkBySize game.Width
    |> Seq.collecti (fun row roomRow -> [
        line roomRow (fun col room -> [
            wall
            toWall room.N
            if col = game.Width-1 then wall
        ])
        line roomRow (fun col room ->
            let roomI = row*game.Width + col
            [
                if roomI = game.Start then space else toWall room.W
                if roomI = game.Player then playerChar else space
                if col = game.Width-1 then toWall (roomI <> game.Finish)
            ]
        )
        if row = game.Height - 1 then
            String(wall, game.Width*2 + 1)
    ])

[<EntryPoint>]
let main args =
    let mutable game = newGame 40 14

    let eraseCursor () =
        Console.CursorLeft <- Console.CursorLeft - 1
        Console.Write " "
        Console.CursorLeft <- Console.CursorLeft - 1
    let render () =
        Console.SetCursorPosition(0, 0)
        render game |> Seq.iter Console.WriteLine
        let quitMsg = "Press [Escape] to quit."
        Console.Write (String(' ', 80))
        Console.CursorLeft <- 0
        Console.Write (
            if game.Won then "You Won!!!  " + quitMsg
            else "Use the arrow keys or H J K L to move to the exit.  " + quitMsg
        )
    let move dir =
        game <- move game dir
        render ()

    Console.CursorVisible <- false
    render ()
    let mutable run = true
    while run do
        match Console.ReadKey().Key with
        | ConsoleKey.Escape -> run <- false
        | _ when game.Won -> eraseCursor ()
        | ConsoleKey.LeftArrow | ConsoleKey.H -> move Left
        | ConsoleKey.DownArrow | ConsoleKey.J -> move Down
        | ConsoleKey.UpArrow | ConsoleKey.K -> move Up
        | ConsoleKey.RightArrow | ConsoleKey.L -> move Right
        | _ -> eraseCursor ()
    0
