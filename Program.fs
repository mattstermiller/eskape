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
    member this.ScreenWidth = this.Width*2 + 1
    member this.ScreenHeight = this.Height*2 + 2

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

let nextRoom width room dir =
    match dir with
    | Up -> room - width
    | Down -> room + width
    | Left -> room - 1
    | Right -> room + 1

let isValidDir game room dir =
    let row = room / game.Width
    let col = room % game.Width
    match dir with
    | Up -> row > 0
    | Down -> row < game.Height-1
    | Left -> col > 0
    | Right -> col < game.Width-1

let canMove game room dir =
    if isValidDir game room dir then
        match dir with
        | Up -> not game.Rooms.[room].N
        | Down -> not game.Rooms.[room+game.Width].N
        | Left -> not game.Rooms.[room].W
        | Right -> not game.Rooms.[room+1].W
    else false

let move game dir =
    if canMove game game.Player dir then
        { game with Player = nextRoom game.Width game.Player dir }
    else game

let roomsVisible game =
    let rec trace room acc dir =
        if canMove game room dir then
            let next = nextRoom game.Width room dir
            trace next (next :: acc) dir
        else acc
    let tracePlayer = trace game.Player []
    set [
        game.Player
        yield! tracePlayer Up
        yield! tracePlayer Down
        yield! tracePlayer Left
        yield! tracePlayer Right
    ]

let render game =
    let wall = '█'
    let playerChar = '*'
    let space = ' '
    let toWall b = if b then wall else space
    let line roomRow f = roomRow |> Seq.collecti f |> Seq.toArray |> String
    let visibleRooms = roomsVisible game
    let visible (room: int) dirs =
        if game.Won then true
        else
            let dirs = dirs |> List.filter (isValidDir game room)
            let rooms = set [
                room
                yield! dirs |> List.map (nextRoom game.Width room)
                dirs |> List.fold (nextRoom game.Width) room
            ]
            visibleRooms |> Set.intersect rooms |> (not << Set.isEmpty)
    let maze =
        game.Rooms
        |> Seq.indexed
        |> Seq.chunkBySize game.Width
        |> Seq.collecti (fun row roomRow -> [
            line roomRow (fun col (i, room) -> [
                toWall (visible i [Left; Up])
                toWall (room.N && visible i [Up])
                if col = game.Width-1 then
                    toWall (visible i [Up])
            ])
            line roomRow (fun col (i, room) -> [
                toWall (room.W && i <> game.Start && visible i [Left])
                if i = game.Player then playerChar else space
                if col = game.Width-1 then
                    toWall (i <> game.Finish && visible i [])
            ])
            if row = game.Height - 1 then
                line roomRow (fun col (i, _) -> [
                    toWall (visible i [Left])
                    toWall (visible i [])
                    if col = game.Width-1 then
                        toWall (visible i [])
                ])
        ])
    let msg =
        String.concat "  " [
            "  Eskape!"
            if game.Won then
                "You Won!!!" 
                "Press N to start a new game."
            else
                "Use the arrow keys or H J K L to move."
            "Press [Escape] to quit."
        ] |> fun s -> s.PadRight (game.ScreenWidth)
    Seq.append [msg] maze

[<EntryPoint>]
let main args =
    Console.Title <- "Eskape"
    Console.CursorVisible <- false
    let w, h = 40, 20
    let mutable game = newGame w h
    Console.SetWindowSize(game.ScreenWidth+1, game.ScreenHeight)

    let eraseCursor () =
        Console.CursorLeft <- max 0 (Console.CursorLeft - 1)
        Console.Write " "
        Console.CursorLeft <- Console.CursorLeft - 1
    let updateScreen () =
        render game |> Seq.iter Console.WriteLine
        Console.SetCursorPosition(0, 0)
    let move dir =
        game <- move game dir
        updateScreen ()
    let newGame () =
        game <- newGame w h
        updateScreen()

    updateScreen ()
    let mutable run = true
    while run do
        let key = Console.ReadKey().Key
        eraseCursor ()
        match key with
        | ConsoleKey.Escape -> run <- false
        | ConsoleKey.N when game.Won -> newGame ()
        | _ when game.Won -> ()
        | ConsoleKey.LeftArrow | ConsoleKey.H -> move Left
        | ConsoleKey.DownArrow | ConsoleKey.J -> move Down
        | ConsoleKey.UpArrow | ConsoleKey.K -> move Up
        | ConsoleKey.RightArrow | ConsoleKey.L -> move Right
        | _ -> ()
    0
