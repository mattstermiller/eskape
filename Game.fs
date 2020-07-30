module Game

open System

let newGame config =
    let (width, height) = (config.Width, config.Height)
    let rooms = Array.init (width*height) (fun _ -> { N = true; W = true })
    let wallsToRemove = [|
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
        Config = config
        Rooms = rooms
        Start = start
        Finish = finish
        Player = start
    }

let private nextRoom width room dir =
    match dir with
    | Up -> room - width
    | Down -> room + width
    | Left -> room - 1
    | Right -> room + 1

let private isValidDir (game: Game) room dir =
    let row = room / game.Width
    let col = room % game.Width
    match dir with
    | Up -> row > 0
    | Down -> row < game.Height-1
    | Left -> col > 0
    | Right -> col < game.Width-1

let private canMove game room dir =
    if isValidDir game room dir then
        match dir with
        | Up -> not game.Rooms.[room].N
        | Down -> not game.Rooms.[room+game.Width].N
        | Left -> not game.Rooms.[room].W
        | Right -> not game.Rooms.[room+1].W
    else false

let move (game: Game) dir =
    if not game.Won && canMove game game.Player dir then
        { game with Player = nextRoom game.Width game.Player dir }
    else game

let private roomsVisible game =
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
    let visibleRooms = if game.Config.Visibility = Sight then roomsVisible game else Set.empty
    let visible (room: int) dirs =
        if game.Config.Visibility = Full || game.Won then true
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
            ""
            if game.Won then
                "You Won!!!" 
            else
                "Use Arrows or H J K L to move."
            "Esc for Menu."
        ] |> fun s -> s.PadRight (game.Config.ScreenWidth)
    Seq.append [msg] maze
