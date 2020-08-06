module Game

open System

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

let toScreenCoord (config: Config) room =
    (room%config.Width*2 + 1, room/config.Width*2 + 1)

let toAreaScreenCoords config room =
    let x, y = toScreenCoord config room
    let minX, maxX = max (x-1) 0, min (x+1) (config.ScreenWidth-1)
    let minY, maxY = max (y-1) 0, min (y+1) (config.ScreenHeight-1)
    [minY..maxY] |> List.collect (fun y -> [minX..maxX] |> List.map (fun x -> (x, y)))

let private updateVisibleCoords game =
    let rec trace room acc dir =
        if canMove game room dir then
            let next = nextRoom game.Width room dir
            trace next (next :: acc) dir
        else acc
    let tracePlayer = trace game.Player []
    let visibleCoords = lazy (
        let visibleRooms = [
            game.Player
            yield! tracePlayer Up
            yield! tracePlayer Down
            yield! tracePlayer Left
            yield! tracePlayer Right
        ]
        visibleRooms |> List.collect (toAreaScreenCoords game.Config) |> set
    )
    match game.Config.Visibility with
    | Full -> game
    | Explored -> { game with VisibleCoords = Set.union game.VisibleCoords visibleCoords.Value }
    | Sight -> { game with VisibleCoords = visibleCoords.Value }

let renderMaze game =
    let wall = '█'
    let space = ' '
    let toChar isWall = if isWall then wall else space
    let line roomRow f = roomRow |> Array.collecti f
    let maze =
        game.Rooms
        |> Array.indexed
        |> Array.chunkBySize game.Width
        |> Array.collecti (fun row roomRow -> [|
            line roomRow (fun col (_, room) -> [|
                wall
                toChar room.N
                if col = game.Width-1 then
                    wall
            |])
            line roomRow (fun col (i, room) -> [|
                toChar (room.W && i <> game.Start)
                space
                if col = game.Width-1 then
                    toChar (i <> game.Finish)
            |])
            if row = game.Height - 1 then
                line roomRow (fun col _ -> [|
                    wall
                    wall
                    if col = game.Width-1 then
                        wall
                |])
        |])
    { game with RenderedMaze = maze }

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
        RenderedMaze = [||]
        VisibleCoords = Set.empty
        Start = start
        Finish = finish
        Player = start
    }
    |> renderMaze
    |> updateVisibleCoords

let move (game: Game) dir =
    if not game.Won && canMove game game.Player dir then
        { game with Player = nextRoom game.Width game.Player dir }
        |> updateVisibleCoords
    else game

let render game =
    let unknown = '░'
    let playerChar = '*'
    let playerCoord = toScreenCoord game.Config game.Player
    let isVisible coord =
        if game.Config.Visibility = Full || game.Won then true
        else game.VisibleCoords |> Set.contains coord
    let maze =
        game.RenderedMaze |> Array.mapi (fun rowi row ->
            row |> Array.mapi (fun coli char ->
                let coord = coli, rowi
                if coord = playerCoord then playerChar
                elif isVisible coord then char
                else unknown
            ) |> String
        )
    let msg =
        String.concat "  " [
            ""
            if game.Won then
                "You Won!!!" 
                "N for new game."
            else
                "Use Arrows or H J K L to move."
            "Esc for Menu."
        ] |> fun s -> s.PadRight (game.Config.ScreenWidth)
    Seq.append [msg] maze
