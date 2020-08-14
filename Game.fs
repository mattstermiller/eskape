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

let toCharCoord (config: Config) room =
    (room%config.RoomWidth*2 + 1, room/config.RoomWidth*2 + 1)

let toAreaCharCoords config room =
    let x, y = toCharCoord config room
    let minX, maxX = max (x-1) 0, min (x+1) (config.CharWidth-1)
    let minY, maxY = max (y-1) 0, min (y+1) (config.CharHeight-1)
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
        visibleRooms |> List.collect (toAreaCharCoords game.Config) |> set
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

let private getViewCenteredOn (config: Config) viewSize (x, y) =
    let viewWidth, viewHeight = viewSize
    let viewLeft = x - viewWidth/2 |> clamp (0, max 0 (config.CharWidth - viewWidth))
    let viewTop = y - viewHeight/2 |> clamp (0, max 0 (config.CharHeight - viewHeight))
    (viewLeft, viewTop)

let newGame viewSize config =
    let (width, height) = (config.RoomWidth, config.RoomHeight)
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
    let view = getViewCenteredOn config viewSize (toCharCoord config start)
    {
        Config = config
        Rooms = rooms
        RenderedMaze = [||]
        VisibleCoords = Set.empty
        ViewPos = view
        PendingViewPos = view
        Start = start
        Finish = finish
        Player = start
    }
    |> renderMaze
    |> updateVisibleCoords

let private updateView viewSize (game: Game) =
    let minDist = 3
    let player = toCharCoord game.Config game.Player
    let center = lazy (getViewCenteredOn game.Config viewSize player)
    let viewDim f =
        if f player < f game.ViewPos + minDist - 1 || f player > f game.ViewPos + f viewSize - minDist then f center.Value
        else f game.ViewPos
    { game with PendingViewPos = (viewDim fst, viewDim snd) }

let move viewSize dir (game: Game) =
    if not game.Won && canMove game game.Player dir then
        { game with Player = nextRoom game.Width game.Player dir }
        |> updateVisibleCoords
        |> updateView viewSize
    else game

let render (viewWidth, viewHeight) game =
    let unknown = '░'
    let playerChar = '*'
    let playerCoord = toCharCoord game.Config game.Player
    let viewLeft, viewTop = game.ViewPos
    let isVisible coord =
        if game.Config.Visibility = Full || game.Won then true
        else game.VisibleCoords |> Set.contains coord
    let maze =
        game.RenderedMaze |> Array.skip viewTop |> Array.truncate viewHeight |> Array.mapi (fun y row ->
            row |> Array.skip viewLeft |> Array.truncate viewWidth |> Array.mapi (fun x char ->
                let coord = x+viewLeft, y+viewTop
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
        ]
    Seq.append [msg] maze
