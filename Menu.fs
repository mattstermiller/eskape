module Menu

let private options = 5
let private widthLimits = (8, 999)
let private heightLimits = (4, 999)

let private cursor menu offset =
    { menu with Cursor = menu.Cursor + offset |> clamp (0, options-1) }

let private activate viewSize (menu: Menu) next =
    let menuConfig config = Menu { menu with Config = config }
    match menu.Cursor with
    | 0 ->
        Game (Game.newGame viewSize menu.Config)
    | 1 ->
        let width = menu.Config.RoomWidth + (if next then 1 else -1) |> clamp widthLimits
        menuConfig { menu.Config with RoomWidth = width }
    | 2 ->
        let height = menu.Config.RoomHeight + (if next then 1 else -1) |> clamp heightLimits
        menuConfig { menu.Config with RoomHeight = height }
    | 3 ->
        let vis = if next then menu.Config.Visibility.Next else menu.Config.Visibility.Prev
        menuConfig { menu.Config with Visibility = vis }
    | 4 ->
        let scroll = if next then menu.Config.ScrollSpeed.Next else menu.Config.ScrollSpeed.Prev
        menuConfig { menu.Config with ScrollSpeed = scroll }
    | _ ->
        Menu menu

let move viewSize dir menu =
    match dir with
    | Up -> Menu (cursor menu -1)
    | Down -> Menu (cursor menu 1)
    | Left | Right -> activate viewSize menu (dir = Right)

let render menu =
    let cursor i = if menu.Cursor = i then ">" else " "
    seq {
        "  Welcome to Eskape!"
        ""
        "Use arrow keys or H J K L"
        "Esc to exit"
        ""
        cursor 0 + " New Game"
        cursor 1 + " Maze Width:  " + string menu.Config.RoomWidth
        cursor 2 + " Maze Height: " + string menu.Config.RoomHeight
        cursor 3 + " Maze Visibility: " + string menu.Config.Visibility
        cursor 4 + " View Scroll Speed: " + string menu.Config.ScrollSpeed
    }
