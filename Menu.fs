module Menu

open System

let private options = 4
let private widthLimits = (16, 64)
let private heightLimits = (4, 32)

let private cursor menu offset =
    { menu with Cursor = menu.Cursor + offset |> max 0 |> min (options-1) }

let updateScreenSize (config: Config) =
    try
        if (Console.WindowWidth, Console.WindowHeight) <> (config.ScreenWidth, config.ScreenHeight) then
            Console.SetWindowSize(config.ScreenWidth, config.ScreenHeight)
        true
    with _ ->
        false

let private tryUpdateScreen menu config =
    if updateScreenSize config then
        Menu { menu with Config = config }
    else
        Menu menu

let private activate (menu: Menu) next =
    match menu.Cursor with
    | 0 ->
        Game (Game.newGame menu.Config)
    | 1 ->
        let width = menu.Config.Width + (if next then 1 else -1) |> clamp widthLimits
        tryUpdateScreen menu { menu.Config with Width = width }
    | 2 ->
        let height = menu.Config.Height + (if next then 1 else -1) |> clamp heightLimits
        tryUpdateScreen menu { menu.Config with Height = height }
    | 3 ->
        let vis = if next then menu.Config.Visibility.Next else menu.Config.Visibility.Prev
        Menu { menu with Config = { menu.Config with Visibility = vis } }
    | _ ->
        Menu menu

let move menu dir =
    match dir with
    | Up -> Menu (cursor menu -1)
    | Down -> Menu (cursor menu 1)
    | Left | Right -> activate menu (dir = Right)

let render menu =
    let cursor i = if menu.Cursor = i then ">" else " "
    seq {
        "  Welcome to Eskape!"
        ""
        "Use arrow keys or H J K L"
        "Esc to exit"
        ""
        cursor 0 + " New Game"
        cursor 1 + " Maze Width:  " + string menu.Config.Width
        cursor 2 + " Maze Height: " + string menu.Config.Height
        cursor 3 + " Maze Visibility: " + string menu.Config.Visibility
    }
