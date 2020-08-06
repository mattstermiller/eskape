open System

let move state dir =
    match state with
    | Menu m -> Menu.move m dir
    | Game g -> Game (Game.move g dir)

let updateScreen (state: State) =
    Console.ForegroundColor <-
        match state with
        | Game g when g.Won -> ConsoleColor.Green
        | _ -> ConsoleColor.White
    let lines =
        match state with
        | Menu m -> Menu.render m
        | Game g -> Game.render g
        |> Seq.toList
    lines @ List.init (state.Config.ScreenHeight + 1 - lines.Length) (fun _ -> "")
    |> Seq.map (fun line -> line.PadRight state.Config.ScreenWidth)
    |> Seq.iter Console.WriteLine
    Console.SetCursorPosition(0, 0)

let eraseCursor () =
    Console.CursorLeft <- max 0 (Console.CursorLeft - 1)
    Console.Write " "
    Console.CursorLeft <- Console.CursorLeft - 1

[<EntryPoint>]
let main args =
    Console.Title <- "Eskape"
    Console.CursorVisible <- false
    let mutable state = Menu { Config = Config.Default; Cursor = 0 }
    Menu.updateScreenSize state.Config |> ignore

    updateScreen state
    let mutable run = true
    while run do
        let key = Console.ReadKey().Key
        eraseCursor ()
        let won =
            match state with
            | Game g when g.Won -> true
            | _ -> false
        let newState =
            match key with
            | ConsoleKey.Escape when state.IsInMenu -> run <- false; state
            | ConsoleKey.Escape -> Menu { Config = state.Config; Cursor = 0 }
            | ConsoleKey.N when won -> Game (Game.newGame state.Config)
            | ConsoleKey.LeftArrow | ConsoleKey.H -> move state Left
            | ConsoleKey.DownArrow | ConsoleKey.J -> move state Down
            | ConsoleKey.UpArrow | ConsoleKey.K -> move state Up
            | ConsoleKey.RightArrow | ConsoleKey.L -> move state Right
            | _ -> state
        if newState <> state then
            state <- newState
            updateScreen state
    0
