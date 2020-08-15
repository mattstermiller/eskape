open System
open System.Threading

let move viewSize state dir =
    match state with
    | Menu m -> Menu.move viewSize dir m
    | Game g -> Game (Game.move viewSize dir g)

let rec updateScreen viewSize (state: State) =
    Console.ForegroundColor <-
        match state with
        | Game g when g.Won -> ConsoleColor.Green
        | _ -> ConsoleColor.White
    let lines =
        match state with
        | Menu m ->
            Console.Clear()
            Menu.render m
        | Game g ->
            Game.render viewSize g
        |> Seq.toList
    lines
    |> Seq.map (fun l -> l.PadRight(fst viewSize))
    |> Seq.iter Console.WriteLine
    Console.SetCursorPosition(0, 0)
    match state with
    | Game game when game.PendingViewPos <> game.ViewPos ->
        Thread.Sleep game.Config.ScrollSpeed.Delay
        let newDim (f: int*int -> int) =
            f game.ViewPos + (f game.PendingViewPos).CompareTo(f game.ViewPos)
        updateScreen viewSize (Game { game with ViewPos = (newDim fst, newDim snd) })
    | _ -> state

let eraseCursor () =
    Console.CursorLeft <- max 0 (Console.CursorLeft - 1)
    Console.Write " "
    Console.CursorLeft <- Console.CursorLeft - 1

[<EntryPoint>]
let main args =
    Console.Title <- "Eskape"
    Console.CursorVisible <- false
    let mutable state = Menu { Config = loadConfig (); Cursor = 0 }

    // we want render slightly less than window size to account for scrollbars and avoid rendering to full buffer width
    let getViewSize () = (Console.WindowWidth-1, Console.WindowHeight-1)
    let mutable viewSize = (0, 0)

    state <- updateScreen (getViewSize ()) state
    let mutable run = true
    while run do
        let key = Console.ReadKey().Key
        eraseCursor ()
        if viewSize <> getViewSize () then
            Console.Clear()
            viewSize <- getViewSize ()
        let won =
            match state with
            | Game g when g.Won -> true
            | _ -> false
        let newState =
            match key with
            | ConsoleKey.Escape when state.IsInMenu -> run <- false; state
            | ConsoleKey.Escape -> Menu { Config = state.Config; Cursor = 0 }
            | ConsoleKey.N when won -> Game (Game.newGame viewSize state.Config)
            | ConsoleKey.LeftArrow | ConsoleKey.H -> move viewSize state Left
            | ConsoleKey.DownArrow | ConsoleKey.J -> move viewSize state Down
            | ConsoleKey.UpArrow | ConsoleKey.K -> move viewSize state Up
            | ConsoleKey.RightArrow | ConsoleKey.L -> move viewSize state Right
            | _ -> state
        if newState <> state then
            state <- updateScreen viewSize newState
    0
