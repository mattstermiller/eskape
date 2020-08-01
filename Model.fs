[<AutoOpen>]
module Model

type Visibility =
    | Full
    | Sight
with
    override this.ToString () =
        match this with
        | Full -> "Full (easy)"
        | Sight -> "Sight (hard)"

    member this.Next =
        match this with
        | Full ->  Sight
        | Sight -> Full

    member this.Prev = this.Next

type Config = {
    Width: int
    Height: int
    Visibility: Visibility
}
with
    member this.ScreenWidth = this.Width*2 + 2
    member this.ScreenHeight = this.Height*2 + 2

    static member Default = {
        Width = 32
        Height = 16
        Visibility = Full
    }

type Menu = {
    Config: Config
    Cursor: int
}


type Direction = Up | Down | Left | Right

type Room = {
    // Whether this room has a wall to the North (Up)
    N: bool
    // Whether this room has a wall to the West (Left)
    W: bool
}

type RoomWall = N | W

type Game = {
    Config: Config
    Rooms: Room array
    Start: int
    Finish: int
    Player: int
}
with
    member this.Width = this.Config.Width
    member this.Height = this.Config.Height
    member this.Won = this.Player = this.Finish


type State =
    | Menu of Menu
    | Game of Game
with
    member this.IsInMenu =
        match this with Menu _ -> true | _ -> false

    member this.Config =
        match this with
        | Menu m -> m.Config
        | Game g -> g.Config