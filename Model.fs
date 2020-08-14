[<AutoOpen>]
module Model

type Visibility =
    | Full
    | Explored
    | Sight
with
    override this.ToString () =
        match this with
        | Full -> "Full (easy)"
        | Explored -> "Explored (medium)"
        | Sight -> "Sight (hard)"

    member this.Next = nextUnionCase this
    member this.Prev = prevUnionCase this

type ScrollSpeed =
    | Slowest
    | Slow
    | Medium
    | Fast
    | Fastest
with
    // override this.ToString () = sprintf "%A" this
    member this.Next = nextUnionCase this
    member this.Prev = prevUnionCase this

    member this.Delay =
        match this with
        | Slowest -> 60
        | Slow -> 45
        | Medium -> 30
        | Fast -> 15
        | Fastest -> 0

type Config = {
    RoomWidth: int
    RoomHeight: int
    Visibility: Visibility
    ScrollSpeed: ScrollSpeed
}
with
    member this.CharWidth = this.RoomWidth*2 + 1
    member this.CharHeight = this.RoomHeight*2 + 1

    static member Default = {
        RoomWidth = 32
        RoomHeight = 16
        Visibility = Explored
        ScrollSpeed = Medium
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
    RenderedMaze: char array array
    /// Set of visible char coordinates
    VisibleCoords: (int * int) Set
    ViewPos: int * int
    PendingViewPos: int * int
    Start: int
    Finish: int
    Player: int
}
with
    member this.Width = this.Config.RoomWidth
    member this.Height = this.Config.RoomHeight
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
