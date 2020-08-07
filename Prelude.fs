[<AutoOpen>]
module Prelude

let rand = System.Random()

let shuffle items =
    let arr = items |> Seq.toArray
    for i in [0..arr.Length-1] do
        let pick = i + rand.Next(arr.Length-i)
        let swap = arr.[pick]
        arr.[pick] <- arr.[i]
        arr.[i] <- swap
    arr

let clamp (minVal, maxVal) value = value |> min maxVal |> max minVal

module Array =
    let collecti f =
        let mutable i = 0
        Array.collect (fun a ->
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
