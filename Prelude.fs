[<AutoOpen>]
module Prelude

open System

let rand = Random()

let shuffle items =
    let arr = items |> Seq.toArray
    for i in [0..arr.Length-1] do
        let pick = i + rand.Next(arr.Length-i)
        let swap = arr.[pick]
        arr.[pick] <- arr.[i]
        arr.[i] <- swap
    arr

let clamp (minVal, maxVal) value = value |> min maxVal |> max minVal

module String =
    let equalsIgnoreCase (a: string) b =
        a.Equals(b, StringComparison.CurrentCultureIgnoreCase)

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

open FSharp.Reflection

let private unionCases<'a> () =
    FSharpType.GetUnionCases(typeof<'a>)
    |> Array.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> 'a)

let private moveUnionCase offset (value: 'a) =
    let cases = unionCases<'a> ()
    let i = cases |> Array.findIndex ((=) value)
    let newI = i+offset |> clamp (0, cases.Length-1)
    cases.[newI]

let nextUnionCase value = moveUnionCase 1 value
let prevUnionCase value = moveUnionCase -1 value
