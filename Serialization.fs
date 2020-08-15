module Serialization

open System
open System.IO
open FSharp.Reflection

let private getPropertyValues a =
    FSharpType.GetRecordFields(a.GetType())
    |> Array.map (fun prop -> (prop, prop.GetValue a))

let private serialize a =
    getPropertyValues a
    |> Array.map (fun (prop, value) -> sprintf "%s=%A" prop.Name value)
    |> String.concat "\n"

let private readKeyValues (str: string) =
    str.Split([|'\n';'\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.choose (fun line ->
        let i = line.IndexOf '='
        if i > 0 then Some (line.Substring(0, i), line.Substring(i+1))
        else None
    )

let private convert (typ: Type) a =
    try
        if FSharpType.IsUnion typ then
            let case = FSharpType.GetUnionCases typ |> Array.find (fun case -> case.Name |> String.equalsIgnoreCase a)
            Some (FSharpValue.MakeUnion(case, [||]))
        else
            Some (Convert.ChangeType(a, typ))
    with _ -> None

let private deserialize (defaultVal: 'a) str =
    let fromStr = readKeyValues str |> Map
    let values =
        getPropertyValues defaultVal
        |> Array.map (fun (prop, defPropVal) ->
            fromStr.TryFind prop.Name
            |> Option.bind (convert prop.PropertyType)
            |> Option.defaultValue defPropVal
        )
    FSharpValue.MakeRecord(typeof<'a>, values) :?> 'a

let appDataDirectory = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)

let readFile (filePath: string) defaultVal =
    if File.Exists filePath then
        File.ReadAllText filePath
        |> deserialize defaultVal
    else defaultVal

let writeFile (filePath: string) value =
    filePath |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
    let str = serialize value
    File.WriteAllText(filePath, str)
