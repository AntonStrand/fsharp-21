// Learn more about F# at http://fsharp.org

open System
open FSharp21.View
open FSharp21.Game
open FSharp21.InputValidation

let play =
  presentResult << playAll << dealToAll <<initGameState

[<EntryPoint>]
let main argv =
    printfn "Welcome to 21."
    printfn "Enter number of players."
    match isValidInput (Console.ReadLine()) with
    | None -> printfn "You have to select a number between 1-9."
    | Some numberOfPlayers -> play numberOfPlayers
    0 // return an integer exit code