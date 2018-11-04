namespace FSharp21
open System

module KnuthFisherYatesShuffle =

  let replaceAt i v list =
    list |> List.mapi (fun j x -> if (j = i) then v else x)

  let swap f t list =
    let tempF = List.item f list
    let tempT = List.item t list
    replaceAt f tempT (replaceAt t tempF list)

  let shuffle xs =
    let rnd = Random()
    let rec loop xs idx =
      let ln = (List.length xs)
      match idx with
      | 0 -> xs
      | i -> loop (swap i (rnd.Next(i, ln)) xs) (i - 1)
    loop xs ((List.length xs) - 1)


