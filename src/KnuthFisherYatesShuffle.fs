namespace FSharp21
open System

module KnuthFisherYatesShuffle =

  let replaceAt i v = List.mapi (fun j x -> if (j = i) then v else x)

  let swap i j list =
    let x = List.item i list
    let y = List.item j list
    replaceAt i y (replaceAt j x list)

  let shuffle xs =
    let rnd = Random()
    let rec loop xs idx =
      let ln = (List.length xs)
      match idx with
      | 0 -> xs
      | i -> loop (swap i (rnd.Next(i, ln)) xs) (i - 1)
    loop xs ((List.length xs) - 1)


