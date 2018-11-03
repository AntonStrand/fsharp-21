namespace FSharp21
open System
open FSharp21.Types
open FSharp21.Card
open FSharp21.Player

module View =
  let cardToString card =
   sprintf "%A of %A" card.Face card.Suit

  let handToString hand =
    hand |> List.map cardToString |> String.concat ", "

  let playerToString player =
    if calcHandValue player.hand > 0
    then sprintf "%s\'s hand is %A with a value of %i" player.name (handToString player.hand) (calcHandValue player.hand)
    else sprintf "%s has not played" player.name

  let getWinnerName dealer player =
    if not (isBusted dealer) && calcHandValue dealer.hand >= calcHandValue player.hand
    then dealer.name
    else player.name

  let generateResult dealer player =
    sprintf "\n%s\n%s\n%s wins!"
      (playerToString player)
      (playerToString dealer)
      (getWinnerName dealer player)

  let presentResult gameState =
    Console.Clear()
    Console.WriteLine "The results"
    gameState.results
      |> List.map Console.WriteLine |> ignore
