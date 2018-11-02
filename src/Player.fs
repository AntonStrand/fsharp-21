namespace FSharp21
open System
open FSharp21.Card

module Player =

  type Hand = Card List

  type Player = { name:String; hand: Hand; Limit:int; id:int }

  let createPlayer limit name id =
    { name = name; Limit = limit; hand = Hand.Empty; id = id }

  let createPlayers n =
    [1..n] |> List.map (fun i -> createPlayer 17 (sprintf "%s %i" "Player" i) i)

  let getAces = List.filter isAce

  let shouldReduce (aces:Card list) total =
    total > 21 && not aces.IsEmpty

  let rec reduceAceWorth aces total =
    if (shouldReduce aces total)
    then reduceAceWorth (List.tail aces) (total - 13)
    else total

  let sumCards = List.map getCardValue >> List.sum

  let calcHandValue hand =
    reduceAceWorth (getAces hand) (sumCards hand)

  let deal player card =
    { player with hand = player.hand @ [card] }

  let dealCard player deck =
    let card, newDeck = drawCard deck
    (deal player card, newDeck)

  let numberOfCards = List.length

  let isBusted player =
    calcHandValue player.hand > 21

  let has21 player =
    calcHandValue player.hand = 21

  let winningByNumberOfCards player =
    (numberOfCards player.hand) = 5 && has21 player

  let discardCards player =
    (player.hand, { player with hand = Hand.Empty })
