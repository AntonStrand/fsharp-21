namespace FSharp21
open System
open FSharp21.Types
open FSharp21.KnuthFisherYatesShuffle

module Card =
  let createCard face suit =
    { Face = face; Suit = suit }

  let createDeck () :Deck = shuffle [
     for face in unbox (Enum.GetValues(typeof<Face>)) do
     for suit in unbox (Enum.GetValues(typeof<Suit>)) do
     yield createCard face suit
  ]

  let getCardValue card =
    match card.Face with
    | Face.Two -> 2
    | Face.Three -> 3
    | Face.Four -> 4
    | Face.Five -> 5
    | Face.Six -> 6
    | Face.Seven -> 7
    | Face.Eight -> 8
    | Face.Nine -> 9
    | Face.Ten -> 10
    | Face.Jack -> 11
    | Face.Queen -> 12
    | Face.King -> 13
    | _ -> 14

  let isAce card = card.Face = Face.Ace

  let reshuffle deck discardPile =
    deck @ discardPile |> shuffle

  let canDrawCard deck =
    List.length deck > 1

  let drawCard (deck: Deck): Card * Deck =
    (List.head deck, List.tail deck)
