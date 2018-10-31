namespace FSharp21
open System
open FSharp21.Random

module Card =
  type Face =
    | Two = 0 | Three = 1 | Four = 2 | Five = 3
    | Six = 4 | Seven = 5 | Eight = 6 | Nine = 7
    | Ten = 8 | Jack = 9  | Queen = 10 | King = 11
    | Ace = 12

  type Suit =
    | Hearts = 0
    | Spade = 1
    | Diamond = 2
    | Clubs = 3

  type Card = { Face: Face; Suit: Suit }

  type Deck = Card List
  type Hand = Card List

  let createCard face suit =
    { Face = face; Suit = suit }

  let shuffle = List.sortBy random

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
