namespace FSharp21
open System

module Types =

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

  type Player = { name:String; hand: Hand; Limit:int; id:int }

  type GameState = {
    players: Player list;
    dealer: Player;
    results: string list;
    deck: Deck;
    discardPile: Card List
  }