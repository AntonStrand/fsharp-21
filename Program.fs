﻿// Learn more about F# at http://fsharp.org

open System

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

let createDeck () :Deck = [
   for face in unbox (Enum.GetValues(typeof<Face>)) do
   for suit in unbox (Enum.GetValues(typeof<Suit>)) do
   yield createCard face suit
]

let runWithTuple fn (x, y) =
  fn x y

type Player = { Name:String; Hand: Hand; Limit:int; id:int }

let createPlayer limit name id =
  { Name = name; Limit = limit; Hand = List<Card>.Empty; id = id }

let createPlayers n =
  [1..n]
    |> List.map (fun i -> ((sprintf "%s %i" "Player" i), i))
    |> List.map (runWithTuple (createPlayer 17))

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
  { player with Hand = List.append player.Hand [card] }

let drawCard (deck: Deck): Card * Deck =
  (List.head deck, List.tail deck)

let dealCard player deck =
  let card, newDeck = drawCard deck
  (deal player card, newDeck)

let rec dealToDone player deck =
  let newPlayer, newDeck = dealCard player deck
  if (calcHandValue newPlayer.Hand < player.Limit)
  then dealToDone newPlayer newDeck
  else (newPlayer, newDeck)

let randomFactory (rand:Random) _ =
  rand.Next ()

let random =
  randomFactory (Random())

let shuffle xs =
  xs |> List.sortBy random

let cardToString card =
  sprintf "%A of %A" card.Face card.Suit

let handToString hand =
  hand |> List.map cardToString |> String.concat ", "

let players = createPlayers 4

type GameState = {
  players: Player list;
  dealer: Player;
  results: string list;
  deck: Deck;
  discardPile: Card List
}

let initGameState numberOfPlayers =
  {
  players = createPlayers numberOfPlayers;
  dealer = createPlayer 17 "Dealer" 0;
  results = [];
  deck = createDeck () |> shuffle;
  discardPile = []
}

let updatePlayer players player =
  players |> List.map (fun p -> if (p.id = player.id) then player else p)

let updateGameState gameState (player, newDeck) =
  { gameState with deck = newDeck; players = (updatePlayer gameState.players player) }

let rec dealToAll players gameState =
  match players with
  | [] -> gameState
  | player::remainingPlayers ->
    dealToAll remainingPlayers (updateGameState gameState (dealCard player gameState.deck))

let dealToDealer gameState =
  let dealer, deck = dealToDone gameState.dealer gameState.deck
  { gameState with dealer = dealer; deck = deck; }

let numberOfCards = List.length

let isBusted player =
  calcHandValue player.Hand > 21

let has21 player =
  calcHandValue player.Hand = 21

let winningByNumberOfCards player =
  (numberOfCards player.Hand) = 5 && has21 player

let shouldDealerPlay player =
  not (isBusted player || has21 player || winningByNumberOfCards player)

let getWinnerName dealer player =
  if not (isBusted dealer) && calcHandValue dealer.Hand >= calcHandValue player.Hand
  then dealer.Name
  else player.Name

let playerToString player =
  if calcHandValue player.Hand > 0
  then sprintf "%s\'s hand is %A with a value of %i" player.Name (handToString player.Hand) (calcHandValue player.Hand)
  else sprintf "%s has not played" player.Name

let generateResult dealer player =
  sprintf "%s\n%s\n%s wins!"
    (playerToString player)
    (playerToString dealer)
    (getWinnerName dealer player)

let addResult gameState player =
  { gameState with results = gameState.results @ [(generateResult gameState.dealer player)] }

let discardCards player =
  (player.Hand, { player with Hand = Hand.Empty })

let resetRound gameState player =
  let playerCards, player = discardCards player
  let dealerCards, dealer = discardCards gameState.dealer
  { gameState with
      players = (updatePlayer gameState.players player);
      dealer = dealer;
      discardPile = gameState.discardPile @ playerCards @ dealerCards
  }

let finishRound gameState player =
  resetRound (addResult gameState player) player

let playerRound gameState player =
  let player, deck = (dealToDone player gameState.deck)
  if (shouldDealerPlay player)
  then finishRound (dealToDealer (updateGameState gameState (player, deck))) player
  else finishRound (updateGameState gameState (player, deck)) player


let rec playAll players gameState =
  match players with
  | [] -> gameState
  | player::remainingPlayers ->
    playAll remainingPlayers (playerRound gameState player)

let separatePlayers state =
  (state.players, state)

let printLine s = printfn "\n%A\n" s 

let presentResult gameState =
  gameState.results
    |> List.map printLine |> ignore


let play =
  presentResult << runWithTuple playAll << separatePlayers << runWithTuple dealToAll << separatePlayers << initGameState


let tryToInt (s:string) = 
  match System.Int32.TryParse s with
  | true, v -> Some v
  | false, _ -> None

let maybeWithinScope x =
  if x > 0 && x < 10
  then Some x
  else None

let isValidInput x =
  tryToInt x |> Option.bind maybeWithinScope


[<EntryPoint>]
let main argv =
    printfn "Welcome to 21."
    printfn "Enter number of players."
    match isValidInput (Console.ReadLine()) with
    | None -> printfn "You have to select a number between 1-9."
    | Some numberOfPlayers -> play numberOfPlayers
    0 // return an integer exit code