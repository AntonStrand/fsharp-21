namespace FSharp21
open FSharp21.Types
open FSharp21.Card
open FSharp21.Player
open FSharp21.View

module Game =
  let separatePlayers state =
    (state.players, state)

  let initGameState numberOfPlayers =
    {
    players = createPlayers numberOfPlayers;
    dealer = createPlayer 17 "Dealer" 0;
    results = [];
    deck = createDeck ();
    discardPile = []
    } |> separatePlayers

  let updatePlayer players player =
    players |> List.map (fun p -> if (p.id = player.id) then player else p)

  let updateGameState gameState (player, newDeck) =
    { gameState with deck = newDeck; players = (updatePlayer gameState.players player) }

  let rec dealToAll (players, gameState) =
    match players with
    | [] -> (gameState.players, gameState)
    | player::remainingPlayers ->
      dealToAll (remainingPlayers, (updateGameState gameState (dealCard player gameState.deck)))

  let rec dealToDone player deck =
    let newPlayer, newDeck = dealCard player deck
    if (calcHandValue newPlayer.hand < player.Limit)
    then dealToDone newPlayer newDeck
    else (newPlayer, newDeck)

  let dealToDealer gameState =
    let dealer, deck = dealToDone gameState.dealer gameState.deck
    { gameState with dealer = dealer; deck = deck; }

  let shouldDealerPlay player =
    not (isBusted player || has21 player || winningByNumberOfCards player)

  let addResult gameState player =
    { gameState with results = gameState.results @ [(generateResult gameState.dealer player)] }

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

  let rec playAll (players, gameState) =
    match players with
    | [] -> gameState
    | player::remainingPlayers ->
      playAll (remainingPlayers, (playerRound gameState player))