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

  let reuseDiscardedCards gameState =
    { gameState with deck = (reshuffle gameState.deck gameState.discardPile); discardPile = [] }

  let rec dealCard player gameState =
    if not (canDrawCard gameState.deck)
    then dealCard player (reuseDiscardedCards gameState)
    else (fst (deal player gameState.deck), updateGameState gameState (deal player gameState.deck))

  let rec dealToAll (players, gameState) =
    match players with
    | [] -> (gameState.players, gameState)
    | player::remainingPlayers ->
      dealToAll (remainingPlayers, (snd (dealCard player gameState)))

  let rec dealToDone player gameState =
    let newPlayer, newState = dealCard player gameState
    if (calcHandValue newPlayer.hand < player.Limit)
    then dealToDone newPlayer newState
    else (newPlayer, newState)

  let dealToDealer gameState =
    let dealer, newState = dealToDone gameState.dealer gameState
    { newState with dealer = dealer }

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
    let player, state = (dealToDone player gameState)
    if (shouldDealerPlay player)
    then finishRound (dealToDealer { state with players = (updatePlayer state.players player) }) player
    else finishRound ({ state with players = (updatePlayer state.players player) }) player

  let rec playAll (players, gameState) =
    match players with
    | [] -> gameState
    | player::remainingPlayers ->
      playAll (remainingPlayers, (playerRound gameState player))
