module TicTacToe (..) where

import Maybe exposing (andThen)
import List.Extra as ListE exposing (minimumBy, maximumBy)


type Player
  = X
  | O


type Square
  = Open
  | Taken Player


type Row
  = T
  | M
  | B


type Column
  = L
  | C
  | R


type alias Position =
  ( Row, Column )


type alias Space =
  ( Position, Square )


type alias Board =
  List Space


type WinState
  = Playing
  | Win Player
  | Draw


opponent : Player -> Player
opponent player =
  case player of
    X ->
      O

    O ->
      X


play : Player -> Board -> Position -> Board
play player board position =
  List.map
    (\( p, s ) ->
      if position == p then
        ( p, Taken player )
      else
        ( p, s )
    )
    board


bothPlayers : List Player
bothPlayers =
  [ X, O ]


allRows : List Row
allRows =
  [ T, M, B ]


allColumns : List Column
allColumns =
  [ L, C, R ]


checkRow : Board -> Row -> Player -> Maybe Player
checkRow board row player =
  let
    takenSpaces =
      List.filter
        (\s ->
          case s of
            ( ( r, _ ), Taken p ) ->
              row == r && player == p

            _ ->
              False
        )
        board
  in
    case takenSpaces of
      [ _, _, _ ] ->
        Just player

      _ ->
        Nothing


checkColumn : Board -> Column -> Player -> Maybe Player
checkColumn board column player =
  let
    takenSpaces =
      List.filter
        (\s ->
          case s of
            ( ( _, c ), Taken p ) ->
              column == c && player == p

            _ ->
              False
        )
        board
  in
    case takenSpaces of
      [ _, _, _ ] ->
        Just player

      _ ->
        Nothing


checkColumns : Board -> List (Maybe Player)
checkColumns board =
  List.concatMap
    (\p ->
      List.map
        (\r ->
          checkColumn board r p
        )
        allColumns
    )
    bothPlayers


checkRows : Board -> List (Maybe Player)
checkRows board =
  List.concatMap
    (\p ->
      List.map
        (\r ->
          checkRow board r p
        )
        allRows
    )
    bothPlayers


getPlayerFromSpace : Space -> Maybe Player
getPlayerFromSpace space =
  let
    ( _, square ) =
      space
  in
    case square of
      Taken player ->
        Just player

      Open ->
        Nothing


getNextPlayerFromBoard : Board -> Player -> Maybe Player
getNextPlayerFromBoard board player =
  let
    nextSpace =
      List.head board

    maybePlayer =
      nextSpace `andThen` getPlayerFromSpace
  in
    maybePlayer
      `andThen` (\nextPlayer ->
                  if nextPlayer == player then
                    Just player
                  else
                    Nothing
                )


checkDiagonal : Board -> Maybe Player
checkDiagonal board =
  List.head board
    `andThen` getPlayerFromSpace
    `andThen` (getNextPlayerFromBoard <| List.drop 4 board)
    `andThen` (getNextPlayerFromBoard <| List.drop 8 board)


checkAntiDiagonal : Board -> Maybe Player
checkAntiDiagonal board =
  List.head (List.drop 2 board)
    `andThen` getPlayerFromSpace
    `andThen` (getNextPlayerFromBoard <| List.drop 4 board)
    `andThen` (getNextPlayerFromBoard <| List.drop 6 board)


checkDiagonals : Board -> List (Maybe Player)
checkDiagonals board =
  [ checkDiagonal board, checkAntiDiagonal board ]


isSpaceOpen : Space -> Bool
isSpaceOpen space =
  case space of
    ( _, Open ) ->
      True

    _ ->
      False


hasOpenSpaces : Board -> Bool
hasOpenSpaces board =
  List.any isSpaceOpen board


checkWin : Board -> WinState
checkWin board =
  let
    checks =
      checkRows board ++ checkColumns board ++ checkDiagonals board

    maybePlayer =
      List.head <| List.filterMap identity checks
  in
    case maybePlayer of
      Just player ->
        Win player

      Nothing ->
        if hasOpenSpaces board then
          Playing
        else
          Draw


openSpace : Row -> Column -> Space
openSpace row column =
  ( ( row, column ), Open )


openBoard : Board
openBoard =
  List.concatMap
    (\r ->
      List.map
        (openSpace r)
        allColumns
    )
    allRows


getScore : Int -> Player -> WinState -> Int
getScore depth player winState =
  case winState of
    Win winner ->
      if winner == player then
        10 - depth
      else
        depth - 10

    _ ->
      0


getPositionFromSpace : Space -> Position
getPositionFromSpace space =
  let
    ( position, _ ) =
      space
  in
    position


type FoldValue value
  = KeepGoing value
  | StopEarly value


minimax : Int -> Int -> Int -> Player -> Player -> Board -> ( Int, Maybe Position )
minimax depth min max originalPlayer currentPlayer board =
  let
    availableSpaces =
      List.filter isSpaceOpen board

    initialWinState =
      checkWin board
  in
    case initialWinState of
      Win _ ->
        ( getScore depth originalPlayer initialWinState, Nothing )

      Draw ->
        ( 0, Nothing )

      _ ->
        let
          availablePositions =
            List.map getPositionFromSpace availableSpaces

          startingValue =
            if originalPlayer == currentPlayer then
              ( KeepGoing min, Nothing )
            else
              ( KeepGoing max, Nothing )

          scoreAndMove =
            List.foldr
              (\position foldValue ->
                let
                  newBoard =
                    play currentPlayer board position

                  winState =
                    checkWin newBoard
                in
                  case winState of
                    Playing ->
                      if originalPlayer == currentPlayer then
                        case foldValue of
                          ( KeepGoing lastScore, lastPosition ) ->
                            let
                              ( score, _ ) =
                                minimax
                                  (depth + 1)
                                  lastScore
                                  max
                                  originalPlayer
                                  (opponent currentPlayer)
                                  newBoard
                            in
                              if score > lastScore then
                                if score > max then
                                  ( StopEarly max, lastPosition )
                                else
                                  ( KeepGoing score, Just position )
                              else
                                ( KeepGoing lastScore, lastPosition )

                          ( StopEarly lastValue, lastPosition ) ->
                            ( StopEarly lastValue, lastPosition )
                      else
                        case foldValue of
                          ( KeepGoing lastScore, lastPosition ) ->
                            let
                              ( score, _ ) =
                                minimax
                                  (depth + 1)
                                  min
                                  lastScore
                                  originalPlayer
                                  (opponent currentPlayer)
                                  newBoard
                            in
                              if score < lastScore then
                                if score < min then
                                  ( StopEarly min, lastPosition )
                                else
                                  ( KeepGoing score, Just position )
                              else
                                ( KeepGoing lastScore, lastPosition )

                          ( StopEarly lastValue, lastPosition ) ->
                            ( StopEarly lastValue, lastPosition )

                    _ ->
                      ( KeepGoing <| getScore depth originalPlayer winState
                      , Just position
                      )
              )
              startingValue
              availablePositions
        in
          case scoreAndMove of
            ( StopEarly score, Just position ) ->
              ( score, Just position )

            ( KeepGoing score, Just position ) ->
              ( score, Just position )

            ( _, Nothing ) ->
              ( 0, Nothing )


getBestMoveForPlayer : Board -> Player -> Maybe Position
getBestMoveForPlayer board player =
  snd <| minimax 0 -10000 10000 player player board
