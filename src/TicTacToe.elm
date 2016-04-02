module TicTacToe where

import Maybe exposing (andThen)
import List.Extra as ListE exposing (minimumBy, maximumBy)


type Player = X | O

type Square = Open | Taken Player

type Row = T | M | B

type Column = L | C | R

type alias Position = (Row, Column)

type alias Space = (Position, Square)

type alias Board = List Space

type WinState = Playing | Win Player | Draw


opponent : Player -> Player
opponent player =
    case player of
        X -> O
        O -> X


play : Player -> Board -> Position -> Board
play player board position =
    List.map 
        (\(p, s)->
            if position == p then
                (p, Taken player)
            else
                (p, s))
        board


bothPlayers : List Player
bothPlayers = [X, O]


allRows : List Row
allRows = [T, M, B]


allColumns : List Column
allColumns = [L, C, R]


checkRow : Board -> Row -> Player -> Maybe Player
checkRow board row player =
    let
        takenSpaces = List.filter
            (\s ->
                case s of
                    ((r, _), Taken p) ->
                        row == r && player == p
                    _ ->
                        False)
            board
    in
        case takenSpaces of
            [_, _, _] ->
                Just player
            _ ->
                Nothing


checkColumn : Board -> Column -> Player -> Maybe Player
checkColumn board column player =
    let
        takenSpaces = List.filter
            (\s ->
                case s of
                    ((_, c), Taken p) ->
                        column == c && player == p
                    _ ->
                        False)
            board
    in
        case takenSpaces of
            [_, _, _] ->
                Just player
            _ ->
                Nothing


checkColumns : Board -> List (Maybe Player)
checkColumns board =
    List.concatMap
        (\p ->
            List.map 
                (\r ->
                    checkColumn board r p)
                allColumns)
        bothPlayers


checkRows : Board -> List (Maybe Player)
checkRows board =
    List.concatMap
        (\p ->
            List.map 
                (\r ->
                    checkRow board r p)
                allRows)
        bothPlayers


getPlayerFromSpace : Space -> Maybe Player
getPlayerFromSpace space =
    let
        (_, square) = space
    in
        case square of
            Taken player -> Just player
            Open -> Nothing


getNextPlayerFromBoard : Board -> Player -> Maybe Player
getNextPlayerFromBoard board player =
    let
        nextSpace = List.head board
        maybePlayer = nextSpace `andThen` getPlayerFromSpace
    in
        maybePlayer `andThen` (\nextPlayer ->
            if nextPlayer == player then
                Just player
            else
                Nothing)


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
    [checkDiagonal board, checkAntiDiagonal board]


isSpaceOpen : Space -> Bool
isSpaceOpen space =
    case space of
        (_, Open) ->
            True
        _ ->
            False


hasOpenSpaces : Board -> Bool
hasOpenSpaces board =
    List.any isSpaceOpen board


checkWin : Board -> WinState
checkWin board =
    let
        checks = checkRows board ++ checkColumns board ++ checkDiagonals board
        maybePlayer = List.head <| List.filterMap identity checks
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
    ((row, column), Open)


openBoard : Board
openBoard =
    List.concatMap 
        (\r -> 
            List.map
                (openSpace r)
                allColumns)
        allRows


rankPlayerWin : Player -> WinState -> Int
rankPlayerWin player winState =
    case winState of
        Win winner ->
            if winner == player then
                10
            else
                -10
        _ ->
            0

minimax: Board -> Player -> Player -> List (Int, Position)
minimax board originalPlayer currentPlayer =
    let
        availableSpaces = List.filter isSpaceOpen board
        positions = 
            List.map
                (\space -> let (position, _) = space in position)
                availableSpaces
    in
        positions `ListE.andThen`
            (\position ->
                let
                    newBoard = play currentPlayer board position
                    winState = checkWin newBoard
                    minOrMax  =
                        if originalPlayer == currentPlayer then
                            maximumBy fst
                        else
                            minimumBy fst
                in
                    case winState of
                        Playing ->
                            let
                                ranks = 
                                    minimax newBoard originalPlayer
                                        <| opponent currentPlayer
                                maybeRankedMove = minOrMax ranks
                            in
                                case maybeRankedMove of
                                    Just rankedMove ->
                                        [rankedMove]
                                    Nothing ->
                                        []
                        _ ->
                            [(rankPlayerWin originalPlayer winState, position)])

findBestPlayForPlayer : Player -> Board -> Maybe Position
findBestPlayForPlayer player board =
    minimax board player player
        |> maximumBy fst
        |> Maybe.map snd
