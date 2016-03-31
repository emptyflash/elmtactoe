module TicTacToe where


type Player = X | O

type Square = Open | Taken Player

type Row = T | M | B

type Column = L | C | R

type alias Position = (Row, Column)

type alias Space = (Position, Square)

type alias Board = List Space


opponent : Player -> Player
opponent player =
    case player of
        X -> O
        O -> X


play : Player -> Position -> Board -> Board
play player position board =
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


checkWin : Board -> (Bool, Maybe Player)
checkWin board =
    let
        checks = checkRows board ++ checkColumns board
        maybePlayer = List.head <| List.filterMap identity checks
    in
        case maybePlayer of
            Just _ ->
                (True, maybePlayer)
            Nothing ->
                (not <| hasOpenSpaces board, maybePlayer)


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

--findBestPlayForPlayer : Player -> Board -> Position
--findBestPlayForPlayer player board =
