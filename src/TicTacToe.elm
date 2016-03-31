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


openSpace : Row -> Column -> Space
openSpace row column =
    ((row, column), Open)


openBoard : Board
openBoard =
    List.concatMap 
        (\r -> 
            List.map
                (openSpace r)
                [L, C, R])
        [T, M, B]

