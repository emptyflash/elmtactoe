module App where

import TicTacToe exposing (..)

import Signal exposing (Address)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model = 
    { board: Board
    , lastPlayer: Player
    }

type Action 
    = Play Position
    | NoOp


init : Model
init =
    { board = openBoard
    , lastPlayer = O
    }


update : Action -> Model -> Model
update action model =
    case action of
        Play position ->
            let
                player = opponent model.lastPlayer
                newBoard = play player model.board position 
            in
                { model 
                | board = newBoard
                , lastPlayer = player
                }
        NoOp ->
            model

borderColor : String
borderColor =
    "#646C8A"

topBorder : (String, String)
topBorder =
    ("border-top", "2px solid " ++ borderColor)

leftBorder : (String, String)
leftBorder =
    ("border-left", "2px solid " ++ borderColor)

rightBorder : (String, String)
rightBorder =
    ("border-right", "2px solid " ++ borderColor)

bottomBorder : (String, String)
bottomBorder =
    ("border-bottom", "2px solid " ++ borderColor)

getBorders : Position -> List (String, String)
getBorders position =
    case position of
        (T, L) -> [bottomBorder, rightBorder]
        (M, L) -> [bottomBorder, rightBorder, topBorder]
        (B, L) -> [topBorder, rightBorder]
        (T, C) -> [leftBorder, rightBorder, bottomBorder]
        (M, C) -> [leftBorder, rightBorder, topBorder, bottomBorder]
        (B, C) -> [leftBorder, rightBorder, topBorder]
        (T, R) -> [leftBorder, bottomBorder]
        (M, R) -> [leftBorder, topBorder, bottomBorder]
        (B, R) -> [topBorder, leftBorder]


viewSpaceStyle : Position -> Attribute
viewSpaceStyle position = 
    style <| [ ("flex", "1") ] ++ getBorders position

viewSpace : Address Action -> Space -> Html
viewSpace address space = 
    case space of
        (position, Open) -> 
            div 
                [ viewSpaceStyle position
                , onClick address <| Play position
                ] 
                [ ]
        (position, Taken player) -> 
            div [ viewSpaceStyle position ] [ text <| toString player ]

viewSpaceRowStyle : Attribute
viewSpaceRowStyle = 
    style
        [ ("flex", "1")
        , ("display", "flex")
        ]

viewSpaceRow : Address Action -> List Space -> Html
viewSpaceRow address spaces =
    div [ viewSpaceRowStyle ]
        <| List.map (viewSpace address) spaces


partitionGrid : Address Action -> List Space -> List Html
partitionGrid address spaces =
    case spaces of
       [] -> [text ""]
       _ ->
           [viewSpaceRow address <| List.take 3 spaces] 
             ++ (partitionGrid address <| List.drop 3 spaces)

viewBoardStyle : Attribute
viewBoardStyle =
    style
        [ ("display", "flex")
        , ("flex-direction", "column")
        , ("width", "500px")
        , ("height", "500px")
        ]

viewBoard : Address Action -> Board -> Html
viewBoard address board =
    div [ viewBoardStyle ]
        <| partitionGrid address board

containerStyle =
    style 
        [ ("display", "flex")
        , ("justify-content", "center")
        ]

view : Address Action -> Model -> Html
view address model =
    div [ containerStyle ]
        [ viewBoard address model.board 
        , minimax 0 (opponent model.lastPlayer) (opponent model.lastPlayer) model.board
            |> toString
            |> text
        ]
