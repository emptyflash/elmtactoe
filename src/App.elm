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
                newBoard = play player position model.board
            in
                { model 
                | board = newBoard
                , lastPlayer = player
                }
        NoOp ->
            model


viewSpaceStyle : Attribute
viewSpaceStyle = 
    style
        [ ("border", "2px solid #dcebdf")
        , ("flex", "1")
        ]

viewSpace : Address Action -> Space -> Html
viewSpace address space = 
    case space of
        (position, Open) -> 
            div 
                [ viewSpaceStyle 
                , onClick address <| Play position
                ] 
                [ ]
        (_, Taken player) -> 
            div [ viewSpaceStyle ] [ text <| toString player ]

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
        [ viewBoard address model.board ]
