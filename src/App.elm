module App (..) where

import TicTacToe exposing (..)
import Signal exposing (Address)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
  { board : Board
  , lastPlayer : Player
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
        player =
          opponent model.lastPlayer

        newBoard =
          play player model.board position
      in
        { model
          | board = newBoard
          , lastPlayer = player
        }

    NoOp ->
      model


borderColor : String
borderColor =
  "#000000"


topBorder : ( String, String )
topBorder =
  ( "border-top", "2px solid " ++ borderColor )


leftBorder : ( String, String )
leftBorder =
  ( "border-left", "2px solid " ++ borderColor )


rightBorder : ( String, String )
rightBorder =
  ( "border-right", "2px solid " ++ borderColor )


bottomBorder : ( String, String )
bottomBorder =
  ( "border-bottom", "2px solid " ++ borderColor )


getBorders : Position -> List ( String, String )
getBorders position =
  case position of
    ( T, L ) ->
      [ bottomBorder, rightBorder ]

    ( M, L ) ->
      [ bottomBorder, rightBorder, topBorder ]

    ( B, L ) ->
      [ topBorder, rightBorder ]

    ( T, C ) ->
      [ leftBorder, rightBorder, bottomBorder ]

    ( M, C ) ->
      [ leftBorder, rightBorder, topBorder, bottomBorder ]

    ( B, C ) ->
      [ leftBorder, rightBorder, topBorder ]

    ( T, R ) ->
      [ leftBorder, bottomBorder ]

    ( M, R ) ->
      [ leftBorder, topBorder, bottomBorder ]

    ( B, R ) ->
      [ topBorder, leftBorder ]


viewSpaceStyle : Position -> Attribute
viewSpaceStyle position =
  style
    <| [ ( "flex", "1" )
       , ( "display", "flex" )
       , ( "justify-content", "center" )
       , ( "align-items", "center" )
       ]
    ++ getBorders position


viewPlayer : Player -> Html
viewPlayer player =
  if player == X then
    i [ class "fa fa-times fa-5x" ] []
  else
    i [ class "fa fa-circle-o fa-5x" ] []


viewSpace : (Position -> Html.Attribute) -> Space -> Html
viewSpace getEventAction space =
  case space of
    ( position, Open ) ->
      div
        [ viewSpaceStyle position
        , getEventAction position
        ]
        []

    ( position, Taken player ) ->
      div [ viewSpaceStyle position ] [ viewPlayer player ]


viewSpaceRowStyle : Attribute
viewSpaceRowStyle =
  style
    [ ( "flex", "1" )
    , ( "display", "flex" )
    ]


viewSpaceRow 
  : (Position -> Html.Attribute)
  -> List Space 
  -> Html
viewSpaceRow getEventAction spaces =
  div [ viewSpaceRowStyle ]
    <| List.map (viewSpace getEventAction) spaces


partitionGrid 
  : (Position -> Html.Attribute) 
  -> List Space 
  -> List Html
partitionGrid getEventAction spaces =
  case spaces of
    [] ->
      [ text "" ]

    _ ->
      [ viewSpaceRow getEventAction <| List.take 3 spaces ]
        ++ (partitionGrid getEventAction <| List.drop 3 spaces)


viewBoardStyle : Attribute
viewBoardStyle =
  style
    [ ( "display", "flex" )
    , ( "flex-direction", "column" )
    , ( "width", "500px" )
    , ( "height", "500px" )
    ]


viewWin : Player -> Html
viewWin player =
    div []
      [ h2 []
          [ player
             |> toString
             |> (flip (++)) " WINS!"
             |> text
          ]
      ]


viewDraw : Html
viewDraw =
    div [] 
      [ h1 [] 
         [ text "IT'S A DRAW" ]
      ]


viewBoard : Address Action -> Board -> Html
viewBoard address board =
  let
    winState = checkWin board
    getEventAction position =
        case winState of
            Playing ->
                onClick address <| Play position
            _ ->
                onClick address <| NoOp
  in
    div [ viewBoardStyle ]
        <| partitionGrid getEventAction board


containerStyle =
  style
    [ ( "display", "flex" )
    , ( "justify-content", "center" )
    , ( "flex-direction", "column-reverse" )
    , ( "align-items", "center" )
    ]


view : Address Action -> Model -> Html
view address model =
  let
      board = model.board
      boardDisplay = viewBoard address board 
      boardAndWinText =
          case checkWin board of
              Playing ->
                 [ boardDisplay ]
              Win player ->
                 [ viewWin player
                 , boardDisplay
                 ]
              Draw ->
                 [ viewDraw
                 , boardDisplay
                 ] 
    in

  div
    [ containerStyle ]
    boardAndWinText
-- minimax 0 (opponent model.lastPlayer) (opponent model.lastPlayer) model.board
