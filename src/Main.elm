import StartApp.Simple exposing (start)
import App exposing (init, update, view)


main = 
    start
        { model = init
        , update = update
        , view = view
        }
