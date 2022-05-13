module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , coloredSquare : Int
 -- , direction : Key
  }

type alias Snake =
  {
    dir : direction
    , body : List Point
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 0
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key

game_trs : Model -> List Html
game_trs model =
    List.map
        ( game_tr model )
        [1..model.size.height]

type Action = Tick Time | KeyPress KeyCode

update : Action -> Model -> ( Model, Effects Action)
update action model = 
  case action of 
    Tick now ->
      ( frame model now, Effects.tick Tick)

    KeyPress key ->
      ( process_key model key, Effects.none )

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}

updateSquare : Model -> Model
updateSquare ({ coloredSquare } as model) =
  coloredSquare + 1
  |> modBy 1600
  |> Setters.setColoredSquareIn model

grow_snake : List Point -> List Point
grow_snake body = body ++ repeat 10 ( Point 0 0 )

eat_apple : Model -> Model
eat_apple model =
    case List.head model.snake.body of
        Nothing -> model -- Snake is empty won't happen
        Just h ->
            if h == model.eat_apple
               then 
                   let ( apple_pos, seed ) =
                       new_apple_position model.random_seed world_s
                   in
                   {
                       model |
                           snake       <- grow_snake model.snake,
                           apple       <- apple_pos,
                           random_seed <- seed
                   }
              else model

toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  not gameStarted
  |> Setters.setGameStartedIn model
  |> Update.none

arrowUp : Model -> ( Model, Cmd Msg )
arrowUp ({ coloredSquare } as model) =
  coloredSquare - 40
  |> Setters.setColoredSquareIn model
  |> Update.none

arrowDown : Model -> ( Model, Cmd Msg )
arrowDown ({ coloredSquare } as model) =
  coloredSquare + 40
  |> Setters.setColoredSquareIn model
  |> Update.none

arrowLeft : Model -> ( Model, Cmd Msg )
arrowLeft ({ coloredSquare } as model) =
  coloredSquare - 1
  |> Setters.setColoredSquareIn model
  |> Update.none

arrowRight : Model -> ( Model, Cmd Msg )
arrowRight ({ coloredSquare } as model) =
  coloredSquare + 1
  |> Setters.setColoredSquareIn model
  |> Update.none

keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
 --({model|direction=key},Cmd.none) 
  case Debug.log "key" key of
    Space   -> toggleGameLoop model
    ArrowUp -> arrowUp model
    ArrowDown  -> arrowDown model
    ArrowLeft  -> arrowLeft model
    ArrowRight -> arrowRight model
  --  _ -> Update.none model

nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= 1000 then
    updateSquare model
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> Update.none
  else
    time_
    |> Setters.setTimeIn model
    |> Update.none

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model

{-| Manage all your view functions here. -}
cell : Int -> Int -> Html msg
cell index active =
  let class = if active == index then "cell active" else "cell" in
  Html.div [ Attributes.class class ] []

numberCells : Int -> Int -> List (Html msg)
numberCells n active =
        case n of 
          0 ->  []
          1 ->  cell 0 active :: []
          x ->  (cell (x-1) active )::(numberCells (x-1) active)


movingSquare : Model -> Html msg
movingSquare { coloredSquare } =
  Html.div [ Attributes.class "grid" ](List.reverse (numberCells 1600 coloredSquare))
  
  

actualTime : Model -> Html msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time"
    , time
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

explanations : Model -> Html Msg
explanations ({ gameStarted } as model) =
  let word = if gameStarted then "Stop" else "Start" in
  Html.div [ Attributes.class "separator" ]
    [ Html.h1 []
      [ Html.text "Welcome to the snake project!" ]
    , actualTime model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ Html.img [ Attributes.src "/logo.svg" ] []
    , explanations model
    , movingSquare model
    ]

{-| Parts for the runtime. Get key presses and subscribe to
 -|   requestAnimationFrame for the game loop. You don't have to bother with
 -|   this. -}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed ArrowUp
    "ArrowLeft" -> Decode.succeed ArrowLeft
    "ArrowRight" -> Decode.succeed ArrowRight
    "ArrowDown" -> Decode.succeed ArrowDown
    " " -> Decode.succeed Space
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }
